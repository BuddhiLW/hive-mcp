(ns hive-mcp.tools.catchup
  "Native Catchup workflow — thin facade delegating to sub-namespaces.

   Gathers session context from Chroma memory with project scoping.
   Designed for the /catchup skill to restore context at session start.

   Sub-namespace delegation (Sprint 2):
   - catchup.scope     — scope-filtered Chroma queries, project context
   - catchup.format    — entry metadata transforms, response builders
   - catchup.git       — git status via Emacs
   - catchup.enrichment — KG enrichment, grounding, co-access
   - catchup.spawn     — spawn-time context injection (dual-mode)
   - catchup.permeation — auto-permeation of ling wraps

   Public API:
   - handle-native-catchup  — main catchup handler
   - handle-native-wrap     — wrap/crystallize handler
   - spawn-context          — re-export from catchup.spawn"
  (:require [hive-mcp.chroma.core :as chroma]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.catchup.scope :as catchup-scope]
            [hive-mcp.tools.catchup.format :as fmt]
            [hive-mcp.tools.catchup.git :as catchup-git]
            [hive-mcp.tools.catchup.enrichment :as enrichment]
            [hive-mcp.tools.catchup.spawn :as catchup-spawn]
            [hive-mcp.tools.catchup.permeation :as permeation]
            [hive-mcp.channel.memory-piggyback :as memory-piggyback]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.project.tree :as project-tree]
            [hive-mcp.dns.result :refer [rescue]]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports (backward compatibility)
;; =============================================================================

(defn spawn-context
  "Generate a compact context payload for ling spawn injection.
   Delegates to catchup.spawn/spawn-context. See that ns for full docs."
  ([directory] (catchup-spawn/spawn-context directory))
  ([directory opts] (catchup-spawn/spawn-context directory opts)))

(defn check-grounding-freshness
  "Check grounding freshness of top project entries.
   Delegates to catchup.enrichment/check-grounding-freshness."
  [project-id & [opts]]
  (enrichment/check-grounding-freshness project-id opts))

;; =============================================================================
;; Parallel Execution Helpers
;; =============================================================================

(defn- safe-deref
  "Deref a future with timeout-ms. Returns default on timeout or exception."
  [fut timeout-ms default]
  (try
    (let [result (deref fut timeout-ms ::timeout)]
      (if (= result ::timeout)
        (do (future-cancel fut)
            (log/debug "catchup: parallel query timed out")
            default)
        result))
    (catch Exception e
      (log/debug "catchup: parallel deref failed:" (.getMessage e))
      default)))

(def ^:private ^:const query-timeout-ms
  "Timeout for individual parallel query futures."
  15000)

;; =============================================================================
;; Main Catchup Handler
;; =============================================================================

(defn handle-native-catchup
  "Native Clojure catchup implementation that queries Chroma directly.
   Returns structured catchup data with proper project scoping.

   Phase 2: Enriches decisions/conventions with KG relationships."
  [args]
  (let [directory (:directory args)]
    (log/info "native-catchup: querying Chroma with project scope" {:directory directory})
    ;; Guard: early return if Chroma not configured
    (if-not (chroma/embedding-configured?)
      (fmt/chroma-not-configured-error)
      (try
        (let [project-id (scope/get-current-project-id directory)
              project-name (catchup-scope/get-current-project-name directory)
              scopes (fmt/build-scopes project-name project-id)

              ;; ── Wave 1: Fire independent queries in parallel ──
              f-axioms      (future (catchup-scope/query-axioms project-id))
              f-principles  (future (catchup-scope/query-scoped-entries "principle" nil project-id 50))
              f-priority    (future (catchup-scope/query-scoped-entries "convention" ["catchup-priority"]
                                                                        project-id 50))
              f-sessions    (future (catchup-scope/query-scoped-entries "note" ["session-summary"] project-id 10))
              f-decisions   (future (catchup-scope/query-scoped-entries "decision" nil project-id 50))
              f-snippets    (future (catchup-scope/query-scoped-entries "snippet" nil project-id 20))
              f-expiring    (future (catchup-scope/query-expiring-entries project-id 20))
              f-git         (future (catchup-git/gather-git-info directory))

              ;; ── Wave 1: Collect with timeout ──
              axioms               (safe-deref f-axioms query-timeout-ms [])
              principles           (safe-deref f-principles query-timeout-ms [])
              priority-conventions (safe-deref f-priority query-timeout-ms [])
              sessions             (safe-deref f-sessions query-timeout-ms [])
              decisions            (safe-deref f-decisions query-timeout-ms [])
              snippets             (safe-deref f-snippets query-timeout-ms [])
              expiring             (safe-deref f-expiring query-timeout-ms [])
              git-info             (safe-deref f-git query-timeout-ms {})

              ;; ── Wave 2: Dependent query (needs axiom + priority IDs) ──
              conventions (catchup-scope/query-regular-conventions project-id
                                                                   (set (map :id axioms))
                                                                   (set (map :id priority-conventions)))

              ;; Convert to metadata (pure, fast)
              axioms-meta (mapv fmt/entry->axiom-meta axioms)
              principles-meta (mapv #(fmt/entry->catchup-meta % 80) principles)
              priority-meta (mapv fmt/entry->priority-meta priority-conventions)
              sessions-meta (mapv #(fmt/entry->catchup-meta % 80) sessions)
              decisions-base (mapv #(fmt/entry->catchup-meta % 80) decisions)
              conventions-base (mapv #(fmt/entry->catchup-meta % 80) conventions)
              snippets-meta (mapv #(fmt/entry->catchup-meta % 60) snippets)
              expiring-meta (mapv #(fmt/entry->catchup-meta % 80) expiring)

              ;; ── Wave 3: Fire enrichment + KG pre-fetch + side effects in parallel ──
              ;; KG stats and stale-files are independent of enrichment results,
              ;; so we fire them here alongside enrichment instead of sequentially after.
              f-decisions-kg   (future (:entries (enrichment/enrich-entries-with-kg decisions-base)))
              f-conventions-kg (future (:entries (enrichment/enrich-entries-with-kg conventions-base)))
              f-permeation     (future (permeation/auto-permeate-wraps directory))
              f-project-tree   (future
                                 (try (project-tree/maybe-scan-project-tree! (or directory "."))
                                      (catch Exception e
                                        (log/debug "Project tree scan failed (non-fatal):" (.getMessage e))
                                        {:scanned false :error (.getMessage e)})))
              f-disc-decay     (future
                                 (try (disc/apply-time-decay-to-all-discs! :project-id project-id)
                                      (catch Exception e
                                        (log/debug "Disc time-decay failed (non-fatal):" (.getMessage e))
                                        {:updated 0 :skipped 0 :errors 1 :error (.getMessage e)})))
              ;; Pre-fetch KG stats + stale-files in parallel with enrichment (Wave 3)
              ;; These do NOT depend on enriched entries — they query global KG state.
              f-kg-stats       (future
                                 (try (kg-edges/edge-stats)
                                      (catch Exception e
                                        (log/debug "KG stats query failed (non-fatal):" (.getMessage e))
                                        {:total-edges 0})))
              f-stale-files    (future
                                 (try (disc/top-stale-files :n 10 :project-id project-id :threshold 0.5)
                                      (catch Exception e
                                        (log/debug "KG stale-files query failed (non-fatal):" (.getMessage e))
                                        [])))

              ;; ── Wave 3: Collect enrichment results ──
              decisions-enriched   (safe-deref f-decisions-kg query-timeout-ms decisions-base)
              conventions-enriched (safe-deref f-conventions-kg query-timeout-ms conventions-base)

              ;; Collect pre-fetched KG data
              kg-stats    (safe-deref f-kg-stats query-timeout-ms {:total-edges 0})
              stale-files (safe-deref f-stale-files query-timeout-ms [])

              ;; ── Wave 4: KG insights (depends on enriched results + pre-fetched data) ──
              ;; Pass pre-computed kg-stats and stale-files to avoid redundant queries
              kg-insights (enrichment/gather-kg-insights decisions-enriched conventions-enriched
                                                         sessions-meta project-id
                                                         {:pre-kg-stats kg-stats
                                                          :pre-stale-files stale-files})

              ;; Co-access suggestions (batch query: 2 queries instead of 2*N)
              all-entry-ids (mapv :id (concat axioms principles priority-conventions
                                              decisions conventions sessions))
              co-access-suggestions (enrichment/find-co-accessed-suggestions
                                     all-entry-ids all-entry-ids)
              kg-insights (if (seq co-access-suggestions)
                            (assoc kg-insights :co-access-suggestions co-access-suggestions)
                            kg-insights)

              ;; ── Collect side effects ──
              permeation-result (safe-deref f-permeation query-timeout-ms nil)
              project-tree-scan (safe-deref f-project-tree query-timeout-ms {:scanned false})
              disc-decay-stats  (safe-deref f-disc-decay query-timeout-ms {:updated 0 :skipped 0 :errors 0})
              _ (when (pos? (:updated disc-decay-stats 0))
                  (log/info "catchup: disc time-decay applied"
                            {:updated (:updated disc-decay-stats)
                             :errors (:errors disc-decay-stats)}))

              ;; Memory piggyback: enqueue axioms + priority conventions for
              ;; incremental delivery via ---MEMORY--- blocks on subsequent calls.
              ;; Axioms first (highest priority), then priority conventions.
              ;;
              ;; CURSOR ISOLATION: Must use the SAME caller identity formula as
              ;; wrap-handler-memory-piggyback in routes.clj for buffer key alignment.
              ;; Uses _caller_id (injected by bb-mcp) for per-caller isolation,
              ;; falls back to "coordinator" for old bb-mcp versions.
              ;; See extract-caller-id in routes.clj.
              caller-id (or (:_caller_id args) "coordinator")
              piggyback-agent-id (if project-id
                                   (str caller-id "-" project-id)
                                   caller-id)
              piggyback-entries (into (into (vec axioms) principles) priority-conventions)

              ;; Dual-write: Cache entry categories in context-store for pass-by-ref mode.
              ;; Uses context-put-batch! to write all categories in parallel via futures.
              ;; Each category gets its own ctx-id with 'catchup' + category tags.
              ;; TTL: 10 minutes (catchup context useful for the session duration).
              ;; Non-fatal: context-store failure doesn't break catchup.
              catchup-ttl 600000
              scope-tag  (or project-id "global")
              context-refs
              (rescue nil
                      (let [refs (context-store/context-put-batch!
                                  {:axioms                {:data axioms
                                                           :tags #{"catchup" "axioms" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :principles            {:data principles
                                                           :tags #{"catchup" "principles" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :priority-conventions  {:data priority-conventions
                                                           :tags #{"catchup" "priority-conventions" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :sessions              {:data sessions
                                                           :tags #{"catchup" "sessions" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :decisions             {:data decisions
                                                           :tags #{"catchup" "decisions" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :conventions           {:data conventions
                                                           :tags #{"catchup" "conventions" scope-tag}
                                                           :ttl-ms catchup-ttl}
                                   :snippets              {:data snippets
                                                           :tags #{"catchup" "snippets" scope-tag}
                                                           :ttl-ms catchup-ttl}})]
                        (when (seq refs)
                          (log/info "catchup: stored" (count refs) "categories in context-store"
                                    {:refs (keys refs)}))
                        refs))

              _ (when (seq piggyback-entries)
                  (memory-piggyback/enqueue! piggyback-agent-id project-id piggyback-entries context-refs))]

          (fmt/build-catchup-response
           {:project-name project-name :project-id project-id
            :scopes scopes :git-info git-info :permeation permeation-result
            :axioms-meta axioms-meta :principles-meta principles-meta
            :priority-meta priority-meta
            :sessions-meta sessions-meta :decisions-meta decisions-enriched
            :conventions-meta conventions-enriched :snippets-meta snippets-meta
            :expiring-meta expiring-meta :kg-insights kg-insights
            :project-tree-scan project-tree-scan
            :context-refs context-refs}))
        (catch Exception e
          (fmt/catchup-error e))))))

;; =============================================================================
;; Wrap Handler
;; =============================================================================

(defn handle-native-wrap
  "Native Clojure wrap implementation that persists to Chroma directly.
   Uses crystal hooks for harvesting and crystallization."
  [args]
  (let [directory (:directory args)
        agent-id (:agent_id args)]
    (log/info "native-wrap: crystallizing to Chroma" {:directory directory :agent-id agent-id})
    (if-not (chroma/embedding-configured?)
      (fmt/chroma-not-configured-error)
      (try
        (let [harvested (crystal-hooks/harvest-all {:directory directory})
              result (crystal-hooks/crystallize-session harvested)
              project-id (scope/get-current-project-id directory)]
          (if (:error result)
            {:type "text"
             :text (json/write-str {:error (:error result) :session (:session result)})
             :isError true}
            {:type "text"
             :text (json/write-str (assoc result :project-id project-id))}))
        (catch Exception e
          (log/error e "native-wrap failed")
          {:type "text"
           :text (json/write-str {:error (.getMessage e)})
           :isError true})))))
