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
  (:require [hive-mcp.chroma :as chroma]
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
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.project.tree :as project-tree]
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

              ;; Query entries (use project-id for scoping, aligned with crud.clj)
              axioms (catchup-scope/query-axioms project-id)
              priority-conventions (catchup-scope/query-scoped-entries "convention" ["catchup-priority"]
                                                                       project-id 50)
              sessions (catchup-scope/query-scoped-entries "note" ["session-summary"] project-id 10)
              decisions (catchup-scope/query-scoped-entries "decision" nil project-id 50)
              conventions (catchup-scope/query-regular-conventions project-id
                                                                   (set (map :id axioms))
                                                                   (set (map :id priority-conventions)))
              snippets (catchup-scope/query-scoped-entries "snippet" nil project-id 20)
              expiring (catchup-scope/query-expiring-entries project-id 20)
              git-info (catchup-git/gather-git-info directory)

              ;; Convert to metadata
              axioms-meta (mapv fmt/entry->axiom-meta axioms)
              priority-meta (mapv fmt/entry->priority-meta priority-conventions)
              sessions-meta (mapv #(fmt/entry->catchup-meta % 80) sessions)

              ;; Phase 2 KG Integration: Enrich decisions and conventions
              ;; This surfaces relationships like supersedes, depends-on, contradicts
              decisions-base (mapv #(fmt/entry->catchup-meta % 80) decisions)
              conventions-base (mapv #(fmt/entry->catchup-meta % 80) conventions)
              decisions-enriched (:entries (enrichment/enrich-entries-with-kg decisions-base))
              conventions-enriched (:entries (enrichment/enrich-entries-with-kg conventions-base))

              ;; Gather KG insights for high-level visibility
              ;; Pass sessions-meta and project-id for traversal queries
              kg-insights (enrichment/gather-kg-insights decisions-enriched conventions-enriched
                                                         sessions-meta project-id)

              ;; Phase 3: Co-access suggestions
              ;; Surface entries frequently recalled alongside current context
              all-entry-ids (mapv :id (concat axioms priority-conventions
                                              decisions conventions sessions))
              co-access-suggestions (enrichment/find-co-accessed-suggestions
                                     all-entry-ids all-entry-ids)
              kg-insights (if (seq co-access-suggestions)
                            (assoc kg-insights :co-access-suggestions co-access-suggestions)
                            kg-insights)

              snippets-meta (mapv #(fmt/entry->catchup-meta % 60) snippets)
              expiring-meta (mapv #(fmt/entry->catchup-meta % 80) expiring)

              ;; Auto-permeate pending ling wraps (Architecture > LLM behavior)
              ;; This ensures coordinator always gets ling learnings without explicit call
              permeation-result (permeation/auto-permeate-wraps directory)

              ;; HCR Wave 2: Check project tree staleness and rescan if needed
              ;; This ensures hierarchy is fresh for hierarchical memory scoping
              project-tree-scan (try
                                  (project-tree/maybe-scan-project-tree! (or directory "."))
                                  (catch Exception e
                                    (log/debug "Project tree scan failed (non-fatal):" (.getMessage e))
                                    {:scanned false :error (.getMessage e)}))

              ;; P0.2: Apply disc certainty time-decay at catchup (L1 maintenance)
              ;; Discs lose certainty over time proportional to their volatility class.
              ;; Catchup is the natural cadence: runs at session start, periodic.
              ;; Bounded, idempotent, non-blocking (errors caught inside).
              disc-decay-stats (try
                                 (disc/apply-time-decay-to-all-discs! :project-id project-id)
                                 (catch Exception e
                                   (log/debug "Disc time-decay failed (non-fatal):" (.getMessage e))
                                   {:updated 0 :skipped 0 :errors 1 :error (.getMessage e)}))
              _ (when (pos? (:updated disc-decay-stats 0))
                  (log/info "catchup: disc time-decay applied"
                            {:updated (:updated disc-decay-stats)
                             :errors (:errors disc-decay-stats)}))

              ;; Memory piggyback: enqueue axioms + priority conventions for
              ;; incremental delivery via ---MEMORY--- blocks on subsequent calls.
              ;; Axioms first (highest priority), then priority conventions.
              ;;
              ;; CURSOR IDENTITY FIX: Must use the SAME stable coordinator identity
              ;; as wrap-handler-memory-piggyback in routes.clj for the buffer key.
              ;; Previously used (or (:agent_id args) ...) which diverged from the
              ;; middleware's drain key when caller passed agent_id (e.g. "coordinator").
              ;; See decision 20260207012136-1c3339e5.
              piggyback-agent-id (if project-id
                                   (str "coordinator-" project-id)
                                   "coordinator")
              piggyback-entries (into (vec axioms) priority-conventions)

              ;; Dual-write: Cache entry categories in context-store for pass-by-ref mode.
              ;; Each category gets its own ctx-id with 'catchup' + category tags.
              ;; TTL: 10 minutes (catchup context useful for the session duration).
              ;; Non-fatal: context-store failure doesn't break catchup.
              catchup-ttl 600000
              context-refs
              (try
                (let [refs (cond-> {}
                             (seq axioms)
                             (assoc :axioms (context-store/context-put!
                                             axioms
                                             :tags #{"catchup" "axioms" (or project-id "global")}
                                             :ttl-ms catchup-ttl))
                             (seq priority-conventions)
                             (assoc :priority-conventions (context-store/context-put!
                                                           priority-conventions
                                                           :tags #{"catchup" "priority-conventions" (or project-id "global")}
                                                           :ttl-ms catchup-ttl))
                             (seq sessions)
                             (assoc :sessions (context-store/context-put!
                                               sessions
                                               :tags #{"catchup" "sessions" (or project-id "global")}
                                               :ttl-ms catchup-ttl))
                             (seq decisions)
                             (assoc :decisions (context-store/context-put!
                                                decisions
                                                :tags #{"catchup" "decisions" (or project-id "global")}
                                                :ttl-ms catchup-ttl))
                             (seq conventions)
                             (assoc :conventions (context-store/context-put!
                                                  conventions
                                                  :tags #{"catchup" "conventions" (or project-id "global")}
                                                  :ttl-ms catchup-ttl))
                             (seq snippets)
                             (assoc :snippets (context-store/context-put!
                                               snippets
                                               :tags #{"catchup" "snippets" (or project-id "global")}
                                               :ttl-ms catchup-ttl)))]
                  (when (seq refs)
                    (log/info "catchup: stored" (count refs) "categories in context-store"
                              {:refs (keys refs)}))
                  refs)
                (catch Exception e
                  (log/warn e "catchup: context-store caching failed (non-fatal)")
                  nil))

              _ (when (seq piggyback-entries)
                  (memory-piggyback/enqueue! piggyback-agent-id project-id piggyback-entries context-refs))]

          (fmt/build-catchup-response
           {:project-name project-name :project-id project-id
            :scopes scopes :git-info git-info :permeation permeation-result
            :axioms-meta axioms-meta :priority-meta priority-meta
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
