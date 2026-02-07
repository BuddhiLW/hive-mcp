(ns hive-mcp.tools.catchup.spawn
  "Spawn context injection for ling priming.

   SOLID: SRP - Single responsibility for spawn-time context generation.
   Extracted from hive-mcp.tools.catchup (Sprint 2 refactoring).

   Tri-mode (feature-flagged):
   - :full (default, backward compatible) — full text injection (~12K chars)
   - :hints — structured pointers only (~500 tokens), ling self-hydrates
   - :ref — context-store references only (~200 tokens), ling fetches via context-get

   Architecture > LLM behavior: inject context at spawn time rather than
   relying on lings to /catchup themselves."
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.catchup.scope :as catchup-scope]
            [hive-mcp.tools.catchup.git :as catchup-git]
            [hive-mcp.tools.catchup.format :as fmt]
            [hive-mcp.agent.hints :as hints]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.context.reconstruction :as reconstruction]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Context-Store Ref Lookup (W3 Task 2.2)
;; =============================================================================

(defn- lookup-context-refs
  "Query context-store for catchup-cached entries by project scope.
   Returns map of category->ctx-id if entries found, nil if none.

   W2 dual-write in catchup.clj stores entries with tags:
   #{\"catchup\" \"axioms\" project-id} etc.
   We query by the \"catchup\" tag and filter by project."
  [project-id]
  (try
    (let [entries (context-store/context-query :tags #{"catchup"} :limit 20)
          ;; Filter to entries that match this project scope
          project-tag (or project-id "global")
          matching (filter (fn [entry]
                             (contains? (:tags entry) project-tag))
                           entries)
          ;; Build category->ctx-id map from tag intersection
          category-tags #{"axioms" "priority-conventions" "sessions"
                          "decisions" "conventions" "snippets"}
          refs (reduce (fn [acc entry]
                         (let [cat-tag (first (filter category-tags (:tags entry)))]
                           (if cat-tag
                             (assoc acc (keyword cat-tag) (:id entry))
                             acc)))
                       {}
                       matching)]
      (when (seq refs)
        refs))
    (catch Exception e
      (log/debug "lookup-context-refs failed:" (.getMessage e))
      nil)))

(defn- serialize-ref-context
  "Serialize context-store refs to a compact markdown block for spawn injection.
   Significantly smaller than :full mode (~200 tokens vs ~3K).

   Includes fetch instructions so the ling knows how to hydrate."
  [refs {:keys [project-name git-info]}]
  (let [sections (atom [])]
    ;; Header
    (swap! sections conj "## Project Context (Ref Mode — Auto-Injected)")
    (swap! sections conj "")
    (when project-name
      (swap! sections conj (str "**Project**: " project-name)))
    (swap! sections conj "")
    (swap! sections conj "**Mode**: ref (context-store references, not full text)")
    (swap! sections conj "**Action**: Fetch these refs via `session context-get` to hydrate your context.")
    (swap! sections conj "")

    ;; Context refs table
    (swap! sections conj "### Context References")
    (swap! sections conj "")
    (swap! sections conj "| Category | Context ID |")
    (swap! sections conj "|----------|------------|")
    (doseq [[category ctx-id] (sort-by key refs)]
      (swap! sections conj (str "| " (name category) " | `" ctx-id "` |")))
    (swap! sections conj "")

    ;; Fetch instructions
    (swap! sections conj "### How to Hydrate")
    (swap! sections conj "")
    (swap! sections conj "Fetch each ref via the session tool:")
    (swap! sections conj "```")
    (doseq [[_category ctx-id] (sort-by key refs)]
      (swap! sections conj (str "session {\"command\": \"context-get\", \"ctx_id\": \"" ctx-id "\"}")))
    (swap! sections conj "```")
    (swap! sections conj "")
    (swap! sections conj (str "**TTL Warning**: These refs expire ~10 minutes after catchup. "
                              "If expired, run `/catchup` to refresh."))
    (swap! sections conj "")

    ;; Git info (always useful, minimal cost)
    (when git-info
      (swap! sections conj "### Git Status")
      (swap! sections conj (str "- **Branch**: " (or (:branch git-info) "unknown")))
      (when (:uncommitted git-info)
        (swap! sections conj "- **Uncommitted changes**: yes"))
      (swap! sections conj (str "- **Last commit**: " (or (:last-commit git-info) "unknown")))
      (swap! sections conj ""))

    (str/join "\n" @sections)))

;; =============================================================================
;; Main API
;; =============================================================================

(defn spawn-context
  "Generate a compact context payload for ling spawn injection.

   Architecture > LLM behavior: inject context at spawn time rather than
   relying on lings to /catchup themselves.

   Tri-mode (feature-flagged):
   - :full (default, backward compatible) — full text injection (~12K chars)
   - :hints — structured pointers only (~500 tokens), ling self-hydrates
   - :ref — context-store references (~200 tokens), ling fetches via context-get

   Arguments:
     directory - Working directory for project scoping
     opts      - Optional map:
                 :mode - :full (default), :hints, or :ref
                 :task - Task description (hints mode: generates semantic queries)

   Returns a markdown string, or nil on error/no-config.

   :ref mode: Looks up context-store for entries cached by W2 catchup dual-write.
   If no refs found (no prior catchup in session), falls back to :full mode.

   CLARITY-C: Composes from existing catchup query helpers.
   CLARITY-I: Validates payload size (< 3K tokens / ~12K chars).
   HCR Wave 4: Uses full hierarchy scope, inherits project context from parent.

   See decision: 20260205213357-62f62a93 (Memory Hints Replace Text Injection)"
  ([directory] (spawn-context directory {}))
  ([directory {:keys [mode task task-id] :or {mode :full}}]
   (when (chroma/embedding-configured?)
     (try
       (let [project-id (scope/get-current-project-id directory)
             project-name (catchup-scope/get-current-project-name directory)]

         (case mode
           ;; === REF MODE: Context-store references (~200 tokens) ===
           :ref
           (let [refs (lookup-context-refs project-id)]
             (if refs
               ;; Found cached refs from W2 dual-write — try KG-compressed reconstruction first
               (let [git-info (catchup-git/gather-git-info directory)
                     ;; Extract decision IDs from refs for KG traversal seeds
                     kg-node-ids (when-let [dec-ref (:decisions refs)]
                                   (try
                                     (when-let [entry (context-store/context-get dec-ref)]
                                       (->> (:data entry)
                                            (take 5)
                                            (keep :id)
                                            vec))
                                     (catch Exception _ [])))
                     ;; Try KG-compressed reconstruction
                     reconstructed (try
                                     (reconstruction/reconstruct-context
                                      refs
                                      (or kg-node-ids [])
                                      project-id)
                                     (catch Exception e
                                       (log/debug "spawn-context :ref KG reconstruction failed (non-fatal):"
                                                  (.getMessage e))
                                       nil))]
                 (log/info "spawn-context :ref mode, found" (count refs) "refs"
                           {:categories (keys refs) :kg-reconstructed (some? reconstructed)})
                 (if reconstructed
                   ;; KG-compressed reconstruction succeeded — append git info
                   (str reconstructed
                        (when git-info
                          (str "\n\n### Git Status\n"
                               "- **Branch**: " (or (:branch git-info) "unknown") "\n"
                               (when (:uncommitted git-info) "- **Uncommitted changes**: yes\n")
                               "- **Last commit**: " (or (:last-commit git-info) "unknown") "\n")))
                   ;; Fallback to simple ref table (no KG available)
                   (serialize-ref-context refs
                                          {:project-name (or project-name project-id "global")
                                           :git-info git-info})))
               ;; No refs found (no prior catchup) — fall back to :full
               (do
                 (log/info "spawn-context :ref mode, no cached refs found, falling back to :full")
                 (spawn-context directory {:mode :full :task task :task-id task-id}))))

           ;; === HINTS MODE: Compact structured pointers (~500 tokens) ===
           :hints
           (let [hint-data (hints/generate-hints project-id {:task task})
                 ;; When task-id provided, enrich hints with KG-driven task hints
                 task-hints (when task-id
                              (try
                                (hints/generate-task-hints {:task-id task-id :depth 2})
                                (catch Exception e
                                  (log/debug "Task hint generation failed (non-fatal):" (.getMessage e))
                                  nil)))
                 ;; Merge task-specific hints into general hints
                 enriched-hints (if task-hints
                                  (update hint-data :memory-hints
                                          (fn [mh]
                                            (cond-> mh
                                              (seq (:l1-ids task-hints))
                                              (update :read-ids (fnil into []) (:l1-ids task-hints))
                                              (seq (:l2-queries task-hints))
                                              (update :queries (fnil into []) (:l2-queries task-hints))
                                              (seq (:l3-seeds task-hints))
                                              (assoc :kg-seeds (mapv :id (:l3-seeds task-hints))
                                                     :kg-depth (get (first (:l3-seeds task-hints)) :depth 2)))))
                                  hint-data)
                 git-info (catchup-git/gather-git-info directory)]
             (hints/serialize-hints enriched-hints
                                    :project-name (or project-name project-id "global")
                                    :git-info git-info))

           ;; === FULL MODE: Full text injection (default, backward compatible) ===
           ;; :full and default case
           (let [;; HCR Wave 4: query-scoped-entries now uses full hierarchy scope
                 ;; so spawned lings get context from parent + child projects
                 axioms (catchup-scope/query-axioms project-id)
                 priority-conventions (catchup-scope/query-scoped-entries "convention" ["catchup-priority"]
                                                                          project-id 50)
                 decisions (catchup-scope/query-scoped-entries "decision" nil project-id 50)
                 git-info (catchup-git/gather-git-info directory)

                 ;; L1 Disc: Surface top-N most stale files needing re-grounding
                 stale-files (try
                               (kg-disc/top-stale-files :n 5 :project-id project-id)
                               (catch Exception e
                                 (log/debug "spawn-context stale-files query failed:" (.getMessage e))
                                 []))

                 ;; Transform to metadata
                 axioms-meta (mapv fmt/entry->axiom-meta axioms)
                 priority-meta (mapv fmt/entry->priority-meta priority-conventions)
                 decisions-meta (mapv #(fmt/entry->catchup-meta % 80) decisions)

                 ;; Serialize to markdown
                 context-str (fmt/serialize-spawn-context
                              {:axioms axioms-meta
                               :priority-conventions priority-meta
                               :decisions decisions-meta
                               :stale-files stale-files
                               :git-info git-info
                               :project-name (or project-name project-id "global")})]

             ;; CLARITY-I: Validate payload size
             (if (> (count context-str) fmt/max-spawn-context-chars)
               (do
                 (log/warn "spawn-context exceeds token budget:"
                           (count context-str) "chars, truncating decisions + stale-files")
                 ;; Truncate: keep axioms + priority conventions, drop decisions + stale files
                 (fmt/serialize-spawn-context
                  {:axioms axioms-meta
                   :priority-conventions priority-meta
                   :decisions []
                   :stale-files []
                   :git-info git-info
                   :project-name (or project-name project-id "global")}))
               context-str))))
       (catch Exception e
         (log/warn "spawn-context failed (non-fatal):" (.getMessage e))
         nil)))))
