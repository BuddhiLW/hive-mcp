(ns hive-mcp.tools.catchup
  "Native Catchup workflow implementation.

   Gathers session context from Chroma memory with project scoping.
   Designed for the /catchup skill to restore context at session start.

   Priority Loading:
   - Swarm conventions tagged 'catchup-priority' are surfaced FIRST
   - These help coordinator trust swarm patterns immediately

   KG Context (Phase 2):
   - Decisions/conventions enriched with Knowledge Graph relationships
   - Surfaces superseded entries, dependencies, and contradictions"
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.knowledge-graph.queries :as kg-queries]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.set :as set]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Utility Helpers
;; =============================================================================

(defn- distinct-by
  "Return distinct elements from coll by the value of (f item).
   Keeps the first occurrence of each unique (f item) value."
  [f coll]
  (let [seen (volatile! #{})]
    (filterv (fn [item]
               (let [key (f item)]
                 (if (contains? @seen key)
                   false
                   (do (vswap! seen conj key) true))))
             coll)))

;; =============================================================================
;; Project Context Helpers
;; =============================================================================

(defn- get-current-project-id
  "Get current project ID from Emacs, or 'global' if not in a project.
   When directory is provided, uses that path to determine project context."
  ([] (get-current-project-id nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--project-id %s)" (pr-str directory))
                   "(hive-mcp-memory--project-id)")
           {:keys [success result]} (ec/eval-elisp elisp)]
       (if (and success result (not= result "nil"))
         (str/replace result #"\"" "")
         "global"))
     (catch Exception _
       "global"))))

(defn- get-current-project-name
  "Get current project name from Emacs.
   When directory is provided, uses that path to determine project context."
  ([] (get-current-project-name nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--get-project-name %s)" (pr-str directory))
                   "(hive-mcp-memory--get-project-name)")
           {:keys [success result]} (ec/eval-elisp elisp)]
       (if (and success result (not= result "nil"))
         (str/replace result #"\"" "")
         nil))
     (catch Exception _
       nil))))

;; =============================================================================
;; Entry Transformation
;; =============================================================================

(defn- entry->catchup-meta
  "Convert a Chroma entry to catchup metadata format.
   Returns map with :id, :type, :preview, :tags."
  [entry preview-len]
  (let [content (:content entry)
        content-str (if (string? content)
                      content
                      (str content))
        preview (subs content-str 0 (min (count content-str) (or preview-len 80)))]
    {:id (:id entry)
     :type (name (or (:type entry) "note"))
     :preview preview
     :tags (vec (or (:tags entry) []))}))

;; =============================================================================
;; Scope Filtering
;; =============================================================================

(defn- filter-by-tags
  "Filter entries to only those containing all specified tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

(defn- query-scoped-entries
  "Query Chroma entries filtered by project scope.
   Uses project-id metadata filtering AND hierarchical scope matching.
   Aligned with crud.clj approach for consistent behavior."
  [entry-type tags project-id limit]
  (when (chroma/embedding-configured?)
    (let [limit-val (or limit 20)
          ;; Pass project-id to Chroma for metadata filtering (like crud.clj)
          entries (chroma/query-entries :type entry-type
                                        :project-id project-id
                                        :limit (* limit-val 5))
          ;; Apply hierarchical scope filter for entries with scope tags
          ;; nil = auto mode, derives scope from project-id
          scope-tag (scope/make-scope-tag project-id)
          scope-filter (scope/derive-hierarchy-scope-filter scope-tag)
          scoped (if scope-filter
                   (filter #(scope/matches-hierarchy-scopes? % scope-filter) entries)
                   entries)
          ;; NOTE: filter-by-tags has signature [entries tags], so we can't use ->>
          filtered (filter-by-tags scoped tags)]
      (take limit-val filtered))))

;; =============================================================================
;; Expiring Entries
;; =============================================================================

(defn- entry-expiring-soon?
  "Check if entry expires within 7 days."
  [entry]
  (when-let [exp (:expires entry)]
    (try
      (let [exp-time (java.time.ZonedDateTime/parse exp)
            now (java.time.ZonedDateTime/now)
            week-later (.plusDays now 7)]
        (.isBefore exp-time week-later))
      (catch Exception _ false))))

(defn- query-expiring-entries
  "Query entries expiring within 7 days, scoped to project."
  [project-id limit]
  (let [entries (chroma/query-entries :project-id project-id :limit 50)
        scope-tag (scope/make-scope-tag project-id)
        scope-filter (scope/derive-hierarchy-scope-filter scope-tag)
        scoped (if scope-filter
                 (filter #(scope/matches-hierarchy-scopes? % scope-filter) entries)
                 entries)]
    (->> scoped
         (filter entry-expiring-soon?)
         (take (or limit 5)))))

;; =============================================================================
;; Git Context
;; =============================================================================

(defn- gather-git-info
  "Gather git information from Emacs for the given directory."
  [directory]
  (try
    (let [git-elisp (if directory
                      (format "(let ((default-directory %s))
                                 (json-encode
                                  (list :branch (string-trim (shell-command-to-string \"git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'\"))
                                        :uncommitted (not (string-empty-p (shell-command-to-string \"git status --porcelain 2>/dev/null\")))
                                        :last-commit (string-trim (shell-command-to-string \"git log -1 --format='%%h - %%s' 2>/dev/null || echo 'none'\")))))"
                              (pr-str directory))
                      "(json-encode
                         (list :branch (string-trim (shell-command-to-string \"git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'\"))
                               :uncommitted (not (string-empty-p (shell-command-to-string \"git status --porcelain 2>/dev/null\")))
                               :last-commit (string-trim (shell-command-to-string \"git log -1 --format='%h - %s' 2>/dev/null || echo 'none'\"))))")
          {:keys [success result]} (ec/eval-elisp git-elisp)]
      (when success
        (json/read-str result :key-fn keyword)))
    (catch Exception _
      {:branch "unknown" :uncommitted false :last-commit "unknown"})))

;; =============================================================================
;; Metadata Conversion
;; =============================================================================

(defn- entry->axiom-meta
  "Convert entry to axiom metadata with full content."
  [entry]
  {:id (:id entry)
   :type "axiom"
   :tags (vec (or (:tags entry) []))
   :content (:content entry)
   :severity "INVIOLABLE"})

(defn- entry->priority-meta
  "Convert entry to priority convention metadata with full content."
  [entry]
  {:id (:id entry)
   :type "convention"
   :tags (vec (or (:tags entry) []))
   :content (:content entry)})

;; =============================================================================
;; Entry Queries
;; =============================================================================

(defn- query-axioms
  "Query axiom entries (both formal type and legacy tagged conventions)."
  [project-id]
  (let [formal (query-scoped-entries "axiom" nil project-id 10)
        legacy (query-scoped-entries "convention" ["axiom"] project-id 10)]
    (distinct-by :id (concat formal legacy))))

(defn- query-regular-conventions
  "Query conventions excluding axioms and priority ones."
  [project-id axiom-ids priority-ids]
  (let [all-conventions (query-scoped-entries "convention" nil project-id 15)
        excluded-ids (set/union axiom-ids priority-ids)]
    (remove #(contains? excluded-ids (:id %)) all-conventions)))

;; =============================================================================
;; Knowledge Graph Context Enrichment
;; =============================================================================

(defn- find-related-via-session-summaries
  "Find entries related to session summaries via :derived-from traversal.
   Session summaries often derive from decisions/conventions made during session.

   Returns vector of related entry IDs."
  [session-ids _project-id]
  (when (seq session-ids)
    (try
      (->> session-ids
           (mapcat (fn [session-id]
                     ;; Traverse incoming :derived-from edges to find source entries
                     ;; Note: Don't pass scope - catchup should see all related entries
                     (let [results (kg-queries/traverse
                                    session-id
                                    {:direction :incoming
                                     :relations #{:derived-from}
                                     :max-depth 2})]
                       (map :node-id results))))
           (distinct)
           (vec))
      (catch Exception e
        (log/debug "Session traversal failed:" (.getMessage e))
        []))))

(defn- find-related-decisions-via-kg
  "Find decisions connected via :implements, :refines, or :depends-on relationships.
   These are decisions that have active dependencies in the knowledge graph.

   Returns vector of related decision IDs."
  [decision-ids _project-id]
  (when (seq decision-ids)
    (try
      (->> decision-ids
           (mapcat (fn [decision-id]
                     ;; Look for entries that implement or refine this decision
                     ;; Note: Don't pass scope - catchup should see all related entries
                     (let [results (kg-queries/traverse
                                    decision-id
                                    {:direction :both
                                     :relations #{:implements :refines :depends-on}
                                     :max-depth 2})]
                       (map :node-id results))))
           (distinct)
           (remove (set decision-ids))  ; Exclude original decisions
           (vec))
      (catch Exception e
        (log/debug "Decision traversal failed:" (.getMessage e))
        []))))

(defn- count-ungrounded-entries
  "Count entries that may need verification (re-grounding).
   Queries Chroma for entries without recent grounding timestamp.

   Returns count of potentially stale entries."
  [project-id]
  (try
    ;; Query decisions and conventions that might need verification
    (let [decisions (chroma/query-entries :type "decision" :project-id project-id :limit 50)
          conventions (chroma/query-entries :type "convention" :project-id project-id :limit 50)
          all-entries (concat decisions conventions)
          ;; Count entries without grounded-at or with old grounding (> 7 days)
          now (java.time.Instant/now)
          week-ago (.minusDays now 7)
          needs-grounding? (fn [entry]
                             (let [grounded-at (get-in entry [:metadata :grounded-at])]
                               (or (nil? grounded-at)
                                   (try
                                     (let [grounded-inst (java.time.Instant/parse grounded-at)]
                                       (.isBefore grounded-inst week-ago))
                                     (catch Exception _ true)))))]
      (count (filter needs-grounding? all-entries)))
    (catch Exception e
      (log/debug "Ungrounded count failed:" (.getMessage e))
      0)))

(defn- extract-kg-relations
  "Extract meaningful KG relationships from node context.

   Returns map with:
   - :supersedes - entries this replaces (outgoing :supersedes)
   - :superseded-by - entries that replace this (incoming :supersedes)
   - :depends-on - prerequisites (outgoing :depends-on)
   - :depended-by - entries depending on this (incoming :depends-on)
   - :derived-from - source materials (outgoing :derived-from)
   - :contradicts - conflicting knowledge (both directions)"
  [{:keys [incoming outgoing]}]
  (let [;; Helper to extract node IDs by relation from edge list
        extract-by-rel (fn [edges rel from?]
                         (->> (:edges edges)
                              (filter #(= (:kg-edge/relation %) rel))
                              (map #(if from?
                                      (:kg-edge/from %)
                                      (:kg-edge/to %)))
                              (vec)))
        ;; Outgoing relations (this node → others)
        supersedes (extract-by-rel outgoing :supersedes false)
        depends-on (extract-by-rel outgoing :depends-on false)
        derived-from (extract-by-rel outgoing :derived-from false)
        ;; Incoming relations (others → this node)
        superseded-by (extract-by-rel incoming :supersedes true)
        depended-by (extract-by-rel incoming :depends-on true)
        ;; Contradictions (both directions)
        contradicts-out (extract-by-rel outgoing :contradicts false)
        contradicts-in (extract-by-rel incoming :contradicts true)
        contradicts (vec (distinct (concat contradicts-out contradicts-in)))]
    ;; Only include non-empty relations
    (cond-> {}
      (seq supersedes) (assoc :supersedes supersedes)
      (seq superseded-by) (assoc :superseded-by superseded-by)
      (seq depends-on) (assoc :depends-on depends-on)
      (seq depended-by) (assoc :depended-by depended-by)
      (seq derived-from) (assoc :derived-from derived-from)
      (seq contradicts) (assoc :contradicts contradicts))))

(defn- enrich-entry-with-kg
  "Enrich a single entry with its KG relationships.
   Returns entry with :kg key if relationships exist."
  [entry]
  (try
    (when-let [entry-id (:id entry)]
      (let [context (kg-queries/get-node-context entry-id)
            relations (extract-kg-relations context)]
        (if (seq relations)
          (assoc entry :kg relations)
          entry)))
    (catch Exception e
      (log/debug "KG enrichment failed for" (:id entry) ":" (.getMessage e))
      entry)))

(defn- enrich-entries-with-kg
  "Enrich a collection of entries with KG context.
   Only entries with KG relationships will have :kg key added.

   Returns {:entries [...] :kg-count n :warnings []}"
  [entries]
  (try
    (let [enriched (mapv enrich-entry-with-kg entries)
          kg-count (count (filter :kg enriched))]
      (when (pos? kg-count)
        (log/debug "Enriched" kg-count "entries with KG context"))
      {:entries enriched
       :kg-count kg-count})
    (catch Exception e
      (log/warn "KG enrichment failed:" (.getMessage e))
      {:entries entries
       :kg-count 0
       :warnings [(.getMessage e)]})))

(defn- gather-kg-insights
  "Gather high-level KG insights for catchup summary.

   Returns comprehensive KG context including:
   - :edge-count - total edges in KG
   - :by-relation - breakdown by relation type
   - :contradictions - entries with conflicts that need attention
   - :superseded - entries that have been replaced (may need cleanup)
   - :dependency-chains - count of entries with dependency relationships
   - :related-decisions - decisions connected via KG traversal
   - :ungrounded-count - entries needing verification"
  [decisions-meta conventions-meta sessions-meta project-id]
  (try
    (let [;; Always query KG stats first - gives overview even without enriched entries
          kg-stats (kg-edges/edge-stats)
          edge-count (:total-edges kg-stats)
          by-relation (:by-relation kg-stats)

          ;; Extract IDs for traversal
          session-ids (mapv :id sessions-meta)
          decision-ids (mapv :id decisions-meta)

          ;; Find related entries via KG traversal
          related-from-sessions (find-related-via-session-summaries session-ids project-id)
          related-decisions (find-related-decisions-via-kg decision-ids project-id)

          ;; Count ungrounded entries
          ungrounded-count (count-ungrounded-entries project-id)

          ;; Find entries with concerning relationships from enriched data
          all-entries (concat decisions-meta conventions-meta)
          contradictions (->> all-entries
                              (filter #(seq (get-in % [:kg :contradicts])))
                              (mapv #(select-keys % [:id :preview :kg])))
          superseded (->> all-entries
                          (filter #(seq (get-in % [:kg :superseded-by])))
                          (mapv #(select-keys % [:id :preview :kg])))
          ;; Find entries with dependencies (decision chains)
          with-deps (->> all-entries
                         (filter #(or (seq (get-in % [:kg :depends-on]))
                                      (seq (get-in % [:kg :depended-by]))))
                         (count))]

      ;; Build insights map - always include edge-count for visibility
      (cond-> {:edge-count edge-count}
        (seq by-relation) (assoc :by-relation by-relation)
        (seq contradictions) (assoc :contradictions contradictions)
        (seq superseded) (assoc :superseded superseded)
        (pos? with-deps) (assoc :dependency-chains with-deps)
        (seq related-from-sessions) (assoc :session-derived related-from-sessions)
        (seq related-decisions) (assoc :related-decisions related-decisions)
        (pos? ungrounded-count) (assoc :ungrounded-count ungrounded-count)))
    (catch Exception e
      (log/warn "KG insights gathering failed:" (.getMessage e))
      {:edge-count 0 :error (.getMessage e)})))

;; =============================================================================
;; Response Building
;; =============================================================================

(defn- build-scopes
  "Build scope list for display."
  [project-name project-id]
  (cond-> ["scope:global"]
    project-name (conj (str "scope:project:" project-name))
    (and project-id (not= project-id project-name))
    (conj (str "scope:project:" project-id))))

(defn- build-catchup-response
  "Build the final catchup response structure."
  [{:keys [project-name project-id scopes git-info permeation
           axioms-meta priority-meta sessions-meta decisions-meta
           conventions-meta snippets-meta expiring-meta kg-insights]}]
  {:type "text"
   :text (json/write-str
          {:success true
           :project (or project-name project-id "global")
           :scopes scopes
           :git git-info
           :permeation permeation  ;; Auto-permeated ling wraps
           :counts {:axioms (count axioms-meta)
                    :priority-conventions (count priority-meta)
                    :sessions (count sessions-meta)
                    :decisions (count decisions-meta)
                    :conventions (count conventions-meta)
                    :snippets (count snippets-meta)
                    :expiring (count expiring-meta)}
           :axioms axioms-meta
           :priority-conventions priority-meta
           :context {:sessions sessions-meta
                     :decisions decisions-meta
                     :conventions conventions-meta
                     :snippets snippets-meta
                     :expiring expiring-meta}
           ;; KG insights surface contradictions and superseded entries
           :kg-insights kg-insights
           :hint "AXIOMS are INVIOLABLE - follow them word-for-word. Priority conventions and axioms loaded with full content. Entries with :kg key have Knowledge Graph relationships. Use mcp_memory_get_full for other entries."})})

(defn- chroma-not-configured-error
  "Return error response when Chroma is not configured."
  []
  {:type "text"
   :text (json/write-str {:success false
                          :error "Chroma not configured"
                          :message "Memory query requires Chroma with embedding provider"})
   :isError true})

;; =============================================================================
;; Auto-Permeation (Architecture > LLM behavior)
;; =============================================================================

(defn- auto-permeate-wraps
  "Automatically permeate pending ling wraps during catchup.

   This ensures coordinator always gets ling learnings without explicit call.
   Architecture-driven: catchup guarantees permeation, no LLM action required.

   Returns map with :permeated count and :agents list."
  [directory]
  (try
    (let [project-id (scope/get-current-project-id directory)
          ;; Include children for hierarchical projects
          queue-items (ds/get-unprocessed-wraps-for-hierarchy project-id)
          processed-count (count queue-items)
          agent-ids (mapv :wrap-queue/agent-id queue-items)]
      ;; Mark each as processed
      (doseq [item queue-items]
        (ds/mark-wrap-processed! (:wrap-queue/id item)))
      (when (pos? processed-count)
        (log/info "catchup auto-permeated" processed-count "ling wraps from agents:" agent-ids))
      {:permeated processed-count
       :agents agent-ids})
    (catch Exception e
      (log/warn "auto-permeate failed (non-fatal):" (.getMessage e))
      {:permeated 0 :agents [] :error (.getMessage e)})))

(defn- catchup-error
  "Return error response for catchup failures."
  [e]
  (log/error e "native-catchup failed")
  {:type "text"
   :text (json/write-str {:success false :error (.getMessage e)})
   :isError true})

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
      (chroma-not-configured-error)
      (try
        (let [project-id (get-current-project-id directory)
              project-name (get-current-project-name directory)
              scopes (build-scopes project-name project-id)

              ;; Query entries (use project-id for scoping, aligned with crud.clj)
              axioms (query-axioms project-id)
              priority-conventions (query-scoped-entries "convention" ["catchup-priority"]
                                                         project-id 5)
              sessions (query-scoped-entries "note" ["session-summary"] project-id 3)
              decisions (query-scoped-entries "decision" nil project-id 10)
              conventions (query-regular-conventions project-id
                                                     (set (map :id axioms))
                                                     (set (map :id priority-conventions)))
              snippets (query-scoped-entries "snippet" nil project-id 5)
              expiring (query-expiring-entries project-id 5)
              git-info (gather-git-info directory)

              ;; Convert to metadata
              axioms-meta (mapv entry->axiom-meta axioms)
              priority-meta (mapv entry->priority-meta priority-conventions)
              sessions-meta (mapv #(entry->catchup-meta % 80) sessions)

              ;; Phase 2 KG Integration: Enrich decisions and conventions
              ;; This surfaces relationships like supersedes, depends-on, contradicts
              decisions-base (mapv #(entry->catchup-meta % 80) decisions)
              conventions-base (mapv #(entry->catchup-meta % 80) conventions)
              decisions-enriched (:entries (enrich-entries-with-kg decisions-base))
              conventions-enriched (:entries (enrich-entries-with-kg conventions-base))

              ;; Gather KG insights for high-level visibility
              ;; Pass sessions-meta and project-id for traversal queries
              kg-insights (gather-kg-insights decisions-enriched conventions-enriched
                                              sessions-meta project-id)

              snippets-meta (mapv #(entry->catchup-meta % 60) snippets)
              expiring-meta (mapv #(entry->catchup-meta % 80) expiring)

              ;; Auto-permeate pending ling wraps (Architecture > LLM behavior)
              ;; This ensures coordinator always gets ling learnings without explicit call
              permeation (auto-permeate-wraps directory)]

          (build-catchup-response
           {:project-name project-name :project-id project-id
            :scopes scopes :git-info git-info :permeation permeation
            :axioms-meta axioms-meta :priority-meta priority-meta
            :sessions-meta sessions-meta :decisions-meta decisions-enriched
            :conventions-meta conventions-enriched :snippets-meta snippets-meta
            :expiring-meta expiring-meta :kg-insights kg-insights}))
        (catch Exception e
          (catchup-error e))))))

(defn handle-native-wrap
  "Native Clojure wrap implementation that persists to Chroma directly.
   Uses crystal hooks for harvesting and crystallization."
  [args]
  (let [directory (:directory args)
        agent-id (:agent_id args)]
    (log/info "native-wrap: crystallizing to Chroma" {:directory directory :agent-id agent-id})
    (if-not (chroma/embedding-configured?)
      (chroma-not-configured-error)
      (try
        (let [harvested (crystal-hooks/harvest-all {:directory directory})
              result (crystal-hooks/crystallize-session harvested)
              project-id (get-current-project-id directory)]
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
