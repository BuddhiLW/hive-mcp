(ns hive-mcp.agent.drone.unified-context
  "Unified KG traversal for drone context injection.

   Replaces multiple separate I/O paths (Chroma search, mcp_get_context tool call,
   extension delegation, disc lookups) with a SINGLE KG-rooted traversal that
   gathers both conventions AND domain knowledge in one pass.

   Pipeline:
     1. Resolve seed nodes from task description (semantic search → memory IDs)
     2. Single KG BFS traversal from seeds (structural edges only)
     3. Batch-get discovered node content from Chroma
     4. Classify by memory type into unified context map
     5. Extension point for enhanced enrichment (delegate-or-noop)

   Returns: {:conventions [...] :decisions [...] :domain [...] :snippets [...]
             :edges [...] :seed-count N :traversal-count N}

   SOLID-S: Single responsibility — unified context gathering.
   SOLID-O: Open for extension via ext/get-extension.
   CLARITY-A: Architectural performance — one traversal instead of four.
   CLARITY-Y: Graceful degradation — missing KG/Chroma returns empty context."
  (:require [hive-mcp.knowledge-graph.queries :as kg-queries]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.extensions.registry :as ext]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const max-seed-nodes
  "Maximum seed nodes to use as traversal entry points."
  5)

(def ^:const max-traversal-nodes
  "Maximum total nodes to collect during traversal."
  15)

(def ^:const max-traversal-depth
  "Maximum BFS depth from each seed."
  2)

(def ^:const max-content-chars
  "Maximum characters per content entry to avoid token bloat."
  500)

(def structural-relations
  "Structural KG relations that carry semantic meaning.
   Co-access edges are noise for context injection."
  #{:implements :supersedes :depends-on :refines :contradicts :derived-from :applies-to})

;; =============================================================================
;; Step 1: Resolve Seed Nodes
;; =============================================================================

(defn- resolve-seeds-from-task
  "Extract seed KG node IDs from task description via semantic search.

   Searches Chroma for memory entries semantically similar to the task,
   then uses their IDs as KG traversal entry points.

   Arguments:
     task       - Task description string
     project-id - Project ID for scoping

   Returns:
     Vector of memory entry ID strings (max max-seed-nodes)."
  [task project-id]
  (try
    (when (and (chroma/embedding-configured?) (seq task))
      (let [results (chroma/search-similar task
                                           :limit max-seed-nodes
                                           :project-ids (when project-id [project-id]))]
        (->> results
             (keep (fn [r]
                     (or (get-in r [:metadata :id])
                         (:id r))))
             (take max-seed-nodes)
             vec)))
    (catch Exception e
      (log/debug "resolve-seeds-from-task failed (non-fatal):" (.getMessage e))
      [])))

(defn- resolve-seeds-from-tags
  "Resolve seed node IDs from explicit topic tags.

   Queries Chroma for entries matching tags, returns their IDs.

   Arguments:
     tags       - Vector of topic tag strings
     project-id - Project ID for scoping

   Returns:
     Vector of memory entry ID strings."
  [tags project-id]
  (try
    (when (seq tags)
      (->> tags
           (mapcat (fn [tag]
                     (chroma/query-entries :type nil
                                           :project-id project-id
                                           :limit 3)))
           (filter (fn [entry]
                     (let [entry-tags (set (map str/lower-case (or (:tags entry) [])))]
                       (some #(entry-tags (str/lower-case %)) tags))))
           (map :id)
           (distinct)
           (take max-seed-nodes)
           vec))
    (catch Exception e
      (log/debug "resolve-seeds-from-tags failed (non-fatal):" (.getMessage e))
      [])))

(defn resolve-seeds
  "Resolve seed node IDs from multiple sources.

   Arguments (map):
     :task       - Task description for semantic search
     :tags       - Explicit topic tags
     :ids        - Explicit memory IDs to pass through
     :project-id - Project ID for scoping

   Returns:
     Vector of unique memory entry ID strings (max max-seed-nodes).
     Extension point: delegates to :uc/resolve-seeds if available."
  [{:keys [task tags ids project-id] :as opts}]
  ;; Extension point: enhanced may have better seed resolution
  (or (when-let [ext-fn (ext/get-extension :uc/resolve-seeds)]
        (try
          (let [result (ext-fn opts)]
            (when (seq result) result))
          (catch Exception e
            (log/debug "Extension :uc/resolve-seeds failed, using default:" (.getMessage e))
            nil)))
      ;; Default: combine all seed sources
      (let [from-ids  (vec (or ids []))
            from-task (when (seq task)
                        (resolve-seeds-from-task task project-id))
            from-tags (when (seq tags)
                        (resolve-seeds-from-tags tags project-id))
            all-seeds (->> (concat from-ids from-task from-tags)
                           (distinct)
                           (take max-seed-nodes)
                           vec)]
        (log/debug "Resolved" (count all-seeds) "seed nodes"
                   {:from-ids (count from-ids)
                    :from-task (count (or from-task []))
                    :from-tags (count (or from-tags []))})
        all-seeds)))

;; =============================================================================
;; Step 2: Single KG Traversal
;; =============================================================================

(defn- traverse-from-seeds
  "Perform single BFS traversal from seed nodes, collecting all reachable nodes
   via structural edges.

   Arguments:
     seed-ids - Vector of starting node IDs
     scope    - Project scope string for edge filtering

   Returns:
     {:nodes #{unique node IDs including seeds}
      :edges [{:from :to :relation :confidence} ...]}"
  [seed-ids scope]
  (when (seq seed-ids)
    (let [all-nodes (atom (set seed-ids))
          all-edges (atom [])]
      (doseq [seed-id seed-ids
              :when (< (count @all-nodes) max-traversal-nodes)]
        (try
          (let [results (kg-queries/traverse
                         seed-id
                         {:direction  :both
                          :relations  structural-relations
                          :max-depth  max-traversal-depth
                          :scope      scope})]
            (doseq [{:keys [node-id edge]} results
                    :when (< (count @all-nodes) max-traversal-nodes)]
              (swap! all-nodes conj node-id)
              (swap! all-edges conj
                     {:from       (:kg-edge/from edge)
                      :to         (:kg-edge/to edge)
                      :relation   (:kg-edge/relation edge)
                      :confidence (:kg-edge/confidence edge)})))
          (catch Exception e
            (log/debug "Traversal failed for seed" seed-id ":" (.getMessage e)))))
      ;; Deduplicate edges by [from, to, relation]
      (let [unique-edges (->> @all-edges
                              (group-by (juxt :from :to :relation))
                              vals
                              (map first)
                              vec)]
        {:nodes @all-nodes
         :edges unique-edges}))))

;; =============================================================================
;; Step 3: Batch-Get Node Content
;; =============================================================================

(defn- batch-get-entries
  "Batch-fetch memory entries from Chroma by IDs.

   Arguments:
     node-ids - Set of memory entry IDs

   Returns:
     Vector of entry maps with :id :content :type :tags"
  [node-ids]
  (when (seq node-ids)
    (try
      (->> node-ids
           (keep (fn [id]
                   (try
                     (chroma/get-entry-by-id id)
                     (catch Exception _ nil))))
           (remove nil?)
           vec)
      (catch Exception e
        (log/debug "batch-get-entries failed:" (.getMessage e))
        []))))

;; =============================================================================
;; Step 4: Classify by Type
;; =============================================================================

(defn- truncate-content
  "Truncate content to max-content-chars to prevent token bloat."
  [content]
  (if (and (string? content) (> (count content) max-content-chars))
    (str (subs content 0 max-content-chars) "...")
    content))

(defn- classify-entries
  "Classify fetched entries by memory type into structured map.

   Arguments:
     entries - Vector of entry maps from Chroma

   Returns:
     {:conventions [entries...]
      :decisions   [entries...]
      :snippets    [entries...]
      :domain      [entries...]  ;; notes, axioms, and other types
     }"
  [entries]
  (reduce
   (fn [acc entry]
     (let [entry-type (or (:type entry) "note")
           truncated  (update entry :content truncate-content)
           category   (case entry-type
                        "convention" :conventions
                        "decision"   :decisions
                        "snippet"    :snippets
                         ;; Everything else is domain knowledge
                        :domain)]
       (update acc category (fnil conj []) truncated)))
   {:conventions [] :decisions [] :snippets [] :domain []}
   entries))

;; =============================================================================
;; Step 5: Extension Point for enhanced Enrichment
;; =============================================================================

(defn- enrich-context
  "Extension point for enhanced enrichment of unified context.

   Delegates to :uc/enrich if registered, otherwise returns context unchanged.

   Arguments:
     context - Unified context map
     opts    - Original options map

   Returns:
     Enriched context map (superset of input)."
  [context opts]
  (if-let [ext-fn (ext/get-extension :uc/enrich)]
    (try
      (ext-fn context opts)
      (catch Exception e
        (log/debug "Extension :uc/enrich failed, returning unenriched:" (.getMessage e))
        context))
    context))

;; =============================================================================
;; Public API
;; =============================================================================

(defn gather-unified-context
  "Single-pass unified context gathering for drone task augmentation.

   Replaces separate convention search, domain priming, and disc lookups
   with one KG traversal that discovers both conventions AND domain knowledge
   via structural edges.

   Arguments (map):
     :task       - Task description (used for semantic seed resolution)
     :seeds      - Explicit topic tags/seeds (optional)
     :ids        - Explicit memory IDs to include (optional)
     :project-id - Project ID for scoping
     :scope      - KG scope for edge filtering (defaults to project-id)

   Returns:
     {:conventions  [{:id :content :type :tags} ...]
      :decisions    [{:id :content :type :tags} ...]
      :snippets     [{:id :content :type :tags} ...]
      :domain       [{:id :content :type :tags} ...]
      :edges        [{:from :to :relation :confidence} ...]
      :seed-count   N
      :traversal-count N
      :node-ids     #{...}}

   Graceful degradation:
   - No Chroma → empty seeds, empty entries
   - No KG edges → seeds are only nodes, still batch-fetched
   - Extension failure → default behavior"
  [{:keys [task seeds ids project-id scope] :as opts}]
  (let [effective-scope (or scope project-id)

        ;; Step 1: Resolve seeds
        seed-ids (resolve-seeds
                  {:task       task
                   :tags       seeds
                   :ids        ids
                   :project-id project-id})

        ;; Step 2: Single KG traversal
        {:keys [nodes edges]
         :or {nodes #{} edges []}}
        (if (seq seed-ids)
          (or (traverse-from-seeds seed-ids effective-scope)
              {:nodes (set seed-ids) :edges []})
          {:nodes #{} :edges []})

        ;; Step 3: Batch-get all discovered nodes
        entries (batch-get-entries nodes)

        ;; Step 4: Classify by type
        classified (classify-entries entries)

        ;; Build result
        result (assoc classified
                      :edges edges
                      :seed-count (count seed-ids)
                      :traversal-count (count nodes)
                      :node-ids nodes)]

    ;; Step 5: Extension enrichment
    (let [enriched (enrich-context result opts)]
      (log/info "Unified context gathered"
                {:seeds (count seed-ids)
                 :traversed (count nodes)
                 :edges (count edges)
                 :conventions (count (:conventions enriched))
                 :decisions (count (:decisions enriched))
                 :domain (count (:domain enriched))
                 :snippets (count (:snippets enriched))})
      enriched)))

;; =============================================================================
;; Formatting for Drone Injection
;; =============================================================================

(defn- format-entries-section
  "Format a category of entries as markdown section.

   Arguments:
     title   - Section title
     entries - Vector of entry maps

   Returns:
     Markdown string or nil if empty."
  [title entries]
  (when (seq entries)
    (str "### " title "\n"
         (str/join "\n---\n"
                   (map-indexed
                    (fn [i {:keys [content]}]
                      (str (inc i) ". " (or content "(no content)")))
                    entries))
         "\n\n")))

(defn- format-edges-section
  "Format KG edges as compact arrow notation.

   Arguments:
     edges - Vector of edge maps

   Returns:
     Markdown string or nil if empty."
  [edges]
  (when (seq edges)
    (let [arrow (fn [{:keys [relation]}]
                  (case relation
                    :implements   "-impl->"
                    :supersedes   "-super->"
                    :depends-on   "-dep->"
                    :refines      "-ref->"
                    :contradicts  "-contra->"
                    :derived-from "-from->"
                    :applies-to   "-apply->"
                    (str "-" (name (or relation "?")) "->")))
          truncate-id (fn [id]
                        (if (and (string? id) (> (count id) 8))
                          (subs id 0 8)
                          (str id)))]
      (str "### KG Structure (" (count edges) " edges)\n"
           (str/join "\n"
                     (map (fn [e]
                            (str "  " (truncate-id (:from e))
                                 " " (arrow e) " "
                                 (truncate-id (:to e))))
                          edges))
           "\n\n"))))

(defn format-unified-context
  "Format unified context as markdown string for drone prompt injection.

   Arguments:
     context - Output from gather-unified-context

   Returns:
     Formatted markdown string, or nil if context is empty."
  [context]
  (when (and context
             (or (seq (:conventions context))
                 (seq (:decisions context))
                 (seq (:domain context))
                 (seq (:snippets context))))
    (str "## Unified Project Context (KG-traversed)\n"
         (format-entries-section "Conventions" (:conventions context))
         (format-entries-section "Decisions" (:decisions context))
         (format-entries-section "Domain Knowledge" (:domain context))
         (format-entries-section "Code Snippets" (:snippets context))
         (format-edges-section (:edges context)))))

;; =============================================================================
;; Availability Check
;; =============================================================================

(defn unified-context-available?
  "Check if unified context gathering is possible.
   Requires at minimum Chroma embedding configuration."
  []
  (try
    (chroma/embedding-configured?)
    (catch Exception _ false)))
