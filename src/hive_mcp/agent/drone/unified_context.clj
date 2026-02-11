(ns hive-mcp.agent.drone.unified-context
  "Unified KG traversal for drone context injection."
  (:require [hive-mcp.knowledge-graph.queries :as kg-queries]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.extensions.registry :as ext]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
  "Structural KG relations that carry semantic meaning."
  #{:implements :supersedes :depends-on :refines :contradicts :derived-from :applies-to})

(defn- resolve-seeds-from-task
  "Extract seed KG node IDs from task description via semantic search."
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
  "Resolve seed node IDs from explicit topic tags."
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
  "Resolve seed node IDs from multiple sources."
  [{:keys [task tags ids project-id] :as opts}]
  (or (when-let [ext-fn (ext/get-extension :uc/resolve-seeds)]
        (try
          (let [result (ext-fn opts)]
            (when (seq result) result))
          (catch Exception e
            (log/debug "Extension :uc/resolve-seeds failed, using default:" (.getMessage e))
            nil)))
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

(defn- traverse-from-seeds
  "Perform single BFS traversal from seed nodes, collecting all reachable nodes."
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
      (let [unique-edges (->> @all-edges
                              (group-by (juxt :from :to :relation))
                              vals
                              (map first)
                              vec)]
        {:nodes @all-nodes
         :edges unique-edges}))))

(defn- batch-get-entries
  "Batch-fetch memory entries from Chroma by IDs."
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

(defn- truncate-content
  "Truncate content to max-content-chars to prevent token bloat."
  [content]
  (if (and (string? content) (> (count content) max-content-chars))
    (str (subs content 0 max-content-chars) "...")
    content))

(defn- classify-entries
  "Classify fetched entries by memory type into structured map."
  [entries]
  (reduce
   (fn [acc entry]
     (let [entry-type (or (:type entry) "note")
           truncated  (update entry :content truncate-content)
           category   (case entry-type
                        "convention" :conventions
                        "decision"   :decisions
                        "snippet"    :snippets
                        :domain)]
       (update acc category (fnil conj []) truncated)))
   {:conventions [] :decisions [] :snippets [] :domain []}
   entries))

(defn- enrich-context
  "Extension point for enhanced enrichment of unified context."
  [context opts]
  (if-let [ext-fn (ext/get-extension :uc/enrich)]
    (try
      (ext-fn context opts)
      (catch Exception e
        (log/debug "Extension :uc/enrich failed, returning unenriched:" (.getMessage e))
        context))
    context))

(defn gather-unified-context
  "Single-pass unified context gathering for drone task augmentation."
  [{:keys [task seeds ids project-id scope] :as opts}]
  (let [effective-scope (or scope project-id)

        seed-ids (resolve-seeds
                  {:task       task
                   :tags       seeds
                   :ids        ids
                   :project-id project-id})

        {:keys [nodes edges]
         :or {nodes #{} edges []}}
        (if (seq seed-ids)
          (or (traverse-from-seeds seed-ids effective-scope)
              {:nodes (set seed-ids) :edges []})
          {:nodes #{} :edges []})

        entries (batch-get-entries nodes)

        classified (classify-entries entries)

        result (assoc classified
                      :edges edges
                      :seed-count (count seed-ids)
                      :traversal-count (count nodes)
                      :node-ids nodes)]

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

(defn- format-entries-section
  "Format a category of entries as markdown section."
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
  "Format KG edges as compact arrow notation."
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
  "Format unified context as markdown string for drone prompt injection."
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

(defn unified-context-available?
  "Check if unified context gathering is possible."
  []
  (try
    (chroma/embedding-configured?)
    (catch Exception _ false)))
