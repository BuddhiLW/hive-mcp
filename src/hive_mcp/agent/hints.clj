(ns hive-mcp.agent.hints
  "Structured memory hints for ling priming. Generates pointer-based context instead of full text injection."
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.queries :as kg-queries]
            [clojure.string :as str]
            [clojure.set :as set]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- query-scoped-entries
  "Query Chroma entries filtered by project scope."
  [entry-type tags project-id limit]
  (when (chroma/embedding-configured?)
    (let [limit-val (or limit 20)
          in-project? (and project-id (not= project-id "global"))
          entries (chroma/query-entries :type entry-type
                                        :limit (min (* limit-val 8) 500))
          full-scope-tags (cond-> (kg-scope/full-hierarchy-scope-tags project-id)
                            in-project? (disj "scope:global"))
          all-visible-ids (cond-> (set (into (vec (kg-scope/visible-scopes project-id))
                                             (kg-scope/descendant-scopes project-id)))
                            in-project? (disj "global"))
          scoped (filter (fn [entry]
                           (let [entry-tags (set (or (:tags entry) []))]
                             (or (some entry-tags full-scope-tags)
                                 (contains? all-visible-ids (:project-id entry)))))
                         entries)
          filtered (if (seq tags)
                     (filter (fn [entry]
                               (let [entry-tags (set (:tags entry))]
                                 (every? #(contains? entry-tags %) tags)))
                             scoped)
                     scoped)]
      (take limit-val filtered))))

(defn- query-axioms
  "Query axiom entries including legacy tagged conventions."
  [project-id]
  (let [seen (volatile! #{})
        distinct-by-id (fn [coll]
                         (filterv (fn [item]
                                    (let [k (:id item)]
                                      (if (contains? @seen k) false
                                          (do (vswap! seen conj k) true))))
                                  coll))
        formal (query-scoped-entries "axiom" nil project-id 100)
        legacy (query-scoped-entries "convention" ["axiom"] project-id 100)]
    (distinct-by-id (concat formal legacy))))

(defn- axiom-ids
  "Extract axiom IDs visible to the given project scope."
  [project-id]
  (try
    (let [axioms (query-axioms project-id)]
      (mapv :id axioms))
    (catch Exception e
      (log/debug "axiom-ids failed:" (.getMessage e))
      [])))

(defn- priority-convention-ids
  "Extract IDs of catchup-priority conventions."
  [project-id]
  (try
    (let [conventions (query-scoped-entries
                       "convention" ["catchup-priority"] project-id 50)]
      (mapv :id conventions))
    (catch Exception e
      (log/debug "priority-convention-ids failed:" (.getMessage e))
      [])))

(defn- decision-ids
  "Extract IDs of active decisions for this project."
  [project-id]
  (try
    (let [decisions (query-scoped-entries "decision" nil project-id 20)]
      (mapv :id decisions))
    (catch Exception e
      (log/debug "decision-ids failed:" (.getMessage e))
      [])))

(defn- task-semantic-queries
  "Generate semantic search queries from a task description."
  [task-description]
  (when (and task-description (not (str/blank? task-description)))
    [(str "conventions for: " (subs task-description 0 (min 100 (count task-description))))]))

(defn- kg-seed-nodes
  "Find KG seed nodes relevant to this project."
  [project-id]
  (try
    (let [stats (kg-edges/edge-stats)
          total-edges (:total-edges stats)]
      (when (> total-edges 5)
        (let [decisions (query-scoped-entries "decision" nil project-id 5)]
          (->> decisions
               (mapv :id)
               (take 3)
               vec))))
    (catch Exception e
      (log/debug "kg-seed-nodes failed:" (.getMessage e))
      [])))

(defn generate-hints
  "Generate structured memory hints for a ling spawn."
  ([project-id] (generate-hints project-id {}))
  ([project-id {:keys [task extra-ids extra-queries extra-tags]}]
   (let [axioms (axiom-ids project-id)
         priorities (priority-convention-ids project-id)
         decisions (decision-ids project-id)
         task-queries (task-semantic-queries task)
         seeds (kg-seed-nodes project-id)
         read-ids (vec (distinct (concat priorities decisions (or extra-ids []))))
         queries (vec (distinct (concat (or task-queries []) (or extra-queries []))))
         tags (vec (or extra-tags []))]

     {:memory-hints
      (cond-> {:axiom-ids axioms}
        (seq read-ids) (assoc :read-ids read-ids)
        (seq queries) (assoc :queries queries)
        (seq tags) (assoc :tags tags)
        (seq seeds) (assoc :kg-seeds seeds :kg-depth 2))})))

(def ^:const max-l1-ids
  "Maximum memory IDs from KG traversal."
  20)

(def ^:const max-l2-queries
  "Maximum semantic queries from KG neighborhood."
  5)

(def ^:const max-l3-seeds
  "Maximum KG seed nodes for ling-side traversal."
  3)

(defn- extract-semantic-themes
  "Extract semantic query themes from KG traversal results."
  [traversal-results entry]
  (let [relation-themes (->> traversal-results
                             (map (comp name :kg-edge/relation :edge))
                             (remove nil?)
                             distinct)
        entry-tags (set (or (:tags entry) []))
        meaningful-tags (remove #(or (str/starts-with? % "scope:")
                                     (str/starts-with? % "agent:")
                                     (= % "catchup-priority"))
                                entry-tags)
        tag-queries (when (seq meaningful-tags)
                      [(str "conventions for: " (str/join ", " (take 3 meaningful-tags)))])
        relation-queries (when (seq relation-themes)
                           [(str "entries related via: " (str/join ", " (take 3 relation-themes)))])
        type-query (when-let [t (:type entry)]
                     [(str (name t) " entries in this project")])]
    (->> (concat tag-queries relation-queries type-query)
         (remove nil?)
         (take max-l2-queries)
         vec)))

(defn- fallback-tag-search
  "Fallback hint generation using entry tags when KG has no edges."
  [entry]
  (let [tags (or (:tags entry) [])
        meaningful (remove #(or (str/starts-with? % "scope:")
                                (str/starts-with? % "agent:"))
                           tags)]
    {:l1-ids []
     :l2-queries (if (seq meaningful)
                   [(str "conventions for: " (str/join ", " (take 4 meaningful)))]
                   [])
     :l3-seeds []}))

(defn generate-task-hints
  "Generate KG-driven hints for a specific task or memory node."
  [{:keys [task-id memory-id depth] :or {depth 2}}]
  (let [node-id (or task-id memory-id)]
    (when node-id
      (try
        (let [entry (when (chroma/embedding-configured?)
                      (chroma/get-entry-by-id node-id))
              traversal (try
                          (kg-queries/traverse node-id
                                               {:direction :both
                                                :max-depth depth})
                          (catch Exception e
                            (log/debug "generate-task-hints KG traverse failed:" (.getMessage e))
                            []))]
          (if (empty? traversal)
            (fallback-tag-search (or entry {}))
            (let [discovered-ids (->> traversal
                                      (map :node-id)
                                      distinct
                                      (take max-l1-ids)
                                      vec)
                  l2-queries (extract-semantic-themes traversal (or entry {}))
                  depth-1-nodes (->> traversal
                                     (filter #(= 1 (:depth %)))
                                     (map :node-id)
                                     distinct)
                  l3-seeds (->> depth-1-nodes
                                (take max-l3-seeds)
                                (mapv (fn [id] {:id id :depth depth})))]
              {:l1-ids discovered-ids
               :l2-queries l2-queries
               :l3-seeds l3-seeds})))
        (catch Exception e
          (log/warn "generate-task-hints failed, returning empty:" (.getMessage e))
          {:l1-ids [] :l2-queries [] :l3-seeds []})))))

(defn serialize-hints
  "Serialize memory hints to a compact markdown block for spawn injection."
  [{:keys [memory-hints]} & {:keys [project-name git-info]}]
  (let [{:keys [axiom-ids read-ids queries tags kg-seeds kg-depth]} memory-hints
        sections (atom [])]

    (swap! sections conj "## Memory Hints (Auto-Injected)")
    (swap! sections conj "")
    (when project-name
      (swap! sections conj (str "**Project**: " project-name)))
    (swap! sections conj "")
    (swap! sections conj "**Mode**: hints (structured pointers, not full text)")
    (swap! sections conj "**Action**: Process these hints during your Silence phase.")
    (swap! sections conj "")

    (when (seq axiom-ids)
      (swap! sections conj "### Axiom IDs (INVIOLABLE — fetch ALL)")
      (swap! sections conj "```")
      (swap! sections conj (str "memory get " (str/join " " (take 10 axiom-ids))))
      (swap! sections conj "```")
      (swap! sections conj (str "Count: " (count axiom-ids)))
      (swap! sections conj ""))

    (when (seq read-ids)
      (swap! sections conj "### Priority Memory IDs (fetch during Silence)")
      (swap! sections conj "```")
      (doseq [batch (partition-all 5 read-ids)]
        (swap! sections conj (str "memory get " (str/join " " batch))))
      (swap! sections conj "```")
      (swap! sections conj (str "Count: " (count read-ids)))
      (swap! sections conj ""))

    (when (seq queries)
      (swap! sections conj "### Semantic Queries (search during Silence)")
      (doseq [q queries]
        (swap! sections conj (str "- `memory search \"" q "\"`")))
      (swap! sections conj ""))

    (when (seq tags)
      (swap! sections conj "### Tag Queries")
      (doseq [tag-set tags]
        (swap! sections conj (str "- `memory query --tags " (str/join "," tag-set) "`")))
      (swap! sections conj ""))

    (when (seq kg-seeds)
      (swap! sections conj (str "### Knowledge Graph Seeds (traverse depth "
                                (or kg-depth 2) ")"))
      (doseq [seed kg-seeds]
        (swap! sections conj (str "- `kg traverse --start " seed " --depth "
                                  (or kg-depth 2) "`")))
      (swap! sections conj ""))

    (when git-info
      (swap! sections conj "### Git Status")
      (swap! sections conj (str "- **Branch**: " (or (:branch git-info) "unknown")))
      (when (:uncommitted git-info)
        (swap! sections conj "- **Uncommitted changes**: yes"))
      (swap! sections conj (str "- **Last commit**: "
                                (or (:last-commit git-info) "unknown")))
      (swap! sections conj ""))

    (str/join "\n" @sections)))

(defn parse-hints-from-context
  "Parse memory hints from an injected context string."
  [context-str]
  (when (and context-str (str/includes? context-str "## Memory Hints"))
    (let [hints-section (second (str/split context-str #"## Memory Hints" 2))
          id-pattern #"memory get\s+([^\n`]+)"
          ids (->> (re-seq id-pattern (or hints-section ""))
                   (mapcat (fn [[_ ids-str]] (str/split (str/trim ids-str) #"\s+")))
                   (remove str/blank?)
                   vec)
          query-pattern #"memory search \"([^\"]+)\""
          queries (->> (re-seq query-pattern (or hints-section ""))
                       (mapv second))
          tag-pattern #"memory query --tags\s+([^\n`]+)"
          tags (->> (re-seq tag-pattern (or hints-section ""))
                    (mapv (fn [[_ tags-str]]
                            (str/split (str/trim tags-str) #","))))
          kg-pattern #"kg traverse --start\s+(\S+)\s+--depth\s+(\d+)"
          kg-matches (re-seq kg-pattern (or hints-section ""))
          kg-seeds (mapv second kg-matches)
          kg-depth (when (seq kg-matches)
                     (Integer/parseInt (nth (first kg-matches) 2)))
          axiom-section (when hints-section
                          (second (re-find #"(?s)Axiom IDs.*?```\n(.*?)```"
                                           hints-section)))
          axiom-ids (when axiom-section
                      (->> (re-seq id-pattern axiom-section)
                           (mapcat (fn [[_ ids-str]]
                                     (str/split (str/trim ids-str) #"\s+")))
                           (remove str/blank?)
                           vec))
          axiom-set (set (or axiom-ids []))
          read-ids (vec (remove axiom-set ids))]

      (when (or (seq axiom-ids) (seq read-ids) (seq queries) (seq kg-seeds))
        (cond-> {}
          (seq axiom-ids) (assoc :axiom-ids axiom-ids)
          (seq read-ids) (assoc :read-ids read-ids)
          (seq queries) (assoc :queries queries)
          (seq tags) (assoc :tags tags)
          (seq kg-seeds) (assoc :kg-seeds kg-seeds)
          kg-depth (assoc :kg-depth kg-depth))))))

(defn execute-hint-ids
  "Fetch memory entries by ID."
  [ids]
  (when (seq ids)
    (->> ids
         (mapv (fn [id]
                 (try
                   (when-let [entry (chroma/get-entry-by-id id)]
                     {:id id
                      :content (:content entry)
                      :type (name (or (:type entry) "note"))
                      :tags (vec (or (:tags entry) []))})
                   (catch Exception e
                     (log/debug "Failed to fetch hint ID:" id (.getMessage e))
                     nil))))
         (filterv some?))))

(defn execute-hint-queries
  "Run semantic searches for hint queries."
  [queries]
  (when (seq queries)
    (->> queries
         (mapv (fn [q]
                 (try
                   (let [results (chroma/search-similar q :limit 5)]
                     {:query q
                      :results (mapv (fn [r]
                                       {:id (:id r)
                                        :preview (subs (or (:content r) "") 0
                                                       (min (count (or (:content r) "")) 200))
                                        :type (name (or (:type r) "note"))})
                                     results)})
                   (catch Exception e
                     (log/debug "Failed to execute hint query:" q (.getMessage e))
                     {:query q :results [] :error (.getMessage e)}))))
         vec)))

(defn execute-hint-kg-traversal
  "Traverse KG from seed nodes for hint resolution."
  [seeds depth]
  (when (seq seeds)
    (->> seeds
         (mapv (fn [seed]
                 (try
                   (let [results (kg-queries/traverse
                                  seed
                                  {:direction :both
                                   :relations #{:implements :refines :depends-on
                                                :supersedes :derived-from}
                                   :max-depth (or depth 2)})]
                     {:seed seed
                      :related (mapv #(select-keys % [:node-id :relation :confidence])
                                     results)})
                   (catch Exception e
                     (log/debug "Failed KG traversal from seed:" seed (.getMessage e))
                     {:seed seed :related [] :error (.getMessage e)}))))
         vec)))

(defn execute-all-hints
  "Execute all hint levels: fetch IDs, run queries, traverse KG."
  [hints]
  (let [axiom-entries (execute-hint-ids (:axiom-ids hints))
        read-entries (execute-hint-ids (:read-ids hints))
        search-results (execute-hint-queries (:queries hints))
        kg-results (execute-hint-kg-traversal (:kg-seeds hints) (:kg-depth hints))]
    {:axioms axiom-entries
     :entries read-entries
     :search-results search-results
     :kg-graph kg-results
     :stats {:axioms-fetched (count axiom-entries)
             :entries-fetched (count read-entries)
             :queries-run (count (or (:queries hints) []))
             :kg-seeds-traversed (count (or (:kg-seeds hints) []))}}))
