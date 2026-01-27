(ns hive-mcp.knowledge-graph.edges
  "CRUD operations for Knowledge Graph edges.

   Provides functions to create, read, update, and delete edges
   between knowledge nodes (memory entries) via the IGraphStore protocol."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.schema :as schema]))

(defn generate-edge-id
  "Generate a unique edge ID."
  []
  (str (random-uuid)))

(defn add-edge!
  "Create a new edge between two knowledge nodes.

   Required keys:
   - :from       - Source node ID (memory entry ID)
   - :to         - Target node ID (memory entry ID)
   - :relation   - Relation type (must be in schema/relation-types)

   Optional keys:
   - :scope         - Scope where edge was discovered
   - :confidence    - Confidence score 0.0-1.0 (default: 1.0)
   - :created-by    - Agent ID that created edge
   - :source-type   - How edge was established (:manual, :automated, :inferred, :co-access)
   - :last-verified - Timestamp of last verification (defaults to creation time)

   Returns the edge ID on success, throws on validation failure."
  [{:keys [from to relation scope confidence created-by source-type last-verified]
    :or {confidence 1.0}}]
  ;; Validate required fields
  (when (or (nil? from) (nil? to))
    (throw (ex-info "Edge requires :from and :to node IDs"
                    {:from from :to to})))
  (when-not (schema/valid-relation? relation)
    (throw (ex-info "Invalid relation type"
                    {:relation relation
                     :valid-relations schema/relation-types})))
  (when-not (schema/valid-confidence? confidence)
    (throw (ex-info "Invalid confidence score (must be 0.0-1.0)"
                    {:confidence confidence})))
  (when (and source-type (not (schema/valid-source-type? source-type)))
    (throw (ex-info "Invalid source type"
                    {:source-type source-type
                     :valid-source-types schema/source-types})))

  (let [edge-id (generate-edge-id)
        now (java.util.Date.)
        edge-data (cond-> {:kg-edge/id edge-id
                           :kg-edge/from from
                           :kg-edge/to to
                           :kg-edge/relation relation
                           :kg-edge/confidence confidence
                           :kg-edge/created-at now
                           :kg-edge/last-verified (or last-verified now)}
                    scope (assoc :kg-edge/scope scope)
                    created-by (assoc :kg-edge/created-by created-by)
                    source-type (assoc :kg-edge/source-type source-type))]
    (conn/transact! [edge-data])
    edge-id))

(defn get-edge
  "Get an edge by its ID.
   Returns the edge entity map or nil if not found."
  [edge-id]
  (when-let [eid (conn/entid [:kg-edge/id edge-id])]
    (conn/pull-entity '[*] eid)))

(defn get-edges-from
  "Query all outgoing edges from a source node.
   Optional scope filter limits to edges visible from that scope."
  ([from-node-id]
   (get-edges-from from-node-id nil))
  ([from-node-id scope]
   (let [base-query '[:find [(pull ?e [*]) ...]
                      :in $ ?from
                      :where [?e :kg-edge/from ?from]]
         scoped-query '[:find [(pull ?e [*]) ...]
                        :in $ ?from ?scope
                        :where
                        [?e :kg-edge/from ?from]
                        [?e :kg-edge/scope ?scope]]]
     (if scope
       (conn/query scoped-query from-node-id scope)
       (conn/query base-query from-node-id)))))

(defn get-edges-to
  "Query all incoming edges to a target node.
   Optional scope filter limits to edges visible from that scope."
  ([to-node-id]
   (get-edges-to to-node-id nil))
  ([to-node-id scope]
   (let [base-query '[:find [(pull ?e [*]) ...]
                      :in $ ?to
                      :where [?e :kg-edge/to ?to]]
         scoped-query '[:find [(pull ?e [*]) ...]
                        :in $ ?to ?scope
                        :where
                        [?e :kg-edge/to ?to]
                        [?e :kg-edge/scope ?scope]]]
     (if scope
       (conn/query scoped-query to-node-id scope)
       (conn/query base-query to-node-id)))))

(defn get-edges-by-relation
  "Query all edges of a specific relation type.
   Optional scope filter."
  ([relation]
   (get-edges-by-relation relation nil))
  ([relation scope]
   (let [base-query '[:find [(pull ?e [*]) ...]
                      :in $ ?rel
                      :where [?e :kg-edge/relation ?rel]]
         scoped-query '[:find [(pull ?e [*]) ...]
                        :in $ ?rel ?scope
                        :where
                        [?e :kg-edge/relation ?rel]
                        [?e :kg-edge/scope ?scope]]]
     (if scope
       (conn/query scoped-query relation scope)
       (conn/query base-query relation)))))

(defn get-edges-by-scope
  "Query all edges within a specific scope.
   Returns all edges that have the given scope."
  [scope]
  (let [query '[:find [(pull ?e [*]) ...]
                :in $ ?scope
                :where
                [?e :kg-edge/id]
                [?e :kg-edge/scope ?scope]]]
    (conn/query query scope)))

(defn find-edge
  "Find an edge between two nodes.
   Optional relation filter only returns edge if it matches.
   Returns the edge entity map or nil if not found."
  ([from-node-id to-node-id]
   (find-edge from-node-id to-node-id nil))
  ([from-node-id to-node-id relation]
   (let [base-query '[:find [(pull ?e [*]) ...]
                      :in $ ?from ?to
                      :where
                      [?e :kg-edge/from ?from]
                      [?e :kg-edge/to ?to]]
         relation-query '[:find [(pull ?e [*]) ...]
                          :in $ ?from ?to ?rel
                          :where
                          [?e :kg-edge/from ?from]
                          [?e :kg-edge/to ?to]
                          [?e :kg-edge/relation ?rel]]
         results (if relation
                   (conn/query relation-query from-node-id to-node-id relation)
                   (conn/query base-query from-node-id to-node-id))]
     (first results))))

(defn update-edge-confidence!
  "Update the confidence score of an edge.
   Returns true on success, throws on validation failure."
  [edge-id new-confidence]
  (when-not (schema/valid-confidence? new-confidence)
    (throw (ex-info "Invalid confidence score (must be 0.0-1.0)"
                    {:confidence new-confidence})))
  (when-let [eid (conn/entid [:kg-edge/id edge-id])]
    (conn/transact! [[:db/add eid :kg-edge/confidence new-confidence]])
    true))

(defn verify-edge!
  "Update the last-verified timestamp of an edge.
   Call when an edge relationship is confirmed to still be valid.
   Returns true on success, nil if edge not found."
  [edge-id]
  (when-let [eid (conn/entid [:kg-edge/id edge-id])]
    (conn/transact! [[:db/add eid :kg-edge/last-verified (java.util.Date.)]])
    true))

(defn increment-confidence!
  "Increment the confidence score of an edge by delta.
   Clamps result to 0.0-1.0 range.
   Returns the new confidence score, or nil if edge not found."
  [edge-id delta]
  (when-let [edge (get-edge edge-id)]
    (let [old-confidence (or (:kg-edge/confidence edge) 1.0)
          new-confidence (-> (+ old-confidence delta)
                             (max 0.0)
                             (min 1.0))]
      (update-edge-confidence! edge-id new-confidence)
      new-confidence)))

(defn remove-edge!
  "Delete an edge by its ID.
   Returns true if edge was removed, false if not found."
  [edge-id]
  (if-let [eid (conn/entid [:kg-edge/id edge-id])]
    (do
      (conn/transact! [[:db/retractEntity eid]])
      true)
    false))

(defn remove-edges-for-node!
  "Remove all edges connected to a node (both incoming and outgoing).
   Use when deleting a memory entry to clean up its KG relationships.
   Returns the count of edges removed."
  [node-id]
  (let [outgoing (get-edges-from node-id)
        incoming (get-edges-to node-id)
        all-edges (distinct (concat outgoing incoming))
        edge-ids (map :kg-edge/id all-edges)]
    (doseq [eid edge-ids]
      (remove-edge! eid))
    (count edge-ids)))

(defn get-all-edges
  "Get all edges in the KG. Use with caution on large graphs.
   Optional scope filter."
  ([]
   (get-all-edges nil))
  ([scope]
   (let [base-query '[:find [(pull ?e [*]) ...]
                      :where [?e :kg-edge/id]]
         scoped-query '[:find [(pull ?e [*]) ...]
                        :in $ ?scope
                        :where
                        [?e :kg-edge/id]
                        [?e :kg-edge/scope ?scope]]]
     (if scope
       (conn/query scoped-query scope)
       (conn/query base-query)))))

(defn count-edges
  "Count total edges, optionally filtered by scope."
  ([]
   (count-edges nil))
  ([scope]
   (count (get-all-edges scope))))

(defn record-co-access!
  "Record co-access pattern between a batch of memory entries.
   Creates :co-accessed edges between pairs that were recalled together.
   If an edge already exists between a pair, increments its confidence instead.

   Arguments:
     entry-ids - Collection of entry IDs recalled in the same batch (min 2)
     opts      - Optional map with:
                 :scope      - Scope where co-access occurred
                 :created-by - Agent/tool that triggered the recall

   Returns the count of edges created or reinforced."
  [entry-ids & [{:keys [scope created-by]}]]
  (let [ids (vec (distinct entry-ids))]
    (when (>= (count ids) 2)
      (let [pairs (for [i (range (count ids))
                        j (range (inc i) (count ids))]
                    [(nth ids i) (nth ids j)])
            ;; Limit pairs to avoid quadratic explosion on large batches
            limited-pairs (take 50 pairs)]
        (count
         (for [[from-id to-id] limited-pairs]
           (if-let [existing (find-edge from-id to-id :co-accessed)]
             ;; Reinforce existing co-access edge
             (do (increment-confidence! (:kg-edge/id existing) 0.1)
                 (verify-edge! (:kg-edge/id existing))
                 :reinforced)
             ;; Create new co-access edge with low initial confidence
             (do (add-edge! (cond-> {:from from-id
                                     :to to-id
                                     :relation :co-accessed
                                     :confidence 0.3
                                     :source-type :co-access}
                              scope (assoc :scope scope)
                              created-by (assoc :created-by created-by)))
                 :created))))))))

(defn get-co-accessed
  "Get entries co-accessed with the given entry.
   Returns entry IDs sorted by confidence (strongest co-access first).

   Arguments:
     entry-id - Entry ID to find co-accessed entries for

   Returns:
     Vector of {:entry-id <id> :confidence <score>}"
  [entry-id]
  (let [outgoing (get-edges-from entry-id)
        incoming (get-edges-to entry-id)
        co-access-edges (->> (concat outgoing incoming)
                             (filter #(= :co-accessed (:kg-edge/relation %))))
        neighbors (map (fn [edge]
                         {:entry-id (if (= (:kg-edge/from edge) entry-id)
                                      (:kg-edge/to edge)
                                      (:kg-edge/from edge))
                          :confidence (or (:kg-edge/confidence edge) 0.3)})
                       co-access-edges)]
    (->> neighbors
         (sort-by :confidence >)
         vec)))

(defn edge-stats
  "Get statistics about edges in the Knowledge Graph.

   Returns:
     {:total-edges  <n>
      :by-relation  {<relation-kw> <count>}
      :by-scope     {<scope-string> <count>}}"
  []
  (let [all-edges (get-all-edges)
        by-relation (frequencies (map :kg-edge/relation all-edges))
        by-scope (frequencies (keep :kg-edge/scope all-edges))]
    {:total-edges (count all-edges)
     :by-relation by-relation
     :by-scope by-scope}))
