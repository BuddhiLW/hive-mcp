(ns hive-mcp.knowledge-graph.edges
  "CRUD operations for Knowledge Graph edges.

   Provides functions to create, read, update, and delete edges
   between knowledge nodes (memory entries) in the DataScript store."
  (:require [datascript.core :as d]
            [hive-mcp.knowledge-graph.connection :as conn]
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
   - :scope      - Scope where edge was discovered
   - :confidence - Confidence score 0.0-1.0 (default: 1.0)
   - :created-by - Agent ID that created edge

   Returns the edge ID on success, throws on validation failure."
  [{:keys [from to relation scope confidence created-by]
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

  (let [edge-id (generate-edge-id)
        edge-data (cond-> {:kg-edge/id edge-id
                           :kg-edge/from from
                           :kg-edge/to to
                           :kg-edge/relation relation
                           :kg-edge/confidence confidence
                           :kg-edge/created-at (java.util.Date.)}
                    scope (assoc :kg-edge/scope scope)
                    created-by (assoc :kg-edge/created-by created-by))]
    (conn/transact! [edge-data])
    edge-id))

(defn get-edge
  "Get an edge by its ID.
   Returns the edge entity map or nil if not found."
  [edge-id]
  (when-let [eid (d/entid @(conn/get-conn) [:kg-edge/id edge-id])]
    (d/pull @(conn/get-conn) '[*] eid)))

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
       (d/q scoped-query @(conn/get-conn) from-node-id scope)
       (d/q base-query @(conn/get-conn) from-node-id)))))

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
       (d/q scoped-query @(conn/get-conn) to-node-id scope)
       (d/q base-query @(conn/get-conn) to-node-id)))))

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
       (d/q scoped-query @(conn/get-conn) relation scope)
       (d/q base-query @(conn/get-conn) relation)))))

(defn update-edge-confidence!
  "Update the confidence score of an edge.
   Returns true on success, throws on validation failure."
  [edge-id new-confidence]
  (when-not (schema/valid-confidence? new-confidence)
    (throw (ex-info "Invalid confidence score (must be 0.0-1.0)"
                    {:confidence new-confidence})))
  (when-let [eid (d/entid @(conn/get-conn) [:kg-edge/id edge-id])]
    (conn/transact! [[:db/add eid :kg-edge/confidence new-confidence]])
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
  (if-let [eid (d/entid @(conn/get-conn) [:kg-edge/id edge-id])]
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
       (d/q scoped-query @(conn/get-conn) scope)
       (d/q base-query @(conn/get-conn))))))

(defn count-edges
  "Count total edges, optionally filtered by scope."
  ([]
   (count-edges nil))
  ([scope]
   (count (get-all-edges scope))))

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
