(ns hive-mcp.knowledge-graph.synthetic
  "CRUD operations for L3 Synthetic (Emergent) Knowledge Nodes.

   Synthetic nodes represent emergent patterns discovered through co-access
   analysis, temporal proximity, or semantic similarity. They project onto
   L2 entries via :projects-to edges, enabling pattern-level queries.

   Per convention 20260131014506-72b6afed: L3 is emergent, not stored as
   raw entries, but as synthesized clusters."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.schema :as schema]))

;; =============================================================================
;; ID Generation
;; =============================================================================

(defn generate-synthetic-id
  "Generate a unique synthetic node ID with timestamp prefix.
   Format: synth-yyyyMMddTHHmmss-XXXXXX"
  []
  (let [now (java.time.LocalDateTime/now)
        formatter (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss")
        timestamp (.format now formatter)
        random-hex (format "%06x" (rand-int 0xFFFFFF))]
    (str "synth-" timestamp "-" random-hex)))

;; =============================================================================
;; Create Operations
;; =============================================================================

(defn create-synthetic!
  "Create a new synthetic (L3 emergent) node.

   Required keys:
   - :type       - Synthetic type (must be in schema/synthetic-types)
   - :members    - Initial set of member entry IDs (at least 2)

   Optional keys:
   - :id           - Custom ID (auto-generated if not provided)
   - :confidence   - Aggregate confidence score 0.0-1.0 (default: 0.5)
   - :label        - Human-readable label
   - :scope        - Project scope where discovered
   - :centroid     - Optional embedding vector

   Returns the synthetic node ID on success, throws on validation failure."
  [{:keys [id type members confidence label scope centroid]
    :or {confidence 0.5}}]
  ;; Validate type
  (when-not (schema/valid-synthetic-type? type)
    (throw (ex-info "Invalid synthetic type"
                    {:type type
                     :valid-types schema/synthetic-types})))
  ;; Validate members
  (when (or (nil? members) (< (count members) 2))
    (throw (ex-info "Synthetic node requires at least 2 members"
                    {:members members})))
  ;; Validate confidence
  (when-not (schema/valid-confidence? confidence)
    (throw (ex-info "Invalid confidence score (must be 0.0-1.0)"
                    {:confidence confidence})))

  (let [synth-id (or id (generate-synthetic-id))
        now (java.util.Date.)
        synth-data (cond-> {:kg-synthetic/id synth-id
                            :kg-synthetic/type type
                            :kg-synthetic/members (set members)
                            :kg-synthetic/confidence confidence
                            :kg-synthetic/created-at now
                            :kg-synthetic/last-reinforced now}
                     label (assoc :kg-synthetic/label label)
                     scope (assoc :kg-synthetic/scope scope)
                     centroid (assoc :kg-synthetic/centroid centroid))]
    (conn/transact! [synth-data])
    synth-id))

;; =============================================================================
;; Read Operations
;; =============================================================================

(defn get-synthetic
  "Get a synthetic node by its ID.
   Returns the synthetic entity map or nil if not found."
  [synth-id]
  (when-let [eid (conn/entid [:kg-synthetic/id synth-id])]
    (conn/pull-entity '[*] eid)))

(defn get-synthetics-by-type
  "Query all synthetic nodes of a specific type.
   Optional scope filter."
  ([synth-type]
   (get-synthetics-by-type synth-type nil))
  ([synth-type scope]
   (let [base-query '[:find [(pull ?e [*]) ...]
                      :in $ ?type
                      :where [?e :kg-synthetic/type ?type]]
         scoped-query '[:find [(pull ?e [*]) ...]
                        :in $ ?type ?scope
                        :where
                        [?e :kg-synthetic/type ?type]
                        [?e :kg-synthetic/scope ?scope]]]
     (if scope
       (conn/query scoped-query synth-type scope)
       (conn/query base-query synth-type)))))

(defn get-all-synthetics
  "Get all synthetic nodes. Use with caution on large graphs.
   Optional scope filter."
  ([]
   (get-all-synthetics nil))
  ([scope]
   (let [base-query '[:find [(pull ?e [*]) ...]
                      :where [?e :kg-synthetic/id]]
         scoped-query '[:find [(pull ?e [*]) ...]
                        :in $ ?scope
                        :where
                        [?e :kg-synthetic/id]
                        [?e :kg-synthetic/scope ?scope]]]
     (if scope
       (conn/query scoped-query scope)
       (conn/query base-query)))))

(defn get-synthetics-for-member
  "Get all synthetic nodes that contain the given member ID.
   Returns vector of synthetic entities."
  [member-id]
  (let [query '[:find [(pull ?e [*]) ...]
                :in $ ?member
                :where [?e :kg-synthetic/members ?member]]]
    (conn/query query member-id)))

;; =============================================================================
;; Update Operations
;; =============================================================================

(defn update-synthetic-confidence!
  "Update the confidence score of a synthetic node.
   Returns true on success, nil if node not found.
   Throws on validation failure."
  [synth-id new-confidence]
  (when-not (schema/valid-confidence? new-confidence)
    (throw (ex-info "Invalid confidence score (must be 0.0-1.0)"
                    {:confidence new-confidence})))
  (when-let [eid (conn/entid [:kg-synthetic/id synth-id])]
    (conn/transact! [[:db/add eid :kg-synthetic/confidence new-confidence]])
    true))

(defn reinforce-synthetic!
  "Update the last-reinforced timestamp of a synthetic node.
   Call when a synthetic pattern is re-observed (e.g., co-access).
   Returns true on success, nil if node not found."
  [synth-id]
  (when-let [eid (conn/entid [:kg-synthetic/id synth-id])]
    (conn/transact! [[:db/add eid :kg-synthetic/last-reinforced (java.util.Date.)]])
    true))

(defn increment-synthetic-confidence!
  "Increment the confidence score of a synthetic node by delta.
   Clamps result to 0.0-1.0 range.
   Returns the new confidence score, or nil if node not found."
  [synth-id delta]
  (when-let [synth (get-synthetic synth-id)]
    (let [old-confidence (or (:kg-synthetic/confidence synth) 0.5)
          new-confidence (-> (+ old-confidence delta)
                             (max 0.0)
                             (min 1.0))]
      (update-synthetic-confidence! synth-id new-confidence)
      new-confidence)))

;; =============================================================================
;; Member Operations
;; =============================================================================

(defn add-member!
  "Add a member entry ID to a synthetic node's cluster.
   Returns true on success, nil if node not found."
  [synth-id member-id]
  (when-let [eid (conn/entid [:kg-synthetic/id synth-id])]
    (conn/transact! [[:db/add eid :kg-synthetic/members member-id]])
    true))

(defn remove-member!
  "Remove a member entry ID from a synthetic node's cluster.
   Returns true on success, nil if node not found.

   Note: Does not delete the synthetic node even if members drop below 2.
   The caller should handle cleanup if needed."
  [synth-id member-id]
  (when-let [eid (conn/entid [:kg-synthetic/id synth-id])]
    (conn/transact! [[:db/retract eid :kg-synthetic/members member-id]])
    true))

(defn get-members
  "Get all member IDs of a synthetic node.
   Returns a set of member IDs, or nil if node not found."
  [synth-id]
  (when-let [synth (get-synthetic synth-id)]
    (set (:kg-synthetic/members synth))))

;; =============================================================================
;; Projection Edge Operations
;; =============================================================================
;;
;; Projection edges connect synthetic L3 nodes to their constituent L2 entries.
;; These are stored as regular KG edges with relation :projects-to.

(defn add-projection-edge!
  "Create a projection edge from a synthetic node to an L2 entry.
   The edge represents that the synthetic pattern 'projects to' the entry.

   Arguments:
   - synth-id  - The synthetic node ID
   - entry-id  - The L2 memory entry ID

   Optional keys:
   - :confidence - Edge confidence (default: matches synthetic confidence)
   - :scope      - Edge scope (defaults to synthetic's scope)

   Returns the edge ID on success."
  [synth-id entry-id & [{:keys [confidence scope]}]]
  (let [synth (get-synthetic synth-id)
        edge-id (conn/gen-edge-id)
        now (java.util.Date.)
        synth-confidence (or (:kg-synthetic/confidence synth) 0.5)
        synth-scope (:kg-synthetic/scope synth)
        edge-data (cond-> {:kg-edge/id edge-id
                           :kg-edge/from synth-id
                           :kg-edge/to entry-id
                           :kg-edge/relation :projects-to
                           :kg-edge/confidence (or confidence synth-confidence)
                           :kg-edge/created-at now
                           :kg-edge/last-verified now
                           :kg-edge/source-type :inferred}
                    (or scope synth-scope) (assoc :kg-edge/scope (or scope synth-scope)))]
    (conn/transact! [edge-data])
    edge-id))

(defn get-projections
  "Get all L2 entry IDs that a synthetic node projects to.
   Returns a vector of entry IDs."
  [synth-id]
  (let [query '[:find [?to ...]
                :in $ ?from
                :where
                [?e :kg-edge/from ?from]
                [?e :kg-edge/relation :projects-to]
                [?e :kg-edge/to ?to]]]
    (vec (conn/query query synth-id))))

(defn get-projected-from
  "Get all synthetic node IDs that project to a given L2 entry.
   Returns a vector of synthetic node IDs."
  [entry-id]
  (let [query '[:find [?from ...]
                :in $ ?to
                :where
                [?e :kg-edge/to ?to]
                [?e :kg-edge/relation :projects-to]
                [?e :kg-edge/from ?from]]]
    (vec (conn/query query entry-id))))

(defn get-projection-edges
  "Get all projection edges for a synthetic node.
   Returns full edge entities with all fields."
  [synth-id]
  (let [query '[:find [(pull ?e [*]) ...]
                :in $ ?from
                :where
                [?e :kg-edge/from ?from]
                [?e :kg-edge/relation :projects-to]]]
    (conn/query query synth-id)))

;; =============================================================================
;; Delete Operations
;; =============================================================================

(defn remove-synthetic!
  "Delete a synthetic node by its ID.
   Does NOT delete projection edges - caller should clean up separately.
   Returns true if node was removed, false if not found."
  [synth-id]
  (if-let [eid (conn/entid [:kg-synthetic/id synth-id])]
    (do
      (conn/transact! [[:db/retractEntity eid]])
      true)
    false))

(defn remove-synthetic-with-edges!
  "Delete a synthetic node and all its projection edges.
   Returns {:synthetic-removed? bool :edges-removed int}"
  [synth-id]
  (let [edges (get-projection-edges synth-id)
        edge-ids (map :kg-edge/id edges)]
    ;; Remove edges first
    (doseq [edge-id edge-ids]
      (when-let [eid (conn/entid [:kg-edge/id edge-id])]
        (conn/transact! [[:db/retractEntity eid]])))
    ;; Remove synthetic node
    (let [removed? (remove-synthetic! synth-id)]
      {:synthetic-removed? removed?
       :edges-removed (count edge-ids)})))

;; =============================================================================
;; Statistics
;; =============================================================================

(defn synthetic-stats
  "Get statistics about synthetic nodes in the Knowledge Graph.

   Returns:
     {:total-synthetics <n>
      :by-type          {<type-kw> <count>}
      :by-scope         {<scope-string> <count>}
      :avg-members      <float>
      :avg-confidence   <float>}"
  []
  (let [all-synthetics (get-all-synthetics)
        by-type (frequencies (map :kg-synthetic/type all-synthetics))
        by-scope (frequencies (keep :kg-synthetic/scope all-synthetics))
        member-counts (map #(count (:kg-synthetic/members %)) all-synthetics)
        confidences (keep :kg-synthetic/confidence all-synthetics)]
    {:total-synthetics (count all-synthetics)
     :by-type by-type
     :by-scope by-scope
     :avg-members (if (seq member-counts)
                    (double (/ (reduce + member-counts) (count member-counts)))
                    0.0)
     :avg-confidence (if (seq confidences)
                       (double (/ (reduce + confidences) (count confidences)))
                       0.0)}))
