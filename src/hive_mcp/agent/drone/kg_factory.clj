(ns hive-mcp.agent.drone.kg-factory
  "Factory for per-drone isolated KG stores.

   Each drone execution gets its own in-memory DataScript-backed KG store
   for isolation. After execution completes, drone KG edges can be merged
   back into the main (global) store via merge-drone-results!.

   Design rationale:
   - DataScript (in-memory) for drone stores: lightweight, fast, no disk I/O
   - Main store can be Datalevin/Datahike (persistent): durable, shared
   - Merge-back is explicit, not automatic: coordinator decides what to keep

   Integration with ExecutionContext:
   - create-drone-store returns an IKGStore that can be passed as :kg-store
     to ->execution-context
   - get-kg-store (in domain.clj) resolves ctx > dynamic > global

   SOLID-O: Open for extension (new merge strategies) without modifying factory.
   CLARITY-I: Validates drone-id on creation.
   CLARITY-T: Logs store lifecycle events."
  (:require [clojure.string :as str]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.protocols.kg :as kg]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; Drone Store Registry (for cleanup)
;;; ============================================================================

;; Registry of active drone stores for lifecycle management.
;; Keys: drone-id (string), Values: IKGStore instance.
(defonce ^:private drone-stores (atom {}))

(defn active-drone-stores
  "Return a snapshot of all active drone stores.
   Returns: map of {drone-id -> IKGStore}"
  []
  @drone-stores)

;;; ============================================================================
;;; Factory: create-drone-store
;;; ============================================================================

(defn create-drone-store
  "Create an isolated in-memory KG store for a drone execution.

   Uses DataScript (in-memory) for lightweight, fast isolation.
   Each drone gets its own store that doesn't interfere with the
   global store or other drones.

   Arguments:
     drone-id - Unique drone identifier (required, non-blank string)

   Returns:
     IKGStore instance (DataScriptStore) with initialized connection

   Throws:
     ex-info if drone-id is nil or blank

   Side effects:
     - Registers store in drone-stores registry
     - Initializes DataScript connection eagerly

   Example:
     (let [store (create-drone-store \"drone-abc-123\")]
       (kg/transact! store [{:kg-edge/id \"edge-1\"
                             :kg-edge/from \"mem-1\"
                             :kg-edge/to \"mem-2\"
                             :kg-edge/relation :implements}]))"
  [drone-id]
  (when (or (nil? drone-id)
            (and (string? drone-id) (str/blank? drone-id)))
    (throw (ex-info "create-drone-store requires non-blank drone-id"
                    {:error-type :validation
                     :field :drone-id
                     :value drone-id})))
  (log/info "Creating isolated KG store for drone" {:drone-id drone-id})
  (let [store (ds-store/create-store)]
    ;; Eagerly initialize the connection
    (kg/ensure-conn! store)
    ;; Register for lifecycle management
    (swap! drone-stores assoc drone-id store)
    (log/debug "Drone KG store registered" {:drone-id drone-id
                                            :store-count (count @drone-stores)})
    store))

;;; ============================================================================
;;; Merge: merge-drone-results!
;;; ============================================================================

(defn- extract-edges
  "Extract all KG edges from a drone's store.

   Queries for all entities with :kg-edge/id attribute.

   Arguments:
     store - IKGStore to extract edges from

   Returns:
     Vector of edge maps (without :db/id, which is store-local)"
  [store]
  (try
    (let [edge-eids (kg/query store
                              '[:find ?e
                                :where [?e :kg-edge/id _]])
          edges (mapv (fn [[eid]]
                        (-> (kg/pull-entity store '[*] eid)
                            (dissoc :db/id)))
                      edge-eids)]
      edges)
    (catch Exception e
      (log/warn "Failed to extract edges from drone store"
                {:error (.getMessage e)})
      [])))

(defn merge-drone-results!
  "Merge KG edges from a drone's isolated store into a target store.

   Extracts all :kg-edge/* entities from the drone store and transacts
   them into the target store. Uses :kg-edge/id as the identity attribute,
   so duplicate edges are upserted (not duplicated).

   Arguments:
     drone-store  - IKGStore (the drone's isolated store)
     target-store - IKGStore (the main/global store to merge into)

   Returns:
     Map with merge results:
       :edges-found    - Number of edges in drone store
       :edges-merged   - Number of edges successfully transacted
       :errors         - Vector of error details (empty on full success)

   Side effects:
     - Transacts edges into target-store
     - Logs merge activity

   Example:
     (let [result (merge-drone-results! drone-store (kg/get-store))]
       (println (:edges-merged result) \"edges merged\"))"
  [drone-store target-store]
  {:pre [(kg/kg-store? drone-store)
         (kg/kg-store? target-store)]}
  (let [edges (extract-edges drone-store)
        edge-count (count edges)]
    (log/info "Merging drone KG results" {:edge-count edge-count})
    (if (zero? edge-count)
      {:edges-found 0 :edges-merged 0 :errors []}
      (let [results (reduce
                     (fn [acc edge]
                       (try
                         (kg/transact! target-store [edge])
                         (update acc :edges-merged inc)
                         (catch Exception e
                           (log/warn "Failed to merge edge"
                                     {:edge-id (:kg-edge/id edge)
                                      :error (.getMessage e)})
                           (update acc :errors conj
                                   {:edge-id (:kg-edge/id edge)
                                    :error (.getMessage e)}))))
                     {:edges-found edge-count
                      :edges-merged 0
                      :errors []}
                     edges)]
        (log/info "Drone KG merge complete" (select-keys results [:edges-found :edges-merged]))
        results))))

;;; ============================================================================
;;; Cleanup: close-drone-store!
;;; ============================================================================

(defn close-drone-store!
  "Close and deregister a drone's KG store.

   Calls close! on the store and removes it from the registry.
   Safe to call multiple times (idempotent).

   Arguments:
     drone-id - The drone identifier whose store to close

   Returns:
     true if store was found and closed, false if not found"
  [drone-id]
  (if-let [store (get @drone-stores drone-id)]
    (do
      (log/debug "Closing drone KG store" {:drone-id drone-id})
      (try
        (kg/close! store)
        (catch Exception e
          (log/warn "Error closing drone KG store"
                    {:drone-id drone-id :error (.getMessage e)})))
      (swap! drone-stores dissoc drone-id)
      true)
    false))

(defn cleanup-all-drone-stores!
  "Close and deregister ALL drone KG stores.

   Used during shutdown or between test runs.

   Returns:
     Number of stores cleaned up"
  []
  (let [store-ids (keys @drone-stores)
        cnt (count store-ids)]
    (log/info "Cleaning up all drone KG stores" {:count cnt})
    (doseq [drone-id store-ids]
      (close-drone-store! drone-id))
    cnt))
