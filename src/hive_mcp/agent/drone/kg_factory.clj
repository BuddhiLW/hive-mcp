(ns hive-mcp.agent.drone.kg-factory
  "Factory for per-drone isolated KG stores with merge-back to global."
  (:require [clojure.string :as str]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.protocols.kg :as kg]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private drone-stores (atom {}))

(defn active-drone-stores
  "Return a snapshot of all active drone stores."
  []
  @drone-stores)

(defn create-drone-store
  "Create an isolated in-memory KG store for a drone execution."
  [drone-id]
  (when (or (nil? drone-id)
            (and (string? drone-id) (str/blank? drone-id)))
    (throw (ex-info "create-drone-store requires non-blank drone-id"
                    {:error-type :validation
                     :field :drone-id
                     :value drone-id})))
  (log/info "Creating isolated KG store for drone" {:drone-id drone-id})
  (let [store (ds-store/create-store)]
    (kg/ensure-conn! store)
    (swap! drone-stores assoc drone-id store)
    (log/debug "Drone KG store registered" {:drone-id drone-id
                                            :store-count (count @drone-stores)})
    store))

(defn- extract-edges
  "Extract all KG edges from a drone's store."
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
  "Merge KG edges from a drone's isolated store into a target store."
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

(defn close-drone-store!
  "Close and deregister a drone's KG store."
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
  "Close and deregister all drone KG stores."
  []
  (let [store-ids (keys @drone-stores)
        cnt (count store-ids)]
    (log/info "Cleaning up all drone KG stores" {:count cnt})
    (doseq [drone-id store-ids]
      (close-drone-store! drone-id))
    cnt))
