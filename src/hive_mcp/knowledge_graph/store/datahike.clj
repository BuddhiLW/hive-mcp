(ns hive-mcp.knowledge-graph.store.datahike
  "Datahike implementation of IKGStore protocol."
  (:require [datahike.api :as d]
            [hive-mcp.protocols.kg :as kg]
            [hive-mcp.knowledge-graph.schema :as schema]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private default-db-path "data/kg/datahike")

(defn- make-config
  "Create Datahike configuration map."
  [& [{:keys [db-path backend index id]
       :or {db-path default-db-path
            backend :file
            index :datahike.index/persistent-set}}]]
  (let [store-id (or id (java.util.UUID/nameUUIDFromBytes (.getBytes "hive-mcp-kg")))]
    (case backend
      :file {:store {:backend :file
                     :path db-path
                     :id store-id}
             :schema-flexibility :read
             :index index}
      (:mem :memory) {:store {:backend :memory
                              :id store-id}
                      :schema-flexibility :read
                      :index index}
      {:store {:backend :file
               :path db-path
               :id store-id}
       :schema-flexibility :read
       :index index})))

(defn- validate-config!
  "Validate Datahike configuration and create directories if needed."
  [cfg]
  (when-not (map? cfg)
    (throw (ex-info "Datahike config must be a map" {:cfg cfg})))
  (when-not (get-in cfg [:store :backend])
    (throw (ex-info "Datahike config missing :store :backend" {:cfg cfg})))

  (when (= :file (get-in cfg [:store :backend]))
    (let [db-path (get-in cfg [:store :path])]
      (when (or (nil? db-path) (empty? db-path))
        (throw (ex-info "Datahike file backend requires :store :path"
                        {:cfg cfg})))
      (let [dir (io/file db-path)]
        (when-not (.exists (.getParentFile dir))
          (log/info "Creating Datahike parent directory" {:path (.getParent dir)})
          (.mkdirs (.getParentFile dir))))))
  cfg)

(defrecord DatahikeStore [conn-atom cfg]
  kg/IKGStore

  (ensure-conn! [_this]
    (when (nil? @conn-atom)
      (log/info "Initializing Datahike KG store" {:cfg cfg})
      (validate-config! cfg)
      (let [dh-schema (schema/full-schema)]
        (log/debug "Datahike schema" {:attributes (count dh-schema)})
        (when-not (d/database-exists? cfg)
          (when (= :file (get-in cfg [:store :backend]))
            (let [dir (io/file (get-in cfg [:store :path]))]
              (when (and (.exists dir) (empty? (.listFiles dir)))
                (.delete dir))))
          (log/info "Creating new Datahike database" {:cfg cfg})
          (d/create-database cfg))
        (reset! conn-atom (d/connect cfg))))
    @conn-atom)

  (transact! [this tx-data]
    (d/transact (kg/ensure-conn! this) tx-data))

  (query [this q]
    (d/q q (d/db (kg/ensure-conn! this))))

  (query [this q inputs]
    (apply d/q q (d/db (kg/ensure-conn! this)) inputs))

  (entity [this eid]
    (d/entity (d/db (kg/ensure-conn! this)) eid))

  (entid [this lookup-ref]
    (let [[attr val] lookup-ref
          results (d/q '[:find ?e .
                         :in $ ?attr ?val
                         :where [?e ?attr ?val]]
                       (d/db (kg/ensure-conn! this))
                       attr val)]
      results))

  (pull-entity [this pattern eid]
    (d/pull (d/db (kg/ensure-conn! this)) pattern eid))

  (db-snapshot [this]
    (d/db (kg/ensure-conn! this)))

  (reset-conn! [this]
    (log/info "Resetting Datahike KG store" {:cfg cfg})
    (when-let [c @conn-atom]
      (try
        (d/release c)
        (catch Exception e
          (log/warn "Failed to release Datahike conn during reset"
                    {:error (.getMessage e)}))))
    (when (d/database-exists? cfg)
      (d/delete-database cfg))
    (reset! conn-atom nil)
    (kg/ensure-conn! this))

  (close! [_this]
    (when-let [c @conn-atom]
      (log/info "Closing Datahike KG store" {:cfg cfg})
      (try
        (d/release c)
        (catch Exception e
          (log/warn "Failed to release Datahike connection"
                    {:error (.getMessage e)})))
      (reset! conn-atom nil)))

  kg/ITemporalKGStore

  (history-db [this]
    (d/history (d/db (kg/ensure-conn! this))))

  (as-of-db [this tx-or-time]
    (d/as-of (d/db (kg/ensure-conn! this)) tx-or-time))

  (since-db [this tx-or-time]
    (d/since (d/db (kg/ensure-conn! this)) tx-or-time)))

(defn create-store
  "Create a new Datahike-backed graph store."
  [& [opts]]
  (try
    (let [cfg (make-config opts)]
      (log/info "Creating Datahike graph store" {:cfg cfg})
      (->DatahikeStore (atom nil) cfg))
    (catch Exception e
      (log/error "Failed to create Datahike store, falling back to DataScript"
                 {:error (.getMessage e) :opts opts})
      nil)))

(defn history-db
  "Get full history database for temporal queries."
  [store]
  (d/history (d/db (kg/ensure-conn! store))))

(defn as-of-db
  "Get database as of a specific transaction or timestamp."
  [store tx-or-time]
  (d/as-of (d/db (kg/ensure-conn! store)) tx-or-time))

(defn since-db
  "Get database with only facts added since a transaction or timestamp."
  [store tx-or-time]
  (d/since (d/db (kg/ensure-conn! store)) tx-or-time))

(defn query-history
  "Query against the full history database."
  [store q & inputs]
  (if (seq inputs)
    (apply d/q q (history-db store) inputs)
    (d/q q (history-db store))))

(defn query-as-of
  "Query the database as it was at a specific point in time."
  [store tx-or-time q & inputs]
  (if (seq inputs)
    (apply d/q q (as-of-db store tx-or-time) inputs)
    (d/q q (as-of-db store tx-or-time))))
