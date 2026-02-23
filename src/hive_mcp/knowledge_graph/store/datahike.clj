(ns hive-mcp.knowledge-graph.store.datahike
  "Datahike implementation of IKGStore protocol."
  (:require [datahike.api :as d]
            [datahike.norm.norm :as norm]
            [hive-mcp.protocols.kg :as kg]
            [hive-mcp.dns.result :refer [rescue]]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private default-db-path "data/kg/datahike")

(defn- make-writer-config
  "Build the :writer section of Datahike config.
   Supports :self (default local), :datahike-server (HTTP), and :kabel (WebSocket).

   :datahike-server requires {:url \"http://...:4444\" :token \"...\"}
   :kabel requires {:peer-id #uuid \"...\" :local-peer <peer-atom>}

   See https://github.com/replikativ/datahike/blob/main/doc/distributed.md"
  [writer-opts]
  (case (get writer-opts :backend :self)
    :self            {:backend :self}
    :datahike-server {:backend :datahike-server
                      :url (:url writer-opts)
                      :token (:token writer-opts)}
    :kabel           {:backend :kabel
                      :peer-id (:peer-id writer-opts)
                      :local-peer (:local-peer writer-opts)}
    ;; Unknown writer backend — default to local
    {:backend :self}))

(defn- make-config
  "Create Datahike configuration map.
   Accepts optional :writer key for distributed write backends:
     {:writer {:backend :datahike-server :url \"http://...\" :token \"...\"}}
     {:writer {:backend :kabel :peer-id #uuid \"...\" :local-peer peer-atom}}"
  [& [{:keys [db-path backend index id writer]
       :or {db-path default-db-path
            backend :file
            index :datahike.index/persistent-set}}]]
  (let [store-id (or id (java.util.UUID/nameUUIDFromBytes (.getBytes "hive-mcp-kg")))
        store-cfg (case backend
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
                     :index index})
        writer-cfg (when writer (make-writer-config writer))]
    (cond-> store-cfg
      writer-cfg (assoc :writer writer-cfg))))

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
      (rescue nil
              (log/info "Initializing Datahike KG store" {:cfg cfg})
              (validate-config! cfg)
              (when-not (d/database-exists? cfg)
                (when (= :file (get-in cfg [:store :backend]))
                  (let [dir (io/file (get-in cfg [:store :path]))]
                    (when (and (.exists dir) (empty? (.listFiles dir)))
                      (.delete dir))))
                (log/info "Creating new Datahike database" {:cfg cfg})
                (d/create-database cfg))
              (let [conn (d/connect cfg)]
                (log/info "Applying KG norms" {:path "hive_mcp/norms/kg"})
                (norm/ensure-norms! conn (io/resource "hive_mcp/norms/kg"))
                (reset! conn-atom conn))))
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
      (rescue nil (d/release c)))
    (when (d/database-exists? cfg)
      (d/delete-database cfg))
    (reset! conn-atom nil)
    (kg/ensure-conn! this))

  (close! [_this]
    (when-let [c @conn-atom]
      (log/info "Closing Datahike KG store" {:cfg cfg})
      (rescue nil (d/release c))
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
  (rescue nil
          (let [cfg (make-config opts)]
            (log/info "Creating Datahike graph store" {:cfg cfg})
            (->DatahikeStore (atom nil) cfg))))

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
