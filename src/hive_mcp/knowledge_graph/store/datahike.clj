(ns hive-mcp.knowledge-graph.store.datahike
  "Datahike implementation of IGraphStore protocol.

   Immutable, content-addressable Datalog store with time-travel capabilities.
   Data is persistent and supports branching/merging for multi-ling exploration.

   Part of the replikativ ecosystem - pairs with yggdrasil for versioning.

   Schema uses DataScript format directly (no translation needed unlike Datalevin).

   CLARITY-I: Validates config before connecting.
   CLARITY-T: Logs backend selection, config, schema.
   CLARITY-Y: Falls back to DataScript with warning if Datahike fails."
  (:require [datahike.api :as d]
            [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.schema :as schema]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private default-db-path "data/kg/datahike")

(defn- make-config
  "Create Datahike configuration map.

   Arguments:
     opts - Optional map with:
       :db-path   - Path for file-based storage (default: data/kg/datahike)
       :backend   - Storage backend (:file, :mem) (default: :file)
       :index     - Index type (:datahike.index/persistent-set) (default)
       :id        - Store identifier (default: auto-generated UUID)

   Returns Datahike config map."
  [& [{:keys [db-path backend index id]
       :or {db-path default-db-path
            backend :file
            index :datahike.index/persistent-set}}]]
  (let [store-id (or id (java.util.UUID/randomUUID))]
    (case backend
      :file {:store {:backend :file
                     :path db-path
                     :id store-id}
             :schema-flexibility :read
             :index index}
      ;; :mem/:memory both map to :memory (Datahike's in-memory backend)
      (:mem :memory) {:store {:backend :memory
                              :id store-id}
                      :schema-flexibility :read
                      :index index}
      ;; Default to file
      {:store {:backend :file
               :path db-path
               :id store-id}
       :schema-flexibility :read
       :index index})))

;; =============================================================================
;; Input Validation (CLARITY-I)
;; =============================================================================

(defn- validate-config!
  "Validate Datahike configuration.
   Creates parent directories for file backend if needed.
   Throws on invalid config."
  [cfg]
  (when-not (map? cfg)
    (throw (ex-info "Datahike config must be a map" {:cfg cfg})))
  (when-not (get-in cfg [:store :backend])
    (throw (ex-info "Datahike config missing :store :backend" {:cfg cfg})))

  ;; Create parent directory for file backend
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

;; =============================================================================
;; Datahike Store Implementation
;; =============================================================================

(defrecord DatahikeStore [conn-atom cfg]
  proto/IGraphStore

  (ensure-conn! [_this]
    (when (nil? @conn-atom)
      (log/info "Initializing Datahike KG store" {:cfg cfg})
      (validate-config! cfg)
      (let [dh-schema (schema/full-schema)]
        (log/debug "Datahike schema" {:attributes (count dh-schema)})
        ;; Create database if it doesn't exist
        ;; With :schema-flexibility :read, Datahike accepts any attributes
        ;; Schema can be transacted later for indexing/unique constraints
        (when-not (d/database-exists? cfg)
          (log/info "Creating new Datahike database" {:cfg cfg})
          (d/create-database cfg))
        ;; Connect to database
        (reset! conn-atom (d/connect cfg))))
    @conn-atom)

  (transact! [this tx-data]
    (d/transact (proto/ensure-conn! this) tx-data))

  (query [this q]
    (d/q q (d/db (proto/ensure-conn! this))))

  (query [this q inputs]
    (apply d/q q (d/db (proto/ensure-conn! this)) inputs))

  (entity [this eid]
    (d/entity (d/db (proto/ensure-conn! this)) eid))

  (entid [this lookup-ref]
    ;; Datahike doesn't have entid like DataScript, so we query for it
    (let [[attr val] lookup-ref
          results (d/q '[:find ?e .
                         :in $ ?attr ?val
                         :where [?e ?attr ?val]]
                       (d/db (proto/ensure-conn! this))
                       attr val)]
      results))

  (pull-entity [this pattern eid]
    (d/pull (d/db (proto/ensure-conn! this)) pattern eid))

  (db-snapshot [this]
    (d/db (proto/ensure-conn! this)))

  (reset-conn! [this]
    (log/info "Resetting Datahike KG store" {:cfg cfg})
    ;; Release existing connection if open
    (when-let [c @conn-atom]
      (try
        (d/release c)
        (catch Exception e
          (log/warn "Failed to release Datahike conn during reset"
                    {:error (.getMessage e)}))))
    ;; Delete and recreate database
    (when (d/database-exists? cfg)
      (d/delete-database cfg))
    (reset! conn-atom nil)
    (proto/ensure-conn! this))

  (close! [_this]
    (when-let [c @conn-atom]
      (log/info "Closing Datahike KG store" {:cfg cfg})
      (try
        (d/release c)
        (catch Exception e
          (log/warn "Failed to release Datahike connection"
                    {:error (.getMessage e)})))
      (reset! conn-atom nil)))

  ;; Temporal Store Protocol - Datahike supports time-travel queries
  proto/ITemporalGraphStore

  (history-db [this]
    (d/history (d/db (proto/ensure-conn! this))))

  (as-of-db [this tx-or-time]
    (d/as-of (d/db (proto/ensure-conn! this)) tx-or-time))

  (since-db [this tx-or-time]
    (d/since (d/db (proto/ensure-conn! this)) tx-or-time)))

;; =============================================================================
;; Store Factory
;; =============================================================================

(defn create-store
  "Create a new Datahike-backed graph store.

   Arguments:
     opts - Optional map with:
       :db-path - Path for file storage (default: data/kg/datahike)
       :backend - :file or :mem (default: :file)
       :index   - Index type (default: :datahike.index/persistent-set)

   Returns an IGraphStore implementation.

   CLARITY-Y: If Datahike fails to initialize, logs warning
   and returns nil (caller should fall back to DataScript)."
  [& [opts]]
  (try
    (let [cfg (make-config opts)]
      (log/info "Creating Datahike graph store" {:cfg cfg})
      (->DatahikeStore (atom nil) cfg))
    (catch Exception e
      (log/error "Failed to create Datahike store, falling back to DataScript"
                 {:error (.getMessage e) :opts opts})
      nil)))

;; =============================================================================
;; Temporal Query Extensions (Datahike-specific)
;; =============================================================================

(defn history-db
  "Get full history database for temporal queries.
   Returns a DB value that includes all historical facts."
  [store]
  (d/history (d/db (proto/ensure-conn! store))))

(defn as-of-db
  "Get database as of a specific transaction ID or timestamp.
   tx-or-time can be:
     - Transaction ID (integer)
     - java.util.Date instance"
  [store tx-or-time]
  (d/as-of (d/db (proto/ensure-conn! store)) tx-or-time))

(defn since-db
  "Get database with only facts added since a transaction ID or timestamp.
   tx-or-time can be:
     - Transaction ID (integer)
     - java.util.Date instance"
  [store tx-or-time]
  (d/since (d/db (proto/ensure-conn! store)) tx-or-time))

(defn query-history
  "Query against the full history database.
   Useful for auditing and change tracking."
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
