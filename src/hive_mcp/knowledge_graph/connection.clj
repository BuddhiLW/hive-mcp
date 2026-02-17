(ns hive-mcp.knowledge-graph.connection
  "Connection management and factory for Knowledge Graph.

   Delegates to the active IGraphStore implementation (DataScript, Datalevin, Datascript, Neo4J etc).
   Maintains backward-compatible API surface for existing KG modules.

   Backend selection (priority):
   1. Explicit set-backend! call
   2. HIVE_KG_BACKEND env var (:datascript | :datalevin)
   3. :kg-backend in .hive-project.edn
   4. Default: :datalevin (persistent — compounding axiom)"

  (:require [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.knowledge-graph.scope :as scope]
            [hive-mcp.config.core :as config]
            [hive-dsl.result :as r]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Config-based Backend Auto-detection
;; =============================================================================

(defn- walk-hierarchy-for-kg-backend
  "Walk up .hive-project.edn hierarchy to find :kg-backend.
   Parent is more authoritative than child — first match walking UP wins.
   Returns keyword or nil."
  []
  (r/rescue nil
            (let [cwd (System/getProperty "user.dir")
                  home (System/getProperty "user.home")]
              (loop [dir (io/file cwd)
               ;; Collect configs child→parent, then reverse for parent-first
                     configs []]
                (cond
                  (nil? dir) nil
                  (= (.getAbsolutePath dir) home)
            ;; Check home dir then stop
                  (let [all-configs (if-let [cfg (scope/read-direct-project-config (.getAbsolutePath dir))]
                                      (conj configs cfg)
                                      configs)
                  ;; Parent-first: last found = most ancestral = highest authority
                        parent-first (reverse all-configs)]
                    (some :kg-backend parent-first))

                  :else
                  (let [cfg (scope/read-direct-project-config (.getAbsolutePath dir))]
                    (recur (.getParentFile dir)
                           (if cfg (conj configs cfg) configs))))))))

(defn- detect-backend
  "Detect the desired KG backend from configuration sources.

   Priority (highest → lowest):
   1. .hive-project.edn hierarchy (parent > child > grandchild)
   2. HIVE_KG_BACKEND env var (explicit override)
   3. config.edn :services.kg.backend (global default)
   4. Fallback: :datalevin

   Rationale: project hierarchy is ground truth (lives with code),
   config.edn is user-global preference, env var is session override."
  []
  (let [hierarchy-backend (walk-hierarchy-for-kg-backend)
        env-backend (some-> (System/getenv "HIVE_KG_BACKEND") keyword)
        config-backend (config/get-service-value :kg :backend :parse keyword)
        backend (or hierarchy-backend env-backend config-backend :datalevin)]
    (log/info "KG backend detection"
              {:hierarchy hierarchy-backend
               :env env-backend
               :config config-backend
               :selected backend})
    backend))

(defn- ensure-store!
  "Ensure a store is configured. Auto-detects backend from config."
  []
  (when-not (proto/store-set?)
    (let [backend (detect-backend)]
      (log/info "Auto-initializing KG backend" {:backend backend})
      (case backend
        :datalevin
        (let [store (r/guard Exception nil
                             (require 'hive-mcp.knowledge-graph.store.datalevin)
                             (let [create-fn (resolve 'hive-mcp.knowledge-graph.store.datalevin/create-store)]
                               (create-fn)))]
          (if store
            (proto/set-store! store)
            (do
              (log/warn "Failed to initialize Datalevin, falling back to DataScript")
              (proto/set-store! (ds-store/create-store)))))

        :datahike
        (let [store (r/guard Exception nil
                             (require 'hive-mcp.knowledge-graph.store.datahike)
                             (let [create-fn (resolve 'hive-mcp.knowledge-graph.store.datahike/create-store)]
                               (create-fn)))]
          (if store
            (proto/set-store! store)
            (do
              (log/warn "Failed to initialize Datahike, falling back to DataScript")
              (proto/set-store! (ds-store/create-store)))))

        ;; Default: DataScript
        (proto/set-store! (ds-store/create-store)))))
  (proto/get-store))

;; =============================================================================
;; Backward-Compatible API
;; =============================================================================

(defn get-conn
  "Get the current connection, initializing if needed.
   Preferred entry point for accessing the KG database.
   Returns the raw backend connection."
  []
  (proto/ensure-conn! (ensure-store!)))

(defn ensure-conn!
  "Ensure connection is initialized. Creates if nil.
   Returns the connection."
  []
  (get-conn))

;; Alias for ensure-conn! without the bang (for backward compatibility)
(def ensure-conn ensure-conn!)

(defn reset-conn!
  "Reset the connection to a fresh database.
   Useful for testing or clearing state."
  []
  (proto/reset-conn! (ensure-store!)))

(defn transact!
  "Transact data to the KG database.
   Delegates to the active store."
  [tx-data]
  (proto/transact! (ensure-store!) tx-data))

(defn query
  "Query the KG database.
   Delegates to the active store."
  [q & inputs]
  (if (seq inputs)
    (proto/query (ensure-store!) q inputs)
    (proto/query (ensure-store!) q)))

(defn entity
  "Get an entity by ID from the KG database.
   Delegates to the active store."
  [eid]
  (proto/entity (ensure-store!) eid))

(defn entid
  "Resolve a lookup ref to an entity ID.
   Delegates to the active store."
  [lookup-ref]
  (proto/entid (ensure-store!) lookup-ref))

(defn pull-entity
  "Pull an entity with a pattern.
   Delegates to the active store."
  [pattern eid]
  (proto/pull-entity (ensure-store!) pattern eid))

(defn db-snapshot
  "Get the current database snapshot.
   Delegates to the active store."
  []
  (proto/db-snapshot (ensure-store!)))

(defn close!
  "Close the active store connection.
   Required for Datalevin to flush LMDB."
  []
  (when (proto/store-set?)
    (proto/close! (proto/get-store))))

;; =============================================================================
;; Store Configuration
;; =============================================================================

(defn set-backend!
  "Configure the KG storage backend.

   Arguments:
     backend - :datascript, :datalevin, or :datahike
     opts    - Backend-specific options:
               :datalevin {:db-path \"data/kg/datalevin\"}
               :datahike  {:db-path \"data/kg/datahike\" :backend :file}"

  [backend & [opts]]
  (log/info "Setting KG backend" {:backend backend :opts opts})
  (case backend
    :datascript
    (proto/set-store! (ds-store/create-store))

    :datalevin
    (let [;; Require datalevin store dynamically to avoid hard dep
          _ (require 'hive-mcp.knowledge-graph.store.datalevin)
          create-fn (resolve 'hive-mcp.knowledge-graph.store.datalevin/create-store)
          store (create-fn opts)]
      (if store
        (proto/set-store! store)
        (do
          (log/warn "Datalevin store creation failed, falling back to DataScript")
          (proto/set-store! (ds-store/create-store)))))

    :datahike
    (let [;; Require datahike store dynamically to avoid hard dep
          _ (require 'hive-mcp.knowledge-graph.store.datahike)
          create-fn (resolve 'hive-mcp.knowledge-graph.store.datahike/create-store)
          store (create-fn opts)]
      (if store
        (proto/set-store! store)
        (do
          (log/warn "Datahike store creation failed, falling back to DataScript")
          (proto/set-store! (ds-store/create-store)))))

    ;; Unknown backend
    (throw (ex-info "Unknown KG backend" {:backend backend
                                          :valid #{:datascript :datalevin :datahike}}))))

;; =============================================================================
;; ID and Timestamp Utilities
;; =============================================================================

(defn gen-edge-id
  "Generate a unique edge ID with timestamp prefix.
   Format: edge-yyyyMMddTHHmmss-XXXXXX
   The timestamp prefix enables chronological sorting."
  []
  (let [now (java.time.LocalDateTime/now)
        formatter (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss")
        timestamp (.format now formatter)
        random-hex (format "%06x" (rand-int 0xFFFFFF))]
    (str "edge-" timestamp "-" random-hex)))

(defn now
  "Return current timestamp as java.util.Date.
   Convenience for edge :created-at fields."
  []
  (java.util.Date.))

;; =============================================================================
;; Temporal Query Facade (W3)
;; =============================================================================

(defn temporal-store?
  "Check if the current store supports temporal queries (time-travel).
   Returns true for Datahike, false for DataScript/Datalevin.

   Use this to guard temporal query calls in application code."
  []
  (proto/temporal-store? (ensure-store!)))

(defn history-db
  "Get a database containing all historical facts.

   Returns a DB value that includes retracted datoms, enabling
   queries over the complete history of the store.

   Returns nil if the store does not support temporal queries.

   Example:
     (when (temporal-store?)
       (query '[:find ?e ?attr ?v ?added
                :where [?e ?attr ?v _ ?added]]
              (history-db)))"
  []
  (let [store (ensure-store!)]
    (when (proto/temporal-store? store)
      (proto/history-db store))))

(defn as-of-db
  "Get the database as of a specific point in time.

   Arguments:
     tx-or-time - Transaction ID (integer) or java.util.Date timestamp

   Returns a DB value representing the state at that point,
   or nil if the store does not support temporal queries.

   Example:
     ;; Query state from 1 hour ago
     (as-of-db (java.util.Date. (- (System/currentTimeMillis) 3600000)))"
  [tx-or-time]
  (let [store (ensure-store!)]
    (when (proto/temporal-store? store)
      (proto/as-of-db store tx-or-time))))

(defn since-db
  "Get a database containing only facts added since a point in time.

   Arguments:
     tx-or-time - Transaction ID (integer) or java.util.Date timestamp

   Returns a DB value with only facts added after that point,
   or nil if the store does not support temporal queries.

   Useful for incremental change tracking and sync operations."
  [tx-or-time]
  (let [store (ensure-store!)]
    (when (proto/temporal-store? store)
      (proto/since-db store tx-or-time))))

(defn query-history
  "Query against the full history database.

   Arguments:
     q      - Datalog query
     inputs - Optional additional query inputs

   Returns query results against history DB, enabling queries
   that span all historical states (including retracted facts).

   Returns nil if the store does not support temporal queries.

   Example:
     ;; Find all values an attribute ever had
     (query-history '[:find ?v ?added
                      :in $ ?e ?attr
                      :where [?e ?attr ?v _ ?added]]
                    [:kg-edge/id \"some-id\"] :kg-edge/weight)"
  [q & inputs]
  (when-let [hdb (history-db)]
    ;; Dynamically require datahike.api to avoid hard dependency
    (require 'datahike.api)
    (if (seq inputs)
      (apply (resolve 'datahike.api/q) q hdb inputs)
      ((resolve 'datahike.api/q) q hdb))))

(defn query-as-of
  "Query the database as it was at a specific point in time.

   Arguments:
     tx-or-time - Transaction ID (integer) or java.util.Date timestamp
     q          - Datalog query
     inputs     - Optional additional query inputs

   Returns query results from the point-in-time snapshot,
   or nil if the store does not support temporal queries.

   Example:
     ;; What edges existed yesterday?
     (query-as-of yesterday
                  '[:find ?id
                    :where [?e :kg-edge/id ?id]])"
  [tx-or-time q & inputs]
  (when-let [aodb (as-of-db tx-or-time)]
    (require 'datahike.api)
    (if (seq inputs)
      (apply (resolve 'datahike.api/q) q aodb inputs)
      ((resolve 'datahike.api/q) q aodb))))
