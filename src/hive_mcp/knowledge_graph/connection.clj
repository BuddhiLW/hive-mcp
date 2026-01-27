(ns hive-mcp.knowledge-graph.connection
  "Connection management and factory for Knowledge Graph.

   Delegates to the active IGraphStore implementation (DataScript or Datalevin).
   Maintains backward-compatible API surface for existing KG modules.

   Backend selection (priority):
   1. Explicit set-backend! call
   2. HIVE_KG_BACKEND env var (:datascript | :datalevin)
   3. :kg-backend in .hive-project.edn
   4. Default: :datascript (safe, no behavior change)

   CLARITY-T: Logs backend selection on initialization.
   CLARITY-Y: Auto-initializes DataScript if no store configured."
  (:require [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Config-based Backend Auto-detection
;; =============================================================================

(defn- read-project-config
  "Read :kg-backend from .hive-project.edn in the current directory.
   Returns keyword (:datascript, :datalevin) or nil."
  []
  (try
    (let [f (io/file ".hive-project.edn")]
      (when (.exists f)
        (-> f slurp edn/read-string :kg-backend)))
    (catch Exception e
      (log/debug "Failed to read .hive-project.edn for KG backend"
                 {:error (.getMessage e)})
      nil)))

(defn- detect-backend
  "Detect the desired KG backend from configuration sources.
   Priority: env var > .hive-project.edn > default (:datascript).
   Returns keyword :datascript or :datalevin."
  []
  (let [env-val (System/getenv "HIVE_KG_BACKEND")
        env-backend (when (and env-val (seq env-val))
                      (keyword env-val))
        project-backend (read-project-config)
        backend (or env-backend project-backend :datascript)]
    (log/debug "KG backend detection" {:env env-backend
                                       :project project-backend
                                       :selected backend})
    backend))

(defn- ensure-store!
  "Ensure a store is configured. Auto-detects backend from config.
   CLARITY-Y: Falls back to DataScript on any failure."
  []
  (when-not (proto/store-set?)
    (let [backend (detect-backend)]
      (log/info "Auto-initializing KG backend" {:backend backend})
      (case backend
        :datalevin
        (try
          (require 'hive-mcp.knowledge-graph.store.datalevin)
          (let [create-fn (resolve 'hive-mcp.knowledge-graph.store.datalevin/create-store)
                store (create-fn)]
            (if store
              (proto/set-store! store)
              (do
                (log/warn "Datalevin store creation returned nil, falling back to DataScript")
                (proto/set-store! (ds-store/create-store)))))
          (catch Exception e
            (log/warn "Failed to initialize Datalevin, falling back to DataScript"
                      {:error (.getMessage e)})
            (proto/set-store! (ds-store/create-store))))

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
     backend - :datascript or :datalevin
     opts    - Backend-specific options:
               :datalevin {:db-path \"data/kg/datalevin\"}

   CLARITY-T: Logs backend selection."
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
        ;; CLARITY-Y: Fall back to DataScript if Datalevin fails
        (do
          (log/warn "Datalevin store creation failed, falling back to DataScript")
          (proto/set-store! (ds-store/create-store)))))

    ;; Unknown backend
    (throw (ex-info "Unknown KG backend" {:backend backend
                                          :valid #{:datascript :datalevin}}))))

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
