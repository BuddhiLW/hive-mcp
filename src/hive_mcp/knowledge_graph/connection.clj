(ns hive-mcp.knowledge-graph.connection
  "DataScript connection management for Knowledge Graph.

   Maintains a separate DataScript database for KG edges, disc entities,
   and knowledge abstraction tracking, following bounded context pattern
   (separate from Chroma memory storage)."
  (:require [datascript.core :as d]
            [hive-mcp.knowledge-graph.schema :as schema]))

;; Atom holding the DataScript connection for Knowledge Graph.
;; Initialized lazily via ensure-conn!
(defonce conn (atom nil))

(defn create-conn
  "Create a new DataScript connection with full KG schema.
   Includes edge, knowledge, and disc schemas.
   Returns the connection."
  []
  (d/create-conn (schema/full-schema)))

(defn reset-conn!
  "Reset the connection to a fresh database.
   Useful for testing or clearing state."
  []
  (reset! conn (create-conn)))

(defn ensure-conn!
  "Ensure connection is initialized. Creates if nil.
   Returns the connection."
  []
  (when (nil? @conn)
    (reset-conn!))
  @conn)

(defn get-conn
  "Get the current connection, initializing if needed.
   Preferred entry point for accessing the KG database."
  []
  (ensure-conn!))

(defn transact!
  "Transact data to the KG database.
   Wraps datascript/transact! with connection management."
  [tx-data]
  (d/transact! (get-conn) tx-data))

(defn query
  "Query the KG database.
   Wraps datascript/q with current database snapshot."
  [q & inputs]
  (apply d/q q @(get-conn) inputs))

;; Alias for ensure-conn! without the bang (for backward compatibility)
(def ensure-conn ensure-conn!)

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
