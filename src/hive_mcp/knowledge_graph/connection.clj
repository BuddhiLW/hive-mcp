(ns hive-mcp.knowledge-graph.connection
  "DataScript connection management for Knowledge Graph.

   Maintains a separate DataScript database for KG edges,
   following bounded context pattern (separate from Chroma memory storage)."
  (:require [datascript.core :as d]
            [hive-mcp.knowledge-graph.schema :refer [kg-schema]]))

;; Atom holding the DataScript connection for Knowledge Graph.
;; Initialized lazily via ensure-conn!
(defonce conn (atom nil))

(defn create-conn
  "Create a new DataScript connection with KG schema.
   Returns the connection."
  []
  (d/create-conn kg-schema))

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
