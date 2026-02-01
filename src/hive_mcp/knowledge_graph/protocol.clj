(ns hive-mcp.knowledge-graph.protocol
  "Protocol definition for Knowledge Graph storage backends.

   Abstracts the Datalog store interface so that both DataScript (in-memory)
   and Datalevin (persistent) can be used interchangeably.

   CLARITY-L: Layers stay pure - protocol is the boundary between
   KG domain logic and storage implementation.
   CLARITY-I: Inputs guarded at protocol boundary.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IGraphStore
  "Storage backend protocol for Knowledge Graph.

   All KG modules (edges, disc, queries) should use this protocol
   instead of calling datascript.core or datalevin.core directly.

   Implementations:
   - DataScriptStore: In-memory, fast, used for tests and default
   - DatalevinStore: Persistent to disk via LMDB, used for production"

  (ensure-conn! [this]
    "Ensure the connection is initialized. Creates if nil.
     Returns the raw connection object (backend-specific).")

  (transact! [this tx-data]
    "Transact data into the store.
     tx-data is a vector of maps or transaction commands.
     Returns the transaction report.")

  (query [this q] [this q inputs]
    "Execute a Datalog query against the current DB snapshot.
     q - query form (vector or map)
     inputs - additional query inputs after the DB
     Returns query results.")

  (entity [this eid]
    "Get an entity by its entity ID.
     Returns the entity map or nil.")

  (entid [this lookup-ref]
    "Resolve a lookup ref to an entity ID.
     lookup-ref - e.g. [:kg-edge/id \"some-id\"]
     Returns entity ID (integer) or nil.")

  (pull-entity [this pattern eid]
    "Pull an entity with a pull pattern.
     pattern - pull pattern e.g. '[*]'
     eid - entity ID (integer)
     Returns pulled entity map.")

  (db-snapshot [this]
    "Get the current database snapshot value.
     Returns the immutable DB value.")

  (reset-conn! [this]
    "Reset the connection to a fresh/empty database.
     Used for testing and state clearing.
     Returns the new connection.")

  (close! [this]
    "Close the connection and release resources.
     No-op for in-memory backends.
     Required for Datalevin to flush LMDB."))

;; =============================================================================
;; Active Store Management
;; =============================================================================

;; Atom holding the currently active IGraphStore implementation.
(defonce ^:private active-store (atom nil))

(defn set-store!
  "Set the active graph store implementation.
   Called during system initialization."
  [store]
  {:pre [(satisfies? IGraphStore store)]}
  (reset! active-store store))

(defn get-store
  "Get the active graph store.
   Throws if no store has been set."
  []
  (or @active-store
      (throw (ex-info "No graph store configured. Call set-store! first."
                      {:hint "Initialize with datascript-store or datalevin-store"}))))

(defn store-set?
  "Check if a store has been configured."
  []
  (some? @active-store))

;; =============================================================================
;; Temporal Store Protocol (Optional Extension)
;; =============================================================================

(defprotocol ITemporalGraphStore
  "Extended protocol for stores that support temporal queries.

   Datahike implements this protocol for time-travel capabilities.
   DataScript and Datalevin do not support this and should return nil.

   Temporal queries enable:
   - Auditing: See what was known at a point in time
   - Debugging: Understand how knowledge evolved
   - Rollback: Query past states without data loss"

  (history-db [this]
    "Get a database containing all historical facts.
     Returns a DB value that includes retracted datoms, enabling
     queries over the complete history of the store.
     Returns nil if not supported.")

  (as-of-db [this tx-or-time]
    "Get the database as of a specific point in time.
     tx-or-time can be:
       - A transaction ID (integer)
       - A java.util.Date timestamp
     Returns nil if not supported.")

  (since-db [this tx-or-time]
    "Get a database containing only facts added since a point in time.
     tx-or-time can be:
       - A transaction ID (integer)
       - A java.util.Date timestamp
     Returns nil if not supported."))

(defn temporal-store?
  "Check if the current store supports temporal queries.
   Returns true if the store implements ITemporalGraphStore."
  [store]
  (satisfies? ITemporalGraphStore store))
