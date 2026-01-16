(ns hive-mcp.graph.protocol
  "GraphStore protocol for knowledge graph storage.
   
   Provides a unified interface for graph-based persistence,
   enabling multiple backend implementations:
   - Datascript (in-memory, persistent via serialization)
   - Datomic (cloud or on-prem, full history)
   - XTDB (bitemporality, document-centric)
   
   SOLID: Interface Segregation - focused graph operations only.
   DDD: Port in hexagonal architecture for persistence adapter.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(defprotocol GraphStore
  "Interface for knowledge graph storage.
   
   Implementations: Datascript, Datomic, XTDB.
   
   All methods should be thread-safe. Write operations (transact!, persist!)
   may block. Read operations (query, entity, find-similar) should be fast
   and preferably non-blocking."

  (transact! [this tx-data]
    "Write facts to the graph.
     
     tx-data follows Datomic/Datascript transaction format:
     - Maps: {:db/id -1 :friction/type :tool-missing ...}
     - Vectors: [:db/add eid attr value] or [:db/retract eid attr value]
     
     Returns {:tx-data [...] :tempids {...}} on success.
     Throws ex-info on schema violation or constraint error.")

  (query
    [this datalog-query]
    [this datalog-query args]
    "Execute Datalog query against the graph.
     
     datalog-query: Standard Datalog with :find, :where, :in clauses
     args: Positional args matching :in clause bindings (after $)
     
     Examples:
       (query store '[:find ?e :where [?e :friction/type :tool-missing]])
       (query store '[:find ?e :in $ ?type :where [?e :friction/type ?type]] :preset-gap)
     
     Returns set of tuples matching :find clause.")

  (entity [this eid]
    "Fetch entity by ID.
     
     eid: Entity ID (long) or lookup ref [:unique-attr value]
     
     Returns map of all attributes for entity, or nil if not found.
     For Datascript, this is the entity map.
     For Datomic/XTDB, may include metadata.")

  (find-similar [this entity-type content]
    "Find similar entries before creating new - for dedup.
     
     entity-type: Keyword like :friction, :knowledge, :agent
     content: String content to match (fuzzy/semantic if supported)
     
     Returns seq of {:eid ... :similarity ...} ordered by similarity desc.
     Empty seq if no similar entries found.
     
     Implementation may use:
     - Simple text matching (Datascript)
     - Full-text search (Datomic)
     - Vector similarity (XTDB with embeddings)")

  (history [this eid]
    "Temporal history for entity - implementation dependent.
     
     eid: Entity ID to get history for
     
     Returns seq of {:tx ... :t ... :changes [...]} or nil if not supported.
     - Datascript: Returns nil (no native history)
     - Datomic: Full transaction history
     - XTDB: Bitemporality (valid-time + transaction-time)")

  (persist! [this]
    "Durably save current graph state.
     
     Implementation dependent:
     - Datascript: Serialize to EDN/Transit file
     - Datomic: No-op (always durable)
     - XTDB: Flush to storage backend
     
     Returns {:persisted-at <instant>} on success.
     Throws on I/O error.")

  (restore! [this]
    "Load persisted state into graph.
     
     Replaces current in-memory state with persisted version.
     Safe to call on startup before any transactions.
     
     Returns {:restored-at <instant> :entity-count n} on success.
     Returns nil if no persisted state exists.
     Throws on corruption or version mismatch."))
