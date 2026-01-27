(ns hive-mcp.knowledge-graph.store.datascript
  "DataScript implementation of IGraphStore protocol.

   In-memory Datalog store. Fast, no persistence, ideal for tests
   and the default backend.

   CLARITY-T: Logs backend selection on initialization."
  (:require [datascript.core :as d]
            [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.schema :as schema]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord DataScriptStore [conn-atom]
  proto/IGraphStore

  (ensure-conn! [_this]
    (when (nil? @conn-atom)
      (log/info "Initializing DataScript KG store (in-memory)")
      (reset! conn-atom (d/create-conn (schema/full-schema))))
    @conn-atom)

  (transact! [this tx-data]
    (d/transact! (proto/ensure-conn! this) tx-data))

  (query [this q]
    (d/q q @(proto/ensure-conn! this)))

  (query [this q inputs]
    (apply d/q q @(proto/ensure-conn! this) inputs))

  (entity [this eid]
    (d/entity @(proto/ensure-conn! this) eid))

  (entid [this lookup-ref]
    (d/entid @(proto/ensure-conn! this) lookup-ref))

  (pull-entity [this pattern eid]
    (d/pull @(proto/ensure-conn! this) pattern eid))

  (db-snapshot [this]
    @(proto/ensure-conn! this))

  (reset-conn! [_this]
    (log/debug "Resetting DataScript KG store")
    (reset! conn-atom (d/create-conn (schema/full-schema)))
    @conn-atom)

  (close! [_this]
    ;; No-op for DataScript (in-memory, nothing to close)
    nil))

(defn create-store
  "Create a new DataScript-backed graph store.
   Returns an IGraphStore implementation."
  []
  (log/info "Creating DataScript graph store (in-memory)")
  (->DataScriptStore (atom nil)))
