(ns hive-mcp.knowledge-graph.store.datascript
  "DataScript implementation of IKGStore protocol.

   In-memory Datalog store. Fast, no persistence, ideal for tests
   and the default backend.

  (:require [datascript.core :as d]
            [hive-mcp.protocols.kg :as kg]
            [hive-mcp.knowledge-graph.schema :as schema]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord DataScriptStore [conn-atom]
  kg/IKGStore

  (ensure-conn! [_this]
    (when (nil? @conn-atom)
      (log/info "Initializing DataScript KG store (in-memory)")
      (reset! conn-atom (d/create-conn (schema/full-schema))))
    @conn-atom)

  (transact! [this tx-data]
    (d/transact! (kg/ensure-conn! this) tx-data))

  (query [this q]
    (d/q q @(kg/ensure-conn! this)))

  (query [this q inputs]
    (apply d/q q @(kg/ensure-conn! this) inputs))

  (entity [this eid]
    (d/entity @(kg/ensure-conn! this) eid))

  (entid [this lookup-ref]
    (d/entid @(kg/ensure-conn! this) lookup-ref))

  (pull-entity [this pattern eid]
    (d/pull @(kg/ensure-conn! this) pattern eid))

  (db-snapshot [this]
    @(kg/ensure-conn! this))

  (reset-conn! [_this]
    (log/debug "Resetting DataScript KG store")
    (reset! conn-atom (d/create-conn (schema/full-schema)))
    @conn-atom)

  (close! [_this]
    ;; No-op for DataScript (in-memory, nothing to close)
    nil))

(defn create-store
  "Create a new DataScript-backed graph store.
   Returns an IKGStore implementation."
  []
  (log/info "Creating DataScript graph store (in-memory)")
  (->DataScriptStore (atom nil)))
