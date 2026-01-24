(ns hive-mcp.swarm.datascript.registry
  "DataScript implementation of ISwarmRegistry protocol.

   Wraps the existing module-level functions from lings.clj and queries.clj
   to provide a protocol-based interface for dependency injection.

   SOLID-D: Dependency Inversion - clients depend on ISwarmRegistry abstraction.
   DDD: Repository pattern with DataScript backend."
  (:require [hive-mcp.swarm.protocol :as proto]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.datascript.queries :as queries]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord DataScriptRegistry []
  proto/ISwarmRegistry

  ;;; Slave Operations

  (add-slave! [_this slave-id opts]
    (lings/add-slave! slave-id opts))

  (get-slave [_this slave-id]
    (queries/get-slave slave-id))

  (update-slave! [_this slave-id updates]
    (lings/update-slave! slave-id updates))

  (remove-slave! [_this slave-id]
    (lings/remove-slave! slave-id))

  (get-all-slaves [_this]
    (queries/get-all-slaves))

  (get-slaves-by-status [_this status]
    (queries/get-slaves-by-status status))

  (get-slaves-by-project [_this project-id]
    (queries/get-slaves-by-project project-id))

  ;;; Task Operations

  (add-task! [_this task-id slave-id opts]
    (lings/add-task! task-id slave-id opts))

  (get-task [_this task-id]
    (queries/get-task task-id))

  (update-task! [_this task-id updates]
    (lings/update-task! task-id updates))

  (get-tasks-for-slave [_this slave-id]
    (queries/get-tasks-for-slave slave-id))

  (get-tasks-for-slave [_this slave-id status]
    (queries/get-tasks-for-slave slave-id status)))

;;; =============================================================================
;;; Default Registry Instance
;;; =============================================================================

(defonce ^{:doc "Default DataScript-backed registry instance.
                 Use this for protocol-based access to swarm state."}
  default-registry
  (->DataScriptRegistry))

(defn get-default-registry
  "Get the default DataScript registry instance.
   Provided for cases where direct var reference isn't suitable."
  []
  default-registry)
