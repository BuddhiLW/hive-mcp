(ns hive-mcp.tools.swarm.wave
  "Facade namespace for wave execution, delegating to decomposed modules."
  (:require [hive-mcp.tools.swarm.wave.domain :as domain]
            [hive-mcp.tools.swarm.wave.validation :as validation]
            [hive-mcp.tools.swarm.wave.execution :as execution]
            [hive-mcp.tools.swarm.wave.status :as status]
            [hive-mcp.tools.swarm.wave.handlers :as handlers]
            [hive-mcp.swarm.datascript :as ds]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const default-concurrency domain/default-concurrency)
(def ^:const drone-timeout-ms domain/drone-timeout-ms)
(def ^:const keepalive-interval-ms domain/keepalive-interval-ms)

(def ensure-parent-dirs! validation/ensure-parent-dirs!)
(def validate-task-paths validation/validate-task-paths)

(def create-plan! handlers/create-plan!)

(defn get-pending-items
  "Get pending items for a plan."
  [plan-id]
  (ds/get-pending-items plan-id))

(defn get-plan-items
  "Get all items for a plan."
  [plan-id]
  (ds/get-plan-items plan-id))

(def execute-wave! execution/execute-wave!)
(def execute-wave-async! execution/execute-wave-async!)
(def cancel-wave! execution/cancel-wave!)

(def get-wave-status status/get-wave-status)
(def get-plan-status status/get-plan-status)

(def handle-get-wave-status handlers/handle-get-wave-status)
(def handle-dispatch-drone-wave handlers/handle-dispatch-drone-wave)
