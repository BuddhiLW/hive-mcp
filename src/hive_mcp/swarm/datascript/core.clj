(ns hive-mcp.swarm.datascript.core
  "Facade for DataScript swarm state management.

   Re-exports all public functions from sub-modules for backward compatibility.
   Import this namespace OR use the original hive-mcp.swarm.datascript ns.

   Sub-modules:
   - schema      - Schema definitions and status enums
   - connection  - Connection lifecycle and helpers
   - lings       - Slave/Task/Claim CRUD operations
   - queries     - Read-only query functions
   - coordination - Coordinators/Wrap/Plans/Waves

   SOLID-D: Dependency Inversion - clients depend on this facade.
   GoF: Facade pattern for simplified access to complex subsystem."
  (:require [hive-mcp.swarm.datascript.schema :as schema]
            [hive-mcp.swarm.datascript.connection :as connection]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.datascript.coordination :as coordination]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Re-exports from schema
;;; =============================================================================

(def slave-statuses schema/slave-statuses)
(def task-statuses schema/task-statuses)
(def coordinator-statuses schema/coordinator-statuses)
(def plan-statuses schema/plan-statuses)
(def item-statuses schema/item-statuses)
(def wave-statuses schema/wave-statuses)
(def critical-op-types schema/critical-op-types)
(def schema schema/schema)

;;; =============================================================================
;;; Re-exports from connection
;;; =============================================================================

(def create-conn connection/create-conn)
(def get-conn connection/get-conn)
(def reset-conn! connection/reset-conn!)
(def ensure-conn connection/ensure-conn)
(def now connection/now)
(def gen-id connection/gen-id)

;;; =============================================================================
;;; Re-exports from lings
;;; =============================================================================

;; Slave operations
(def add-slave! lings/add-slave!)
(def update-slave! lings/update-slave!)
(def remove-slave! lings/remove-slave!)

;; Critical ops guard
(def enter-critical-op! lings/enter-critical-op!)
(def exit-critical-op! lings/exit-critical-op!)
(def get-critical-ops lings/get-critical-ops)
(def can-kill? lings/can-kill?)
;; Note: with-critical-op is a macro - users must import directly from lings

;; Task operations
(def add-task! lings/add-task!)
(def complete-task! lings/complete-task!)
(def fail-task! lings/fail-task!)
(def update-task! lings/update-task!)

;; Claim operations
(def claim-file! lings/claim-file!)
(def release-claim! lings/release-claim!)
(def release-claims-for-slave! lings/release-claims-for-slave!)
(def release-claims-for-task! lings/release-claims-for-task!)

;;; =============================================================================
;;; Re-exports from queries
;;; =============================================================================

;; Slave queries
(def get-slave queries/get-slave)
(def get-all-slaves queries/get-all-slaves)
(def get-slaves-by-status queries/get-slaves-by-status)

;; Task queries
(def get-task queries/get-task)
(def get-tasks-for-slave queries/get-tasks-for-slave)
(def get-completed-tasks queries/get-completed-tasks)

;; Claim queries
(def get-claims-for-file queries/get-claims-for-file)
(def get-all-claims queries/get-all-claims)
(def has-conflict? queries/has-conflict?)
(def check-file-conflicts queries/check-file-conflicts)

;; Stats/debug
(def db-stats queries/db-stats)
(def dump-db queries/dump-db)

;;; =============================================================================
;;; Re-exports from coordination
;;; =============================================================================

;; Wrap queue
(def add-wrap-notification! coordination/add-wrap-notification!)
(def get-unprocessed-wraps coordination/get-unprocessed-wraps)
(def mark-wrap-processed! coordination/mark-wrap-processed!)

;; Plans
(def create-plan! coordination/create-plan!)
(def get-plan coordination/get-plan)
(def get-pending-items coordination/get-pending-items)
(def get-plan-items coordination/get-plan-items)
(def update-item-status! coordination/update-item-status!)
(def update-plan-status! coordination/update-plan-status!)

;; Waves
(def create-wave! coordination/create-wave!)
(def get-wave coordination/get-wave)
(def update-wave-counts! coordination/update-wave-counts!)
(def complete-wave! coordination/complete-wave!)

;; Coordinators
(def register-coordinator! coordination/register-coordinator!)
(def update-heartbeat! coordination/update-heartbeat!)
(def get-coordinator coordination/get-coordinator)
(def get-all-coordinators coordination/get-all-coordinators)
(def get-coordinators-by-status coordination/get-coordinators-by-status)
(def get-coordinators-for-project coordination/get-coordinators-for-project)
(def mark-coordinator-terminated! coordination/mark-coordinator-terminated!)
(def mark-coordinator-stale! coordination/mark-coordinator-stale!)
(def cleanup-stale-coordinators! coordination/cleanup-stale-coordinators!)
(def remove-coordinator! coordination/remove-coordinator!)

;; Completed tasks (session-scoped registry)
(def register-completed-task! coordination/register-completed-task!)
(def get-completed-task coordination/get-completed-task)
(def get-completed-tasks-this-session coordination/get-completed-tasks-this-session)
(def clear-completed-tasks! coordination/clear-completed-tasks!)
