(ns hive-mcp.swarm.registry
  "Backward-compatible wrappers for swarm registry operations.

   Migration strategy (ADR-004):
   =============================
   1. Phase 1 (Current): Wrappers call DataScript directly via ds/
   2. Phase 2: Wrappers call ISwarmRegistry implementation
   3. Phase 3: Consumers never change - only this file does

   This module provides a stable API layer between consumers and the underlying
   storage mechanism. When we switch from DataScript to Datomic/XTDB, only
   this file needs to change - all consumers continue to work unchanged.

   Pattern Documentation:
   =====================
   Each wrapper documents:
   - What ds/ call(s) it replaces
   - Which files currently use this pattern
   - ISwarmRegistry protocol method it will delegate to (Phase 2)

  (:require [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.tools.memory.scope :as scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Slave Read Operations
;;; =============================================================================

(defn get-slave
  "Get a slave by ID.

   Replaces: (ds/get-slave slave-id)

   Currently used by:
   - tools/swarm/state.clj:35        - get-slave-working-status
   - tools/swarm/lifecycle.clj:113   - check-kill-cross-project-guard
   - tools/swarm/health.clj:182      - recover-drone!
   - agent/drone.clj (implicit via proto/get-slave)

   Phase 2: Delegates to (proto/get-slave registry slave-id)

   Arguments:
     slave-id - String identifier for the slave

   Returns:
     Map with slave attributes (:slave/id, :slave/name, :slave/status, etc.)
     or nil if not found."
  [slave-id]
  (ds/get-slave slave-id))

(defn slave-exists?
  "Check if a slave is registered.

   Replaces: (some? (ds/get-slave slave-id))

   Currently used by:
   - hivemind.clj (implicit via proto/get-slave nil check)
   - tools/swarm/health.clj:182 - before remove

   Phase 2: Delegates to (some? (proto/get-slave registry slave-id))

   Arguments:
     slave-id - String identifier for the slave

   Returns:
     true if slave exists, false otherwise."
  [slave-id]
  (some? (ds/get-slave slave-id)))

(defn list-slaves
  "Get all slaves, optionally filtered.

   Replaces:
   - (ds/get-all-slaves)                        - no filter
   - (ds/get-slaves-by-status status)           - by status
   - (ds/get-slaves-by-project project-id)      - by project
   - (ds/get-slave-ids-by-project project-id)   - IDs only

   Currently used by:
   - tools/swarm/registry.clj:84                - get-available-lings
   - tools/swarm/lifecycle.clj:240              - kill-all project scoping
   - hivemind.clj (implicit via proto/get-all-slaves)

   Phase 2: Delegates to proto/get-all-slaves or proto/get-slaves-by-*

   Arguments:
     opts - Optional map with filter keys:
            :status     - Filter by status keyword (:idle, :busy, etc.)
            :project-id - Filter by project-id string
            :ids-only?  - If true, return only slave IDs (not full maps)

   Returns:
     Seq of slave maps (or slave-id strings if :ids-only? true).
     Empty seq if no matches."
  ([]
   (list-slaves {}))
  ([{:keys [status project-id ids-only?]}]
   (let [slaves (cond
                  status     (ds/get-slaves-by-status status)
                  project-id (ds/get-slaves-by-project project-id)
                  :else      (ds/get-all-slaves))]
     (if ids-only?
       (map :slave/id slaves)
       slaves))))

(defn get-slave-ids-by-project
  "Get slave IDs for a specific project.

   Replaces: (ds/get-slave-ids-by-project project-id)

   Currently used by:
   - tools/swarm/lifecycle.clj:240 - kill-all with project scoping

   Phase 2: Delegates to (map :slave/id (proto/get-slaves-by-project registry project-id))

   Arguments:
     project-id - Project identifier string

   Returns:
     Seq of slave-id strings belonging to the project."
  [project-id]
  (ds/get-slave-ids-by-project project-id))

;;; =============================================================================
;;; Slave Write Operations
;;; =============================================================================

(defn add-slave!
  "Add a new slave to the registry.

   Replaces: (ds/add-slave! slave-id opts)

   Currently used by:
   - tools/swarm/registry.clj:60     - register-ling!
   - agent/drone.clj:298             - register-drone-agent

   Phase 2: Delegates to (proto/add-slave! registry slave-id opts)

   Arguments:
     slave-id - Unique string identifier (required)
     opts     - Map with optional keys:
                :name       - Human-readable name (defaults to slave-id)
                :status     - Initial status keyword (default :idle)
                :depth      - Hierarchy depth integer (default 1)
                :parent     - Parent slave-id string
                :presets    - Collection of preset name strings
                :cwd        - Working directory path
                :project-id - Project ID (derived from :cwd if not provided)
                :kanban-task-id - Linked kanban task ID

   Returns:
     Transaction report with :tx-data on success."
  [slave-id opts]
  (let [;; Derive project-id from cwd if not explicitly provided
        cwd (:cwd opts)
        project-id (or (:project-id opts)
                       (when cwd (scope/get-current-project-id cwd)))]
    (ds/add-slave! slave-id (assoc opts :project-id project-id))))

(defn update-slave!
  "Update an existing slave's attributes.

   Replaces: (ds/update-slave! slave-id updates)

   Currently used by:
   - hivemind.clj (implicit via proto/update-slave!)
   - swarm/sync.clj (implicit via proto/update-slave!)

   Phase 2: Delegates to (proto/update-slave! registry slave-id updates)

   Arguments:
     slave-id - Slave to update
     updates  - Map of attributes to update (uses :slave/* keys)

   Returns:
     Transaction report on success, nil if slave not found."
  [slave-id updates]
  (ds/update-slave! slave-id updates))

(defn remove-slave!
  "Remove a slave from the registry.

   Replaces: (ds/remove-slave! slave-id)

   Currently used by:
   - tools/swarm/registry.clj:74     - unregister-ling!
   - tools/swarm/health.clj:183      - recover-drone!
   - agent/drone.clj:551             - cleanup on timeout/error

   Phase 2: Delegates to (proto/remove-slave! registry slave-id)

   Note: Also releases file claims held by this slave (via lings/remove-slave!).

   Arguments:
     slave-id - Slave to remove

   Returns:
     Transaction report on success, nil if slave not found."
  [slave-id]
  (ds/remove-slave! slave-id))

;;; =============================================================================
;;; Connection Management
;;; =============================================================================

(defn reset-conn!
  "Reset the DataScript connection (clear all state).

   Replaces: (ds/reset-conn!)

   Currently used by:
   - tools/swarm/registry.clj:97     - clear-registry!

   Phase 2: Will need backend-specific handling.
   For Datomic/XTDB, may transact retractions instead of conn reset.

   WARNING: Destructive operation - clears all swarm state.
   GUARDED: No-op if coordinator is running (see connection.clj).

   Returns:
     nil (side effect only)"
  []
  (conn/reset-conn!))

;;; =============================================================================
;;; Operational Checks (Extended Operations - not in ISwarmRegistry)
;;; =============================================================================

(defn can-kill?
  "Check if a slave can be safely killed.

   Replaces: (ds/can-kill? slave-id)

   Currently used by:
   - tools/swarm/lifecycle.clj:148   - handle-swarm-kill guard
   - tools/swarm/lifecycle.clj:179   - handle-swarm-kill guard

   Phase 2: May move to extended protocol or remain DataScript-specific.
   This is a complex operation involving critical-ops tracking.

   Arguments:
     slave-id - Slave to check

   Returns:
     Map with:
     - :can-kill?     - boolean, true if safe to kill
     - :blocking-ops  - seq of operations blocking kill (if any)"
  [slave-id]
  (ds/can-kill? slave-id))

;;; =============================================================================
;;; Task Operations (ISwarmRegistry methods)
;;; =============================================================================

(defn add-task!
  "Add a new task to the registry.

   Replaces: (ds/add-task! task-id slave-id opts)

   Currently used by:
   - swarm/sync.clj (implicit via proto/add-task!)

   Phase 2: Delegates to (proto/add-task! registry task-id slave-id opts)

   Arguments:
     task-id  - Unique identifier string (or nil for auto-generated)
     slave-id - Owning slave's id (required)
     opts     - Map with :status, :prompt, :files

   Returns:
     Transaction report with :tempids containing the task-id."
  [task-id slave-id opts]
  (ds/add-task! task-id slave-id opts))

(defn get-task
  "Get a task by ID.

   Replaces: (ds/get-task task-id)

   Phase 2: Delegates to (proto/get-task registry task-id)

   Arguments:
     task-id - Task identifier string

   Returns:
     Map with task attributes or nil if not found."
  [task-id]
  (ds/get-task task-id))

;;; =============================================================================
;;; File Claims (Extended Operations - not yet in ISwarmRegistry)
;;; =============================================================================

(defn claim-file!
  "Claim a file for a task (conflict detection).

   Replaces: (ds/claim-file! file-path slave-id task-id)

   Currently used by:
   - swarm/sync.clj:183 - handle-task-dispatched

   Phase 2: May add to ISwarmRegistry or keep as extended operation.

   Arguments:
     file-path - Absolute file path
     slave-id  - Slave claiming the file
     task-id   - Task associated with the claim

   Returns:
     Transaction report."
  [file-path slave-id task-id]
  (ds/claim-file! file-path slave-id task-id))

(defn has-conflict?
  "Check if files have existing claims.

   Replaces: (ds/has-conflict? files slave-id)

   Phase 2: May add to ISwarmRegistry or keep as extended operation.

   Arguments:
     files    - Collection of file paths to check
     slave-id - Slave requesting the check (excluded from conflicts)

   Returns:
     true if any file has a conflicting claim, false otherwise."
  [files slave-id]
  (ds/has-conflict? files slave-id))

(defn check-file-conflicts
  "Check files for conflicts with details.

   Replaces: (ds/check-file-conflicts files slave-id)

   Phase 2: May add to ISwarmRegistry or keep as extended operation.

   Arguments:
     files    - Collection of file paths to check
     slave-id - Slave requesting the check

   Returns:
     Map with:
     - :has-conflicts - boolean
     - :conflicts     - seq of {:file :claimed-by :task-id}"
  [files slave-id]
  (ds/check-file-conflicts files slave-id))

;;; =============================================================================
;;; Debug/Stats (Utility functions)
;;; =============================================================================

(defn db-stats
  "Get database statistics.

   Replaces: (ds/db-stats) or (queries/db-stats)

   Currently used by:
   - swarm/sync.clj:363 - sync-status

   Phase 2: Backend-specific implementation.

   Returns:
     Map with :slaves, :tasks, :claims counts."
  []
  (queries/db-stats))

;;; =============================================================================
;;; Migration Notes
;;; =============================================================================

;; Wave/Plan operations (ds/create-plan!, ds/get-wave, etc.) are NOT wrapped here.
;; They are coordination-specific and used only by tools/swarm/wave.clj.
;; These may get their own protocol (IWaveCoordinator) or remain DataScript-specific.
;;
;; Critical ops (ds/enter-critical-op!, ds/with-critical-op) are also not wrapped.
;; They are DataScript-specific transaction semantics.
;;
;; File claiming (claim-file!, has-conflict?, check-file-conflicts) are included
;; as they're used across multiple modules and may need backend abstraction.

(comment
  ;; Example usage (Phase 1 - current):
  (get-slave "ling-1")
  (slave-exists? "ling-1")
  (list-slaves)
  (list-slaves {:status :idle})
  (list-slaves {:project-id "hive-mcp"})
  (list-slaves {:project-id "hive-mcp" :ids-only? true})
  (add-slave! "ling-1" {:name "Worker" :cwd "/home/user/project"})
  (update-slave! "ling-1" {:slave/status :busy})
  (remove-slave! "ling-1")
  (can-kill? "ling-1")
  (db-stats))
