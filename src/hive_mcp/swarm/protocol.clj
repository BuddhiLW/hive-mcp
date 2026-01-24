(ns hive-mcp.swarm.protocol
  "ISwarmRegistry protocol for swarm state management.

   Provides a unified interface for swarm registry persistence,
   enabling multiple backend implementations:
   - DataScript (in-memory, persistent via serialization)
   - Datomic (cloud or on-prem, full history)
   - XTDB (bitemporality, document-centric)
   - Atom (simple in-memory for testing)

   SOLID: Interface Segregation - focused swarm registry operations only.
   DDD: Repository pattern for swarm entity lifecycle management.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol ISwarmRegistry
  "Interface for swarm state registry.

   Implementations: DataScript, Datomic, XTDB, Atom.

   All methods should be thread-safe. Write operations (add-*, update-*, remove-*)
   may block. Read operations (get-*, list-*) should be fast and preferably non-blocking.

   Entity types managed:
   - Slave: Ling/drone instances with status, presets, project scope
   - Task: Work units assigned to slaves with status lifecycle"

  ;;; =========================================================================
  ;;; Slave Operations
  ;;; =========================================================================

  (add-slave! [this slave-id opts]
    "Add a new slave to the registry.

     Arguments:
       slave-id - Unique string identifier (required)
       opts     - Map with optional keys:
                  :name       - Human-readable name (defaults to slave-id)
                  :status     - Initial status keyword (default :idle)
                  :depth      - Hierarchy depth integer (default 1 for ling)
                  :parent     - Parent slave-id string
                  :presets    - Collection of preset name strings
                  :cwd        - Working directory path
                  :project-id - Project ID for scoping

     Returns:
       Transaction report with :tempids on success.

     Throws:
       ex-info if slave-id already exists or validation fails.")

  (get-slave [this slave-id]
    "Get a slave by ID.

     Arguments:
       slave-id - Slave identifier string

     Returns:
       Map with slave attributes or nil if not found.
       Keys include: :slave/id, :slave/name, :slave/status, :slave/depth,
                     :slave/parent, :slave/presets, :slave/cwd, :slave/project-id,
                     :slave/tasks-completed, :slave/created-at")

  (update-slave! [this slave-id updates]
    "Update an existing slave's attributes.

     Arguments:
       slave-id - Slave to update
       updates  - Map of attributes to update (uses :slave/* keys)

     Returns:
       Transaction report on success, nil if slave not found.

     Example:
       (update-slave! registry \"ling-1\" {:slave/status :busy})")

  (remove-slave! [this slave-id]
    "Remove a slave from the registry.

     Also releases any file claims held by this slave.
     Implementations should handle cascading cleanup.

     Arguments:
       slave-id - Slave to remove

     Returns:
       Transaction report on success, nil if slave not found.")

  (get-all-slaves [this]
    "Get all slaves in the registry.

     Returns:
       Seq of maps with slave attributes (may be empty).")

  (get-slaves-by-status [this status]
    "Get slaves filtered by status.

     Arguments:
       status - Status keyword to filter by (:idle, :busy, :blocked, etc.)

     Returns:
       Seq of slave maps matching status (may be empty).")

  (get-slaves-by-project [this project-id]
    "Get slaves filtered by project-id.

     Arguments:
       project-id - Project ID string to filter by

     Returns:
       Seq of slave maps belonging to the project (may be empty).")

  ;;; =========================================================================
  ;;; Task Operations
  ;;; =========================================================================

  (add-task! [this task-id slave-id opts]
    "Add a new task to the registry.

     Arguments:
       task-id  - Unique identifier string (optional, auto-generated if nil)
       slave-id - Owning slave's id (required)
       opts     - Map with keys:
                  :status - Initial status (default :dispatched)
                  :prompt - Task description string
                  :files  - Collection of file paths involved

     Returns:
       Transaction report with :tempids containing the task-id.

     Throws:
       ex-info if slave-id doesn't exist or validation fails.")

  (get-task [this task-id]
    "Get a task by ID.

     Arguments:
       task-id - Task identifier string

     Returns:
       Map with task attributes or nil if not found.
       Keys include: :task/id, :task/slave, :task/status, :task/prompt,
                     :task/files, :task/started-at, :task/completed-at")

  (update-task! [this task-id updates]
    "Update an existing task's attributes.

     Arguments:
       task-id - Task to update
       updates - Map of attributes to update (uses :task/* keys)

     Returns:
       Transaction report on success, nil if task not found.

     Common updates:
       {:task/status :completed :task/completed-at (java.util.Date.)}
       {:task/status :error}")

  (get-tasks-for-slave
    [this slave-id]
    [this slave-id status]
    "Get tasks for a specific slave.

     Arguments:
       slave-id - Slave to get tasks for
       status   - Optional status filter keyword

     Returns:
       Seq of task maps (may be empty)."))

;;; =============================================================================
;;; Registry Status Constants (for reference by implementations)
;;; =============================================================================

(def slave-statuses
  "Valid slave status values."
  #{:idle :busy :blocked :error :terminated})

(def task-statuses
  "Valid task status values."
  #{:dispatched :running :completed :error :timeout :queued})

;;; =============================================================================
;;; Public API Wrappers (backward-compatible delegation)
;;; =============================================================================
;;
;; These functions provide a stable public API that delegates to the ISwarmRegistry
;; protocol. Enables gradual migration without breaking existing code.
;;
;; Usage: (require '[hive-mcp.swarm.protocol :as proto])
;;        (proto/add-slave! registry "ling-1" {:name "Worker"})
;;

;;; ---------------------------------------------------------------------------
;;; Slave Operations
;;; ---------------------------------------------------------------------------

(defn add-slave!*
  "Add a new slave to the registry. Wrapper for ISwarmRegistry/add-slave!

   Arguments:
     registry - ISwarmRegistry implementation
     slave-id - Unique string identifier
     opts     - Map with :name, :status, :depth, :parent, :presets, :cwd, :project-id

   Returns: Transaction report with :tempids"
  [registry slave-id opts]
  (add-slave! registry slave-id opts))

(defn get-slave*
  "Get a slave by ID. Wrapper for ISwarmRegistry/get-slave

   Arguments:
     registry - ISwarmRegistry implementation
     slave-id - Slave identifier string

   Returns: Map with slave attributes or nil"
  [registry slave-id]
  (get-slave registry slave-id))

(defn update-slave!*
  "Update an existing slave. Wrapper for ISwarmRegistry/update-slave!

   Arguments:
     registry - ISwarmRegistry implementation
     slave-id - Slave to update
     updates  - Map of attributes to update (uses :slave/* keys)

   Returns: Transaction report on success, nil if not found"
  [registry slave-id updates]
  (update-slave! registry slave-id updates))

(defn remove-slave!*
  "Remove a slave from the registry. Wrapper for ISwarmRegistry/remove-slave!

   Arguments:
     registry - ISwarmRegistry implementation
     slave-id - Slave to remove

   Returns: Transaction report on success, nil if not found"
  [registry slave-id]
  (remove-slave! registry slave-id))

(defn get-all-slaves*
  "Get all slaves in the registry. Wrapper for ISwarmRegistry/get-all-slaves

   Arguments:
     registry - ISwarmRegistry implementation

   Returns: Seq of slave maps"
  [registry]
  (get-all-slaves registry))

(defn get-slaves-by-status*
  "Get slaves filtered by status. Wrapper for ISwarmRegistry/get-slaves-by-status

   Arguments:
     registry - ISwarmRegistry implementation
     status   - Status keyword (:idle, :busy, :blocked, etc.)

   Returns: Seq of matching slave maps"
  [registry status]
  (get-slaves-by-status registry status))

(defn get-slaves-by-project*
  "Get slaves filtered by project. Wrapper for ISwarmRegistry/get-slaves-by-project

   Arguments:
     registry   - ISwarmRegistry implementation
     project-id - Project ID string

   Returns: Seq of matching slave maps"
  [registry project-id]
  (get-slaves-by-project registry project-id))

;;; ---------------------------------------------------------------------------
;;; Task Operations
;;; ---------------------------------------------------------------------------

(defn add-task!*
  "Add a new task to the registry. Wrapper for ISwarmRegistry/add-task!

   Arguments:
     registry - ISwarmRegistry implementation
     task-id  - Unique identifier (or nil for auto-generated)
     slave-id - Owning slave's id
     opts     - Map with :status, :prompt, :files

   Returns: Transaction report with :tempids"
  [registry task-id slave-id opts]
  (add-task! registry task-id slave-id opts))

(defn get-task*
  "Get a task by ID. Wrapper for ISwarmRegistry/get-task

   Arguments:
     registry - ISwarmRegistry implementation
     task-id  - Task identifier string

   Returns: Map with task attributes or nil"
  [registry task-id]
  (get-task registry task-id))

(defn update-task!*
  "Update an existing task. Wrapper for ISwarmRegistry/update-task!

   Arguments:
     registry - ISwarmRegistry implementation
     task-id  - Task to update
     updates  - Map of attributes to update (uses :task/* keys)

   Returns: Transaction report on success, nil if not found"
  [registry task-id updates]
  (update-task! registry task-id updates))

(defn get-tasks-for-slave*
  "Get tasks for a specific slave. Wrapper for ISwarmRegistry/get-tasks-for-slave

   Arguments:
     registry - ISwarmRegistry implementation
     slave-id - Slave to get tasks for
     status   - Optional status filter keyword

   Returns: Seq of task maps"
  ([registry slave-id]
   (get-tasks-for-slave registry slave-id))
  ([registry slave-id status]
   (get-tasks-for-slave registry slave-id status)))
