(ns hive-mcp.swarm.datascript.schema
  "DataScript schema definitions for swarm hivemind coordination.

   Contains:
   - Entity schemas (slave, task, claim, wrap-queue, etc.)
   - Status enumerations for validation
   - Schema documentation

   SOLID-S: Single Responsibility - only schema definitions.
   DDD: Value Objects for status enums, schema as domain model.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Status Enumerations (Value Objects)
;;; =============================================================================

(def slave-statuses
  "Valid slave status values.

   :idle      - Ready for work
   :spawning  - Being created
   :starting  - Process starting
   :working   - Executing a task
   :error     - In error state"
  #{:idle :spawning :starting :working :error})

(def task-statuses
  "Valid task status values.

   :queued     - Waiting for dispatch (file conflicts)
   :dispatched - Sent to slave, in progress
   :completed  - Successfully finished
   :timeout    - Timed out waiting
   :error      - Failed with error"
  #{:queued :dispatched :completed :timeout :error})

(def coordinator-statuses
  "Valid coordinator status values.

   :active     - Currently running and sending heartbeats
   :stale      - Not sending heartbeats (likely crashed)
   :terminated - Gracefully shutdown"
  #{:active :stale :terminated})

(def plan-statuses
  "Valid change plan status values."
  #{:pending :in-progress :completed :failed})

(def item-statuses
  "Valid change item status values."
  #{:pending :dispatched :completed :failed})

(def wave-statuses
  "Valid wave status values."
  #{:running :completed :partial-failure})

(def critical-op-types
  "Valid critical operation types that block kill.
   :wrap    - Session crystallization in progress
   :commit  - Git commit operation in progress
   :dispatch - Task dispatch in progress"
  #{:wrap :commit :dispatch})

;;; =============================================================================
;;; Schema Definition
;;; =============================================================================

(def schema
  "DataScript schema for swarm state.

   Design notes:
   - :db.type/ref for entity relationships (enables joins)
   - :db/unique for primary keys
   - :db.cardinality/many for collections (presets, files)"

  {;;; =========================================================================
   ;;; Slave Entity
   ;;; =========================================================================

   :slave/id
   {:db/doc "Unique identifier for the slave (e.g., 'swarm-worker-123')"
    :db/unique :db.unique/identity}

   :slave/name
   {:db/doc "Human-readable name for the slave"}

   :slave/status
   {:db/doc "Current status: :idle :spawning :starting :working :error"}

   :slave/depth
   {:db/doc "Hierarchy depth: 0=hivemind, 1=ling, 2=drone"}

   :slave/parent
   {:db/doc "Reference to parent slave (for hierarchy)"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :slave/presets
   {:db/doc "Applied presets (e.g., 'tdd', 'reviewer')"
    :db/cardinality :db.cardinality/many}

   :slave/cwd
   {:db/doc "Current working directory"}

   :slave/current-task
   {:db/doc "Reference to currently executing task"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :slave/tasks-completed
   {:db/doc "Count of completed tasks"}

   :slave/created-at
   {:db/doc "Timestamp when slave was created"}

   :slave/critical-ops
   {:db/doc "Set of currently active critical operations (:wrap :commit :dispatch)"
    :db/cardinality :db.cardinality/many}

   ;;; =========================================================================
   ;;; Task Entity
   ;;; =========================================================================

   :task/id
   {:db/doc "Unique identifier for the task"
    :db/unique :db.unique/identity}

   :task/slave
   {:db/doc "Reference to owning slave"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :task/status
   {:db/doc "Current status: :queued :dispatched :completed :timeout :error"}

   :task/prompt
   {:db/doc "Task description/prompt text"}

   :task/files
   {:db/doc "Files this task operates on"
    :db/cardinality :db.cardinality/many}

   :task/started-at
   {:db/doc "Timestamp when task started"}

   :task/completed-at
   {:db/doc "Timestamp when task completed (nil if pending)"}

   ;;; =========================================================================
   ;;; Claim Entity
   ;;; =========================================================================

   :claim/file
   {:db/doc "File path being claimed (unique - one claim per file)"
    :db/unique :db.unique/identity}

   :claim/slave
   {:db/doc "Reference to slave holding the claim"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :claim/task
   {:db/doc "Reference to task that created this claim"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :claim/created-at
   {:db/doc "Timestamp when claim was created"}

   ;;; =========================================================================
   ;;; Wrap Queue Entity (Crystal Convergence)
   ;;; =========================================================================

   :wrap-queue/id
   {:db/doc "Unique identifier for wrap notification"
    :db/unique :db.unique/identity}

   :wrap-queue/agent-id
   {:db/doc "ID of the ling that wrapped"}

   :wrap-queue/session-id
   {:db/doc "Session tag (e.g., session:2026-01-14:ling-123)"}

   :wrap-queue/created-ids
   {:db/doc "Memory entry IDs created during this wrap"
    :db/cardinality :db.cardinality/many}

   :wrap-queue/stats
   {:db/doc "Map of stats {:notes N :decisions N :conventions N}"}

   :wrap-queue/processed?
   {:db/doc "Whether coordinator has processed this wrap"}

   :wrap-queue/created-at
   {:db/doc "Timestamp when wrap occurred"}

   ;;; =========================================================================
   ;;; Change Plan Entity (dispatch_drone_wave)
   ;;; =========================================================================

   :change-plan/id
   {:db/doc "Unique identifier for the change plan"
    :db/unique :db.unique/identity}

   :change-plan/status
   {:db/doc "Plan status: :pending :in-progress :completed :failed"}

   :change-plan/preset
   {:db/doc "Drone preset for all items (e.g., 'drone-worker')"}

   :change-plan/created-at
   {:db/doc "Timestamp when plan was created"}

   :change-plan/completed-at
   {:db/doc "Timestamp when plan completed (nil if pending)"}

   ;;; =========================================================================
   ;;; Change Item Entity (dispatch_drone_wave items)
   ;;; =========================================================================

   :change-item/id
   {:db/doc "Unique identifier for the change item"
    :db/unique :db.unique/identity}

   :change-item/plan
   {:db/doc "Reference to parent change plan"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :change-item/file
   {:db/doc "File path this item operates on"}

   :change-item/task
   {:db/doc "Task description for this item"}

   :change-item/status
   {:db/doc "Item status: :pending :dispatched :completed :failed"}

   :change-item/drone-id
   {:db/doc "Drone slave-id if dispatched"}

   :change-item/result
   {:db/doc "Result message on completion/failure"}

   :change-item/created-at
   {:db/doc "Timestamp when item was created"}

   :change-item/completed-at
   {:db/doc "Timestamp when item completed"}

   ;;; =========================================================================
   ;;; Wave Entity (dispatch_drone_wave execution)
   ;;; =========================================================================

   :wave/id
   {:db/doc "Unique identifier for the wave execution"
    :db/unique :db.unique/identity}

   :wave/plan
   {:db/doc "Reference to change plan being executed"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :wave/concurrency
   {:db/doc "Max concurrent drones (default: 3)"}

   :wave/active-count
   {:db/doc "Currently active drone count"}

   :wave/completed-count
   {:db/doc "Number of completed items"}

   :wave/failed-count
   {:db/doc "Number of failed items"}

   :wave/status
   {:db/doc "Wave status: :running :completed :partial-failure"}

   :wave/started-at
   {:db/doc "Timestamp when wave started"}

   :wave/completed-at
   {:db/doc "Timestamp when wave completed"}

   ;;; =========================================================================
   ;;; Coordinator Entity (Multi-coordinator lifecycle management)
   ;;; =========================================================================

   :coordinator/id
   {:db/doc "Unique identifier for the coordinator"
    :db/unique :db.unique/identity}

   :coordinator/project
   {:db/doc "Project identifier this coordinator is bound to"}

   :coordinator/pid
   {:db/doc "Operating system process ID"}

   :coordinator/session-id
   {:db/doc "Random UUID for this session (survives process restarts)"}

   :coordinator/started-at
   {:db/doc "Timestamp when coordinator was started"}

   :coordinator/heartbeat-at
   {:db/doc "Timestamp of last heartbeat"}

   :coordinator/status
   {:db/doc "Current status: :active :stale :terminated"}

   ;;; =========================================================================
   ;;; Completed Task Entity (Session-scoped task completions for wrap)
   ;;; =========================================================================

   :completed-task/id
   {:db/doc "Unique identifier for the completed task (e.g., kanban task ID)"
    :db/unique :db.unique/identity}

   :completed-task/title
   {:db/doc "Task title/description"}

   :completed-task/agent-id
   {:db/doc "ID of the ling/agent that completed the task"}

   :completed-task/completed-at
   {:db/doc "Timestamp when task was completed"}

   ;;; =========================================================================
   ;;; Wait-Queue Entity (File-Claim Event Cascade)
   ;;; =========================================================================

   :wait-queue/id
   {:db/doc "Unique identifier for wait-queue entry"
    :db/unique :db.unique/identity}

   :wait-queue/ling-id
   {:db/doc "ID of the ling waiting for file access"}

   :wait-queue/file
   {:db/doc "File path the ling is waiting for"}

   :wait-queue/queued-at
   {:db/doc "Timestamp when ling started waiting"}})
