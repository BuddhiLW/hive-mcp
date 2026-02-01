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

   :idle         - Ready for work
   :spawning     - Being created
   :starting     - Process starting
   :initializing - Spawned, awaiting preset injection completion
   :working      - Executing a task
   :blocked      - Waiting on external resource
   :error        - In error state
   :terminated   - Killed/stopped"
  #{:idle :spawning :starting :initializing :working :blocked :error :terminated})

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

(def daemon-statuses
  "Valid Emacs daemon status values.

   :active     - Running and sending heartbeats
   :stale      - Not sending heartbeats (likely crashed)
   :error      - In error state (Emacs reported errors)
   :terminated - Gracefully shutdown"
  #{:active :stale :error :terminated})

(def olympus-layout-modes
  "Valid Olympus layout mode values.
   :auto    - Automatically calculate optimal layout
   :manual  - User-controlled window positions
   :stacked - Overlapping/tabbed windows"
  #{:auto :manual :stacked})

(def agent-types
  "Valid agent type values (IAgent discrimination).
   :ling  - Persistent Claude Code instance (can chain tools)
   :drone - Ephemeral API call (single task, stateless)"
  #{:ling :drone})

(def task-types
  "Valid task type values for drone routing.
   :coding  - Code implementation tasks
   :docs    - Documentation tasks
   :review  - Code review tasks
   :test    - Testing tasks
   :refactor - Refactoring tasks"
  #{:coding :docs :review :test :refactor})

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

   :slave/project-id
   {:db/doc "Project ID derived from cwd (for project-scoped operations like swarm_kill 'all')"}

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

   :slave/kanban-task-id
   {:db/doc "Optional kanban task ID this ling is working on.
            Enables task-aware lifecycle (auto-move to done when ling wraps).
            Queryable for 'which ling owns task X?' lookups."}

   ;; Agent type discrimination (IAgent support)
   :slave/agent-type
   {:db/doc "Agent type: :ling (persistent Claude Code) or :drone (ephemeral API)"
    :db/index true}

   :slave/model
   {:db/doc "OpenRouter model ID for drones (e.g., 'anthropic/claude-sonnet-4')"}

   :slave/task-type
   {:db/doc "Task type for routing: :coding, :docs, :review, etc."}

   :slave/max-steps
   {:db/doc "Maximum step budget for drones (limits API calls)"}

   :slave/sandbox
   {:db/doc "Sandbox constraints EDN map (e.g., {:allow-write false :allow-bash false})"}

   :slave/upgraded-from
   {:db/doc "Original drone-id if this ling was upgraded from a drone"}

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

   :claim/expires-at
   {:db/doc "Timestamp when claim should auto-expire (TTL, stored as epoch millis)"}

   :claim/heartbeat-at
   {:db/doc "Last heartbeat timestamp for liveness tracking"}

   :claim/wave-id
   {:db/doc "Wave ID that created this claim (for wave-scoped cleanup)"}

   ;; Contextual claim fields (hash tracking and change history)
   :claim/prior-hash
   {:db/doc "File content hash at claim acquisition time"}

   :claim/released-hash
   {:db/doc "File content hash at claim release time"}

   :claim/changes
   {:db/doc "References to claim-change entities summarizing modifications"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}

   :claim/kg-edges-created
   {:db/doc "Knowledge graph edge IDs created during this claim"
    :db/cardinality :db.cardinality/many}

   ;;; =========================================================================
   ;;; Claim Change Entity (Change summaries for contextual claims)
   ;;; =========================================================================

   :claim-change/id
   {:db/doc "Unique identifier for the change entry"
    :db/unique :db.unique/identity}

   :claim-change/lines-added
   {:db/doc "Number of lines added in this change"}

   :claim-change/lines-removed
   {:db/doc "Number of lines removed in this change"}

   :claim-change/hunk-count
   {:db/doc "Number of diff hunks in this change"}

   :claim-change/summary
   {:db/doc "Human-readable summary of the change"}

   :claim-change/computed-at
   {:db/doc "Timestamp when this change summary was computed"}

   ;;; =========================================================================
   ;;; Claim History Entity (CC.6 - Recent changes tracking)
   ;;; =========================================================================

   :claim-history/id
   {:db/doc "Unique identifier for the claim history entry"
    :db/unique :db.unique/identity}

   :claim-history/file
   {:db/doc "File path that was claimed"
    :db/index true}

   :claim-history/slave-id
   {:db/doc "ID of the slave that held the claim"}

   :claim-history/prior-hash
   {:db/doc "File content hash at claim acquisition time"}

   :claim-history/released-hash
   {:db/doc "File content hash at claim release time"}

   :claim-history/lines-added
   {:db/doc "Number of lines added during the claim period"}

   :claim-history/lines-removed
   {:db/doc "Number of lines removed during the claim period"}

   :claim-history/released-at
   {:db/doc "Timestamp when the claim was released"}

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

   :wrap-queue/project-id
   {:db/doc "Project ID for scoping (derived from ling's working directory)"}

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
   {:db/doc "Timestamp when ling started waiting"}

   ;;; =========================================================================
   ;;; Health Event Entity (Centralized Error Tracking)
   ;;; =========================================================================

   :health-event/id
   {:db/doc "Unique identifier for the health event"
    :db/unique :db.unique/identity}

   :health-event/type
   {:db/doc "Type of error (e.g., :harvest-failed, :chroma-unavailable)"}

   :health-event/severity
   {:db/doc "Severity level: :info :warn :error :fatal"}

   :health-event/message
   {:db/doc "Human-readable error message"}

   :health-event/context
   {:db/doc "Additional context map (optional, extra data)"}

   :health-event/timestamp
   {:db/doc "When the event occurred"}

   :health-event/recoverable?
   {:db/doc "Whether the error is recoverable"}

   ;;; =========================================================================
   ;;; Olympus Entity (Grid View State)
   ;;; =========================================================================

   :olympus/id
   {:db/doc "Singleton identifier for Olympus state (always 'olympus')"
    :db/unique :db.unique/identity}

   :olympus/active-tab
   {:db/doc "Currently active tab index (0-based) for tabbed layouts"}

   :olympus/layout-mode
   {:db/doc "Layout mode: :auto (optimal), :manual (user-positioned), :stacked (overlapping)"}

   :olympus/focused-ling
   {:db/doc "Currently focused/maximized ling ID, or nil for grid view"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   :olympus/ling-positions
   {:db/doc "Map of {ling-id {:tab T :row R :col C}} positions (stored as EDN string)"}

   ;;; =========================================================================
   ;;; Emacs Daemon Entity (Daemon lifecycle management)
   ;;; =========================================================================

   :emacs-daemon/id
   {:db/doc "Unique identifier for the Emacs daemon (e.g., socket name)"
    :db/unique :db.unique/identity}

   :emacs-daemon/socket-name
   {:db/doc "Emacs daemon socket name (for emacsclient -s)"}

   :emacs-daemon/pid
   {:db/doc "Operating system process ID of the Emacs daemon"}

   :emacs-daemon/emacsclient
   {:db/doc "Path to the emacsclient binary used to communicate"}

   :emacs-daemon/status
   {:db/doc "Current status: :active :stale :error :terminated"}

   :emacs-daemon/started-at
   {:db/doc "Timestamp when daemon was registered"}

   :emacs-daemon/heartbeat-at
   {:db/doc "Timestamp of last successful heartbeat"}

   :emacs-daemon/error-message
   {:db/doc "Last error message (set when status is :error)"}

   :emacs-daemon/error-count
   {:db/doc "Cumulative count of errors encountered"}

   :emacs-daemon/lings
   {:db/doc "Set of ling/slave IDs bound to this daemon"
    :db/cardinality :db.cardinality/many}})
