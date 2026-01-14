(ns hive-mcp.hooks.handlers
  "Built-in hook handlers for event-driven workflows.

   All handlers follow the contract: (fn [event context] -> result-map)

   Handlers are PURE - they return action maps describing side effects
   rather than executing them directly. The caller is responsible for
   interpreting and executing these actions.

   Action Types:
   - :shout      - Broadcast message via hivemind channel
   - :git-commit - Create a git commit with specified files
   - :run-workflow - Trigger a named workflow (e.g., :wrap)
   - :kanban-sync - Synchronize kanban state

   SOLID Principles:
   - SRP: Each handler does one thing
   - OCP: New handlers via addition, not modification
   - DIP: Handlers return data, caller handles execution

   CLARITY Framework:
   - Composition over modification: Actions compose via returned maps
   - Represented intent: Action maps clearly express intent
   - Yield safe failure: Nil returns indicate no-op"
  (:require [clojure.string :as str]))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- error-message
  "Extract error message from various error representations."
  [error]
  (cond
    (instance? Throwable error) (.getMessage ^Throwable error)
    (string? error) error
    (map? error) (or (:message error) (str error))
    :else (str error)))

(defn- task-title-or-default
  "Get task title or generate a default."
  [event]
  (or (:title event)
      (str "Task " (:task-id event "unknown"))))

;; =============================================================================
;; :task-complete Handlers
;; =============================================================================

(defn shout-completion
  "Broadcast task completion via hivemind channel.

   Event: {:type :task-complete :task-id string :title string?}
   Context: {:agent-id string :project string?}

   Returns: {:action :shout
             :event-type :completed
             :message string
             :data {:task-id string ...}}"
  [event context]
  (let [task-id (:task-id event)
        title (task-title-or-default event)
        agent-id (:agent-id context "unknown")]
    {:action :shout
     :event-type :completed
     :message (str "Task completed: " title)
     :data {:task-id task-id
            :title title
            :agent-id agent-id
            :project (:project context)}}))

(defn commit-if-files-modified
  "Create git commit if files were modified during task.

   Event: {:type :task-complete :task-id string :title string?}
   Context: {:modified-files [string]}

   Returns: {:action :git-commit :files [string] :message string}
            or nil if no files modified"
  [event context]
  (let [files (:modified-files context)]
    (when (seq files)
      (let [title (task-title-or-default event)
            ;; Generate conventional commit message
            commit-msg (if (str/starts-with? (str/lower-case title) "fix")
                         (str "fix: " title)
                         (str "feat: " title))]
        {:action :git-commit
         :files (vec files)
         :message commit-msg
         :task-id (:task-id event)}))))

;; =============================================================================
;; :session-end Handlers
;; =============================================================================

(defn run-wrap
  "Trigger wrap workflow at session end.

   Event: {:type :session-end}
   Context: {:session-id string :project string :start-time string?}

   Returns: {:action :run-workflow
             :workflow :wrap
             :params {...}}"
  [event context]
  {:action :run-workflow
   :workflow :wrap
   :params {:session-id (:session-id context)
            :project (:project context)
            :start-time (:start-time context)}})

(defn sync-kanban
  "Synchronize kanban state at session end.

   Event: {:type :session-end}
   Context: {:project string}

   Returns: {:action :kanban-sync
             :project string
             :direction :bidirectional}"
  [event context]
  {:action :kanban-sync
   :project (:project context)
   :direction :bidirectional})

;; =============================================================================
;; :error Handlers
;; =============================================================================

(defn shout-error
  "Broadcast error via hivemind channel.

   Event: {:type :error-occurred :error (string|Exception) :task-id string?}
   Context: {:agent-id string}

   Returns: {:action :shout
             :event-type :error
             :message string
             :data {:error string :task-id string?}}"
  [event context]
  (let [error (:error event)
        err-msg (error-message error)
        task-id (:task-id event)]
    {:action :shout
     :event-type :error
     :message (str "Error: " err-msg)
     :data {:error err-msg
            :task-id task-id
            :agent-id (:agent-id context)}}))

;; =============================================================================
;; Handler Collection
;; =============================================================================

(def builtin-handlers
  "Map of event type -> vector of handler functions.

   Used by register-builtins! to register all built-in handlers."
  {:task-complete [shout-completion commit-if-files-modified]
   :session-end [run-wrap sync-kanban]
   :error-occurred [shout-error]})

;; =============================================================================
;; Registration Helper
;; =============================================================================

(defn register-builtins!
  "Register all built-in handlers to a hook registry.

   Arguments:
   - registry: The hook registry atom
   - register-fn: Function (fn [registry event handler]) to register hooks

   This allows injection of the registration function for testing
   or alternative registry implementations."
  [registry register-fn]
  (doseq [[event handlers] builtin-handlers
          handler handlers]
    (register-fn registry event handler)))
