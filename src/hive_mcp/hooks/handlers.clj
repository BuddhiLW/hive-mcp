(ns hive-mcp.hooks.handlers
  "DEPRECATED: Use hive-mcp.events.handlers and ev/dispatch instead.

   This namespace is deprecated as of P5-6. All handlers have been migrated
   to the new event system:

   | Old Handler              | New Event               |
   |--------------------------|-------------------------|
   | shout-completion         | [:task/shout-complete]  |
   | commit-if-files-modified | [:git/commit-modified]  |
   | run-wrap                 | [:session/wrap]         |
   | sync-kanban              | [:kanban/sync]          |
   | shout-error              | [:error/shout]          |

   Migration example:
   ```clojure
   ;; Old way (deprecated)
   (require '[hive-mcp.hooks.handlers :as h])
   (h/shout-completion {:task-id \"123\"} {:agent-id \"agent-1\"})

   ;; New way
   (require '[hive-mcp.events.core :as ev])
   (ev/dispatch [:task/shout-complete {:task-id \"123\" :agent-id \"agent-1\"}])
   ```

   The functions in this namespace now delegate to ev/dispatch and will
   be removed in a future version.

   ---
   ORIGINAL DOCSTRING (for reference):

   Built-in hook handlers for event-driven workflows.

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
  (:require [clojure.string :as str]
            [hive-mcp.events.core :as ev]))

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

(defn ^:deprecated shout-completion
  "DEPRECATED: Use (ev/dispatch [:task/shout-complete data]) instead.

   Broadcast task completion via hivemind channel.

   Event: {:type :task-complete :task-id string :title string?}
   Context: {:agent-id string :project string?}

   Returns: {:action :shout
             :event-type :completed
             :message string
             :data {:task-id string ...}}"
  [event context]
  (println "WARN: shout-completion is deprecated. Use (ev/dispatch [:task/shout-complete ...]) instead.")
  (let [task-id (:task-id event)
        title (task-title-or-default event)
        agent-id (:agent-id context "unknown")]
    ;; Dispatch to new event system
    (ev/dispatch [:task/shout-complete {:task-id task-id
                                        :title title
                                        :agent-id agent-id
                                        :project (:project context)}])
    ;; Return legacy format for backwards compatibility
    {:action :shout
     :event-type :completed
     :message (str "Task completed: " title)
     :data {:task-id task-id
            :title title
            :agent-id agent-id
            :project (:project context)}}))

(defn ^:deprecated commit-if-files-modified
  "DEPRECATED: Use (ev/dispatch [:git/commit-modified data]) instead.

   Create git commit if files were modified during task.

   Event: {:type :task-complete :task-id string :title string?}
   Context: {:modified-files [string]}

   Returns: {:action :git-commit :files [string] :message string}
            or nil if no files modified"
  [event context]
  (println "WARN: commit-if-files-modified is deprecated. Use (ev/dispatch [:git/commit-modified ...]) instead.")
  (let [files (:modified-files context)]
    (when (seq files)
      (let [title (task-title-or-default event)
            task-id (:task-id event)
            ;; Generate conventional commit message
            commit-msg (if (str/starts-with? (str/lower-case title) "fix")
                         (str "fix: " title)
                         (str "feat: " title))]
        ;; Dispatch to new event system
        (ev/dispatch [:git/commit-modified {:task-id task-id
                                            :title title
                                            :modified-files (vec files)}])
        ;; Return legacy format for backwards compatibility
        {:action :git-commit
         :files (vec files)
         :message commit-msg
         :task-id task-id}))))

;; =============================================================================
;; :session-end Handlers
;; =============================================================================

(defn ^:deprecated run-wrap
  "DEPRECATED: Use (ev/dispatch [:session/wrap data]) instead.

   Trigger wrap workflow at session end.

   Event: {:type :session-end}
   Context: {:session-id string :project string :start-time string?}

   Returns: {:action :run-workflow
             :workflow :wrap
             :params {...}}"
  [_event context]
  (println "WARN: run-wrap is deprecated. Use (ev/dispatch [:session/wrap ...]) instead.")
  ;; Dispatch to new event system
  (ev/dispatch [:session/wrap {:session-id (:session-id context)
                               :project (:project context)
                               :start-time (:start-time context)}])
  ;; Return legacy format for backwards compatibility
  {:action :run-workflow
   :workflow :wrap
   :params {:session-id (:session-id context)
            :project (:project context)
            :start-time (:start-time context)}})

(defn ^:deprecated sync-kanban
  "DEPRECATED: Use (ev/dispatch [:kanban/sync data]) instead.

   Synchronize kanban state at session end.

   Event: {:type :session-end}
   Context: {:project string}

   Returns: {:action :kanban-sync
             :project string
             :direction :bidirectional}"
  [_event context]
  (println "WARN: sync-kanban is deprecated. Use (ev/dispatch [:kanban/sync ...]) instead.")
  ;; Dispatch to new event system
  (ev/dispatch [:kanban/sync {:project (:project context)}])
  ;; Return legacy format for backwards compatibility
  {:action :kanban-sync
   :project (:project context)
   :direction :bidirectional})

;; =============================================================================
;; :error Handlers
;; =============================================================================

(defn ^:deprecated shout-error
  "DEPRECATED: Error handling should use the new event system.

   NOTE: No direct event equivalent exists yet. This function continues
   to work but is deprecated. Future versions should dispatch to
   [:error/shout data] once that handler is implemented.

   Broadcast error via hivemind channel.

   Event: {:type :error-occurred :error (string|Exception) :task-id string?}
   Context: {:agent-id string}

   Returns: {:action :shout
             :event-type :error
             :message string
             :data {:error string :task-id string?}}"
  [event context]
  (println "WARN: shout-error is deprecated. Migrate to event-based error handling.")
  (let [error (:error event)
        err-msg (error-message error)
        task-id (:task-id event)]
    ;; Legacy return format - no event dispatch yet
    {:action :shout
     :event-type :error
     :message (str "Error: " err-msg)
     :data {:error err-msg
            :task-id task-id
            :agent-id (:agent-id context)}}))

;; =============================================================================
;; Handler Collection
;; =============================================================================

#_{:clj-kondo/ignore [:deprecated-var]}
(def ^:deprecated builtin-handlers
  "DEPRECATED: Use hive-mcp.events.handlers/register-handlers! instead.

   Map of event type -> vector of handler functions.

   Migration:
   - :task-complete  -> [:task/shout-complete] and [:git/commit-modified]
   - :session-end    -> [:session/wrap] and [:kanban/sync]
   - :error-occurred -> [:error/shout] (pending)

   Used by register-builtins! to register all built-in handlers."
  {:task-complete [shout-completion commit-if-files-modified]
   :session-end [run-wrap sync-kanban]
   :error-occurred [shout-error]})

;; =============================================================================
;; Registration Helper
;; =============================================================================

(defn ^:deprecated register-builtins!
  "DEPRECATED: Use (require '[hive-mcp.events.handlers :as h]) (h/register-handlers!) instead.

   Register all built-in handlers to a hook registry.

   Arguments:
   - registry: The hook registry atom
   - register-fn: Function (fn [registry event handler]) to register hooks

   This allows injection of the registration function for testing
   or alternative registry implementations.

   NOTE: This function still works but is deprecated. The new event system
   should be initialized via hive-mcp.events.handlers/register-handlers!"
  [registry register-fn]
  (println "WARN: register-builtins! is deprecated. Use hive-mcp.events.handlers/register-handlers! instead.")
  #_:clj-kondo/ignore
  (doseq [[event handlers] builtin-handlers
          handler handlers]
    (register-fn registry event handler)))
