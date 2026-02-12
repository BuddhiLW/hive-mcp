(ns hive-mcp.events.handlers.task
  "Task lifecycle event handlers.

   Handles events related to task completion and broadcasting:
   - :task/complete     - Signal task completion to hivemind
   - :task/shout-complete - Broadcast completion with message
   - :git/commit-modified - Git commit if files changed"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Helpers
;; =============================================================================

(defn- task-title-or-default
  "Extract task title or generate default from task-id."
  [{:keys [title task-id]}]
  (or title (str "Task " (or task-id "unknown"))))

;; =============================================================================
;; Handler: :task/complete
;; =============================================================================

(defn handle-task-complete
  "Handler for :task/complete events.

   Expects event data:
   {:task-id   \"task-123\"
    :agent-id  \"ling-worker-1\"
    :result    \"success\" | \"failure\" | any}

   Produces effects:
   - :log   - Log completion message
   - :shout - Broadcast to hivemind coordinator"
  [_coeffects [_ {:keys [task-id agent-id result]}]]
  {:log {:level :info
         :message (str "Task " task-id " completed by " agent-id)}
   :shout {:agent-id agent-id
           :event-type :completed
           :data {:task-id task-id
                  :result result}}})

;; =============================================================================
;; Handler: :task/shout-complete
;; =============================================================================

(defn handle-task-shout-complete
  "Handler for :task/shout-complete events.

   Broadcasts task completion via hivemind channel with a human-readable message.

   Expects event data:
   {:task-id   \"task-123\"
    :title     \"Fix the bug\"
    :agent-id  \"ling-worker-1\"
    :project   \"hive-mcp\"}

   Produces effects:
   - :shout - Broadcast completion to hivemind with message"
  [_coeffects [_ {:keys [task-id title agent-id project]}]]
  (let [effective-title (or title task-id "unknown task")
        effective-agent (or agent-id
                            (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                            "unknown")]
    {:shout {:agent-id effective-agent
             :event-type :completed
             :message (str "Task completed: " effective-title)
             :data {:task-id task-id
                    :title effective-title
                    :agent-id effective-agent
                    :project project}}}))

;; =============================================================================
;; Handler: :git/commit-modified
;; =============================================================================

(defn handle-git-commit-modified
  "Handler for :git/commit-modified events.

   Creates git commit effect if files were modified during task.

   Expects event data:
   {:task-id        \"task-123\"
    :title          \"Add feature\"  ; optional
    :modified-files [\"src/a.clj\"]}

   Produces effects:
   - :log        - Log commit action
   - :git-commit - Execute git add + commit

   Returns {} if no files modified (no-op)."
  [_coeffects [_ {:keys [task-id title modified-files]}]]
  (if (seq modified-files)
    (let [task-title (task-title-or-default {:title title :task-id task-id})
          ;; Generate conventional commit message
          commit-msg (if (re-find #"(?i)^fix" task-title)
                       (str "fix: " task-title)
                       (str "feat: " task-title))]
      {:log {:level :info
             :message (str "Git commit for task " task-id ": " (count modified-files) " files")}
       :git-commit {:files (vec modified-files)
                    :message commit-msg
                    :task-id task-id}})
    ;; No files modified - return empty effects map (no-op)
    {}))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register task-related event handlers."
  []
  (ev/reg-event :task/complete
                [interceptors/debug]
                handle-task-complete)

  (ev/reg-event :task/shout-complete
                [interceptors/debug]
                handle-task-shout-complete)

  (ev/reg-event :git/commit-modified
                [interceptors/debug]
                handle-git-commit-modified))
