(ns hive-mcp.events.handlers.kanban
  "Kanban state event handlers.

   Handles events related to kanban task management:
   - :kanban/done - Kanban task completed
   - :kanban/sync - Sync kanban at session end"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Handler: :kanban/done (EVENTS-09)
;; =============================================================================

(defn handle-kanban-done
  "Handler for :kanban/done events.

   Creates an ephemeral progress note via :memory-write effect and
   updates DataScript to track completion. Used for kanban task
   completion tracking and progress visibility.

   Expects event data:
   {:task-id     \"kanban-task-uuid\"
    :task-title  \"Fix the bug\"
    :agent-id    \"swarm-worker-123\"
    :result      \"success\" | \"failure\"}

   Produces effects:
   - :log          - Log completion
   - :memory-write - Create ephemeral progress note
   - :ds-transact  - Update kanban state in DataScript"
  [_coeffects [_ {:keys [task-id task-title agent-id result]}]]
  (let [effective-agent (or agent-id
                            (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                            "unknown-agent")
        note-content (str "## Kanban Task Completed\n\n"
                          "- **Task ID:** " task-id "\n"
                          "- **Title:** " (or task-title "Untitled") "\n"
                          "- **Agent:** " effective-agent "\n"
                          "- **Result:** " (or result "completed") "\n"
                          "- **Timestamp:** " (java.time.Instant/now))]
    {:log {:level :info
           :message (str "Kanban task done: " (or task-title task-id))}
     :memory-write {:type "note"
                    :content note-content
                    :tags ["kanban-progress" "ephemeral"]
                    :duration "ephemeral"
                    :agent-id effective-agent}
     :ds-transact [{:kanban/task-id task-id
                    :kanban/status :done
                    :kanban/completed-by effective-agent
                    :kanban/completed-at (java.util.Date.)}]}))

;; =============================================================================
;; Handler: :kanban/sync (P5-4)
;; =============================================================================

(defn handle-kanban-sync
  "Handler for :kanban/sync events.

   Synchronizes kanban state at session end.

   Expects event data:
   {:project \"override-project\"}  ; optional, uses coeffects if not provided

   Coeffects used:
   - [:agent-context :project] - Project for scoping

   Produces effects:
   - :log         - Log sync action
   - :kanban-sync - Execute bidirectional kanban sync"
  [coeffects [_ event-data]]
  (let [project (or (:project event-data)
                    (get-in coeffects [:agent-context :project])
                    "unknown-project")]
    {:log {:level :info
           :message (str "Kanban sync for project: " project)}
     :kanban-sync {:project project
                   :direction :bidirectional}}))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register kanban-related event handlers."
  []
  (ev/reg-event :kanban/done
                [interceptors/debug]
                handle-kanban-done)

  (ev/reg-event :kanban/sync
                [interceptors/debug]
                handle-kanban-sync))
