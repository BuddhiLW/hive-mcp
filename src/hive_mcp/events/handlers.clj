(ns hive-mcp.events.handlers
  "Event handlers for hive-mcp events.
   
   Each handler is a function that receives [coeffects event] and returns
   an effects map describing side-effects to execute.
   
   Handlers are registered via `reg-event` with optional interceptors.
   
   ## Usage
   ```clojure
   (require '[hive-mcp.events.handlers :as handlers])
   (handlers/register-handlers!)
   ```
   
   ## Available Events
   - :task/complete  - Signal task completion to hivemind
   - :ling/started   - Ling spawned and initialized (EVENTS-03)
   - :ling/completed - Ling finished all work (EVENTS-03)
   - :session/end    - Session ending, trigger auto-wrap (EVENTS-06)
   - :kanban/done    - Kanban task completed (EVENTS-09)
   
   SOLID: Single Responsibility - event-to-effect mapping only
   CLARITY: R - Represented intent through clear effect descriptions"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]))

;; =============================================================================
;; Handler: :task/complete
;; =============================================================================

(defn- handle-task-complete
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
;; Handler: :ling/started (EVENTS-03)
;; =============================================================================

(defn- handle-ling-started
  "Handler for :ling/started events.
   
   Called when a ling is spawned and initialized. Registers the ling
   in DataScript for swarm coordination and logs the spawn.
   
   Expects event data:
   {:slave-id  \"swarm-worker-123\"
    :name      \"task-name\"
    :presets   [\"tdd\" \"clarity\"]
    :cwd       \"/path/to/project\"
    :depth     1}
   
   Produces effects:
   - :log         - Log spawn message
   - :ds-transact - Register ling in DataScript"
  [_coeffects [_ {:keys [slave-id name presets cwd depth]}]]
  (let [effective-id (or slave-id name "unknown-ling")
        effective-depth (or depth 1)]
    {:log {:level :info
           :message (str "Ling started: " effective-id
                         (when (seq presets) (str " presets=" presets)))}
     :ds-transact [{:slave/id effective-id
                    :slave/name (or name effective-id)
                    :slave/status :starting
                    :slave/depth effective-depth
                    :slave/tasks-completed 0
                    :slave/created-at (java.util.Date.)
                    :slave/presets (vec (or presets []))
                    :slave/cwd cwd}]}))

;; =============================================================================
;; Handler: :ling/completed (EVENTS-03)
;; =============================================================================

(defn- handle-ling-completed
  "Handler for :ling/completed events.
   
   Called when a ling finishes all work. Broadcasts completion to hivemind
   and dispatches :session/end for auto-wrap behavior.
   
   Expects event data:
   {:slave-id \"swarm-worker-123\"
    :result   \"success\" | \"failure\" | any
    :reason   \"task completed\" | \"error\" | etc}
   
   Produces effects:
   - :log      - Log completion message
   - :shout    - Broadcast completion to hivemind coordinator
   - :dispatch - Dispatch :session/end for auto-wrap"
  [_coeffects [_ {:keys [slave-id result reason]}]]
  (let [effective-id (or slave-id
                         (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                         "unknown-ling")]
    {:log {:level :info
           :message (str "Ling completed: " effective-id
                         (when result (str " result=" result)))}
     :shout {:agent-id effective-id
             :event-type :completed
             :data {:result result
                    :reason (or reason "task-finished")}}
     :dispatch [:session/end {:slave-id effective-id
                              :reason (or reason "ling-completed")}]}))

;; =============================================================================
;; Handler: :session/end (EVENTS-06)
;; =============================================================================

(defn- handle-session-end
  "Handler for :session/end events.
   
   Emits :wrap-notify effect for coordinator permeation. This enables
   the auto-wrap convergence pattern where lings' session learnings
   are automatically queued for coordinator to permeate.
   
   Expects event data:
   {:slave-id \"swarm-worker-123\"
    :reason   \"auto-wrap\" | \"ling-completed\" | etc}
   
   Produces effects:
   - :log         - Log session end
   - :wrap-notify - Queue wrap for coordinator permeation"
  [coeffects [_ {:keys [slave-id reason]}]]
  (let [agent-id (or slave-id
                     (get-in coeffects [:agent-context :agent-id])
                     (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                     "unknown-agent")
        session-id (or (get-in coeffects [:agent-context :session-id])
                       (str "session:" (java.time.LocalDate/now) ":" agent-id))]
    {:log {:level :info
           :message (str "Session ending: " agent-id)}
     :wrap-notify {:agent-id agent-id
                   :session-id session-id
                   :stats {:triggered-by "auto-wrap"
                           :reason (or reason "session-end")}}}))

;; =============================================================================
;; Handler: :kanban/done (EVENTS-09)
;; =============================================================================

(defn- handle-kanban-done
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
;; Handler: :task/shout-complete (P5-1 - ported from hooks/handlers.clj)
;; =============================================================================

(defn- handle-task-shout-complete
  "Handler for :task/shout-complete events.
   
   Ported from hooks/handlers.clj shout-completion. Broadcasts task
   completion via hivemind channel with a human-readable message.
   
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
;; Handler: :session/wrap (P5-3 - ported from hooks/handlers.clj)
;; =============================================================================

(defn- handle-session-wrap
  "Handler for :session/wrap events.
   
   Ported from hooks/handlers.clj run-wrap. Triggers the wrap workflow
   at session end for crystallizing session learnings.
   
   Expects event data:
   {:session-id  \"session-abc\"
    :project     \"hive-mcp\"
    :start-time  \"2024-01-01T10:00:00Z\"}
   
   Produces effects:
   - :log          - Log wrap trigger
   - :run-workflow - Trigger :wrap workflow with params"
  [coeffects [_ {:keys [session-id project start-time]}]]
  (let [agent-id (or (get-in coeffects [:agent-context :agent-id])
                     (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                     "unknown-agent")
        effective-session-id (or session-id
                                 (str "session:" (java.time.LocalDate/now) ":" agent-id))]
    {:log {:level :info
           :message (str "Triggering wrap workflow for session: " effective-session-id)}
     :run-workflow {:workflow :wrap
                    :params {:session-id effective-session-id
                             :project project
                             :start-time start-time}}}))

;; =============================================================================
;; Handler: :git/commit-modified (P5-2 - ported from hooks/handlers.clj)
;; =============================================================================

(defn- task-title-or-default
  "Extract task title or generate default from task-id."
  [{:keys [title task-id]}]
  (or title (str "Task " (or task-id "unknown"))))

(defn- handle-git-commit-modified
  "Handler for :git/commit-modified events.
   
   Ported from hooks/handlers.clj commit-if-files-modified.
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
;; Handler: :kanban/sync (P5-4 - ported from hooks/handlers.clj)
;; =============================================================================

(defn- handle-kanban-sync
  "Handler for :kanban/sync events.
   
   Ported from hooks/handlers.clj sync-kanban.
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

(defonce ^:private *registered (atom false))

(defn register-handlers!
  "Register all event handlers. Call at startup.
   
   Safe to call multiple times - only registers once.
   
   Registers:
   - :task/complete       - Signal task completion to hivemind
   - :task/shout-complete - Broadcast completion with message (P5-1)
   - :git/commit-modified - Git commit if files changed (P5-2)
   - :ling/started        - Ling spawned (EVENTS-03)
   - :ling/completed      - Ling finished (EVENTS-03)
   - :session/end         - Session ending, auto-wrap (EVENTS-06)
   - :session/wrap        - Trigger wrap workflow (P5-3)
   - :kanban/sync         - Sync kanban at session end (P5-4)
   - :kanban/done         - Kanban task completed (EVENTS-09)
   
   Returns true if handlers were registered, false if already registered."
  []
  (when-not @*registered
    ;; :task/complete - Signal task completion to hivemind
    (ev/reg-event :task/complete
                  [interceptors/debug]
                  handle-task-complete)

    ;; :task/shout-complete - Broadcast with message (P5-1)
    (ev/reg-event :task/shout-complete
                  [interceptors/debug]
                  handle-task-shout-complete)

    ;; :git/commit-modified - Git commit if files changed (P5-2)
    (ev/reg-event :git/commit-modified
                  [interceptors/debug]
                  handle-git-commit-modified)

    ;; :ling/started - Register ling in DataScript (EVENTS-03)
    (ev/reg-event :ling/started
                  [interceptors/debug]
                  handle-ling-started)

    ;; :ling/completed - Shout + dispatch :session/end (EVENTS-03)
    (ev/reg-event :ling/completed
                  [interceptors/debug]
                  handle-ling-completed)

    ;; :session/end - Auto-wrap trigger (EVENTS-06)
    (ev/reg-event :session/end
                  [interceptors/debug]
                  handle-session-end)

    ;; :session/wrap - Trigger wrap workflow (P5-3)
    (ev/reg-event :session/wrap
                  [interceptors/debug]
                  handle-session-wrap)

    ;; :kanban/sync - Sync kanban at session end (P5-4)
    (ev/reg-event :kanban/sync
                  [interceptors/debug]
                  handle-kanban-sync)

    ;; :kanban/done - Progress note + DataScript (EVENTS-09)
    (ev/reg-event :kanban/done
                  [interceptors/debug]
                  handle-kanban-done)

    (reset! *registered true)
    (println "[hive-events] Handlers registered: :task/complete :task/shout-complete :git/commit-modified :ling/started :ling/completed :session/end :session/wrap :kanban/sync :kanban/done")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
