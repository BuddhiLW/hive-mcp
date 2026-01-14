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
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-handlers!
  "Register all event handlers. Call at startup.
   
   Safe to call multiple times - only registers once.
   
   Registers:
   - :task/complete  - Signal task completion to hivemind
   - :ling/started   - Ling spawned (EVENTS-03)
   - :ling/completed - Ling finished (EVENTS-03)
   - :session/end    - Session ending, auto-wrap (EVENTS-06)
   - :kanban/done    - Kanban task completed (EVENTS-09)
   
   Returns true if handlers were registered, false if already registered."
  []
  (when-not @*registered
    ;; :task/complete - Signal task completion to hivemind
    (ev/reg-event :task/complete
                  [interceptors/debug]
                  handle-task-complete)

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

    ;; :kanban/done - Progress note + DataScript (EVENTS-09)
    (ev/reg-event :kanban/done
                  [interceptors/debug]
                  handle-kanban-done)

    (reset! *registered true)
    (println "[hive-events] Handlers registered: :task/complete :ling/started :ling/completed :session/end :kanban/done")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
