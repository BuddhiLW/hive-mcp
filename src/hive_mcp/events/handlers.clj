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
;; Handler: :crystal/wrap-request (Option A - Unified wrap path)
;; =============================================================================

(defn- format-session-summary
  "Format session summary content from wrap data."
  [{:keys [accomplishments decisions conventions in-progress next-actions date]}]
  (let [date-str (or date (str (java.time.LocalDate/now)))]
    (str "## Session Summary: " date-str "\n\n"
         "### Completed\n"
         (if (seq accomplishments)
           (clojure.string/join "\n" (map #(str "- [x] " %) accomplishments))
           "- (none)")
         "\n\n### Decisions Made\n"
         (if (seq decisions)
           (clojure.string/join "\n" (map #(str "- " %) decisions))
           "- (none)")
         "\n\n### Conventions Added\n"
         (if (seq conventions)
           (clojure.string/join "\n" (map #(str "- " %) conventions))
           "- (none)")
         "\n\n### In Progress\n"
         (if (seq in-progress)
           (clojure.string/join "\n" (map #(str "- [ ] " %) in-progress))
           "- (none)")
         "\n\n### Next Actions\n"
         (if (seq next-actions)
           (clojure.string/join "\n" (map #(str "- " %) next-actions))
           "- (none)"))))

(defn- handle-crystal-wrap-request
  "Handler for :crystal/wrap-request events.
   
   Option A implementation - Unified wrap path. Receives wrap data from elisp
   via channel, stores to memory, and emits wrap_notify for Crystal Convergence.
   
   Expects event data:
   {:accomplishments  [\"Task 1\" \"Task 2\"]     ; list of completed tasks
    :decisions        [\"Decision 1\"]           ; list of decisions made
    :conventions      [\"Convention 1\"]         ; list of conventions
    :in-progress      [\"WIP task\"]             ; list of in-progress items
    :next-actions     [\"Next 1\"]               ; list of next session priorities
    :completed-tasks  [\"kanban-id-1\"]          ; kanban task IDs to mark done
    :project          \"hive-mcp\"}              ; project name for scoping
   
   Produces effects:
   - :log          - Log wrap request
   - :memory-write - Store session summary as note
   - :memory-write - Store each decision (if any)
   - :memory-write - Store each convention (if any)
   - :wrap-notify  - Queue for coordinator permeation
   - :shout        - Broadcast completion to hivemind"
  [coeffects [_ {:keys [accomplishments decisions conventions in-progress
                        next-actions completed-tasks project] :as data}]]
  (let [agent-id (or (get-in coeffects [:agent-context :agent-id])
                     (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                     "unknown-agent")
        session-id (str "session:" (java.time.LocalDate/now) ":" agent-id)
        date-str (str (java.time.LocalDate/now))
        summary-content (format-session-summary (assoc data :date date-str))
        ;; Build effects map
        base-effects {:log {:level :info
                            :message (str "Wrap request from " agent-id
                                          ": " (count accomplishments) " accomplishments, "
                                          (count decisions) " decisions, "
                                          (count conventions) " conventions")}}
        ;; Add session summary note effect
        summary-effect {:memory-write {:type "note"
                                       :content summary-content
                                       :tags ["session-summary" "wrap" "full-summary"]
                                       :duration "short"
                                       :directory project}}
        ;; Add wrap-notify effect for Crystal Convergence
        notify-effect {:wrap-notify {:agent-id agent-id
                                     :session-id session-id
                                     :stats {:accomplishments (count accomplishments)
                                             :decisions (count decisions)
                                             :conventions (count conventions)}}}
        ;; Add shout effect
        shout-effect {:shout {:agent-id agent-id
                              :event-type :completed
                              :message (str "Session wrapped: " (count decisions) " decisions, "
                                            (count conventions) " conventions")}}]
    ;; Merge all effects
    (merge base-effects summary-effect notify-effect shout-effect)))

;; =============================================================================
;; Handler: :wave/start (dispatch_drone_wave lifecycle)
;; =============================================================================

(defn- handle-wave-start
  "Handler for :wave/start events.

   Called when a drone wave execution begins. Publishes event to channel
   for coordinator monitoring and logs the wave start.

   Expects event data:
   {:plan-id    \"plan-uuid\"
    :wave-id    \"wave-uuid\"
    :item-count 5}

   Produces effects:
   - :channel-publish - Broadcast wave-started to WebSocket clients
   - :log             - Log wave start message"
  [_coeffects [_ {:keys [plan-id wave-id item-count]}]]
  {:channel-publish {:event :wave-started
                     :data {:plan-id plan-id
                            :wave-id wave-id
                            :item-count item-count}}
   :log {:level :info
         :message (str "Wave started: " wave-id " with " item-count " items")}})

;; =============================================================================
;; Handler: :wave/item-done (dispatch_drone_wave lifecycle)
;; =============================================================================

(defn- handle-wave-item-done
  "Handler for :wave/item-done events.

   Called when a single drone task completes (success or failure).
   Publishes event to channel for real-time monitoring.

   Expects event data:
   {:item-id \"item-uuid\"
    :status  :completed | :failed
    :wave-id \"wave-uuid\"}

   Produces effects:
   - :channel-publish - Broadcast wave-item-done to WebSocket clients"
  [_coeffects [_ {:keys [item-id status wave-id]}]]
  {:channel-publish {:event :wave-item-done
                     :data {:item-id item-id
                            :status status
                            :wave-id wave-id}}})

;; =============================================================================
;; Handler: :wave/complete (dispatch_drone_wave lifecycle)
;; =============================================================================

(defn- handle-wave-complete
  "Handler for :wave/complete events.

   Called when all drone tasks in a wave have finished.
   Publishes completion event and logs results.

   Expects event data:
   {:plan-id  \"plan-uuid\"
    :wave-id  \"wave-uuid\"
    :results  {:completed N :failed M}}

   Produces effects:
   - :channel-publish - Broadcast wave-complete to WebSocket clients
   - :log             - Log wave completion message"
  [_coeffects [_ {:keys [plan-id wave-id results]}]]
  (let [{:keys [completed failed]} results]
    {:channel-publish {:event :wave-complete
                       :data {:plan-id plan-id
                              :wave-id wave-id
                              :results results}}
     :log {:level :info
           :message (str "Wave complete: " wave-id
                         " - " completed " completed, " failed " failed")}}))

;; =============================================================================
;; Handler: :ling/ready-for-wrap (Auto-wrap hook)
;; =============================================================================

(defn- handle-ling-ready-for-wrap
  "Handler for :ling/ready-for-wrap events.

   Called when auto-wrap hook detects a ling has completed work and
   is ready for session crystallization. Dispatches :session/wrap
   to trigger the wrap workflow.

   Expects event data:
   {:slave-id   \"swarm-worker-123\"
    :reason     \"task-completed\" | \"idle-detected\" | \"manual\"
    :session-id \"session:2026-01-14:worker-123\"}

   Produces effects:
   - :log      - Log auto-wrap trigger
   - :dispatch - Dispatch :session/wrap to run wrap workflow"
  [_coeffects [_ {:keys [slave-id reason session-id]}]]
  (let [effective-id (or slave-id
                         (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                         "unknown-ling")]
    {:log {:level :info
           :message (str "Auto-wrap triggered for " effective-id
                         " (reason: " (or reason "task-completed") ")")}
     :dispatch [:session/wrap {:session-id session-id
                               :slave-id effective-id
                               :triggered-by "auto-wrap"
                               :reason (or reason "task-completed")}]}))

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-handlers!
  "Register all event handlers. Call at startup.

   Safe to call multiple times - only registers once.

   Registers:
   - :task/complete         - Signal task completion to hivemind
   - :task/shout-complete   - Broadcast completion with message (P5-1)
   - :git/commit-modified   - Git commit if files changed (P5-2)
   - :ling/started          - Ling spawned (EVENTS-03)
   - :ling/completed        - Ling finished (EVENTS-03)
   - :ling/ready-for-wrap   - Auto-wrap hook on ling completion
   - :session/end           - Session ending, auto-wrap (EVENTS-06)
   - :session/wrap          - Trigger wrap workflow (P5-3)
   - :kanban/sync           - Sync kanban at session end (P5-4)
   - :kanban/done           - Kanban task completed (EVENTS-09)
   - :crystal/wrap-request  - Unified wrap path (Option A)
   - :wave/start            - Wave execution started
   - :wave/item-done        - Wave item completed/failed
   - :wave/complete         - Wave execution finished

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

    ;; :ling/ready-for-wrap - Auto-wrap hook on ling completion
    (ev/reg-event :ling/ready-for-wrap
                  [interceptors/debug]
                  handle-ling-ready-for-wrap)

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

    ;; :crystal/wrap-request - Unified wrap path (Option A)
    (ev/reg-event :crystal/wrap-request
                  [interceptors/debug]
                  handle-crystal-wrap-request)

    ;; :wave/start - Wave execution started (dispatch_drone_wave)
    (ev/reg-event :wave/start
                  [interceptors/debug]
                  handle-wave-start)

    ;; :wave/item-done - Wave item completed/failed (dispatch_drone_wave)
    (ev/reg-event :wave/item-done
                  [interceptors/debug]
                  handle-wave-item-done)

    ;; :wave/complete - Wave execution finished (dispatch_drone_wave)
    (ev/reg-event :wave/complete
                  [interceptors/debug]
                  handle-wave-complete)

    (reset! *registered true)
    (println "[hive-events] Handlers registered: :task/complete :task/shout-complete :git/commit-modified :ling/started :ling/completed :ling/ready-for-wrap :session/end :session/wrap :kanban/sync :kanban/done :crystal/wrap-request :wave/start :wave/item-done :wave/complete")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
