(ns hive-mcp.scheduler.dag-waves
  "DAGWave Scheduler — self-scheduling Forja Belt via KG dependencies.

   Uses KG `depends-on` edges as a DAG to determine task dispatch order.
   Automatically dispatches lings for ready tasks and re-dispatches when
   tasks complete, creating a self-scheduling execution loop.

   Data Flow:
   1. Coordinator feeds plan → plan-to-kanban → creates kanban tasks + KG depends-on edges
   2. DAGWave scheduler queries: kanban todos WHERE all depends-on targets have status=done
   3. Scheduler spawns lings for frontier tasks (up to max-slots)
   4. On :ling/completed event → mark kanban done → re-query frontier → auto-spawn
   5. Repeat until DAG empty or user says stop

   Key building blocks (all pre-existing):
   - plan/tool.clj: compute-waves (Kahn's algo), creates KG depends-on edges
   - edges.clj: get-edges-from with relation filter for :depends-on
   - agent/ling.clj: create-ling! with kanban-task-id linking
   - memory-kanban.clj: Chroma-based kanban CRUD
   - hivemind.clj: shout! for coordinator notifications
   - channel.clj: subscribe! for event listening (core.async pub/sub)

   SOLID: SRP - Pure scheduling logic, delegates to existing subsystems.
   CLARITY: L - Layers stay pure; scheduler only orchestrates."
  (:require [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.tools.memory.scope :as scope]
            [clojure.core.async :as async :refer [go-loop <! close!]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce dag-state
  (atom {:active    false
         :plan-id   nil
         :max-slots 5
         :wave-log  []
         :dispatched {}      ; {kanban-task-id -> ling-id}
         :completed  #{}     ; set of kanban-task-ids
         :failed     #{}     ; set of kanban-task-ids
         :opts       {}}))   ; original start opts

;; =============================================================================
;; Kanban Helpers
;; =============================================================================

(defn- get-kanban-todos
  "Get all kanban tasks with status 'todo' for the given project.
   Returns seq of {:id :title :status :priority}."
  [directory]
  (try
    (let [result (mem-kanban/handle-mem-kanban-list-slim
                  {:status "todo" :directory directory})]
      (when-not (:isError result)
        (let [parsed (json/read-str (:text result) :key-fn keyword)]
          (if (sequential? parsed) parsed []))))
    (catch Exception e
      (log/warn "Failed to get kanban todos:" (.getMessage e))
      [])))

(defn- get-kanban-task
  "Get a kanban task by ID from Chroma. Returns the entry map or nil."
  [task-id]
  (try
    (chroma/get-entry-by-id task-id)
    (catch Exception _
      nil)))

(defn- kanban-task-done?
  "Check if a kanban task has been completed (moved to done = deleted from Chroma).
   Returns true if the task no longer exists (deleted on done) or is in completed set."
  [task-id completed-set]
  (or (contains? completed-set task-id)
      (nil? (get-kanban-task task-id))))

(defn- move-kanban-done!
  "Move a kanban task to 'done' status.
   Note: In memory-kanban, moving to 'done' DELETES the entry."
  [task-id directory]
  (try
    (mem-kanban/handle-mem-kanban-move
     {:task_id task-id :new_status "done" :directory directory})
    (catch Exception e
      (log/warn "Failed to move kanban task to done:" task-id (.getMessage e)))))

;; =============================================================================
;; KG Dependency Helpers
;; =============================================================================

(defn- get-task-dependencies
  "Get the task IDs that a given task depends on (via KG :depends-on edges).
   
   A task-B --depends-on--> task-A means B depends on A completing first.
   So we look for outgoing :depends-on edges FROM this task.
   
   Returns set of task-ids that this task depends on."
  [task-id]
  (try
    (let [edges (kg-edges/get-edges-from task-id)
          depends-on-edges (filter #(= :depends-on (:kg-edge/relation %)) edges)]
      (set (map :kg-edge/to depends-on-edges)))
    (catch Exception e
      (log/warn "Failed to get dependencies for task:" task-id (.getMessage e))
      #{})))

;; =============================================================================
;; Core Pure Functions
;; =============================================================================

(defn find-ready-tasks
  "Find tasks whose ALL dependencies have been completed.
   
   A task is 'ready' when:
   1. It has kanban status 'todo'
   2. ALL of its :depends-on targets are in the completed set OR no longer exist in Chroma
   3. It is NOT already dispatched or completed
   
   Arguments:
     directory    - Working directory for kanban scoping
     completed    - Set of task IDs already completed
     dispatched   - Map of task-id -> ling-id (currently running)
     failed       - Set of task IDs that failed
   
   Returns vector of {:task-id :title :deps :dep-count}."
  [directory completed dispatched failed]
  (let [todos (get-kanban-todos directory)
        already-handled (into (set (keys dispatched))
                              (into completed failed))]
    (->> todos
         (remove #(contains? already-handled (:id %)))
         (keep (fn [task]
                 (let [task-id (:id task)
                       deps (get-task-dependencies task-id)
                       ;; A dep is satisfied if it's completed or no longer exists
                       all-deps-done? (every? #(kanban-task-done? % completed) deps)]
                   (when all-deps-done?
                     {:task-id task-id
                      :title   (:title task)
                      :deps    deps
                      :dep-count (count deps)}))))
         vec)))

;; =============================================================================
;; Stateful Dispatch Functions
;; =============================================================================

(defn dispatch-wave!
  "Dispatch lings for ready tasks up to available slots.
   
   Arguments:
     ready-tasks - Vector from find-ready-tasks
     max-slots   - Maximum concurrent lings
     opts        - Map with :cwd, :presets, :project-id
   
   Returns {:dispatched-count N :dispatched-tasks [...] :skipped-count N}."
  [ready-tasks max-slots opts]
  (let [{:keys [cwd presets project-id]} opts
        current-dispatched (:dispatched @dag-state)
        available-slots (max 0 (- max-slots (count current-dispatched)))
        tasks-to-dispatch (take available-slots ready-tasks)
        skipped (- (count ready-tasks) (count tasks-to-dispatch))]

    (when (pos? (count tasks-to-dispatch))
      (log/info "DAGWaves dispatching" (count tasks-to-dispatch)
                "tasks (slots:" available-slots "ready:" (count ready-tasks) ")"))

    (let [dispatched-results
          (doall
           (for [{:keys [task-id title]} tasks-to-dispatch]
             (try
               (let [safe-title (-> (or title "task")
                                    str/lower-case
                                    (str/replace #"[^a-z0-9]+" "-"))
                     truncated (subs safe-title 0 (min 30 (count safe-title)))
                     ling-id (str "swarm-dag-" truncated "-" (System/currentTimeMillis))
                     ;; Spawn ling with kanban-task-id linking
                     _ (ling/create-ling! ling-id
                                          {:cwd cwd
                                           :presets (or presets ["ling"])
                                           :project-id project-id
                                           :kanban-task-id task-id
                                           :task title})]
                 ;; Track dispatch in state
                 (swap! dag-state update :dispatched assoc task-id ling-id)
                 ;; Log wave progress
                 (swap! dag-state update :wave-log conj
                        {:event :dispatched
                         :task-id task-id
                         :ling-id ling-id
                         :title title
                         :timestamp (System/currentTimeMillis)})
                 {:task-id task-id :ling-id ling-id :status :dispatched})
               (catch Exception e
                 (log/error "Failed to dispatch ling for task:" task-id (.getMessage e))
                 ;; Mark as failed so we don't keep retrying
                 (swap! dag-state update :failed conj task-id)
                 {:task-id task-id :status :failed :error (.getMessage e)}))))]

      {:dispatched-count (count (filter #(= :dispatched (:status %)) dispatched-results))
       :dispatched-tasks dispatched-results
       :skipped-count skipped})))

;; =============================================================================
;; Completion Handler
;; =============================================================================

(defn on-ling-complete
  "Handle ling completion event. Called when a ling shouts :completed.
   
   1. Finds the kanban-task-id from the ling's DataScript record
   2. Marks the kanban task as done
   3. Removes from dispatched, adds to completed
   4. Re-queries find-ready-tasks
   5. Auto-dispatches newly unblocked tasks
   
   Arguments:
     event - Map with :agent-id, :project-id, :data
   
   Returns nil (side-effecting)."
  [{:keys [agent-id project-id data]}]
  (when (:active @dag-state)
    (let [;; Find the kanban-task-id for this ling
          slave (ds-queries/get-slave agent-id)
          kanban-task-id (:slave/kanban-task-id slave)
          directory (or (:slave/cwd slave)
                        (get-in @dag-state [:opts :cwd]))]

      (if-not kanban-task-id
        (log/debug "DAGWaves: ling" agent-id "completed but no kanban-task-id (not a DAG task)")

        ;; Only handle tasks that are in our dispatched set
        (when (contains? (:dispatched @dag-state) kanban-task-id)
          (log/info "DAGWaves: ling" agent-id "completed task" kanban-task-id)

          ;; Check if this is a success or failure
          (let [is-failure? (or (= (:event-type data) :error)
                                (= (:event-type data) :blocked)
                                (= (str (:result data)) "failure"))]

            (if is-failure?
              ;; Failed: move to failed set, don't mark kanban done
              (do
                (swap! dag-state (fn [s]
                                   (-> s
                                       (update :dispatched dissoc kanban-task-id)
                                       (update :failed conj kanban-task-id)
                                       (update :wave-log conj
                                               {:event :failed
                                                :task-id kanban-task-id
                                                :ling-id agent-id
                                                :timestamp (System/currentTimeMillis)}))))
                (log/warn "DAGWaves: task" kanban-task-id "FAILED via" agent-id))

              ;; Success: mark done, dispatch next wave
              (do
                ;; Move kanban to done
                (move-kanban-done! kanban-task-id directory)

                ;; Update state
                (swap! dag-state (fn [s]
                                   (-> s
                                       (update :dispatched dissoc kanban-task-id)
                                       (update :completed conj kanban-task-id)
                                       (update :wave-log conj
                                               {:event :completed
                                                :task-id kanban-task-id
                                                :ling-id agent-id
                                                :timestamp (System/currentTimeMillis)}))))

                ;; Auto-dispatch next wave
                (let [ready (find-ready-tasks directory
                                              (:completed @dag-state)
                                              (:dispatched @dag-state)
                                              (:failed @dag-state))
                      max-slots (:max-slots @dag-state)]
                  (when (seq ready)
                    (log/info "DAGWaves: auto-dispatching" (count ready)
                              "newly ready tasks after" kanban-task-id)
                    (dispatch-wave! ready max-slots (:opts @dag-state)))

                  ;; Check if DAG is complete
                  (when (and (empty? ready)
                             (empty? (:dispatched @dag-state)))
                    (log/info "DAGWaves: ALL TASKS COMPLETE for plan" (:plan-id @dag-state))
                    (hivemind/shout! "coordinator" :completed
                                     {:task (str "DAGWaves plan " (:plan-id @dag-state))
                                      :message (str "All tasks complete. "
                                                    (count (:completed @dag-state)) " succeeded, "
                                                    (count (:failed @dag-state)) " failed.")})
                    (swap! dag-state assoc :active false)))))))))))

;; =============================================================================
;; Channel Subscription (core.async pub/sub)
;; =============================================================================

(defonce ^:private dag-sub-channel (atom nil))

(defn- start-event-listener!
  "Start a go-loop that listens to :hivemind-completed events from the channel
   pub/sub system and routes them to on-ling-complete.
   
   channel/subscribe! returns a core.async channel that receives events
   matching the given :type keyword."
  []
  (let [ch (channel/subscribe! :hivemind-completed)]
    (reset! dag-sub-channel ch)
    (go-loop []
      (when-let [event (<! ch)]
        (when (:active @dag-state)
          (try
            (on-ling-complete {:agent-id  (:agent-id event)
                               :project-id (:project-id event)
                               :data       (:data event)})
            (catch Exception e
              (log/error "DAGWaves event listener error:" (.getMessage e)))))
        (recur)))
    (log/info "DAGWaves event listener started")))

(defn- stop-event-listener!
  "Stop the event listener go-loop by closing the subscription channel."
  []
  (when-let [ch @dag-sub-channel]
    (try
      (channel/unsubscribe! :hivemind-completed ch)
      (catch Exception _
        ;; Fallback: just close the channel
        (try (close! ch) (catch Exception _))))
    (reset! dag-sub-channel nil)
    (log/info "DAGWaves event listener stopped")))

;; =============================================================================
;; Lifecycle Functions
;; =============================================================================

(defn start-dag!
  "Initialize and start the DAG scheduler for a plan.
   
   Arguments:
     plan-id - Memory entry ID of the plan (already converted to kanban via plan-to-kanban)
     opts    - Map with:
               :max-slots  - Max concurrent lings (default: 5)
               :cwd        - Working directory (required)
               :presets    - Ling presets (default: [\"ling\"])
               :project-id - Project ID (auto-detected from cwd if nil)
   
   Returns {:started true :plan-id ... :initial-dispatch ...}
   
   Side effects:
   - Initializes dag-state atom
   - Subscribes to channel events for completion detection
   - Dispatches first wave of ready tasks"
  [plan-id opts]
  (when (:active @dag-state)
    (throw (ex-info "DAG already active. Call stop-dag! first."
                    {:current-plan (:plan-id @dag-state)})))

  (let [{:keys [max-slots cwd presets project-id]
         :or {max-slots 5
              presets ["ling"]}} opts
        effective-project-id (or project-id
                                 (when cwd (scope/get-current-project-id cwd)))]

    ;; Initialize state
    (reset! dag-state
            {:active     true
             :plan-id    plan-id
             :max-slots  max-slots
             :wave-log   []
             :dispatched {}
             :completed  #{}
             :failed     #{}
             :opts       {:cwd cwd
                          :presets presets
                          :project-id effective-project-id}})

    ;; Start event listener for completion detection
    (start-event-listener!)

    ;; Shout start
    (hivemind/shout! "coordinator" :started
                     {:task (str "DAGWaves scheduler for plan " plan-id)
                      :message (str "Max slots: " max-slots " project: " effective-project-id)})

    ;; Find and dispatch first wave
    (let [ready (find-ready-tasks cwd #{} {} #{})
          result (when (seq ready)
                   (dispatch-wave! ready max-slots (:opts @dag-state)))]

      (log/info "DAGWaves started. Plan:" plan-id
                "Ready tasks:" (count ready)
                "Dispatched:" (or (:dispatched-count result) 0))

      {:started true
       :plan-id plan-id
       :max-slots max-slots
       :ready-count (count ready)
       :initial-dispatch result})))

(defn stop-dag!
  "Stop the DAG scheduler. Deregisters event handlers but does NOT kill active lings.
   
   Returns final stats map."
  []
  (let [state @dag-state]
    ;; Stop event listener
    (stop-event-listener!)

    ;; Mark as inactive
    (swap! dag-state assoc :active false)

    (log/info "DAGWaves stopped. Plan:" (:plan-id state)
              "Completed:" (count (:completed state))
              "Failed:" (count (:failed state))
              "Still dispatched:" (count (:dispatched state)))

    {:stopped true
     :plan-id (:plan-id state)
     :completed-count (count (:completed state))
     :failed-count (count (:failed state))
     :dispatched-count (count (:dispatched state))
     :wave-log (:wave-log state)}))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn dag-status
  "Get current DAG progress.
   
   Returns map with:
   - :active        - Whether scheduler is running
   - :plan-id       - Plan memory entry ID
   - :completed     - Count of completed tasks
   - :failed        - Count of failed tasks  
   - :dispatched    - Count of currently running tasks
   - :ready         - Count of tasks ready to dispatch
   - :wave-log      - Recent dispatch/completion events"
  []
  (let [state @dag-state
        directory (get-in state [:opts :cwd])
        ready (when (:active state)
                (try
                  (find-ready-tasks directory
                                    (:completed state)
                                    (:dispatched state)
                                    (:failed state))
                  (catch Exception _ [])))]
    {:active       (:active state)
     :plan-id      (:plan-id state)
     :max-slots    (:max-slots state)
     :completed    (count (:completed state))
     :failed       (count (:failed state))
     :dispatched   (count (:dispatched state))
     :ready        (count (or ready []))
     :completed-ids (:completed state)
     :failed-ids   (:failed state)
     :dispatched-map (:dispatched state)
     :wave-log     (take-last 20 (:wave-log state))}))

;; =============================================================================
;; Comment / REPL Usage
;; =============================================================================

(comment
  ;; Typical usage:
  ;; 1. Coordinator creates plan and runs plan-to-kanban
  ;; 2. Start the DAG scheduler:
  (start-dag! "plan-memory-id-here"
              {:cwd "/home/lages/PP/hive/hive-mcp"
               :max-slots 5
               :presets ["ling"]})

  ;; 3. Monitor progress:
  (dag-status)

  ;; 4. Stop when done:
  (stop-dag!)

  ;; Manual completion test (simulates a ling finishing):
  (on-ling-complete {:agent-id "swarm-test-123"
                     :project-id "hive-mcp"
                     :data {:result "success"}})

  ;; Find ready tasks manually:
  (find-ready-tasks "/home/lages/PP/hive/hive-mcp" #{} {} #{}))
