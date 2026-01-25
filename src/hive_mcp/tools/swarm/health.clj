(ns hive-mcp.tools.swarm.health
  "Drone health monitoring - detect stuck/failed drones and auto-recover.

   Provides:
   - Heartbeat tracking for active drones
   - Stuck drone detection with configurable timeouts
   - Auto-recovery actions (kill, release claims, retry)
   - Wave health dashboard MCP tool

   TIMEOUT THRESHOLDS:
   - Alert: 2 minutes without progress
   - Kill: 5 minutes without progress

   CLARITY: T - Telemetry first (heartbeat monitoring)
   CLARITY: Y - Yield safe failure (auto-recovery)
   SOLID: SRP - Single responsibility for drone health."
  (:require [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.events.core :as ev]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.telemetry.prometheus :as prom]
            [clojure.core.async :as async :refer [go-loop timeout]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Configuration Constants
;;; =============================================================================

(def ^:const check-interval-ms
  "How often to check drone health (30 seconds)."
  30000)

(def ^:const alert-timeout-ms
  "Alert if no progress for 2 minutes."
  120000)

(def ^:const kill-timeout-ms
  "Auto-kill after 5 minutes of no progress."
  300000)

(def ^:const claim-ttl-ms
  "Claims expire after 5 minutes of no heartbeat."
  300000)

(def ^:const max-retry-attempts
  "Maximum retry attempts before escalation to ling."
  3)

;;; =============================================================================
;;; Heartbeat State
;;; =============================================================================

;; Map of drone-id -> {:last-beat timestamp :status :active/:alert/:stuck :wave-id :task}
(defonce drone-heartbeats (atom {}))

;;; =============================================================================
;;; Retry Queue State (CLARITY-Y: Graceful degradation via retry)
;;; =============================================================================

;; Queue of tasks to retry: [{:drone-id :task :files :attempt :wave-id :queued-at}]
(defonce retry-queue (atom []))

;; Flag to control the background monitoring loop
(defonce monitor-running? (atom false))

;; Channel to stop the monitoring loop
(defonce monitor-control-chan (atom nil))

;;; =============================================================================
;;; Heartbeat Tracking
;;; =============================================================================

(defn record-heartbeat!
  "Record a heartbeat for an active drone.

   Arguments:
     drone-id - Unique drone identifier
     opts     - Optional map with :wave-id :task :files :status :refresh-claims?

   Updates the heartbeat timestamp and any provided metadata.
   Called during drone execution to indicate progress.
   Stores task/files info for retry queue in case of stuck recovery.
   Optionally refreshes claim timestamps to prevent stale expiration.

   CLARITY-T: Telemetry first - observable drone activity.
   CLARITY-Y: Refreshes claims to prevent false-positive TTL expiration."
  [drone-id & [{:keys [wave-id task files status refresh-claims?]
                :or {refresh-claims? true}}]]
  (let [now (System/currentTimeMillis)
        had-files? (boolean (seq (get-in @drone-heartbeats [drone-id :files])))]
    (swap! drone-heartbeats update drone-id
           (fn [current]
             (merge (or current {})
                    {:last-beat now
                     :status (or status :active)}
                    (when wave-id {:wave-id wave-id})
                    (when task {:task task})
                    (when files {:files files}))))
    ;; Refresh claim timestamps to prevent TTL expiration
    ;; Only do this periodically (when claims exist) to avoid unnecessary DB ops
    (when (and refresh-claims? (or files had-files?))
      (try
        (when-let [claim-files (or files (get-in @drone-heartbeats [drone-id :files]))]
          (doseq [f claim-files]
            (lings/refresh-claim! f)))
        (catch Exception e
          (log/debug "Could not refresh claims:" (.getMessage e)))))
    (log/debug "Heartbeat recorded:" drone-id)))

(defn clear-heartbeat!
  "Remove heartbeat tracking for a drone (called on completion/failure).

   Arguments:
     drone-id - Drone identifier to clear"
  [drone-id]
  (swap! drone-heartbeats dissoc drone-id)
  (log/debug "Heartbeat cleared:" drone-id))

(defn get-heartbeat
  "Get heartbeat info for a specific drone."
  [drone-id]
  (get @drone-heartbeats drone-id))

(defn get-all-heartbeats
  "Get all active drone heartbeats."
  []
  @drone-heartbeats)

(defn refresh-drone-claims!
  "Refresh claim timestamps for a drone's files.

   Called automatically when heartbeat is recorded to keep claims fresh.
   Prevents claim TTL expiration for active drones.

   Arguments:
     drone-id - Drone identifier

   CLARITY-Y: Prevents false-positive stale claim detection."
  [drone-id]
  (when-let [heartbeat (get-heartbeat drone-id)]
    (when-let [files (:files heartbeat)]
      (doseq [f files]
        (try
          (lings/refresh-claim! f)
          (catch Exception e
            (log/debug "Could not refresh claim for" f (.getMessage e))))))))

;;; =============================================================================
;;; Retry Queue Management (CLARITY-Y: Graceful degradation)
;;; =============================================================================

(defn queue-for-retry!
  "Queue a failed task for retry with attempt tracking.

   Arguments:
     task-info - Map with :drone-id :task :files :wave-id

   Returns:
     Updated attempt count (1 if first retry, 2 if second, etc)"
  [{:keys [drone-id task files wave-id]}]
  (let [existing (->> @retry-queue
                      (filter #(= (:task %) task))
                      first)
        attempt (if existing
                  (inc (:attempt existing))
                  1)]
    (if (>= attempt max-retry-attempts)
      ;; Max retries reached - escalate to ling
      (do
        (log/error "Max retries reached for task, escalating to ling"
                   {:drone-id drone-id :task task :attempts attempt})
        (prom/inc-events-total! :drone/retry-escalated :error)
        ;; Remove from retry queue
        (swap! retry-queue #(vec (remove (fn [t] (= (:task t) task)) %)))
        {:escalated true :attempts attempt})
      ;; Queue for retry
      (do
        (swap! retry-queue
               (fn [q]
                 (let [filtered (vec (remove #(= (:task %) task) q))]
                   (conj filtered
                         {:drone-id drone-id
                          :task task
                          :files files
                          :wave-id wave-id
                          :attempt attempt
                          :queued-at (System/currentTimeMillis)}))))
        (log/info "Queued task for retry" {:drone-id drone-id :attempt attempt :task (subs task 0 (min 50 (count task)))})
        (prom/inc-events-total! :drone/retry-queued :info)
        {:queued true :attempts attempt}))))

(defn get-retry-queue
  "Get current retry queue."
  []
  @retry-queue)

(defn pop-retry-task!
  "Pop the next task from the retry queue (FIFO).
   Returns the task map or nil if queue is empty."
  []
  (let [task (first @retry-queue)]
    (when task
      (swap! retry-queue #(vec (rest %)))
      task)))

(defn clear-retry-queue!
  "Clear the retry queue. For testing or manual reset."
  []
  (reset! retry-queue [])
  (log/info "Retry queue cleared"))

;;; =============================================================================
;;; Stuck Detection
;;; =============================================================================

(defn- classify-drone-health
  "Classify a drone's health based on time since last heartbeat.

   Returns :active, :alert, or :stuck."
  [last-beat-ms now-ms]
  (let [elapsed (- now-ms last-beat-ms)]
    (cond
      (> elapsed kill-timeout-ms) :stuck
      (> elapsed alert-timeout-ms) :alert
      :else :active)))

(defn drone-stuck?
  "Check if a drone is stuck (no heartbeat for kill-timeout-ms).

   Arguments:
     drone-id   - Drone identifier
     timeout-ms - Optional custom timeout (default: kill-timeout-ms)

   Returns true if drone should be considered stuck."
  ([drone-id]
   (drone-stuck? drone-id kill-timeout-ms))
  ([drone-id timeout-ms]
   (let [heartbeat (get-heartbeat drone-id)]
     (if-not heartbeat
       false ; No heartbeat means drone not tracked
       (> (- (System/currentTimeMillis) (:last-beat heartbeat)) timeout-ms)))))

(defn check-all-drones
  "Check health of all tracked drones.

   Returns map with:
     :active - Vector of healthy drone-ids
     :alert  - Vector of drones needing attention
     :stuck  - Vector of stuck drone-ids needing recovery"
  []
  (let [now (System/currentTimeMillis)
        heartbeats @drone-heartbeats
        classified (reduce-kv
                    (fn [acc drone-id {:keys [last-beat wave-id task]}]
                      (let [health (classify-drone-health last-beat now)]
                        (update acc health conj
                                {:drone-id drone-id
                                 :last-beat last-beat
                                 :elapsed-ms (- now last-beat)
                                 :wave-id wave-id
                                 :task task})))
                    {:active [] :alert [] :stuck []}
                    heartbeats)]
    classified))

;;; =============================================================================
;;; Recovery Actions
;;; =============================================================================

(defn- release-drone-claims!
  "Release any file claims held by a drone.

   Uses the coordinator to find and release claims associated with the drone's task."
  [drone-id]
  (try
    ;; Try to find task-id from drone-id pattern
    (let [task-id (str "task-" drone-id)]
      (coordinator/release-task-claims! task-id)
      (log/info "Released claims for stuck drone:" drone-id))
    (catch Exception e
      (log/warn "Failed to release claims for drone:" drone-id (.getMessage e)))))

(defn- kill-stuck-drone!
  "Kill a drone process via DataScript (mark as terminated).

   Arguments:
     drone-id - ID of the drone to kill

   Returns :killed or :not-found"
  [drone-id]
  (try
    (when (ds/get-slave drone-id)
      (ds/remove-slave! drone-id)
      (log/info "Killed stuck drone:" drone-id)
      :killed)
    (catch Exception e
      (log/error "Failed to kill drone:" drone-id (.getMessage e))
      :error)))

(defn recover-stuck-drone!
  "Recover from a stuck drone.

   Actions:
   1. Release file claims
   2. Kill the drone process
   3. Emit recovery event
   4. Clear heartbeat tracking
   5. Queue task for retry (if retry? true and task info available)

   Arguments:
     drone-id - ID of the stuck drone
     opts     - Optional map with :retry? :wave-id

   Returns map with recovery status and retry info."
  [drone-id & [{:keys [retry? wave-id]}]]
  (log/warn "Recovering stuck drone:" drone-id {:retry? retry? :wave-id wave-id})

  ;; Get heartbeat info before clearing (for retry task info)
  (let [heartbeat-info (get-heartbeat drone-id)]

    ;; 1. Release file claims first
    (release-drone-claims! drone-id)

    ;; 2. Kill the drone
    (let [kill-result (kill-stuck-drone! drone-id)]

      ;; 3. Emit recovery event
      (ev/dispatch [:drone/recovered {:drone-id drone-id
                                      :kill-result kill-result
                                      :wave-id wave-id}])

      ;; 4. Clear heartbeat
      (clear-heartbeat! drone-id)

      ;; 5. Queue for retry if requested and task info available
      (let [retry-result (when (and retry? (:task heartbeat-info))
                           (queue-for-retry! {:drone-id drone-id
                                              :task (:task heartbeat-info)
                                              :files (:files heartbeat-info)
                                              :wave-id (or wave-id (:wave-id heartbeat-info))}))]

        ;; 6. Record for telemetry
        (prom/inc-events-total! :drone/stuck-recovered :warn)

        (cond-> {:drone-id drone-id
                 :status :recovered
                 :kill-result kill-result
                 :claims-released true}
          retry-result (assoc :retry-info retry-result))))))

;;; =============================================================================
;;; Background Monitoring Loop
;;; =============================================================================

(defn- check-and-release-stale-claims!
  "Check for stale claims and auto-release them.

   Uses claim-ttl-ms (5 minutes) as the threshold.
   Emits telemetry event for each released claim.

   CLARITY-Y: Graceful degradation via automatic claim cleanup."
  []
  (let [stale-claims (lings/get-stale-claims claim-ttl-ms)]
    (when (seq stale-claims)
      (log/warn "Found stale claims to auto-release:" (count stale-claims)
                {:files (mapv :file stale-claims)})
      (doseq [{:keys [file slave-id age-minutes]} stale-claims]
        (lings/release-claim! file)
        (prom/inc-events-total! :claim/expired :warn)
        (log/info "Auto-released stale claim:" file "after" age-minutes "minutes"
                  {:owner slave-id})))))

(defn- run-health-check!
  "Execute one health check cycle.

   Called periodically by the monitoring loop.

   Actions:
   1. Alert on drones approaching timeout
   2. Auto-recover stuck drones (with retry queue)
   3. Check and release stale claims (CLAIM TTL)"
  []
  (let [{:keys [alert stuck]} (check-all-drones)]

    ;; 1. Log alerts
    (doseq [{:keys [drone-id elapsed-ms]} alert]
      (log/warn "Drone alert - no progress for" (/ elapsed-ms 1000) "seconds:" drone-id)
      ;; Update status to alert
      (swap! drone-heartbeats assoc-in [drone-id :status] :alert))

    ;; 2. Auto-recover stuck drones with retry enabled
    (doseq [{:keys [drone-id wave-id elapsed-ms]} stuck]
      (log/error "Drone stuck - auto-recovering after" (/ elapsed-ms 1000) "seconds:" drone-id)
      (recover-stuck-drone! drone-id {:retry? true :wave-id wave-id}))

    ;; 3. Check and release stale claims (CLAIM TTL - ADR Phase 1)
    (try
      (check-and-release-stale-claims!)
      (catch Exception e
        (log/error e "Error checking stale claims")))))

(defn start-monitoring!
  "Start the background health monitoring loop.

   Checks drone health every check-interval-ms.
   Idempotent - safe to call multiple times."
  []
  (when-not @monitor-running?
    (reset! monitor-running? true)
    (let [ctrl-chan (async/chan)]
      (reset! monitor-control-chan ctrl-chan)

      (go-loop []
        (let [[_ ch] (async/alts! [ctrl-chan (timeout check-interval-ms)])]
          (when-not (= ch ctrl-chan)
            ;; Not cancelled - run health check
            (try
              (run-health-check!)
              (catch Exception e
                (log/error e "Error in drone health check")))
            (recur))))

      (log/info "Drone health monitoring started (interval:" (/ check-interval-ms 1000) "s)"))))

(defn stop-monitoring!
  "Stop the background health monitoring loop."
  []
  (when @monitor-running?
    (when-let [ctrl-chan @monitor-control-chan]
      (async/close! ctrl-chan))
    (reset! monitor-running? false)
    (reset! monitor-control-chan nil)
    (log/info "Drone health monitoring stopped")))

;;; =============================================================================
;;; Wave Health Dashboard
;;; =============================================================================

(defn get-wave-health
  "Get health status for all drones in a wave.

   Arguments:
     wave-id - Optional wave ID to filter by

   Returns map with:
     :total-drones   - Number of tracked drones
     :active         - Healthy drones
     :alert          - Drones needing attention
     :stuck          - Stuck drones
     :retry-queue    - Tasks queued for retry
     :monitoring?    - Whether background monitoring is active"
  [& [wave-id]]
  (let [{:keys [active alert stuck]} (check-all-drones)
        filter-by-wave (fn [drones]
                         (if wave-id
                           (filterv #(= wave-id (:wave-id %)) drones)
                           drones))
        queue @retry-queue
        filtered-queue (if wave-id
                         (filterv #(= wave-id (:wave-id %)) queue)
                         queue)]
    {:total-drones (count @drone-heartbeats)
     :active (filter-by-wave active)
     :alert (filter-by-wave alert)
     :stuck (filter-by-wave stuck)
     :retry-queue {:count (count filtered-queue)
                   :tasks (mapv #(select-keys % [:drone-id :attempt :queued-at]) filtered-queue)}
     :monitoring? @monitor-running?
     :thresholds {:alert-timeout-ms alert-timeout-ms
                  :kill-timeout-ms kill-timeout-ms
                  :claim-ttl-ms claim-ttl-ms
                  :check-interval-ms check-interval-ms
                  :max-retry-attempts max-retry-attempts}}))

;;; =============================================================================
;;; Event Handlers (CLARITY-C: Composition over modification)
;;; =============================================================================

(defonce ^:private events-registered? (atom false))

(defn register-event-handlers!
  "Register event handlers to automatically track drone lifecycle.

   Hooks into:
   - :drone/started  -> record-heartbeat! (with task/files for retry)
   - :drone/progress -> record-heartbeat! (auto-heartbeat on activity)
   - :drone/completed -> clear-heartbeat!
   - :drone/failed   -> clear-heartbeat!
   - :drone/tool-result -> record-heartbeat! (auto-heartbeat on tool calls)

   CLARITY-C: Uses event composition instead of modifying drone.clj
   CLARITY-Y: Captures task/files info for retry queue on stuck recovery"
  []
  (when-not @events-registered?
    ;; Handler for drone started - capture all info for potential retry
    (ev/reg-event :drone/started
                  []
                  (fn [_coeffects [_ {:keys [drone-id wave-id task files]}]]
                    (record-heartbeat! drone-id {:wave-id wave-id
                                                 :task task
                                                 :files files
                                                 :status :active})
                    {}))

    ;; Handler for drone progress - auto-heartbeat on activity
    ;; This is the key for reducing reliance on manual heartbeats
    (ev/reg-event :drone/progress
                  []
                  (fn [_coeffects [_ {:keys [drone-id wave-id]}]]
                    (record-heartbeat! drone-id {:wave-id wave-id
                                                 :status :active})
                    {}))

    ;; Handler for tool results - auto-heartbeat on tool execution
    ;; Bridges tool activity to heartbeat system
    (ev/reg-event :drone/tool-result
                  []
                  (fn [_coeffects [_ {:keys [drone-id wave-id tool-name]}]]
                    (record-heartbeat! drone-id {:wave-id wave-id
                                                 :status :active})
                    (log/debug "Auto-heartbeat from tool result:" drone-id tool-name)
                    {}))

    ;; Handler for drone completed - clear heartbeat
    (ev/reg-event :drone/completed
                  []
                  (fn [_coeffects [_ {:keys [drone-id]}]]
                    (clear-heartbeat! drone-id)
                    {}))

    ;; Handler for drone failed - clear heartbeat
    (ev/reg-event :drone/failed
                  []
                  (fn [_coeffects [_ {:keys [drone-id]}]]
                    (clear-heartbeat! drone-id)
                    {}))

    ;; Handler for drone recovered (from this module)
    (ev/reg-event :drone/recovered
                  []
                  (fn [_coeffects [_ {:keys [drone-id]}]]
                    (log/info "Drone recovery event processed:" drone-id)
                    {}))

    (reset! events-registered? true)
    (log/info "Drone health event handlers registered (with auto-heartbeat)")))

(defn init!
  "Initialize drone health monitoring system.

   - Registers event handlers for automatic heartbeat tracking
   - Optionally starts background monitoring loop

   Arguments:
     opts - Map with :auto-start? (default true)"
  [& [{:keys [auto-start?] :or {auto-start? true}}]]
  (register-event-handlers!)
  (when auto-start?
    (start-monitoring!))
  (log/info "Drone health monitoring initialized"))

;;; =============================================================================
;;; MCP Tool Handlers
;;; =============================================================================

(defn handle-drone-health-status
  "Get current drone health status.

   Parameters:
     wave_id - Optional wave ID to filter results

   Returns drone health dashboard data."
  [{:keys [wave_id]}]
  (try
    (let [health (get-wave-health wave_id)]
      (mcp-json health))
    (catch Exception e
      (log/error e "Failed to get drone health status")
      (mcp-json {:error (.getMessage e)}))))

(defn handle-recover-drone
  "Manually recover a stuck drone.

   Parameters:
     drone_id - ID of the drone to recover (required)
     retry    - Whether to queue for retry (default: false)

   Returns recovery status."
  [{:keys [drone_id retry]}]
  (try
    (if-not drone_id
      (mcp-json {:error "drone_id is required"})
      (let [result (recover-stuck-drone! drone_id {:retry? retry})]
        (mcp-json result)))
    (catch Exception e
      (log/error e "Failed to recover drone:" drone_id)
      (mcp-json {:error (.getMessage e)}))))

(defn handle-drone-health-control
  "Control drone health monitoring.

   Parameters:
     action - \"start\" or \"stop\"

   Returns monitoring status."
  [{:keys [action]}]
  (try
    (case action
      "start" (do (start-monitoring!)
                  (mcp-json {:status "started" :monitoring? true}))
      "stop" (do (stop-monitoring!)
                 (mcp-json {:status "stopped" :monitoring? false}))
      (mcp-json {:error "action must be 'start' or 'stop'"}))
    (catch Exception e
      (log/error e "Failed to control monitoring")
      (mcp-json {:error (.getMessage e)}))))

(defn handle-retry-queue-status
  "Get the current retry queue status.

   Returns queue info with tasks waiting for retry."
  [_params]
  (try
    (let [queue @retry-queue]
      (mcp-json {:queue-length (count queue)
                 :tasks (mapv #(-> %
                                   (select-keys [:drone-id :attempt :wave-id :queued-at])
                                   (assoc :task-preview (subs (:task %) 0 (min 50 (count (:task %))))))
                              queue)
                 :max-retry-attempts max-retry-attempts}))
    (catch Exception e
      (log/error e "Failed to get retry queue status")
      (mcp-json {:error (.getMessage e)}))))

(defn handle-pop-retry-task
  "Pop the next task from the retry queue for re-execution.

   Returns the task info if available, nil if queue is empty."
  [_params]
  (try
    (if-let [task (pop-retry-task!)]
      (mcp-json {:success true
                 :task task
                 :hint "Re-dispatch this task to a new drone"})
      (mcp-json {:success false
                 :message "Retry queue is empty"}))
    (catch Exception e
      (log/error e "Failed to pop retry task")
      (mcp-json {:error (.getMessage e)}))))

;;; =============================================================================
;;; Tool Definitions
;;; =============================================================================

(def tools
  "MCP tool definitions for drone health monitoring."
  [{:name "drone_health_status"
    :description "Get current drone health status including active, alert, stuck drones, and retry queue. Use to monitor drone wave execution and identify problems."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Optional wave ID to filter results"}}
                  :required []}
    :handler handle-drone-health-status}

   {:name "drone_recover"
    :description "Manually recover a stuck drone by releasing its file claims and killing the process. Automatically queues task for retry if retry=true."
    :inputSchema {:type "object"
                  :properties {"drone_id" {:type "string"
                                           :description "ID of the drone to recover"}
                               "retry" {:type "boolean"
                                        :description "Whether to queue task for retry (default: false)"}}
                  :required ["drone_id"]}
    :handler handle-recover-drone}

   {:name "drone_health_control"
    :description "Start or stop the background drone health monitoring. Monitoring checks every 30 seconds, alerts at 2 minutes, auto-kills at 5 minutes, and releases stale claims at 5 minutes."
    :inputSchema {:type "object"
                  :properties {"action" {:type "string"
                                         :enum ["start" "stop"]
                                         :description "Action to perform: 'start' or 'stop' monitoring"}}
                  :required ["action"]}
    :handler handle-drone-health-control}

   {:name "drone_retry_queue"
    :description "Get the retry queue status showing tasks waiting for re-execution after drone failure. Tasks are auto-queued when stuck drones are recovered."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-retry-queue-status}

   {:name "drone_pop_retry"
    :description "Pop the next task from the retry queue for manual re-execution. Use to fetch a failed task and dispatch it to a new drone."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-pop-retry-task}])
