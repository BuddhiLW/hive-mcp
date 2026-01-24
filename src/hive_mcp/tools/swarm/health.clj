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
            [hive-mcp.events.core :as ev]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.telemetry.prometheus :as prom]
            [clojure.core.async :as async :refer [go-loop <! timeout]]
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

;;; =============================================================================
;;; Heartbeat State
;;; =============================================================================

;; Map of drone-id -> {:last-beat timestamp :status :active/:alert/:stuck :wave-id :task}
(defonce drone-heartbeats (atom {}))

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
     opts     - Optional map with :wave-id :task :status

   Updates the heartbeat timestamp and any provided metadata.
   Called during drone execution to indicate progress.

   CLARITY-T: Telemetry first - observable drone activity."
  [drone-id & [{:keys [wave-id task status]}]]
  (let [now (System/currentTimeMillis)]
    (swap! drone-heartbeats update drone-id
           (fn [current]
             (merge (or current {})
                    {:last-beat now
                     :status (or status :active)}
                    (when wave-id {:wave-id wave-id})
                    (when task {:task task}))))
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
   5. Optionally record for retry

   Arguments:
     drone-id - ID of the stuck drone
     opts     - Optional map with :retry? :wave-id

   Returns map with recovery status."
  [drone-id & [{:keys [retry? wave-id]}]]
  (log/warn "Recovering stuck drone:" drone-id {:retry? retry? :wave-id wave-id})

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

    ;; 5. Record for telemetry
    (prom/inc-events-total! :drone/stuck-recovered :warn)

    {:drone-id drone-id
     :status :recovered
     :kill-result kill-result
     :claims-released true}))

;;; =============================================================================
;;; Background Monitoring Loop
;;; =============================================================================

(defn- run-health-check!
  "Execute one health check cycle.

   Called periodically by the monitoring loop."
  []
  (let [{:keys [alert stuck]} (check-all-drones)]

    ;; Log alerts
    (doseq [{:keys [drone-id elapsed-ms]} alert]
      (log/warn "Drone alert - no progress for" (/ elapsed-ms 1000) "seconds:" drone-id)
      ;; Update status to alert
      (swap! drone-heartbeats assoc-in [drone-id :status] :alert))

    ;; Auto-recover stuck drones
    (doseq [{:keys [drone-id wave-id elapsed-ms]} stuck]
      (log/error "Drone stuck - auto-recovering after" (/ elapsed-ms 1000) "seconds:" drone-id)
      (recover-stuck-drone! drone-id {:wave-id wave-id}))))

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
     :monitoring?    - Whether background monitoring is active"
  [& [wave-id]]
  (let [{:keys [active alert stuck]} (check-all-drones)
        filter-by-wave (fn [drones]
                         (if wave-id
                           (filterv #(= wave-id (:wave-id %)) drones)
                           drones))]
    {:total-drones (count @drone-heartbeats)
     :active (filter-by-wave active)
     :alert (filter-by-wave alert)
     :stuck (filter-by-wave stuck)
     :monitoring? @monitor-running?
     :thresholds {:alert-timeout-ms alert-timeout-ms
                  :kill-timeout-ms kill-timeout-ms
                  :check-interval-ms check-interval-ms}}))

;;; =============================================================================
;;; Event Handlers (CLARITY-C: Composition over modification)
;;; =============================================================================

(defonce ^:private events-registered? (atom false))

(defn register-event-handlers!
  "Register event handlers to automatically track drone lifecycle.

   Hooks into:
   - :drone/started  -> record-heartbeat!
   - :drone/completed -> clear-heartbeat!
   - :drone/failed   -> clear-heartbeat!

   CLARITY-C: Uses event composition instead of modifying drone.clj"
  []
  (when-not @events-registered?
    ;; Handler for drone started
    (ev/reg-event :drone/started
                  []
                  (fn [_coeffects [_ {:keys [drone-id wave-id task]}]]
                    (record-heartbeat! drone-id {:wave-id wave-id
                                                 :task task
                                                 :status :active})
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
    (log/info "Drone health event handlers registered")))

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

;;; =============================================================================
;;; Tool Definitions
;;; =============================================================================

(def tools
  "MCP tool definitions for drone health monitoring."
  [{:name "drone_health_status"
    :description "Get current drone health status including active, alert, and stuck drones. Use to monitor drone wave execution and identify problems."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Optional wave ID to filter results"}}
                  :required []}
    :handler handle-drone-health-status}

   {:name "drone_recover"
    :description "Manually recover a stuck drone by releasing its file claims and killing the process. Use when auto-recovery hasn't triggered or you need immediate intervention."
    :inputSchema {:type "object"
                  :properties {"drone_id" {:type "string"
                                           :description "ID of the drone to recover"}
                               "retry" {:type "boolean"
                                        :description "Whether to queue task for retry (default: false)"}}
                  :required ["drone_id"]}
    :handler handle-recover-drone}

   {:name "drone_health_control"
    :description "Start or stop the background drone health monitoring. Monitoring checks every 30 seconds, alerts at 2 minutes, auto-kills at 5 minutes."
    :inputSchema {:type "object"
                  :properties {"action" {:type "string"
                                         :enum ["start" "stop"]
                                         :description "Action to perform: 'start' or 'stop' monitoring"}}
                  :required ["action"]}
    :handler handle-drone-health-control}])
