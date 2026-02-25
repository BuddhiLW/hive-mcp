(ns hive-mcp.scheduler.housekeeping
  "Periodic global housekeeping: clean up stale JVM-side resources.

   Runs on a scheduled executor (like decay.clj) every 5 minutes.
   Each cleanup task is individually guarded — one failure doesn't
   stop the sweep.

   Targets:
   - Stale callbacks (> 30 min)
   - Completed multi-async batches
   - Event journal old entries
   - Completed SAA states
   - Drone KG stores for dead drones"
  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.config.core :as config]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private scheduler-state
  (atom {:executor nil
         :running? false
         :sweep-count 0
         :last-run nil
         :last-result nil}))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- resolve-and-call
  "Resolve sym via requiring-resolve; call f with the resolved fn.
   Returns result or fallback on failure."
  [sym call-fn fallback]
  (if-let [f (requiring-resolve sym)]
    (result/rescue fallback (call-fn f))
    {:skipped true :reason (str (name sym) " not available")}))

;; =============================================================================
;; Housekeeping Sweep (Pure Logic)
;; =============================================================================

(defn housekeeping-sweep!
  "Run a single housekeeping sweep. Each task is guarded independently."
  []
  (let [start-ms (System/currentTimeMillis)
        sweep-num (-> (swap! scheduler-state update :sweep-count inc)
                      :sweep-count)
        _ (log/debug "Housekeeping: sweep" sweep-num "starting")

        ;; 1. Stale callbacks (30 min threshold)
        callback-result
        (resolve-and-call
         'hive-mcp.swarm.callback/cleanup-stale!
         #(%) {:cleaned 0})

        ;; 2. Completed multi-async batches
        async-result
        (resolve-and-call
         'hive-mcp.tools.multi-async/gc-completed!
         #(%) {:cleaned 0})

        ;; 3. Event journal cleanup
        journal-result
        (resolve-and-call
         'hive-mcp.tools.swarm.channel/clear-event-journal!
         #(%) {:cleared true})

        ;; 4. Completed SAA states
        saa-result
        (resolve-and-call
         'hive-mcp.agent.saa.orchestrator/clear-completed-states!
         #(%) {:cleaned 0})

        elapsed-ms (- (System/currentTimeMillis) start-ms)
        result {:callbacks callback-result
                :async-batches async-result
                :event-journal journal-result
                :saa-states saa-result
                :sweep-number sweep-num
                :duration-ms elapsed-ms
                :timestamp (java.time.Instant/now)}]

    ;; Update state
    (swap! scheduler-state assoc
           :last-run (java.time.Instant/now)
           :last-result result)

    (log/info "Housekeeping: sweep" sweep-num "completed in" elapsed-ms "ms")
    result))

;; =============================================================================
;; Scheduler Lifecycle
;; =============================================================================

(defn- get-housekeeping-config
  "Read housekeeping config with defaults applied."
  []
  (let [cfg (config/get-service-config :housekeeping)]
    {:enabled (get cfg :enabled true)
     :interval-minutes (get cfg :interval-minutes 5)}))

(defn- make-sweep-task
  "Create a Runnable that runs a housekeeping sweep."
  []
  (reify Runnable
    (run [_]
      ;; Boundary — MUST catch Throwable: ScheduledExecutorService silently
      ;; stops scheduling future executions if a Runnable throws.
      (try
        (housekeeping-sweep!)
        (catch Throwable t
          (log/error t "Housekeeping: sweep threw (caught at boundary)"))))))

(defn start!
  "Start the periodic housekeeping scheduler."
  []
  (let [{:keys [enabled interval-minutes]} (get-housekeeping-config)]
    (cond
      (not enabled)
      (do
        (log/info "Housekeeping: scheduler disabled via config")
        {:started false :reason "disabled"})

      (:running? @scheduler-state)
      (do
        (log/info "Housekeeping: already running, skipping start")
        {:started false :reason "already-running"})

      :else
      (try
        (let [^ScheduledExecutorService executor
              (Executors/newSingleThreadScheduledExecutor
               (reify java.util.concurrent.ThreadFactory
                 (newThread [_ r]
                   (doto (Thread. r "hive-housekeeping")
                     (.setDaemon true)))))
              task (make-sweep-task)]
          (.scheduleWithFixedDelay executor task
                                   (long interval-minutes)
                                   (long interval-minutes)
                                   TimeUnit/MINUTES)
          (swap! scheduler-state assoc
                 :executor executor
                 :running? true)
          (log/info "Housekeeping: scheduler started"
                    {:interval-minutes interval-minutes})
          {:started true :interval-minutes interval-minutes})
        (catch Exception e
          (log/error e "Housekeeping: failed to start scheduler")
          {:started false :reason (.getMessage e)})))))

(defn stop!
  "Stop the periodic housekeeping scheduler."
  []
  (if-not (:running? @scheduler-state)
    {:stopped false :reason "not-running"}
    (try
      (let [^ScheduledExecutorService executor (:executor @scheduler-state)
            sweeps (:sweep-count @scheduler-state)]
        (.shutdown executor)
        (when-not (.awaitTermination executor 5 TimeUnit/SECONDS)
          (.shutdownNow executor)
          (log/warn "Housekeeping: forced shutdown after 5s timeout"))
        (swap! scheduler-state assoc
               :executor nil
               :running? false)
        (log/info "Housekeeping: scheduler stopped after" sweeps "sweeps")
        {:stopped true :sweeps-completed sweeps})
      (catch Exception e
        (log/error e "Housekeeping: error during shutdown")
        (swap! scheduler-state assoc :executor nil :running? false)
        {:stopped true :error (.getMessage e)}))))

(defn status
  "Return current housekeeping scheduler status."
  []
  (let [state @scheduler-state]
    {:running? (:running? state)
     :sweep-count (:sweep-count state)
     :last-run (:last-run state)
     :last-result (:last-result state)
     :config (get-housekeeping-config)}))
