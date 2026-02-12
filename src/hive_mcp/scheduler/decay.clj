(ns hive-mcp.scheduler.decay
  "Periodic background decay for memory, edges, and discs."
  (:require [hive-mcp.config.core :as config]
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
         :cycle-count 0
         :last-run nil
         :last-result nil}))

;; =============================================================================
;; Decay Cycle (Pure Logic)
;; =============================================================================

(defn- run-memory-decay!
  "Run memory staleness decay cycle."
  [opts]
  (try
    (if-let [run-fn (requiring-resolve 'hive-mcp.tools.memory.lifecycle/run-decay-cycle!)]
      (run-fn opts)
      {:skipped true :reason "lifecycle/run-decay-cycle! not available"})
    (catch Exception e
      (log/warn "Scheduler: memory decay failed (non-blocking):" (.getMessage e))
      {:error (.getMessage e) :decayed 0 :expired 0 :total-scanned 0})))

(defn- run-edge-decay!
  "Run edge confidence decay cycle."
  [opts]
  (try
    (if-let [decay-fn (requiring-resolve 'hive-mcp.knowledge-graph.edges/decay-unverified-edges!)]
      (decay-fn opts)
      {:skipped true :reason "edges/decay-unverified-edges! not available"})
    (catch Exception e
      (log/warn "Scheduler: edge decay failed (non-blocking):" (.getMessage e))
      {:error (.getMessage e) :decayed 0 :pruned 0 :fresh 0 :evaluated 0})))

(defn- run-disc-decay!
  "Run disc certainty time-decay."
  [opts]
  (try
    (if-let [disc-fn (requiring-resolve 'hive-mcp.knowledge-graph.disc/apply-time-decay-to-all-discs!)]
      (disc-fn :project-id (:project-id opts))
      {:skipped true :reason "disc/apply-time-decay-to-all-discs! not available"})
    (catch Exception e
      (log/warn "Scheduler: disc decay failed (non-blocking):" (.getMessage e))
      {:error (.getMessage e) :updated 0 :skipped 0 :errors 1})))

(defn run-decay-cycle!
  "Run a complete decay cycle: memory + edges + discs."
  ([] (run-decay-cycle! {}))
  ([{:keys [directory project-id memory-limit edge-limit disc-enabled]
     :or {memory-limit 50 edge-limit 100 disc-enabled true}}]
   (let [start-ms (System/currentTimeMillis)
         cycle-num (-> (swap! scheduler-state update :cycle-count inc)
                       :cycle-count)
         _ (log/info "Scheduler: decay cycle" cycle-num "starting")

         ;; Resolve project-id from directory if not explicit
         resolved-project-id (or project-id
                                 (try
                                   (when directory
                                     (let [scope-fn (requiring-resolve 'hive-mcp.tools.memory.scope/get-current-project-id)]
                                       (scope-fn directory)))
                                   (catch Exception _ nil)))

         ;; 1. Memory staleness decay
         memory-stats (run-memory-decay! {:directory directory
                                          :limit memory-limit})

         ;; 2. Edge confidence decay
         edge-stats (run-edge-decay! {:scope resolved-project-id
                                      :limit edge-limit
                                      :created-by "scheduler:decay"})

         ;; 3. Disc certainty time-decay
         disc-stats (if disc-enabled
                      (run-disc-decay! {:project-id resolved-project-id})
                      {:skipped true :reason "disc-decay-disabled"})

         elapsed-ms (- (System/currentTimeMillis) start-ms)
         result {:memory-stats memory-stats
                 :edge-stats edge-stats
                 :disc-stats disc-stats
                 :cycle-number cycle-num
                 :duration-ms elapsed-ms
                 :timestamp (java.time.Instant/now)}]

     ;; Update state
     (swap! scheduler-state assoc
            :last-run (java.time.Instant/now)
            :last-result result)

     ;; Log summary
     (log/info "Scheduler: decay cycle" cycle-num "completed in" elapsed-ms "ms"
               {:memory-decayed (or (:decayed memory-stats) 0)
                :memory-expired (or (:expired memory-stats) 0)
                :edges-decayed (or (:decayed edge-stats) 0)
                :edges-pruned (or (:pruned edge-stats) 0)
                :discs-updated (or (:updated disc-stats) 0)})
     result)))

;; =============================================================================
;; Scheduler Lifecycle
;; =============================================================================

(defn- get-scheduler-config
  "Read scheduler config with defaults applied."
  []
  (let [cfg (config/get-service-config :scheduler)]
    {:enabled (get cfg :enabled true)
     :interval-minutes (get cfg :interval-minutes 60)
     :memory-limit (get cfg :memory-limit 50)
     :edge-limit (get cfg :edge-limit 100)
     :disc-enabled (get cfg :disc-enabled true)
     :project-id (get cfg :project-id nil)}))

(defn- make-decay-task
  "Create a Runnable that runs a decay cycle."
  [config]
  (reify Runnable
    (run [_]
      (try
        (run-decay-cycle! {:memory-limit (:memory-limit config)
                           :edge-limit (:edge-limit config)
                           :disc-enabled (:disc-enabled config)
                           :project-id (:project-id config)})
        (catch Throwable t
          ;; CRITICAL: Must catch Throwable, not just Exception.
          ;; If a Runnable throws, ScheduledExecutorService silently
          ;; stops scheduling future executions.
          (log/error t "Scheduler: decay cycle threw (caught at boundary)"))))))

(defn start!
  "Start the periodic decay scheduler."
  []
  (let [{:keys [enabled interval-minutes] :as cfg} (get-scheduler-config)]
    (cond
      (not enabled)
      (do
        (log/info "Scheduler: decay scheduler disabled via config")
        {:started false :reason "disabled"})

      (:running? @scheduler-state)
      (do
        (log/info "Scheduler: already running, skipping start")
        {:started false :reason "already-running"})

      :else
      (try
        (let [^ScheduledExecutorService executor
              (Executors/newSingleThreadScheduledExecutor
               (reify java.util.concurrent.ThreadFactory
                 (newThread [_ r]
                   (doto (Thread. r "hive-decay-scheduler")
                     (.setDaemon true)))))
              task (make-decay-task cfg)]
          ;; Schedule with fixed delay (not fixed rate) to prevent
          ;; overlapping cycles if one takes longer than the interval
          (.scheduleWithFixedDelay executor task
                                   (long interval-minutes) ;; initial delay
                                   (long interval-minutes) ;; subsequent delay
                                   TimeUnit/MINUTES)
          (swap! scheduler-state assoc
                 :executor executor
                 :running? true)
          (log/info "Scheduler: decay scheduler started"
                    {:interval-minutes interval-minutes
                     :memory-limit (:memory-limit cfg)
                     :edge-limit (:edge-limit cfg)
                     :disc-enabled (:disc-enabled cfg)})
          {:started true
           :interval-minutes interval-minutes
           :config (dissoc cfg :project-id)})
        (catch Exception e
          (log/error e "Scheduler: failed to start decay scheduler")
          {:started false :reason (.getMessage e)})))))

(defn stop!
  "Stop the periodic decay scheduler."
  []
  (if-not (:running? @scheduler-state)
    {:stopped false :reason "not-running"}
    (try
      (let [^ScheduledExecutorService executor (:executor @scheduler-state)
            cycles (:cycle-count @scheduler-state)]
        (.shutdown executor)
        (when-not (.awaitTermination executor 5 TimeUnit/SECONDS)
          (.shutdownNow executor)
          (log/warn "Scheduler: forced shutdown after 5s timeout"))
        (swap! scheduler-state assoc
               :executor nil
               :running? false)
        (log/info "Scheduler: decay scheduler stopped after" cycles "cycles")
        {:stopped true :cycles-completed cycles})
      (catch Exception e
        (log/error e "Scheduler: error during shutdown")
        (swap! scheduler-state assoc :executor nil :running? false)
        {:stopped true :error (.getMessage e)}))))

(defn restart!
  "Stop and restart the decay scheduler."
  []
  (stop!)
  (start!))

;; =============================================================================
;; Status / Introspection
;; =============================================================================

(defn status
  "Return current scheduler status."
  []
  (let [state @scheduler-state
        cfg (get-scheduler-config)]
    {:running? (:running? state)
     :cycle-count (:cycle-count state)
     :last-run (:last-run state)
     :last-result (when-let [r (:last-result state)]
                    ;; Compact summary, not full result
                    {:memory-decayed (get-in r [:memory-stats :decayed] 0)
                     :memory-expired (get-in r [:memory-stats :expired] 0)
                     :edges-decayed (get-in r [:edge-stats :decayed] 0)
                     :edges-pruned (get-in r [:edge-stats :pruned] 0)
                     :discs-updated (get-in r [:disc-stats :updated] 0)
                     :duration-ms (:duration-ms r)
                     :cycle-number (:cycle-number r)})
     :config cfg}))
