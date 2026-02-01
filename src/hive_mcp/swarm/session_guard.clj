(ns hive-mcp.swarm.session-guard
  "Session completion guard - detects when lings shout :completed
   but skip calling session_complete.

   Problem:
   Lings shout `:completed` but skip calling `session_complete`. This breaks:
   - Git commits not created
   - Kanban tasks not moved to done
   - No crystallization of learnings

   Solution:
   - Track when a ling shouts :completed
   - Set a timer (default 30s)
   - If no session_complete call detected, emit warning event

   CLARITY Framework:
   - C: Composition - integrates with hivemind and session_complete
   - L: Layers pure - tracking state isolated from effects
   - T: Telemetry first - warnings visible via hivemind
   - Y: Yield safe failure - graceful timeout handling"
  (:require [hive-mcp.hivemind :as hivemind]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private pending-completions
  "Map of agent-id -> {:timestamp :task :message :timer-future}
   Tracks lings that have shouted :completed but not called session_complete."
  (atom {}))

(defonce ^:private config
  "Configuration for session guard."
  (atom {:timeout-ms 30000
         :warning-handler nil}))

(defonce ^:private scheduler
  "Scheduled executor for timeout tasks."
  (delay (ScheduledThreadPoolExecutor. 1)))

;; =============================================================================
;; Configuration
;; =============================================================================

(defn set-timeout-ms!
  "Set the timeout in milliseconds before warning is emitted.
   Default: 30000 (30 seconds)"
  [ms]
  (swap! config assoc :timeout-ms ms))

(defn set-warning-handler!
  "Set a custom warning handler for testing.
   Handler signature: (fn [agent-id warning-data])"
  [handler]
  (swap! config assoc :warning-handler handler))

(defn get-timeout-ms
  "Get current timeout setting."
  []
  (:timeout-ms @config))

;; =============================================================================
;; Warning Emission
;; =============================================================================

(defn- emit-warning!
  "Emit a warning that a ling shouted :completed without session_complete.
   Uses custom handler if set (for testing), otherwise hivemind shout."
  [agent-id data]
  (let [warning-data (assoc data
                            :source "session-guard"
                            :action-required "Call session_complete to properly close session")]
    (if-let [handler (:warning-handler @config)]
      ;; Custom handler (for testing)
      (handler agent-id warning-data)
      ;; Production: shout via hivemind
      (do
        (hivemind/shout! agent-id :session-incomplete
                         {:task "session_complete missing"
                          :message (str "REMINDER: Ling " agent-id " shouted :completed "
                                        (long (:elapsed-ms data)) "ms ago but never called "
                                        "session_complete. Git commit, kanban, and "
                                        "crystallization were skipped.")
                          :original-task (:original-task data)
                          :elapsed-ms (:elapsed-ms data)})
        (log/warn "Session guard: ling" agent-id
                  "shouted :completed without session_complete"
                  "- elapsed:" (:elapsed-ms data) "ms")))))

;; =============================================================================
;; Timer Management
;; =============================================================================

(defn- create-timer-task
  "Create a timer task that checks if session_complete was called."
  [agent-id original-data]
  (let [start-time (System/currentTimeMillis)]
    (fn []
      (try
        (when-let [pending (get @pending-completions agent-id)]
          ;; Only emit if still pending and same timestamp (not reset)
          (when (= (:timestamp pending) (:timestamp original-data))
            (let [elapsed (- (System/currentTimeMillis) start-time)]
              (emit-warning! agent-id
                             {:original-task (:task original-data)
                              :original-message (:message original-data)
                              :elapsed-ms elapsed})
              ;; Remove from pending after warning
              (swap! pending-completions dissoc agent-id))))
        (catch Exception e
          (log/error "Session guard timer error:" (.getMessage e)))))))

(defn- schedule-timer!
  "Schedule a timer to check for session_complete."
  [agent-id data]
  (let [timeout-ms (:timeout-ms @config)
        task (create-timer-task agent-id data)]
    (.schedule @scheduler
               ^Runnable task
               ^long timeout-ms
               TimeUnit/MILLISECONDS)))

(defn- cancel-timer!
  "Cancel pending timer for an agent."
  [agent-id]
  (when-let [{:keys [timer-future]} (get @pending-completions agent-id)]
    (when timer-future
      (try
        (.cancel timer-future false)
        (catch Exception _)))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn register-completion-shout!
  "Register that a ling has shouted :completed.
   Starts a timer to check for session_complete.

   Arguments:
     agent-id - The ling's identifier
     data     - Map with :task, :message from the shout

   Called by hivemind when ling shouts :completed."
  [agent-id data]
  ;; Cancel existing timer if any (duplicate shout resets timer)
  (cancel-timer! agent-id)

  (let [now (System/currentTimeMillis)
        tracking-data {:timestamp now
                       :task (:task data)
                       :message (:message data)}
        timer-future (schedule-timer! agent-id tracking-data)]
    (swap! pending-completions assoc agent-id
           (assoc tracking-data :timer-future timer-future))
    (log/debug "Session guard: tracking completion for" agent-id)))

(defn mark-session-complete!
  "Mark that a ling has properly called session_complete.
   Clears the pending status and cancels the timer.

   Arguments:
     agent-id - The ling's identifier

   Called by session_complete tool."
  [agent-id]
  (when (contains? @pending-completions agent-id)
    (cancel-timer! agent-id)
    (swap! pending-completions dissoc agent-id)
    (log/debug "Session guard: cleared pending for" agent-id)))

(defn pending-completion?
  "Check if a ling has a pending completion (shouted but no session_complete)."
  [agent-id]
  (contains? @pending-completions agent-id))

(defn get-pending-completions
  "Get all pending completions as a map of agent-id -> data."
  []
  (into {}
        (map (fn [[k v]]
               [k (dissoc v :timer-future)])
             @pending-completions)))

(defn reset-state!
  "Reset all state. For testing."
  []
  ;; Cancel all timers
  (doseq [[agent-id _] @pending-completions]
    (cancel-timer! agent-id))
  (reset! pending-completions {})
  (reset! config {:timeout-ms 30000
                  :warning-handler nil}))

;; =============================================================================
;; Event Handler Registration
;; =============================================================================

(defn handle-ling-completed-event
  "Handler for :ling/completed events from hivemind.
   Registers the completion shout for tracking."
  [{:keys [agent-id data]}]
  (when agent-id
    (register-completion-shout! agent-id data)))

(defonce ^:private *handler-registered (atom false))

(defn register-event-handler!
  "Register event handler for :ling/completed events.
   Safe to call multiple times."
  []
  (when-not @*handler-registered
    (try
      (require '[hive-mcp.events.core :as ev])
      (let [reg-event (resolve 'hive-mcp.events.core/reg-event)]
        (reg-event :ling/completed
                   []
                   (fn [coeffects [_ event-data]]
                     ;; Track the completion
                     (handle-ling-completed-event event-data)
                     ;; Return empty effects (just tracking)
                     {})))
      (reset! *handler-registered true)
      (log/info "[session-guard] Event handler registered for :ling/completed")
      (catch Exception e
        (log/warn "Session guard: could not register event handler -"
                  (.getMessage e)))))
  @*handler-registered)

(defn reset-handler-registration!
  "Reset handler registration state. For testing."
  []
  (reset! *handler-registered false))
