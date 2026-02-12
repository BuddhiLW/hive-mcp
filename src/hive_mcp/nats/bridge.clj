(ns hive-mcp.nats.bridge
  "NATS to event journal bridge for push-based drone completion.

   Subscribes to drone events on NATS and populates the event journal —
   the same atom that `collect` already checks via check-event-journal.

   Publisher side: called from :nats-publish effect handler.
   Subscriber side: populates event journal for instant collect."

  (:require [hive-mcp.nats.client :as nats]
            [hive-mcp.tools.swarm.channel :as channel]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Subject Hierarchy
;; =============================================================================

(def ^:private subject-prefix "hive.v1.drone")

(defn task-subject
  "Build NATS subject for a specific task event.
   E.g. hive.v1.drone.completed.task-123"
  [task-id event-type]
  (str subject-prefix "." (name event-type) "." task-id))

(defn wildcard-subject
  "Build NATS wildcard subject for all events of a type.
   E.g. hive.v1.drone.completed.>"
  [event-type]
  (str subject-prefix "." (name event-type) ".>"))

;; =============================================================================
;; Publisher Side (called from :nats-publish effect)
;; =============================================================================

(defn publish-drone-event!
  "Publish drone event to NATS. Called by :nats-publish effect handler."
  [{:keys [event-type task-id] :as payload}]
  (let [subject (task-subject task-id event-type)]
    (nats/publish! subject payload)))

;; =============================================================================
;; Callback Bridge (requiring-resolve to avoid circular deps)
;; =============================================================================

(defn- fire-callback-if-registered!
  "Fire callback for task if registered. Uses requiring-resolve to avoid
   circular dependency on hive-mcp.swarm.callback."
  [task-id event-data]
  (try
    (when-let [notify! (requiring-resolve 'hive-mcp.swarm.callback/notify-completion!)]
      (notify! task-id event-data))
    (catch Exception e
      (log/debug "[NATS] Callback fire failed for" task-id (.getMessage e)))))

;; =============================================================================
;; Hivemind Auto-Shout (drone visibility via piggyback)
;; =============================================================================

(defn- auto-shout-drone-event!
  "Auto-shout drone completion/failure to hivemind for visibility.
   Makes drone wave results appear in ---HIVEMIND--- piggyback blocks.
   Uses requiring-resolve to avoid circular dep on hivemind.messaging."
  [task-id event-type summary-msg]
  (try
    (when-let [shout-fn (requiring-resolve 'hive-mcp.hivemind.core/shout!)]
      (shout-fn (str "drone:" task-id)
                event-type
                {:message summary-msg
                 :task (str "drone-task:" task-id)}))
    (catch Exception e
      (log/debug "[NATS] Auto-shout failed for" task-id (.getMessage e)))))

;; =============================================================================
;; Subscriber Side (populates event journal + fires callbacks + auto-shout)
;; =============================================================================

(defn- handle-drone-completed
  "Handle drone completion from NATS — write to event journal, fire callback, and auto-shout."
  [{:keys [task-id parent-id result] :as _msg}]
  (log/info "[NATS] drone completed:" task-id)
  (let [files-modified (get result :files-modified [])
        files-failed (get result :files-failed [])
        duration-ms (get result :duration-ms)
        event-data {:status "completed"
                    :result result
                    :slave-id parent-id
                    :timestamp (System/currentTimeMillis)
                    :via "nats-push"}]
    (channel/record-nats-event! task-id event-data)
    (fire-callback-if-registered! task-id event-data)
    (auto-shout-drone-event!
     task-id :completed
     (str "Drone " task-id " completed: "
          (count files-modified) " files modified"
          (when (seq files-failed) (str ", " (count files-failed) " failed"))
          (when duration-ms (str " (" duration-ms "ms)"))))))

(defn- handle-drone-failed
  "Handle drone failure from NATS — write to event journal, fire callback, and auto-shout."
  [{:keys [task-id parent-id error] :as _msg}]
  (log/info "[NATS] drone failed:" task-id)
  (let [event-data {:status "failed"
                    :error error
                    :slave-id parent-id
                    :timestamp (System/currentTimeMillis)
                    :via "nats-push"}]
    (channel/record-nats-event! task-id event-data)
    (fire-callback-if-registered! task-id event-data)
    (auto-shout-drone-event!
     task-id :error
     (str "Drone " task-id " failed: " (or error "unknown error")))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn start-subscriptions!
  "Subscribe to drone completion/failure events on NATS.
   No-op if NATS is not connected."
  []
  (when (nats/connected?)
    (nats/subscribe! (wildcard-subject :completed) handle-drone-completed)
    (nats/subscribe! (wildcard-subject :failed) handle-drone-failed)
    (log/info "[NATS] Bridge subscriptions started")))

(defn stop-subscriptions!
  "Unsubscribe from drone events on NATS."
  []
  (nats/unsubscribe! (wildcard-subject :completed))
  (nats/unsubscribe! (wildcard-subject :failed))
  (log/info "[NATS] Bridge subscriptions stopped"))
