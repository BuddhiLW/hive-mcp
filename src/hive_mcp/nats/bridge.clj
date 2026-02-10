(ns hive-mcp.nats.bridge
  "NATS to event journal bridge for push-based drone completion.

   Subscribes to drone events on NATS and populates the event journal —
   the same atom that `collect` already checks via check-event-journal.

   Publisher side: called from :nats-publish effect handler.
   Subscriber side: populates event journal for instant collect.

   CLARITY-Y: Yield safe failure — all operations no-op when NATS unavailable."
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
;; Subscriber Side (populates event journal)
;; =============================================================================

(defn- handle-drone-completed
  "Handle drone completion from NATS — write to event journal."
  [{:keys [task-id parent-id result] :as _msg}]
  (log/info "[NATS] drone completed:" task-id)
  (channel/record-nats-event! task-id
                              {:status "completed"
                               :result result
                               :slave-id parent-id
                               :timestamp (System/currentTimeMillis)
                               :via "nats-push"}))

(defn- handle-drone-failed
  "Handle drone failure from NATS — write to event journal."
  [{:keys [task-id parent-id error] :as _msg}]
  (log/info "[NATS] drone failed:" task-id)
  (channel/record-nats-event! task-id
                              {:status "failed"
                               :error error
                               :slave-id parent-id
                               :timestamp (System/currentTimeMillis)
                               :via "nats-push"}))

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
