(ns hive-mcp.tools.swarm.channel
  "Channel-based event management for swarm task tracking via core.async and NATS."
  (:require [clojure.core.async :as async :refer [go-loop <! close!]]
            [taoensso.timbre :as log]
            [hive-mcp.dns.result :refer [rescue]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private event-journal (atom {}))

(defonce ^:private channel-subscriptions (atom []))

(defn- try-require-channel
  "Attempt to require the channel namespace, returns true if available."
  []
  (rescue false
          (require 'hive-mcp.channel.core)
          true))

(defn- channel-subscribe!
  "Subscribe to an event type if channel is available."
  [event-type]
  (when (try-require-channel)
    (when-let [subscribe-fn (resolve 'hive-mcp.channel.core/subscribe!)]
      (subscribe-fn event-type))))

(defn- handle-task-completed
  "Handle task-completed event from channel."
  [event]
  (let [task-id (get event "task-id")
        slave-id (get event "slave-id")
        result (get event "result")
        timestamp (get event "timestamp")]
    (log/info "Channel: task-completed" task-id "from" slave-id)
    (swap! event-journal assoc (str task-id)
           {:status "completed"
            :result result
            :slave-id slave-id
            :timestamp (or timestamp (System/currentTimeMillis))})))

(defn- handle-task-failed
  "Handle task-failed event from channel."
  [event]
  (let [task-id (get event "task-id")
        slave-id (get event "slave-id")
        error (get event "error")
        timestamp (get event "timestamp")]
    (log/info "Channel: task-failed" task-id "from" slave-id ":" error)
    (swap! event-journal assoc (str task-id)
           {:status "failed"
            :error error
            :slave-id slave-id
            :timestamp (or timestamp (System/currentTimeMillis))})))

(defn- handle-prompt-shown
  "Handle prompt-shown event from channel."
  [event]
  (let [slave-id (get event "slave-id")
        _prompt (get event "prompt")
        _timestamp (get event "timestamp")]
    (log/info "Channel: prompt-shown from" slave-id)
    ;; For now just log - could add to a prompts journal if needed
    ))

(defn start-channel-subscriptions!
  "Start listening for swarm events via channel subscriptions."
  []
  (when (try-require-channel)
    (log/info "Starting channel subscriptions for swarm events...")

    ;; Subscribe to task-completed
    (when-let [sub (channel-subscribe! :task-completed)]
      (swap! channel-subscriptions conj sub)
      (go-loop []
        (when-let [event (<! sub)]
          (handle-task-completed event)
          (recur))))

    ;; Subscribe to task-failed
    (when-let [sub (channel-subscribe! :task-failed)]
      (swap! channel-subscriptions conj sub)
      (go-loop []
        (when-let [event (<! sub)]
          (handle-task-failed event)
          (recur))))

    ;; Subscribe to prompt-shown
    (when-let [sub (channel-subscribe! :prompt-shown)]
      (swap! channel-subscriptions conj sub)
      (go-loop []
        (when-let [event (<! sub)]
          (handle-prompt-shown event)
          (recur))))

    (log/info "Channel subscriptions started")))

(defn stop-channel-subscriptions!
  "Stop all channel subscriptions."
  []
  (doseq [sub @channel-subscriptions]
    (close! sub))
  (reset! channel-subscriptions [])
  (log/info "Channel subscriptions stopped"))

(defn check-event-journal
  "Check event journal for task completion, returns the event or nil."
  [task-id]
  (get @event-journal (str task-id)))

(defn clear-event-journal!
  "Clear all entries from the event journal."
  []
  (reset! event-journal {}))

(defn record-nats-event!
  "Record event from NATS into the shared event-journal atom."
  [task-id event-data]
  (swap! event-journal assoc (str task-id) event-data))
