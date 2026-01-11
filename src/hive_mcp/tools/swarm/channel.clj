(ns hive-mcp.tools.swarm.channel
  "Channel-based event management for swarm task tracking.
   
   Provides push-based task completion detection via channel subscriptions.
   Maintains an event journal for sub-100ms task completion detection.
   
   Functions:
   - start-channel-subscriptions! / stop-channel-subscriptions!
   - check-event-journal / clear-event-journal!
   - Event handlers for task-completed, task-failed, prompt-shown"
  (:require [clojure.core.async :as async :refer [go go-loop <! close!]]
            [taoensso.timbre :as log]))

;; ============================================================
;; Event Journal (Push-based task tracking)
;; ============================================================

;; In-memory journal of swarm events received via channel.
;; Maps task-id -> {:status :result :timestamp :slave-id}
(defonce ^:private event-journal (atom {}))

;; Active channel subscriptions (for cleanup).
(defonce ^:private channel-subscriptions (atom []))

;; ============================================================
;; Channel Integration
;; ============================================================

(defn- try-require-channel
  "Attempt to require the channel namespace. Returns true if available."
  []
  (try
    (require 'hive-mcp.channel)
    true
    (catch Exception _
      false)))

(defn- channel-subscribe!
  "Subscribe to an event type if channel is available.
   Returns the subscription channel or nil."
  [event-type]
  (when (try-require-channel)
    (when-let [subscribe-fn (resolve 'hive-mcp.channel/subscribe!)]
      (subscribe-fn event-type))))

;; ============================================================
;; Event Handlers
;; ============================================================

(defn- handle-task-completed
  "Handle task-completed event from channel.
   Note: bencode returns string keys, so we use get instead of keywords."
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
  "Handle task-failed event from channel.
   Note: bencode returns string keys, so we use get instead of keywords."
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
  "Handle prompt-shown event from channel.
   Note: bencode returns string keys, so we use get instead of keywords."
  [event]
  (let [slave-id (get event "slave-id")
        prompt (get event "prompt")
        timestamp (get event "timestamp")]
    (log/info "Channel: prompt-shown from" slave-id)
    ;; For now just log - could add to a prompts journal if needed
    ))

;; ============================================================
;; Public API
;; ============================================================

(defn start-channel-subscriptions!
  "Start listening for swarm events via channel.
   Called at startup if channel is available."
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
  "Check event journal for task completion.
   Returns the event if found, nil otherwise."
  [task-id]
  (get @event-journal (str task-id)))

(defn clear-event-journal!
  "Clear all entries from the event journal."
  []
  (reset! event-journal {}))
