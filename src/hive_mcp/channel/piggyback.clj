(ns hive-mcp.channel.piggyback
  "Piggyback communication — instruction queue and message cursor for hivemind↔ling messaging."
  (:require [clojure.spec.alpha :as s]
            [hive-mcp.server.guards :as guards]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Specs for piggyback messages
(s/def ::agent-id string?)
(s/def ::a string?)  ; abbreviated agent-id
(s/def ::e string?)  ; event-type
(s/def ::m string?)  ; message
(s/def ::hivemind-message (s/keys :req-un [::a ::e ::m]))
(s/def ::messages (s/nilable (s/coll-of ::hivemind-message :kind vector?)))

;; Instruction Queue (hivemind → ling)

(defonce ^{:doc "Map of agent-id -> [instructions...]. Drained when piggybacked."}
  instruction-queues
  (atom {}))

(defn clear-instruction-queues!
  "Clear instruction queues. Guarded — no-op if coordinator running."
  []
  (guards/when-not-coordinator
   "clear-instruction-queues! called"
   (reset! instruction-queues {})))

(defn push-instruction!
  "Push an instruction to an agent's queue for piggyback delivery."
  [agent-id instruction]
  (swap! instruction-queues update agent-id (fnil conj []) instruction))

(defn drain-instructions!
  "Drain all pending instructions for an agent."
  [agent-id]
  (let [instructions (get @instruction-queues agent-id [])]
    (swap! instruction-queues dissoc agent-id)
    instructions))

(defn peek-instructions
  "Peek at pending instructions without draining. For debugging."
  [agent-id]
  (get @instruction-queues agent-id []))

;; Message Source (injectable for DIP)

(defonce ^{:doc "Injected fn returning all hivemind messages."}
  message-source-fn
  (atom nil))

(defn register-message-source!
  "Register function that provides hivemind messages."
  [source-fn]
  (reset! message-source-fn source-fn))

;; Message Cursors (per-agent-per-project read tracking)

(defonce ^{:doc "Map of [agent-id project-id] -> last-read-timestamp for cursor isolation."}
  agent-read-cursors
  (atom {}))

(defonce ^{:doc "Monotonic counter for message IDs. Incremented atomically."}
  message-id-counter
  (atom 0))

(defn next-message-id!
  "Generate next monotonic message ID. Thread-safe."
  []
  (swap! message-id-counter inc))

(s/def ::project-id (s/nilable string?))

(s/fdef get-messages
  :args (s/cat :agent-id ::agent-id
               :kwargs (s/keys* :opt-un [::project-id]))
  :ret ::messages)

(defn get-messages
  "Get new hivemind messages since last call for this agent+project."
  [agent-id & {:keys [project-id]}]
  (when (and (nil? project-id) (not= agent-id "coordinator"))
    (log/warn "Agent" agent-id "reading hivemind without project-id - using global cursor"))
  (when-let [source-fn @message-source-fn]
    (let [effective-project (or project-id "global")
          cursor-key [agent-id effective-project]
          last-cursor (get @agent-read-cursors cursor-key 0)
          all-msgs (source-fn)
          new-msgs (->> all-msgs
                        (filter (fn [msg]
                                  (and (> (:timestamp msg) last-cursor)
                                       (if project-id
                                         (or (= (:project-id msg) project-id)
                                             (= (:project-id msg) "global"))
                                         (= (:project-id msg) "global")))))
                        (sort-by :timestamp)
                        vec)
          max-ts (when (seq new-msgs)
                   (apply max (map :timestamp new-msgs)))
          formatted-msgs (mapv (fn [{:keys [agent-id event-type message task]}]
                                 (cond-> {:a agent-id
                                          :e (if (keyword? event-type)
                                               (name event-type)
                                               event-type)
                                          :m message}
                                   task (assoc :t task)))
                               new-msgs)]
      (when max-ts
        (swap! agent-read-cursors assoc cursor-key max-ts))
      (when (seq formatted-msgs)
        formatted-msgs))))

(defn fetch-history
  "Get hivemind messages without marking as read."
  [& {:keys [since limit project-id] :or {since 0 limit 100}}]
  (if-let [source-fn @message-source-fn]
    (->> (source-fn)
         (filter (fn [msg]
                   (and (> (:timestamp msg) since)
                        (or (nil? project-id)
                            (= (:project-id msg) project-id)
                            (= (:project-id msg) "global")))))
         ;; Sort by timestamp BEFORE map for consistent FIFO order
         (sort-by :timestamp)
         (take limit)
         (mapv (fn [{:keys [agent-id event-type message task timestamp project-id]}]
                 (cond-> {:a agent-id
                          :e (if (keyword? event-type)
                               (name event-type)
                               event-type)
                          :m message
                          :ts timestamp
                          :p project-id}
                   task (assoc :t task)))))
    []))

(defn reset-cursor!
  "Reset read cursor for an agent+project. Next get-messages returns all messages.

   Arguments:
   - agent-id: Agent identifier
   - :project-id: Optional project-id. When provided, resets only that
     project's cursor. When nil, resets the 'global' cursor."
  [agent-id & {:keys [project-id]}]
  (let [effective-project (or project-id "global")
        cursor-key [agent-id effective-project]]
    (swap! agent-read-cursors dissoc cursor-key)))

(defn reset-all-cursors!
  "Reset all read cursors. For testing/debugging."
  []
  (reset! agent-read-cursors {}))
