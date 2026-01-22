(ns hive-mcp.channel.piggyback
  "Piggyback communication system for hivemind↔ling bidirectional messaging.

   Two independent subsystems:

   1. **Instruction Queue** (hivemind → ling):
      Push instructions that get drained into tool responses.
      Instructions are delivery-once: drained when piggybacked.

   2. **Message Cursor** (ling reads hivemind broadcasts):
      Track per-agent read position. Each agent has independent cursor.
      Uses injectable message-source for DIP (no hivemind dependency).

   SOLID Principles:
   - SRP: All piggyback logic in one module
   - DIP: Message source is injectable, not hardcoded to hivemind"
  (:require [clojure.spec.alpha :as s]
            [hive-mcp.guards :as guards]))
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

;; =============================================================================
;; Instruction Queue (hivemind → ling)
;; =============================================================================

(defonce ^{:doc "Map of agent-id -> [instructions...]. Drained when piggybacked."}
  instruction-queues
  (atom {}))

(defn clear-instruction-queues!
  "Clear instruction queues. GUARDED - no-op if coordinator running.

   CLARITY-Y: Yield safe failure - prevents test fixtures from
   corrupting production instruction queue state."
  []
  (guards/when-not-coordinator
   "clear-instruction-queues! called"
   (reset! instruction-queues {})))

(defn push-instruction!
  "Push an instruction to an agent's queue.

   Instruction will be piggybacked on next tool response for this agent.

   Common instruction types:
   - {:type \"flow\" :action \"pause\"}
   - {:type \"priority\" :level \"urgent\"}
   - {:type \"context\" :file \"/changed.clj\" :message \"Modified\"}
   - {:type \"coordinate\" :wait-for \"ling-2\"}"
  [agent-id instruction]
  (swap! instruction-queues update agent-id (fnil conj []) instruction))

(defn drain-instructions!
  "Drain all pending instructions for an agent.

   Returns vector of instructions and removes them from queue.
   Returns empty vector if no instructions pending."
  [agent-id]
  (let [instructions (get @instruction-queues agent-id [])]
    (swap! instruction-queues dissoc agent-id)
    instructions))

(defn peek-instructions
  "Peek at pending instructions without draining. For debugging."
  [agent-id]
  (get @instruction-queues agent-id []))

;; =============================================================================
;; Message Source (injectable for DIP)
;; =============================================================================

(defonce ^{:doc "Function that returns all messages from hivemind.
                 Set via register-message-source! on startup.
                 Returns: seq of {:agent-id :event-type :message :timestamp :project-id}"}
  message-source-fn
  (atom nil))

(defn register-message-source!
  "Register function that provides hivemind messages.

   The function should return a sequence of maps with keys:
   :agent-id, :event-type, :message, :timestamp, :project-id

   Called by hivemind.clj on initialization to inject itself."
  [source-fn]
  (reset! message-source-fn source-fn))

;; =============================================================================
;; Message Cursors (per-agent-per-project read tracking)
;; =============================================================================

(defonce ^{:doc "Map of [agent-id project-id] -> last-read-timestamp.
                 Per-agent-per-project cursors ensure cross-project isolation.
                 Uses timestamp for cursor tracking (monotonic within project).

                 CRITICAL FIX: Without project scoping, coordinator-Y would advance
                 a global cursor and consume messages meant for coordinator-X."}
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
  "Get new hivemind messages since last call for this agent+project.

   Token-efficient format: [{:a agent-id :e event-type :m message} ...]

   Arguments:
   - agent-id: Identifier for the requesting agent (coordinator)
   - :project-id: Optional project-id to filter messages. When provided:
     - Only messages for that project are returned
     - Only that project's cursor is advanced
     - Other projects' messages remain available for their coordinators

   Each agent+project combination has independent cursor. This prevents
   cross-project shout leakage where coordinator-Y consumes coordinator-X's shouts.

   Messages are returned in FIFO order (sorted by timestamp).

   Returns nil if no new messages or message source not registered."
  [agent-id & {:keys [project-id]}]
  (when-let [source-fn @message-source-fn]
    (let [;; Use composite key [agent-id project-id] for cursor isolation
          ;; nil project-id falls back to 'global' for backwards compat
          effective-project (or project-id "global")
          cursor-key [agent-id effective-project]
          last-cursor (get @agent-read-cursors cursor-key 0)
          all-msgs (source-fn)
          ;; Filter by project-id AND timestamp > last cursor
          new-msgs (->> all-msgs
                        (filter (fn [msg]
                                  (and (> (:timestamp msg) last-cursor)
                                       ;; Filter by project if specified
                                       (or (nil? project-id)
                                           (= (:project-id msg) project-id)
                                           ;; Also include 'global' messages for all projects
                                           (= (:project-id msg) "global")))))
                        ;; Sort by timestamp BEFORE map to ensure FIFO order
                        (sort-by :timestamp)
                        vec)
          ;; Update cursor to max timestamp seen
          max-ts (when (seq new-msgs)
                   (apply max (map :timestamp new-msgs)))
          formatted-msgs (mapv (fn [{:keys [agent-id event-type message]}]
                                 {:a agent-id
                                  :e (if (keyword? event-type)
                                       (name event-type)
                                       event-type)
                                  :m message})
                               new-msgs)]
      ;; Only update cursor if we have new messages
      (when max-ts
        (swap! agent-read-cursors assoc cursor-key max-ts))
      (when (seq formatted-msgs)
        formatted-msgs))))

(defn fetch-history
  "Get hivemind messages without marking as read. For context retrieval.

   Options:
   - :since - timestamp to fetch from (default: 0 = all)
   - :limit - max messages to return (default: 100)
   - :project-id - filter by project (default: nil = all projects)

   Returns full format with timestamps for inspection. FIFO ordered."
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
         (mapv (fn [{:keys [agent-id event-type message timestamp project-id]}]
                 {:a agent-id
                  :e (if (keyword? event-type)
                       (name event-type)
                       event-type)
                  :m message
                  :ts timestamp
                  :p project-id})))
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
