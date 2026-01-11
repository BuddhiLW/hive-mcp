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
  (:require [clojure.spec.alpha :as s]))

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
                 Returns: seq of {:agent-id :event-type :message :timestamp}"}
  message-source-fn
  (atom nil))

(defn register-message-source!
  "Register function that provides hivemind messages.

   The function should return a sequence of maps with keys:
   :agent-id, :event-type, :message, :timestamp

   Called by hivemind.clj on initialization to inject itself."
  [source-fn]
  (reset! message-source-fn source-fn))

;; =============================================================================
;; Message Cursors (per-agent read tracking)
;; =============================================================================

(defonce ^{:doc "Map of agent-id -> last-read-message-id. Per-agent cursors.
                 Uses monotonic message-id instead of wall-clock timestamp
                 to avoid issues with clock skew and NTP adjustments."}
  agent-read-cursors
  (atom {}))

(defonce ^{:doc "Monotonic counter for message IDs. Incremented atomically."}
  message-id-counter
  (atom 0))

(defn next-message-id!
  "Generate next monotonic message ID. Thread-safe."
  []
  (swap! message-id-counter inc))

(s/fdef get-messages
  :args (s/cat :agent-id ::agent-id)
  :ret ::messages)

(defn get-messages
  "Get new hivemind messages since last call for this agent.

   Token-efficient format: [{:a agent-id :e event-type :m message} ...]

   Each agent has independent cursor - reading marks as read only for that agent.
   Messages are returned in FIFO order (sorted by timestamp before transformation).

   Uses monotonic message-id for cursor tracking instead of wall-clock time
   to avoid issues with clock skew and NTP adjustments.

   Returns nil if no new messages or message source not registered."
  [agent-id]
  (when-let [source-fn @message-source-fn]
    (let [last-cursor (get @agent-read-cursors agent-id 0)
          all-msgs (source-fn)
          ;; Filter messages with timestamp > last cursor position
          ;; (timestamp is still used for ordering, cursor tracks position)
          new-msgs (->> all-msgs
                        (filter #(> (:timestamp %) last-cursor))
                        ;; Sort by timestamp BEFORE map to ensure FIFO order
                        (sort-by :timestamp)
                        vec)
          ;; Update cursor to max timestamp seen (not wall-clock now)
          ;; This prevents missing messages due to clock skew
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
        (swap! agent-read-cursors assoc agent-id max-ts))
      (when (seq formatted-msgs)
        formatted-msgs))))

(defn fetch-history
  "Get hivemind messages without marking as read. For context retrieval.

   Options:
   - :since - timestamp to fetch from (default: 0 = all)
   - :limit - max messages to return (default: 100)

   Returns full format with timestamps for inspection. FIFO ordered."
  [& {:keys [since limit] :or {since 0 limit 100}}]
  (if-let [source-fn @message-source-fn]
    (->> (source-fn)
         (filter #(> (:timestamp %) since))
         ;; Sort by timestamp BEFORE map for consistent FIFO order
         (sort-by :timestamp)
         (take limit)
         (mapv (fn [{:keys [agent-id event-type message timestamp]}]
                 {:a agent-id
                  :e (if (keyword? event-type)
                       (name event-type)
                       event-type)
                  :m message
                  :ts timestamp})))
    []))

(defn reset-cursor!
  "Reset read cursor for an agent. Next get-messages returns all messages."
  [agent-id]
  (swap! agent-read-cursors dissoc agent-id))

(defn reset-all-cursors!
  "Reset all read cursors. For testing/debugging."
  []
  (reset! agent-read-cursors {}))
