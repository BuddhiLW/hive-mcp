(ns hive-mcp.domain.hivemind
  "Domain entities for hivemind messaging.

   DDD Domain Layer - Value Objects with encapsulated validation.

   Entities:
   - HivemindMessage: Represents a single hivemind broadcast message
   - PiggybackEnvelope: Wraps messages for tool response embedding

   SOLID Principles:
   - SRP: Each record handles its own domain concern
   - OCP: New message types can extend without modifying existing
   - DIP: Callers depend on factory fns, not implementation details

   CLARITY Framework:
   - I (Inputs guarded): Validation in constructors, fail-fast
   - R (Represented intent): Domain language, not raw maps"
  (:require [clojure.spec.alpha :as s]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Specs (internal to domain)
;; =============================================================================

(s/def ::agent-id (s/and string? #(pos? (count %))))
(s/def ::event-type #{:started :progress :completed :error :blocked})
(s/def ::event-type-string #{"started" "progress" "completed" "error" "blocked"})
(s/def ::message (s/nilable string?))
(s/def ::timestamp pos-int?)
(s/def ::cursor-ts nat-int?)

;; Abbreviated form for wire protocol (token-efficient)
(s/def ::a ::agent-id)  ; agent-id abbreviated
(s/def ::e string?)     ; event-type as string
(s/def ::m ::message)   ; message abbreviated

;; =============================================================================
;; HivemindMessage Record
;; =============================================================================

(defrecord HivemindMessage [agent-id event-type message timestamp]
  Object
  (toString [this]
    (format "[%s] %s: %s" agent-id (name event-type) (or message ""))))

(defn- normalize-event-type
  "Normalize event-type to keyword. Accepts string or keyword."
  [et]
  (cond
    (keyword? et) et
    (string? et) (keyword et)
    :else (throw (ex-info "Invalid event-type: must be keyword or string"
                          {:event-type et :type (type et)}))))

(defn- validate-agent-id!
  "Validate agent-id is non-empty string. Throws on invalid."
  [agent-id]
  (when-not (and (string? agent-id) (pos? (count agent-id)))
    (throw (ex-info "Invalid agent-id: must be non-empty string"
                    {:agent-id agent-id :type (type agent-id)}))))

(defn- validate-timestamp!
  "Validate timestamp is positive integer. Throws on invalid."
  [ts]
  (when-not (and (integer? ts) (pos? ts))
    (throw (ex-info "Invalid timestamp: must be positive integer"
                    {:timestamp ts :type (type ts)}))))

(defn hivemind-message
  "Factory function to create HivemindMessage with validation.

   Parameters:
   - agent-id: Non-empty string identifying the agent
   - event-type: Keyword or string from #{:started :progress :completed :error :blocked}
   - message: Optional string message (can be nil)
   - timestamp: Positive integer (epoch millis), defaults to current time

   Throws ex-info on invalid inputs.

   Example:
   (hivemind-message \"ling-1\" :progress \"Step 2/3 complete\")
   (hivemind-message \"ling-1\" \"completed\" \"Done\" 1234567890)"
  ([agent-id event-type message]
   (hivemind-message agent-id event-type message (System/currentTimeMillis)))
  ([agent-id event-type message timestamp]
   (validate-agent-id! agent-id)
   (validate-timestamp! timestamp)
   (let [normalized-et (normalize-event-type event-type)]
     (when-not (s/valid? ::event-type normalized-et)
       (throw (ex-info "Invalid event-type: must be one of #{:started :progress :completed :error :blocked}"
                       {:event-type event-type :normalized normalized-et})))
     (->HivemindMessage agent-id normalized-et message timestamp))))

(defn hivemind-message?
  "Check if x is a HivemindMessage record."
  [x]
  (instance? HivemindMessage x))

(defn message->map
  "Convert HivemindMessage to plain map (for JSON serialization)."
  [^HivemindMessage msg]
  {:agent-id (:agent-id msg)
   :event-type (:event-type msg)
   :message (:message msg)
   :timestamp (:timestamp msg)})

(defn message->abbreviated
  "Convert HivemindMessage to abbreviated wire format.
   Token-efficient: {:a agent-id :e event-type :m message}

   Used for piggyback embedding in tool responses."
  [^HivemindMessage msg]
  {:a (:agent-id msg)
   :e (name (:event-type msg))
   :m (:message msg)})

(defn map->message
  "Convert plain map to HivemindMessage. Validates inputs."
  [{:keys [agent-id event-type message timestamp]}]
  (hivemind-message agent-id event-type message (or timestamp (System/currentTimeMillis))))

;; =============================================================================
;; PiggybackEnvelope Record
;; =============================================================================

(defrecord PiggybackEnvelope [messages cursor-ts]
  Object
  (toString [this]
    (format "PiggybackEnvelope[%d messages, cursor=%d]" (count messages) cursor-ts)))

(defn piggyback-envelope
  "Factory function to create PiggybackEnvelope with validation.

   Parameters:
   - messages: Vector of HivemindMessage records (or maps to convert)
   - cursor-ts: Non-negative integer cursor timestamp

   The envelope holds messages for embedding in tool responses.
   cursor-ts tracks read position for per-agent message delivery.

   Example:
   (piggyback-envelope [(hivemind-message \"a1\" :progress \"hi\")] 12345)"
  [messages cursor-ts]
  (when-not (and (integer? cursor-ts) (>= cursor-ts 0))
    (throw (ex-info "Invalid cursor-ts: must be non-negative integer"
                    {:cursor-ts cursor-ts :type (type cursor-ts)})))
  (let [;; Normalize messages: convert maps to records if needed
        normalized-msgs (mapv (fn [m]
                                (cond
                                  (hivemind-message? m) m
                                  (map? m) (map->message m)
                                  :else (throw (ex-info "Invalid message in envelope"
                                                        {:message m :type (type m)}))))
                              messages)]
    (->PiggybackEnvelope normalized-msgs cursor-ts)))

(defn piggyback-envelope?
  "Check if x is a PiggybackEnvelope record."
  [x]
  (instance? PiggybackEnvelope x))

(defn envelope->abbreviated
  "Convert PiggybackEnvelope to abbreviated wire format.
   Returns vector of abbreviated messages for tool response embedding."
  [^PiggybackEnvelope env]
  (mapv message->abbreviated (:messages env)))

(defn envelope-empty?
  "Check if envelope has no messages."
  [^PiggybackEnvelope env]
  (empty? (:messages env)))

;; =============================================================================
;; Convenience Constructors
;; =============================================================================

(defn started-message
  "Create a :started event message."
  [agent-id message]
  (hivemind-message agent-id :started message))

(defn progress-message
  "Create a :progress event message."
  [agent-id message]
  (hivemind-message agent-id :progress message))

(defn completed-message
  "Create a :completed event message."
  [agent-id message]
  (hivemind-message agent-id :completed message))

(defn error-message
  "Create an :error event message."
  [agent-id message]
  (hivemind-message agent-id :error message))

(defn blocked-message
  "Create a :blocked event message."
  [agent-id message]
  (hivemind-message agent-id :blocked message))

;; =============================================================================
;; Spec Definitions for Records
;; =============================================================================

(s/def ::hivemind-message-record
  (s/and hivemind-message?
         #(s/valid? ::agent-id (:agent-id %))
         #(s/valid? ::event-type (:event-type %))
         #(s/valid? ::timestamp (:timestamp %))))

(s/def ::piggyback-envelope-record
  (s/and piggyback-envelope?
         #(s/valid? (s/coll-of ::hivemind-message-record) (:messages %))
         #(s/valid? ::cursor-ts (:cursor-ts %))))
