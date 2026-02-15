(ns hive-mcp.hivemind.event-type
  "EventType ADT — closed sum type for hivemind agent status events.

   6 enum variants (no associated data):
   - :started      Agent has begun work on a task
   - :progress     Agent reports intermediate progress
   - :completed    Agent has finished successfully (terminal)
   - :error        Agent encountered an error (terminal)
   - :blocked      Agent is blocked, needs external input
   - :wrap_notify  Session wrap notification (terminal, non-MCP)

   Runtime representation: plain maps with :adt/type and :adt/variant keys.
   Legacy keywords (:started etc.) coerce via ->event-type.

   Design: Pure ADT namespace with zero hive-mcp deps (only hive-dsl.adt).
   Metadata (descriptions, severity, slave-status) stays in event_registry.clj."
  (:require [hive-dsl.adt :refer [defadt adt-case adt-variant]]))

;; =============================================================================
;; ADT Definition
;; =============================================================================

(defadt EventType
  "Hivemind event types for agent status communication."
  :started
  :progress
  :completed
  :error
  :blocked
  :wrap_notify)

;; =============================================================================
;; Enhanced Coercion (extends generated ->event-type)
;; =============================================================================

(defn keyword->event-type
  "Coerce a keyword to an EventType variant. Throws on unknown keyword.
   Unlike ->event-type (which returns nil), this throws for invalid input.
   Use at validation boundaries."
  [kw]
  (or (->event-type kw)
      (throw (ex-info (str "Unknown EventType keyword: " kw)
                      {:keyword kw
                       :valid-types #{:started :progress :completed
                                      :error :blocked :wrap_notify}}))))

(defn string->event-type
  "Coerce a string to an EventType variant. Throws on unknown string.
   Use at MCP/transport boundary where strings arrive from JSON."
  [s]
  (keyword->event-type (keyword s)))

(defn event-type->keyword
  "Extract the variant keyword from an EventType value.
   Returns :started, :progress, :completed, :error, :blocked, or :wrap_notify."
  [et]
  (adt-variant et))

(defn event-type->string
  "Serialize an EventType to a string. For MCP/JSON output."
  [et]
  (name (adt-variant et)))

;; =============================================================================
;; Derived Predicates
;; =============================================================================

(defn terminal?
  "True if this event type ends a workflow (no further events expected)."
  [et]
  (adt-case EventType et
            :started    false
            :progress   false
            :completed  true
            :error      true
            :blocked    false
            :wrap_notify true))

(defn mcp-visible?
  "True if this event type should appear in MCP tool schema enums."
  [et]
  (adt-case EventType et
            :started    true
            :progress   true
            :completed  true
            :error      true
            :blocked    true
            :wrap_notify false))

(defn severity
  "Get the severity level for an event type."
  [et]
  (adt-case EventType et
            :started    :info
            :progress   :info
            :completed  :info
            :error      :error
            :blocked    :warn
            :wrap_notify :info))

(defn slave-status
  "Get the DataScript slave status keyword for an event type."
  [et]
  (adt-case EventType et
            :started    :working
            :progress   :working
            :completed  :idle
            :error      :error
            :blocked    :blocked
            :wrap_notify :idle))

;; =============================================================================
;; Constants
;; =============================================================================

(def all-variants
  "Set of all EventType variant keywords."
  #{:started :progress :completed :error :blocked :wrap_notify})

(def mcp-enum
  "Ordered vector of MCP-visible event type strings."
  ["started" "progress" "completed" "error" "blocked"])

(def terminal-variants
  "Set of terminal event type keywords."
  #{:completed :error :wrap_notify})
