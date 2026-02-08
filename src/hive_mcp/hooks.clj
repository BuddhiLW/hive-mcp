(ns hive-mcp.hooks
  "Domain-driven hooks system for event-driven workflow automation.

   Implements a registry-based hook system where handlers can be registered
   for specific events and triggered with context.

   Architecture:
   - HookRegistry: Atom containing map of event-type -> [handlers]
   - Handlers: Functions (fn [context]) that receive event context
   - Events: Keywords from the hook-events set"
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Hook Events Definition
;; =============================================================================

(def hook-events
  "Set of valid hook event types.
   Used for validation and documentation of available lifecycle points."
  #{:task-start        ; Task begins execution
    :task-complete     ; Task finishes successfully
    :session-start     ; Session initialization
    :session-end       ; Session termination/cleanup
    :error-occurred    ; Error caught during execution
    :file-changed      ; File modification detected
    :commit-created    ; Git commit was created
    :branch-switched}) ; Git branch change

;; =============================================================================
;; Input Validation (CLARITY: Guard inputs)
;; =============================================================================

(defn- validate-event-type!
  "Validates that event is a known hook event type.
   Throws ExceptionInfo if invalid."
  [event]
  (when-not (contains? hook-events event)
    (throw (ex-info (str "Invalid hook event type: " event
                         ". Valid events: " (pr-str hook-events))
                    {:event event
                     :valid-events hook-events}))))

(defn- validate-handler!
  "Validates that handler is a function.
   Throws ExceptionInfo if invalid."
  [handler]
  (when-not (fn? handler)
    (throw (ex-info (str "Hook handler must be a function, got: " (type handler))
                    {:handler handler
                     :type (type handler)}))))

;; =============================================================================
;; Registry Creation
;; =============================================================================

(defn create-registry
  "Creates a new hook registry initialized with empty vectors for all events.

   Returns: Atom containing map of event-type -> []

   Example:
     (def registry (create-registry))
     @registry => {:task-complete [] :session-end [] ...}"
  []
  (atom (into {} (map (fn [event] [event []]) hook-events))))

;; =============================================================================
;; Hook Registration
;; =============================================================================

(defn register-hook
  "Register a handler function for a specific event type.

   Arguments:
   - registry: Atom from create-registry
   - event: Keyword from hook-events set
   - handler: Function (fn [context]) to call when event triggers

   Throws: ExceptionInfo if event is invalid or handler is not a function

   Example:
     (register-hook registry :task-complete
       (fn [ctx] (println \"Task done:\" (:task-id ctx))))"
  [registry event handler]
  (validate-event-type! event)
  (validate-handler! handler)
  (swap! registry update event conj handler)
  (log/debug "Registered hook for" event)
  nil)

;; =============================================================================
;; Hook Triggering (CLARITY: Yield safe failure)
;; =============================================================================

(defn- safe-call-handler
  "Safely call a handler, catching and logging any exceptions.
   Returns {:result <value>} on success or {:error <ex>} on failure."
  [handler context]
  (try
    {:result (handler context)}
    (catch Exception e
      (log/warn e "Hook handler threw exception")
      {:error e})))

(defn trigger-hooks
  "Trigger all handlers for an event with the given context.

   Handlers are executed in registration order. If a handler throws
   an exception, it is logged but execution continues with remaining
   handlers (safe failure).

   Arguments:
   - registry: Atom from create-registry
   - event: Keyword from hook-events set
   - context: Map of data passed to all handlers

   Returns: Vector of results (or {:error ex} for failed handlers)

   Example:
     (trigger-hooks registry :task-complete
       {:task-id \"123\" :result :success})"
  [registry event context]
  (validate-event-type! event)
  (let [handlers (get @registry event [])
        results (mapv #(safe-call-handler % context) handlers)]
    (log/debug "Triggered" (count handlers) "hooks for" event)
    (mapv #(if (:error %) % (:result %)) results)))

