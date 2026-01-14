(ns hive-mcp.hooks
  "Domain-driven hooks system for event-driven workflow automation.

   Implements a registry-based hook system where handlers can be registered
   for specific events and triggered with context. All hook events can be
   emitted through the hivemind shout channel for Emacs integration.

   Architecture:
   - HookRegistry: Atom containing map of event-type -> [handlers]
   - Handlers: Functions (fn [context]) that receive event context
   - Events: Keywords from the hook-events set

   SOLID Principles Applied:
   - SRP: Each function has single responsibility
   - OCP: New events/handlers via registration, not modification
   - DIP: Depends on abstractions (fn interface), not concretions

   CLARITY Framework:
   - Inputs guarded: All public functions validate inputs
   - Yield safe failure: Handler errors don't stop other handlers"
  (:require [taoensso.timbre :as log]))

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

(defn unregister-hook
  "Remove a specific handler from an event.

   Arguments:
   - registry: Atom from create-registry
   - event: Keyword from hook-events set
   - handler: The exact function reference to remove

   Example:
     (unregister-hook registry :task-complete my-handler)"
  [registry event handler]
  (validate-event-type! event)
  (swap! registry update event (fn [handlers]
                                 (vec (remove #(= % handler) handlers))))
  (log/debug "Unregistered hook for" event)
  nil)

;; =============================================================================
;; Hook Listing
;; =============================================================================

(defn list-hooks
  "List all handlers registered for an event.

   Arguments:
   - registry: Atom from create-registry
   - event: Keyword from hook-events set

   Returns: Vector of handler functions"
  [registry event]
  (validate-event-type! event)
  (get @registry event []))

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

;; =============================================================================
;; Hook Clearing
;; =============================================================================

(defn clear-hooks
  "Remove all handlers for a specific event.

   Arguments:
   - registry: Atom from create-registry
   - event: Keyword from hook-events set"
  [registry event]
  (validate-event-type! event)
  (swap! registry assoc event [])
  (log/debug "Cleared all hooks for" event)
  nil)

(defn clear-all-hooks
  "Remove all handlers from all events in the registry.

   Arguments:
   - registry: Atom from create-registry"
  [registry]
  (reset! registry (into {} (map (fn [event] [event []]) hook-events)))
  (log/debug "Cleared all hooks")
  nil)

;; =============================================================================
;; Default Hooks
;; =============================================================================

(def default-hooks
  "Map of event -> [default handler fns].
   These are registered automatically by create-registry-with-defaults."
  {:task-complete [(fn [ctx]
                     (log/info "Task completed:" (:task-id ctx "unknown")))]
   :error-occurred [(fn [ctx]
                      (log/error "Error in task:" (:task-id ctx "unknown")
                                 "-" (:error ctx "no message")))]})

(defn create-registry-with-defaults
  "Creates a registry with default handlers pre-registered.

   Returns: Atom containing registry with default-hooks applied"
  []
  (let [registry (create-registry)]
    (doseq [[event handlers] default-hooks
            handler handlers]
      (register-hook registry event handler))
    registry))

;; =============================================================================
;; Hook Event Emission (Integration with Hivemind)
;; =============================================================================

(defn create-hook-event-payload
  "Create a properly structured payload for hook event emission.

   Arguments:
   - event: Keyword from hook-events set
   - context: Map of event context data

   Returns: Map with :event and :context keys

   Throws: ExceptionInfo if event is invalid"
  [event context]
  (validate-event-type! event)
  {:event event
   :context context
   :timestamp (System/currentTimeMillis)})

(defn emit-hook-event!
  "Emit a hook event through the hivemind shout channel.

   This allows Emacs to receive hook notifications for UI updates
   or further processing.

   Arguments:
   - hivemind-shout-fn: The hivemind/shout! function (dependency injection)
   - agent-id: Identifier for the emitting agent
   - event: Keyword from hook-events set
   - context: Map of event context data

   Example:
     (emit-hook-event! hivemind/shout! \"agent-1\" :task-complete
       {:task-id \"123\" :result :success})"
  [hivemind-shout-fn agent-id event context]
  (let [payload (create-hook-event-payload event context)]
    (hivemind-shout-fn agent-id :hook-event payload)
    payload))
