(ns hive-mcp.events.core
  "Re-frame inspired event system for hive-mcp.
   
   Provides:
   - Interceptor chain execution (before/after phases)
   - Coeffects (inputs) and Effects (outputs) management
   - Event dispatch infrastructure
   - Input validation at dispatch boundary (CLARITY: Inputs are guarded)
   
   Adapted from re-frame/interceptor.cljc for JVM-only use.
   
   ## Core Concepts
   
   **Interceptors** are maps with optional :before and :after functions:
   ```clojure
   {:id     :my-interceptor
    :before (fn [context] ...) ; called during forward pass
    :after  (fn [context] ...)} ; called during reverse pass
   ```
   
   **Context** flows through the interceptor chain:
   ```clojure
   {:coeffects {:event [:event-id data]  ; inputs
                :db    app-state}
    :effects   {:db new-state            ; outputs
                :fx [[:dispatch ...]]}
    :queue     [...]                      ; remaining interceptors
    :stack     [...]}                     ; completed interceptors
   ```
   
   SOLID: Single responsibility - interceptor execution only
   CLARITY: Composition over modification - chain interceptors
   CLARITY: Inputs are guarded - malli validation at dispatch boundary"
  (:require [clojure.set :as set]
            [malli.core :as m]
            [malli.error :as me]
            [hive-mcp.events.schemas :as schemas]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.channel.websocket :as ws]))

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private *initialized (atom false))
(defonce ^:private *fx-handlers (atom {}))
(defonce ^:private *cofx-handlers (atom {}))
(defonce ^:private *event-handlers (atom {}))

;; =============================================================================
;; Metrics State (CLARITY: Telemetry first)
;; =============================================================================

(def ^:private *metrics
  "Metrics atom tracking event dispatch statistics.

   Shape:
   {:events-dispatched N      ; Total events dispatched
    :events-by-type    {kw N} ; Count per event-id keyword
    :effects-executed  N      ; Total effects successfully executed
    :errors           N       ; Total effect execution errors
    :timings-by-type  {kw []} ; Rolling window of dispatch times per event type (ms)
    :timings          [...]}  ; Rolling window of all dispatch times (ms)"
  (atom {:events-dispatched 0
         :events-by-type {}
         :effects-executed 0
         :errors 0
         :timings-by-type {}
         :timings []}))

(defn get-metrics
  "Get current metrics snapshot.

   Returns map with:
   - :events-dispatched  - Total events dispatched
   - :events-by-type     - Map of event-id -> count
   - :effects-executed   - Total effects successfully executed
   - :errors             - Total effect execution errors
   - :avg-dispatch-ms    - Average dispatch time across all events (ms)
   - :avg-by-type        - Map of event-id -> average dispatch time (ms)
   - :timings-count      - Number of timing samples
   - :timings-by-type    - Map of event-id -> [timing samples]

   CLARITY: Telemetry first - observable system."
  []
  (let [m @*metrics
        timings (:timings m)
        timings-by-type (:timings-by-type m)
        avg-ms (if (seq timings)
                 (/ (reduce + timings) (count timings))
                 0)
        avg-by-type (reduce-kv
                     (fn [acc event-id event-timings]
                       (if (seq event-timings)
                         (assoc acc event-id (/ (reduce + event-timings) (count event-timings)))
                         acc))
                     {}
                     timings-by-type)]
    (assoc m
           :avg-dispatch-ms avg-ms
           :avg-by-type avg-by-type
           :timings-count (count timings))))

(defn reset-metrics!
  "Reset all metrics counters. For testing."
  []
  (reset! *metrics {:events-dispatched 0
                    :events-by-type {}
                    :effects-executed 0
                    :errors 0
                    :timings-by-type {}
                    :timings []}))

;; =============================================================================
;; Interceptor Construction
;; =============================================================================

(def ^:private mandatory-interceptor-keys #{:id})
(def ^:private optional-interceptor-keys #{:before :after :comment})

(defn interceptor?
  "Returns true if m is a valid interceptor map."
  [m]
  (and (map? m)
       (contains? m :id)))

(defn ->interceptor
  "Create an interceptor from keyword arguments.
   
   Args:
   - :id      - Required identifier keyword
   - :before  - Optional fn [context] -> context, called in forward pass
   - :after   - Optional fn [context] -> context, called in reverse pass
   - :comment - Optional documentation string
   
   Example:
   ```clojure
   (->interceptor
     :id :debug
     :before (fn [ctx] (println \"Event:\" (get-coeffect ctx :event)) ctx)
     :after  (fn [ctx] (println \"Effects:\" (:effects ctx)) ctx))
   ```"
  [& {:as m :keys [id before after comment]}]
  (when-let [unknown-keys (seq (set/difference
                                (set (keys m))
                                mandatory-interceptor-keys
                                optional-interceptor-keys))]
    (throw (ex-info (str "->interceptor has unknown keys: " unknown-keys)
                    {:unknown-keys unknown-keys :interceptor m})))
  (when-not id
    (throw (ex-info "->interceptor requires :id" {:interceptor m})))
  (cond-> {:id id
           :before (or before identity)
           :after (or after identity)}
    comment (assoc :comment comment)))

;; =============================================================================
;; Context Access
;; =============================================================================

(defn get-coeffect
  "Get a coeffect value from context."
  ([context]
   (:coeffects context))
  ([context key]
   (get-in context [:coeffects key]))
  ([context key not-found]
   (get-in context [:coeffects key] not-found)))

(defn assoc-coeffect
  "Associate a coeffect value in context."
  [context key value]
  (assoc-in context [:coeffects key] value))

(defn update-coeffect
  "Update a coeffect value in context."
  [context key f & args]
  (apply update-in context [:coeffects key] f args))

(defn get-effect
  "Get an effect value from context."
  ([context]
   (:effects context))
  ([context key]
   (get-in context [:effects key]))
  ([context key not-found]
   (get-in context [:effects key] not-found)))

(defn assoc-effect
  "Associate an effect value in context."
  [context key value]
  (assoc-in context [:effects key] value))

(defn update-effect
  "Update an effect value in context."
  [context key f & args]
  (apply update-in context [:effects key] f args))

;; =============================================================================
;; Queue Management
;; =============================================================================

(defn enqueue
  "Add interceptors to the context's queue."
  [context interceptors]
  (update context :queue into interceptors))

;; =============================================================================
;; Interceptor Execution
;; =============================================================================

(defn- invoke-interceptor-fn
  "Invoke a single interceptor function in the given direction."
  [context interceptor direction]
  (if-let [f (get interceptor direction)]
    (try
      (f context)
      (catch Exception e
        (throw (ex-info (str "Interceptor " (:id interceptor) " " direction " threw exception")
                        {:interceptor-id (:id interceptor)
                         :direction direction
                         :cause e}
                        e))))
    context))

(defn- invoke-interceptors
  "Execute all interceptors in the queue for the given direction.
   
   Walks the queue, calling each interceptor's direction fn,
   moving processed interceptors to the stack."
  [context direction]
  (loop [ctx context]
    (let [queue (:queue ctx)]
      (if (empty? queue)
        ctx
        (let [interceptor (first queue)
              stack (:stack ctx)]
          (recur (-> ctx
                     (assoc :queue (vec (rest queue)))
                     (assoc :stack (conj (or stack []) interceptor))
                     (invoke-interceptor-fn interceptor direction))))))))

(defn- change-direction
  "Prepare context for the :after phase by reversing the stack into queue."
  [context]
  (-> context
      (assoc :queue (:stack context))
      (assoc :stack [])))

(defn- create-context
  "Create a fresh execution context."
  ([event interceptors]
   {:coeffects {:event event
                :original-event event}
    :effects {}
    :queue (vec interceptors)
    :stack []})
  ([event interceptors initial-coeffects]
   (-> (create-context event interceptors)
       (update :coeffects merge initial-coeffects))))

(defn execute
  "Execute an interceptor chain for the given event.
   
   Walks interceptors calling :before functions, then reverses
   and walks calling :after functions.
   
   Args:
   - event        - The event vector, e.g. [:event-id data]
   - interceptors - Collection of interceptor maps
   
   Returns the final context with :coeffects and :effects."
  ([event interceptors]
   (execute event interceptors {}))
  ([event interceptors initial-coeffects]
   (-> (create-context event interceptors initial-coeffects)
       (invoke-interceptors :before)
       change-direction
       (invoke-interceptors :after))))

;; =============================================================================
;; Effect & Coeffect Registration
;; =============================================================================

(defn reg-fx
  "Register an effect handler.
   
   Effect handlers execute side effects described in the :effects map.
   
   Example:
   ```clojure
   (reg-fx :shout
     (fn [data]
       (hivemind/shout! (:event-type data) (:message data))))
   ```"
  [id handler-fn]
  (swap! *fx-handlers assoc id handler-fn))

(defn reg-cofx
  "Register a coeffect handler.

   Coeffect handlers inject values into the context's :coeffects.

   Example:
   ```clojure
   (reg-cofx :now
     (fn [coeffects]
       (assoc coeffects :now (java.time.Instant/now))))
   ```"
  [id handler-fn]
  (swap! *cofx-handlers assoc id handler-fn))

(defn get-fx-handler
  "Get a registered effect handler by id. Primarily for testing."
  [id]
  (get @*fx-handlers id))

(defn get-cofx-handler
  "Get a registered coeffect handler by id. Primarily for testing."
  [id]
  (get @*cofx-handlers id))

(defn inject-cofx
  "Create an interceptor that injects a coeffect.
   
   The coeffect handler registered with `reg-cofx` will be called
   during the :before phase."
  ([id]
   (->interceptor
    :id (keyword (str "cofx-" (name id)))
    :before (fn [context]
              (if-let [handler (get @*cofx-handlers id)]
                (update context :coeffects handler)
                (do
                  (println "Warning: No coeffect handler for" id)
                  context)))))
  ([id value]
   (->interceptor
    :id (keyword (str "cofx-" (name id)))
    :before (fn [context]
              (if-let [handler (get @*cofx-handlers id)]
                (update context :coeffects #(handler % value))
                (do
                  (println "Warning: No coeffect handler for" id)
                  context))))))

;; =============================================================================
;; Effect Execution
;; =============================================================================

(defn do-fx
  "Execute all effects in the context's :effects map.
   
   Arities:
   - [context] - Uses global fx-handlers registry (production)
   - [context fx-handlers] - Uses provided registry (testing)
   
   Looks up registered effect handlers and calls them.
   Increments metrics counters for effects executed and errors.
   
   CLARITY: Telemetry first - tracks effect execution metrics."
  ([context]
   (do-fx context @*fx-handlers))
  ([context fx-handlers]
   (doseq [[effect-id effect-data] (:effects context)]
     (if-let [handler (get fx-handlers effect-id)]
       (try
         (handler effect-data)
         (swap! *metrics update :effects-executed inc)
         (catch Exception e
           (swap! *metrics update :errors inc)
           (println "Effect" effect-id "failed:" (.getMessage e))))
       (println "Warning: No effect handler for" effect-id)))
   context))

;; =============================================================================
;; Event Registration & Dispatch
;; =============================================================================

(defn reg-event
  "Register an event handler with interceptors.
   
   Example:
   ```clojure
   (reg-event :task-complete
     [debug-interceptor validate-interceptor]
     (fn [coeffects event]
       {:shout {:event-type :completed
                :message (str \"Task \" (second event) \" done\")}}))
   ```"
  [event-id interceptors handler-fn]
  (swap! *event-handlers assoc event-id
         {:interceptors interceptors
          :handler handler-fn}))

(defn dispatch
  "Dispatch an event through its registered handler chain.
   
   Arities:
   - [event] - Uses global registries (production)
   - [event handlers fx-handlers] - Uses provided registries (testing)
   
   Validates event format at boundary (CLARITY: Inputs are guarded),
   then looks up the event handler, builds the interceptor chain,
   executes it, and runs effects.
   
   Throws: ExceptionInfo if event is invalid or no handler registered."
  ([event]
   (dispatch event @*event-handlers @*fx-handlers))
  ([event handlers fx-handlers]
   (schemas/validate-event! event) ;; CLARITY: Guard inputs at boundary
   (let [event-id (first event)]
     (if-let [{:keys [interceptors handler]} (get handlers event-id)]
       (let [;; Handler interceptor converts coeffects to effects
             handler-interceptor (->interceptor
                                  :id :handler
                                  :before (fn [context]
                                            (let [coeffects (:coeffects context)
                                                  effects (handler coeffects event)]
                                              (update context :effects merge effects))))
             ;; Build full chain: registered interceptors + handler
             full-chain (conj (vec interceptors) handler-interceptor)
             ;; Execute and apply effects
             result (execute event full-chain)]
         (do-fx result fx-handlers)
         result)
       (throw (ex-info (str "No handler registered for event: " event-id)
                       {:event event}))))))

(defn dispatch-sync
  "Synchronous dispatch - same as dispatch for now.
   Future: dispatch may become async."
  [event]
  (dispatch event))

;; =============================================================================
;; Built-in Interceptors
;; =============================================================================

(def debug
  "Interceptor that logs event and effects for debugging."
  (->interceptor
   :id :debug
   :comment "Logs event in :before, effects in :after"
   :before (fn [context]
             (println "[DEBUG] Event:" (get-coeffect context :event))
             context)
   :after (fn [context]
            (println "[DEBUG] Effects:" (:effects context))
            context)))

(def metrics
  "Interceptor that tracks event dispatch metrics per event type.

   Records:
   - Total event count (incremented in :before)
   - Per-event-type count (tracked in :events-by-type)
   - Dispatch timing in ms (recorded in :after)
   - Per-event-type timings (tracked in :timings-by-type)

   Uses a rolling window of 100 timing samples per event type to compute averages.

   CLARITY Principle: Telemetry first - observable system behavior."
  (->interceptor
   :id :metrics
   :comment "Tracks event dispatch metrics for observability"
   :before (fn [context]
             (let [event (get-coeffect context :event)
                   event-id (when (vector? event) (first event))]
               (swap! *metrics
                      (fn [m]
                        (-> m
                            (update :events-dispatched inc)
                            (update-in [:events-by-type event-id] (fnil inc 0)))))
               (-> context
                   (assoc-coeffect :metrics-start-ns (System/nanoTime))
                   (assoc-coeffect :metrics-event-id event-id))))
   :after (fn [context]
            (let [start-ns (get-coeffect context :metrics-start-ns)
                  event-id (get-coeffect context :metrics-event-id)
                  elapsed-ms (when start-ns
                               (/ (- (System/nanoTime) start-ns) 1000000.0))]
              (when elapsed-ms
                (swap! *metrics
                       (fn [m]
                         (-> m
                             (update :timings #(take 100 (conj % elapsed-ms)))
                             (update-in [:timings-by-type event-id]
                                        #(take 100 (conj (or % []) elapsed-ms))))))))
            context)))

(def trim-v
  "Interceptor that removes the event-id from the event vector.
   
   Transforms [:event-id arg1 arg2] to [arg1 arg2] in :event coeffect."
  (->interceptor
   :id :trim-v
   :before (fn [context]
             (let [event (get-coeffect context :event)]
               (assoc-coeffect context :event (vec (rest event)))))))

;; =============================================================================

(defn validate-event
  "Create a validation interceptor that validates event data against a malli schema.
   
   The validation interceptor runs in the :before phase and validates:
   1. Event vector structure (always - uses schemas/Event)
   2. Event data (optional - if schema is provided)
   
   When schema is provided, it validates the event data (second element of
   the event vector) against the malli schema.
   
   Args:
   - schema (optional) - Malli schema to validate the event data against
   
   Usage:
   ```clojure
   ;; Basic structure validation only
   (reg-event :my-event
     [(validate-event)]
     handler-fn)
   
   ;; Structure + data schema validation
   (def TaskData [:map [:id :string] [:title :string]])
   (reg-event :task/create
     [(validate-event TaskData)]
     handler-fn)
   ```
   
   On validation failure, throws ex-info with:
   - :event       - The invalid event
   - :error       - Humanized error message
   - :schema-type - :structure or :data (which validation failed)
   
   CLARITY Principle: Inputs are guarded at boundaries.
   POC-14: Validation interceptor for event system."
  ([]
   (->interceptor
    :id :validate-event
    :comment "Validates event structure"
    :before (fn [context]
              (let [event (get-coeffect context :event)]
                ;; Always validate basic event structure
                (when-not (schemas/valid-event? event)
                  (throw (ex-info "Invalid event structure: event must be a vector with keyword first"
                                  {:event event
                                   :error (schemas/explain-event event)
                                   :schema-type :structure})))
                context))))
  ([data-schema]
   (->interceptor
    :id :validate-event
    :comment "Validates event structure and data schema"
    :before (fn [context]
              (let [event (get-coeffect context :event)]
                ;; First validate basic event structure
                (when-not (schemas/valid-event? event)
                  (throw (ex-info "Invalid event structure: event must be a vector with keyword first"
                                  {:event event
                                   :error (schemas/explain-event event)
                                   :schema-type :structure})))
                ;; Then validate event data against provided schema
                (let [event-data (second event)]
                  (when-not (m/validate data-schema event-data)
                    (throw (ex-info "Invalid event data: data does not match schema"
                                    {:event event
                                     :event-data event-data
                                     :error (me/humanize (m/explain data-schema event-data))
                                     :schema-type :data}))))
                context)))))

;; Initialization
;; =============================================================================

(defn init!
  "Initialize the event system.
   
   Registers built-in coeffects including:
   - :now         - Current timestamp (java.time.Instant)
   - :random      - Random number (0-1)
   - :agent-context - Swarm agent environment context (EVENTS-05)
   - :db-snapshot  - Current DataScript database state (EVENTS-05)
   
   Safe to call multiple times."
  []
  (when-not @*initialized
    ;; Register built-in coeffects
    (reg-cofx :now
              (fn [coeffects]
                (assoc coeffects :now (java.time.Instant/now))))

    (reg-cofx :random
              (fn [coeffects]
                (assoc coeffects :random (rand))))

    ;; EVENTS-05: Agent context coeffect
    ;; Injects swarm agent environment information
    (reg-cofx :agent-context
              (fn [coeffects]
                (assoc coeffects :agent-context
                       {:agent-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                        :parent-id (System/getenv "CLAUDE_SWARM_PARENT_ID")
                        :depth (some-> (System/getenv "CLAUDE_SWARM_DEPTH")
                                       Integer/parseInt)
                        :role (System/getenv "CLAUDE_SWARM_ROLE")})))

    ;; EVENTS-05: DataScript snapshot coeffect
    ;; Injects current database state for queries
    (reg-cofx :db-snapshot
              (fn [coeffects]
                (assoc coeffects :db-snapshot @(ds/get-conn))))

    ;; =============================================================================
    ;; Built-in Effects
    ;; =============================================================================

    ;; POC-05: Channel publish effect
    ;; Publishes typed events to WebSocket channel for Emacs consumption
    (reg-fx :channel-publish
            (fn [{:keys [event data]}]
              (ws/emit! event data)))

    ;; Mark as initialized
    (reset! *initialized true)
    (println "[hive-events] Event system initialized with coeffects: :now :random :agent-context :db-snapshot")
    (println "[hive-events] Registered effects: :channel-publish"))
  @*initialized)

(defn handler-registered?
  "Check if a handler is registered for the given event-id.
   Public API to avoid accessing private state."
  [event-id]
  (contains? @*event-handlers event-id))

(defmacro with-clean-registry
  "Execute body with fresh, isolated registries. For testing.
   
   Creates a clean slate for event handlers, effect handlers, and
   coeffect handlers, then restores the original state after the
   body executes (even if an exception occurs).
   
   Example:
   ```clojure
   (with-clean-registry
     (reg-event :test [] (fn [_ _] {:log {:msg \"test\"}}))
     (dispatch [:test])
     ;; registries reset after block
   )
   ```
   
   SOLID: Enables unit testing without global state pollution
   CLARITY: Clean boundary for test isolation"
  [& body]
  `(let [event-handlers# (var-get #'*event-handlers)
         fx-handlers# (var-get #'*fx-handlers)
         cofx-handlers# (var-get #'*cofx-handlers)
         old-handlers# @event-handlers#
         old-fx# @fx-handlers#
         old-cofx# @cofx-handlers#]
     (try
       (clojure.core/reset! event-handlers# {})
       (clojure.core/reset! fx-handlers# {})
       (clojure.core/reset! cofx-handlers# {})
       ~@body
       (finally
         (clojure.core/reset! event-handlers# old-handlers#)
         (clojure.core/reset! fx-handlers# old-fx#)
         (clojure.core/reset! cofx-handlers# old-cofx#)))))

(defn reset-all!
  "Reset all event system state. Primarily for testing.
   
   Resets handlers, coeffects, effects, and metrics."
  []
  (clojure.core/reset! *initialized false)
  (clojure.core/reset! *fx-handlers {})
  (clojure.core/reset! *cofx-handlers {})
  (clojure.core/reset! *event-handlers {})
  (reset-metrics!)
  nil)
