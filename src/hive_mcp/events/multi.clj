(ns hive-mcp.events.multi
  "Pure orchestration layer using multimethods for event dispatch.

   Provides an extensible, multimethod-based event handling system that
   sits above the core event machinery (hive-mcp.events.core) and enables:

   1. **Open dispatch** — New event types via `defmethod`, no modification needed
   2. **Composable pipelines** — Chain orchestrations via `:next` / `:dispatch-n`
   3. **Cross-domain coordination** — Orchestrate flows spanning ling, wave, session, crystal
   4. **Pure handlers** — Orchestrators return effect maps, no side effects
   5. **Middleware** — Pre/post processing via `wrap-orchestrator`

   ## Architecture

   ```
   [event vector]
       |
       v
   orchestrate (defmulti, dispatch on event-id)
       |
       v
   {:effects {...}        ;; Side effects to execute
    :next    [:event ...]  ;; Optional: chain to next event
    :dispatch-n [...]      ;; Optional: fan-out to multiple events
    :halt?   true}         ;; Optional: stop pipeline (don't chain)
   ```

   ## Relationship to Existing Namespaces

   - **events/core.clj**: Low-level dispatch + interceptor chain. `multi` uses core for
     effect execution but provides a higher-level orchestration API.
   - **events/handlers.clj**: Domain-specific handlers (ling, wave, session, etc.).
     Registered via `reg-event`. `multi` can delegate to these or compose them.
   - **events/bridge.clj**: Legacy hook->event transformation (defmulti hook->event).
     `multi` is the next stage — what happens *after* transformation.
   - **events/effects.clj**: Concrete fx/cofx implementations.
     `multi` produces effect maps that effects.clj executes.

   ## Usage

   ```clojure
   ;; Define an orchestrator for a cross-domain flow
   (defmethod orchestrate :forge/cycle-complete
     [event]
     (let [{:keys [agent-id results]} (second event)]
       {:effects {:shout {:agent-id agent-id
                          :event-type :completed
                          :data results}
                  :log {:level :info
                        :message (str \"Forge cycle done: \" agent-id)}}
        :next [:kanban/sync {:project (get results :project)}]}))

   ;; Execute the orchestration
   (execute! [:forge/cycle-complete {:agent-id \"ling-1\" :results {...}}])
   ```"

  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Core Multimethod: orchestrate
;; =============================================================================

(defmulti orchestrate
  "Dispatch an event to its orchestration handler.

   Dispatches on the first element (event-id) of the event vector.
   Returns an orchestration result map:

   {:effects    {kw data}    ;; Effect map (same shape as reg-event handlers)
    :next       [:event ...] ;; Optional: single event to chain
    :dispatch-n [[:ev1 ...] [:ev2 ...]] ;; Optional: fan-out events
    :halt?      boolean      ;; Optional: stop pipeline processing
    :ctx        map}         ;; Optional: context to pass downstream

   Arguments:
   - event: Event vector like [:event-id data-map]

   Example:
   ```clojure
   (defmethod orchestrate :session/full-lifecycle
     [event]
     (let [{:keys [agent-id]} (second event)]
       {:effects {:log {:level :info :message (str \"Full lifecycle: \" agent-id)}}
        :next [:session/wrap {:agent-id agent-id}]}))
   ```"
  (fn [event] (first event)))

;; Default: unknown events return empty orchestration (no-op)
(defmethod orchestrate :default
  [event]
  (log/trace "[multi] No orchestrator for:" (first event))
  nil)

;; =============================================================================
;; Orchestration Result Accessors
;; =============================================================================

(defn effects
  "Extract effects map from orchestration result."
  [result]
  (:effects result))

(defn next-event
  "Extract the next event to chain from orchestration result."
  [result]
  (:next result))

(defn fan-out-events
  "Extract fan-out events from orchestration result."
  [result]
  (:dispatch-n result))

(defn halt?
  "Check if orchestration signals pipeline halt."
  [result]
  (boolean (:halt? result)))

(defn orchestration-ctx
  "Extract downstream context from orchestration result."
  [result]
  (:ctx result))

;; =============================================================================
;; Pipeline Execution
;; =============================================================================

(defn- valid-event?
  "Check if value is a valid event vector (keyword-first vector)."
  [v]
  (and (vector? v)
       (seq v)
       (keyword? (first v))))

(defn- resolve-dispatch-fn
  "Lazily resolve hive-mcp.events.core/dispatch to avoid circular deps.
   Returns the dispatch function or nil."
  []
  (try
    (requiring-resolve 'hive-mcp.events.core/dispatch)
    (catch Exception _
      nil)))

(defn- resolve-handler-registered-fn
  "Lazily resolve hive-mcp.events.core/handler-registered? to avoid circular deps."
  []
  (try
    (requiring-resolve 'hive-mcp.events.core/handler-registered?)
    (catch Exception _
      nil)))

(defn- execute-effects!
  "Execute effects from an orchestration result through hive-events fx system.

   Dispatches a synthetic event through core/dispatch if the event-id has a
   registered handler, otherwise executes effects directly via do-fx.

   For orchestration results, we use the fx registry directly rather than
   going through the full reg-event dispatch chain."
  [result event]
  (when-let [fx-map (effects result)]
    (when (seq fx-map)
      (let [event-id (first event)]
        (doseq [[fx-id fx-data] fx-map]
          (try
            (if-let [get-fx (requiring-resolve 'hive.events.fx/get-fx)]
              (if-let [handler (get-fx fx-id)]
                (do
                  (handler fx-data)
                  (log/trace "[multi] Effect executed:" fx-id))
                (log/warn "[multi] No fx handler for:" fx-id "in orchestration of" event-id))
              (log/warn "[multi] Cannot resolve hive.events.fx/get-fx"))
            (catch Exception e
              (log/error "[multi] Effect" fx-id "failed in orchestration of" event-id
                         ":" (.getMessage e)))))))))

(defn- dispatch-event!
  "Dispatch a single event through the core event system.

   If the event-id has a registered handler in core, dispatches there.
   Otherwise, recursively orchestrates via this namespace."
  [event]
  (when (valid-event? event)
    (let [handler-registered? (resolve-handler-registered-fn)
          dispatch-fn (resolve-dispatch-fn)]
      (if (and handler-registered? (handler-registered? (first event)))
        ;; Delegate to core dispatch (has interceptors, metrics, etc.)
        (when dispatch-fn
          (try
            (dispatch-fn event)
            (catch Exception e
              (log/error "[multi] Core dispatch failed for" (first event) ":" (.getMessage e)))))
        ;; Try orchestration (recursive, for multi-only events)
        (let [result (orchestrate event)]
          (when result
            (execute-effects! result event)
            ;; Chain next event if not halted
            (when-not (halt? result)
              (when-let [next-ev (next-event result)]
                (dispatch-event! next-ev))
              (when-let [fan-events (fan-out-events result)]
                (doseq [ev fan-events]
                  (dispatch-event! ev))))))))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn execute!
  "Execute an orchestration pipeline for the given event.

   1. Calls `orchestrate` multimethod to get the orchestration result
   2. Executes effects from the result via hive-events fx system
   3. Chains to `:next` event if present (and not `:halt?`)
   4. Fans out to `:dispatch-n` events if present

   Events that have registered handlers in core are dispatched there.
   Events only handled by orchestrate stay in the multi pipeline.

   Args:
   - event: Event vector like [:event-id data-map]

   Returns the orchestration result map (or nil if no orchestrator).

   Example:
   ```clojure
   (execute! [:forge/cycle-complete {:agent-id \"ling-1\"}])
   ;; => {:effects {:shout {...}} :next [:kanban/sync {...}]}
   ```"
  [event]
  {:pre [(valid-event? event)]}
  (let [result (orchestrate event)]
    (when result
      (execute-effects! result event)
      ;; Chain next event if not halted
      (when-not (halt? result)
        (when-let [next-ev (next-event result)]
          (dispatch-event! next-ev))
        (when-let [fan-events (fan-out-events result)]
          (doseq [ev fan-events]
            (dispatch-event! ev)))))
    result))

(defn execute-async!
  "Like `execute!` but runs the pipeline in a future for non-blocking execution.

   Returns a future that will contain the orchestration result.

   Use when orchestration triggers side effects that may be slow
   (e.g., git commits, memory writes, network calls).

   Example:
   ```clojure
   (def result-future (execute-async! [:forge/cycle-complete {:agent-id \"ling-1\"}]))
   ;; ... do other work ...
   @result-future ;; => {:effects {...} :next [...]}
   ```"
  [event]
  {:pre [(valid-event? event)]}
  (future
    (try
      (execute! event)
      (catch Exception e
        (log/error "[multi] Async orchestration failed for" (first event)
                   ":" (.getMessage e))
        nil))))

;; =============================================================================
;; Middleware / Wrapping
;; =============================================================================

(defn wrap-orchestrator
  "Create a wrapped orchestrator with pre/post processing.

   Takes an orchestration function and optional middleware:
   - :before — fn [event] -> event (transform before orchestration)
   - :after  — fn [result event] -> result (transform after orchestration)
   - :error  — fn [exception event] -> result (handle errors)

   Returns a function suitable for use in defmethod bodies.

   Example:
   ```clojure
   (def with-logging
     (wrap-orchestrator
       (fn [event]
         (let [{:keys [agent-id]} (second event)]
           {:effects {:shout {:agent-id agent-id :event-type :completed}}}))
       :before (fn [event]
                 (log/info \"Orchestrating:\" (first event))
                 event)
       :after (fn [result event]
                (assoc-in result [:effects :log]
                  {:level :debug :message (str \"Orchestrated: \" (first event))}))))

   (defmethod orchestrate :my/event [event] (with-logging event))
   ```"
  [handler-fn & {:keys [before after error]}]
  (let [handler (if (keyword? handler-fn)
                  (fn [event] (orchestrate event))
                  handler-fn)]
    (fn [event]
      (try
        (let [event' (if before (before event) event)
              result (handler event')
              result' (if after (after result event') result)]
          result')
        (catch Exception e
          (if error
            (error e event)
            (do
              (log/error "[multi] Orchestration error for" (first event)
                         ":" (.getMessage e))
              {:effects {:log {:level :error
                               :message (str "Orchestration failed: " (.getMessage e))}}})))))))

;; =============================================================================
;; Composition Helpers
;; =============================================================================

(defn merge-results
  "Merge multiple orchestration results into one.

   Effects maps are merged (last wins for same key).
   :next events are collected into :dispatch-n.
   :halt? is true if any result halts.

   Useful for composing multiple orchestration concerns:
   ```clojure
   (defmethod orchestrate :complex/flow [event]
     (merge-results
       (logging-orchestration event)
       (telemetry-orchestration event)
       (domain-orchestration event)))
   ```"
  [& results]
  (let [valid (remove nil? results)]
    (when (seq valid)
      (let [all-effects (apply merge (map effects valid))
            all-nexts (keep next-event valid)
            all-fan-outs (mapcat (fn [r] (or (fan-out-events r) [])) valid)
            all-events (into (vec all-nexts) all-fan-outs)
            any-halt (some halt? valid)
            merged-ctx (apply merge (keep orchestration-ctx valid))]
        (cond-> {:effects all-effects}
          (= 1 (count all-events)) (assoc :next (first all-events))
          (> (count all-events) 1) (assoc :dispatch-n all-events)
          any-halt                  (assoc :halt? true)
          (seq merged-ctx)          (assoc :ctx merged-ctx))))))

(defn chain
  "Create a sequential chain of events.

   Returns an orchestration result that dispatches events in sequence
   via nested :next chaining. First event's effects are returned immediately.

   ```clojure
   (defmethod orchestrate :deploy/full [event]
     (chain
       [:deploy/build {:target \"prod\"}]
       [:deploy/test {:suite \"integration\"}]
       [:deploy/promote {:env \"production\"}]))
   ```"
  [& events]
  (let [valid-events (filter valid-event? events)]
    (when (seq valid-events)
      (reduce
       (fn [inner event]
         {:effects {}
          :next event
          :ctx {:_chain-continuation inner}})
       nil
       (reverse valid-events)))))

(defn fan-out
  "Create a fan-out of parallel events.

   Returns an orchestration result that dispatches all events concurrently.
   No ordering guarantees between events.

   ```clojure
   (defmethod orchestrate :notify/all [event]
     (let [{:keys [agent-ids message]} (second event)]
       (fan-out
         (for [id agent-ids]
           [:notify/agent {:agent-id id :message message}]))))
   ```"
  [events]
  (let [valid-events (vec (filter valid-event? events))]
    (when (seq valid-events)
      {:effects {}
       :dispatch-n valid-events})))

;; =============================================================================
;; Query / Introspection
;; =============================================================================

(defn registered-orchestrators
  "List all registered orchestrator event-ids.

   Returns a set of keywords that have defmethod implementations
   (excluding :default).

   Useful for debugging and introspection."
  []
  (disj (set (keys (methods orchestrate))) :default))

(defn orchestrator-registered?
  "Check if an orchestrator is registered for the given event-id."
  [event-id]
  (contains? (set (keys (methods orchestrate))) event-id))

;; =============================================================================
;; Built-in Orchestrators
;; =============================================================================

;; Orchestrator: :multi/ping — Health check / smoke test
(defmethod orchestrate :multi/ping
  [_event]
  {:effects {:log {:level :info
                   :message "[multi] pong — orchestration pipeline healthy"}}})

;; Orchestrator: :multi/echo — Returns event data as effects for testing
(defmethod orchestrate :multi/echo
  [event]
  (let [data (second event)]
    {:effects {:log {:level :debug
                     :message (str "[multi] echo: " (pr-str data))}}
     :ctx data}))
