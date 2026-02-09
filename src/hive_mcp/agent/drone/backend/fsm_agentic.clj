(ns hive-mcp.agent.drone.backend.fsm-agentic
  "FSMAgenticBackend — IDroneExecutionBackend via composable FSM drone-loop.

   Executes drone tasks using the FSM-based agentic loop (drone_loop.clj),
   which composes sub-FSMs for context-gathering, LLM calls, and tool execution
   into a multi-turn think-act-observe cycle with KG context reconstruction.

   ## Architecture

   The FSM drone loop is a state machine with loop edges:
     init -> reconstruct-context -> llm-call -> execute-tools
          -> compress-to-kg -> check-done -> (loop back or finalize -> end)

   Each phase delegates to a compiled sub-FSM:
   - context-gather: KG-compressed context reconstruction
   - llm-call: LLM API interaction with tool schema injection
   - tool-execution: Permission-checked tool invocation with observation capture

   ## Resource Wiring

   The backend translates the IDroneExecutionBackend task-context into
   FSM resources and initial data, then runs the compiled drone-loop FSM.

   ## Dependency: drone_loop.clj

   Uses requiring-resolve to lazily resolve the drone-loop FSM namespace.
   This allows the backend to be registered before drone_loop.clj is created
   (step-8 of the plan). The backend will fail gracefully if the namespace
   is not yet on classpath.

   SOLID-O: Registered via defmethod, no modification to execution.clj.
   SOLID-D: Depends on IDroneExecutionBackend abstraction + FSM engine.
   CLARITY-Y: Graceful degradation when drone-loop FSM unavailable."
  (:require [hive-mcp.agent.drone.backend :as backend]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Lazy Resolution (drone_loop.clj may not exist yet — step-8 dependency)
;; =============================================================================

(defn- resolve-drone-loop-fn
  "Lazily resolve a function from the drone-loop FSM namespace.

   Returns the resolved var or nil if namespace is not available.
   Uses requiring-resolve to avoid hard compile-time dependency."
  [sym]
  (try
    (requiring-resolve sym)
    (catch Exception e
      (log/debug "FSM drone-loop not available:" sym (.getMessage e))
      nil)))

(defn- get-compiled-drone-loop
  "Get the compiled drone-loop FSM.

   Resolves hive-mcp.workflows.drone-loop/compiled (a delay)
   and derefs it. Returns nil if not available."
  []
  (when-let [compiled-var (resolve-drone-loop-fn
                           'hive-mcp.workflows.drone-loop/compiled)]
    (try
      @compiled-var
      (catch Exception e
        (log/warn "Failed to deref compiled drone-loop FSM:" (.getMessage e))
        nil))))

(defn- get-run-drone-loop
  "Get the run-drone-loop convenience function.

   Resolves hive-mcp.workflows.drone-loop/run-drone-loop.
   Returns the function or nil if not available."
  []
  (resolve-drone-loop-fn 'hive-mcp.workflows.drone-loop/run-drone-loop))

;; =============================================================================
;; Resource Wiring: task-context -> FSM resources + initial data
;; =============================================================================

(defn- build-fsm-resources
  "Build the FSM resources map from IDroneExecutionBackend task-context.

   The drone-loop FSM expects resources with sub-FSM compiled specs
   and external dependencies (LLM backend, tool executor, etc.).

   Arguments:
     task-context - Map from IDroneExecutionBackend/execute-drone contract
     opts         - Additional options from FSMAgenticBackend record

   Returns:
     Resources map for fsm/run."
  [task-context opts]
  (let [{:keys [model tools preset sandbox cwd drone-id]} task-context
        {:keys [llm-backend-factory tool-executor-fn]} opts]
    (cond-> {:model       model
             :tools       (or tools [])
             :preset      preset
             :sandbox     sandbox
             :cwd         (or cwd ".")
             :drone-id    drone-id}
      ;; Wire LLM backend factory if provided
      llm-backend-factory
      (assoc :llm-backend (llm-backend-factory model))

      ;; Wire tool executor if provided
      tool-executor-fn
      (assoc :tool-executor tool-executor-fn))))

(defn- build-fsm-initial-data
  "Build the FSM initial data map from task-context.

   The drone-loop FSM initial data contains the task description,
   file targets, and execution budget.

   Arguments:
     task-context - Map from IDroneExecutionBackend/execute-drone contract

   Returns:
     Initial data map for fsm/run {:data ...}."
  [task-context]
  (let [{:keys [task max-steps drone-id]} task-context
        files (get-in task-context [:sandbox :allowed-files])]
    {:task       task
     :files      (if (set? files) (vec files) (or files []))
     :max-turns  (or max-steps 20)
     :turn       0
     :drone-id   drone-id
     :messages   []
     :kg-observations []
     :tool-results    []
     :status     :running}))

;; =============================================================================
;; Result Adaptation: FSM result -> IDroneExecutionBackend contract
;; =============================================================================

(defn- adapt-fsm-result
  "Adapt the drone-loop FSM result to IDroneExecutionBackend contract.

   FSM returns:
     {:status :completed/:failed/:max-turns-exceeded,
      :result string, :turns int, :tool-calls int,
      :usage {:input-tokens N :output-tokens N}, :kg-observations [...]}

   Protocol expects:
     {:status :completed/:failed/:timeout, :result str,
      :tokens {:input-tokens N :output-tokens N}, :model str,
      :steps int, :tool-calls int}"
  [fsm-result model]
  (let [status (case (:status fsm-result)
                 :completed          :completed
                 :max-turns-exceeded :timeout
                 :failed)
        usage (or (:usage fsm-result) {})]
    {:status     status
     :result     (or (:result fsm-result) "")
     :tokens     {:input-tokens  (or (:input-tokens usage) 0)
                  :output-tokens (or (:output-tokens usage) 0)}
     :model      (or model "unknown")
     :steps      (or (:turns fsm-result) (:turn fsm-result) 0)
     :tool-calls (or (:tool-calls fsm-result) 0)
     :metadata   {:backend        :fsm-agentic
                  :kg-observations (count (or (:kg-observations fsm-result) []))}}))

;; =============================================================================
;; FSMAgenticBackend Record
;; =============================================================================

(defrecord FSMAgenticBackend [opts]
  backend/IDroneExecutionBackend

  (execute-drone [_this task-context]
    (let [{:keys [task model drone-id]} task-context]
      (log/info {:event    :fsm-agentic-backend/executing
                 :drone-id drone-id
                 :model    model
                 :task-preview (subs (or task "") 0 (min 80 (count (or task ""))))})

      ;; Try the convenience function first (preferred), then compiled FSM
      (if-let [run-fn (get-run-drone-loop)]
        ;; Path A: Use run-drone-loop convenience function
        (let [resources (build-fsm-resources task-context opts)
              init-data (build-fsm-initial-data task-context)]
          (log/debug {:event :fsm-agentic-backend/using-run-fn
                      :drone-id drone-id})
          (try
            (let [result (run-fn init-data resources)]
              (log/info {:event    :fsm-agentic-backend/completed
                         :drone-id drone-id
                         :status   (:status result)
                         :turns    (:turns result)})
              (adapt-fsm-result result model))
            (catch Exception e
              (log/error e "FSM agentic execution failed"
                         {:drone-id drone-id})
              {:status   :failed
               :result   (str "FSM drone-loop execution failed: " (ex-message e))
               :tokens   {:input-tokens 0 :output-tokens 0}
               :model    (or model "unknown")
               :steps    0
               :metadata {:backend :fsm-agentic
                          :error   (ex-message e)}})))

        ;; Path B: drone-loop FSM not available yet
        (do
          (log/warn {:event    :fsm-agentic-backend/unavailable
                     :drone-id drone-id
                     :message  "drone-loop FSM namespace not on classpath (step-8 pending)"})
          {:status   :failed
           :result   "FSM drone-loop is not yet available. The drone-loop FSM (step-8) must be implemented first."
           :tokens   {:input-tokens 0 :output-tokens 0}
           :model    (or model "unknown")
           :steps    0
           :metadata {:backend :fsm-agentic
                      :reason  :unavailable}}))))

  (supports-validation? [_this]
    ;; FSM agentic loop can include validation as a sub-FSM step
    true)

  (backend-type [_this]
    :fsm-agentic))

;; =============================================================================
;; Constructor
;; =============================================================================

(defn make-fsm-agentic-backend
  "Create an FSMAgenticBackend instance.

   Arguments:
     opts - Optional map with:
       :llm-backend-factory — (fn [model] -> LLMBackend) for LLM calls
       :tool-executor-fn    — (fn [tool-call sandbox] -> result) for tool exec

   These are wired into the drone-loop FSM resources."
  ([] (make-fsm-agentic-backend {}))
  ([opts]
   (->FSMAgenticBackend opts)))

;; =============================================================================
;; resolve-backend Registration
;; =============================================================================

(defmethod backend/resolve-backend :fsm-agentic [context]
  (make-fsm-agentic-backend
   (select-keys context [:llm-backend-factory :tool-executor-fn])))
