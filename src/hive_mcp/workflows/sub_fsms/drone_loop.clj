(ns hive-mcp.workflows.sub-fsms.drone-loop
  "Parent drone-loop FSM — composable multi-turn agentic execution.

   Replaces the imperative recur loop in drone/loop.clj with a deterministic
   FSM that composes three sub-FSMs:
   1. context-gather — compressed context reconstruction
   2. llm-call       — message building + API call + response parsing
   3. tool-execution — permission check + execute + capture result

   This is the first hive-events FSM with an internal loop edge:
   ::check-done dispatches back to ::reconstruct-context for continuation.

   ## State Graph

   ::init → ::reconstruct-context → ::llm-call → ::dispatch-response
     → (tool_calls) ::execute-tools → ::compress → ::check-done
     → (text/error) ::check-done
   ::check-done → ::reconstruct-context (LOOP) | ::finalize → ::fsm/end

   ## Data Flow

   Input (from parent/caller):
     {:task          string    ;; task description (required)
      :files         [string]  ;; target file paths
      :drone-id      string    ;; drone identifier (required)
      :max-turns     int       ;; max loop iterations (default 10)
      :model         string    ;; LLM model name
      :tools         [string]  ;; tool name allowlist (nil = all)
      :permissions   #{...}    ;; tool permission set
      :trace?        boolean   ;; emit progress events}

   Output:
     {:status         :completed|:max_steps|:error|:noop
      :result         string    ;; final text response
      :steps          [...]     ;; turn-by-turn history
      :tool_calls_made int
      :tokens         {:input :output :total}
      :turns          int
      :model          string
      :kg-stats       {:observations :reasoning :facts}}

   ## Resources (injected at run time)

     :backend          — LLMBackend instance (required)
     :tool-schemas     — vector of tool schema maps
     :session-store    — IKGStore for session store (optional)
     :agent-id         — agent ID for executor/tracking
     :execute-tools-fn — (fn [agent-id calls permissions] -> [result-msgs])
     :reconstruct-fn   — (fn [store task turn] -> context-string)

   Fire-and-forget effects (seed, emit, record) are declared as FX data
   by handlers and executed by registered FX handlers in
   hive-mcp.events.effects.drone-loop.

  (:require [hive.events.fsm :as fsm]
            [hive-mcp.workflows.sub-fsms.llm-call :as llm-call]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const default-max-turns
  "Default maximum agentic loop iterations."
  10)

(def ^:const failure-threshold
  "Consecutive failures before forced termination."
  3)

;; =============================================================================
;; Termination Logic (ported from drone/loop.clj)
;; =============================================================================

(defn should-terminate?
  "Evaluate termination heuristics for the agentic loop.

   Priority order:
   1. Max turns reached
   2. Last response was text-only (no tool calls = LLM done)
   3. Consecutive failures exceed threshold
   4. Completion language detected
   5. No progress in last 3 turns

   Arguments:
     data - FSM data map with loop state

   Returns:
     {:terminate? boolean :reason string}"
  [data]
  (let [{:keys [turn max-turns steps consecutive-failures
                last-response-type completion?]} data
        max-turns (or max-turns default-max-turns)]
    (cond
      ;; 1. Max turns reached
      (>= (or turn 0) max-turns)
      {:terminate? true :reason (str "Max turns reached (" max-turns ")")}

      ;; 2. Text-only response (no tool calls = task complete)
      (= :text last-response-type)
      {:terminate? true :reason "Text-only response (no tool calls = task complete)"}

      ;; 3. Too many consecutive failures
      (>= (or consecutive-failures 0) failure-threshold)
      {:terminate? true :reason (str "Too many consecutive failures (" consecutive-failures ")")}

      ;; 4. Completion language detected by llm-call sub-FSM
      completion?
      {:terminate? true :reason "Completion language detected in response"}

      ;; 5. No progress: last 3 turns all had failures
      (and (>= (count (or steps [])) 3)
           (every? (fn [step]
                     (or (= :error (:response-type step))
                         (and (= :tool_calls (:response-type step))
                              (:all-failed? step))))
                   (take-last 3 (or steps []))))
      {:terminate? true :reason "No progress in last 3 turns"}

      ;; Continue
      :else
      {:terminate? false :reason "Continuing"})))

;; =============================================================================
;; Handlers (Pure: resources, data -> {:data data', :fx [...]})
;; =============================================================================

(defn handle-init
  "Initialize the drone loop session.

   Sets up:
   - Turn counter, token accumulators, step history
   - System prompt from task spec

   FX declared: :drone/seed-session, :drone/emit
   Resources used: :session-store (passed to FX)"
  [resources data]
  (let [{:keys [task files drone-id max-turns model trace?]} data
        {:keys [session-store]} resources
        max-turns (or max-turns default-max-turns)]

    (log/info "Drone loop FSM init"
              {:drone-id drone-id
               :max-turns max-turns
               :model model
               :files (count (or files []))
               :has-session-store? (some? session-store)})

    {:data (assoc data
                  :turn 0
                  :steps []
                  :tool-calls-made 0
                  :total-tokens {:input 0 :output 0 :total 0}
                  :consecutive-failures 0
                  :last-response-type nil
                  :last-text nil
                  :completion? false
                  :obs-count 0
                  :reason-count 0
                  :max-turns max-turns
                  :system-prompt (llm-call/default-system-prompt
                                  {:files (or files [])}))
     :fx (cond-> []
           session-store
           (conj [:drone/seed-session {:store session-store
                                       :task task
                                       :files files}])
           trace?
           (conj [:drone/emit {:event-type :agentic-loop-started
                               :data {:drone-id drone-id
                                      :max-turns max-turns
                                      :model model}}]))}))

(defn handle-reconstruct-context
  "Reconstruct compressed context from session store for current turn.

   On turn 0: uses raw task as context (no history yet).
   On turn N>0: delegates to reconstruct-fn which queries the session store
   for compressed observations/reasoning from previous turns.

   Resources used: :reconstruct-fn, :session-store"
  [resources data]
  (let [{:keys [turn task]} data
        {:keys [reconstruct-fn session-store]} resources
        context (if (and reconstruct-fn session-store (pos? (or turn 0)))
                  (try
                    (reconstruct-fn session-store task turn)
                    (catch Exception e
                      (log/debug "Context reconstruction failed, using raw task"
                                 {:turn turn :error (.getMessage e)})
                      task))
                  task)]
    (assoc data :context context)))

(defn handle-llm-call
  "Delegate to the llm-call sub-FSM for LLM interaction.

   Builds input from current context + history, runs the llm-call sub-FSM,
   and merges response data back into parent state.

   Resources used: :backend, :tool-schemas (via llm-call sub-FSM)"
  [resources data]
  (let [{:keys [context system-prompt turn messages]} data
        {:keys [backend tool-schemas]} resources

        ;; Build messages for llm-call
        llm-input {:task context
                   :files (:files data)
                   :turn (or turn 0)
                   :system-prompt system-prompt
                   :messages messages}

        ;; Run llm-call sub-FSM
        llm-result (if backend
                     (llm-call/run-llm-call llm-input
                                            {:backend backend
                                             :tool-schemas tool-schemas})
                     {:response-type :error
                      :error "No LLM backend provided"
                      :tool-calls nil
                      :text-content nil
                      :completion? false
                      :usage nil})]

    (log/debug "LLM call result"
               {:turn turn
                :response-type (:response-type llm-result)
                :has-tool-calls? (boolean (seq (:tool-calls llm-result)))
                :completion? (:completion? llm-result)
                :usage (:usage llm-result)})

    ;; Merge sub-FSM output into parent data
    (let [usage (:usage llm-result)
          prev-tokens (or (:total-tokens data) {:input 0 :output 0 :total 0})
          updated-tokens (if usage
                           {:input  (+ (:input prev-tokens)  (or (:input usage) 0))
                            :output (+ (:output prev-tokens) (or (:output usage) 0))
                            :total  (+ (:total prev-tokens)  (or (:total usage) 0))}
                           prev-tokens)]
      (assoc data
             :response-type (:response-type llm-result)
             :tool-calls    (:tool-calls llm-result)
             :text-content  (:text-content llm-result)
             :completion?   (:completion? llm-result)
             :error         (:error llm-result)
             :total-tokens  updated-tokens))))

(defn handle-dispatch-response
  "Route based on LLM response type — text, tool_calls, or error.

   Does NOT change data — this is a pure routing state.
   The dispatch predicates determine the next state."
  [_resources data]
  data)

(defn handle-execute-tools
  "Execute tool calls from the LLM response.

   Delegates to execute-tools-fn resource for actual execution.
   Collects results and tracks success/failure metrics.

   FX declared: :drone/emit (when trace?)
   Resources used: :execute-tools-fn, :agent-id"
  [resources data]
  (let [{:keys [tool-calls turn drone-id permissions trace?]} data
        {:keys [execute-tools-fn agent-id]} resources
        agent-id (or agent-id drone-id)]

    (if-not (and execute-tools-fn (seq tool-calls))
      ;; No tools to execute or no executor
      {:data (assoc data
                    :tool-results []
                    :all-failed? false
                    :last-response-type :tool_calls
                    :last-text "No tools executed")
       :fx []}

      (let [tool-names (mapv :name tool-calls)
            _ (log/info "Executing tools"
                        {:drone-id drone-id :turn turn :tools tool-names})

            ;; Execute via resource fn (value-producing — stays inline)
            tool-results (try
                           (execute-tools-fn agent-id tool-calls (or permissions #{}))
                           (catch Exception e
                             (log/error e "Tool execution failed"
                                        {:drone-id drone-id :turn turn})
                             ;; Return error results for each call
                             (mapv (fn [_] {:content (str "Error: " (ex-message e))})
                                   tool-calls)))

            ;; Count failures
            batch-failures (count (filter
                                   (fn [r] (str/starts-with?
                                            (or (:content r) "") "Error:"))
                                   tool-results))
            all-failed? (and (seq tool-results)
                             (= batch-failures (count tool-results)))
            result-summary (str/join "; " (map :content tool-results))]

        {:data (assoc data
                      :tool-results (vec (map vector tool-calls tool-results))
                      :all-failed? all-failed?
                      :last-response-type :tool_calls
                      :last-text result-summary
                      :tool-calls-made (+ (or (:tool-calls-made data) 0)
                                          (count tool-calls))
                      :consecutive-failures (if all-failed?
                                              (inc (or (:consecutive-failures data) 0))
                                              0))
         :fx (cond-> []
               trace?
               (conj [:drone/emit {:event-type :agentic-loop-turn
                                   :data {:drone-id drone-id :turn turn
                                          :phase :executing-tools
                                          :tools tool-names}}]))}))))

(defn handle-compress
  "Declare FX for recording tool results and reasoning in session store.

   Each tool call result becomes an observation FX. The LLM's reasoning
   (tool selection rationale) is recorded separately via FX.

   FX declared: :drone/record-obs (per tool result), :drone/record-reason
   Resources used: :session-store (passed to FX)"
  [resources data]
  (let [{:keys [turn tool-results files]} data
        {:keys [session-store]} resources
        new-obs-count (+ (or (:obs-count data) 0)
                         (count (or tool-results [])))
        tool-names (mapv (comp :name first) tool-results)

        ;; Build observation FX — one per tool result
        obs-fx (when (and session-store (seq tool-results))
                 (mapv (fn [[call result-msg]]
                         (let [tool-name (:name call)
                               content (or (:content result-msg) "")
                               success? (not (str/starts-with? content "Error:"))]
                           [:drone/record-obs
                            {:store     session-store
                             :turn      turn
                             :tool-name tool-name
                             :result    {:success success?
                                         :result  {:text content}
                                         :error   (when-not success? content)}
                             :opts      {:file (first files)}}]))
                       tool-results))

        ;; Build reasoning FX
        reason-fx (when session-store
                    [[:drone/record-reason
                      {:store  session-store
                       :turn   turn
                       :action (str "Execute tools: " (str/join ", " tool-names))
                       :detail "LLM requested tool execution"}]])]

    ;; Increment turn and record step (pure data transform)
    {:data (-> data
               (assoc :obs-count new-obs-count
                      :reason-count (inc (or (:reason-count data) 0))
                      :turn (inc (or turn 0)))
               (update :steps conj {:turn turn
                                    :response-type (:response-type data)
                                    :tool-calls-count (count tool-results)
                                    :all-failed? (:all-failed? data)}))
     :fx (into (or obs-fx []) reason-fx)}))

(defn handle-check-done
  "Evaluate termination heuristics to decide: loop or finalize.

   For text/error responses (no tool execution path), also increments
   turn counter and records the step before checking termination.

   FX declared: :drone/record-reason (for text responses), :drone/emit
   Resources used: :session-store (passed to FX)
   The dispatch predicates (done? / continue?) determine the next state."
  [resources data]
  (let [{:keys [turn response-type text-content error trace?]} data
        {:keys [session-store]} resources

        ;; For text/error responses, we need to advance turn here
        ;; (tool responses go through ::execute-tools → ::compress first)
        [data text-fx]
        (if (#{:text :error} response-type)
          (let [content (or text-content error "")
                reason-fx (when (and session-store (= :text response-type))
                            [[:drone/record-reason
                              {:store  session-store
                               :turn   turn
                               :action "Final response (text-only)"
                               :detail (subs content 0 (min 200 (count content)))}]])]
            [(-> data
                 (assoc :turn (inc (or turn 0))
                        :last-text content
                        :last-response-type response-type
                        :consecutive-failures
                        (if (= :error response-type)
                          (inc (or (:consecutive-failures data) 0))
                          0))
                 (update :steps conj {:turn turn
                                      :response-type response-type
                                      :all-failed? (= :error response-type)}))
             (or reason-fx [])])
          [data []])

        term-result (should-terminate? data)

        emit-fx (when trace?
                  [[:drone/emit {:event-type :agentic-loop-turn
                                 :data {:drone-id (:drone-id data)
                                        :turn (:turn data)
                                        :phase :check-done
                                        :terminate? (:terminate? term-result)
                                        :reason (:reason term-result)}}]])]

    ;; Store termination result for dispatch predicates
    {:data (assoc data :termination term-result)
     :fx (into text-fx (or emit-fx []))}))

(defn handle-finalize
  "Build the final execution result.

   Computes status from termination state and assembles the result map
   matching the contract of run-agentic-loop in drone/loop.clj.

   FX declared: :drone/emit (when trace?)"
  [_resources data]
  (let [{:keys [turn max-turns last-response-type last-text
                steps tool-calls-made total-tokens model
                obs-count reason-count drone-id termination trace?]} data

        status (cond
                 (>= (or turn 0) (or max-turns default-max-turns)) :max_steps
                 (= :text last-response-type)                      :completed
                 :else                                              :completed)

        final-result (or last-text
                         (str "Loop terminated: "
                              (:reason termination "unknown")))]

    (log/info "Drone loop FSM finalized"
              {:drone-id drone-id
               :status status
               :turns turn
               :reason (:reason termination)
               :tool-calls tool-calls-made})

    ;; Return the result in the same shape as drone/loop.clj
    {:data (assoc data
                  :fsm-result
                  {:status          status
                   :result          final-result
                   :steps           (or steps [])
                   :tool_calls_made (or tool-calls-made 0)
                   :tokens          (or total-tokens {:input 0 :output 0 :total 0})
                   :turns           (or turn 0)
                   :model           (or model "unknown")
                   :kg-stats        {:observations (or obs-count 0)
                                     :reasoning    (or reason-count 0)
                                     :facts        0}})
     :fx (cond-> []
           trace?
           (conj [:drone/emit {:event-type :agentic-loop-completed
                               :data {:drone-id drone-id
                                      :status status
                                      :turns turn
                                      :tool-calls tool-calls-made}}]))}))

;; =============================================================================
;; Dispatch Predicates
;; =============================================================================

(defn initialized?
  "Data has been initialized (turn counter exists)."
  [data]
  (contains? data :turn))

(defn has-context?
  "Context has been reconstructed."
  [data]
  (contains? data :context))

(defn llm-responded?
  "LLM response has been parsed."
  [data]
  (contains? data :response-type))

(defn has-tool-calls?
  "LLM response contains tool calls to execute."
  [data]
  (boolean (and (= :tool_calls (:response-type data))
                (seq (:tool-calls data)))))

(defn no-tool-calls?
  "LLM response is text or error (no tools to execute)."
  [data]
  (or (= :text (:response-type data))
      (= :error (:response-type data))
      (and (= :tool_calls (:response-type data))
           (empty? (:tool-calls data)))))

(defn tools-executed?
  "Tool execution phase is complete."
  [data]
  (contains? data :tool-results))

(defn compressed?
  "Compression phase is complete (turn incremented, step recorded)."
  [data]
  (and (contains? data :obs-count)
       (pos? (count (or (:steps data) [])))))

(defn done?
  "Termination heuristics say we should stop."
  [data]
  (get-in data [:termination :terminate?] false))

(defn continue?
  "Termination heuristics say we should continue (loop edge)."
  [data]
  (not (done? data)))

(defn finalized?
  "FSM result has been built."
  [data]
  (contains? data :fsm-result))

;; =============================================================================
;; FSM Spec
;; =============================================================================

(def drone-loop-spec
  "Parent drone-loop FSM specification.

   First hive-events FSM with an internal loop edge:
   ::check-done → ::reconstruct-context (when continue?)

   States:
   1. ::init                 — Setup session, seed KG, zero counters
   2. ::reconstruct-context  — Rebuild compressed context from session store
   3. ::llm-call             — Call LLM via llm-call sub-FSM
   4. ::dispatch-response    — Route based on response type
   5. ::execute-tools        — Execute tool calls
   6. ::compress       — Record observations in session store
   7. ::check-done           — Evaluate termination heuristics
   8. ::finalize             — Build result, cleanup"
  {:fsm {::fsm/start
         {:handler    handle-init
          :dispatches [[::reconstruct-context initialized?]
                       [::fsm/error (constantly true)]]}

         ::reconstruct-context
         {:handler    handle-reconstruct-context
          :dispatches [[::llm-call has-context?]
                       [::fsm/error (constantly true)]]}

         ::llm-call
         {:handler    handle-llm-call
          :dispatches [[::dispatch-response llm-responded?]
                       [::fsm/error (constantly true)]]}

         ::dispatch-response
         {:handler    handle-dispatch-response
          :dispatches [[::execute-tools has-tool-calls?]
                       [::check-done no-tool-calls?]
                       [::fsm/error (constantly true)]]}

         ::execute-tools
         {:handler    handle-execute-tools
          :dispatches [[::compress tools-executed?]
                       [::fsm/error (constantly true)]]}

         ::compress
         {:handler    handle-compress
          :dispatches [[::check-done compressed?]
                       [::fsm/error (constantly true)]]}

         ::check-done
         {:handler    handle-check-done
          :dispatches [;; LOOP EDGE: continue → back to context reconstruction
                       [::reconstruct-context continue?]
                       ;; Terminal: done → finalize
                       [::finalize done?]
                       [::fsm/error (constantly true)]]}

         ::finalize
         {:handler    handle-finalize
          :dispatches [[::fsm/end finalized?]
                       [::fsm/error (constantly true)]]}

         ::fsm/end
         {:handler (fn [_r data] data)}

         ::fsm/error
         {:handler (fn [_r data]
                     (log/warn "drone-loop FSM error:" (:error data))
                     (assoc data
                            :fsm-result
                            {:status          :error
                             :result          (or (:error data) "FSM error")
                             :steps           (or (:steps data) [])
                             :tool_calls_made (or (:tool-calls-made data) 0)
                             :tokens          (or (:total-tokens data)
                                                  {:input 0 :output 0 :total 0})
                             :turns           (or (:turn data) 0)
                             :model           (or (:model data) "unknown")
                             :kg-stats        {:observations (or (:obs-count data) 0)
                                               :reasoning (or (:reason-count data) 0)
                                               :facts 0}}))}}

   :opts {:max-trace 50}})  ;; Higher trace limit for looping FSM

;; Compile once, reuse
(def compiled
  "Pre-compiled drone-loop FSM.
   Use with: (fsm/run @compiled resources {:data input-map})"
  (delay (fsm/compile drone-loop-spec)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn- clean-internal-fields
  "Remove FSM-internal fields from the result data.
   ::fsm/end handler does NOT execute (engine design), so cleanup
   happens here in the public API wrapper."
  [data]
  (dissoc data
          :context :response-type :tool-calls :text-content
          :completion? :error :tool-results :all-failed?
          :termination :system-prompt :messages
          :obs-count :reason-count))

(defn run-drone-loop
  "Execute the drone-loop FSM.

   Main entry point for FSM-based agentic execution.
   Replaces the imperative loop in drone/loop.clj.

   Args:
     data      - Input map (see ns docstring for shape)
     resources - Map with :backend, :tool-schemas, :execute-tools-fn, etc.

   Returns:
     Result map matching drone/loop.clj run-agentic-loop contract:
     {:status :completed|:max_steps|:error|:noop
      :result string
      :steps [...]
      :tool_calls_made int
      :tokens {:input :output :total}
      :turns int
      :model string
      :kg-stats {...}}"
  [data resources]
  (if-not (:backend resources)
    {:status          :noop
     :result          "No LLM backend provided"
     :steps           []
     :tool_calls_made 0
     :tokens          {:input 0 :output 0 :total 0}
     :turns           0
     :model           "none"
     :kg-stats        nil}
    ;; Run the FSM
    (try
      (let [fsm-result (fsm/run @compiled resources {:data data})
            result-data (-> (:data fsm-result)
                            clean-internal-fields)]
        ;; Extract the :fsm-result from data (our structured output)
        (or (:fsm-result result-data)
            ;; Fallback: build result from raw data
            {:status          :error
             :result          "FSM completed without building result"
             :steps           (or (:steps result-data) [])
             :tool_calls_made (or (:tool-calls-made result-data) 0)
             :tokens          (or (:total-tokens result-data)
                                  {:input 0 :output 0 :total 0})
             :turns           (or (:turn result-data) 0)
             :model           (or (:model result-data) "unknown")
             :kg-stats        nil}))
      (catch Exception e
        (log/error e "drone-loop FSM unexpected error" {:drone-id (:drone-id data)})
        {:status          :error
         :result          (str "FSM exception: " (ex-message e))
         :steps           []
         :tool_calls_made 0
         :tokens          {:input 0 :output 0 :total 0}
         :turns           0
         :model           (or (:model data) "unknown")
         :kg-stats        nil}))))

(defn make-production-resources
  "Create resources map wired to production dependencies.

   Lazily resolves implementations via requiring-resolve to avoid
   hard namespace dependencies.

   Args:
     backend       - LLMBackend instance
     opts          - Optional overrides:
       :session-store  - IKGStore for session store
       :tools          - tool name allowlist
       :permissions    - tool permission set
       :agent-id       - agent ID string

   Note: :trace? is now read from FSM data (not resources).
   Fire-and-forget effects use registered FX handlers.

   Returns:
     Resources map for run-drone-loop."
  [backend & [opts]]
  (let [{:keys [session-store tools permissions agent-id]} opts
        resolve-fn (fn [sym]
                     (try (requiring-resolve sym)
                          (catch Exception _ nil)))
        tool-schemas (when-let [get-schemas-fn (resolve-fn 'hive-mcp.agent.registry/get-schemas)]
                       (get-schemas-fn tools))
        execute-fn (when-let [exec-fn (resolve-fn 'hive-mcp.agent.executor/execute-tool-calls)]
                     exec-fn)]
    ;; Fire-and-forget effects (seed, emit, record-obs, record-reason)
    ;; are now declared as FX data by handlers and executed by registered
    ;; FX handlers in hive-mcp.events.effects.drone-loop.
    {:backend          backend
     :tool-schemas     tool-schemas
     :session-store    session-store
     :agent-id         agent-id
     :execute-tools-fn (when execute-fn
                         (fn [aid calls perms]
                           (if-let [with-ctx (resolve-fn 'hive-mcp.agent.context/with-request-context)]
                             ;; Use macro-expanded form
                             (let [ctx-fn (resolve-fn 'hive-mcp.agent.context/set-request-context!)
                                   clear-fn (resolve-fn 'hive-mcp.agent.context/clear-request-context!)]
                               (try
                                 (when ctx-fn (ctx-fn {:agent-id aid}))
                                 (execute-fn aid calls perms)
                                 (finally
                                   (when clear-fn (clear-fn)))))
                             (execute-fn aid calls perms))))
     :reconstruct-fn   (when-let [f (resolve-fn 'hive-mcp.agent.drone.session-kg/reconstruct-context)]
                         f)}))
