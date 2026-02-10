(ns hive-mcp.agent.drone.loop
  "In-process agentic drone loop.

   Provides a think-act-observe loop for drone task execution:
   - Each turn: reconstruct compressed context → call LLM → execute tools → record observations
   - Context compression reduces multi-turn token usage vs raw message history
   - Termination heuristics: max turns, text-only completion, failure threshold, completion language
   - Result evaluation: structural quality assessment from tool outcomes

   Two-tier context compression:
   - Primary: enhanced extension via extension registry
   - Fallback: built-in session store (structural compression)

   When extensions are registered, enhanced implementations override:
   - Completion language detection
   - Tool selection heuristics
   - Context compression
   - Goal satisfaction analysis

   Built-in layer provides functional implementations using structural heuristics.

   CLARITY-Y: Graceful degradation — nil backends produce safe noop results.
   CLARITY-T: All loop turns logged with drone-id, turn, token usage."
  (:require [hive-mcp.agent.drone.session-kg :as session-kg]
            [hive-mcp.agent.drone.kg-session :as kg-session]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.executor :as executor]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.protocols.kg]
            [hive-mcp.channel.core :as channel]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Helpers
;; =============================================================================

;; =============================================================================
;; Completion Language Detection
;; =============================================================================

(def ^:private completion-patterns
  "Regex patterns that indicate the LLM considers its task complete.
   Built-in: Simple keyword matching. Extension may override with NLP scoring."
  [#"(?i)task\s+(is\s+)?complet(e|ed)"
   #"(?i)i('ve|\s+have)\s+(successfully\s+)?(complet|finish|done)"
   #"(?i)all\s+(changes|modifications|updates)\s+(have\s+been\s+)?(made|applied|complet)"
   #"(?i)the\s+(fix|implementation|change|update)\s+(is|has been)\s+(ready|complet|done|applied)"
   #"(?i)successfully\s+(implemented|applied|fixed|updated|created|modified)"
   #"(?i)here('s| is)\s+the\s+(summary|result|final)"
   #"(?i)(nothing|no)\s+(more|else|further)\s+(to|needs?\s+to\s+be)\s+(do|change|fix)"
   #"(?i)all\s+done"])

(defn- completion-language?
  "Check if text contains language indicating task completion.
   Returns true if any completion pattern matches."
  [text]
  (when (and text (not (str/blank? text)))
    (boolean (some #(re-find % text) completion-patterns))))

;; =============================================================================
;; Termination Logic
;; =============================================================================

(defn should-terminate?
  "Determine if the agentic loop should stop.

   Built-in implementation evaluates (in priority order):
   1. Max turns reached
   2. Last response was text-only (no tool calls = LLM considers task done)
   3. Consecutive failures exceed threshold (3)
   4. Completion language detected in last text response
   5. No progress: last 3 turns all failed

   When enhanced extension is registered, overrides with enhanced implementation
   (weighted scoring, goal satisfaction analysis).

   Arguments:
     state - Map with :turn, :steps, :consecutive-failures, :last-response-type, :last-text
     opts  - Optional map with :max-turns (default 10)

   Returns:
     {:terminate? boolean :reason string}"
  [state & [opts]]
  ;; Try enhanced extension first
  (if-let [ext-fn (ext/get-extension :al/terminate?)]
    (ext-fn state opts)
    ;; Built-in: structural termination heuristics
    (let [{:keys [turn steps consecutive-failures last-response-type last-text]} state
          max-turns (or (:max-turns opts) 10)
          failure-threshold 3]
      (cond
        ;; 1. Max turns reached
        (>= turn max-turns)
        {:terminate? true :reason (str "Max turns reached (" max-turns ")")}

        ;; 2. Last response was text-only (no tool calls = task complete)
        (= :text last-response-type)
        {:terminate? true :reason "Text-only response (no tool calls = task complete)"}

        ;; 3. Too many consecutive failures
        (>= (or consecutive-failures 0) failure-threshold)
        {:terminate? true :reason (str "Too many consecutive failures (" consecutive-failures ")")}

        ;; 4. Completion language detected
        (and last-text (completion-language? last-text))
        {:terminate? true :reason "Completion language detected in response"}

        ;; 5. No progress: check last 3 steps for all-failures
        (and (>= (count steps) 3)
             (every? (fn [step]
                       (or (= :error (:type step))
                           (and (= :tool_calls (:type step))
                                (every? (fn [c] (not (:success c))) (:calls step)))))
                     (take-last 3 steps)))
        {:terminate? true :reason "No progress in last 3 turns"}

        ;; Continue
        :else
        {:terminate? false :reason "Continuing"}))))

;; =============================================================================
;; Result Evaluation
;; =============================================================================

(defn evaluate-result
  "Evaluate whether a tool result moves toward the goal.

   Built-in implementation: structural quality assessment based on tool outcome.
   - Success → :good, continue
   - Failure with retryable error → :bad, continue (may retry)
   - Failure with fatal error → :bad, stop

   When enhanced extension is registered, overrides with enhanced implementation
   (relevance scoring against observation history).

   Arguments:
     result  - Tool execution result map {:success :result :error}
     goal    - String describing the desired outcome
     history - Vector of previous step maps

   Returns:
     Map with :quality (:good/:bad/:unknown), :continue? boolean, :reason string"
  [result goal history]
  ;; Try enhanced extension first
  (if-let [ext-fn (ext/get-extension :al/evaluate)]
    (ext-fn result goal history)
    ;; Built-in: structural quality assessment
    (let [success? (:success result)
          error-msg (str (:error result))
          ;; Retryable errors: file not found (may need to search first), permission denied
          retryable? (and (not success?)
                          (or (re-find #"(?i)not found" error-msg)
                              (re-find #"(?i)no such file" error-msg)
                              (re-find #"(?i)timeout" error-msg)))
          ;; Fatal: unknown tool, validation error
          fatal? (and (not success?)
                      (or (re-find #"(?i)unknown tool" error-msg)
                          (re-find #"(?i)rejected" error-msg)))]
      (cond
        success?
        {:quality :good :continue? true :reason "Tool execution succeeded"}

        fatal?
        {:quality :bad :continue? false :reason (str "Fatal error: " error-msg)}

        retryable?
        {:quality :bad :continue? true :reason (str "Retryable error: " error-msg)}

        :else
        {:quality :bad :continue? true :reason (str "Error: " error-msg)}))))

;; =============================================================================
;; Tool Selection
;; =============================================================================

(defn select-next-tool
  "Select the next tool to invoke based on current state and observations.

   Built-in implementation: returns nil (let LLM decide via function calling).
   This is the correct default — the LLM's tool selection is usually good enough.

   When enhanced extension is registered, overrides with enhanced implementation
   (observation pattern analysis, task decomposition, proactive tool sequences).

   Arguments:
     observations     - Vector of observation maps from session store
     available-tools  - Set of tool name strings
     task-context     - Map with :task, :files, :cwd

   Returns:
     Tool selection map or nil (nil = let LLM choose)."
  [observations available-tools task-context]
  ;; Try enhanced extension first
  (if-let [ext-fn (ext/get-extension :al/next-tool)]
    (ext-fn observations available-tools task-context)
    ;; Built-in: Let LLM decide
    nil))

;; =============================================================================
;; Message Formatting Helpers
;; =============================================================================

(defn- build-system-prompt
  "Build the system prompt for the drone LLM call."
  [task-spec]
  (let [{:keys [files]} task-spec]
    (str "You are a coding drone. You MUST use the propose_diff tool for ALL file changes. "
         "NEVER output code as plain text or markdown code blocks. "
         "Every file modification MUST go through the propose_diff tool with file_path, old_content, and new_content parameters. "
         "If you output code as text instead of calling propose_diff, your changes will be LOST and the task will FAIL. "
         "Use read_file to examine files, then propose_diff to make changes. Be concise and focused."
         (when (seq files)
           (str "\n\nTarget files: " (str/join ", " files))))))

;; =============================================================================
;; Turn Execution Helper (returns data, no recur)
;; =============================================================================

(defn- execute-turn
  "Execute a single turn of the agentic loop.

   Returns a map describing what happened:
   - {:recur? true ...} — loop should continue with these new values
   - {:recur? false :final {...}} — loop should return the :final map

   This function is extracted from the loop body so that try/catch
   doesn't wrap recur (which is illegal in Clojure)."
  [backend messages tool-schemas session-store task-spec
   {:keys [drone-id agent-id turn steps tool-calls-made total-tokens
           consecutive-failures obs-count reason-count model-name
           permissions emit!]}]
  (try
    (let [response (proto/chat backend messages tool-schemas)
          usage (:usage response)
          updated-tokens (if usage
                           {:input (+ (:input total-tokens) (or (:input usage) 0))
                            :output (+ (:output total-tokens) (or (:output usage) 0))
                            :total (+ (:total total-tokens) (or (:total usage) 0))}
                           total-tokens)]

      (case (:type response)
        ;; Text response = no tool calls = task likely complete
        :text
        (let [content (:content response)]
          (when session-store
            (session-kg/record-reasoning! session-store turn
                                          "Final response (text-only)"
                                          (subs content 0 (min 200 (count content)))))
          {:recur? true
           :turn (inc turn)
           :steps (conj steps (assoc response :turn turn))
           :tool-calls-made tool-calls-made
           :tokens updated-tokens
           :consecutive-failures 0
           :last-response-type :text
           :last-text content
           :obs-count obs-count
           :reason-count (inc reason-count)})

        ;; Tool calls = execute and observe
        :tool_calls
        (let [calls (:calls response)
              tool-names (mapv :name calls)
              _ (log/info "Agentic loop executing tools"
                          {:drone-id drone-id :turn turn :tools tool-names})
              _ (emit! :agentic-loop-turn {:drone-id drone-id :turn turn
                                           :phase :executing-tools :tools tool-names})

              tool-results (ctx/with-request-context {:agent-id agent-id}
                             (executor/execute-tool-calls agent-id calls permissions))

              new-obs-count
              (if session-store
                (reduce
                 (fn [cnt [call result-msg]]
                   (let [tool-name (:name call)
                         success? (not (str/starts-with?
                                        (or (:content result-msg) "") "Error:"))]
                     (session-kg/record-observation!
                      session-store turn tool-name
                      {:success success?
                       :result {:text (:content result-msg)}
                       :error (when-not success? (:content result-msg))}
                      {:file (first (:files task-spec))})
                     (inc cnt)))
                 obs-count
                 (map vector calls tool-results))
                obs-count)

              _ (when session-store
                  (session-kg/record-reasoning!
                   session-store turn
                   (str "Execute tools: " (str/join ", " tool-names))
                   "LLM requested tool execution"))

              batch-failures (count (filter
                                     (fn [r] (str/starts-with?
                                              (or (:content r) "") "Error:"))
                                     tool-results))
              all-failed? (= batch-failures (count tool-results))
              result-summary (str/join "; " (map :content tool-results))]

          {:recur? true
           :turn (inc turn)
           :steps (conj steps (assoc response :turn turn))
           :tool-calls-made (+ tool-calls-made (count calls))
           :tokens updated-tokens
           :consecutive-failures (if all-failed? (inc consecutive-failures) 0)
           :last-response-type :tool_calls
           :last-text result-summary
           :obs-count new-obs-count
           :reason-count (inc reason-count)})

        ;; Error response from LLM
        :error
        (let [error-msg (or (:error response) "Unknown LLM error")]
          (log/warn "Agentic loop LLM error" {:drone-id drone-id :turn turn :error error-msg})
          (when session-store
            (session-kg/record-observation!
             session-store turn "llm-error"
             {:success false :error error-msg}))
          {:recur? true
           :turn (inc turn)
           :steps (conj steps (assoc response :turn turn))
           :tool-calls-made tool-calls-made
           :tokens updated-tokens
           :consecutive-failures (inc consecutive-failures)
           :last-response-type :error
           :last-text error-msg
           :obs-count (inc obs-count)
           :reason-count reason-count})

        ;; Unknown response type — terminate with error
        (do
          (log/error "Unknown LLM response type" {:type (:type response) :drone-id drone-id})
          {:recur? false
           :final {:status :error
                   :result (str "Unknown response type: " (:type response))
                   :steps steps
                   :tool_calls_made tool-calls-made
                   :tokens updated-tokens
                   :turns turn
                   :model model-name
                   :kg-stats {:observations obs-count
                              :reasoning reason-count
                              :facts 0}}})))

    (catch Exception e
      (log/error e "Agentic loop exception" {:drone-id drone-id :turn turn})
      (emit! :agentic-loop-error {:drone-id drone-id :turn turn :error (ex-message e)})
      {:recur? false
       :final {:status :error
               :result (str "Exception at turn " turn ": " (ex-message e))
               :steps steps
               :tool_calls_made tool-calls-made
               :tokens total-tokens
               :turns turn
               :model model-name
               :kg-stats {:observations obs-count
                          :reasoning reason-count
                          :facts 0}}})))

;; =============================================================================
;; Core Agentic Loop
;; =============================================================================

(defn run-agentic-loop
  "Run the think-act-observe agentic loop for a drone task.

   Built-in implementation provides:
   - Multi-turn agent loop with compressed context reconstruction
   - Each turn: reconstruct context → call LLM → execute tools → record observations
   - Context compression reduces token usage vs raw message history
   - Termination via should-terminate? heuristics
   - Token tracking across all LLM calls

   When enhanced extension is registered, overrides with enhanced implementation.

   Arguments:
     task-spec - TaskSpec record or map with :task, :files, :cwd
     ctx       - ExecutionContext record or map with :drone-id, :kg-store
     opts      - Optional map with:
       :max-turns    - Max loop iterations (default: 10)
       :backend      - LLMBackend instance (required)
       :tools        - List of tool name strings to allow (nil = all)
       :permissions  - Permission set for tool execution
       :trace?       - Emit progress events (default: false)
       :agent-id     - Agent ID for tracking

   Returns:
     {:status    :completed|:max_steps|:error|:noop
      :result    \"final text response\"
      :steps     [{:type :text|:tool_calls, :turn N, ...}]
      :tool_calls_made  N
      :tokens    {:input N :output N :total N}
      :turns     N
      :model     \"model-name\"
      :kg-stats  {:observations N :reasoning N :facts N}}"
  [task-spec ctx & [opts]]
  (let [drone-id (or (:drone-id ctx) "unknown")]
    ;; Try enhanced extension first
    (if-let [ext-fn (ext/get-extension :al/run)]
      (do
        (log/info "Delegating to enhanced agentic loop" {:drone-id drone-id})
        (ext-fn task-spec ctx opts))

      ;; Built-in: structural agentic loop
      (let [{:keys [max-turns backend tools permissions trace? agent-id]
             :or {max-turns 10
                  permissions #{}
                  trace? false}} opts
            agent-id (or agent-id drone-id)
            session-store (:kg-store ctx)
            task (:task task-spec)
            files (or (:files task-spec) [])
            ;; Try enhanced compression session (nil when extension not registered)
            enhanced-session (when (kg-session/compression-available?)
                               (try
                                 (kg-session/create-session-kg! drone-id task)
                                 (catch Exception e
                                   (log/debug "Enhanced session creation failed (non-fatal)"
                                              {:error (.getMessage e)})
                                   nil)))]

        (log/info "Starting agentic loop"
                  {:drone-id drone-id
                   :max-turns max-turns
                   :has-backend? (some? backend)
                   :has-session-store? (some? session-store)
                   :has-enhanced? (some? enhanced-session)
                   :files (count files)})

        ;; Validate backend
        (when-not backend
          (log/warn "No LLM backend provided, returning noop"
                    {:drone-id drone-id}))

        ;; Return noop if no backend
        (if-not backend
          {:status :noop
           :result "No LLM backend provided"
           :steps []
           :tool_calls_made 0
           :tokens {:input 0 :output 0 :total 0}
           :turns 0
           :model "none"
           :kg-stats nil}

          (let [model-name (proto/model-name backend)
                tool-schemas (registry/get-schemas tools)
                system-prompt (build-system-prompt task-spec)
                emit! (fn [event-type data]
                        (when trace?
                          (channel/emit-event! event-type (assoc data :agent-id agent-id))))]

            ;; Seed session store from global context
            (when session-store
              (try
                (when (hive-mcp.protocols.kg/store-set?)
                  (session-kg/seed-from-global!
                   session-store
                   (hive-mcp.protocols.kg/get-store)
                   {:task task :files files :cwd (:cwd task-spec)}))
                (catch Exception e
                  (log/debug "Session seeding failed (non-fatal)" {:error (.getMessage e)}))))

            (emit! :agentic-loop-started {:drone-id drone-id :max-turns max-turns :model model-name})

            ;; Main loop
            (loop [turn 0
                   steps []
                   tool-calls-made 0
                   total-tokens {:input 0 :output 0 :total 0}
                   consecutive-failures 0
                   last-response-type nil
                   last-text nil
                   obs-count 0
                   reason-count 0]

              ;; Check termination
              (let [term-state {:turn turn
                                :steps steps
                                :consecutive-failures consecutive-failures
                                :last-response-type last-response-type
                                :last-text last-text}
                    term-result (should-terminate? term-state {:max-turns max-turns})]

                (if (:terminate? term-result)
                  ;; === TERMINATE ===
                  (let [status (cond
                                 (>= turn max-turns) :max_steps
                                 (= :text last-response-type) :completed
                                 :else :completed)
                        final-result (or last-text
                                         (str "Loop terminated: " (:reason term-result)))]
                    (log/info "Agentic loop terminated"
                              {:drone-id drone-id
                               :status status
                               :turns turn
                               :reason (:reason term-result)
                               :tool-calls tool-calls-made})
                    (emit! :agentic-loop-completed {:drone-id drone-id
                                                    :status status
                                                    :turns turn
                                                    :tool-calls tool-calls-made})

                    ;; Cleanup: promote enhanced session nodes, then close
                    (when enhanced-session
                      (try
                        (when (and (= status :completed)
                                   (hive-mcp.protocols.kg/store-set?))
                          (let [promo-result (kg-session/promote-to-global!
                                              enhanced-session
                                              (hive-mcp.protocols.kg/get-store))]
                            (log/debug "Enhanced session promotion result" promo-result)))
                        (catch Exception e
                          (log/debug "Enhanced session promotion failed (non-fatal)"
                                     {:error (.getMessage e)})))
                      (try
                        (let [enhanced-stats (kg-session/close-session! enhanced-session)]
                          (log/debug "Enhanced session closed" enhanced-stats))
                        (catch Exception e
                          (log/debug "Enhanced session close failed (non-fatal)"
                                     {:error (.getMessage e)}))))

                    {:status status
                     :result final-result
                     :steps steps
                     :tool_calls_made tool-calls-made
                     :tokens total-tokens
                     :turns turn
                     :model model-name
                     :kg-stats {:observations obs-count
                                :reasoning reason-count
                                :facts 0}})

                  ;; === CONTINUE — one turn of think-act-observe ===
                  (let [;; THINK: Reconstruct compressed context
                        ;; Two-tier: enhanced → built-in → raw task
                        context-prompt (if (and session-store (pos? turn))
                                         (session-kg/reconstruct-context session-store task turn)
                                         task)

                        ;; Build base messages
                        base-messages [{:role "system" :content system-prompt}
                                       {:role "user" :content context-prompt}]

                        ;; Apply enhanced message compression if available
                        ;; When enhanced session is nil, returns base-messages unchanged (noop).
                        messages (if (and enhanced-session (pos? turn))
                                   (kg-session/build-compressed-messages
                                    enhanced-session system-prompt base-messages base-messages)
                                   base-messages)

                        _ (log/debug "Agentic loop turn" {:drone-id drone-id :turn turn
                                                          :context-size (count (str messages))
                                                          :enhanced? (and (some? enhanced-session) (pos? turn))})
                        _ (emit! :agentic-loop-turn {:drone-id drone-id :turn turn :phase :calling-llm})

                        ;; Execute turn (try/catch inside, returns data map)
                        turn-result (execute-turn
                                     backend messages tool-schemas session-store task-spec
                                     {:drone-id drone-id
                                      :agent-id agent-id
                                      :turn turn
                                      :steps steps
                                      :tool-calls-made tool-calls-made
                                      :total-tokens total-tokens
                                      :consecutive-failures consecutive-failures
                                      :obs-count obs-count
                                      :reason-count reason-count
                                      :model-name model-name
                                      :permissions permissions
                                      :emit! emit!})]

                    ;; Compress turn into enhanced session (when available)
                    ;; Extracts meaning from messages for context reconstruction next turn.
                    (when (and enhanced-session (:recur? turn-result))
                      (try
                        (kg-session/compress-turn! enhanced-session messages)
                        (catch Exception e
                          (log/debug "Enhanced session compress-turn! failed (non-fatal)"
                                     {:turn turn :error (.getMessage e)}))))

                    ;; Recur or return based on turn-result
                    (if (:recur? turn-result)
                      (recur (:turn turn-result)
                             (:steps turn-result)
                             (:tool-calls-made turn-result)
                             (:tokens turn-result)
                             (:consecutive-failures turn-result)
                             (:last-response-type turn-result)
                             (:last-text turn-result)
                             (:obs-count turn-result)
                             (:reason-count turn-result))
                      ;; Terminal turn — also clean up enhanced session
                      (do
                        (when enhanced-session
                          (try
                            (kg-session/close-session! enhanced-session)
                            (catch Exception _ nil)))
                        (:final turn-result)))))))))))))
