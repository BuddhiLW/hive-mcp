(ns hive-mcp.workflows.sub-fsms.tool-execution
  "Tool-execution sub-FSM — reusable FSM for sandboxed tool execution.

   States: validate-permissions -> execute -> capture-result -> ::fsm/end

   Validates tool calls against a sandbox allowlist, executes permitted tools
   with timeout protection, and captures results as observations for the
   parent drone-loop FSM.

   ## Sub-FSM Composition

   Called from parent FSM handler via:
     (fsm/run compiled-tool-execution resources {:data input-map})

   The parent handler merges the sub-FSM result into its own data.

   ## Data Flow

   Input (from parent):
     {:tool-call   {:id str :name str :arguments map}  ;; tool call from LLM
      :drone-id    str    ;; drone identifier for audit
      :turn        int    ;; current turn number
      :files       [str]} ;; allowed files (optional, for sandbox context)

   Output (to parent):
     {:observation       str      ;; formatted result for LLM message history
      :tool-result       any      ;; raw tool execution result
      :denied?           boolean  ;; true if tool was denied by sandbox
      :permission-result map      ;; {:allowed? bool :reason str}
      :execution-time-ms long     ;; execution duration in ms
      :error             str}     ;; error message (or nil)

   ## Resources (injected at run time)

     :sandbox           — Sandbox spec from drone.sandbox/create-sandbox (required)
     :execute-tool-fn   — (fn [tool-name args] result) tool executor (required)
     :record-observation-fn — (fn [observation-map] nil) KG recorder (optional)
     :timeout-ms        — Max execution time per tool (default 30000)
     :audit-fn          — (fn [tool-name args allowed?] nil) audit logger (optional)

  (:require [hive.events.fsm :as fsm]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const default-timeout-ms
  "Default maximum execution time per tool call (30 seconds)."
  30000)

;; =============================================================================
;; Handlers (Pure: resources, data -> data')
;; =============================================================================

(defn handle-validate-permissions
  "Validate that the tool call is permitted by the sandbox.

   Checks:
   1. Tool call has required fields (:name)
   2. Sandbox allows the tool+args combination

   Input:  {:tool-call {:name str :arguments map}}
   Output: assoc :permission-result {:allowed? bool :reason str}
                 :denied? boolean

   Resources used: :sandbox (required), :audit-fn (optional)"
  [resources data]
  (let [{:keys [tool-call drone-id]} data
        tool-name (:name tool-call)
        tool-args (or (:arguments tool-call) {})
        sandbox   (:sandbox resources)
        audit-fn  (:audit-fn resources)]

    (cond
      ;; No tool call provided
      (nil? tool-call)
      (assoc data
             :permission-result {:allowed? false :reason "No tool call provided"}
             :denied? true
             :error "No tool call provided")

      ;; No tool name
      (nil? tool-name)
      (assoc data
             :permission-result {:allowed? false :reason "Tool call missing :name"}
             :denied? true
             :error "Tool call missing :name")

      ;; No sandbox — permissive mode (log warning)
      (nil? sandbox)
      (do
        (log/warn "tool-execution: no sandbox provided, allowing all tools"
                  {:tool tool-name :drone-id drone-id})
        (assoc data
               :permission-result {:allowed? true :reason "No sandbox (permissive mode)"}
               :denied? false))

      ;; Check sandbox permissions
      :else
      (let [result (if (and (map? sandbox) (:sandbox-allows-fn resources))
                     ;; Use injected sandbox-allows-fn for testability
                     ((:sandbox-allows-fn resources) sandbox tool-name tool-args)
                     ;; Default: check sandbox map directly
                     ;; Sandbox spec has :blocked-tools, :allowed-files, etc.
                     (let [blocked? (contains? (:blocked-tools sandbox) tool-name)]
                       (if blocked?
                         {:allowed? false
                          :reason (str "Tool '" tool-name "' is blocked for drones")}
                         {:allowed? true})))
            allowed? (:allowed? result)]

        ;; Audit log
        (when audit-fn
          (try
            (audit-fn tool-name tool-args allowed?)
            (catch Exception e
              (log/debug "tool-execution: audit-fn error:" (.getMessage e)))))

        (log/debug "tool-execution: permission check"
                   {:tool tool-name
                    :drone-id drone-id
                    :allowed? allowed?
                    :reason (:reason result)})

        (assoc data
               :permission-result result
               :denied? (not allowed?))))))

(defn handle-execute
  "Execute the permitted tool call with timeout protection.

   Only runs if permission was granted (not denied).
   On denial, creates a denial observation and skips execution.

   Resources used: :execute-tool-fn (required), :timeout-ms (optional)"
  [resources data]
  (if (:denied? data)
    ;; Tool denied — create denial result without executing
    (let [tool-name (get-in data [:tool-call :name])
          reason    (get-in data [:permission-result :reason])]
      (log/debug "tool-execution: tool denied, skipping execution"
                 {:tool tool-name :reason reason})
      (assoc data
             :tool-result {:type "text"
                           :text (str "SANDBOX VIOLATION: " reason)
                           :isError true}
             :execution-time-ms 0))

    ;; Tool permitted — execute with timeout
    (let [{:keys [tool-call drone-id]} data
          tool-name    (:name tool-call)
          tool-args    (or (:arguments tool-call) {})
          execute-fn   (:execute-tool-fn resources)
          timeout-ms   (or (:timeout-ms resources) default-timeout-ms)]

      (if-not execute-fn
        ;; No executor — graceful degradation
        (do
          (log/warn "tool-execution: no execute-tool-fn provided"
                    {:tool tool-name :drone-id drone-id})
          (assoc data
                 :tool-result nil
                 :error "No tool executor provided"
                 :execution-time-ms 0))

        ;; Execute with timing
        (let [start-ms (System/currentTimeMillis)]
          (try
            (let [result-future (future (execute-fn tool-name tool-args))
                  result        (deref result-future timeout-ms ::timeout)]
              (let [elapsed (- (System/currentTimeMillis) start-ms)]
                (if (= result ::timeout)
                  ;; Timeout — cancel and report
                  (do
                    (future-cancel result-future)
                    (log/warn "tool-execution: timeout"
                              {:tool tool-name
                               :drone-id drone-id
                               :timeout-ms timeout-ms})
                    (assoc data
                           :tool-result {:type "text"
                                         :text (str "Tool execution timed out after "
                                                    timeout-ms "ms")
                                         :isError true}
                           :error (str "Timeout after " timeout-ms "ms")
                           :execution-time-ms elapsed))

                  ;; Success
                  (do
                    (log/debug "tool-execution: completed"
                               {:tool tool-name
                                :drone-id drone-id
                                :elapsed-ms elapsed})
                    (assoc data
                           :tool-result result
                           :execution-time-ms elapsed)))))

            (catch Exception e
              (let [elapsed (- (System/currentTimeMillis) start-ms)]
                (log/error e "tool-execution: execution error"
                           {:tool tool-name :drone-id drone-id})
                (assoc data
                       :tool-result {:type "text"
                                     :text (str "Tool execution error: " (ex-message e))
                                     :isError true}
                       :error (str "Execution error: " (ex-message e))
                       :execution-time-ms elapsed)))))))))

(defn handle-capture-result
  "Capture the tool result as a formatted observation.

   Formats the tool result into a string suitable for inclusion
   in LLM message history as a tool-result/observation message.

   Also optionally records the observation to the KG via
   :record-observation-fn resource.

   Resources used: :record-observation-fn (optional)"
  [resources data]
  (let [{:keys [tool-call tool-result denied? execution-time-ms
                drone-id turn]} data
        tool-name  (:name tool-call)
        tool-id    (or (:id tool-call) (str "call-" tool-name "-" (or turn 0)))
        record-fn  (:record-observation-fn resources)

        ;; Format observation string from tool result
        observation (cond
                      ;; Tool result is a map with :text
                      (and (map? tool-result) (:text tool-result))
                      (:text tool-result)

                      ;; Tool result is a map with :content (list of content blocks)
                      (and (map? tool-result) (sequential? (:content tool-result)))
                      (->> (:content tool-result)
                           (map #(or (:text %) (str %)))
                           (clojure.string/join "\n"))

                      ;; Tool result is a string
                      (string? tool-result)
                      tool-result

                      ;; Tool result is nil
                      (nil? tool-result)
                      "(no result)"

                      ;; Fallback: pr-str
                      :else
                      (pr-str tool-result))

        ;; Build observation map for KG recording
        observation-map {:tool-call-id    tool-id
                         :tool-name       tool-name
                         :drone-id        drone-id
                         :turn            turn
                         :denied?         denied?
                         :execution-time-ms execution-time-ms
                         :observation     observation
                         :is-error?       (boolean
                                           (or (:error data)
                                               (and (map? tool-result)
                                                    (:isError tool-result))))}]

    ;; Record observation to KG if recorder available
    (when record-fn
      (try
        (record-fn observation-map)
        (catch Exception e
          (log/debug "tool-execution: record-observation-fn error:" (.getMessage e)))))

    (assoc data
           :observation     observation
           :tool-call-id    tool-id
           :observation-map observation-map)))

;; =============================================================================
;; Dispatch Predicates
;; =============================================================================

(defn permission-checked?
  "Data has permission-result (validation was attempted)."
  [data]
  (contains? data :permission-result))

(defn execution-attempted?
  "Data has tool-result or denial (execution phase completed)."
  [data]
  (or (contains? data :tool-result)
      (:denied? data)))

(defn observation-captured?
  "Data has observation string (capture phase completed)."
  [data]
  (contains? data :observation))

;; =============================================================================
;; FSM Spec
;; =============================================================================

(def tool-execution-spec
  "Tool-execution sub-FSM specification.

   States: validate-permissions -> execute -> capture-result -> ::fsm/end
   Error path: any state -> ::fsm/error on unexpected failure"
  {:fsm {::fsm/start
         {:handler    handle-validate-permissions
          :dispatches [[::execute permission-checked?]
                       [::fsm/error (constantly true)]]}

         ::execute
         {:handler    handle-execute
          :dispatches [[::capture-result execution-attempted?]
                       [::fsm/error (constantly true)]]}

         ::capture-result
         {:handler    handle-capture-result
          :dispatches [[::fsm/end observation-captured?]
                       [::fsm/error (constantly true)]]}

         ::fsm/end
         {:handler (fn [_r data] data)}

         ::fsm/error
         {:handler (fn [_r data]
                     (log/warn "tool-execution FSM error:" (:error data))
                     data)}}

   :opts {:max-trace 10}})

;; Compile once, reuse across all invocations
(def compiled
  "Pre-compiled tool-execution sub-FSM. Use with:
     (fsm/run compiled resources {:data input-map})"
  (delay (fsm/compile tool-execution-spec)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn- clean-internal-fields
  "Remove FSM-internal fields from the result data.
   ::fsm/end handler does NOT execute (engine design), so cleanup
   happens here in the public API wrapper."
  [data]
  (dissoc data :permission-result :observation-map))

(defn run-tool-execution
  "Execute the tool-execution sub-FSM.

   Convenience wrapper over fsm/run for direct invocation.

   Args:
     data      - Input map (see ns docstring for shape)
     resources - Map with :sandbox, :execute-tool-fn, etc.

   Returns: Final data map with :observation, :tool-result, :denied?, etc.
            (Extracts :data from the FSM state map returned by fsm/run.)"
  [data resources]
  (let [fsm-result (fsm/run @compiled resources {:data data})]
    (-> (:data fsm-result)
        clean-internal-fields)))

(defn run-tool-execution-batch
  "Execute multiple tool calls sequentially.

   Runs the tool-execution sub-FSM once per tool call, accumulating
   results into a vector of observations.

   Args:
     tool-calls - Vector of {:id :name :arguments} maps
     resources  - Shared resources map
     base-data  - Base data map (drone-id, turn, etc.)

   Returns: Vector of result maps, one per tool call."
  [tool-calls resources base-data]
  (mapv (fn [tc]
          (run-tool-execution (assoc base-data :tool-call tc) resources))
        tool-calls))

(defn run-sub-fsm
  "Helper for embedding tool-execution in a parent FSM handler.

   Extracts tool-call info from parent data, runs the sub-FSM,
   and merges :observation, :tool-result, :denied? back into parent.

   Usage in parent handler:
     (defn handle-execute-tools [resources parent-data]
       (tool-execution/run-sub-fsm resources parent-data))

   Or with explicit tool-call:
     (tool-execution/run-sub-fsm resources parent-data
       {:tool-call {:name \"read_file\" :arguments {:path \"foo.clj\"}}})"
  ([resources parent-data]
   (run-sub-fsm resources parent-data nil))
  ([resources parent-data overrides]
   (let [input (merge (select-keys parent-data [:tool-call :drone-id :turn :files])
                      overrides)
         sub-resources (or (:tool-execution-resources resources)
                           (select-keys resources [:sandbox :execute-tool-fn
                                                   :record-observation-fn
                                                   :timeout-ms :audit-fn
                                                   :sandbox-allows-fn]))
         result (run-tool-execution input sub-resources)]
     (merge parent-data
            (select-keys result [:observation :tool-result :denied?
                                 :tool-call-id :execution-time-ms :error])))))
