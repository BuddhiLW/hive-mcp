(ns hive-mcp.agent.loop
  "Agent tool-use loop orchestration.
   
   Application layer use case that orchestrates:
   - LLM calls via backend protocol
   - Tool execution via executor
   - Conversation history management
   - Progress tracking via channel events"
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.executor :as executor]
            [hive-mcp.channel :as channel]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;;; ============================================================
;;; Agent Loop
;;; ============================================================

(defn- format-assistant-tool-calls
  "Format tool calls for assistant message in OpenAI format.
   Arguments must be JSON string, not map."
  [calls]
  (mapv (fn [{:keys [id name arguments]}]
          {:id id
           :type "function"
           :function {:name name
                      :arguments (if (string? arguments)
                                   arguments
                                   (json/write-str arguments))}})
        calls))

(defn run-loop
  "Run the agent tool-use loop until completion or max steps.
   
   Options:
     :backend     - LLMBackend instance (required)
     :task        - Task description (required)
     :tools       - List of tool names to allow (nil = all)
     :permissions - Set of permissions (:auto-approve skips human checks)
     :max-steps   - Maximum tool-use iterations (default: 15)
     :agent-id    - Agent identifier for tracking
     :trace?      - If true, emit progress events via channel
   
   Returns:
     {:status :completed|:max_steps|:error
      :result \"final text response\"
      :steps [{:type :text|:tool_calls ...} ...]
      :tool_calls_made N}"
  [{:keys [backend task tools permissions max-steps agent-id trace?]
    :or {max-steps 15
         permissions #{}
         trace? false
         agent-id (str "agent-" (System/currentTimeMillis))}}]
  (let [tool-schemas (registry/get-schemas tools)
        initial-messages [{:role "system"
                           :content "You are a helpful coding assistant. Use tools to complete the task. Be concise."}
                          {:role "user"
                           :content task}]
        emit! (fn [event-type data]
                (when trace?
                  (channel/emit-event! event-type (assoc data :agent-id agent-id))))]
    (loop [messages initial-messages
           steps []
           tool-calls-made 0
           step-count 0]
      (if (>= step-count max-steps)
        (do
          (emit! :agent-max-steps {:step step-count :max max-steps})
          {:status :max_steps
           :result (str "Reached max steps (" max-steps ")")
           :steps steps
           :tool_calls_made tool-calls-made})

        (let [_ (log/debug "Agent step" step-count "- calling" (proto/model-name backend))
              _ (emit! :agent-step {:step step-count :phase :calling-llm})
              response (proto/chat backend messages tool-schemas)]

          (case (:type response)
            ;; Text response = task complete
            :text
            (do
              (emit! :agent-completed {:step step-count :tool-calls-made tool-calls-made})
              {:status :completed
               :result (:content response)
               :steps (conj steps response)
               :tool_calls_made tool-calls-made})

            ;; Tool calls = execute and continue
            :tool_calls
            (let [calls (:calls response)
                  tool-names (mapv :name calls)
                  _ (log/info "Agent executing" (count calls) "tool calls:" tool-names)
                  _ (emit! :agent-step {:step step-count :phase :executing-tools :tools tool-names})
                  tool-results (executor/execute-tool-calls agent-id calls permissions)
                  assistant-msg {:role "assistant"
                                 :tool_calls (format-assistant-tool-calls calls)}
                  new-messages (vec (concat messages [assistant-msg] tool-results))]
              (recur new-messages
                     (conj steps response)
                     (+ tool-calls-made (count calls))
                     (inc step-count)))

            ;; Unknown response type
            (do
              (emit! :agent-error {:step step-count :error (str "Unknown response: " (:type response))})
              {:status :error
               :result (str "Unknown response type: " (:type response))
               :steps steps
               :tool_calls_made tool-calls-made})))))))
