(ns hive-mcp.agent.loop
  "Agent tool-use loop orchestration.

   Application layer use case that orchestrates:
   - LLM calls via backend protocol
   - Tool execution via executor
   - Conversation history management
   - Progress tracking via channel events
   - Tool proxy for non-tool-capable models (two-tier architecture)"
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.executor :as executor]
            [hive-mcp.agent.drone.tool-proxy :as tool-proxy]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.channel :as channel]
            [clojure.data.json :as json]
            [clojure.string]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
      :tool_calls_made N
      :tokens {:input N :output N :total N}  ;; CLARITY-T: Accumulated token usage
      :model \"model-name\"}"
  [{:keys [backend task tools permissions max-steps agent-id trace?]
    :or {max-steps 15
         permissions #{}
         trace? false
         agent-id (str "agent-" (System/currentTimeMillis))}}]
  (let [tool-schemas (registry/get-schemas tools)
        model-name (proto/model-name backend)
        needs-proxy? (routing/needs-tool-proxy? model-name)
        ;; Build system prompt - add proxy instructions if model needs tool proxy
        system-prompt (if needs-proxy?
                        (str "You are a helpful coding assistant. Complete the task using the available tools.\n\n"
                             "IMPORTANT: To use a tool, output this EXACT format:\n"
                             "[TOOL:tool_name param1=value1 param2=\"value with spaces\"]\n\n"
                             "Examples:\n"
                             "- [TOOL:read_file path=/src/core.clj]\n"
                             "- [TOOL:grep pattern=\"TODO\" path=/src recursive=true]\n"
                             "- [TOOL:propose_diff file_path=/src/foo.clj old_content=\"...\" new_content=\"...\"]\n\n"
                             "Available tools: " (clojure.string/join ", " (map :name tool-schemas)) "\n\n"
                             "Be concise. Use tools to gather information and make changes.")
                        (str "You are a coding drone. You MUST use the propose_diff tool for ALL file changes. "
                             "NEVER output code as plain text or markdown code blocks. "
                             "Every file modification MUST go through the propose_diff tool with file_path, old_content, and new_content parameters. "
                             "If you output code as text instead of calling propose_diff, your changes will be LOST and the task will FAIL. "
                             "Use read_file to examine files, then propose_diff to make changes. Be concise and focused."))
        initial-messages [{:role "system" :content system-prompt}
                          {:role "user" :content task}]
        emit! (fn [event-type data]
                (when trace?
                  (channel/emit-event! event-type (assoc data :agent-id agent-id))))
        _ (when needs-proxy?
            (log/info "Tool proxy mode enabled for" model-name))]
    ;; CLARITY-T: Track accumulated token usage across all LLM calls
    (loop [messages initial-messages
           steps []
           tool-calls-made 0
           step-count 0
           total-tokens {:input 0 :output 0 :total 0}]
      (if (>= step-count max-steps)
        (do
          (emit! :agent-max-steps {:step step-count :max max-steps})
          {:status :max_steps
           :result (str "Reached max steps (" max-steps ")")
           :steps steps
           :tool_calls_made tool-calls-made
           :tokens total-tokens
           :model model-name})

        (let [_ (log/debug "Agent step" step-count "- calling" model-name)
              _ (emit! :agent-step {:step step-count :phase :calling-llm})
              response (proto/chat backend messages tool-schemas)
              ;; CLARITY-T: Accumulate token usage from this call
              usage (:usage response)
              updated-tokens (if usage
                               {:input (+ (:input total-tokens) (or (:input usage) 0))
                                :output (+ (:output total-tokens) (or (:output usage) 0))
                                :total (+ (:total total-tokens) (or (:total usage) 0))}
                               total-tokens)]

          (case (:type response)
            ;; Text response = task complete OR contains tool intents (proxy models)
            :text
            (let [content (:content response)
                  needs-proxy? (routing/needs-tool-proxy? model-name)
                  has-intents? (and needs-proxy? (tool-proxy/has-tool-intent? content))]
              (if has-intents?
                ;; TOOL PROXY: Extract intents, execute via executor, continue loop
                (let [parsed (tool-proxy/extract-intents-with-positions content)
                      intents (:intents parsed)
                      _ (log/info "Tool proxy: processing" (count intents) "intents from" model-name)
                      _ (emit! :agent-step {:step step-count
                                            :phase :proxy-tool-execution
                                            :intent-count (count intents)})
                      ;; Execute each tool intent via executor
                      tool-results (mapv
                                    (fn [{:keys [tool params] :as _intent}]
                                      (let [call-id (str "proxy-" (java.util.UUID/randomUUID))
                                            result (executor/execute-tool tool params)]
                                        {:id call-id
                                         :tool tool
                                         :result result
                                         :formatted (tool-proxy/format-result-for-tier1
                                                     (assoc result
                                                            :tool tool
                                                            :success (:success result)
                                                            :content (get-in result [:result :text])
                                                            :error (:error result)))}))
                                    intents)
                      ;; Build continuation message with results
                      results-formatted (clojure.string/join "\n\n"
                                                             (map :formatted tool-results))
                      continuation-msg {:role "user"
                                        :content (str "Tool execution results:\n\n"
                                                      results-formatted
                                                      "\n\nPlease continue with the task.")}
                      ;; Add original response + results to messages
                      new-messages (conj messages
                                         {:role "assistant" :content content}
                                         continuation-msg)]
                  (recur new-messages
                         (conj steps (assoc response :proxy-intents intents))
                         (+ tool-calls-made (count intents))
                         (inc step-count)
                         updated-tokens))
                ;; No tool intents - task complete
                (do
                  (emit! :agent-completed {:step step-count :tool-calls-made tool-calls-made})
                  {:status :completed
                   :result content
                   :steps (conj steps response)
                   :tool_calls_made tool-calls-made
                   :tokens updated-tokens
                   :model model-name})))

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
                     (inc step-count)
                     updated-tokens))

            ;; Error response from backend (e.g., empty content validation)
            ;; CLARITY-Y: Yield safe failure - propagate error instead of silent success
            :error
            (do
              (emit! :agent-error {:step step-count :error (:error response)})
              (log/warn "Agent received error response" {:error (:error response)})
              {:status :error
               :result (:error response)
               :steps steps
               :tool_calls_made tool-calls-made
               :tokens updated-tokens
               :model model-name})

            ;; Unknown response type
            (do
              (emit! :agent-error {:step step-count :error (str "Unknown response: " (:type response))})
              {:status :error
               :result (str "Unknown response type: " (:type response))
               :steps steps
               :tool_calls_made tool-calls-made
               :tokens updated-tokens
               :model model-name})))))))
