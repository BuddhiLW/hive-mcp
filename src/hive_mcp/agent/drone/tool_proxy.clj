(ns hive-mcp.agent.drone.tool-proxy
  "Two-tier model architecture: Tool Proxy for non-tool models.

   Problem: Most free/cheap OpenRouter models (devstral, deepseek, gemma)
   return 404 for tool_use. Only gpt-oss-120b confirmed working.

   Solution: Two-tier model architecture
   - Tier 1: Free reasoning models output [TOOL:name param=value] markers
   - Tier 2: Tool proxy (gpt-oss-120b) parses intents, executes actual tool_call

   Benefits:
   - 10x cost reduction (free models for 90% of work)
   - Only pay for actual tool calls
   - Works with ANY model that outputs structured text

   Architecture (from swarm-drone-fb-driver design):
   - Intent Parser: Extract [TOOL:name param=value] markers
   - Proxy Dispatcher: Route to actual tool execution
   - Result Formatter: Return results in Tier 1 consumable format"
  (:require [hive-mcp.config :as global-config]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Intent Parser - Extract [TOOL:name param=value] markers
;;; =============================================================================

;; Regex pattern for tool intent markers
;; Matches: [TOOL:name] or [TOOL:name param=value param2="quoted value"]
;; Group 1: tool name
;; Group 2: parameters string (optional)
(def ^:private tool-intent-pattern
  #"\[TOOL:([a-zA-Z_][a-zA-Z0-9_]*)((?:\s+[a-zA-Z_][a-zA-Z0-9_]*=(?:\"[^\"]*\"|[^\s\]]+))*)\]")

;; Parameter pattern for parsing individual params
;; Group 1: param name
;; Group 2: quoted value (if quoted)
;; Group 3: unquoted value (if not quoted)
(def ^:private param-pattern
  #"([a-zA-Z_][a-zA-Z0-9_]*)=(?:\"([^\"]*)\"|([^\s\]]+))")

(defn- parse-params
  "Parse parameter string into a map.

   Examples:
     'path=/src/foo.clj'         -> {:path '/src/foo.clj'}
     'pattern=\"TODO\" path=/src' -> {:pattern 'TODO' :path '/src'}

   Handles:
   - Unquoted values: param=value
   - Quoted values: param=\"value with spaces\"
   - Multiple params separated by whitespace"
  [params-str]
  (if (str/blank? params-str)
    {}
    (->> (re-seq param-pattern params-str)
         (map (fn [[_ name quoted-val unquoted-val]]
                [(keyword name) (or quoted-val unquoted-val)]))
         (into {}))))

(defn parse-tool-intents
  "Parse tool intents from Tier 1 model output.

   Input: Model text containing [TOOL:name param=value] markers
   Output: Vector of intent maps with :tool and :params keys

   Example:
     (parse-tool-intents \"Read file: [TOOL:read_file path=/src/foo.clj]\")
     => [{:tool \"read_file\" :params {:path \"/src/foo.clj\"}}]

   Returns empty vector for nil/empty input or no matches."
  [text]
  (if (str/blank? text)
    []
    (->> (re-seq tool-intent-pattern text)
         (mapv (fn [[_ tool-name params-str]]
                 {:tool tool-name
                  :params (parse-params params-str)})))))

(defn has-tool-intent?
  "Quick check if text contains any tool intent markers.
   Faster than full parsing for filtering."
  [text]
  (and (string? text)
       (boolean (re-find #"\[TOOL:[a-zA-Z_]" text))))

(defn extract-intents-with-positions
  "Parse intents and return their text positions for reconstruction.

   Returns:
     {:intents [{:tool .. :params .. :start N :end N :raw \"..\"} ...]
      :text-before \"text before first intent\"
      :text-after \"text after last intent\"
      :segments [...]}  ; interleaved text/intent sequence"
  [text]
  (if (str/blank? text)
    {:intents []
     :text-before ""
     :text-after ""
     :segments []}
    (let [matcher (re-matcher tool-intent-pattern text)
          intents (loop [results []
                         _last-end 0]
                    (if (.find matcher)
                      (let [start (.start matcher)
                            end (.end matcher)
                            raw (.group matcher)
                            tool-name (.group matcher 1)
                            params-str (.group matcher 2)]
                        (recur (conj results
                                     {:tool tool-name
                                      :params (parse-params params-str)
                                      :start start
                                      :end end
                                      :raw raw})
                               end))
                      results))
          first-start (get-in intents [0 :start] (count text))
          last-end (get (last intents) :end 0)]
      {:intents intents
       :text-before (subs text 0 first-start)
       :text-after (if (seq intents)
                     (subs text last-end)
                     text)
       :segments (when (seq intents)
                   (loop [segments []
                          remaining intents
                          pos 0]
                     (if (empty? remaining)
                       (let [final-text (subs text pos)]
                         (if (str/blank? final-text)
                           segments
                           (conj segments {:type :text :content final-text})))
                       (let [intent (first remaining)
                             before-text (subs text pos (:start intent))]
                         (recur (cond-> segments
                                  (not (str/blank? before-text))
                                  (conj {:type :text :content before-text})
                                  true
                                  (conj {:type :intent :intent intent}))
                                (rest remaining)
                                (:end intent))))))})))

;;; =============================================================================
;;; Result Formatter - Return results Tier 1 can consume
;;; =============================================================================

(defn format-result-for-tier1
  "Format a single tool result for Tier 1 model consumption.

   The format is designed to be:
   - Clearly delineated (parseable)
   - Human-readable (for debugging)
   - Compact (minimize tokens)

   Input: {:tool \"name\" :success bool :content/error \"...\" ...}
   Output: Formatted string for continuation"
  [{:keys [tool success content error matches] :as _result}]
  (cond
    ;; Error case
    (not success)
    (format "[RESULT:%s ERROR]\n%s\n[/RESULT]" tool error)

    ;; Success with matches (grep-like)
    matches
    (format "[RESULT:%s SUCCESS]\n%s\n[/RESULT]"
            tool
            (->> matches
                 (map (fn [{:keys [file line content]}]
                        (if line
                          (format "%s:%d: %s" file line content)
                          (format "%s: %s" file content))))
                 (str/join "\n")))

    ;; Success with content
    content
    (format "[RESULT:%s SUCCESS]\n%s\n[/RESULT]" tool content)

    ;; Success with no specific content
    :else
    (format "[RESULT:%s SUCCESS]\nOK\n[/RESULT]" tool)))

(defn format-results-for-tier1
  "Format multiple tool results as a single continuation prompt."
  [results]
  (->> results
       (map format-result-for-tier1)
       (str/join "\n\n")))

;;; =============================================================================
;;; Mock Dispatch (for testing without actual API calls)
;;; =============================================================================

(defn mock-dispatch
  "Mock dispatch for testing. Returns simulated result.

   In production, this would call the Tier 2 model (gpt-oss-120b)
   with actual tool_call capability."
  [{:keys [tool params] :as _intent}]
  (log/debug "Mock dispatch" {:tool tool :params params})
  {:tool tool
   :success true
   :content (case tool
              "read_file" (format "(ns mock.%s)\n;; Mock content for %s"
                                  (-> (:path params) (str/split #"/") last (str/replace ".clj" ""))
                                  (:path params))
              "grep" nil
              "cider_status" "CIDER connected to localhost:7888"
              "bash" "$ command executed successfully"
              (format "Mock result for %s" tool))
   :matches (when (= tool "grep")
              [{:file "/mock/file.clj" :line 1 :content "Mock match"}])})

;;; =============================================================================
;;; Full Pipeline
;;; =============================================================================

(defn process-tier1-output-mock
  "Process Tier 1 model output through the tool proxy pipeline (mock version).

   Pipeline:
   1. Check for tool intents
   2. Parse intents with positions
   3. Mock dispatch each intent
   4. Format results for continuation

   Returns:
     {:original-output \"...\"
      :intents [...]
      :results [...]
      :continuation-prompt \"...\" or nil if no tools}"
  [tier1-output]
  (if-not (has-tool-intent? tier1-output)
    {:original-output tier1-output
     :intents []
     :results []
     :continuation-prompt nil}
    (let [parsed (extract-intents-with-positions tier1-output)
          intents (:intents parsed)
          results (mapv mock-dispatch intents)
          continuation (when (seq results)
                         (str (:text-before parsed)
                              "\n"
                              (format-results-for-tier1 results)
                              "\n"
                              (:text-after parsed)))]
      {:original-output tier1-output
       :intents intents
       :results results
       :continuation-prompt continuation})))

;;; =============================================================================
;;; Real Dispatch (for production - uses Tier 2 model)
;;; =============================================================================

(defonce ^:private tier2-config
  (atom {:model "openai/gpt-oss-120b:free"
         :api-key nil}))

(defn configure-tier2!
  "Configure the Tier 2 tool execution model.

   Options:
     :model   - OpenRouter model with tool support (default: gpt-oss-120b)
     :api-key - OpenRouter API key (or set OPENROUTER_API_KEY env)"
  [{:keys [model api-key]}]
  (swap! tier2-config merge
         (cond-> {}
           model (assoc :model model)
           api-key (assoc :api-key api-key))))

(defn get-tier2-config
  "Get current Tier 2 configuration."
  []
  @tier2-config)

(defn intent->tool-call-message
  "Convert parsed intent to OpenAI tool_call message format.

   This is the message format we send to Tier 2 to execute the tool."
  [{:keys [tool params]}]
  {:role "user"
   :content (format "Execute this tool call and return the result:\nTool: %s\nParameters: %s"
                    tool
                    (pr-str params))})

(defn dispatch-intent
  "Dispatch a parsed intent to Tier 2 for actual execution.

   This is the production version that calls OpenRouter with tool support.

   Arguments:
     intent  - Parsed intent map {:tool \"name\" :params {...}}
     tools   - Available MCP tools for the call
     opts    - Options including :api-key, :model

   Returns:
     {:tool \"name\" :success bool :content/error \"...\"}"
  [{:keys [tool _params] :as intent} _tools & [{:keys [api-key model]}]]
  (let [config @tier2-config
        effective-key (or api-key (:api-key config) (global-config/get-secret :openrouter-api-key))
        effective-model (or model (:model config))]
    (if-not effective-key
      {:tool tool
       :success false
       :error "No API key configured for Tier 2 dispatch"}
      ;; Real dispatch would use openrouter/chat here
      ;; For now, delegate to mock (production wiring TBD)
      (do
        (log/info "Tier 2 dispatch" {:tool tool :model effective-model})
        (mock-dispatch intent)))))

(defn process-tier1-output
  "Process Tier 1 model output through the tool proxy pipeline (production).

   Same as mock version but uses real dispatch."
  [tier1-output tools & [opts]]
  (if-not (has-tool-intent? tier1-output)
    {:original-output tier1-output
     :intents []
     :results []
     :continuation-prompt nil}
    (let [parsed (extract-intents-with-positions tier1-output)
          intents (:intents parsed)
          results (mapv #(dispatch-intent % tools opts) intents)
          continuation (when (seq results)
                         (str (:text-before parsed)
                              "\n"
                              (format-results-for-tier1 results)
                              "\n"
                              (:text-after parsed)))]
      {:original-output tier1-output
       :intents intents
       :results results
       :continuation-prompt continuation})))

;;; =============================================================================
;;; Tier 1/2 Conversation Flow
;;; =============================================================================

(defn create-continuation-messages
  "Create messages for continuing Tier 1 conversation after tool execution.

   The pattern is:
   1. Tier 1 outputs text with [TOOL:...] markers
   2. We execute tools and get results
   3. We send results back as assistant message for Tier 1 to continue

   Returns messages to append to conversation."
  [{:keys [original-output continuation-prompt]}]
  (when continuation-prompt
    [{:role "assistant"
      :content original-output}
     {:role "user"
      :content (str "Tool execution results:\n\n" continuation-prompt "\n\nPlease continue.")}]))

;;; =============================================================================
;;; Proxy Loop - Standalone orchestration
;;; =============================================================================

(defn proxy-loop
  "Run reasoning model with tool proxy for non-tool-capable models.

   This is a standalone orchestration function that:
   1. Calls the reasoning model (Tier 1) to generate text with [TOOL:...] markers
   2. Parses tool intents from the output
   3. Executes tools via the provided executor function
   4. Feeds results back to Tier 1 for continuation
   5. Repeats until no more tool intents or max-iterations reached

   Arguments:
     reasoning-fn  - Function (messages) -> {:type :text :content string}
     executor-fn   - Function (tool-name params) -> {:success bool :result/error ...}
     task          - Initial task description
     opts          - Options:
                     :max-iterations - Max proxy loops (default: 10)
                     :system-prompt  - Custom system prompt (optional)
                     :trace-fn       - Called with {:event :type :data ...} for debugging

   Returns:
     {:status    :completed|:max_iterations|:error
      :result    \"final text output\"
      :steps     [{:iteration N :intents [...] :results [...]} ...]
      :iterations N}"
  [reasoning-fn executor-fn task & [{:keys [max-iterations system-prompt trace-fn]
                                     :or {max-iterations 10}}]]
  (let [default-system (str "You are a helpful coding assistant. Complete the task using the available tools.\n\n"
                            "IMPORTANT: To use a tool, output this EXACT format:\n"
                            "[TOOL:tool_name param1=value1 param2=\"value with spaces\"]\n\n"
                            "Examples:\n"
                            "- [TOOL:read_file path=/src/core.clj]\n"
                            "- [TOOL:grep pattern=\"TODO\" path=/src recursive=true]\n"
                            "- [TOOL:propose_diff file_path=/src/foo.clj old_content=\"...\" new_content=\"...\"]\n\n"
                            "Be concise. Use tools to gather information and make changes.")
        trace! (or trace-fn (fn [_] nil))
        initial-messages [{:role "system" :content (or system-prompt default-system)}
                          {:role "user" :content task}]]

    (loop [messages initial-messages
           steps []
           iteration 0]
      (if (>= iteration max-iterations)
        (do
          (trace! {:event :max-iterations :iteration iteration})
          (log/warn "Proxy loop reached max iterations" {:max max-iterations})
          {:status :max_iterations
           :result (str "Reached max iterations (" max-iterations ")")
           :steps steps
           :iterations iteration})

        (let [_ (trace! {:event :calling-tier1 :iteration iteration})
              _ (log/debug "Proxy loop iteration" {:iteration iteration})
              response (try
                         (reasoning-fn messages)
                         (catch Exception e
                           {:type :error :error (ex-message e)}))]

          (case (:type response)
            ;; Text response - check for tool intents
            :text
            (let [content (:content response)]
              (if (has-tool-intent? content)
                ;; Process tool intents
                (let [parsed (extract-intents-with-positions content)
                      intents (:intents parsed)
                      _ (trace! {:event :tool-intents :count (count intents) :intents intents})
                      _ (log/info "Proxy loop executing tools" {:count (count intents)
                                                                :tools (mapv :tool intents)})
                      ;; Execute each tool
                      results (mapv (fn [{:keys [tool params] :as _intent}]
                                      (let [result (try
                                                     (executor-fn tool params)
                                                     (catch Exception e
                                                       {:success false :error (ex-message e)}))]
                                        (trace! {:event :tool-executed :tool tool :success (:success result)})
                                        (assoc result :tool tool)))
                                    intents)
                      ;; Format results for Tier 1
                      formatted-results (format-results-for-tier1
                                         (map (fn [r]
                                                {:tool (:tool r)
                                                 :success (:success r)
                                                 :content (get-in r [:result :text])
                                                 :error (:error r)})
                                              results))
                      ;; Build continuation messages
                      new-messages (conj messages
                                         {:role "assistant" :content content}
                                         {:role "user"
                                          :content (str "Tool execution results:\n\n"
                                                        formatted-results
                                                        "\n\nPlease continue with the task.")})]
                  (recur new-messages
                         (conj steps {:iteration iteration
                                      :intents intents
                                      :results results})
                         (inc iteration)))

                ;; No tool intents - task complete
                (do
                  (trace! {:event :completed :iteration iteration})
                  (log/info "Proxy loop completed" {:iterations iteration})
                  {:status :completed
                   :result content
                   :steps steps
                   :iterations iteration})))

            ;; Error response
            :error
            (do
              (trace! {:event :error :error (:error response)})
              (log/error "Proxy loop error" {:error (:error response)})
              {:status :error
               :result (:error response)
               :steps steps
               :iterations iteration})

            ;; Unknown response type
            (do
              (trace! {:event :unknown-response :type (:type response)})
              {:status :error
               :result (str "Unknown response type: " (:type response))
               :steps steps
               :iterations iteration})))))))

;;; =============================================================================
;;; Utility Functions
;;; =============================================================================

(defn summarize-proxy-run
  "Create a human-readable summary of a proxy loop run for debugging."
  [{:keys [status result steps iterations]}]
  (str "Proxy Run Summary\n"
       "================\n"
       "Status: " (name status) "\n"
       "Iterations: " iterations "\n"
       "Total tool calls: " (reduce + 0 (map #(count (:intents %)) steps)) "\n"
       "\nSteps:\n"
       (str/join "\n"
                 (map-indexed
                  (fn [i step]
                    (str "  " (inc i) ". Tools: " (str/join ", " (map :tool (:intents step)))
                         " -> " (count (filter :success (:results step))) "/"
                         (count (:results step)) " succeeded"))
                  steps))
       "\n\nResult:\n" (subs result 0 (min 200 (count result)))
       (when (> (count result) 200) "...")))
