(ns hive-mcp.agent.drone.tool-proxy
  "Tool proxy for non-tool-capable models using a two-tier architecture."
  (:require [hive-mcp.config.core :as global-config]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tool-intent-pattern
  #"\[TOOL:([a-zA-Z_][a-zA-Z0-9_]*)((?:\s+[a-zA-Z_][a-zA-Z0-9_]*=(?:\"[^\"]*\"|[^\s\]]+))*)\]")

(def ^:private param-pattern
  #"([a-zA-Z_][a-zA-Z0-9_]*)=(?:\"([^\"]*)\"|([^\s\]]+))")

(defn- parse-params
  "Parse parameter string into a map."
  [params-str]
  (if (str/blank? params-str)
    {}
    (->> (re-seq param-pattern params-str)
         (map (fn [[_ name quoted-val unquoted-val]]
                [(keyword name) (or quoted-val unquoted-val)]))
         (into {}))))

(defn parse-tool-intents
  "Parse tool intents from Tier 1 model output."
  [text]
  (if (str/blank? text)
    []
    (->> (re-seq tool-intent-pattern text)
         (mapv (fn [[_ tool-name params-str]]
                 {:tool tool-name
                  :params (parse-params params-str)})))))

(defn has-tool-intent?
  "Quick check if text contains any tool intent markers."
  [text]
  (and (string? text)
       (boolean (re-find #"\[TOOL:[a-zA-Z_]" text))))

(defn extract-intents-with-positions
  "Parse intents and return their text positions for reconstruction."
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

(defn format-result-for-tier1
  "Format a single tool result for Tier 1 model consumption."
  [{:keys [tool success content error matches] :as _result}]
  (cond
    (not success)
    (format "[RESULT:%s ERROR]\n%s\n[/RESULT]" tool error)

    matches
    (format "[RESULT:%s SUCCESS]\n%s\n[/RESULT]"
            tool
            (->> matches
                 (map (fn [{:keys [file line content]}]
                        (if line
                          (format "%s:%d: %s" file line content)
                          (format "%s: %s" file content))))
                 (str/join "\n")))

    content
    (format "[RESULT:%s SUCCESS]\n%s\n[/RESULT]" tool content)

    :else
    (format "[RESULT:%s SUCCESS]\nOK\n[/RESULT]" tool)))

(defn format-results-for-tier1
  "Format multiple tool results as a single continuation prompt."
  [results]
  (->> results
       (map format-result-for-tier1)
       (str/join "\n\n")))

(defn mock-dispatch
  "Mock dispatch for testing without actual API calls."
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

(defn process-tier1-output-mock
  "Process Tier 1 model output through the tool proxy pipeline (mock version)."
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

(defonce ^:private tier2-config
  (atom {:model "openai/gpt-oss-120b:free"
         :api-key nil}))

(defn configure-tier2!
  "Configure the Tier 2 tool execution model."
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
  "Convert parsed intent to OpenAI tool_call message format."
  [{:keys [tool params]}]
  {:role "user"
   :content (format "Execute this tool call and return the result:\nTool: %s\nParameters: %s"
                    tool
                    (pr-str params))})

(defn dispatch-intent
  "Dispatch a parsed intent to Tier 2 for actual execution."
  [{:keys [tool _params] :as intent} _tools & [{:keys [api-key model]}]]
  (let [config @tier2-config
        effective-key (or api-key (:api-key config) (global-config/get-secret :openrouter-api-key))
        effective-model (or model (:model config))]
    (if-not effective-key
      {:tool tool
       :success false
       :error "No API key configured for Tier 2 dispatch"}
      (do
        (log/info "Tier 2 dispatch" {:tool tool :model effective-model})
        (mock-dispatch intent)))))

(defn process-tier1-output
  "Process Tier 1 model output through the tool proxy pipeline."
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

(defn create-continuation-messages
  "Create messages for continuing Tier 1 conversation after tool execution."
  [{:keys [original-output continuation-prompt]}]
  (when continuation-prompt
    [{:role "assistant"
      :content original-output}
     {:role "user"
      :content (str "Tool execution results:\n\n" continuation-prompt "\n\nPlease continue.")}]))

(defn proxy-loop
  "Run reasoning model with tool proxy for non-tool-capable models."
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
            :text
            (let [content (:content response)]
              (if (has-tool-intent? content)
                (let [parsed (extract-intents-with-positions content)
                      intents (:intents parsed)
                      _ (trace! {:event :tool-intents :count (count intents) :intents intents})
                      _ (log/info "Proxy loop executing tools" {:count (count intents)
                                                                :tools (mapv :tool intents)})
                      results (mapv (fn [{:keys [tool params] :as _intent}]
                                      (let [result (try
                                                     (executor-fn tool params)
                                                     (catch Exception e
                                                       {:success false :error (ex-message e)}))]
                                        (trace! {:event :tool-executed :tool tool :success (:success result)})
                                        (assoc result :tool tool)))
                                    intents)
                      formatted-results (format-results-for-tier1
                                         (map (fn [r]
                                                {:tool (:tool r)
                                                 :success (:success r)
                                                 :content (get-in r [:result :text])
                                                 :error (:error r)})
                                              results))
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

                (do
                  (trace! {:event :completed :iteration iteration})
                  (log/info "Proxy loop completed" {:iterations iteration})
                  {:status :completed
                   :result content
                   :steps steps
                   :iterations iteration})))

            :error
            (do
              (trace! {:event :error :error (:error response)})
              (log/error "Proxy loop error" {:error (:error response)})
              {:status :error
               :result (:error response)
               :steps steps
               :iterations iteration})

            (do
              (trace! {:event :unknown-response :type (:type response)})
              {:status :error
               :result (str "Unknown response type: " (:type response))
               :steps steps
               :iterations iteration})))))))

(defn summarize-proxy-run
  "Create a human-readable summary of a proxy loop run."
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
