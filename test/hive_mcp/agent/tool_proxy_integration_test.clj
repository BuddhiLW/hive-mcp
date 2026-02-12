(ns hive-mcp.agent.tool-proxy-integration-test
  "Integration tests for Tool Proxy architecture.

   Tests the full flow:
   1. Model routing identifies proxy-needed models
   2. System prompt includes proxy instructions
   3. Tool intents are parsed from text responses
   4. Tools are executed and results formatted
   5. Conversation continues with tool results"
  (:require [clojure.test :refer :all]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.agent.drone.tool-proxy :as tool-proxy]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Routing Integration Tests
;; =============================================================================

(deftest proxy-models-identified
  (testing "Free-tier models without tool support are identified"
    (is (true? (routing/needs-tool-proxy? "mistralai/devstral-2512:free"))
        "devstral should need proxy")
    (is (true? (routing/needs-tool-proxy? "google/gemma-3-4b-it:free"))
        "gemma should need proxy")
    (is (true? (routing/needs-tool-proxy? "google/gemma-2-9b-it:free"))
        "gemma-2 should need proxy")))

(deftest tool-capable-models-identified
  (testing "Models with native tool support don't need proxy"
    (is (false? (routing/needs-tool-proxy? "openai/gpt-oss-120b:free"))
        "gpt-oss-120b has native tool support")
    (is (false? (routing/needs-tool-proxy? "anthropic/claude-3-haiku"))
        "claude has native tool support")
    (is (false? (routing/needs-tool-proxy? "unknown-model"))
        "unknown models assumed to have tool support")))

(deftest tier2-model-available
  (testing "Tier 2 proxy model is configured"
    (is (string? (routing/get-tool-proxy-model)))
    (is (re-find #"gpt-oss" (routing/get-tool-proxy-model))
        "Tier 2 should use gpt-oss-120b")))

;; =============================================================================
;; Full Pipeline Integration Tests
;; =============================================================================

(def simulated-devstral-response
  "Simulated response from devstral (Tier 1) with tool intent."
  "I need to read the file to understand the code structure.

[TOOL:read_file path=/src/hive_mcp/core.clj]

Then I'll analyze it and propose changes.")

(def simulated-multi-tool-response
  "Response with multiple tool intents."
  "Let me gather information first.

[TOOL:read_file path=/src/foo.clj]

Now checking for patterns:

[TOOL:grep pattern=\"defn \" path=/src]

Finally I'll make the change:

[TOOL:propose_diff file_path=/src/foo.clj old_content=\"(defn old\" new_content=\"(defn new\"]")

(deftest full-pipeline-single-tool
  (testing "Full pipeline: parse -> execute (mock) -> format"
    (let [result (tool-proxy/process-tier1-output-mock simulated-devstral-response)]
      (is (map? result))
      (is (= 1 (count (:intents result)))
          "Should find one tool intent")
      (is (= "read_file" (-> result :intents first :tool))
          "Should identify read_file tool")
      (is (string? (:continuation-prompt result))
          "Should generate continuation prompt")
      (is (re-find #"RESULT" (:continuation-prompt result))
          "Continuation should include result markers"))))

(deftest full-pipeline-multi-tool
  (testing "Full pipeline with multiple tools"
    (let [result (tool-proxy/process-tier1-output-mock simulated-multi-tool-response)]
      (is (= 3 (count (:intents result)))
          "Should find three tool intents")
      (is (= ["read_file" "grep" "propose_diff"]
             (mapv :tool (:intents result)))
          "Should identify all tools in order")
      (is (= 3 (count (:results result)))
          "Should have result for each tool"))))

(deftest pipeline-no-tools
  (testing "Pipeline passes through when no tools"
    (let [normal-response "Here's my analysis. No tools needed."
          result (tool-proxy/process-tier1-output-mock normal-response)]
      (is (nil? (:continuation-prompt result))
          "Should not need continuation")
      (is (= normal-response (:original-output result))
          "Original should pass through"))))

;; =============================================================================
;; Configuration Tests
;; =============================================================================

(deftest tier2-config-accessible
  (testing "Tier 2 configuration can be read"
    (let [config (tool-proxy/get-tier2-config)]
      (is (map? config))
      (is (contains? config :model)))))

(deftest routing-tool-proxy-config
  (testing "Tool proxy configuration is accessible via routing"
    (let [config (routing/get-tool-proxy-config)]
      (is (map? config))
      (is (contains? config :enabled))
      (is (contains? config :model))
      (is (contains? config :max-iterations))))
  (testing "Tool proxy enabled check"
    (is (boolean? (routing/tool-proxy-enabled?)))))

(deftest intent-extraction-preserves-positions
  (testing "Intent extraction includes text positions for reconstruction"
    (let [result (tool-proxy/extract-intents-with-positions simulated-devstral-response)]
      (is (string? (:text-before result))
          "Should capture text before first intent")
      (is (string? (:text-after result))
          "Should capture text after last intent")
      (is (integer? (-> result :intents first :start))
          "Should track intent start position")
      (is (integer? (-> result :intents first :end))
          "Should track intent end position"))))

;; =============================================================================
;; Proxy Loop Tests (mocked end-to-end)
;; =============================================================================

(def ^:private mock-responses
  "Sequence of mock LLM responses for testing proxy loop."
  (atom []))

(defn- reset-mock-responses! [responses]
  (reset! mock-responses (vec responses)))

(defn- mock-reasoning-fn
  "Mock reasoning function that returns pre-configured responses."
  [_messages]
  (if (empty? @mock-responses)
    {:type :text :content "Done with the task."}
    (let [response (first @mock-responses)]
      (swap! mock-responses rest)
      response)))

(defn- mock-executor-fn
  "Mock executor that returns simulated tool results."
  [tool-name params]
  (case tool-name
    "read_file" {:success true
                 :result {:text (str "(ns mock." (last (clojure.string/split (str (:path params)) #"/")) ")\n;; Mock content")}}
    "grep" {:success true
            :result {:text "Found matches:\n/src/a.clj:10: (defn foo [x])"}}
    "cider_status" {:success true
                    :result {:text "CIDER connected to localhost:7888"}}
    {:success true :result {:text (str "Mock result for " tool-name)}}))

(deftest proxy-loop-single-iteration
  (testing "Proxy loop completes in one iteration when no tools"
    (reset-mock-responses!
     [{:type :text :content "Here's my analysis. No tools needed."}])
    (let [result (tool-proxy/proxy-loop mock-reasoning-fn mock-executor-fn "Analyze this")]
      (is (= :completed (:status result)))
      (is (= 0 (:iterations result)))
      (is (empty? (:steps result))))))

(deftest proxy-loop-with-tools
  (testing "Proxy loop processes tool intents and continues"
    (reset-mock-responses!
     [{:type :text :content "Let me read the file first.\n\n[TOOL:read_file path=/src/foo.clj]\n\nI'll analyze it."}
      {:type :text :content "Based on the file content, everything looks good."}])
    (let [result (tool-proxy/proxy-loop mock-reasoning-fn mock-executor-fn "Check the code")]
      (is (= :completed (:status result)))
      (is (= 1 (:iterations result)))
      (is (= 1 (count (:steps result))))
      (is (= "read_file" (-> result :steps first :intents first :tool))))))

(deftest proxy-loop-multi-step
  (testing "Proxy loop handles multiple iterations with different tools"
    (reset-mock-responses!
     [{:type :text :content "First, let me read the file.\n[TOOL:read_file path=/src/a.clj]"}
      {:type :text :content "Now searching for patterns.\n[TOOL:grep pattern=\"defn\" path=/src]"}
      {:type :text :content "Analysis complete. All functions look correct."}])
    (let [result (tool-proxy/proxy-loop mock-reasoning-fn mock-executor-fn "Analyze functions")]
      (is (= :completed (:status result)))
      (is (= 2 (:iterations result)))
      (is (= 2 (count (:steps result))))
      (is (= "read_file" (-> result :steps first :intents first :tool)))
      (is (= "grep" (-> result :steps second :intents first :tool))))))

(deftest proxy-loop-max-iterations
  (testing "Proxy loop stops at max iterations"
    ;; Keep returning tool intents forever
    (reset-mock-responses!
     (repeat 20 {:type :text :content "[TOOL:read_file path=/src/test.clj]"}))
    (let [result (tool-proxy/proxy-loop mock-reasoning-fn mock-executor-fn "Loop forever"
                                        {:max-iterations 3})]
      (is (= :max_iterations (:status result)))
      (is (= 3 (:iterations result))))))

(deftest proxy-loop-error-handling
  (testing "Proxy loop handles LLM errors gracefully"
    (reset-mock-responses!
     [{:type :error :error "API rate limited"}])
    (let [result (tool-proxy/proxy-loop mock-reasoning-fn mock-executor-fn "Try something")]
      (is (= :error (:status result)))
      (is (= "API rate limited" (:result result))))))

(deftest proxy-loop-trace-fn
  (testing "Proxy loop calls trace function for debugging"
    (let [traces (atom [])]
      (reset-mock-responses!
       [{:type :text :content "[TOOL:cider_status]"}
        {:type :text :content "Done."}])
      (tool-proxy/proxy-loop mock-reasoning-fn mock-executor-fn "Check status"
                             {:trace-fn #(swap! traces conj %)})
      (is (some #(= :calling-tier1 (:event %)) @traces)
          "Should trace LLM calls")
      (is (some #(= :tool-intents (:event %)) @traces)
          "Should trace tool parsing")
      (is (some #(= :tool-executed (:event %)) @traces)
          "Should trace tool execution")
      (is (some #(= :completed (:event %)) @traces)
          "Should trace completion"))))

(deftest proxy-loop-summary
  (testing "Proxy run summary is human-readable"
    (let [run-result {:status :completed
                      :result "Task completed successfully."
                      :steps [{:intents [{:tool "read_file"}] :results [{:success true}]}
                              {:intents [{:tool "grep"}] :results [{:success true}]}]
                      :iterations 2}
          summary (tool-proxy/summarize-proxy-run run-result)]
      (is (string? summary))
      (is (re-find #"Status: completed" summary))
      (is (re-find #"Iterations: 2" summary))
      (is (re-find #"Total tool calls: 2" summary)))))

(comment
  ;; Run tests in REPL
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent.tool-proxy-integration-test))
