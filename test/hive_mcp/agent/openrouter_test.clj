(ns hive-mcp.agent.openrouter-test
  "Tests for OpenRouter backend response validation.

   CLARITY-Y: Validates that empty/malformed responses yield safe failures
   rather than silent success with 0-byte file writes."
  (:require [clojure.test :refer :all]
            [hive-mcp.agent.openrouter :as openrouter]
            [hive-mcp.agent.protocol :as proto]))

;; =============================================================================
;; Test Data - Simulated OpenRouter Responses
;; =============================================================================

(def valid-text-response
  "Standard successful text response."
  {:choices [{:message {:role "assistant"
                        :content "Here is the implementation..."}}]})

(def empty-content-response
  "Response with empty string content - should be treated as error."
  {:choices [{:message {:role "assistant"
                        :content ""}}]})

(def nil-content-response
  "Response with nil content - should be treated as error."
  {:choices [{:message {:role "assistant"
                        :content nil}}]})

(def whitespace-only-response
  "Response with only whitespace - should be treated as error."
  {:choices [{:message {:role "assistant"
                        :content "   \n\t   "}}]})

(def valid-tool-call-response
  "Response with tool calls - should be processed normally."
  {:choices [{:message {:role "assistant"
                        :tool_calls [{:id "call_123"
                                      :type "function"
                                      :function {:name "read_file"
                                                 :arguments "{\"path\": \"/foo/bar.clj\"}"}}]}}]})

;; =============================================================================
;; Response Parsing Tests
;; =============================================================================

(deftest parse-valid-text-response
  (testing "Valid text response returns :text type with content"
    (let [choice (get-in valid-text-response [:choices 0 :message])
          result (openrouter/parse-response choice)]
      (is (= :text (:type result)))
      (is (= "Here is the implementation..." (:content result))))))

(deftest parse-empty-content-response
  (testing "Empty string content returns :error type"
    (let [choice (get-in empty-content-response [:choices 0 :message])
          result (openrouter/parse-response choice)]
      (is (= :error (:type result))
          "Empty content should return error type")
      (is (string? (:error result))
          "Error should contain descriptive message"))))

(deftest parse-nil-content-response
  (testing "Nil content returns :error type"
    (let [choice (get-in nil-content-response [:choices 0 :message])
          result (openrouter/parse-response choice)]
      (is (= :error (:type result))
          "Nil content should return error type")
      (is (string? (:error result))
          "Error should contain descriptive message"))))

(deftest parse-whitespace-only-response
  (testing "Whitespace-only content returns :error type"
    (let [choice (get-in whitespace-only-response [:choices 0 :message])
          result (openrouter/parse-response choice)]
      (is (= :error (:type result))
          "Whitespace-only content should return error type")
      (is (string? (:error result))
          "Error should contain descriptive message"))))

(deftest parse-valid-tool-calls
  (testing "Tool calls are processed normally"
    (let [choice (get-in valid-tool-call-response [:choices 0 :message])
          result (openrouter/parse-response choice)]
      (is (= :tool_calls (:type result)))
      (is (= 1 (count (:calls result))))
      (is (= "read_file" (-> result :calls first :name))))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest parse-response-missing-message
  (testing "Missing message in choice returns error"
    (let [result (openrouter/parse-response nil)]
      (is (= :error (:type result))
          "Nil message should return error"))))

(deftest parse-response-empty-tool-calls-with-empty-content
  (testing "Empty tool_calls array with empty content returns error"
    (let [choice {:role "assistant" :tool_calls [] :content ""}
          result (openrouter/parse-response choice)]
      (is (= :error (:type result))
          "Empty tool_calls with empty content should return error"))))

;; =============================================================================
;; Metrics Tests
;; =============================================================================

(deftest metrics-tracking-exists
  (testing "Metrics functions are available"
    (is (fn? openrouter/reset-metrics!))
    (is (fn? openrouter/get-metrics))))

(deftest metrics-reset-works
  (testing "reset-metrics! resets all counters"
    (openrouter/reset-metrics!)
    (let [m (openrouter/get-metrics)]
      (is (= 0 (:request-count m)))
      (is (= 0 (:success-count m)))
      (is (= 0 (:error-count m))))))

(comment
  ;; Run tests in REPL
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent.openrouter-test)

  ;; Run specific test
  (parse-empty-content-response))
