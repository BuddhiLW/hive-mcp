(ns hive-mcp.agent.drone.backend.sdk-drone-test
  "Tests for SDKDroneBackend — IDroneExecutionBackend via Claude Agent SDK.

   Tests cover:
   - Protocol satisfaction and method contracts
   - resolve-backend multimethod registration
   - Constructor defaults and overrides
   - Internal helper functions (drain, extract, convert)"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [hive-mcp.agent.drone.backend :as backend]
            [hive-mcp.agent.drone.backend.sdk-drone :as sdk-drone]))

;; =============================================================================
;; Section 1: Protocol Satisfaction
;; =============================================================================

(deftest protocol-satisfaction
  (testing "SDKDroneBackend satisfies IDroneExecutionBackend"
    (let [b (sdk-drone/->sdk-drone-backend)]
      (is (satisfies? backend/IDroneExecutionBackend b)
          "SDKDroneBackend should satisfy the protocol"))))

;; =============================================================================
;; Section 2: Protocol Method Contracts
;; =============================================================================

(deftest backend-type-returns-sdk-drone
  (testing "backend-type returns :sdk-drone"
    (let [b (sdk-drone/->sdk-drone-backend)]
      (is (= :sdk-drone (backend/backend-type b))
          "Backend type should be :sdk-drone"))))

(deftest supports-validation-is-true
  (testing "supports-validation? returns true for SDK drone backend"
    (let [b (sdk-drone/->sdk-drone-backend)]
      (is (true? (backend/supports-validation? b))
          "SDK drone backend should support validation"))))

;; =============================================================================
;; Section 3: resolve-backend Registration
;; =============================================================================

(deftest resolve-backend-sdk-drone
  (testing "resolve-backend dispatches on :sdk-drone"
    (let [b (backend/resolve-backend {:backend :sdk-drone :model "test-model"})]
      (is (satisfies? backend/IDroneExecutionBackend b)
          "Resolved backend should satisfy the protocol")
      (is (= :sdk-drone (backend/backend-type b))
          "Resolved backend should have :sdk-drone type"))))

(deftest resolve-backend-passes-config
  (testing "resolve-backend passes model and timeout from context"
    (let [b (backend/resolve-backend {:backend :sdk-drone
                                      :model "custom-model"
                                      :timeout-ms 60000})]
      (is (= "custom-model" (:default-model b))
          "Model should be passed from context")
      (is (= 60000 (:timeout-ms b))
          "Timeout should be passed from context"))))

;; =============================================================================
;; Section 4: Constructor
;; =============================================================================

(deftest constructor-defaults
  (testing "Default constructor uses sensible defaults"
    (let [b (sdk-drone/->sdk-drone-backend)]
      (is (= "claude-sonnet-4-20250514" (:default-model b))
          "Default model should be claude-sonnet-4-20250514")
      (is (= 300000 (:timeout-ms b))
          "Default timeout should be 300000ms (5 min)"))))

(deftest constructor-custom-options
  (testing "Constructor accepts custom model and timeout"
    (let [b (sdk-drone/->sdk-drone-backend {:default-model "custom"
                                             :timeout-ms 10000})]
      (is (= "custom" (:default-model b)))
      (is (= 10000 (:timeout-ms b))))))

;; =============================================================================
;; Section 5: Internal Helper Functions
;; =============================================================================

(deftest drain-output-channel-collects-messages
  (testing "drain-output-channel collects all messages until channel closes"
    (let [ch (async/chan 10)]
      (async/>!! ch {:type :message :data "hello"})
      (async/>!! ch {:type :message :data "world"})
      (async/close! ch)
      (let [result (#'sdk-drone/drain-output-channel ch 5000)]
        (is (= 2 (count (:messages result)))
            "Should collect 2 messages")
        (is (false? (:timed-out? result))
            "Should not be timed out")))))

(deftest drain-output-channel-timeout
  (testing "drain-output-channel returns timed-out on timeout"
    (let [ch (async/chan 10)]
      ;; Don't close channel — will timeout
      (let [result (#'sdk-drone/drain-output-channel ch 100)]
        (is (true? (:timed-out? result))
            "Should report timed out")))))

(deftest extract-result-text-joins-messages
  (testing "extract-result-text joins message data, skipping errors"
    (let [messages [{:type :message :data "line 1"}
                    {:type :error :error "bad"}
                    {:type :message :data "line 2"}]]
      (is (= "line 1\nline 2"
             (#'sdk-drone/extract-result-text messages))
          "Should join only :message types, skip :error"))))

(deftest extract-result-text-handles-empty
  (testing "extract-result-text handles empty messages"
    (is (= "" (#'sdk-drone/extract-result-text []))
        "Empty messages should return empty string")))

(deftest extract-result-text-handles-nil-data
  (testing "extract-result-text skips nil data"
    (let [messages [{:type :message :data nil}
                    {:type :message :data "valid"}]]
      (is (= "valid" (#'sdk-drone/extract-result-text messages))
          "Should skip messages with nil data"))))

;; =============================================================================
;; Section 6: messages->result-map
;; =============================================================================

(deftest result-map-completed
  (testing "Completed execution returns :completed status"
    (let [output {:messages [{:type :message :data "done"}]
                  :timed-out? false}
          result (#'sdk-drone/messages->result-map output "test-model")]
      (is (= :completed (:status result))
          "Status should be :completed")
      (is (= "done" (:result result))
          "Result should contain message data")
      (is (= "test-model" (:model result))
          "Model should be passed through"))))

(deftest result-map-timeout
  (testing "Timed out execution returns :timeout status"
    (let [output {:messages [{:type :message :data "partial"}]
                  :timed-out? true}
          result (#'sdk-drone/messages->result-map output "test-model")]
      (is (= :timeout (:status result))
          "Status should be :timeout")
      (is (str/includes? (:result result) "timed out")
          "Result should mention timeout"))))

(deftest result-map-error
  (testing "Error messages return :failed status"
    (let [output {:messages [{:type :error :error "explosion"}]
                  :timed-out? false}
          result (#'sdk-drone/messages->result-map output "test-model")]
      (is (= :failed (:status result))
          "Status should be :failed")
      (is (str/includes? (:result result) "explosion")
          "Result should contain error message"))))

(deftest result-map-empty-messages
  (testing "Empty messages return :completed with empty result"
    (let [output {:messages [] :timed-out? false}
          result (#'sdk-drone/messages->result-map output "test-model")]
      (is (= :completed (:status result))
          "Empty messages should be :completed")
      (is (= "" (:result result))
          "Result should be empty string"))))

(comment
  ;; Run all SDKDroneBackend tests
  (clojure.test/run-tests 'hive-mcp.agent.drone.backend.sdk-drone-test))
