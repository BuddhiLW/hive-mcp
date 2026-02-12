(ns hive-mcp.agent.ling.agent-sdk-strategy-test
  "Tests for AgentSDKStrategy — ILingStrategy implementation for Claude Agent SDK.

   Tests cover:
   1. Strategy satisfies ILingStrategy protocol
   2. Graceful degradation when SDK unavailable
   3. Spawn/status/kill lifecycle (via SDK session registry)
   4. Factory function

   Note: These tests do NOT require Python or libpython-clj.
   SDK availability is tested via the cached status atom.
   Integration tests requiring actual SDK would be tagged :integration."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.strategy :refer [ILingStrategy
                                                   strategy-spawn!
                                                   strategy-dispatch!
                                                   strategy-status
                                                   strategy-kill!]]
            [hive-mcp.agent.ling.agent-sdk-strategy :as sut]
            [hive-mcp.agent.headless-sdk :as sdk]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn with-clean-sdk-state [f]
  "Reset SDK session registry and availability cache between tests."
  (sdk/kill-all-sdk!)
  (sdk/reset-availability!)
  (f)
  (sdk/kill-all-sdk!))

(use-fixtures :each with-clean-sdk-state)

(defn gen-ling-id []
  (str "test-sdk-strategy-" (java.util.UUID/randomUUID)))

;; =============================================================================
;; Protocol Satisfaction Tests
;; =============================================================================

(deftest strategy-satisfies-protocol-test
  (testing "AgentSDKStrategy satisfies ILingStrategy protocol"
    (let [strategy (sut/->agent-sdk-strategy)]
      (is (satisfies? ILingStrategy strategy)))))

(deftest factory-creates-strategy-test
  (testing "->agent-sdk-strategy creates an AgentSDKStrategy record"
    (let [strategy (sut/->agent-sdk-strategy)]
      (is (instance? hive_mcp.agent.ling.agent_sdk_strategy.AgentSDKStrategy strategy)))))

(deftest strategy-is-record-test
  (testing "AgentSDKStrategy is a proper Clojure record"
    (let [strategy (sut/->agent-sdk-strategy)]
      (is (record? strategy))
      (is (map? strategy)))))

;; =============================================================================
;; Graceful Degradation Tests
;; =============================================================================

(deftest spawn-without-sdk-throws-with-info-test
  (testing "Spawn throws ExceptionInfo when SDK unavailable"
    ;; In test environments, SDK is typically not available
    (when-not (sdk/available?)
      (let [strategy (sut/->agent-sdk-strategy)
            ling-id (gen-ling-id)]
        (is (thrown-with-msg?
              clojure.lang.ExceptionInfo
              #"Claude Agent SDK not available"
              (strategy-spawn! strategy
                {:id ling-id :cwd "/tmp" :presets []}
                {})))))))

(deftest spawn-error-includes-sdk-status-test
  (testing "Spawn error data includes SDK status and hint"
    (when-not (sdk/available?)
      (let [strategy (sut/->agent-sdk-strategy)
            ling-id (gen-ling-id)]
        (try
          (strategy-spawn! strategy
            {:id ling-id :cwd "/tmp" :presets []}
            {})
          (is false "Should have thrown")
          (catch clojure.lang.ExceptionInfo e
            (let [data (ex-data e)]
              (is (= ling-id (:ling-id data)))
              (is (keyword? (:sdk-status data)))
              (is (= :agent-sdk (:spawn-mode data)))
              (is (string? (:hint data))))))))))

(deftest sdk-available-predicate-test
  (testing "sdk-available? returns boolean"
    (is (boolean? (sut/sdk-available?)))))

;; =============================================================================
;; Status Tests (No SDK Required)
;; =============================================================================

(deftest status-without-session-returns-nil-test
  (testing "Status returns nil when no SDK session and no ds-status"
    (let [strategy (sut/->agent-sdk-strategy)
          result (strategy-status strategy {:id "nonexistent"} nil)]
      (is (nil? result)))))

(deftest status-without-session-preserves-ds-status-test
  (testing "Status preserves ds-status and adds sdk-alive? false"
    (let [strategy (sut/->agent-sdk-strategy)
          ds-status {:slave/id "nonexistent" :slave/status :idle}
          result (strategy-status strategy {:id "nonexistent"} ds-status)]
      (is (= "nonexistent" (:slave/id result)))
      (is (= :idle (:slave/status result)))
      (is (false? (:sdk-alive? result))))))

;; =============================================================================
;; Kill Tests (No SDK Required)
;; =============================================================================

(deftest kill-nonexistent-returns-gracefully-test
  (testing "Kill nonexistent session returns killed with reason"
    (let [strategy (sut/->agent-sdk-strategy)
          result (strategy-kill! strategy {:id "nonexistent"})]
      (is (true? (:killed? result)))
      (is (= "nonexistent" (:id result)))
      (is (= :session-not-found (:reason result))))))

;; =============================================================================
;; Lifecycle Tests (SDK availability dependent)
;; =============================================================================

(deftest full-lifecycle-when-sdk-available-test
  (testing "Full spawn/status/kill lifecycle when SDK is available"
    (when (sdk/available?)
      (let [strategy (sut/->agent-sdk-strategy)
            ling-id (gen-ling-id)
            ;; Spawn
            spawn-result (strategy-spawn! strategy
                           {:id ling-id :cwd "/tmp" :presets ["ling"]}
                           {})
            ;; Status
            status-result (strategy-status strategy {:id ling-id} nil)]
        ;; Verify spawn
        (is (= ling-id spawn-result))
        ;; Verify status
        (is (true? (:sdk-alive? status-result)))
        (is (= :agent-sdk (:ling/spawn-mode status-result)))
        (is (= ling-id (:slave/id status-result)))
        (is (keyword? (:sdk-phase status-result)))
        ;; Kill
        (let [kill-result (strategy-kill! strategy {:id ling-id})]
          (is (true? (:killed? kill-result)))
          (is (= ling-id (:id kill-result)))
          (is (= :agent-sdk (:backend kill-result))))
        ;; Verify killed
        (let [post-kill-status (strategy-status strategy {:id ling-id} nil)]
          (is (nil? post-kill-status)))))))

(deftest spawn-with-task-dispatches-test
  (testing "Spawn with :task option dispatches immediately"
    (when (sdk/available?)
      (let [strategy (sut/->agent-sdk-strategy)
            ling-id (gen-ling-id)
            _ (strategy-spawn! strategy
                {:id ling-id :cwd "/tmp" :presets []}
                {:task "List files in /tmp"})
            status (strategy-status strategy {:id ling-id} nil)]
        ;; Should be in some phase (not :idle if task was dispatched)
        (is (some? status))
        (is (true? (:sdk-alive? status)))
        ;; Cleanup
        (strategy-kill! strategy {:id ling-id})))))

;; =============================================================================
;; Dispatch Tests
;; =============================================================================

(deftest dispatch-without-session-throws-test
  (testing "Dispatch to nonexistent session throws"
    (let [strategy (sut/->agent-sdk-strategy)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (strategy-dispatch! strategy
                     {:id "nonexistent"}
                     {:task "do something"}))))))

(comment
  ;; Run tests
  ;; (clojure.test/run-tests 'hive-mcp.agent.ling.agent-sdk-strategy-test)
  )
