(ns hive-mcp.agent.drone.backend.hive-agent-test
  "Tests for HiveAgentBackend — IDroneExecutionBackend implementation
   wrapping the hive-agent bridge.

   Tests cover:
   - Protocol satisfaction
   - Backend type and validation support
   - Result adaptation from bridge format to protocol format
   - resolve-backend multimethod registration
   - Graceful degradation when hive-agent unavailable"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.agent.drone.backend :as backend]
            [hive-mcp.agent.drone.backend.hive-agent :as ha-backend]))

;; =============================================================================
;; Section 1: Protocol Satisfaction
;; =============================================================================

(deftest hive-agent-backend-satisfies-protocol
  (testing "HiveAgentBackend satisfies IDroneExecutionBackend"
    (let [b (ha-backend/make-hive-agent-backend)]
      (is (satisfies? backend/IDroneExecutionBackend b)
          "HiveAgentBackend must satisfy the protocol"))))

;; =============================================================================
;; Section 2: Backend Type & Validation
;; =============================================================================

(deftest backend-type-returns-hive-agent
  (testing "backend-type returns :hive-agent"
    (let [b (ha-backend/make-hive-agent-backend)]
      (is (= :hive-agent (backend/backend-type b))))))

(deftest supports-validation-returns-true
  (testing "supports-validation? returns true"
    (let [b (ha-backend/make-hive-agent-backend)]
      (is (true? (backend/supports-validation? b))))))

;; =============================================================================
;; Section 3: Result Adaptation
;; =============================================================================

(deftest adapt-bridge-result-completed
  (testing "Adapts completed bridge result to protocol format"
    (let [bridge-result {:status :completed
                         :result "Task done successfully"
                         :tokens {:input 100 :output 50 :total 150}
                         :model "deepseek/deepseek-chat"
                         :tool_calls_made 5
                         :hive-agent-metadata {:turns 3 :source :hive-agent}}
          adapted (#'ha-backend/adapt-bridge-result bridge-result)]
      (is (= :completed (:status adapted)))
      (is (= "Task done successfully" (:result adapted)))
      (is (= {:input-tokens 100 :output-tokens 50} (:tokens adapted)))
      (is (= "deepseek/deepseek-chat" (:model adapted)))
      (is (= 5 (:steps adapted)))
      (is (= :hive-agent (get-in adapted [:metadata :backend]))))))

(deftest adapt-bridge-result-error
  (testing "Adapts error bridge result — status mapped to :failed"
    (let [bridge-result {:status :error
                         :result "hive-agent error: connection refused"
                         :tokens {:input 0 :output 0 :total 0}
                         :model "unknown"
                         :tool_calls_made 0
                         :hive-agent-metadata {:turns 0 :source :hive-agent}}
          adapted (#'ha-backend/adapt-bridge-result bridge-result)]
      (is (= :failed (:status adapted))
          "Error status from bridge should map to :failed")
      (is (str/includes? (:result adapted) "connection refused")))))

(deftest adapt-bridge-result-missing-fields
  (testing "Adapts result with missing optional fields gracefully"
    (let [bridge-result {:status :completed
                         :result nil
                         :tokens nil
                         :model nil}
          adapted (#'ha-backend/adapt-bridge-result bridge-result)]
      (is (= :completed (:status adapted)))
      (is (= "" (:result adapted))
          "Nil result should default to empty string")
      (is (= {:input-tokens 0 :output-tokens 0} (:tokens adapted))
          "Nil tokens should default to zeros")
      (is (= "unknown" (:model adapted))
          "Nil model should default to unknown"))))

;; =============================================================================
;; Section 4: resolve-backend Registration
;; =============================================================================

(deftest resolve-backend-hive-agent-registered
  (testing "resolve-backend dispatches :hive-agent to HiveAgentBackend"
    (let [resolved (backend/resolve-backend {:backend :hive-agent})]
      (is (satisfies? backend/IDroneExecutionBackend resolved))
      (is (= :hive-agent (backend/backend-type resolved))))))

;; =============================================================================
;; Section 5: execute-drone Graceful Degradation
;; =============================================================================

(deftest execute-drone-unavailable-hive-agent
  (testing "execute-drone returns :failed when hive-agent not on classpath"
    (let [b (ha-backend/make-hive-agent-backend)
          ;; hive-agent is not on classpath in test env,
          ;; so bridge/run-agent-via-bridge returns nil
          result (backend/execute-drone b {:task "test task"
                                           :model "test-model"})]
      (is (= :failed (:status result))
          "Should return :failed when hive-agent unavailable")
      (is (string? (:result result)))
      (is (contains? result :tokens))
      (is (contains? result :model)))))

(comment
  ;; Run all hive-agent backend tests
  (clojure.test/run-tests 'hive-mcp.agent.drone.backend.hive-agent-test))
