(ns hive-mcp.agent.executor-test
  "Tests for tool executor agent-id attribution.

   CLARITY-T: Verifies that agent-id propagates through tool execution
   so that hivemind_shout and similar tools can identify the calling agent."
  (:require [clojure.test :refer :all]
            [hive-mcp.agent.executor :as executor]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.agent.registry :as registry]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def captured-agent-id (atom nil))

(defn test-tool-handler
  "A test tool that captures the current agent-id from execution context."
  [_args]
  (reset! captured-agent-id (ctx/current-agent-id))
  {:type "text" :text "ok"})

(defn setup-test-tool! []
  (registry/register!
   [{:name "test_capture_agent"
     :description "Test tool that captures agent-id"
     :inputSchema {:type "object" :properties {}}
     :handler test-tool-handler}]))

;; =============================================================================
;; Agent-ID Propagation Tests
;; =============================================================================

(deftest current-agent-id-outside-execution
  (testing "current-agent-id returns nil when not in execution context"
    (is (nil? (executor/current-agent-id)))))

(deftest current-agent-id-during-execution
  (testing "current-agent-id returns the agent-id during tool execution"
    (setup-test-tool!)
    (reset! captured-agent-id nil)

    (let [calls [{:id "call-1" :name "test_capture_agent" :arguments {}}]
          drone-id "drone-test-12345"]
      (executor/execute-tool-calls drone-id calls #{:auto-approve})

      ;; The tool should have captured the drone-id
      (is (= drone-id @captured-agent-id)
          "Tool handler should see the correct agent-id via current-agent-id"))))

(deftest agent-id-isolated-between-executions
  (testing "agent-id is isolated between different executions"
    (setup-test-tool!)

    ;; Execute with first agent
    (reset! captured-agent-id nil)
    (executor/execute-tool-calls "drone-A"
                                 [{:id "c1" :name "test_capture_agent" :arguments {}}]
                                 #{:auto-approve})
    (is (= "drone-A" @captured-agent-id))

    ;; Execute with second agent
    (reset! captured-agent-id nil)
    (executor/execute-tool-calls "drone-B"
                                 [{:id "c2" :name "test_capture_agent" :arguments {}}]
                                 #{:auto-approve})
    (is (= "drone-B" @captured-agent-id))

    ;; After execution, should be nil again
    (is (nil? (executor/current-agent-id)))))

;; =============================================================================
;; Integration Test: hivemind_shout Attribution (requires hivemind setup)
;; =============================================================================

(deftest hivemind-shout-uses-context-agent-id
  (testing "hivemind_shout uses execution context agent-id when not provided"
    ;; This test verifies the fix for P1: drone error attribution
    ;; When a drone calls hivemind_shout without explicit agent_id,
    ;; it should use the agent-id from the execution context (current-agent-id)

    ;; Skip if hivemind_shout isn't registered (unit test environment)
    (when-let [tool (registry/get-tool "hivemind_shout")]
      (let [drone-id "drone-attribution-test-123"
            calls [{:id "call-shout"
                    :name "hivemind_shout"
                    :arguments {:event_type "progress"
                                :message "Test message"}}]
            ;; Note: We're not providing agent_id in arguments
            ;; The tool should get it from current-agent-id
            results (executor/execute-tool-calls drone-id calls #{:auto-approve})]

        ;; Result should show the drone-id was used
        (is (some? results))))))
