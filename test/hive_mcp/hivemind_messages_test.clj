(ns hive-mcp.hivemind-messages-test
  "Pinning tests for hivemind_messages tool.

   Tests verify:
   - Agent lookup works when agent exists
   - Error handling when agent doesn't exist
   - Bug regression: agent_id parameter is correctly received"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.hivemind :as hivemind]))

;;; Test fixtures

(defn reset-hivemind-state
  "Fixture to ensure clean agent-registry state between tests."
  [f]
  ;; Reset agent-registry
  (reset! @(resolve 'hive-mcp.hivemind/agent-registry) {})
  (f)
  (reset! @(resolve 'hive-mcp.hivemind/agent-registry) {}))

(use-fixtures :each reset-hivemind-state)

;;; Helper to get the handler

(defn get-hivemind-messages-handler
  "Extract the hivemind_messages handler from tools list."
  []
  (let [tool (first (filter #(= "hivemind_messages" (:name %)) hivemind/tools))]
    (:handler tool)))

;;; Basic functionality tests

(deftest hivemind-messages-returns-messages-for-registered-agent-test
  (testing "hivemind_messages returns messages for agent that has shouted"
    ;; Register agent by having it shout
    (hivemind/shout! "test-agent-123" :progress {:task "testing" :message "Hello"})

    (let [handler (get-hivemind-messages-handler)
          result (handler {:agent_id "test-agent-123"})
          parsed (json/read-str (:text result) :key-fn keyword)]
      (is (= "test-agent-123" (:agent_id parsed)))
      (is (seq (:messages parsed)) "Should have messages")
      (is (= :progress (keyword (:event-type (first (:messages parsed)))))))))

(deftest hivemind-messages-returns-error-for-missing-agent-test
  (testing "hivemind_messages returns error when agent doesn't exist"
    (let [handler (get-hivemind-messages-handler)
          result (handler {:agent_id "nonexistent-agent"})
          parsed (json/read-str (:text result) :key-fn keyword)]
      (is (= "Agent not found: nonexistent-agent" (:error parsed)))
      (is (vector? (:available-agents parsed))))))

;;; Bug regression tests

(deftest hivemind-messages-uses-provided-agent-id-in-error-test
  (testing "Bug: error message should use the queried agent_id, not 'coordinator'"
    ;; Register a different agent
    (hivemind/shout! "actual-agent" :progress {:task "work" :message "busy"})

    (let [handler (get-hivemind-messages-handler)
          ;; Query for a different (nonexistent) agent
          result (handler {:agent_id "queried-agent"})
          parsed (json/read-str (:text result) :key-fn keyword)]
      ;; The error should mention "queried-agent", NOT "coordinator"
      (is (= "Agent not found: queried-agent" (:error parsed))
          "Error should reference the queried agent_id, not some default")
      ;; available-agents should show the actual registered agent
      (is (some #(= "actual-agent" %) (:available-agents parsed))))))

(deftest hivemind-messages-handles-string-keys-test
  (testing "Bug: handler should work when MCP passes string keys instead of keyword keys"
    ;; Register agent
    (hivemind/shout! "string-key-agent" :completed {:task "done" :message "finished"})

    (let [handler (get-hivemind-messages-handler)
          ;; Simulate MCP passing string keys (as might happen from JSON parsing)
          result (handler {"agent_id" "string-key-agent"})
          parsed (json/read-str (:text result) :key-fn keyword)]
      ;; Should find the agent, not return "Agent not found"
      (is (= "string-key-agent" (:agent_id parsed))
          "Should work with string keys from JSON")
      (is (seq (:messages parsed)) "Should have messages"))))

(deftest hivemind-messages-nil-agent-id-error-test
  (testing "When agent_id is nil, error should show nil not 'coordinator'"
    (let [handler (get-hivemind-messages-handler)
          result (handler {}) ; No agent_id provided
          parsed (json/read-str (:text result) :key-fn keyword)]
      ;; Error should NOT say "coordinator" - it should say nil or the actual param
      (is (string? (:error parsed)))
      (is (not (.contains (:error parsed) "coordinator"))
          "Error should not inject 'coordinator' as default agent_id"))))
