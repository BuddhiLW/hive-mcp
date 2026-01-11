(ns hive-mcp.tools.core-test
  "Tests for MCP tool response core utilities.
   
   Covers:
   - Basic response formatting (mcp-success, mcp-error, mcp-json)
   - Instruction queue management (push, drain)
   - Piggyback instructions on responses"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.tools.core :as core]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn clear-instruction-queues!
  "Clear all instruction queues for test isolation."
  []
  (reset! core/instruction-queues {}))

(use-fixtures :each (fn [f]
                      (clear-instruction-queues!)
                      (f)
                      (clear-instruction-queues!)))

;; =============================================================================
;; Test: Basic Response Formatting
;; =============================================================================

(deftest test-mcp-success-basic
  (testing "mcp-success creates correct response structure"
    (let [response (core/mcp-success "Hello")]
      (is (= "text" (:type response)))
      (is (= "Hello" (:text response)))
      (is (nil? (:isError response))))))

(deftest test-mcp-success-non-string
  (testing "mcp-success converts non-strings with pr-str"
    (let [response (core/mcp-success {:foo "bar"})]
      (is (= "text" (:type response)))
      (is (string? (:text response))))))

(deftest test-mcp-error-basic
  (testing "mcp-error creates error response structure"
    (let [response (core/mcp-error "Something failed")]
      (is (= "text" (:type response)))
      (is (= "Something failed" (:text response)))
      (is (true? (:isError response))))))

(deftest test-mcp-json-basic
  (testing "mcp-json creates JSON response"
    (let [response (core/mcp-json {:status "ok" :count 5})]
      (is (= "text" (:type response)))
      (let [parsed (json/read-str (:text response) :key-fn keyword)]
        (is (= "ok" (:status parsed)))
        (is (= 5 (:count parsed)))))))

;; =============================================================================
;; Test: Instruction Queue Management
;; =============================================================================

(deftest test-push-instruction-single
  (testing "push-instruction! adds instruction to agent queue"
    (core/push-instruction! "ling-1" {:action "pause" :reason "rate limit"})
    (is (= 1 (count (get @core/instruction-queues "ling-1"))))))

(deftest test-push-instruction-multiple
  (testing "push-instruction! accumulates multiple instructions"
    (core/push-instruction! "ling-1" {:action "pause"})
    (core/push-instruction! "ling-1" {:action "resume"})
    (core/push-instruction! "ling-1" {:action "abort"})
    (is (= 3 (count (get @core/instruction-queues "ling-1"))))))

(deftest test-push-instruction-multiple-agents
  (testing "Instructions are isolated per agent"
    (core/push-instruction! "ling-1" {:action "pause"})
    (core/push-instruction! "ling-2" {:action "continue"})
    (core/push-instruction! "ling-1" {:action "resume"})
    (is (= 2 (count (get @core/instruction-queues "ling-1"))))
    (is (= 1 (count (get @core/instruction-queues "ling-2"))))))

(deftest test-drain-instructions-returns-all
  (testing "drain-instructions! returns all queued instructions"
    (core/push-instruction! "ling-1" {:action "A"})
    (core/push-instruction! "ling-1" {:action "B"})
    (let [instructions (core/drain-instructions! "ling-1")]
      (is (= 2 (count instructions)))
      (is (= {:action "A"} (first instructions)))
      (is (= {:action "B"} (second instructions))))))

(deftest test-drain-instructions-clears-queue
  (testing "drain-instructions! clears the queue after draining"
    (core/push-instruction! "ling-1" {:action "test"})
    (core/drain-instructions! "ling-1")
    (is (empty? (get @core/instruction-queues "ling-1" [])))))

(deftest test-drain-instructions-empty
  (testing "drain-instructions! returns empty vector for unknown agent"
    (let [instructions (core/drain-instructions! "unknown-agent")]
      (is (empty? instructions)))))

(deftest test-drain-instructions-isolated
  (testing "Draining one agent doesn't affect others"
    (core/push-instruction! "ling-1" {:action "A"})
    (core/push-instruction! "ling-2" {:action "B"})
    (core/drain-instructions! "ling-1")
    (is (= 1 (count (get @core/instruction-queues "ling-2"))))))

;; =============================================================================
;; Test: Piggyback Instructions in Responses
;; =============================================================================

(deftest test-mcp-success-without-agent-id
  (testing "mcp-success without agent-id has no pending_instructions"
    (core/push-instruction! "ling-1" {:action "test"})
    (let [response (core/mcp-success "Hello")]
      (is (nil? (:pending_instructions response)))
      ;; Queue should still have the instruction
      (is (= 1 (count (get @core/instruction-queues "ling-1")))))))

(deftest test-mcp-success-with-agent-id-no-instructions
  (testing "mcp-success with agent-id but no instructions omits pending_instructions"
    (let [response (core/mcp-success "Hello" :agent-id "ling-1")]
      (is (nil? (:pending_instructions response))))))

(deftest test-mcp-success-with-agent-id-has-instructions
  (testing "mcp-success with agent-id includes pending_instructions"
    (core/push-instruction! "ling-1" {:action "pause" :reason "rate limit"})
    (core/push-instruction! "ling-1" {:action "change-priority" :level "high"})
    (let [response (core/mcp-success "Task completed" :agent-id "ling-1")]
      (is (= "Task completed" (:text response)))
      (is (= 2 (count (:pending_instructions response))))
      (is (= {:action "pause" :reason "rate limit"}
             (first (:pending_instructions response)))))))

(deftest test-mcp-success-drains-queue
  (testing "mcp-success with agent-id drains the instruction queue"
    (core/push-instruction! "ling-1" {:action "test"})
    (core/mcp-success "Done" :agent-id "ling-1")
    (is (empty? (get @core/instruction-queues "ling-1" [])))))

(deftest test-mcp-json-without-agent-id
  (testing "mcp-json without agent-id has no pending_instructions"
    (core/push-instruction! "ling-1" {:action "test"})
    (let [response (core/mcp-json {:status "ok"})]
      (is (nil? (:pending_instructions response))))))

(deftest test-mcp-json-with-agent-id-has-instructions
  (testing "mcp-json with agent-id includes pending_instructions"
    (core/push-instruction! "ling-1" {:action "redirect" :target "drone-2"})
    (let [response (core/mcp-json {:status "ok"} :agent-id "ling-1")]
      (is (= 1 (count (:pending_instructions response))))
      (is (= {:action "redirect" :target "drone-2"}
             (first (:pending_instructions response)))))))

(deftest test-mcp-json-drains-queue
  (testing "mcp-json with agent-id drains the instruction queue"
    (core/push-instruction! "ling-1" {:action "test"})
    (core/mcp-json {:data "value"} :agent-id "ling-1")
    (is (empty? (get @core/instruction-queues "ling-1" [])))))

;; =============================================================================
;; Test: Instruction Types (examples of what can be piggybacked)
;; =============================================================================

(deftest test-instruction-types
  (testing "Various instruction types can be queued"
    ;; Flow control
    (core/push-instruction! "ling-1" {:type "flow" :action "pause"})
    (core/push-instruction! "ling-1" {:type "flow" :action "abort"})

    ;; Priority changes
    (core/push-instruction! "ling-1" {:type "priority" :level "urgent"})

    ;; Context updates
    (core/push-instruction! "ling-1" {:type "context"
                                      :file "/src/changed.clj"
                                      :message "File modified externally"})

    ;; Coordination
    (core/push-instruction! "ling-1" {:type "coordinate"
                                      :wait-for "ling-2"
                                      :timeout-ms 5000})

    (let [instructions (core/drain-instructions! "ling-1")]
      (is (= 5 (count instructions)))
      (is (= "flow" (:type (first instructions))))
      (is (= "coordinate" (:type (last instructions)))))))

;; =============================================================================
;; Test: Thread Safety (basic)
;; =============================================================================

(deftest test-concurrent-push-drain
  (testing "Concurrent pushes don't lose instructions"
    (let [agent-id "concurrent-test"
          push-count 100
          futures (doall
                   (for [i (range push-count)]
                     (future
                       (core/push-instruction! agent-id {:index i}))))]
      ;; Wait for all pushes
      (doseq [f futures] @f)
      ;; Drain and verify count
      (let [instructions (core/drain-instructions! agent-id)]
        (is (= push-count (count instructions)))))))
