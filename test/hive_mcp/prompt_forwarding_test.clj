(ns hive-mcp.prompt-forwarding-test
  "Tests for Phase 1 prompt forwarding flow.

   Verifies the end-to-end flow:
   1. Elisp emits prompt-shown event
   2. sync.clj handle-prompt-shown receives event
   3. hivemind/add-swarm-prompt! stores the prompt
   4. get-status returns pending prompts
   5. remove-swarm-prompt! clears after response

   This is the architectural guarantee that prompts are visible to coordinator."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.swarm.sync :as sync]))

;;; Test fixtures

(defn reset-prompts-fixture
  "Reset pending-swarm-prompts between tests."
  [f]
  (reset! @(resolve 'hive-mcp.hivemind/pending-swarm-prompts) {})
  (f)
  (reset! @(resolve 'hive-mcp.hivemind/pending-swarm-prompts) {}))

(use-fixtures :each reset-prompts-fixture)

;;; Core functionality: add-swarm-prompt!

(deftest add-swarm-prompt!-stores-prompt-test
  (testing "add-swarm-prompt! stores prompt data correctly"
    (let [result (hivemind/add-swarm-prompt!
                  "test-slave-1"
                  "Allow tool: edit_file?"
                  "session-123"
                  1704067200000)]
      ;; Returns the stored data
      (is (map? result))
      (is (= "Allow tool: edit_file?" (:prompt result)))
      (is (= "session-123" (:session-id result)))
      (is (= 1704067200000 (:timestamp result)))
      (is (number? (:received-at result))))))

(deftest add-swarm-prompt!-keyed-by-slave-id-test
  (testing "Prompts are keyed by slave-id in atom"
    (hivemind/add-swarm-prompt! "slave-a" "Prompt A" nil nil)
    (hivemind/add-swarm-prompt! "slave-b" "Prompt B" nil nil)

    (let [prompts (hivemind/get-swarm-prompts)]
      (is (= 2 (count prompts)))
      (is (contains? prompts "slave-a"))
      (is (contains? prompts "slave-b"))
      (is (= "Prompt A" (get-in prompts ["slave-a" :prompt])))
      (is (= "Prompt B" (get-in prompts ["slave-b" :prompt]))))))

(deftest add-swarm-prompt!-overwrites-previous-test
  (testing "New prompt from same slave overwrites previous"
    (hivemind/add-swarm-prompt! "slave-x" "First prompt" nil nil)
    (hivemind/add-swarm-prompt! "slave-x" "Second prompt" nil nil)

    (let [prompts (hivemind/get-swarm-prompts)]
      (is (= 1 (count prompts)))
      (is (= "Second prompt" (get-in prompts ["slave-x" :prompt]))))))

;;; Core functionality: remove-swarm-prompt!

(deftest remove-swarm-prompt!-clears-prompt-test
  (testing "remove-swarm-prompt! removes prompt from atom"
    (hivemind/add-swarm-prompt! "slave-to-clear" "Pending prompt" nil nil)
    (is (contains? (hivemind/get-swarm-prompts) "slave-to-clear"))

    (hivemind/remove-swarm-prompt! "slave-to-clear")
    (is (not (contains? (hivemind/get-swarm-prompts) "slave-to-clear")))))

(deftest remove-swarm-prompt!-handles-nonexistent-test
  (testing "remove-swarm-prompt! handles nonexistent slave gracefully"
    ;; Should not throw
    (hivemind/remove-swarm-prompt! "nonexistent-slave")
    (is (empty? (hivemind/get-swarm-prompts)))))

;;; Integration: get-status includes prompts

(deftest get-status-includes-pending-swarm-prompts-test
  (testing "get-status includes pending-swarm-prompts in response"
    (hivemind/add-swarm-prompt! "prompt-slave-1" "Allow bash?" "sess-1" 1000)
    (hivemind/add-swarm-prompt! "prompt-slave-2" "Allow edit?" "sess-2" 2000)

    (let [status (hivemind/get-status)
          prompts (:pending-swarm-prompts status)]
      (is (vector? prompts))
      (is (= 2 (count prompts)))
      ;; Each prompt should have required fields
      (let [prompt-1 (first (filter #(= "prompt-slave-1" (:slave-id %)) prompts))]
        (is (= "Allow bash?" (:prompt prompt-1)))
        (is (= "sess-1" (:session-id prompt-1)))
        (is (= 1000 (:timestamp prompt-1)))
        (is (number? (:received-at prompt-1)))))))

(deftest get-status-returns-empty-vector-when-no-prompts-test
  (testing "get-status returns empty vector when no prompts pending"
    (let [status (hivemind/get-status)
          prompts (:pending-swarm-prompts status)]
      (is (vector? prompts))
      (is (empty? prompts)))))

;;; Integration: sync.clj handle-prompt-shown

(deftest handle-prompt-shown-forwards-to-hivemind-test
  (testing "sync.clj handle-prompt-shown calls add-swarm-prompt!"
    ;; Use the private handler directly
    (let [event {"slave-id" "sync-slave-123"
                 "prompt" "Allow this tool?"
                 "session-id" "sync-session"
                 "timestamp" 999}]
      (#'sync/handle-prompt-shown event))

    (let [prompts (hivemind/get-swarm-prompts)]
      (is (contains? prompts "sync-slave-123"))
      (is (= "Allow this tool?" (get-in prompts ["sync-slave-123" :prompt]))))))

(deftest handle-prompt-shown-handles-keyword-keys-test
  (testing "handle-prompt-shown handles both string and keyword keys"
    ;; Keyword keys (as might come from internal code)
    (#'sync/handle-prompt-shown {:slave-id "kw-slave"
                                 :prompt "Keyword prompt"
                                 :session-id nil
                                 :timestamp nil})

    (is (contains? (hivemind/get-swarm-prompts) "kw-slave"))))

(deftest handle-prompt-shown-requires-slave-id-and-prompt-test
  (testing "handle-prompt-shown is no-op without slave-id or prompt"
    ;; Missing slave-id
    (#'sync/handle-prompt-shown {"prompt" "orphan"})
    ;; Missing prompt
    (#'sync/handle-prompt-shown {"slave-id" "no-prompt"})

    (is (empty? (hivemind/get-swarm-prompts))
        "Should not store incomplete events")))

;;; End-to-end flow test

(deftest prompt-forwarding-flow-test
  (testing "Complete prompt forwarding flow: emit -> store -> status -> clear"
    ;; 1. Simulate prompt-shown event from Elisp
    (#'sync/handle-prompt-shown {"slave-id" "ling-e2e"
                                 "prompt" "Allow file write?"
                                 "session-id" "e2e-session"
                                 "timestamp" 12345})

    ;; 2. Verify it appears in get-status
    (let [status (hivemind/get-status)
          prompts (:pending-swarm-prompts status)
          ling-prompt (first (filter #(= "ling-e2e" (:slave-id %)) prompts))]
      (is (some? ling-prompt) "Prompt should appear in status")
      (is (= "Allow file write?" (:prompt ling-prompt))))

    ;; 3. Simulate response (would be via swarm_respond_prompt tool)
    (hivemind/remove-swarm-prompt! "ling-e2e")

    ;; 4. Verify it's cleared from status
    (let [status (hivemind/get-status)
          prompts (:pending-swarm-prompts status)]
      (is (not (some #(= "ling-e2e" (:slave-id %)) prompts))
          "Prompt should be cleared after response"))))

(comment
  ;; Run tests from REPL
  (clojure.test/run-tests 'hive-mcp.prompt-forwarding-test))
