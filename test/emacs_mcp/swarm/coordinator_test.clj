(ns emacs-mcp.swarm.coordinator-test
  "Integration tests for swarm coordinator.
   
   Tests real-world scenarios:
   - Two agents trying to edit same file → conflict detected, second queued
   - Agent completes → claims released → queued task becomes ready
   - Full dispatch-or-queue lifecycle"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [emacs-mcp.swarm.coordinator :as coord]
            [emacs-mcp.swarm.logic :as logic]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-state-fixture
  "Reset both logic database and coordinator queue before each test."
  [f]
  (logic/reset-db!)
  (coord/clear-queue!)
  (f))

(use-fixtures :each reset-state-fixture)

;; =============================================================================
;; Scenario: Two Agents Same File
;; =============================================================================

(deftest two-agents-same-file-conflict-test
  (testing "When two agents try to edit the same file, second is blocked"
    ;; Setup: Agent Alice is working on core.clj
    (logic/add-slave! "alice" :working)
    (logic/add-claim! "/src/core.clj" "alice")

    ;; Agent Bob tries to dispatch a task that touches core.clj
    (logic/add-slave! "bob" :idle)

    (let [check-result (coord/pre-flight-check
                        {:slave-id "bob"
                         :files ["/src/core.clj"]})]

      (is (not (:approved? check-result))
          "Bob should NOT be approved - file conflict")
      (is (:queue? check-result)
          "Bob's task should be queued")
      (is (= 1 (count (:conflicts check-result)))
          "Should have exactly one conflict")
      (is (= "/src/core.clj" (:file (first (:conflicts check-result))))
          "Conflict should be on core.clj")
      (is (= "alice" (:held-by (first (:conflicts check-result))))
          "File should be held by alice"))))

(deftest two-agents-different-files-no-conflict-test
  (testing "When two agents edit different files, no conflict"
    ;; Setup: Agent Alice is working on core.clj
    (logic/add-slave! "alice" :working)
    (logic/add-claim! "/src/core.clj" "alice")

    ;; Agent Bob tries to dispatch a task on different file
    (logic/add-slave! "bob" :idle)

    (let [check-result (coord/pre-flight-check
                        {:slave-id "bob"
                         :files ["/src/utils.clj"]})]

      (is (:approved? check-result)
          "Bob should be approved - no conflict")
      (is (not (:queue? check-result))
          "Bob's task should NOT be queued")
      (is (empty? (:conflicts check-result))
          "Should have no conflicts"))))

;; =============================================================================
;; Scenario: Claim Release and Queue Processing
;; =============================================================================

(deftest claim-release-unblocks-queued-task-test
  (testing "When agent completes and releases claims, queued task becomes ready"
    ;; Setup: Alice working on core.clj
    (logic/add-slave! "alice" :working)
    (logic/add-task! "task-alice" "alice" :dispatched)
    (coord/register-task-claims! "task-alice" "alice" ["/src/core.clj"])

    ;; Bob's task gets queued due to conflict
    (logic/add-slave! "bob" :idle)
    (let [queue-result (coord/dispatch-or-queue!
                        {:slave-id "bob"
                         :prompt "Edit /src/core.clj"
                         :files ["/src/core.clj"]})]

      (is (= :queued (:action queue-result))
          "Bob's task should be queued")

      ;; Queue should have Bob's task
      (is (= 1 (count (coord/get-queue)))
          "Queue should have one task")

      ;; Alice completes - release claims
      (coord/release-task-claims! "task-alice")

      ;; Process queue - Bob's task should now be ready
      (let [ready-tasks (coord/get-ready-tasks)]
        (is (= 1 (count ready-tasks))
            "Bob's task should be ready after Alice releases claims")
        (is (= "bob" (:slave-id (first ready-tasks)))
            "Ready task should belong to Bob")))))

;; =============================================================================
;; Scenario: dispatch-or-queue! Lifecycle
;; =============================================================================

(deftest dispatch-or-queue-approved-test
  (testing "dispatch-or-queue! returns :dispatch when no conflicts"
    (logic/add-slave! "alice" :idle)

    (let [result (coord/dispatch-or-queue!
                  {:slave-id "alice"
                   :prompt "Create new file"
                   :files ["/src/new-feature.clj"]})]

      (is (= :dispatch (:action result))
          "Should approve dispatch")
      (is (= ["/src/new-feature.clj"] (:files result))
          "Should return the files"))))

(deftest dispatch-or-queue-queued-test
  (testing "dispatch-or-queue! returns :queued when conflicts exist"
    ;; Alice has claim
    (logic/add-slave! "alice" :working)
    (logic/add-claim! "/src/core.clj" "alice")

    ;; Bob tries to dispatch
    (logic/add-slave! "bob" :idle)
    (let [result (coord/dispatch-or-queue!
                  {:slave-id "bob"
                   :prompt "Refactor core"
                   :files ["/src/core.clj"]})]

      (is (= :queued (:action result))
          "Should queue the task")
      (is (string? (:task-id result))
          "Should return a task ID")
      (is (seq (:conflicts result))
          "Should include conflicts"))))

(deftest dispatch-or-queue-deadlock-blocked-test
  (testing "dispatch-or-queue! returns :blocked on circular dependency"
    ;; Setup circular dependency scenario
    (logic/add-slave! "alice" :working)
    (logic/add-slave! "bob" :working)
    (logic/add-task! "task-a" "alice" :dispatched)
    (logic/add-task! "task-b" "bob" :dispatched)
    (logic/add-dependency! "task-a" "task-b")

    ;; Try to add dependency that would create cycle: b -> a -> b
    (let [result (coord/pre-flight-check
                  {:slave-id "bob"
                   :files []
                   :dependencies [["task-b" "task-a"]]})]

      (is (not (:approved? result))
          "Should not approve - would deadlock")
      (is (seq (:would-deadlock result))
          "Should report deadlock pairs"))))

;; =============================================================================
;; Scenario: File Extraction from Prompt
;; =============================================================================

(deftest file-extraction-from-prompt-test
  (testing "Extracts file paths from task prompts when files not explicit"
    (logic/add-slave! "alice" :idle)

    (let [result (coord/pre-flight-check
                  {:slave-id "alice"
                   :prompt "Please edit /home/user/project/src/core.clj and add logging"})]

      (is (seq (:extracted-files result))
          "Should extract files from prompt")
      (is (some #(re-find #"core\.clj" %) (:extracted-files result))
          "Should find core.clj in prompt"))))

;; =============================================================================
;; Scenario: Multiple Files, Partial Conflict
;; =============================================================================

(deftest partial-file-conflict-test
  (testing "Conflict detected even when only some files conflict"
    ;; Alice has claim on one file
    (logic/add-slave! "alice" :working)
    (logic/add-claim! "/src/core.clj" "alice")

    ;; Bob wants to edit two files, one conflicts
    (logic/add-slave! "bob" :idle)
    (let [result (coord/pre-flight-check
                  {:slave-id "bob"
                   :files ["/src/core.clj" "/src/utils.clj"]})]

      (is (not (:approved? result))
          "Should not approve - partial conflict")
      (is (= 1 (count (:conflicts result)))
          "Should report one conflict")
      (is (= "/src/core.clj" (:file (first (:conflicts result))))
          "Conflict should be on core.clj only"))))

;; =============================================================================
;; Scenario: Three Agents, Chain of Conflicts
;; =============================================================================

(deftest three-agent-conflict-chain-test
  (testing "Multiple agents queued for same file, released in order"
    ;; Alice working on core.clj
    (logic/add-slave! "alice" :working)
    (logic/add-task! "task-alice" "alice" :dispatched)
    (coord/register-task-claims! "task-alice" "alice" ["/src/core.clj"])

    ;; Bob and Carol both want core.clj - both get queued
    (logic/add-slave! "bob" :idle)
    (logic/add-slave! "carol" :idle)

    (let [bob-result (coord/dispatch-or-queue!
                      {:slave-id "bob"
                       :files ["/src/core.clj"]})
          carol-result (coord/dispatch-or-queue!
                        {:slave-id "carol"
                         :files ["/src/core.clj"]})]

      (is (= :queued (:action bob-result)))
      (is (= :queued (:action carol-result)))
      (is (= 2 (count (coord/get-queue)))
          "Queue should have two tasks")

      ;; Alice completes
      (coord/release-task-claims! "task-alice")

      ;; Both Bob and Carol are ready (both want same file, no one holds it now)
      (let [ready (coord/get-ready-tasks)]
        (is (= 2 (count ready))
            "Both tasks should be ready after Alice releases")))))
