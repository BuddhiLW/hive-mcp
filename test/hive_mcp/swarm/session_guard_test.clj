(ns hive-mcp.swarm.session-guard-test
  "Unit tests for session guard - detects when lings shout :completed
   but skip calling session_complete.

   TDD: Tests written first, implementation follows.

   Test scenarios:
   1. Ling shouts completed, no session_complete within 30s → warning emitted
   2. Ling shouts completed, calls session_complete → no warning
   3. Multiple lings tracked independently
   4. Timer cleanup on session_complete"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.session-guard :as guard]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:dynamic *warnings* (atom []))

(defn with-captured-warnings
  "Fixture that captures warning emissions."
  [f]
  (reset! *warnings* [])
  (guard/reset-state!)
  ;; Override warning handler to capture warnings
  (guard/set-warning-handler!
   (fn [agent-id data]
     (swap! *warnings* conj {:agent-id agent-id :data data})))
  (try
    (f)
    (finally
      (guard/reset-state!)
      (reset! *warnings* []))))

(use-fixtures :each with-captured-warnings)

;; =============================================================================
;; Core Functionality Tests
;; =============================================================================

(deftest track-completion-shout
  (testing "Completion shout is tracked"
    (guard/register-completion-shout! "ling-123" {:task "test task"})
    (is (guard/pending-completion? "ling-123")
        "Should track pending completion")))

(deftest session-complete-clears-tracking
  (testing "Calling session_complete clears pending status"
    (guard/register-completion-shout! "ling-456" {:task "test task"})
    (is (guard/pending-completion? "ling-456"))

    (guard/mark-session-complete! "ling-456")
    (is (not (guard/pending-completion? "ling-456"))
        "Should no longer be pending after session_complete")))

(deftest multiple-lings-tracked-independently
  (testing "Multiple lings are tracked independently"
    (guard/register-completion-shout! "ling-A" {:task "task A"})
    (guard/register-completion-shout! "ling-B" {:task "task B"})

    (is (guard/pending-completion? "ling-A"))
    (is (guard/pending-completion? "ling-B"))

    ;; Complete only A
    (guard/mark-session-complete! "ling-A")
    (is (not (guard/pending-completion? "ling-A")))
    (is (guard/pending-completion? "ling-B")
        "Ling B should still be pending")))

(deftest warning-emitted-on-timeout
  (testing "Warning is emitted when session_complete not called within timeout"
    ;; Use shorter timeout for testing
    (guard/set-timeout-ms! 100)

    (guard/register-completion-shout! "ling-timeout" {:task "will timeout"})

    ;; Wait for timeout
    (Thread/sleep 200)

    ;; Should have warning
    (is (= 1 (count @*warnings*))
        "Should emit exactly one warning")
    (is (= "ling-timeout" (:agent-id (first @*warnings*)))
        "Warning should be for correct agent")))

(deftest no-warning-when-session-complete-called
  (testing "No warning when session_complete called before timeout"
    (guard/set-timeout-ms! 200)

    (guard/register-completion-shout! "ling-good" {:task "will complete properly"})

    ;; Complete before timeout
    (Thread/sleep 50)
    (guard/mark-session-complete! "ling-good")

    ;; Wait past timeout
    (Thread/sleep 250)

    (is (empty? @*warnings*)
        "Should not emit warning when session_complete called")))

(deftest warning-includes-original-shout-data
  (testing "Warning includes data from original completion shout"
    (guard/set-timeout-ms! 100)

    (guard/register-completion-shout! "ling-data"
                                      {:task "important task"
                                       :message "Task completed successfully"})

    ;; Wait for timeout
    (Thread/sleep 200)

    (is (= 1 (count @*warnings*)))
    (let [warning-data (:data (first @*warnings*))]
      (is (= "important task" (:original-task warning-data))
          "Should include original task")
      (is (contains? warning-data :elapsed-ms)
          "Should include elapsed time"))))

(deftest duplicate-shouts-reset-timer
  (testing "Duplicate completion shouts reset the timer"
    (guard/set-timeout-ms! 150)

    (guard/register-completion-shout! "ling-dup" {:task "first shout"})
    (Thread/sleep 100)

    ;; Second shout should reset timer
    (guard/register-completion-shout! "ling-dup" {:task "second shout"})
    (Thread/sleep 100)

    ;; Should not have timed out yet (only 100ms since second shout)
    (is (empty? @*warnings*))

    ;; Wait for actual timeout
    (Thread/sleep 100)

    (is (= 1 (count @*warnings*))
        "Should emit warning after reset timer expires")))

(deftest get-pending-returns-all-pending
  (testing "get-pending-completions returns all pending agents"
    (guard/register-completion-shout! "ling-P1" {:task "task 1"})
    (guard/register-completion-shout! "ling-P2" {:task "task 2"})
    (guard/register-completion-shout! "ling-P3" {:task "task 3"})

    (guard/mark-session-complete! "ling-P2")

    (let [pending (guard/get-pending-completions)]
      (is (= 2 (count pending)))
      (is (contains? pending "ling-P1"))
      (is (contains? pending "ling-P3"))
      (is (not (contains? pending "ling-P2"))))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest session-complete-without-shout-is-noop
  (testing "Marking session complete without prior shout is a no-op"
    (guard/mark-session-complete! "ling-never-shouted")
    ;; Should not throw, just be a no-op
    (is (not (guard/pending-completion? "ling-never-shouted")))))

(deftest reset-clears-all-pending
  (testing "reset-state! clears all pending completions"
    (guard/register-completion-shout! "ling-R1" {:task "task"})
    (guard/register-completion-shout! "ling-R2" {:task "task"})

    (guard/reset-state!)

    (is (empty? (guard/get-pending-completions)))))
