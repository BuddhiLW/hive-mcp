(ns hive-mcp.guards-test
  "Tests for test isolation guards.
   
   CLARITY-Y: Yield safe failure - tests validate protection mechanisms."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.guards :as guards]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-guards-fixture [f]
  ;; Ensure clean state before each test
  (guards/clear-coordinator-running!)
  (f)
  ;; Clean up after
  (guards/clear-coordinator-running!))

(use-fixtures :each reset-guards-fixture)

;; =============================================================================
;; Unit Tests
;; =============================================================================

(deftest coordinator-running?-test
  (testing "initially returns false"
    (is (false? (guards/coordinator-running?))))
  
  (testing "returns true after mark-coordinator-running!"
    (guards/mark-coordinator-running!)
    (is (true? (guards/coordinator-running?))))
  
  (testing "returns false after clear-coordinator-running!"
    (guards/mark-coordinator-running!)
    (guards/clear-coordinator-running!)
    (is (false? (guards/coordinator-running?)))))

(deftest when-not-coordinator-test
  (testing "executes body when coordinator not running"
    (let [executed? (atom false)]
      (guards/when-not-coordinator
        "test warning"
        (reset! executed? true))
      (is (true? @executed?))))
  
  (testing "returns body result when coordinator not running"
    (let [result (guards/when-not-coordinator
                   "test warning"
                   :expected-result)]
      (is (= :expected-result result))))
  
  (testing "does NOT execute body when coordinator running"
    (guards/mark-coordinator-running!)
    (let [executed? (atom false)]
      (guards/when-not-coordinator
        "test warning"
        (reset! executed? true))
      (is (false? @executed?))))
  
  (testing "returns nil when coordinator running"
    (guards/mark-coordinator-running!)
    (let [result (guards/when-not-coordinator
                   "test warning"
                   :should-not-return)]
      (is (nil? result)))))

(deftest idempotence-test
  (testing "mark-coordinator-running! is idempotent"
    (guards/mark-coordinator-running!)
    (guards/mark-coordinator-running!)
    (guards/mark-coordinator-running!)
    (is (true? (guards/coordinator-running?))))
  
  (testing "clear-coordinator-running! is idempotent"
    (guards/mark-coordinator-running!)
    (guards/clear-coordinator-running!)
    (guards/clear-coordinator-running!)
    (guards/clear-coordinator-running!)
    (is (false? (guards/coordinator-running?)))))
