(ns hive-mcp.agent.drone.diff-mgmt-test
  "Tests for drone diff management.

   Tests diff lifecycle operations extracted from drone.clj:
   - DiffResults record creation
   - Diff ID tracking (before/after capture)
   - Auto-apply logic
   - Wave tagging"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.drone.diff-mgmt :as diff-mgmt]))

;; =============================================================================
;; DiffResults Record Tests
;; =============================================================================

(deftest test-diff-results-creation
  (testing "DiffResults record creation"
    (let [results (diff-mgmt/->diff-results
                    ["file1.clj" "file2.clj"]
                    [{:file "file3.clj" :error "lint error"}]
                    ["diff-123"])]
      (is (= ["file1.clj" "file2.clj"] (:applied results)))
      (is (= [{:file "file3.clj" :error "lint error"}] (:failed results)))
      (is (= ["diff-123"] (:proposed results))))))

(deftest test-empty-diff-results
  (testing "Empty DiffResults has all empty vectors"
    (let [results (diff-mgmt/empty-diff-results)]
      (is (= [] (:applied results)))
      (is (= [] (:failed results)))
      (is (= [] (:proposed results))))))

;; =============================================================================
;; Diff ID Tracking Tests
;; =============================================================================

(deftest test-get-new-diff-ids
  (testing "Calculates new diff IDs correctly"
    (let [before #{"a" "b" "c"}
          after #{"a" "b" "c" "d" "e"}]
      (is (= #{"d" "e"} (diff-mgmt/get-new-diff-ids before after)))))

  (testing "Returns empty set when no new diffs"
    (let [before #{"a" "b"}
          after #{"a" "b"}]
      (is (= #{} (diff-mgmt/get-new-diff-ids before after)))))

  (testing "Handles empty before set"
    (let [before #{}
          after #{"a" "b"}]
      (is (= #{"a" "b"} (diff-mgmt/get-new-diff-ids before after))))))

;; =============================================================================
;; Summarize Results Tests
;; =============================================================================

(deftest test-summarize-diff-results
  (testing "Summarizes proposed diffs"
    (let [results (diff-mgmt/->diff-results [] [] ["d1" "d2"])]
      (is (= "2 diffs proposed for review"
             (diff-mgmt/summarize-diff-results results)))))

  (testing "Summarizes successful applies"
    (let [results (diff-mgmt/->diff-results ["f1" "f2" "f3"] [] [])]
      (is (= "3 diffs applied successfully"
             (diff-mgmt/summarize-diff-results results)))))

  (testing "Summarizes mixed results"
    (let [results (diff-mgmt/->diff-results ["f1"] [{:file "f2"}] [])]
      (is (= "1 applied, 1 failed"
             (diff-mgmt/summarize-diff-results results)))))

  (testing "Summarizes only failures"
    (let [results (diff-mgmt/->diff-results [] [{:file "f1"}] [])]
      (is (= "1 diffs failed"
             (diff-mgmt/summarize-diff-results results)))))

  (testing "Summarizes no diffs"
    (let [results (diff-mgmt/empty-diff-results)]
      (is (= "No diffs"
             (diff-mgmt/summarize-diff-results results))))))

;; =============================================================================
;; Integration Tests (require mocking pending-diffs atom)
;; =============================================================================

(comment
  ;; These would require mocking the diff/pending-diffs atom
  ;; Left as placeholder for future expansion
  (deftest test-auto-apply-diffs-integration)
  (deftest test-tag-diffs-with-wave-integration))
