(ns hive-mcp.crystal.core-test
  "TDD tests for crystal/core.clj - progressive crystallization logic.
   
   These tests PIN the behavior of summarize-session-progress and related
   functions to catch regressions like the NPE bug where nil content caused
   str/split-lines to fail."
  (:require [clojure.test :refer [deftest testing is are]]
            [hive-mcp.crystal.core :as core]))

;; =============================================================================
;; Session ID/Tag Tests
;; =============================================================================

(deftest session-id-test
  (testing "session-id returns date string"
    (let [sid (core/session-id)]
      (is (string? sid))
      (is (re-matches #"\d{4}-\d{2}-\d{2}" sid)))))

(deftest session-tag-test
  (testing "session-tag creates proper tag format"
    (is (= "session:2026-01-11" (core/session-tag "2026-01-11")))
    (is (string? (core/session-tag)))
    (is (re-matches #"session:\d{4}-\d{2}-\d{2}" (core/session-tag)))))

;; =============================================================================
;; Summarize Session Progress - The NPE Bug Area
;; =============================================================================

(deftest summarize-session-progress-happy-path-test
  (testing "summarize with valid notes and commits"
    (let [notes [{:content "Fixed bug in auth" :tags ["completed-task"]}
                 {:content "Added new feature" :tags ["completed-task"]}]
          commits ["abc123 Fix auth bug"
                   "def456 Add feature"]
          result (core/summarize-session-progress notes commits)]
      (is (map? result))
      (is (= :note (:type result)))
      (is (string? (:content result)))
      (is (vector? (:tags result)))
      (is (some #{"session-summary"} (:tags result)))
      (is (some #{"wrap-generated"} (:tags result))))))

(deftest summarize-session-progress-nil-content-test
  (testing "BUG FIX: summarize handles nil content in notes"
    ;; This was the NPE bug - str/split-lines fails on nil
    (let [notes [{:content nil :tags ["completed-task"]}
                 {:content "Valid content" :tags ["completed-task"]}]
          commits []]
      ;; Should NOT throw NPE
      (is (map? (core/summarize-session-progress notes commits))))))

(deftest summarize-session-progress-empty-notes-test
  (testing "summarize handles empty notes collection"
    (let [result (core/summarize-session-progress [] [])]
      (is (map? result))
      (is (string? (:content result))))))

(deftest summarize-session-progress-nil-notes-test
  (testing "summarize handles nil notes collection"
    ;; Edge case: what if notes is nil instead of empty?
    (let [result (core/summarize-session-progress nil nil)]
      (is (map? result))
      (is (string? (:content result))))))

(deftest summarize-session-progress-missing-content-key-test
  (testing "summarize handles notes with missing :content key"
    (let [notes [{:tags ["completed-task"]} ;; no :content key at all
                 {:content "Has content" :tags ["other"]}]
          result (core/summarize-session-progress notes [])]
      (is (map? result))
      (is (string? (:content result))))))

(deftest summarize-session-progress-structured-content-test
  (testing "summarize handles notes with map content (kanban tasks)"
    ;; Kanban notes often have structured content like:
    ;; {:task-type "kanban" :title "..." :status "done"}
    (let [notes [{:content {:task-type "kanban"
                            :title "Fix the bug"
                            :status "done"}
                  :tags ["kanban"]}]
          result (core/summarize-session-progress notes [])]
      (is (map? result))
      (is (string? (:content result))))))

(deftest summarize-session-progress-mixed-content-types-test
  (testing "summarize handles mix of string, nil, and map content"
    (let [notes [{:content "String content" :tags ["completed-task"]}
                 {:content nil :tags ["completed-task"]}
                 {:content {:title "Map content"} :tags ["kanban"]}
                 {:tags ["no-content"]}] ;; missing key entirely
          commits ["commit1" "commit2"]]
      (is (map? (core/summarize-session-progress notes commits))))))

;; =============================================================================
;; Promotion Score Tests
;; =============================================================================

(deftest calculate-promotion-score-test
  (testing "promotion score with no recalls"
    (let [result (core/calculate-promotion-score [])]
      (is (map? result))
      (is (contains? result :score))
      (is (contains? result :breakdown))
      (is (zero? (:score result)))))

  (testing "promotion score with various recall types"
    ;; Direct recall has weight 1.0, assistant has 0.7
    (let [recalls [{:context :explicit-reference :count 2}
                   {:context :cross-session :count 1}]
          result (core/calculate-promotion-score recalls)]
      (is (pos? (:score result)))
      (is (= 2 (count (:breakdown result)))))))

(deftest should-promote-test
  (testing "should-promote? with no recalls"
    (let [result (core/should-promote? {:duration :ephemeral :recalls []})]
      (is (map? result))
      (is (false? (:promote? result)))
      (is (zero? (:current-score result)))))

  (testing "should-promote? respects duration thresholds"
    ;; Ephemeral needs score >= 5.0 to promote
    (let [entry {:duration :ephemeral :recalls []}
          result (core/should-promote? entry)]
      (is (false? (:promote? result)))
      (is (= 5.0 (:threshold result)))))

  (testing "should-promote? returns true when threshold met"
    (let [entry {:duration :ephemeral
                 :recalls [{:context :explicit-reference :count 5}]}
          result (core/should-promote? entry)]
      (is (true? (:promote? result)))
      (is (= :short (:next-duration result))))))

;; =============================================================================
;; Duration Progression Tests
;; =============================================================================

(deftest current-duration->next-test
  (testing "duration progression follows expected order"
    (is (= :short (core/current-duration->next :ephemeral)))
    (is (= :medium (core/current-duration->next :short)))
    (is (= :long (core/current-duration->next :medium)))
    (is (= :permanent (core/current-duration->next :long)))
    (is (= :permanent (core/current-duration->next :permanent)))))

;; =============================================================================
;; Task to Progress Note Tests
;; =============================================================================

(deftest task-to-progress-note-test
  (testing "converts kanban task to progress note"
    (let [task {:title "Fix the bug"
                :context "Authentication module"
                :priority "high"
                :started "2026-01-11T10:00:00"}
          result (core/task-to-progress-note task)]
      (is (map? result))
      (is (string? (:content result)))
      (is (vector? (:tags result))))))

(deftest task-to-progress-note-nil-fields-test
  (testing "handles task with nil/missing fields"
    (let [task {:title "Only title"} ;; missing context, priority, started
          result (core/task-to-progress-note task)]
      (is (map? result))
      (is (string? (:content result))))))

;; =============================================================================
;; Regression Test: "Key must be integer" error
;; =============================================================================

(deftest summarize-session-progress-non-map-items-test
  (testing "BUG FIX: summarize handles non-map items without 'Key must be integer' error"
    ;; This regression test ensures that if notes contains vectors or strings
    ;; instead of maps, we don't get "Key must be integer" when accessing (:tags item).
    ;; The bug occurred when JSON parsing returned nested arrays or malformed data.
    (let [notes-with-vectors [["nested" "array"]  ;; vector - would throw without guard
                              {:content "Valid note" :tags ["completed-task"]}
                              "just a string"      ;; string - would not throw but is invalid
                              {:content "Another valid" :tags ["other"]}
                              [1 2 3]]             ;; another vector
          result (core/summarize-session-progress notes-with-vectors [])]
      (is (map? result))
      (is (string? (:content result)))
      ;; Only valid maps should be processed for task count
      (is (string? (get-in result [:content])))))

  (testing "summarize filters out non-map items correctly"
    ;; Only the 1 item with "completed-task" tag should be counted
    (let [notes [["ignored" "vector"]
                 {:content "Task done" :tags ["completed-task"]}
                 "ignored string"
                 {:content "Not a task" :tags ["note"]}]
          result (core/summarize-session-progress notes [])
          content (:content result)]
      ;; Should contain "Completed Tasks: 1" since only one valid map has completed-task tag
      (is (re-find #"Completed Tasks: 1" content)))))
