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
  (testing "summarize returns nil for empty notes AND empty commits (no content)"
    ;; NEW BEHAVIOR: Returns nil when there's nothing to summarize
    (let [result (core/summarize-session-progress [] [])]
      (is (nil? result)))))

(deftest summarize-session-progress-nil-notes-test
  (testing "summarize returns nil for nil notes AND nil commits (no content)"
    ;; Edge case: what if notes is nil instead of empty?
    ;; NEW BEHAVIOR: Returns nil when there's nothing to summarize
    (let [result (core/summarize-session-progress nil nil)]
      (is (nil? result)))))

(deftest summarize-session-progress-all-nil-content-test
  (testing "summarize returns nil when ALL notes have nil/empty content"
    ;; NEW BEHAVIOR: Filters out nil/empty content notes BEFORE processing
    ;; If no notes remain with actual content AND no commits, returns nil
    (let [notes [{:content nil :tags ["completed-task"]}
                 {:content "" :tags ["completed-task"]}
                 {:content "   " :tags ["note"]}  ;; blank string
                 {:tags ["no-content"]}]          ;; missing key entirely
          result (core/summarize-session-progress notes [])]
      (is (nil? result))))

  (testing "summarize returns map when at least one note has content"
    ;; Even if most notes are nil/empty, having ONE valid note = returns map
    (let [notes [{:content nil :tags ["completed-task"]}
                 {:content "Valid content here" :tags ["completed-task"]}
                 {:content "" :tags ["note"]}]
          result (core/summarize-session-progress notes [])]
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

;; =============================================================================
;; Staleness Decay Tests (W2)
;; =============================================================================

(deftest days-since-test
  (testing "days-since with valid timestamps"
    (let [now (java.time.ZonedDateTime/now)
          week-ago (str (.minusDays now 7))]
      (is (= 7 (core/days-since week-ago)))))

  (testing "days-since with nil/empty returns nil"
    (is (nil? (core/days-since nil)))
    (is (nil? (core/days-since "")))
    (is (nil? (core/days-since "   "))))

  (testing "days-since with unparseable returns nil"
    (is (nil? (core/days-since "not-a-date")))))

(deftest decay-candidate-test
  (testing "low access + non-permanent + non-axiom is candidate"
    (is (true? (core/decay-candidate? {:access-count 0 :duration "short" :type "note"})))
    (is (true? (core/decay-candidate? {:access-count 2 :duration "medium" :type "convention"}))))

  (testing "high access count is NOT candidate"
    (is (false? (core/decay-candidate? {:access-count 5 :duration "short" :type "note"})))
    (is (false? (core/decay-candidate? {:access-count 3 :duration "short" :type "note"}))))

  (testing "permanent duration is NOT candidate"
    (is (false? (core/decay-candidate? {:access-count 0 :duration "permanent" :type "note"}))))

  (testing "axiom type is NOT candidate"
    (is (false? (core/decay-candidate? {:access-count 0 :duration "long" :type "axiom"}))))

  (testing "nil access-count treated as 0 (candidate)"
    (is (true? (core/decay-candidate? {:access-count nil :duration "short" :type "note"}))))

  (testing "custom access-threshold"
    (is (true? (core/decay-candidate?
                {:access-count 4 :duration "short" :type "note"}
                {:access-threshold 5})))
    (is (false? (core/decay-candidate?
                 {:access-count 5 :duration "short" :type "note"}
                 {:access-threshold 5})))))

(deftest calculate-decay-delta-test
  (testing "recently accessed entry has no decay"
    (let [yesterday (str (.minusDays (java.time.ZonedDateTime/now) 1))]
      (is (= 0.0 (core/calculate-decay-delta
                  {:access-count 0 :last-accessed yesterday :duration "short"})))))

  (testing "old entry with no access decays"
    (let [month-ago (str (.minusDays (java.time.ZonedDateTime/now) 30))]
      (is (pos? (core/calculate-decay-delta
                 {:access-count 0 :last-accessed month-ago :duration "medium"})))))

  (testing "never-accessed entry (nil last-accessed) decays as 30 days idle"
    (let [delta (core/calculate-decay-delta
                 {:access-count 0 :last-accessed nil :duration "medium"})]
      (is (pos? delta))))

  (testing "ephemeral decays faster than long"
    (let [month-ago (str (.minusDays (java.time.ZonedDateTime/now) 30))
          entry-base {:access-count 0 :last-accessed month-ago}
          ephemeral-delta (core/calculate-decay-delta (assoc entry-base :duration "ephemeral"))
          long-delta (core/calculate-decay-delta (assoc entry-base :duration "long"))]
      (is (> ephemeral-delta long-delta))))

  (testing "higher access count dampens decay"
    (let [month-ago (str (.minusDays (java.time.ZonedDateTime/now) 30))
          low-access (core/calculate-decay-delta
                      {:access-count 0 :last-accessed month-ago :duration "medium"})
          high-access (core/calculate-decay-delta
                       {:access-count 2 :last-accessed month-ago :duration "medium"})]
      (is (> low-access high-access))))

  (testing "custom recency-days threshold"
    (let [five-days-ago (str (.minusDays (java.time.ZonedDateTime/now) 5))]
      ;; Default recency is 7 days, so 5 days ago should not decay
      (is (= 0.0 (core/calculate-decay-delta
                  {:access-count 0 :last-accessed five-days-ago :duration "medium"})))
      ;; With recency-days=3, 5 days ago SHOULD decay
      (is (pos? (core/calculate-decay-delta
                 {:access-count 0 :last-accessed five-days-ago :duration "medium"}
                 {:recency-days 3}))))))

;; =============================================================================
;; Cross-Pollination Detection Tests (W5)
;; =============================================================================

(deftest extract-xpoll-projects-test
  (testing "no xpoll tags returns empty set"
    (is (= #{} (core/extract-xpoll-projects {:tags ["scope:project:hive-mcp" "decision"]})))
    (is (= #{} (core/extract-xpoll-projects {:tags []})))
    (is (= #{} (core/extract-xpoll-projects {:tags nil})))
    (is (= #{} (core/extract-xpoll-projects {}))))

  (testing "extracts project IDs from xpoll tags"
    (is (= #{"k8s-cluster"} (core/extract-xpoll-projects
                             {:tags ["xpoll:project:k8s-cluster"]})))
    (is (= #{"k8s" "webapp"} (core/extract-xpoll-projects
                              {:tags ["xpoll:project:k8s" "xpoll:project:webapp"]})))
    (is (= #{"a" "b" "c"} (core/extract-xpoll-projects
                           {:tags ["other-tag" "xpoll:project:a" "scope:project:x"
                                   "xpoll:project:b" "xpoll:project:c"]}))))

  (testing "deduplicates project IDs"
    (is (= #{"k8s"} (core/extract-xpoll-projects
                     {:tags ["xpoll:project:k8s" "xpoll:project:k8s"]})))))

(deftest cross-pollination-count-test
  (testing "returns count of distinct cross-project accesses"
    (is (= 0 (core/cross-pollination-count {:tags []})))
    (is (= 1 (core/cross-pollination-count {:tags ["xpoll:project:a"]})))
    (is (= 3 (core/cross-pollination-count
              {:tags ["xpoll:project:a" "xpoll:project:b" "xpoll:project:c"]})))))

(deftest cross-pollination-score-test
  (testing "score is n-projects * cross-project weight (3.0)"
    (is (= 0.0 (core/cross-pollination-score {:tags []})))
    (is (= 3.0 (core/cross-pollination-score {:tags ["xpoll:project:a"]})))
    (is (= 6.0 (core/cross-pollination-score {:tags ["xpoll:project:a" "xpoll:project:b"]})))
    (is (= 9.0 (core/cross-pollination-score
                {:tags ["xpoll:project:a" "xpoll:project:b" "xpoll:project:c"]})))))

(deftest cross-pollination-candidate-test
  (testing "needs 2+ projects to be candidate"
    (is (false? (core/cross-pollination-candidate? {:tags [] :duration "short" :type "note"})))
    (is (false? (core/cross-pollination-candidate?
                 {:tags ["xpoll:project:a"] :duration "short" :type "note"})))
    (is (true? (core/cross-pollination-candidate?
                {:tags ["xpoll:project:a" "xpoll:project:b"] :duration "short" :type "note"}))))

  (testing "permanent entries are NOT candidates"
    (is (false? (core/cross-pollination-candidate?
                 {:tags ["xpoll:project:a" "xpoll:project:b"]
                  :duration "permanent" :type "note"}))))

  (testing "axioms are NOT candidates"
    (is (false? (core/cross-pollination-candidate?
                 {:tags ["xpoll:project:a" "xpoll:project:b"]
                  :duration "long" :type "axiom"}))))

  (testing "custom min-projects threshold"
    (is (false? (core/cross-pollination-candidate?
                 {:tags ["xpoll:project:a" "xpoll:project:b"] :duration "short" :type "note"}
                 {:min-projects 3})))
    (is (true? (core/cross-pollination-candidate?
                {:tags ["xpoll:project:a" "xpoll:project:b" "xpoll:project:c"]
                 :duration "short" :type "note"}
                {:min-projects 3})))))

(deftest cross-pollination-promotion-tiers-test
  (testing "0-1 projects = 0 tiers"
    (is (= 0 (core/cross-pollination-promotion-tiers {:tags []})))
    (is (= 0 (core/cross-pollination-promotion-tiers {:tags ["xpoll:project:a"]}))))

  (testing "2 projects = 1 tier"
    (is (= 1 (core/cross-pollination-promotion-tiers
              {:tags ["xpoll:project:a" "xpoll:project:b"]}))))

  (testing "3 projects = 2 tiers"
    (is (= 2 (core/cross-pollination-promotion-tiers
              {:tags ["xpoll:project:a" "xpoll:project:b" "xpoll:project:c"]}))))

  (testing "4+ projects = 4 tiers (promote to permanent)"
    (is (= 4 (core/cross-pollination-promotion-tiers
              {:tags ["xpoll:project:a" "xpoll:project:b"
                      "xpoll:project:c" "xpoll:project:d"]})))
    (is (= 4 (core/cross-pollination-promotion-tiers
              {:tags ["xpoll:project:a" "xpoll:project:b"
                      "xpoll:project:c" "xpoll:project:d" "xpoll:project:e"]})))))

(deftest should-promote-with-cross-pollination-test
  (testing "cross-pollination boost auto-detected from tags"
    (let [entry {:duration :ephemeral
                 :recalls []
                 :tags ["xpoll:project:a" "xpoll:project:b" "xpoll:project:c"]}
          result (core/should-promote? entry)]
      ;; 3 projects * 3.0 weight = 9.0 boost, threshold for ephemeral is 5.0
      (is (true? (:promote? result)))
      (is (= 9.0 (:cross-pollination-boost result)))
      (is (= :short (:next-duration result)))))

  (testing "cross-pollination boost combines with recall score"
    (let [entry {:duration :short
                 :recalls [{:context :explicit-reference :count 2}]
                 :tags ["xpoll:project:a" "xpoll:project:b"]}
          result (core/should-promote? entry)]
      ;; recall score: 2.0, xpoll boost: 6.0, total: 8.0
      ;; short->medium threshold: 10.0, so NOT promoted yet
      (is (false? (:promote? result)))
      (is (= 8.0 (:current-score result)))))

  (testing "explicit cross-pollination-boost overrides auto-detection"
    (let [entry {:duration :ephemeral
                 :recalls []
                 :tags ["xpoll:project:a" "xpoll:project:b"]}
          result (core/should-promote? entry {:cross-pollination-boost 20.0})]
      (is (true? (:promote? result)))
      (is (= 20.0 (:cross-pollination-boost result)))))

  (testing "no tags = no boost (backward compatible)"
    (let [entry {:duration :ephemeral :recalls [] :tags []}
          result (core/should-promote? entry)]
      (is (= 0.0 (:cross-pollination-boost result)))
      (is (false? (:promote? result)))))

  (testing "nil tags = no boost (backward compatible)"
    (let [entry {:duration :ephemeral :recalls []}
          result (core/should-promote? entry)]
      (is (= 0.0 (:cross-pollination-boost result)))
      (is (false? (:promote? result))))))
