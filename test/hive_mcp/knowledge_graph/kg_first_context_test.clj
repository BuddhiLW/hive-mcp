(ns hive-mcp.knowledge-graph.kg-first-context-test
  "Unit tests for kg-first-context — the Structural Differential function.

   Tests cover:
   - Files with fresh KG data → classified as :kg-known
   - Files with stale KG data → classified as :stale
   - Files with no KG data → classified as :needs-read
   - Mixed batches of files → correct classification
   - Edge cases: empty input, nil/blank paths, duplicates
   - Summary counts are accurate
   - Staleness threshold configuration
   - touch-disc! / read tracking integration

   Each test uses a fresh DataScript connection via fixture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(use-fixtures :each fixtures/datascript-fixture)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn gen-path
  "Generate a unique file path for testing."
  []
  (str "/test/kg-first/" (subs (str (java.util.UUID/randomUUID)) 0 8) ".clj"))

(defn add-fresh-disc!
  "Add a disc entity with fresh content hash and recent timestamps."
  [path]
  (disc/add-disc! {:path path
                   :content-hash (disc/compute-hash "test-content")
                   :analyzed-at (java.util.Date.)
                   :project-id "test-project"})
  ;; Mark as recently read
  (disc/update-disc! path {:disc/last-read-at (java.util.Date.)
                           :disc/read-count 3}))

(defn add-stale-disc!
  "Add a disc entity with a content hash that doesn't match the stored value
   (simulates file changed since last analysis) and old timestamps."
  [path]
  (let [old-date (java.util.Date. (- (System/currentTimeMillis)
                                     (* 40 24 60 60 1000)))] ; 40 days ago
    (disc/add-disc! {:path path
                     :content-hash "old-hash-that-wont-match-anything"
                     :analyzed-at old-date
                     :project-id "test-project"})
    (disc/update-disc! path {:disc/last-read-at old-date
                             :disc/read-count 1})))

;; =============================================================================
;; Basic Classification Tests
;; =============================================================================

(deftest missing-files-classified-as-needs-read-test
  (testing "Files with no disc entity are classified as :needs-read"
    (let [paths [(gen-path) (gen-path)]
          result (disc/kg-first-context paths)]
      (is (= 2 (count (:needs-read result))))
      (is (empty? (:kg-known result)))
      (is (empty? (:stale result))))))

(deftest fresh-disc-classified-as-kg-known-test
  (testing "Files with fresh disc data are classified as :kg-known"
    (let [path (gen-path)]
      (add-fresh-disc! path)
      ;; Fresh disc: recently analyzed, recently read, no hash mismatch
      ;; (file doesn't exist on disk → exists?=false → hash-mismatch?=false)
      ;; Score: 0.0 (no hash mismatch) + 0.0 (just read) + 0.0 (just analyzed) = 0.0
      (let [result (disc/kg-first-context [path])]
        (is (contains? (:kg-known result) path)
            "Fresh disc should be classified as :kg-known")
        (is (empty? (:needs-read result)))
        (is (empty? (:stale result)))))))

(deftest empty-input-returns-empty-result-test
  (testing "Empty input returns empty result"
    (let [result (disc/kg-first-context [])]
      (is (empty? (:kg-known result)))
      (is (empty? (:needs-read result)))
      (is (empty? (:stale result)))
      (is (= 0 (:total (:summary result)))))))

(deftest nil-input-returns-empty-result-test
  (testing "Nil input returns empty result"
    (let [result (disc/kg-first-context nil)]
      (is (empty? (:kg-known result)))
      (is (empty? (:needs-read result)))
      (is (empty? (:stale result))))))

(deftest stale-disc-classified-as-stale-test
  (testing "Files with stale disc data are classified as :stale"
    (let [path (gen-path)]
      (add-stale-disc! path)
      ;; Stale disc: old hash, 40 days since read
      ;; Score: 0.0 (file doesn't exist → no hash mismatch) + 0.5 (>30 days) + 0.0 = 0.5
      (let [result (disc/kg-first-context [path])]
        (is (contains? (set (:stale result)) path)
            "Stale disc should be classified as :stale")
        (is (empty? (:kg-known result)))
        (is (empty? (:needs-read result)))))))

(deftest mixed-classification-test
  (testing "Mixed batch: fresh, stale, and missing files classified correctly"
    (let [fresh-path (gen-path)
          stale-path (gen-path)
          missing-path (gen-path)]
      (add-fresh-disc! fresh-path)
      (add-stale-disc! stale-path)
      ;; missing-path has no disc entity
      (let [result (disc/kg-first-context [fresh-path stale-path missing-path])]
        (is (contains? (:kg-known result) fresh-path)
            "Fresh disc should be in :kg-known")
        (is (contains? (set (:stale result)) stale-path)
            "Stale disc should be in :stale")
        (is (contains? (set (:needs-read result)) missing-path)
            "Missing disc should be in :needs-read")
        (is (= 3 (:total (:summary result))))
        (is (= 1 (:known (:summary result))))
        (is (= 1 (:stale (:summary result))))
        (is (= 1 (:needs-read (:summary result))))))))

;; =============================================================================
;; Summary Accuracy Tests
;; =============================================================================

(deftest summary-counts-match-result-test
  (testing "Summary counts match the actual classified collections"
    (let [path1 (gen-path)
          path2 (gen-path)
          path3 (gen-path)
          ;; path1 = no disc (needs-read)
          ;; path2 = has disc (classification depends on staleness)
          ;; path3 = no disc (needs-read)
          _ (disc/add-disc! {:path path2 :content-hash "test" :project-id "test"})
          _ (disc/update-disc! path2 {:disc/last-read-at (java.util.Date.)
                                      :disc/read-count 1})
          result (disc/kg-first-context [path1 path2 path3])]
      (is (= (:total (:summary result))
             (+ (count (:kg-known result))
                (count (:needs-read result))
                (count (:stale result))))
          "Summary total = known + needs-read + stale"))))

;; =============================================================================
;; Duplicate & Edge Case Tests
;; =============================================================================

(deftest duplicate-paths-deduplicated-test
  (testing "Duplicate paths are deduplicated"
    (let [path (gen-path)
          result (disc/kg-first-context [path path path])]
      (is (= 1 (:total (:summary result)))
          "Duplicates should be removed"))))

(deftest blank-paths-filtered-test
  (testing "Blank and nil paths are filtered out"
    (let [valid-path (gen-path)
          result (disc/kg-first-context [nil "" "  " valid-path])]
      ;; nil and "" are filtered; "  " has content but is not empty
      ;; The valid-path and "  " should remain
      (is (<= (:total (:summary result)) 2)
          "Nil and empty-string paths should be filtered"))))

;; =============================================================================
;; Staleness Threshold Tests
;; =============================================================================

(deftest custom-staleness-threshold-test
  (testing "Custom staleness threshold changes classification"
    (let [path (gen-path)
          ;; 40 days ago → days-since-read > 30 → +0.5 staleness
          old-date (java.util.Date. (- (System/currentTimeMillis)
                                       (* 40 24 60 60 1000)))]
      ;; add-disc! always sets analyzed-at to now when nil, so never-analyzed?=false.
      ;; Set last-read-at to 40 days ago for score = 0.0 + 0.5 + 0.0 = 0.5
      (disc/add-disc! {:path path :content-hash "test"})
      (disc/update-disc! path {:disc/last-read-at old-date})
      ;; With default threshold 0.3, 0.5 > 0.3 → stale
      (let [result-default (disc/kg-first-context [path])]
        (is (contains? (set (:stale result-default)) path)
            "Should be stale with default threshold"))
      ;; With relaxed threshold 0.6, 0.5 <= 0.6 → fresh (kg-known)
      (let [result-relaxed (disc/kg-first-context [path]
                                                  {:staleness-threshold 0.6})]
        (is (contains? (:kg-known result-relaxed) path)
            "Should be kg-known with relaxed threshold")))))

;; =============================================================================
;; Result Structure Tests
;; =============================================================================

(deftest result-structure-test
  (testing "Result has correct top-level keys"
    (let [result (disc/kg-first-context [(gen-path)])]
      (is (contains? result :kg-known))
      (is (contains? result :needs-read))
      (is (contains? result :stale))
      (is (contains? result :summary)))))

(deftest kg-known-entry-structure-test
  (testing "kg-known entries have expected diagnostic fields"
    (let [path (gen-path)]
      ;; Create disc with all fields populated
      (disc/add-disc! {:path path
                       :content-hash "test"
                       :analyzed-at (java.util.Date.)
                       :project-id "test"})
      (disc/update-disc! path {:disc/last-read-at (java.util.Date.)
                               :disc/read-count 5})
      ;; Use very high threshold so it classifies as known
      (let [result (disc/kg-first-context [path] {:staleness-threshold 1.0})
            entry (get (:kg-known result) path)]
        (is (some? entry) "Entry should be in kg-known")
        (when entry
          (is (contains? entry :disc))
          (is (contains? entry :staleness-score))
          (is (contains? entry :days-since-read))
          (is (contains? entry :hash-mismatch?))
          (is (contains? entry :never-analyzed?))
          (is (contains? entry :read-count))
          (is (contains? entry :last-read-at))
          (is (= 5 (:read-count entry))))))))

(deftest needs-read-contains-only-paths-test
  (testing ":needs-read contains only path strings"
    (let [paths [(gen-path) (gen-path)]
          result (disc/kg-first-context paths)]
      (is (every? string? (:needs-read result))))))

(deftest stale-contains-only-paths-test
  (testing ":stale contains only path strings"
    (let [path (gen-path)]
      ;; Add disc with no analyzed-at and no last-read-at → stale
      (disc/add-disc! {:path path :content-hash "test"})
      (let [result (disc/kg-first-context [path])]
        (is (every? string? (:stale result)))))))

;; =============================================================================
;; Integration with touch-disc! (Read Tracking)
;; =============================================================================

(deftest touch-disc-affects-classification-test
  (testing "touch-disc! updates read tracking fields used by classification"
    (let [path (gen-path)]
      ;; Initially: no disc → needs-read
      (let [result1 (disc/kg-first-context [path])]
        (is (= [path] (:needs-read result1))))
      ;; After touch: disc exists with recent read
      (disc/touch-disc! path :project-id "test")
      ;; Now the disc exists but may still be stale depending on hash matching
      ;; The key test is that it's no longer :needs-read (it's either :kg-known or :stale)
      (let [result2 (disc/kg-first-context [path])]
        (is (empty? (:needs-read result2))
            "After touch-disc!, file should no longer be in needs-read")
        (is (= 1 (+ (count (:kg-known result2))
                    (count (:stale result2))))
            "File should be in either kg-known or stale after touch")))))
