(ns hive-mcp.knowledge-graph.bootstrap-test
  "Tests for KG bootstrap edge inference.

   Tests pure inference functions (no Chroma/DataScript needed)
   and integration test with mocked backends."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.bootstrap :as bootstrap]))

;; =============================================================================
;; Test Data
;; =============================================================================

(def entry-a
  {:id "20260201213407-48e854c2"
   :type "axiom"
   :content "# Communication Abbreviations\nBracketed abbreviations for token-efficient communication."
   :tags ["axiom" "communication" "abbreviations" "vocabulary" "language"
          "catchup-priority" "agent:coordinator" "scope:project:hive-mcp"]})

(def entry-b
  {:id "20260201232534-7f5ce5fe"
   :type "axiom"
   :content "# Code Quality Principles\nAll projects MUST favor DDD, TDD, SOLID, CLARITY."
   :tags ["axiom" "code-quality" "DDD" "TDD" "SOLID" "CLARITY"
          "scope:global" "agent:coordinator"]})

(def entry-c
  {:id "20260203011547-61d0d62f"
   :type "axiom"
   :content "# Language Conventions Become Polysemantic\nShorthand conventions evolve through use."
   :tags ["axiom" "language" "conventions" "polysemy" "vocabulary" "meta"
          "agent:coordinator" "scope:project:hive-mcp"]})

(def entry-d-supersedes
  {:id "20260205174500-5ea20404"
   :type "convention"
   :content "This convention supersedes 20260201213407-48e854c2. Updated abbreviations."
   :tags ["convention" "communication" "abbreviations" "updated"
          "scope:project:hive-mcp"]})

(def entry-e-depends
  {:id "20260205191822-13c273bb"
   :type "decision"
   :content "This decision depends on 20260201232534-7f5ce5fe for code quality standards and builds on 20260203011547-61d0d62f."
   :tags ["decision" "architecture" "headless" "scaling"
          "scope:project:hive-mcp"]})

(def session-1
  {:id "20260204000000-aaaa0001"
   :type "note"
   :content "Session summary: worked on KG edges"
   :tags ["session-summary" "wrap" "scope:project:hive-mcp"]
   :created "2026-02-04T10:00:00"})

(def session-2
  {:id "20260204120000-aaaa0002"
   :type "note"
   :content "Session summary: continued KG work"
   :tags ["session-summary" "wrap" "scope:project:hive-mcp"]
   :created "2026-02-04T12:00:00"})

(def session-3
  {:id "20260204180000-aaaa0003"
   :type "note"
   :content "Session summary: finalized KG bootstrap"
   :tags ["session-wrap" "scope:project:hive-mcp"]
   :created "2026-02-04T18:00:00"})

;; =============================================================================
;; meaningful-tags tests
;; =============================================================================

(deftest meaningful-tags-filters-noise
  (testing "Removes scope: and agent: prefixed tags"
    (let [result (bootstrap/meaningful-tags
                  ["axiom" "language" "scope:project:hive-mcp" "agent:coordinator"])]
      (is (= #{"axiom" "language"} result))))

  (testing "Returns nil for nil input"
    (is (nil? (bootstrap/meaningful-tags nil))))

  (testing "Returns nil for empty input"
    (is (nil? (bootstrap/meaningful-tags []))))

  (testing "Removes known noise tags"
    (let [result (bootstrap/meaningful-tags
                  ["axiom" "catchup-priority" "permanent" "language"])]
      (is (= #{"axiom" "language"} result)))))

;; =============================================================================
;; tag-overlap-score tests
;; =============================================================================

(deftest tag-overlap-score-basic
  (testing "Returns nil when overlap < 3"
    (is (nil? (bootstrap/tag-overlap-score entry-a entry-b))))

  (testing "Returns score when overlap >= 3"
    (let [result (bootstrap/tag-overlap-score entry-a entry-c)]
      (is (some? result))
      (is (>= (:overlap result) 3))
      (is (> (:confidence result) 0.0))
      (is (<= (:confidence result) 1.0))
      (is (set? (:shared-tags result)))))

  (testing "Shared tags are correct for entry-a and entry-c"
    ;; entry-a meaningful: #{axiom communication abbreviations vocabulary language}
    ;; entry-c meaningful: #{axiom language conventions polysemy vocabulary meta}
    ;; shared: #{axiom language vocabulary} = 3
    (let [result (bootstrap/tag-overlap-score entry-a entry-c)]
      (is (= 3 (:overlap result)))
      (is (= #{"axiom" "language" "vocabulary"} (:shared-tags result)))))

  (testing "Confidence = overlap / max-tags"
    (let [result (bootstrap/tag-overlap-score entry-a entry-c)]
      ;; entry-a meaningful: 5 tags, entry-c meaningful: 6 tags, overlap: 3
      ;; confidence = 3/6 = 0.5
      (is (= 0.5 (:confidence result))))))

(deftest tag-overlap-score-symmetric
  (testing "Score is symmetric (a,b) = (b,a)"
    (let [score-ab (bootstrap/tag-overlap-score entry-a entry-c)
          score-ba (bootstrap/tag-overlap-score entry-c entry-a)]
      (is (= (:overlap score-ab) (:overlap score-ba)))
      (is (= (:confidence score-ab) (:confidence score-ba)))
      (is (= (:shared-tags score-ab) (:shared-tags score-ba))))))

;; =============================================================================
;; extract-referenced-ids tests
;; =============================================================================

(deftest extract-referenced-ids-basic
  (testing "Extracts entry IDs from content"
    (let [ids (bootstrap/extract-referenced-ids
               "See 20260201213407-48e854c2 and 20260201232534-7f5ce5fe")]
      (is (= #{"20260201213407-48e854c2" "20260201232534-7f5ce5fe"} ids))))

  (testing "Returns nil for nil content"
    (is (nil? (bootstrap/extract-referenced-ids nil))))

  (testing "Returns nil for blank content"
    (is (nil? (bootstrap/extract-referenced-ids ""))))

  (testing "Returns empty set for content without IDs"
    (is (empty? (bootstrap/extract-referenced-ids "No IDs here")))))

;; =============================================================================
;; infer-supersedes tests
;; =============================================================================

(deftest infer-supersedes-basic
  (testing "Detects 'supersedes' keyword and extracts target ID"
    (let [result (bootstrap/infer-supersedes entry-d-supersedes)]
      (is (some? result))
      (is (contains? result "20260201213407-48e854c2"))))

  (testing "Returns nil when no supersedes keyword"
    (is (nil? (bootstrap/infer-supersedes entry-a))))

  (testing "Handles 'replaces' as synonym"
    (let [entry {:id "test" :content "This replaces 20260201213407-48e854c2"}]
      (is (some? (bootstrap/infer-supersedes entry)))))

  (testing "Handles 'obsoletes' as synonym"
    (let [entry {:id "test" :content "This obsoletes 20260201213407-48e854c2"}]
      (is (some? (bootstrap/infer-supersedes entry))))))

;; =============================================================================
;; infer-depends-on tests
;; =============================================================================

(deftest infer-depends-on-basic
  (testing "Detects 'depends on' keyword and extracts target IDs"
    (let [result (bootstrap/infer-depends-on entry-e-depends)]
      (is (some? result))
      (is (contains? result "20260201232534-7f5ce5fe"))
      (is (contains? result "20260203011547-61d0d62f"))))

  (testing "Returns nil when no depends-on keyword"
    (is (nil? (bootstrap/infer-depends-on entry-a))))

  (testing "Handles 'requires' as synonym"
    (let [entry {:id "test" :content "This requires 20260201213407-48e854c2"}]
      (is (some? (bootstrap/infer-depends-on entry)))))

  (testing "Handles 'based on' as synonym"
    (let [entry {:id "test" :content "Based on 20260201213407-48e854c2 findings"}]
      (is (some? (bootstrap/infer-depends-on entry))))))

;; =============================================================================
;; session-summaries-chronological tests
;; =============================================================================

(deftest session-summaries-chronological-basic
  (testing "Filters and sorts session summaries"
    (let [entries [session-2 entry-a session-1 session-3 entry-b]
          result (bootstrap/session-summaries-chronological entries)]
      (is (= 3 (count result)))
      (is (= (:id session-1) (:id (first result))))
      (is (= (:id session-2) (:id (second result))))
      (is (= (:id session-3) (:id (nth result 2))))))

  (testing "Returns empty for entries with no session summaries"
    (let [result (bootstrap/session-summaries-chronological [entry-a entry-b])]
      (is (empty? result))))

  (testing "Recognizes session-wrap tag"
    (let [result (bootstrap/session-summaries-chronological [session-3])]
      (is (= 1 (count result))))))

;; =============================================================================
;; Integration test with mocked backends
;; =============================================================================

(deftest backfill-dry-run-test
  (testing "Dry run computes edges without creating them"
    (let [test-entries [entry-a entry-c entry-d-supersedes entry-e-depends
                        session-1 session-2 session-3]]
      (with-redefs [hive-mcp.chroma.core/query-entries
                    (fn [& {:keys [type limit]}]
                      (filter #(= type (:type %)) test-entries))
                    ;; Mock find-edge to always return nil (no existing edges)
                    hive-mcp.knowledge-graph.edges/find-edge
                    (fn [_ _ _] nil)]
        (let [result (bootstrap/backfill-edges-from-memory!
                      {:dry-run? true})]
          (is (pos? (:total-entries result)))
          (is (pos? (:edges-created result)))
          (is (map? (:by-relation result))))))))

(deftest backfill-respects-edge-limit
  (testing "Edge creation stops at edge-limit"
    (let [;; Create many entries with overlapping tags to generate lots of edges
          entries (for [i (range 20)]
                   {:id (str "2026020100000" (format "%01d" i) "-" (format "%08x" i))
                    :type "note"
                    :content (str "Entry " i)
                    :tags ["shared-a" "shared-b" "shared-c" "shared-d"
                           (str "unique-" i)]})]
      (with-redefs [hive-mcp.chroma.core/query-entries
                    (fn [& {:keys [type limit]}]
                      (filter #(= type (:type %)) entries))
                    hive-mcp.knowledge-graph.edges/find-edge
                    (fn [_ _ _] nil)]
        (let [result (bootstrap/backfill-edges-from-memory!
                      {:edge-limit 5 :dry-run? true})]
          (is (<= (:edges-created result) 5)))))))

(deftest backfill-idempotent-when-edges-exist
  (testing "Skips edges that already exist"
    (let [test-entries [entry-a entry-c]]
      (with-redefs [hive-mcp.chroma.core/query-entries
                    (fn [& {:keys [type limit]}]
                      (filter #(= type (:type %)) test-entries))
                    ;; Mock find-edge to always return truthy (edge exists)
                    hive-mcp.knowledge-graph.edges/find-edge
                    (fn [_ _ _] {:kg-edge/id "existing"})
                    hive-mcp.knowledge-graph.edges/add-edge!
                    (fn [_] (throw (ex-info "Should not be called" {})))]
        (let [result (bootstrap/backfill-edges-from-memory! {})]
          (is (zero? (:edges-created result)))
          (is (pos? (:edges-skipped result))))))))
