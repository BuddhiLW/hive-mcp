(ns hive-mcp.crystal.graph-test
  "Unit tests for crystal/graph.clj memory graph operations.

   Tests cover:
   - add-entry-fact! - adding memory entries to the graph
   - session-entries - querying entries by session
   - entry-lineage - tracking entry references
   - promotion-candidates - finding entries ready for promotion
   - orphaned-entries - finding entries without sessions

   Uses reset-db! to ensure test isolation.
   Does NOT test rebuild-from-memory! (requires Emacs connection).

   Architecture:
   - hive-mcp.crystal.graph provides the public API
   - hive-mcp.graph.datascript provides the underlying store
   - hive-mcp.crystal.core provides promotion scoring logic"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.crystal.graph :as graph]
            [hive-mcp.crystal.core :as crystal]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-graph-fixture
  "Fixture that resets the graph database before each test."
  [f]
  (graph/reset-db!)
  (try
    (f)
    (finally
      (graph/reset-db!))))

(use-fixtures :each reset-graph-fixture)

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn gen-test-id
  "Generate unique test ID."
  []
  (str "test-" (java.util.UUID/randomUUID)))

(defn make-test-entry
  "Create a test entry map.

   Defaults:
   - type: :note
   - duration: :short
   - tags: []
   - recalls: []"
  [& {:keys [id type duration tags recalls]
      :or {type :note
           duration :short
           tags []
           recalls []}}]
  {:id (or id (gen-test-id))
   :type type
   :duration duration
   :tags tags
   :recalls recalls})

(defn make-session-entry
  "Create a test entry with a session tag."
  [session-id & {:keys [id type duration]
                 :or {type :note duration :short}}]
  (make-test-entry
   :id (or id (gen-test-id))
   :type type
   :duration duration
   :tags [(str "session:" session-id)]))

;; =============================================================================
;; Test: add-entry-fact!
;; =============================================================================

(deftest test-add-entry-basic
  (testing "add-entry-fact! adds entry to graph"
    (let [entry (make-test-entry :id "entry-1" :type :note :duration :short)]
      (graph/add-entry-fact! entry)

      ;; Verify entry was added via stats
      (let [stats (graph/graph-stats)]
        (is (= 1 (:entries stats)))))))

(deftest test-add-entry-with-nil-id-skipped
  (testing "add-entry-fact! skips entries with nil id"
    (let [entry {:id nil :type :note :duration :short}]
      (graph/add-entry-fact! entry)

      ;; Should not add anything
      (let [stats (graph/graph-stats)]
        (is (= 0 (:entries stats)))))))

(deftest test-add-multiple-entries
  (testing "add-entry-fact! handles multiple entries"
    (dotimes [i 5]
      (graph/add-entry-fact! (make-test-entry :id (str "entry-" i))))

    (let [stats (graph/graph-stats)]
      (is (= 5 (:entries stats))))))

(deftest test-add-entry-with-session-tag
  (testing "add-entry-fact! extracts session from tags"
    (let [entry (make-session-entry "2026-01-11" :id "session-entry-1")]
      (graph/add-entry-fact! entry)

      (let [stats (graph/graph-stats)]
        (is (= 1 (:entries stats)))
        (is (= 1 (:sessions stats)))))))

(deftest test-add-entry-with-recalls
  (testing "add-entry-fact! handles recall tracking"
    (let [entry (make-test-entry
                 :id "recalled-entry"
                 :recalls [{:context :explicit-reference :count 2}
                           {:context :cross-session :count 1}])]
      (graph/add-entry-fact! entry)

      (let [stats (graph/graph-stats)]
        (is (= 1 (:entries stats)))
        (is (= 2 (:access-records stats)))))))

(deftest test-add-entry-types
  (testing "add-entry-fact! handles all memory types"
    (doseq [t [:note :snippet :convention :decision]]
      (graph/add-entry-fact! (make-test-entry :id (str "type-" (name t)) :type t)))

    (let [type-stats (graph/entries-by-type-stats)]
      (is (= 1 (:note type-stats)))
      (is (= 1 (:snippet type-stats)))
      (is (= 1 (:convention type-stats)))
      (is (= 1 (:decision type-stats))))))

;; =============================================================================
;; Test: session-entries
;; =============================================================================

(deftest test-session-entries-empty
  (testing "session-entries returns empty for non-existent session"
    (let [results (graph/session-entries "non-existent-session")]
      (is (empty? results)))))

(deftest test-session-entries-single
  (testing "session-entries finds entries for a session"
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "s1"))

    (let [results (graph/session-entries "2026-01-11")]
      (is (= 1 (count results)))
      (is (= "s1" (:id (first results)))))))

(deftest test-session-entries-multiple
  (testing "session-entries returns all entries for a session"
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "s1"))
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "s2"))
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "s3"))

    (let [results (graph/session-entries "2026-01-11")]
      (is (= 3 (count results)))
      (is (= #{"s1" "s2" "s3"} (set (map :id results)))))))

(deftest test-session-entries-isolation
  (testing "session-entries isolates by session"
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "jan11"))
    (graph/add-entry-fact! (make-session-entry "2026-01-12" :id "jan12"))

    (let [jan11-results (graph/session-entries "2026-01-11")
          jan12-results (graph/session-entries "2026-01-12")]
      (is (= 1 (count jan11-results)))
      (is (= "jan11" (:id (first jan11-results))))
      (is (= 1 (count jan12-results)))
      (is (= "jan12" (:id (first jan12-results)))))))

(deftest test-session-entries-with-type
  (testing "session-entries returns type information"
    (graph/add-entry-fact! (make-session-entry "2026-01-11"
                                               :id "typed-entry"
                                               :type :convention))

    (let [results (graph/session-entries "2026-01-11")]
      (is (= :convention (:type (first results)))))))

;; =============================================================================
;; Test: entry-lineage (reverse references)
;; =============================================================================

(deftest test-entry-lineage-empty
  (testing "entry-lineage returns empty when no references exist"
    (graph/add-entry-fact! (make-test-entry :id "lonely-entry"))

    (let [results (graph/entry-lineage "lonely-entry")]
      (is (empty? results)))))

(deftest test-entry-lineage-single-reference
  (testing "entry-lineage finds entries that reference target"
    (graph/add-entry-fact! (make-test-entry :id "target"))
    (graph/add-entry-fact! (make-test-entry :id "referencer"))
    (graph/add-reference! "referencer" "target")

    (let [results (graph/entry-lineage "target")]
      ;; entry-lineage finds entries that reference the target (reverse lookup)
      ;; Since referencer -> target, querying lineage of target should return referencer
      (is (= 1 (count results)))
      (is (= "referencer" (:id (first results)))))))

(deftest test-entry-references-to
  (testing "entry-references-to finds forward references"
    (graph/add-entry-fact! (make-test-entry :id "source"))
    (graph/add-entry-fact! (make-test-entry :id "target"))
    (graph/add-reference! "source" "target")

    (let [results (graph/entry-references-to "source")]
      (is (= 1 (count results)))
      (is (= "target" (:id (first results)))))))

(deftest test-reference-bidirectional
  (testing "References can be queried in both directions"
    (graph/add-entry-fact! (make-test-entry :id "doc-a" :type :note))
    (graph/add-entry-fact! (make-test-entry :id "doc-b" :type :snippet))
    (graph/add-reference! "doc-a" "doc-b")

    ;; Forward: doc-a references doc-b
    (let [forward (graph/entry-references-to "doc-a")]
      (is (= 1 (count forward)))
      (is (= "doc-b" (:id (first forward)))))

    ;; Verify stats
    (let [stats (graph/graph-stats)]
      (is (= 1 (:references stats))))))

;; =============================================================================
;; Test: promotion-candidates
;; =============================================================================

(deftest test-promotion-candidates-empty
  (testing "promotion-candidates returns empty when no entries qualify"
    (let [results (graph/promotion-candidates)]
      (is (empty? results)))))

(deftest test-promotion-candidates-excludes-permanent
  (testing "promotion-candidates excludes permanent entries"
    (graph/add-entry-fact! (make-test-entry :id "perm" :duration :permanent
                                            :recalls [{:context :explicit-reference :count 10}]))

    (let [results (graph/promotion-candidates)]
      (is (empty? results)))))

(deftest test-promotion-candidates-with-recalls
  (testing "promotion-candidates finds entries meeting threshold"
    ;; Ephemeral entry with enough recalls to promote
    ;; Threshold for ephemeral->short is 5.0 (from crystal/core)
    ;; :explicit-reference weight is 1.0, so need count >= 5
    (graph/add-entry-fact! (make-test-entry
                            :id "promotable"
                            :duration :ephemeral
                            :recalls [{:context :explicit-reference :count 5}]))

    (let [results (graph/promotion-candidates)]
      (is (= 1 (count results)))
      (let [candidate (first results)]
        (is (= "promotable" (:id candidate)))
        (is (= :ephemeral (:current-duration candidate)))
        (is (= :short (:next-duration candidate)))
        (is (>= (:score candidate) 5.0))))))

(deftest test-promotion-candidates-below-threshold
  (testing "promotion-candidates excludes entries below threshold"
    ;; Entry with recalls below threshold
    (graph/add-entry-fact! (make-test-entry
                            :id "not-promotable"
                            :duration :ephemeral
                            :recalls [{:context :explicit-reference :count 2}]))

    (let [results (graph/promotion-candidates)]
      (is (empty? results)))))

(deftest test-promotion-candidates-sorted-by-score
  (testing "promotion-candidates are sorted by score descending"
    ;; Two promotable entries with different scores
    (graph/add-entry-fact! (make-test-entry
                            :id "high-score"
                            :duration :ephemeral
                            :recalls [{:context :explicit-reference :count 10}]))
    (graph/add-entry-fact! (make-test-entry
                            :id "low-score"
                            :duration :ephemeral
                            :recalls [{:context :explicit-reference :count 5}]))

    (let [results (graph/promotion-candidates)]
      (is (= 2 (count results)))
      (is (= "high-score" (:id (first results))))
      (is (= "low-score" (:id (second results))))
      (is (> (:score (first results)) (:score (second results)))))))

(deftest test-promotion-cross-session-weighting
  (testing "Cross-session recalls have higher weight for promotion"
    ;; :cross-session weight is 2.0, so count 3 = score 6.0 (above threshold)
    (graph/add-entry-fact! (make-test-entry
                            :id "cross-session-entry"
                            :duration :ephemeral
                            :recalls [{:context :cross-session :count 3}]))

    (let [results (graph/promotion-candidates)]
      (is (= 1 (count results)))
      (is (>= (:score (first results)) 5.0)))))

;; =============================================================================
;; Test: orphaned-entries
;; =============================================================================

(deftest test-orphaned-entries-empty
  (testing "orphaned-entries returns empty when all have sessions"
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "with-session"))

    (let [results (graph/orphaned-entries)]
      (is (empty? results)))))

(deftest test-orphaned-entries-finds-orphans
  (testing "orphaned-entries finds entries without session tags"
    (graph/add-entry-fact! (make-test-entry :id "orphan-1"))
    (graph/add-entry-fact! (make-test-entry :id "orphan-2"))

    (let [results (graph/orphaned-entries)]
      (is (= 2 (count results)))
      (is (= #{"orphan-1" "orphan-2"} (set (map :id results)))))))

(deftest test-orphaned-entries-mixed
  (testing "orphaned-entries distinguishes orphans from session entries"
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "has-session"))
    (graph/add-entry-fact! (make-test-entry :id "no-session"))

    (let [results (graph/orphaned-entries)]
      (is (= 1 (count results)))
      (is (= "no-session" (:id (first results)))))))

(deftest test-orphaned-entries-returns-type-info
  (testing "orphaned-entries returns type and duration"
    (graph/add-entry-fact! (make-test-entry :id "orphan" :type :convention :duration :long))

    (let [results (graph/orphaned-entries)
          orphan (first results)]
      (is (= :convention (:type orphan)))
      (is (= :long (:duration orphan))))))

;; =============================================================================
;; Test: Additional Query Functions
;; =============================================================================

(deftest test-entries-by-tag
  (testing "entries-by-tag finds entries with specific tag"
    (graph/add-entry-fact! (make-test-entry :id "tagged-1" :tags ["important"]))
    (graph/add-entry-fact! (make-test-entry :id "tagged-2" :tags ["important" "urgent"]))
    (graph/add-entry-fact! (make-test-entry :id "untagged"))

    (let [results (graph/entries-by-tag "important")]
      (is (= 2 (count results)))
      (is (= #{"tagged-1" "tagged-2"} (set (map :id results)))))))

(deftest test-entries-by-duration
  (testing "entries-by-duration finds entries with specific duration"
    (graph/add-entry-fact! (make-test-entry :id "short-1" :duration :short))
    (graph/add-entry-fact! (make-test-entry :id "short-2" :duration :short))
    (graph/add-entry-fact! (make-test-entry :id "long-1" :duration :long))

    (let [results (graph/entries-by-duration :short)]
      (is (= 2 (count results)))
      (is (= #{"short-1" "short-2"} (set (map :id results)))))))

(deftest test-entry-recall-summary
  (testing "entry-recall-summary returns recall breakdown"
    (graph/add-entry-fact! (make-test-entry
                            :id "recalled"
                            :recalls [{:context :explicit-reference :count 3}
                                      {:context :cross-session :count 2}]))

    (let [results (graph/entry-recall-summary "recalled")]
      (is (= 2 (count results)))
      (is (some #(= :explicit-reference (:context %)) results))
      (is (some #(= :cross-session (:context %)) results)))))

;; =============================================================================
;; Test: Mutation Functions
;; =============================================================================

(deftest test-update-entry-duration
  (testing "update-entry-duration! changes entry duration"
    (graph/add-entry-fact! (make-test-entry :id "to-update" :duration :short))
    (graph/update-entry-duration! "to-update" :long)

    (let [results (graph/entries-by-duration :long)]
      (is (= 1 (count results)))
      (is (= "to-update" (:id (first results)))))))

(deftest test-log-access-new
  (testing "log-access! creates new recall record"
    (graph/add-entry-fact! (make-test-entry :id "accessed"))
    (graph/log-access! "accessed" :explicit-reference)

    (let [recalls (graph/entry-recall-summary "accessed")]
      (is (= 1 (count recalls)))
      (is (= :explicit-reference (:context (first recalls))))
      (is (= 1 (:count (first recalls)))))))

(deftest test-log-access-increment
  (testing "log-access! increments existing recall count"
    (graph/add-entry-fact! (make-test-entry :id "multi-access"))
    (graph/log-access! "multi-access" :explicit-reference)
    (graph/log-access! "multi-access" :explicit-reference)
    (graph/log-access! "multi-access" :explicit-reference)

    (let [recalls (graph/entry-recall-summary "multi-access")
          recall (first (filter #(= :explicit-reference (:context %)) recalls))]
      (is (= 3 (:count recall))))))

;; =============================================================================
;; Test: Connected Entries (Graph Traversal)
;; =============================================================================

(deftest test-connected-entries-empty
  (testing "connected-entries returns empty for isolated entry"
    (graph/add-entry-fact! (make-test-entry :id "isolated"))

    (let [results (graph/connected-entries "isolated")]
      (is (empty? results)))))

(deftest test-connected-entries-direct
  (testing "connected-entries finds directly connected entries"
    (graph/add-entry-fact! (make-test-entry :id "root"))
    (graph/add-entry-fact! (make-test-entry :id "child"))
    (graph/add-reference! "root" "child")

    (let [results (graph/connected-entries "root" :max-depth 1)]
      (is (= 1 (count results)))
      (is (= "child" (:id (first results)))))))

;; =============================================================================
;; Test: Graph Statistics
;; =============================================================================

(deftest test-graph-stats-empty
  (testing "graph-stats works on empty graph"
    (let [stats (graph/graph-stats)]
      (is (= 0 (:entries stats)))
      (is (= 0 (:sessions stats)))
      (is (= 0 (:references stats)))
      (is (= 0 (:access-records stats))))))

(deftest test-graph-stats-populated
  (testing "graph-stats returns accurate counts"
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "e1"))
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "e2"))
    (graph/add-entry-fact! (make-session-entry "2026-01-12" :id "e3"))
    (graph/add-reference! "e1" "e2")

    (let [stats (graph/graph-stats)]
      (is (= 3 (:entries stats)))
      (is (= 2 (:sessions stats)))
      (is (= 1 (:references stats))))))

(deftest test-dump-graph
  (testing "dump-graph returns graph state for debugging"
    (graph/add-entry-fact! (make-session-entry "2026-01-11" :id "dump-test"))

    (let [dump (graph/dump-graph)]
      (is (contains? dump :entries))
      (is (contains? dump :sessions))
      (is (contains? dump :references))
      (is (contains? dump :accesses)))))

;; =============================================================================
;; Test: Reset Database
;; =============================================================================

(deftest test-reset-db
  (testing "reset-db! clears all data"
    ;; Add some data
    (graph/add-entry-fact! (make-test-entry :id "to-clear-1"))
    (graph/add-entry-fact! (make-test-entry :id "to-clear-2"))

    ;; Verify data exists
    (is (= 2 (:entries (graph/graph-stats))))

    ;; Reset
    (graph/reset-db!)

    ;; Verify cleared
    (is (= 0 (:entries (graph/graph-stats))))))
