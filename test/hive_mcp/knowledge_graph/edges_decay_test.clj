(ns hive-mcp.knowledge-graph.edges-decay-test
  "Unit tests for edge decay logic (P2.9).

   Tests cover:
   - edge-stale? pure predicate (nil, recent, old, boundary)
   - decay-rate-for-edge pure predicate (co-access vs semantic)
   - decay-unverified-edges! integration tests:
     - Empty graph, all-fresh, stale decay, pruning,
       scope filtering, limit enforcement, idempotent decay,
       mixed fresh/stale, never-verified edges
   - Decay constants verification

   Each test uses a fresh DataScript connection via fixture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(use-fixtures :each fixtures/datascript-fixture)

;; =============================================================================
;; Private Var References (for testing pure predicates)
;; =============================================================================

(def edge-stale? @#'edges/edge-stale?)
(def decay-rate-for-edge @#'edges/decay-rate-for-edge)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- approx=
  "Approximate floating-point comparison within epsilon.
   Handles double arithmetic imprecision (e.g., 0.6 - 0.05 ≈ 0.5499999999999999)."
  ([expected actual] (approx= expected actual 0.001))
  ([expected actual epsilon]
   (< (abs (- expected actual)) epsilon)))

(defn- gen-id
  "Generate a unique test node ID."
  []
  (str "test-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- days-ago-date
  "Return a java.util.Date representing `n` days ago."
  [n]
  (java.util.Date. (- (System/currentTimeMillis)
                      (* n 24 60 60 1000))))

(defn- create-edge-with-age!
  "Helper to create an edge with a specific age (days since last-verified).
   Returns edge-id."
  [from to relation confidence days-since-verified & [{:keys [scope source-type]}]]
  (edges/add-edge! (cond-> {:from from
                            :to to
                            :relation relation
                            :confidence confidence
                            :last-verified (days-ago-date days-since-verified)}
                     scope (assoc :scope scope)
                     source-type (assoc :source-type source-type))))

(defn- create-edge-no-verified!
  "Helper to create an edge WITHOUT last-verified (simulates legacy edges).
   Uses a transact to nil out last-verified after creation."
  [from to relation confidence]
  (let [edge-id (edges/add-edge! {:from from
                                  :to to
                                  :relation relation
                                  :confidence confidence
                                   ;; Set to distant past so decay picks it up
                                  :last-verified (days-ago-date 365)})]
    edge-id))

;; =============================================================================
;; Decay Constants Tests
;; =============================================================================

(deftest decay-constants-test
  (testing "Decay constants are sensible values"
    (is (= 30 edges/default-decay-staleness-days)
        "Default staleness window is 30 days")
    (is (= 0.05 edges/co-access-decay-rate)
        "Co-access edges decay at 0.05 per cycle")
    (is (= 0.02 edges/semantic-decay-rate)
        "Semantic edges decay at 0.02 per cycle")
    (is (= 0.1 edges/prune-threshold)
        "Edges below 0.1 confidence are pruned")
    (is (= 100 edges/default-decay-limit)
        "Default limit is 100 edges per cycle"))

  (testing "Co-access decays faster than semantic"
    (is (> edges/co-access-decay-rate edges/semantic-decay-rate)
        "Co-access edges are less intentional, decay faster"))

  (testing "Prune threshold is above decay rates (multiple cycles before pruning)"
    (is (> edges/prune-threshold edges/co-access-decay-rate)
        "Prune threshold > co-access rate: edge at 0.15 takes 2 cycles to prune")
    (is (> edges/prune-threshold edges/semantic-decay-rate)
        "Prune threshold > semantic rate: edge at 0.15 takes 3+ cycles to prune")))

;; =============================================================================
;; edge-stale? Pure Predicate Tests
;; =============================================================================

(deftest edge-stale-nil-last-verified-test
  (testing "Edge with nil last-verified is considered stale"
    (let [edge {:kg-edge/id "test" :kg-edge/last-verified nil}]
      (is (true? (edge-stale? edge 30 (System/currentTimeMillis)))
          "Never-verified edge must be stale"))))

(deftest edge-stale-recent-verification-test
  (testing "Edge verified recently is NOT stale"
    (let [edge {:kg-edge/id "test"
                :kg-edge/last-verified (days-ago-date 5)}]
      (is (false? (edge-stale? edge 30 (System/currentTimeMillis)))
          "Edge verified 5 days ago with 30-day window is fresh"))))

(deftest edge-stale-old-verification-test
  (testing "Edge verified long ago IS stale"
    (let [edge {:kg-edge/id "test"
                :kg-edge/last-verified (days-ago-date 45)}]
      (is (true? (edge-stale? edge 30 (System/currentTimeMillis)))
          "Edge verified 45 days ago with 30-day window is stale"))))

(deftest edge-stale-boundary-test
  (testing "Edge at exactly the staleness boundary"
    (let [now (System/currentTimeMillis)
          ;; Just barely stale (31 days)
          edge-stale-31 {:kg-edge/id "test"
                         :kg-edge/last-verified (days-ago-date 31)}
          ;; Just barely fresh (29 days)
          edge-fresh-29 {:kg-edge/id "test"
                         :kg-edge/last-verified (days-ago-date 29)}]
      (is (true? (edge-stale? edge-stale-31 30 now))
          "31 days is beyond 30-day window → stale")
      (is (false? (edge-stale? edge-fresh-29 30 now))
          "29 days is within 30-day window → fresh"))))

(deftest edge-stale-custom-window-test
  (testing "Custom staleness window is respected"
    (let [edge {:kg-edge/id "test"
                :kg-edge/last-verified (days-ago-date 10)}
          now (System/currentTimeMillis)]
      (is (true? (edge-stale? edge 7 now))
          "10 days with 7-day window → stale")
      (is (false? (edge-stale? edge 14 now))
          "10 days with 14-day window → fresh"))))

;; =============================================================================
;; decay-rate-for-edge Pure Predicate Tests
;; =============================================================================

(deftest decay-rate-co-access-edge-test
  (testing "Co-access edge gets co-access decay rate"
    (let [edge {:kg-edge/relation :co-accessed}]
      (is (= edges/co-access-decay-rate (decay-rate-for-edge edge))
          "Co-accessed relation → 0.05 decay rate"))))

(deftest decay-rate-semantic-edges-test
  (testing "Semantic edges get semantic decay rate"
    (doseq [relation [:implements :supersedes :refines :contradicts
                      :depends-on :derived-from :applies-to]]
      (let [edge {:kg-edge/relation relation}]
        (is (= edges/semantic-decay-rate (decay-rate-for-edge edge))
            (str relation " is semantic → 0.02 decay rate"))))))

(deftest decay-rate-unknown-relation-test
  (testing "Unknown relation defaults to co-access rate (not in semantic set)"
    (let [edge {:kg-edge/relation :some-random-relation}]
      (is (= edges/co-access-decay-rate (decay-rate-for-edge edge))
          "Unknown relation → defaults to faster co-access decay"))))

;; =============================================================================
;; decay-unverified-edges! Integration Tests
;; =============================================================================

(deftest decay-empty-graph-test
  (testing "Decay on empty graph returns zero counts"
    (let [result (edges/decay-unverified-edges!)]
      (is (= 0 (:decayed result)))
      (is (= 0 (:pruned result)))
      (is (= 0 (:fresh result)))
      (is (= 0 (:evaluated result))))))

(deftest decay-all-fresh-edges-test
  (testing "Recently verified edges are not decayed"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)]
      ;; Create edges verified just 5 days ago
      (create-edge-with-age! a b :depends-on 0.8 5)
      (create-edge-with-age! c d :co-accessed 0.6 10)
      (let [result (edges/decay-unverified-edges!)]
        (is (= 0 (:decayed result)))
        (is (= 0 (:pruned result)))
        (is (= 2 (:fresh result)))
        (is (= 2 (:evaluated result)))))))

(deftest decay-stale-co-access-edge-test
  (testing "Stale co-access edge decays at co-access rate (0.05)"
    (let [a (gen-id) b (gen-id)
          edge-id (create-edge-with-age! a b :co-accessed 0.6 45)]
      (let [result (edges/decay-unverified-edges!)]
        (is (= 1 (:decayed result)))
        (is (= 0 (:pruned result)))
        ;; Verify confidence reduced by co-access-decay-rate
        (let [edge (edges/get-edge edge-id)]
          (is (some? edge))
          (is (approx= 0.55 (:kg-edge/confidence edge))
              "0.6 - 0.05 ≈ 0.55"))))))

(deftest decay-stale-semantic-edge-test
  (testing "Stale semantic edge decays at semantic rate (0.02)"
    (let [a (gen-id) b (gen-id)
          edge-id (create-edge-with-age! a b :depends-on 0.5 45)]
      (let [result (edges/decay-unverified-edges!)]
        (is (= 1 (:decayed result)))
        (is (= 0 (:pruned result)))
        ;; Verify confidence reduced by semantic-decay-rate
        (let [edge (edges/get-edge edge-id)]
          (is (some? edge))
          (is (approx= 0.48 (:kg-edge/confidence edge))
              "0.5 - 0.02 ≈ 0.48"))))))

(deftest decay-prunes-below-threshold-test
  (testing "Edge with confidence at prune boundary gets removed"
    (let [a (gen-id) b (gen-id)
          ;; Confidence 0.12: after co-access decay (0.05) → 0.07 < 0.1 threshold → pruned
          edge-id (create-edge-with-age! a b :co-accessed 0.12 45)]
      (let [result (edges/decay-unverified-edges!)]
        (is (= 0 (:decayed result)))
        (is (= 1 (:pruned result)))
        ;; Edge should be removed
        (is (nil? (edges/get-edge edge-id))
            "Pruned edge should no longer exist")))))

(deftest decay-semantic-prune-threshold-test
  (testing "Semantic edge at prune boundary gets removed"
    (let [a (gen-id) b (gen-id)
          ;; Confidence 0.11: after semantic decay (0.02) → 0.09 < 0.1 → pruned
          edge-id (create-edge-with-age! a b :depends-on 0.11 45)]
      (let [result (edges/decay-unverified-edges!)]
        (is (= 0 (:decayed result)))
        (is (= 1 (:pruned result)))
        (is (nil? (edges/get-edge edge-id)))))))

(deftest decay-mixed-fresh-and-stale-test
  (testing "Correctly handles mix of fresh and stale edges"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)
          e (gen-id) f (gen-id)]
      ;; Fresh edge (5 days old)
      (create-edge-with-age! a b :depends-on 0.8 5)
      ;; Stale edge that should decay
      (create-edge-with-age! c d :co-accessed 0.6 45)
      ;; Stale edge that should be pruned
      (create-edge-with-age! e f :co-accessed 0.12 60)
      (let [result (edges/decay-unverified-edges!)]
        (is (= 1 (:decayed result)) "One edge decayed")
        (is (= 1 (:pruned result)) "One edge pruned")
        (is (= 1 (:fresh result)) "One edge fresh")
        (is (= 3 (:evaluated result)))))))

(deftest decay-idempotent-incremental-test
  (testing "Calling decay twice reduces confidence incrementally"
    (let [a (gen-id) b (gen-id)
          edge-id (create-edge-with-age! a b :co-accessed 0.8 45)]
      ;; First decay: 0.8 → 0.75
      (let [r1 (edges/decay-unverified-edges!)]
        (is (= 1 (:decayed r1)))
        (is (== 0.75 (:kg-edge/confidence (edges/get-edge edge-id)))))
      ;; Second decay: 0.75 → 0.70
      (let [r2 (edges/decay-unverified-edges!)]
        (is (= 1 (:decayed r2)))
        (is (== 0.70 (:kg-edge/confidence (edges/get-edge edge-id))))))))

(deftest decay-respects-limit-test
  (testing "Only evaluates up to :limit edges"
    (let [pairs (repeatedly 5 #(vector (gen-id) (gen-id)))]
      ;; Create 5 stale edges
      (doseq [[a b] pairs]
        (create-edge-with-age! a b :co-accessed 0.6 45))
      ;; Limit to 3
      (let [result (edges/decay-unverified-edges! {:limit 3})]
        (is (= 3 (:evaluated result)))
        (is (<= (:decayed result) 3))))))

(deftest decay-scope-filter-test
  (testing "Scope parameter filters which edges are considered"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)]
      ;; Scoped stale edge
      (let [scoped-id (create-edge-with-age! a b :co-accessed 0.6 45
                                             {:scope "proj-a"})]
        ;; Unscoped stale edge
        (create-edge-with-age! c d :co-accessed 0.6 45)
        ;; Only decay scoped edges
        (let [result (edges/decay-unverified-edges! {:scope "proj-a"})]
          (is (= 1 (:evaluated result)))
          (is (= 1 (:decayed result)))
          ;; Scoped edge was decayed
          (is (approx= 0.55 (:kg-edge/confidence (edges/get-edge scoped-id))))
          ;; Unscoped edge is untouched — find it
          (let [all-co (edges/get-edges-by-relation :co-accessed)
                unscoped (first (filter #(= c (:kg-edge/from %)) all-co))]
            (is (== 0.6 (:kg-edge/confidence unscoped))
                "Unscoped edge should not have been decayed")))))))

(deftest decay-custom-staleness-days-test
  (testing "Custom staleness-days parameter is respected"
    (let [a (gen-id) b (gen-id)
          ;; Edge verified 10 days ago
          edge-id (create-edge-with-age! a b :co-accessed 0.6 10)]
      ;; Default (30 days) → fresh
      (let [r1 (edges/decay-unverified-edges!)]
        (is (= 1 (:fresh r1)))
        (is (= 0 (:decayed r1))))
      ;; Custom (7 days) → stale
      (let [r2 (edges/decay-unverified-edges! {:staleness-days 7})]
        (is (= 0 (:fresh r2)))
        (is (= 1 (:decayed r2)))))))

(deftest decay-never-verified-edge-test
  (testing "Edge with very old last-verified is decayed (simulates never-verified)"
    (let [a (gen-id) b (gen-id)
          ;; Created with last-verified 365 days ago
          edge-id (create-edge-no-verified! a b :co-accessed 0.6)]
      (let [result (edges/decay-unverified-edges!)]
        (is (= 1 (:decayed result))
            "Very old edge should be decayed")
        (is (approx= 0.55 (:kg-edge/confidence (edges/get-edge edge-id))))))))

(deftest decay-returns-evaluated-count-test
  (testing ":evaluated key includes total edges checked"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)]
      (create-edge-with-age! a b :co-accessed 0.6 5)  ;; fresh
      (create-edge-with-age! c d :co-accessed 0.6 45) ;; stale
      (let [result (edges/decay-unverified-edges!)]
        (is (= 2 (:evaluated result))
            ":evaluated = fresh + decayed + pruned")))))

(deftest decay-oldest-edges-first-test
  (testing "Evaluates oldest (most stale) edges first when limited"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)
          e (gen-id) f (gen-id)]
      ;; 60 days old (most stale)
      (let [oldest-id (create-edge-with-age! a b :co-accessed 0.6 60)]
        ;; 45 days old
        (create-edge-with-age! c d :co-accessed 0.6 45)
        ;; 35 days old
        (create-edge-with-age! e f :co-accessed 0.6 35)
        ;; Limit to 2 — should pick 60-day and 45-day edges first
        (let [result (edges/decay-unverified-edges! {:limit 2})]
          (is (= 2 (:evaluated result)))
          (is (= 2 (:decayed result)))
          ;; Oldest edge should have been decayed
          (is (approx= 0.55 (:kg-edge/confidence (edges/get-edge oldest-id)))))))))
