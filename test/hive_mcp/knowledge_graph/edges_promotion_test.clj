(ns hive-mcp.knowledge-graph.edges-promotion-test
  "Unit tests for co-access → depends-on promotion logic (P1.6).

   Tests cover:
   - promote-co-access-edges! with various scenarios
   - Threshold filtering (below/above)
   - Idempotency (duplicate promotion prevention)
   - Scope filtering
   - Limit enforcement
   - Edge attribution (source-type, created-by)
   - Empty graph handling
   - Constants verification

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
;; Helper Functions
;; =============================================================================

(defn- gen-id
  "Generate a unique test node ID."
  []
  (str "test-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- create-co-access-edge!
  "Helper to create a co-access edge with specific confidence."
  [from to confidence & [{:keys [scope]}]]
  (edges/add-edge! (cond-> {:from from
                            :to to
                            :relation :co-accessed
                            :confidence confidence
                            :source-type :co-access}
                     scope (assoc :scope scope))))

;; =============================================================================
;; Constants Tests
;; =============================================================================

(deftest default-constants-test
  (testing "Default promotion constants are sensible"
    (is (= 0.7 edges/default-promotion-threshold)
        "Threshold requires ~5 co-accesses (0.3 + 4*0.1)")
    (is (= 0.5 edges/default-promoted-confidence)
        "Promoted confidence is lower than manual (1.0)")
    (is (= 20 edges/default-promotion-limit)
        "Limit bounds evaluation to 20 edges")))

;; =============================================================================
;; Empty Graph Tests
;; =============================================================================

(deftest promote-empty-graph-test
  (testing "Promotion on empty graph returns zero counts"
    (let [result (edges/promote-co-access-edges!)]
      (is (= 0 (:promoted result)))
      (is (= 0 (:skipped result)))
      (is (= 0 (:below result)))
      (is (= 0 (:evaluated result))))))

;; =============================================================================
;; Threshold Tests
;; =============================================================================

(deftest promote-below-threshold-test
  (testing "Co-access edges below threshold are not promoted"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.5)
      (let [result (edges/promote-co-access-edges!)]
        (is (= 0 (:promoted result)))
        (is (= 1 (:below result)))
        (is (= 1 (:evaluated result)))
        ;; No :depends-on edge should exist
        (is (nil? (edges/find-edge a b :depends-on)))))))

(deftest promote-at-threshold-test
  (testing "Co-access edge at exactly threshold gets promoted"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.7)
      (let [result (edges/promote-co-access-edges!)]
        (is (= 1 (:promoted result)))
        (is (= 0 (:below result)))
        ;; :depends-on edge should now exist
        (is (some? (edges/find-edge a b :depends-on)))))))

(deftest promote-above-threshold-test
  (testing "Co-access edge above threshold gets promoted"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.9)
      (let [result (edges/promote-co-access-edges!)]
        (is (= 1 (:promoted result)))
        (let [edge (edges/find-edge a b :depends-on)]
          (is (some? edge))
          (is (= :depends-on (:kg-edge/relation edge)))
          (is (= 0.5 (:kg-edge/confidence edge))
              "Promoted edge has default-promoted-confidence"))))))

(deftest promote-custom-threshold-test
  (testing "Custom threshold parameter is respected"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.4)
      ;; Default threshold (0.7) would skip this
      (let [result (edges/promote-co-access-edges! {:threshold 0.3})]
        (is (= 1 (:promoted result)))
        (is (some? (edges/find-edge a b :depends-on)))))))

;; =============================================================================
;; Idempotency Tests
;; =============================================================================

(deftest promote-idempotent-test
  (testing "Calling promote twice does not create duplicate :depends-on edges"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.8)
      ;; First promotion
      (let [r1 (edges/promote-co-access-edges!)]
        (is (= 1 (:promoted r1))))
      ;; Second promotion — should skip because :depends-on already exists
      (let [r2 (edges/promote-co-access-edges!)]
        (is (= 0 (:promoted r2)))
        (is (= 1 (:skipped r2))))
      ;; Only one :depends-on edge should exist
      (let [depends-edges (edges/get-edges-by-relation :depends-on)]
        (is (= 1 (count depends-edges)))))))

(deftest promote-idempotent-reverse-direction-test
  (testing "Skips promotion if :depends-on exists in reverse direction"
    (let [a (gen-id) b (gen-id)]
      ;; Co-access goes A→B
      (create-co-access-edge! a b 0.8)
      ;; But :depends-on exists B→A (reverse direction)
      (edges/add-edge! {:from b :to a :relation :depends-on :confidence 1.0})
      (let [result (edges/promote-co-access-edges!)]
        (is (= 0 (:promoted result)))
        (is (= 1 (:skipped result)))))))

;; =============================================================================
;; Multiple Edges Tests
;; =============================================================================

(deftest promote-multiple-edges-test
  (testing "Promotes multiple qualifying edges in one cycle"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)]
      (create-co-access-edge! a b 0.8)
      (create-co-access-edge! c d 0.9)
      (let [result (edges/promote-co-access-edges!)]
        (is (= 2 (:promoted result)))
        (is (some? (edges/find-edge a b :depends-on)))
        (is (some? (edges/find-edge c d :depends-on)))))))

(deftest promote-mixed-above-and-below-test
  (testing "Correctly handles mix of above and below threshold"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)
          e (gen-id) f (gen-id)]
      (create-co-access-edge! a b 0.8)  ;; above
      (create-co-access-edge! c d 0.3)  ;; below
      (create-co-access-edge! e f 0.9)  ;; above
      (let [result (edges/promote-co-access-edges!)]
        (is (= 2 (:promoted result)))
        (is (= 1 (:below result)))
        (is (= 3 (:evaluated result)))))))

;; =============================================================================
;; Limit Tests
;; =============================================================================

(deftest promote-respects-limit-test
  (testing "Only evaluates up to :limit edges"
    (let [pairs (repeatedly 5 #(vector (gen-id) (gen-id)))]
      ;; Create 5 high-confidence co-access edges
      (doseq [[a b] pairs]
        (create-co-access-edge! a b 0.9))
      ;; Limit to 3
      (let [result (edges/promote-co-access-edges! {:limit 3})]
        (is (= 3 (:evaluated result)))
        (is (= 3 (:promoted result)))))))

(deftest promote-highest-confidence-first-test
  (testing "Promotes highest confidence edges first when limited"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)
          e (gen-id) f (gen-id)]
      (create-co-access-edge! a b 0.7)  ;; lowest qualifying
      (create-co-access-edge! c d 0.9)  ;; highest
      (create-co-access-edge! e f 0.8)  ;; middle
      ;; Limit to 2 — should pick 0.9 and 0.8
      (let [result (edges/promote-co-access-edges! {:limit 2})]
        (is (= 2 (:promoted result)))
        (is (some? (edges/find-edge c d :depends-on)) "Highest confidence promoted")
        (is (some? (edges/find-edge e f :depends-on)) "Second highest promoted")
        (is (nil? (edges/find-edge a b :depends-on)) "Lowest was not evaluated")))))

;; =============================================================================
;; Attribution Tests
;; =============================================================================

(deftest promote-edge-has-inferred-source-type-test
  (testing "Promoted edge has :inferred source-type"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.8)
      (edges/promote-co-access-edges!)
      (let [edge (edges/find-edge a b :depends-on)]
        (is (= :inferred (:kg-edge/source-type edge)))))))

(deftest promote-edge-has-custom-created-by-test
  (testing "Promoted edge has custom :created-by when specified"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.8)
      (edges/promote-co-access-edges! {:created-by "agent:test-promoter"})
      (let [edge (edges/find-edge a b :depends-on)]
        (is (= "agent:test-promoter" (:kg-edge/created-by edge)))))))

(deftest promote-edge-has-custom-confidence-test
  (testing "Promoted edge uses custom confidence when specified"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.8)
      (edges/promote-co-access-edges! {:confidence 0.6})
      (let [edge (edges/find-edge a b :depends-on)]
        (is (= 0.6 (:kg-edge/confidence edge)))))))

;; =============================================================================
;; Scope Tests
;; =============================================================================

(deftest promote-with-scope-filter-test
  (testing "Scope parameter filters which co-access edges are considered"
    (let [a (gen-id) b (gen-id)
          c (gen-id) d (gen-id)]
      ;; One scoped, one unscoped
      (create-co-access-edge! a b 0.8 {:scope "proj-a"})
      (create-co-access-edge! c d 0.8)
      ;; Only promote scoped edges
      (let [result (edges/promote-co-access-edges! {:scope "proj-a"})]
        (is (= 1 (:promoted result)))
        (is (some? (edges/find-edge a b :depends-on)))
        (is (nil? (edges/find-edge c d :depends-on)))))))

(deftest promote-scope-propagated-to-new-edge-test
  (testing "Scope from options is propagated to the promoted :depends-on edge"
    (let [a (gen-id) b (gen-id)]
      (create-co-access-edge! a b 0.8 {:scope "proj-a"})
      (edges/promote-co-access-edges! {:scope "proj-a"})
      (let [edge (edges/find-edge a b :depends-on)]
        (is (= "proj-a" (:kg-edge/scope edge)))))))

;; =============================================================================
;; Co-access edge not modified tests
;; =============================================================================

(deftest promote-preserves-original-co-access-edge-test
  (testing "Promotion does not modify or remove the original co-access edge"
    (let [a (gen-id) b (gen-id)]
      (let [co-edge-id (create-co-access-edge! a b 0.8)]
        (edges/promote-co-access-edges!)
        ;; Original co-access edge still exists and is unchanged
        (let [co-edge (edges/get-edge co-edge-id)]
          (is (some? co-edge))
          (is (= :co-accessed (:kg-edge/relation co-edge)))
          (is (= 0.8 (:kg-edge/confidence co-edge))))
        ;; New :depends-on edge also exists
        (is (some? (edges/find-edge a b :depends-on)))))))

;; =============================================================================
;; Non-co-access edges are ignored
;; =============================================================================

(deftest promote-ignores-non-co-access-edges-test
  (testing "Only :co-accessed edges are considered for promotion"
    (let [a (gen-id) b (gen-id)]
      ;; Create a :refines edge with high confidence (should NOT be promoted)
      (edges/add-edge! {:from a :to b :relation :refines :confidence 0.9})
      (let [result (edges/promote-co-access-edges!)]
        (is (= 0 (:promoted result)))
        (is (= 0 (:evaluated result)))))))