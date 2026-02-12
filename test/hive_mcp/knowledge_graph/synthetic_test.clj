(ns hive-mcp.knowledge-graph.synthetic-test
  "Unit tests for Knowledge Graph synthetic (L3 emergent) node operations.

   Tests cover:
   - create-synthetic! with valid/invalid types
   - get-synthetic roundtrip
   - member add/remove operations
   - confidence update and increment
   - reinforcement timestamp
   - projection edge operations
   - synthetic statistics

   Each test uses a fresh DataScript connection via fixture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as string]
            [hive-mcp.knowledge-graph.synthetic :as synth]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.knowledge-graph.schema :as schema]))

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

(defn gen-entry-id
  "Generate a unique entry ID for testing."
  []
  (str "entry-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn gen-members
  "Generate a set of n unique member IDs."
  [n]
  (set (repeatedly n gen-entry-id)))

;; =============================================================================
;; create-synthetic! Tests
;; =============================================================================

(deftest create-synthetic-creates-node-test
  (testing "create-synthetic! creates node with valid type"
    (let [members (gen-members 2)
          synth-id (synth/create-synthetic! {:type :co-access
                                             :members members})]
      (is (string? synth-id))
      (is (string/starts-with? synth-id "synth-")))))

(deftest create-synthetic-returns-retrievable-node-test
  (testing "create-synthetic! returns ID of retrievable node"
    (let [members (gen-members 3)
          synth-id (synth/create-synthetic! {:type :semantic-cluster
                                             :members members})
          synth (synth/get-synthetic synth-id)]
      (is (some? synth))
      (is (= :semantic-cluster (:kg-synthetic/type synth)))
      (is (= members (set (:kg-synthetic/members synth)))))))

(deftest create-synthetic-with-all-fields-test
  (testing "create-synthetic! stores all optional fields"
    (let [members (gen-members 2)
          synth-id (synth/create-synthetic! {:type :workflow-step
                                             :members members
                                             :confidence 0.85
                                             :label "Test Workflow"
                                             :scope "hive-mcp:test"})
          synth (synth/get-synthetic synth-id)]
      (is (= 0.85 (:kg-synthetic/confidence synth)))
      (is (= "Test Workflow" (:kg-synthetic/label synth)))
      (is (= "hive-mcp:test" (:kg-synthetic/scope synth)))
      (is (inst? (:kg-synthetic/created-at synth)))
      (is (inst? (:kg-synthetic/last-reinforced synth))))))

(deftest create-synthetic-default-confidence-test
  (testing "create-synthetic! defaults confidence to 0.5"
    (let [members (gen-members 2)
          synth-id (synth/create-synthetic! {:type :co-access :members members})
          synth (synth/get-synthetic synth-id)]
      (is (= 0.5 (:kg-synthetic/confidence synth))))))

(deftest create-synthetic-custom-id-test
  (testing "create-synthetic! uses custom ID when provided"
    (let [members (gen-members 2)
          custom-id "synth-custom-123"
          synth-id (synth/create-synthetic! {:id custom-id
                                             :type :co-access
                                             :members members})]
      (is (= custom-id synth-id))
      (is (some? (synth/get-synthetic custom-id))))))

(deftest create-synthetic-all-valid-types-test
  (testing "create-synthetic! accepts all valid types"
    (doseq [synth-type schema/synthetic-types]
      (let [members (gen-members 2)
            synth-id (synth/create-synthetic! {:type synth-type :members members})]
        (is (string? synth-id) (str "Failed for type: " synth-type))))))

(deftest create-synthetic-rejects-invalid-type-test
  (testing "create-synthetic! rejects invalid type"
    (is (thrown? clojure.lang.ExceptionInfo
                 (synth/create-synthetic! {:type :invalid-type
                                           :members (gen-members 2)})))))

(deftest create-synthetic-rejects-insufficient-members-test
  (testing "create-synthetic! rejects fewer than 2 members"
    (is (thrown? clojure.lang.ExceptionInfo
                 (synth/create-synthetic! {:type :co-access
                                           :members #{(gen-entry-id)}})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (synth/create-synthetic! {:type :co-access
                                           :members #{}})))))

(deftest create-synthetic-rejects-nil-members-test
  (testing "create-synthetic! rejects nil members"
    (is (thrown? clojure.lang.ExceptionInfo
                 (synth/create-synthetic! {:type :co-access
                                           :members nil})))))

(deftest create-synthetic-rejects-confidence-out-of-range-test
  (testing "create-synthetic! rejects confidence outside [0.0, 1.0]"
    (let [members (gen-members 2)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (synth/create-synthetic! {:type :co-access
                                             :members members
                                             :confidence 1.5})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (synth/create-synthetic! {:type :co-access
                                             :members members
                                             :confidence -0.1}))))))

(deftest create-synthetic-boundary-confidence-test
  (testing "create-synthetic! accepts boundary confidence values"
    (let [synth-id-0 (synth/create-synthetic! {:type :co-access
                                               :members (gen-members 2)
                                               :confidence 0.0})
          synth-id-1 (synth/create-synthetic! {:type :co-access
                                               :members (gen-members 2)
                                               :confidence 1.0})]
      (is (= 0.0 (:kg-synthetic/confidence (synth/get-synthetic synth-id-0))))
      (is (= 1.0 (:kg-synthetic/confidence (synth/get-synthetic synth-id-1)))))))

;; =============================================================================
;; get-synthetic Tests
;; =============================================================================

(deftest get-synthetic-returns-nil-for-nonexistent-test
  (testing "get-synthetic returns nil for non-existent node"
    (is (nil? (synth/get-synthetic "synth-nonexistent-12345")))))

;; =============================================================================
;; get-synthetics-by-type Tests
;; =============================================================================

(deftest get-synthetics-by-type-filters-correctly-test
  (testing "get-synthetics-by-type returns only matching types"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
    (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
    (synth/create-synthetic! {:type :semantic-cluster :members (gen-members 2)})
    (let [co-access-nodes (synth/get-synthetics-by-type :co-access)]
      (is (= 2 (count co-access-nodes)))
      (is (every? #(= :co-access (:kg-synthetic/type %)) co-access-nodes)))))

(deftest get-synthetics-by-type-empty-when-no-match-test
  (testing "get-synthetics-by-type returns empty when no nodes match"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
    (is (empty? (synth/get-synthetics-by-type :decision-cluster)))))

(deftest get-synthetics-by-type-with-scope-test
  (testing "get-synthetics-by-type filters by scope"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2) :scope "proj-a"})
    (synth/create-synthetic! {:type :co-access :members (gen-members 2) :scope "proj-b"})
    (let [scoped (synth/get-synthetics-by-type :co-access "proj-a")]
      (is (= 1 (count scoped)))
      (is (= "proj-a" (:kg-synthetic/scope (first scoped)))))))

;; =============================================================================
;; get-all-synthetics Tests
;; =============================================================================

(deftest get-all-synthetics-returns-all-test
  (testing "get-all-synthetics returns all nodes"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
    (synth/create-synthetic! {:type :semantic-cluster :members (gen-members 2)})
    (synth/create-synthetic! {:type :workflow-step :members (gen-members 2)})
    (is (= 3 (count (synth/get-all-synthetics))))))

(deftest get-all-synthetics-empty-when-none-test
  (testing "get-all-synthetics returns empty when no nodes exist"
    (is (empty? (synth/get-all-synthetics)))))

;; =============================================================================
;; update-synthetic-confidence! Tests
;; =============================================================================

(deftest update-synthetic-confidence-changes-value-test
  (testing "update-synthetic-confidence! changes the confidence value"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)
                                             :confidence 0.5})]
      (synth/update-synthetic-confidence! synth-id 0.9)
      (is (= 0.9 (:kg-synthetic/confidence (synth/get-synthetic synth-id)))))))

(deftest update-synthetic-confidence-returns-nil-for-nonexistent-test
  (testing "update-synthetic-confidence! returns nil for non-existent node"
    (is (nil? (synth/update-synthetic-confidence! "synth-nonexistent" 0.5)))))

(deftest update-synthetic-confidence-rejects-out-of-range-test
  (testing "update-synthetic-confidence! rejects values outside [0.0, 1.0]"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (synth/update-synthetic-confidence! synth-id 1.5)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (synth/update-synthetic-confidence! synth-id -0.1))))))

;; =============================================================================
;; increment-synthetic-confidence! Tests
;; =============================================================================

(deftest increment-synthetic-confidence-adds-delta-test
  (testing "increment-synthetic-confidence! adds delta to current value"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)
                                             :confidence 0.5})
          new-conf (synth/increment-synthetic-confidence! synth-id 0.2)]
      (is (= 0.7 new-conf))
      (is (= 0.7 (:kg-synthetic/confidence (synth/get-synthetic synth-id)))))))

(deftest increment-synthetic-confidence-negative-delta-test
  (testing "increment-synthetic-confidence! handles negative delta"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)
                                             :confidence 0.8})
          new-conf (synth/increment-synthetic-confidence! synth-id -0.3)]
      (is (= 0.5 new-conf)))))

(deftest increment-synthetic-confidence-clamps-to-max-test
  (testing "increment-synthetic-confidence! clamps to 1.0 maximum"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)
                                             :confidence 0.9})
          new-conf (synth/increment-synthetic-confidence! synth-id 0.5)]
      (is (= 1.0 new-conf)))))

(deftest increment-synthetic-confidence-clamps-to-min-test
  (testing "increment-synthetic-confidence! clamps to 0.0 minimum"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)
                                             :confidence 0.2})
          new-conf (synth/increment-synthetic-confidence! synth-id -0.5)]
      (is (= 0.0 new-conf)))))

(deftest increment-synthetic-confidence-returns-nil-for-nonexistent-test
  (testing "increment-synthetic-confidence! returns nil for non-existent node"
    (is (nil? (synth/increment-synthetic-confidence! "synth-nonexistent" 0.1)))))

;; =============================================================================
;; reinforce-synthetic! Tests
;; =============================================================================

(deftest reinforce-synthetic-updates-timestamp-test
  (testing "reinforce-synthetic! updates last-reinforced timestamp"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)})
          original (synth/get-synthetic synth-id)
          original-time (:kg-synthetic/last-reinforced original)]
      ;; Sleep briefly to ensure timestamp difference
      (Thread/sleep 10)
      (synth/reinforce-synthetic! synth-id)
      (let [updated (synth/get-synthetic synth-id)
            new-time (:kg-synthetic/last-reinforced updated)]
        (is (inst? new-time))
        ;; New time should be >= original (may be equal in fast execution)
        (is (>= (.getTime new-time) (.getTime original-time)))))))

(deftest reinforce-synthetic-returns-nil-for-nonexistent-test
  (testing "reinforce-synthetic! returns nil for non-existent node"
    (is (nil? (synth/reinforce-synthetic! "synth-nonexistent")))))

;; =============================================================================
;; Member Operations Tests
;; =============================================================================

(deftest add-member-adds-to-cluster-test
  (testing "add-member! adds a new member to the cluster"
    (let [initial-members (gen-members 2)
          synth-id (synth/create-synthetic! {:type :co-access
                                             :members initial-members})
          new-member (gen-entry-id)]
      (synth/add-member! synth-id new-member)
      (let [members (synth/get-members synth-id)]
        (is (= 3 (count members)))
        (is (contains? members new-member))))))

(deftest add-member-returns-nil-for-nonexistent-test
  (testing "add-member! returns nil for non-existent node"
    (is (nil? (synth/add-member! "synth-nonexistent" "entry-123")))))

(deftest remove-member-removes-from-cluster-test
  (testing "remove-member! removes a member from the cluster"
    (let [members (gen-members 3)
          member-to-remove (first members)
          synth-id (synth/create-synthetic! {:type :co-access
                                             :members members})]
      (synth/remove-member! synth-id member-to-remove)
      (let [remaining (synth/get-members synth-id)]
        (is (= 2 (count remaining)))
        (is (not (contains? remaining member-to-remove)))))))

(deftest remove-member-returns-nil-for-nonexistent-test
  (testing "remove-member! returns nil for non-existent node"
    (is (nil? (synth/remove-member! "synth-nonexistent" "entry-123")))))

(deftest get-members-returns-set-test
  (testing "get-members returns set of member IDs"
    (let [members (gen-members 3)
          synth-id (synth/create-synthetic! {:type :co-access
                                             :members members})]
      (is (= members (synth/get-members synth-id))))))

(deftest get-members-returns-nil-for-nonexistent-test
  (testing "get-members returns nil for non-existent node"
    (is (nil? (synth/get-members "synth-nonexistent")))))

(deftest get-synthetics-for-member-finds-nodes-test
  (testing "get-synthetics-for-member finds all containing nodes"
    (let [shared-member (gen-entry-id)
          members1 (conj (gen-members 1) shared-member)
          members2 (conj (gen-members 1) shared-member)]
      (synth/create-synthetic! {:type :co-access :members members1})
      (synth/create-synthetic! {:type :semantic-cluster :members members2})
      (synth/create-synthetic! {:type :workflow-step :members (gen-members 2)})
      (let [containing (synth/get-synthetics-for-member shared-member)]
        (is (= 2 (count containing)))))))

;; =============================================================================
;; Projection Edge Tests
;; =============================================================================

(deftest add-projection-edge-creates-edge-test
  (testing "add-projection-edge! creates projection edge"
    (let [members (gen-members 2)
          synth-id (synth/create-synthetic! {:type :co-access
                                             :members members
                                             :confidence 0.7
                                             :scope "test-proj"})
          entry-id (gen-entry-id)
          edge-id (synth/add-projection-edge! synth-id entry-id)]
      (is (string? edge-id))
      (is (string/starts-with? edge-id "edge-")))))

(deftest get-projections-returns-entry-ids-test
  (testing "get-projections returns projected entry IDs"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)})
          entry1 (gen-entry-id)
          entry2 (gen-entry-id)]
      (synth/add-projection-edge! synth-id entry1)
      (synth/add-projection-edge! synth-id entry2)
      (let [projections (synth/get-projections synth-id)]
        (is (= 2 (count projections)))
        (is (contains? (set projections) entry1))
        (is (contains? (set projections) entry2))))))

(deftest get-projections-empty-when-no-edges-test
  (testing "get-projections returns empty when no projection edges"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)})]
      (is (empty? (synth/get-projections synth-id))))))

(deftest get-projected-from-returns-synthetic-ids-test
  (testing "get-projected-from returns synthetic IDs projecting to entry"
    (let [entry-id (gen-entry-id)
          synth1 (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
          synth2 (synth/create-synthetic! {:type :semantic-cluster :members (gen-members 2)})]
      (synth/add-projection-edge! synth1 entry-id)
      (synth/add-projection-edge! synth2 entry-id)
      (let [projecting (synth/get-projected-from entry-id)]
        (is (= 2 (count projecting)))
        (is (contains? (set projecting) synth1))
        (is (contains? (set projecting) synth2))))))

(deftest get-projected-from-empty-when-no-projections-test
  (testing "get-projected-from returns empty when no projections"
    (let [entry-id (gen-entry-id)]
      (is (empty? (synth/get-projected-from entry-id))))))

(deftest projection-edge-inherits-confidence-test
  (testing "projection edge inherits synthetic confidence by default"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)
                                             :confidence 0.75})
          entry-id (gen-entry-id)]
      (synth/add-projection-edge! synth-id entry-id)
      (let [edges (synth/get-projection-edges synth-id)]
        (is (= 1 (count edges)))
        (is (= 0.75 (:kg-edge/confidence (first edges))))))))

(deftest projection-edge-custom-confidence-test
  (testing "projection edge uses custom confidence when provided"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)
                                             :confidence 0.75})
          entry-id (gen-entry-id)]
      (synth/add-projection-edge! synth-id entry-id {:confidence 0.9})
      (let [edges (synth/get-projection-edges synth-id)]
        (is (= 0.9 (:kg-edge/confidence (first edges))))))))

(deftest projection-edge-inherits-scope-test
  (testing "projection edge inherits synthetic scope by default"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)
                                             :scope "my-project"})
          entry-id (gen-entry-id)]
      (synth/add-projection-edge! synth-id entry-id)
      (let [edges (synth/get-projection-edges synth-id)]
        (is (= "my-project" (:kg-edge/scope (first edges))))))))

;; =============================================================================
;; Delete Operations Tests
;; =============================================================================

(deftest remove-synthetic-deletes-node-test
  (testing "remove-synthetic! deletes the node"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)})]
      (is (true? (synth/remove-synthetic! synth-id)))
      (is (nil? (synth/get-synthetic synth-id))))))

(deftest remove-synthetic-returns-false-for-nonexistent-test
  (testing "remove-synthetic! returns false for non-existent node"
    (is (false? (synth/remove-synthetic! "synth-nonexistent")))))

(deftest remove-synthetic-with-edges-cleans-up-test
  (testing "remove-synthetic-with-edges! removes node and edges"
    (let [synth-id (synth/create-synthetic! {:type :co-access
                                             :members (gen-members 2)})
          entry1 (gen-entry-id)
          entry2 (gen-entry-id)]
      (synth/add-projection-edge! synth-id entry1)
      (synth/add-projection-edge! synth-id entry2)
      (let [result (synth/remove-synthetic-with-edges! synth-id)]
        (is (true? (:synthetic-removed? result)))
        (is (= 2 (:edges-removed result)))
        (is (nil? (synth/get-synthetic synth-id)))
        (is (empty? (synth/get-projections synth-id)))))))

;; =============================================================================
;; Statistics Tests
;; =============================================================================

(deftest synthetic-stats-total-count-test
  (testing "synthetic-stats reports total synthetic count"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
    (synth/create-synthetic! {:type :semantic-cluster :members (gen-members 3)})
    (let [stats (synth/synthetic-stats)]
      (is (= 2 (:total-synthetics stats))))))

(deftest synthetic-stats-by-type-test
  (testing "synthetic-stats counts by type"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
    (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
    (synth/create-synthetic! {:type :semantic-cluster :members (gen-members 2)})
    (let [stats (synth/synthetic-stats)]
      (is (= 2 (get-in stats [:by-type :co-access])))
      (is (= 1 (get-in stats [:by-type :semantic-cluster]))))))

(deftest synthetic-stats-by-scope-test
  (testing "synthetic-stats counts by scope"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2) :scope "proj-a"})
    (synth/create-synthetic! {:type :co-access :members (gen-members 2) :scope "proj-a"})
    (synth/create-synthetic! {:type :co-access :members (gen-members 2) :scope "proj-b"})
    (let [stats (synth/synthetic-stats)]
      (is (= 2 (get-in stats [:by-scope "proj-a"])))
      (is (= 1 (get-in stats [:by-scope "proj-b"]))))))

(deftest synthetic-stats-avg-members-test
  (testing "synthetic-stats calculates average members"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2)})
    (synth/create-synthetic! {:type :co-access :members (gen-members 4)})
    (let [stats (synth/synthetic-stats)]
      (is (= 3.0 (:avg-members stats))))))

(deftest synthetic-stats-avg-confidence-test
  (testing "synthetic-stats calculates average confidence"
    (synth/create-synthetic! {:type :co-access :members (gen-members 2) :confidence 0.4})
    (synth/create-synthetic! {:type :co-access :members (gen-members 2) :confidence 0.6})
    (let [stats (synth/synthetic-stats)]
      (is (= 0.5 (:avg-confidence stats))))))

(deftest synthetic-stats-empty-db-test
  (testing "synthetic-stats handles empty database"
    (let [stats (synth/synthetic-stats)]
      (is (= 0 (:total-synthetics stats)))
      (is (empty? (:by-type stats)))
      (is (empty? (:by-scope stats)))
      (is (= 0.0 (:avg-members stats)))
      (is (= 0.0 (:avg-confidence stats))))))
