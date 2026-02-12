(ns hive-mcp.agent.drone.kg-factory-test
  "Tests for per-drone KG factory.
   Validates: store creation, isolation, merge-back, cleanup."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.drone.kg-factory :as factory]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.protocols.kg :as kg]))

;;; ============================================================
;;; Fixtures
;;; ============================================================

(use-fixtures :each
  (fn [f]
    ;; Clean up drone stores before each test
    (factory/cleanup-all-drone-stores!)
    (f)
    (factory/cleanup-all-drone-stores!)))

;;; ============================================================
;;; Helpers
;;; ============================================================

(defn- make-test-edge
  "Create a test KG edge map."
  [id from to relation]
  {:kg-edge/id id
   :kg-edge/from from
   :kg-edge/to to
   :kg-edge/relation relation
   :kg-edge/confidence 0.9
   :kg-edge/created-by "test-drone"
   :kg-edge/source-type :automated})

;;; ============================================================
;;; create-drone-store
;;; ============================================================

(deftest create-drone-store-returns-kg-store
  (testing "create-drone-store returns an IKGStore implementation"
    (let [store (factory/create-drone-store "drone-test-1")]
      (is (some? store))
      (is (kg/kg-store? store)))))

(deftest create-drone-store-validates-drone-id
  (testing "create-drone-store throws on nil drone-id"
    (is (thrown? clojure.lang.ExceptionInfo
                (factory/create-drone-store nil))))
  (testing "create-drone-store throws on blank drone-id"
    (is (thrown? clojure.lang.ExceptionInfo
                (factory/create-drone-store ""))))
  (testing "create-drone-store throws on whitespace drone-id"
    (is (thrown? clojure.lang.ExceptionInfo
                (factory/create-drone-store "   ")))))

(deftest create-drone-store-registers-in-registry
  (testing "created store appears in active-drone-stores"
    (factory/create-drone-store "drone-reg-1")
    (is (contains? (factory/active-drone-stores) "drone-reg-1"))))

(deftest create-drone-store-connection-initialized
  (testing "store connection is eagerly initialized (ensure-conn! already called)"
    (let [store (factory/create-drone-store "drone-eager-1")]
      ;; Should be able to transact immediately without calling ensure-conn!
      (is (some? (kg/transact! store [(make-test-edge "e1" "m1" "m2" :implements)]))))))

;;; ============================================================
;;; Isolation
;;; ============================================================

(deftest drone-stores-are-isolated
  (testing "two drone stores don't share data"
    (let [store-a (factory/create-drone-store "drone-iso-a")
          store-b (factory/create-drone-store "drone-iso-b")]
      ;; Write to store-a
      (kg/transact! store-a [(make-test-edge "edge-a" "m1" "m2" :implements)])
      ;; store-b should be empty
      (let [edges-b (kg/query store-b '[:find ?e :where [?e :kg-edge/id _]])]
        (is (empty? edges-b) "store-b should have no edges"))
      ;; store-a should have one edge
      (let [edges-a (kg/query store-a '[:find ?e :where [?e :kg-edge/id _]])]
        (is (= 1 (count edges-a)) "store-a should have exactly one edge")))))

(deftest drone-store-isolated-from-global
  (testing "drone store is isolated from global store"
    (let [global-store (ds-store/create-store)
          _ (kg/ensure-conn! global-store)
          drone-store (factory/create-drone-store "drone-iso-global")]
      ;; Write to drone
      (kg/transact! drone-store [(make-test-edge "edge-drone" "m1" "m2" :refines)])
      ;; Global should be empty
      (let [global-edges (kg/query global-store '[:find ?e :where [?e :kg-edge/id _]])]
        (is (empty? global-edges) "global store should be unaffected by drone writes")))))

;;; ============================================================
;;; merge-drone-results!
;;; ============================================================

(deftest merge-drone-results-empty-store
  (testing "merging from empty drone store returns zero counts"
    (let [drone-store (factory/create-drone-store "drone-merge-empty")
          target-store (ds-store/create-store)
          _ (kg/ensure-conn! target-store)
          result (factory/merge-drone-results! drone-store target-store)]
      (is (= 0 (:edges-found result)))
      (is (= 0 (:edges-merged result)))
      (is (empty? (:errors result))))))

(deftest merge-drone-results-transfers-edges
  (testing "edges from drone store are merged into target store"
    (let [drone-store (factory/create-drone-store "drone-merge-1")
          target-store (ds-store/create-store)
          _ (kg/ensure-conn! target-store)]
      ;; Add edges to drone store
      (kg/transact! drone-store [(make-test-edge "edge-1" "mem-a" "mem-b" :implements)])
      (kg/transact! drone-store [(make-test-edge "edge-2" "mem-b" "mem-c" :refines)])
      ;; Merge
      (let [result (factory/merge-drone-results! drone-store target-store)]
        (is (= 2 (:edges-found result)))
        (is (= 2 (:edges-merged result)))
        (is (empty? (:errors result))))
      ;; Verify edges exist in target
      (let [target-edges (kg/query target-store '[:find ?id :where [?e :kg-edge/id ?id]])]
        (is (= #{["edge-1"] ["edge-2"]} target-edges))))))

(deftest merge-drone-results-preserves-edge-attributes
  (testing "merged edges retain all their attributes"
    (let [drone-store (factory/create-drone-store "drone-merge-attrs")
          target-store (ds-store/create-store)
          _ (kg/ensure-conn! target-store)]
      (kg/transact! drone-store [(make-test-edge "edge-attr" "from-1" "to-1" :depends-on)])
      (factory/merge-drone-results! drone-store target-store)
      ;; Pull the merged edge and check attributes
      (let [eid (kg/entid target-store [:kg-edge/id "edge-attr"])
            edge (kg/pull-entity target-store '[*] eid)]
        (is (= "from-1" (:kg-edge/from edge)))
        (is (= "to-1" (:kg-edge/to edge)))
        (is (= :depends-on (:kg-edge/relation edge)))
        (is (= 0.9 (:kg-edge/confidence edge)))))))

(deftest merge-drone-results-upserts-duplicates
  (testing "merging same edge-id twice upserts rather than duplicates"
    (let [drone-store (factory/create-drone-store "drone-merge-upsert")
          target-store (ds-store/create-store)
          _ (kg/ensure-conn! target-store)]
      ;; First merge
      (kg/transact! drone-store [(make-test-edge "edge-dup" "m1" "m2" :implements)])
      (factory/merge-drone-results! drone-store target-store)
      ;; Second merge (same edge-id)
      (factory/merge-drone-results! drone-store target-store)
      ;; Should still be just one edge with that ID
      (let [edges (kg/query target-store '[:find ?e :where [?e :kg-edge/id "edge-dup"]])]
        (is (= 1 (count edges)) "duplicate edge-id should be upserted, not duplicated")))))

;;; ============================================================
;;; Cleanup
;;; ============================================================

(deftest close-drone-store-removes-from-registry
  (testing "close-drone-store! removes store from registry"
    (factory/create-drone-store "drone-close-1")
    (is (contains? (factory/active-drone-stores) "drone-close-1"))
    (let [result (factory/close-drone-store! "drone-close-1")]
      (is (true? result))
      (is (not (contains? (factory/active-drone-stores) "drone-close-1"))))))

(deftest close-drone-store-returns-false-for-unknown
  (testing "close-drone-store! returns false for unknown drone-id"
    (is (false? (factory/close-drone-store! "nonexistent-drone")))))

(deftest close-drone-store-idempotent
  (testing "close-drone-store! is idempotent"
    (factory/create-drone-store "drone-idem-1")
    (is (true? (factory/close-drone-store! "drone-idem-1")))
    (is (false? (factory/close-drone-store! "drone-idem-1")))))

(deftest cleanup-all-drone-stores-clears-registry
  (testing "cleanup-all-drone-stores! removes all stores"
    (factory/create-drone-store "drone-cleanup-1")
    (factory/create-drone-store "drone-cleanup-2")
    (factory/create-drone-store "drone-cleanup-3")
    (is (= 3 (count (factory/active-drone-stores))))
    (let [cleaned (factory/cleanup-all-drone-stores!)]
      (is (= 3 cleaned))
      (is (empty? (factory/active-drone-stores))))))
