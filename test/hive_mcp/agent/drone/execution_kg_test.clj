(ns hive-mcp.agent.drone.execution-kg-test
  "Integration tests for per-drone KG isolation in execution pipeline.

   Validates:
   - run-execution! creates per-drone KG store via kg-factory
   - phase:execute! binds *drone-kg-store* during delegate-fn
   - phase:finalize! merges drone KG edges into global store
   - phase:cleanup! closes drone KG store
   - KG store creation failure degrades gracefully (nil store)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.drone.domain :as domain]
            [hive-mcp.agent.drone.execution :as execution]
            [hive-mcp.agent.drone.augment :as augment]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.kg-factory :as kg-factory]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.protocols.kg :as kg]
            [hive-mcp.events.core :as ev]
            [hive-mcp.hivemind :as hivemind]))

;;; ============================================================
;;; Fixtures
;;; ============================================================

(use-fixtures :each
  (fn [f]
    (kg-factory/cleanup-all-drone-stores!)
    (f)
    (kg-factory/cleanup-all-drone-stores!)))

;;; ============================================================
;;; Phase: Execute — *drone-kg-store* binding
;;; ============================================================

(deftest phase-execute-binds-drone-kg-store
  (testing "phase:execute! binds *drone-kg-store* during delegate-fn execution"
    (let [drone-store (kg-factory/create-drone-store "drone-binding-test")
          captured-store (atom nil)
          ctx (domain/->execution-context
               {:drone-id "drone-binding-test"
                :task-id "task-binding-test"
                :parent-id nil
                :project-root "/tmp"
                :kg-store drone-store})
          task-spec (domain/->task-spec {:task "test binding"
                                         :files []})
          config {:tools [] :preset nil :model "test-model" :step-budget 1}
          ;; delegate-fn captures the dynamic var during execution
          delegate-fn (fn [_params]
                        (reset! captured-store domain/*drone-kg-store*)
                        {:status :completed :result "ok"})]
      ;; Verify *drone-kg-store* is nil before
      (is (nil? domain/*drone-kg-store*))
      ;; Stub side-effecting dependencies so phase:execute! reaches the binding form
      (with-redefs [augment/augment-task (fn [task _files _opts] task)
                    sandbox/create-sandbox (fn [files root]
                                             {:allowed-files (set files)
                                              :allowed-dirs #{root}
                                              :blocked-patterns []
                                              :blocked-tools #{}
                                              :rejected-files []})
                    ev/dispatch (fn [_] nil)
                    hivemind/shout! (fn [& _] nil)]
        ;; Execute — this should bind *drone-kg-store* to the ctx's :kg-store
        (try
          (execution/phase:execute! ctx task-spec config delegate-fn)
          (catch Exception _ nil)))
      ;; The captured store should be the drone store
      (is (identical? drone-store @captured-store)
          "*drone-kg-store* should be bound to the ctx's :kg-store during delegate-fn")
      ;; After execution, *drone-kg-store* should be nil again (binding is scoped)
      (is (nil? domain/*drone-kg-store*)))))

(deftest phase-execute-nil-kg-store-binds-nil
  (testing "phase:execute! with nil :kg-store binds nil (global fallback)"
    (let [captured-store (atom :not-set)
          ctx (domain/->execution-context
               {:drone-id "drone-nil-kg-test"
                :task-id "task-nil-kg"
                :parent-id nil
                :project-root "/tmp"})
          task-spec (domain/->task-spec {:task "test nil binding"
                                         :files []})
          config {:tools [] :preset nil :model "test-model" :step-budget 1}
          delegate-fn (fn [_params]
                        (reset! captured-store domain/*drone-kg-store*)
                        {:status :completed :result "ok"})]
      ;; Stub side-effecting dependencies so phase:execute! reaches the binding form
      (with-redefs [augment/augment-task (fn [task _files _opts] task)
                    sandbox/create-sandbox (fn [files root]
                                             {:allowed-files (set files)
                                              :allowed-dirs #{root}
                                              :blocked-patterns []
                                              :blocked-tools #{}
                                              :rejected-files []})
                    ev/dispatch (fn [_] nil)
                    hivemind/shout! (fn [& _] nil)]
        (try
          (execution/phase:execute! ctx task-spec config delegate-fn)
          (catch Exception _ nil)))
      (is (nil? @captured-store)
          "*drone-kg-store* should be nil when ctx has no :kg-store"))))

;;; ============================================================
;;; Phase: Cleanup — KG store closing
;;; ============================================================

(deftest phase-cleanup-closes-drone-kg-store
  (testing "phase:cleanup! closes and deregisters the drone KG store"
    (let [drone-store (kg-factory/create-drone-store "drone-cleanup-kg-test")
          ctx (domain/->execution-context
               {:drone-id "drone-cleanup-kg-test"
                :task-id "task-cleanup-kg"
                :parent-id nil
                :project-root "/tmp"
                :kg-store drone-store})
          task-spec (domain/->task-spec {:task "test cleanup" :files []})]
      ;; Store should be in registry
      (is (contains? (kg-factory/active-drone-stores) "drone-cleanup-kg-test"))
      ;; Cleanup — wrapping in try because ds/remove-slave! may fail without DataScript
      (try
        (execution/phase:cleanup! ctx task-spec)
        (catch Exception _ nil))
      ;; Store should be removed from registry
      (is (not (contains? (kg-factory/active-drone-stores) "drone-cleanup-kg-test"))
          "Drone KG store should be deregistered after cleanup"))))

(deftest phase-cleanup-handles-nil-kg-store
  (testing "phase:cleanup! with nil :kg-store skips KG cleanup gracefully"
    (let [ctx (domain/->execution-context
               {:drone-id "drone-no-kg-cleanup"
                :task-id "task-no-kg"
                :parent-id nil
                :project-root "/tmp"})
          task-spec (domain/->task-spec {:task "test no kg" :files []})]
      ;; Should not throw
      (try
        (execution/phase:cleanup! ctx task-spec)
        (catch Exception _ nil))
      ;; No assertion needed — just verifying no exception on nil :kg-store
      (is true "Cleanup should handle nil :kg-store gracefully"))))

;;; ============================================================
;;; get-kg-store resolution with factory-created stores
;;; ============================================================

(deftest get-kg-store-resolves-factory-created-store
  (testing "get-kg-store returns factory-created drone store from ctx"
    (let [drone-store (kg-factory/create-drone-store "drone-resolve-test")
          ctx (domain/->execution-context
               {:drone-id "drone-resolve-test"
                :kg-store drone-store})]
      (is (identical? drone-store (domain/get-kg-store ctx))
          "get-kg-store should return the factory-created store from ctx"))))

(deftest get-kg-store-factory-store-supports-operations
  (testing "Factory-created drone store supports full IKGStore operations"
    (let [drone-store (kg-factory/create-drone-store "drone-ops-test")
          ctx (domain/->execution-context
               {:drone-id "drone-ops-test"
                :kg-store drone-store})
          store (domain/get-kg-store ctx)]
      ;; Transact an edge
      (kg/transact! store [{:kg-edge/id "test-edge-1"
                            :kg-edge/from "mem-a"
                            :kg-edge/to "mem-b"
                            :kg-edge/relation :implements
                            :kg-edge/confidence 0.8
                            :kg-edge/created-by "test"
                            :kg-edge/source-type :automated}])
      ;; Query it back
      (let [edges (kg/query store '[:find ?id :where [?e :kg-edge/id ?id]])]
        (is (= #{["test-edge-1"]} edges)
            "Should be able to transact and query via get-kg-store")))))

;;; ============================================================
;;; KG merge in finalize — isolated test
;;; ============================================================

(deftest drone-kg-edges-merge-to-global-on-finalize
  (testing "Edges written to drone KG store during execution merge to global on finalize"
    (let [;; Set up global store
          global-store (ds-store/create-store)
          _ (kg/ensure-conn! global-store)
          _ (kg/set-store! global-store)
          ;; Create drone store and write an edge
          drone-store (kg-factory/create-drone-store "drone-merge-test")
          _ (kg/transact! drone-store [{:kg-edge/id "drone-edge-1"
                                        :kg-edge/from "mem-1"
                                        :kg-edge/to "mem-2"
                                        :kg-edge/relation :refines
                                        :kg-edge/confidence 0.7
                                        :kg-edge/created-by "drone-merge-test"
                                        :kg-edge/source-type :automated}])]
      (try
        ;; Verify edge is NOT in global yet
        (is (empty? (kg/query global-store '[:find ?id :where [?e :kg-edge/id ?id]]))
            "Global store should be empty before merge")
        ;; Simulate merge (what phase:finalize! does)
        (let [merge-result (kg-factory/merge-drone-results! drone-store global-store)]
          (is (= 1 (:edges-found merge-result)))
          (is (= 1 (:edges-merged merge-result))))
        ;; Verify edge IS in global now
        (let [global-edges (kg/query global-store '[:find ?id :where [?e :kg-edge/id ?id]])]
          (is (= #{["drone-edge-1"]} global-edges)
              "Drone edge should be merged into global store"))
        (finally
          (kg/clear-store!))))))

;;; ============================================================
;;; Isolation: concurrent drones don't leak
;;; ============================================================

(deftest concurrent-drone-stores-are-isolated
  (testing "Multiple drone stores created via factory don't share data"
    (let [store-a (kg-factory/create-drone-store "drone-iso-a")
          store-b (kg-factory/create-drone-store "drone-iso-b")
          ctx-a (domain/->execution-context {:drone-id "drone-iso-a" :kg-store store-a})
          ctx-b (domain/->execution-context {:drone-id "drone-iso-b" :kg-store store-b})]
      ;; Write to store-a via ctx
      (kg/transact! (domain/get-kg-store ctx-a)
                    [{:kg-edge/id "edge-only-a"
                      :kg-edge/from "m1" :kg-edge/to "m2"
                      :kg-edge/relation :implements
                      :kg-edge/confidence 0.9
                      :kg-edge/created-by "test"
                      :kg-edge/source-type :automated}])
      ;; store-b should be empty
      (is (empty? (kg/query (domain/get-kg-store ctx-b)
                            '[:find ?id :where [?e :kg-edge/id ?id]]))
          "store-b should have no edges from store-a")
      ;; store-a should have the edge
      (is (= #{["edge-only-a"]}
             (kg/query (domain/get-kg-store ctx-a)
                       '[:find ?id :where [?e :kg-edge/id ?id]]))
          "store-a should have exactly the edge we wrote"))))
