(ns hive-mcp.agent.drone.domain-test
  "Tests for drone domain value objects.
   Focus: :kg-store field on ExecutionContext and get-kg-store resolution."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.drone.domain :as domain]
            [hive-mcp.protocols.kg :as kg]))

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(defn make-test-store
  "Create a NoopKGStore for testing. Each call returns a distinct instance."
  []
  (kg/->NoopKGStore))

(defn make-test-ctx
  "Create a minimal ExecutionContext for testing."
  ([] (make-test-ctx {}))
  ([overrides]
   (domain/->execution-context
    (merge {:drone-id "test-drone-1"}
           overrides))))

;;; ============================================================
;;; ExecutionContext :kg-store field
;;; ============================================================

(deftest execution-context-kg-store-default-nil
  (testing "ExecutionContext :kg-store defaults to nil when not provided"
    (let [ctx (make-test-ctx)]
      (is (nil? (:kg-store ctx)))
      (is (domain/execution-context? ctx)))))

(deftest execution-context-kg-store-explicit
  (testing "ExecutionContext accepts explicit :kg-store"
    (let [store (make-test-store)
          ctx (make-test-ctx {:kg-store store})]
      (is (= store (:kg-store ctx)))
      (is (kg/kg-store? (:kg-store ctx))))))

(deftest execution-context-kg-store-preserves-other-fields
  (testing ":kg-store doesn't interfere with other ExecutionContext fields"
    (let [store (make-test-store)
          ctx (make-test-ctx {:kg-store store
                              :parent-id "parent-1"
                              :project-root "/tmp/test"})]
      (is (= "test-drone-1" (:drone-id ctx)))
      (is (= "parent-1" (:parent-id ctx)))
      (is (= "/tmp/test" (:project-root ctx)))
      (is (= store (:kg-store ctx))))))

;;; ============================================================
;;; get-kg-store resolution
;;; ============================================================

(deftest get-kg-store-returns-ctx-store-when-set
  (testing "get-kg-store returns :kg-store from context when present"
    (let [store (make-test-store)
          ctx (make-test-ctx {:kg-store store})]
      (is (= store (domain/get-kg-store ctx))))))

(deftest get-kg-store-falls-back-to-dynamic-var
  (testing "get-kg-store uses *drone-kg-store* when ctx has nil :kg-store"
    (let [dynamic-store (make-test-store)
          ctx (make-test-ctx)]
      (binding [domain/*drone-kg-store* dynamic-store]
        (is (= dynamic-store (domain/get-kg-store ctx)))))))

(deftest get-kg-store-ctx-takes-precedence-over-dynamic
  (testing "ctx :kg-store takes precedence over *drone-kg-store*"
    (let [ctx-store (make-test-store)
          dynamic-store (make-test-store)
          ctx (make-test-ctx {:kg-store ctx-store})]
      (binding [domain/*drone-kg-store* dynamic-store]
        (is (identical? ctx-store (domain/get-kg-store ctx))
            "Should return the exact ctx-store instance")
        (is (not (identical? dynamic-store (domain/get-kg-store ctx)))
            "Should NOT return the dynamic-store instance")))))

(deftest get-kg-store-falls-back-to-global-when-all-nil
  (testing "get-kg-store falls back to global kg/get-store when ctx and dynamic are nil"
    (let [global-store (make-test-store)
          ctx (make-test-ctx)]
      ;; Set global store temporarily
      (kg/set-store! global-store)
      (try
        (is (= global-store (domain/get-kg-store ctx)))
        (finally
          (kg/clear-store!))))))

(deftest get-kg-store-nil-ctx-uses-dynamic
  (testing "get-kg-store with nil ctx falls back to dynamic var"
    (let [dynamic-store (make-test-store)]
      (binding [domain/*drone-kg-store* dynamic-store]
        (is (= dynamic-store (domain/get-kg-store nil)))))))

;;; ============================================================
;;; Dynamic var *drone-kg-store*
;;; ============================================================

(deftest drone-kg-store-dynamic-var-default-nil
  (testing "*drone-kg-store* defaults to nil"
    (is (nil? domain/*drone-kg-store*))))

(deftest drone-kg-store-binding-scoped
  (testing "*drone-kg-store* binding is thread-local and scoped"
    (let [store (make-test-store)]
      (is (nil? domain/*drone-kg-store*))
      (binding [domain/*drone-kg-store* store]
        (is (= store domain/*drone-kg-store*)))
      (is (nil? domain/*drone-kg-store*)))))

;;; ============================================================
;;; TaskSpec (existing behavior preserved)
;;; ============================================================

(deftest task-spec-creation
  (testing "TaskSpec creation still works correctly"
    (let [spec (domain/->task-spec {:task "test task"})]
      (is (domain/task-spec? spec))
      (is (= "test task" (:task spec))))))

(deftest task-spec-requires-task
  (testing "TaskSpec throws on blank task"
    (is (thrown? clojure.lang.ExceptionInfo
                 (domain/->task-spec {:task ""})))))
