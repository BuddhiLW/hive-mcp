(ns hive-mcp.hooks-test
  (:require [clojure.test :refer :all]
            [hive-mcp.hooks :as hooks]))

;; =============================================================================
;; Hook Events Definition Tests
;; =============================================================================

(deftest hook-events-defined
  (testing "Core hook events are defined"
    (is (contains? hooks/hook-events :task-complete))
    (is (contains? hooks/hook-events :session-end)))

  (testing "Additional workflow events exist"
    (is (contains? hooks/hook-events :task-start))
    (is (contains? hooks/hook-events :error-occurred))
    (is (contains? hooks/hook-events :file-changed))))

(deftest hook-events-is-set
  (testing "hook-events is a set for O(1) lookup"
    (is (set? hooks/hook-events))))

;; =============================================================================
;; Registry Creation Tests
;; =============================================================================

(deftest create-registry-returns-atom
  (testing "create-registry returns an atom containing a map"
    (let [registry (hooks/create-registry)]
      (is (instance? clojure.lang.Atom registry))
      (is (map? @registry)))))

(deftest create-registry-initializes-empty-hooks
  (testing "Registry initializes with empty vectors for each event"
    (let [registry (hooks/create-registry)]
      (doseq [event hooks/hook-events]
        (is (vector? (get @registry event)))
        (is (empty? (get @registry event)))))))

;; =============================================================================
;; Hook Registration Tests
;; =============================================================================

(deftest register-hook-adds-handler
  (testing "register-hook adds a handler function to the registry"
    (let [registry (hooks/create-registry)
          handler (fn [ctx] :handled)]
      (hooks/register-hook registry :task-complete handler)
      (is (= 1 (count (get @registry :task-complete))))
      (is (= handler (first (get @registry :task-complete)))))))

(deftest register-hook-allows-multiple-handlers
  (testing "Multiple handlers can be registered for same event"
    (let [registry (hooks/create-registry)
          handler1 (fn [ctx] :first)
          handler2 (fn [ctx] :second)]
      (hooks/register-hook registry :task-complete handler1)
      (hooks/register-hook registry :task-complete handler2)
      (is (= 2 (count (get @registry :task-complete)))))))

(deftest register-hook-validates-event-type
  (testing "register-hook throws on invalid event type"
    (let [registry (hooks/create-registry)]
      (is (thrown? Exception
                   (hooks/register-hook registry :invalid-event (fn [_] nil)))))))

(deftest register-hook-validates-handler-is-function
  (testing "register-hook throws when handler is not a function"
    (let [registry (hooks/create-registry)]
      (is (thrown? Exception
                   (hooks/register-hook registry :task-complete "not-a-function"))))))

;; =============================================================================
;; Hook Triggering Tests
;; =============================================================================

(deftest trigger-hooks-executes-handlers
  (testing "trigger-hooks executes all registered handlers"
    (let [registry (hooks/create-registry)
          called (atom false)]
      (hooks/register-hook registry :task-complete (fn [_ctx] (reset! called true)))
      (hooks/trigger-hooks registry :task-complete {})
      (is @called))))

(deftest trigger-hooks-passes-context
  (testing "trigger-hooks passes context to handlers"
    (let [registry (hooks/create-registry)
          received-ctx (atom nil)]
      (hooks/register-hook registry :task-complete
                           (fn [_ctx] (reset! received-ctx _ctx)))
      (hooks/trigger-hooks registry :task-complete {:task-id "123" :result :success})
      (is (= {:task-id "123" :result :success} @received-ctx)))))

(deftest trigger-hooks-executes-in-order
  (testing "Handlers execute in registration order"
    (let [registry (hooks/create-registry)
          order (atom [])]
      (hooks/register-hook registry :task-complete (fn [_] (swap! order conj :first)))
      (hooks/register-hook registry :task-complete (fn [_] (swap! order conj :second)))
      (hooks/trigger-hooks registry :task-complete {})
      (is (= [:first :second] @order)))))

(deftest trigger-hooks-continues-on-handler-error
  (testing "Error in one handler doesn't stop others"
    (let [registry (hooks/create-registry)
          second-called (atom false)]
      (hooks/register-hook registry :task-complete
                           (fn [_] (throw (ex-info "Handler error" {}))))
      (hooks/register-hook registry :task-complete
                           (fn [_] (reset! second-called true)))
      (hooks/trigger-hooks registry :task-complete {})
      (is @second-called))))

(deftest trigger-hooks-returns-results
  (testing "trigger-hooks returns vector of results"
    (let [registry (hooks/create-registry)]
      (hooks/register-hook registry :task-complete (fn [_] :result-1))
      (hooks/register-hook registry :task-complete (fn [_] :result-2))
      (let [results (hooks/trigger-hooks registry :task-complete {})]
        (is (vector? results))
        (is (= 2 (count results)))))))

(deftest trigger-hooks-validates-event-type
  (testing "trigger-hooks throws on invalid event type"
    (let [registry (hooks/create-registry)]
      (is (thrown? Exception
                   (hooks/trigger-hooks registry :invalid-event {}))))))

