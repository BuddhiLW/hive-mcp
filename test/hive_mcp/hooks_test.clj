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
      (hooks/register-hook registry :task-complete (fn [ctx] (reset! called true)))
      (hooks/trigger-hooks registry :task-complete {})
      (is @called))))

(deftest trigger-hooks-passes-context
  (testing "trigger-hooks passes context to handlers"
    (let [registry (hooks/create-registry)
          received-ctx (atom nil)]
      (hooks/register-hook registry :task-complete
                           (fn [ctx] (reset! received-ctx ctx)))
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

;; =============================================================================
;; List Hooks Tests
;; =============================================================================

(deftest list-hooks-returns-registered-handlers
  (testing "list-hooks returns all handlers for an event"
    (let [registry (hooks/create-registry)
          handler (fn [_] nil)]
      (hooks/register-hook registry :task-complete handler)
      (is (= [handler] (hooks/list-hooks registry :task-complete))))))

(deftest list-hooks-returns-empty-for-no-registrations
  (testing "list-hooks returns empty vector when no handlers registered"
    (let [registry (hooks/create-registry)]
      (is (empty? (hooks/list-hooks registry :task-complete))))))

;; =============================================================================
;; Unregister Hook Tests
;; =============================================================================

(deftest unregister-hook-removes-handler
  (testing "unregister-hook removes a specific handler"
    (let [registry (hooks/create-registry)
          handler (fn [_] nil)]
      (hooks/register-hook registry :task-complete handler)
      (hooks/unregister-hook registry :task-complete handler)
      (is (empty? (hooks/list-hooks registry :task-complete))))))

(deftest unregister-hook-leaves-other-handlers
  (testing "unregister-hook only removes specified handler"
    (let [registry (hooks/create-registry)
          handler1 (fn [_] :one)
          handler2 (fn [_] :two)]
      (hooks/register-hook registry :task-complete handler1)
      (hooks/register-hook registry :task-complete handler2)
      (hooks/unregister-hook registry :task-complete handler1)
      (is (= [handler2] (hooks/list-hooks registry :task-complete))))))

;; =============================================================================
;; Default Hooks Tests
;; =============================================================================

(deftest default-hooks-registered
  (testing "Registry with defaults has hooks for key events"
    (let [registry (hooks/create-registry-with-defaults)]
      (is (seq (hooks/list-hooks registry :task-complete))))))

(deftest default-hooks-map-exists
  (testing "default-hooks map is defined"
    (is (map? hooks/default-hooks))))

;; =============================================================================
;; Hook Event Emission Tests
;; =============================================================================

(deftest emit-hook-event-creates-valid-payload
  (testing "emit-hook-event creates properly structured payload"
    (let [payload (hooks/create-hook-event-payload :task-complete
                                                   {:task-id "test-123" :result :success})]
      (is (map? payload))
      (is (= :task-complete (:event payload)))
      (is (= "test-123" (get-in payload [:context :task-id]))))))

(deftest emit-hook-event-validates-event-type
  (testing "emit-hook-event throws on invalid event"
    (is (thrown? Exception
                 (hooks/create-hook-event-payload :invalid-event {})))))

;; =============================================================================
;; Clear Hooks Tests
;; =============================================================================

(deftest clear-hooks-removes-all-for-event
  (testing "clear-hooks removes all handlers for an event"
    (let [registry (hooks/create-registry)]
      (hooks/register-hook registry :task-complete (fn [_] nil))
      (hooks/register-hook registry :task-complete (fn [_] nil))
      (hooks/clear-hooks registry :task-complete)
      (is (empty? (hooks/list-hooks registry :task-complete))))))

(deftest clear-all-hooks-removes-everything
  (testing "clear-all-hooks removes all handlers from all events"
    (let [registry (hooks/create-registry)]
      (hooks/register-hook registry :task-complete (fn [_] nil))
      (hooks/register-hook registry :session-end (fn [_] nil))
      (hooks/clear-all-hooks registry)
      (is (empty? (hooks/list-hooks registry :task-complete)))
      (is (empty? (hooks/list-hooks registry :session-end))))))
