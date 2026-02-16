(ns hive-mcp.agent.ling.headless-addon-strategy-test
  "Tests for HeadlessAddonStrategy ILingStrategy adapter.

   Tests behavioral contracts:
   - Happy path delegation: spawn!/dispatch! delegate to backend methods
   - Critical paths propagate exceptions: spawn!/dispatch! let exceptions through
   - Non-critical paths use rescue: status/kill!/interrupt! don't throw
   - Strategy satisfies ILingStrategy protocol"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.ling.headless-addon-strategy :as has]
            [hive-mcp.addons.headless :as headless]
            [hive-mcp.agent.ling.strategy :as strategy]))

;; =============================================================================
;; Mock Backend Factories
;; =============================================================================

(defn- make-happy-backend
  "Backend where all methods succeed."
  [id]
  (reify
    headless/IHeadlessBackend
    (headless-id [_] id)
    (headless-spawn! [_ ctx _opts]
      (str "slave-" (name id) "-" (:id ctx)))
    (headless-dispatch! [_ _ctx _task-opts]
      {:dispatched? true})
    (headless-status [_ ctx ds-status]
      (merge ds-status {:alive? true :slave/id (:id ctx)}))
    (headless-kill! [_ ctx]
      {:killed? true :id (:id ctx)})
    (headless-interrupt! [_ ctx]
      {:success? true :ling-id (:id ctx)})))

(defn- make-throwing-backend
  "Backend where all methods throw."
  [id]
  (reify
    headless/IHeadlessBackend
    (headless-id [_] id)
    (headless-spawn! [_ _ctx _opts]
      (throw (ex-info "spawn failed" {:id (name id)})))
    (headless-dispatch! [_ _ctx _task-opts]
      (throw (ex-info "dispatch failed" {:id (name id)})))
    (headless-status [_ _ctx _ds-status]
      (throw (ex-info "status failed" {:id (name id)})))
    (headless-kill! [_ _ctx]
      (throw (ex-info "kill failed" {:id (name id)})))
    (headless-interrupt! [_ _ctx]
      (throw (ex-info "interrupt failed" {:id (name id)})))))

;; =============================================================================
;; Test: Happy path delegation
;; =============================================================================

(deftest spawn-delegates-to-backend
  (testing "strategy-spawn! delegates to headless-spawn!"
    (let [strat (has/->headless-addon-strategy (make-happy-backend :test-be))
          ctx {:id "ling-42"}
          result (strategy/strategy-spawn! strat ctx {})]
      (is (= "slave-test-be-ling-42" result)))))

(deftest dispatch-delegates-to-backend
  (testing "strategy-dispatch! delegates to headless-dispatch!"
    (let [strat (has/->headless-addon-strategy (make-happy-backend :test-be))
          ctx {:id "ling-42"}
          result (strategy/strategy-dispatch! strat ctx {:task "do stuff"})]
      (is (= {:dispatched? true} result)))))

(deftest status-delegates-to-backend
  (testing "strategy-status delegates to headless-status"
    (let [strat (has/->headless-addon-strategy (make-happy-backend :test-be))
          ctx {:id "ling-42"}
          result (strategy/strategy-status strat ctx {:slave/status :running})]
      (is (:alive? result))
      (is (= "ling-42" (:slave/id result))))))

(deftest kill-delegates-to-backend
  (testing "strategy-kill! delegates to headless-kill!"
    (let [strat (has/->headless-addon-strategy (make-happy-backend :test-be))
          ctx {:id "ling-42"}
          result (strategy/strategy-kill! strat ctx)]
      (is (:killed? result))
      (is (= "ling-42" (:id result))))))

(deftest interrupt-delegates-to-backend
  (testing "strategy-interrupt! delegates to headless-interrupt!"
    (let [strat (has/->headless-addon-strategy (make-happy-backend :test-be))
          ctx {:id "ling-42"}
          result (strategy/strategy-interrupt! strat ctx)]
      (is (:success? result))
      (is (= "ling-42" (:ling-id result))))))

;; =============================================================================
;; Test: Critical paths propagate exceptions
;; =============================================================================

(deftest spawn-propagates-exception
  (testing "strategy-spawn! lets exceptions propagate (critical path)"
    (let [strat (has/->headless-addon-strategy (make-throwing-backend :fail-be))
          ctx {:id "ling-99"}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"spawn failed"
                            (strategy/strategy-spawn! strat ctx {}))))))

(deftest dispatch-propagates-exception
  (testing "strategy-dispatch! lets exceptions propagate (critical path)"
    (let [strat (has/->headless-addon-strategy (make-throwing-backend :fail-be))
          ctx {:id "ling-99"}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"dispatch failed"
                            (strategy/strategy-dispatch! strat ctx {:task "boom"}))))))

;; =============================================================================
;; Test: Non-critical paths use rescue (don't throw)
;; =============================================================================

(deftest status-does-not-throw
  (testing "strategy-status uses rescue, returns safe fallback on exception"
    (let [strat (has/->headless-addon-strategy (make-throwing-backend :fail-be))
          ctx {:id "ling-99"}
          result (strategy/strategy-status strat ctx {:slave/status :running})]
      ;; rescue returns nil for the addon call, then falls back to ds-status with error flag
      (is (some? result))
      (is (:headless-addon-error? result)))))

(deftest status-nil-ds-status-does-not-throw
  (testing "strategy-status with nil ds-status returns nil on backend error"
    (let [strat (has/->headless-addon-strategy (make-throwing-backend :fail-be))
          ctx {:id "ling-99"}
          result (strategy/strategy-status strat ctx nil)]
      ;; rescue returns nil, ds-status is nil, so overall nil
      (is (nil? result)))))

(deftest kill-does-not-throw
  (testing "strategy-kill! uses rescue, returns safe fallback on exception"
    (let [strat (has/->headless-addon-strategy (make-throwing-backend :fail-be))
          ctx {:id "ling-99"}
          result (strategy/strategy-kill! strat ctx)]
      (is (map? result))
      (is (false? (:killed? result))))))

(deftest interrupt-does-not-throw
  (testing "strategy-interrupt! uses rescue, returns safe fallback on exception"
    (let [strat (has/->headless-addon-strategy (make-throwing-backend :fail-be))
          ctx {:id "ling-99"}
          result (strategy/strategy-interrupt! strat ctx)]
      (is (map? result))
      (is (false? (:success? result))))))

;; =============================================================================
;; Test: Strategy satisfies ILingStrategy
;; =============================================================================

(deftest strategy-satisfies-protocol
  (testing "HeadlessAddonStrategy satisfies ILingStrategy"
    (let [strat (has/->headless-addon-strategy (make-happy-backend :proto-test))]
      (is (satisfies? strategy/ILingStrategy strat)))))

(deftest constructor-rejects-non-backend
  (testing "->headless-addon-strategy rejects objects that don't satisfy IHeadlessBackend"
    (is (thrown? AssertionError
                 (has/->headless-addon-strategy {:not "a backend"})))))
