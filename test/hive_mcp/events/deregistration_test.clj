(ns hive-mcp.events.deregistration-test
  "Tests for E1: Handler deregistration API for hive-event.

   Validates:
   - unreg-event removes event handlers
   - unreg-fx removes effect handlers
   - unreg-cofx removes coeffect handlers
   - Deregistering non-existent IDs returns false, no error
   - registered-events / registered-effects / registered-coeffects inspection
   - handler-registry-status shape
   - with-clean-registry isolates test state"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.events.core :as ev]))

;; =============================================================================
;; Test fixture: Use with-clean-registry for each test
;; =============================================================================

(defn clean-registry-fixture [f]
  (ev/with-clean-registry
    (f)))

(use-fixtures :each clean-registry-fixture)

;; =============================================================================
;; E1: unreg-event tests
;; =============================================================================

(deftest unreg-event-removes-handler
  (testing "Register then unreg-event: handler no longer fires"
    (ev/reg-event :test/removable
                  []
                  (fn [_coeffects _event]
                    {:log {:level :info :message "should not fire"}}))
    (is (true? (ev/handler-registered? :test/removable))
        "Handler should be registered")
    (is (true? (ev/unreg-event :test/removable))
        "unreg-event should return true when handler found")
    (is (false? (ev/handler-registered? :test/removable))
        "Handler should no longer be registered")
    ;; Dispatching should now throw
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"No handler registered"
                          (ev/dispatch [:test/removable {:data "test"}])))))

(deftest unreg-event-returns-false-for-nonexistent
  (testing "Unreg non-existent event: returns false, no error"
    (is (false? (ev/unreg-event :test/does-not-exist))
        "Should return false for non-existent event")))

(deftest unreg-event-idempotent
  (testing "Unregistering the same event twice returns false on second call"
    (ev/reg-event :test/once-only
                  []
                  (fn [_ _] {}))
    (is (true? (ev/unreg-event :test/once-only)))
    (is (false? (ev/unreg-event :test/once-only))
        "Second unreg should return false")))

;; =============================================================================
;; E1: unreg-fx tests
;; =============================================================================

(deftest unreg-fx-removes-effect-handler
  (testing "Register then unreg-fx: effect no longer executes"
    (let [executed (atom false)]
      (ev/reg-fx :test/removable-fx
                 (fn [_] (reset! executed true)))
      (is (fn? (ev/get-fx-handler :test/removable-fx))
          "Handler should be registered")
      (is (true? (ev/unreg-fx :test/removable-fx))
          "unreg-fx should return true when handler found")
      (is (nil? (ev/get-fx-handler :test/removable-fx))
          "Handler should be nil after removal")
      (is (false? @executed)
          "Effect should never have fired"))))

(deftest unreg-fx-returns-false-for-nonexistent
  (testing "Unreg non-existent fx: returns false, no error"
    (is (false? (ev/unreg-fx :test/no-such-fx))
        "Should return false for non-existent fx")))

;; =============================================================================
;; E1: unreg-cofx tests
;; =============================================================================

(deftest unreg-cofx-removes-coeffect-handler
  (testing "Register then unreg-cofx: coeffect handler removed"
    (ev/reg-cofx :test/removable-cofx
                 (fn [coeffects]
                   (assoc coeffects :test-val 42)))
    (is (fn? (ev/get-cofx-handler :test/removable-cofx))
        "Cofx handler should be registered")
    (is (true? (ev/unreg-cofx :test/removable-cofx))
        "unreg-cofx should return true when handler found")
    (is (nil? (ev/get-cofx-handler :test/removable-cofx))
        "Cofx handler should be nil after removal")))

(deftest unreg-cofx-returns-false-for-nonexistent
  (testing "Unreg non-existent cofx: returns false, no error"
    (is (false? (ev/unreg-cofx :test/no-such-cofx))
        "Should return false for non-existent cofx")))

;; =============================================================================
;; E1: Inspection API — registered-events
;; =============================================================================

(deftest registered-events-returns-correct-set
  (testing "registered-events returns set of all registered event IDs"
    (is (= #{} (ev/registered-events))
        "Should be empty in clean registry")
    (ev/reg-event :test/alpha [] (fn [_ _] {}))
    (ev/reg-event :test/beta  [] (fn [_ _] {}))
    (ev/reg-event :test/gamma [] (fn [_ _] {}))
    (is (= #{:test/alpha :test/beta :test/gamma}
           (ev/registered-events))
        "Should contain all three registered events")))

(deftest registered-events-reflects-unreg
  (testing "registered-events updates after unreg-event"
    (ev/reg-event :test/stay    [] (fn [_ _] {}))
    (ev/reg-event :test/go-away [] (fn [_ _] {}))
    (is (= #{:test/stay :test/go-away} (ev/registered-events)))
    (ev/unreg-event :test/go-away)
    (is (= #{:test/stay} (ev/registered-events))
        "Should only contain :test/stay after unreg")))

;; =============================================================================
;; E1: Inspection API — registered-effects
;; =============================================================================

(deftest registered-effects-returns-correct-set
  (testing "registered-effects returns set of registered fx IDs"
    (ev/reg-fx :test/fx-alpha (fn [_] nil))
    (ev/reg-fx :test/fx-beta  (fn [_] nil))
    (let [fxs (ev/registered-effects)]
      (is (set? fxs) "Should return a set")
      (is (contains? fxs :test/fx-alpha))
      (is (contains? fxs :test/fx-beta)))))

;; =============================================================================
;; E1: Inspection API — registered-coeffects
;; =============================================================================

(deftest registered-coeffects-returns-correct-set
  (testing "registered-coeffects returns set of registered cofx IDs"
    (ev/reg-cofx :test/cofx-alpha (fn [c] c))
    (ev/reg-cofx :test/cofx-beta  (fn [c] c))
    (let [cofxs (ev/registered-coeffects)]
      (is (set? cofxs) "Should return a set")
      (is (contains? cofxs :test/cofx-alpha))
      (is (contains? cofxs :test/cofx-beta)))))

;; =============================================================================
;; E1: Inspection API — handler-registry-status
;; =============================================================================

(deftest handler-registry-status-shape
  (testing "handler-registry-status returns correct shape"
    (ev/reg-event :test/evt-1 [] (fn [_ _] {}))
    (ev/reg-event :test/evt-2 [] (fn [_ _] {}))
    (ev/reg-fx :test/fx-1 (fn [_] nil))
    (ev/reg-cofx :test/cofx-1 (fn [c] c))
    (let [status (ev/handler-registry-status)]
      (is (map? status) "Should return a map")
      (is (= 2 (:event-count status)) "Should count 2 events")
      (is (pos? (:fx-count status)) "Should have at least 1 fx")
      (is (pos? (:cofx-count status)) "Should have at least 1 cofx")
      (is (vector? (:events status)) "Events should be a vector")
      (is (vector? (:effects status)) "Effects should be a vector")
      (is (vector? (:coeffects status)) "Coeffects should be a vector")
      ;; Verify events are sorted
      (is (= (sort (:events status)) (seq (:events status)))
          "Events should be sorted")
      ;; Verify our registrations are present
      (is (some #{:test/evt-1} (:events status)))
      (is (some #{:test/evt-2} (:events status)))
      (is (some #{:test/fx-1} (:effects status)))
      (is (some #{:test/cofx-1} (:coeffects status))))))

;; =============================================================================
;; E1: with-clean-registry isolation
;; =============================================================================

(deftest with-clean-registry-isolates-state
  (testing "with-clean-registry provides isolation and restores state"
    ;; Register something in the outer (test fixture) registry
    (ev/reg-event :test/outer [] (fn [_ _] {}))
    (is (ev/handler-registered? :test/outer))
    ;; Inner block should not see outer registrations (our fixture already
    ;; gives us a clean registry, so we nest another)
    (ev/with-clean-registry
      (is (not (ev/handler-registered? :test/outer))
          "Inner block should not see outer event handler")
      (ev/reg-event :test/inner [] (fn [_ _] {}))
      (is (ev/handler-registered? :test/inner)
          "Inner registration should be visible inside block"))
    ;; After block, outer should be restored, inner should be gone
    (is (ev/handler-registered? :test/outer)
        "Outer handler should be restored after block")
    (is (not (ev/handler-registered? :test/inner))
        "Inner handler should not leak out")))

;; =============================================================================
;; E1: Thread-safety smoke test
;; =============================================================================

(deftest unreg-event-thread-safety
  (testing "Concurrent register/unreg does not throw"
    (let [ids (mapv #(keyword "test" (str "concurrent-" %)) (range 100))]
      ;; Register all
      (doseq [id ids]
        (ev/reg-event id [] (fn [_ _] {})))
      ;; Concurrently unreg from multiple threads
      (let [futures (mapv (fn [id]
                            (future (ev/unreg-event id)))
                          ids)
            results (mapv deref futures)]
        (is (every? true? results)
            "All concurrent unreg calls should succeed")
        (is (empty? (ev/registered-events))
            "All events should be unregistered")))))

;; =============================================================================
;; E1: Integration — register, dispatch, unreg, dispatch fails
;; =============================================================================

(deftest full-lifecycle-register-dispatch-unreg
  (testing "Full lifecycle: register -> dispatch -> unreg -> dispatch throws"
    (let [fx-log (atom [])]
      ;; Register effect and event
      (ev/reg-fx :test/track (fn [data] (swap! fx-log conj data)))
      (ev/reg-event :test/lifecycle
                    []
                    (fn [_coeffects event]
                      {:test/track {:event-id (first event)
                                    :data (second event)}}))
      ;; Dispatch should work
      (ev/dispatch [:test/lifecycle {:step "first"}])
      (is (= 1 (count @fx-log))
          "Effect should have fired once")
      (is (= {:event-id :test/lifecycle :data {:step "first"}}
             (first @fx-log)))
      ;; Unreg event
      (ev/unreg-event :test/lifecycle)
      ;; Dispatch should now throw
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"No handler registered"
                            (ev/dispatch [:test/lifecycle {:step "second"}])))
      ;; Effect count unchanged
      (is (= 1 (count @fx-log))
          "Effect should not have fired again"))))
