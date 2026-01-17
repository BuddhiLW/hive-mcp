(ns hive-mcp.events.metrics-test
  "Tests for metrics interceptor - POC-15.

   Validates:
   - Per-event-type dispatch counting
   - Handler execution time measurement
   - get-metrics and reset-metrics! functions"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.events.core :as ev]
            [hive-mcp.telemetry.prometheus :as prom]))

;; =============================================================================
;; Test fixture: Reset metrics before each test
;; =============================================================================

(defn reset-metrics-fixture [f]
  (ev/reset-metrics!)
  (f)
  (ev/reset-metrics!))

(use-fixtures :each reset-metrics-fixture)

;; =============================================================================
;; POC-15: Metrics interceptor tests
;; =============================================================================

(deftest metrics-interceptor-exists
  (testing "metrics interceptor is defined"
    (is (some? ev/metrics) "metrics interceptor should be defined")
    (is (ev/interceptor? ev/metrics) "metrics should be a valid interceptor")
    (is (= :metrics (:id ev/metrics)) "interceptor id should be :metrics")))

(deftest get-metrics-returns-snapshot
  (testing "get-metrics returns expected structure"
    (let [m (ev/get-metrics)]
      (is (map? m) "Should return a map")
      (is (contains? m :events-dispatched) "Should have :events-dispatched")
      (is (contains? m :events-by-type) "Should have :events-by-type")
      (is (contains? m :effects-executed) "Should have :effects-executed")
      (is (contains? m :errors) "Should have :errors")
      (is (contains? m :avg-dispatch-ms) "Should have :avg-dispatch-ms")
      (is (contains? m :avg-by-type) "Should have :avg-by-type")
      (is (contains? m :timings-count) "Should have :timings-count"))))

(deftest reset-metrics-clears-all-counters
  (testing "reset-metrics! clears all metrics to initial state"
    ;; First, let's manually set some values to ensure reset works
    (let [before (ev/get-metrics)]
      (ev/reset-metrics!)
      (let [after (ev/get-metrics)]
        (is (= 0 (:events-dispatched after)))
        (is (= {} (:events-by-type after)))
        (is (= 0 (:effects-executed after)))
        (is (= 0 (:errors after)))
        (is (= 0 (:timings-count after)))
        (is (= {} (:avg-by-type after)))))))

(deftest metrics-interceptor-tracks-total-count
  (testing "metrics interceptor increments total event count"
    (let [initial (ev/get-metrics)
          context {:coeffects {:event [:test-event {:data "test"}]}
                   :effects {}
                   :queue []
                   :stack []}
          ;; Execute the before phase
          before-fn (:before ev/metrics)
          after-ctx (before-fn context)
          final (ev/get-metrics)]
      (is (= 1 (- (:events-dispatched final)
                  (:events-dispatched initial)))
          "Should increment total event count by 1"))))

(deftest metrics-interceptor-tracks-per-event-type
  (testing "metrics interceptor tracks count per event type"
    (ev/reset-metrics!)
    (let [before-fn (:before ev/metrics)
          after-fn (:after ev/metrics)
          ;; Dispatch :event-a twice
          ctx-a1 {:coeffects {:event [:event-a {:x 1}]} :effects {} :queue [] :stack []}
          ctx-a2 {:coeffects {:event [:event-a {:x 2}]} :effects {} :queue [] :stack []}
          ;; Dispatch :event-b once
          ctx-b {:coeffects {:event [:event-b {:y 1}]} :effects {} :queue [] :stack []}
          _ (-> ctx-a1 before-fn after-fn)
          _ (-> ctx-a2 before-fn after-fn)
          _ (-> ctx-b before-fn after-fn)
          m (ev/get-metrics)]
      (is (= 3 (:events-dispatched m)) "Total should be 3")
      (is (= 2 (get-in m [:events-by-type :event-a])) ":event-a should have 2")
      (is (= 1 (get-in m [:events-by-type :event-b])) ":event-b should have 1"))))

(deftest metrics-interceptor-records-timing
  (testing "metrics interceptor records execution time"
    (ev/reset-metrics!)
    (let [before-fn (:before ev/metrics)
          after-fn (:after ev/metrics)
          ctx {:coeffects {:event [:timed-event {}]} :effects {} :queue [] :stack []}
          ;; Run through the interceptor
          ctx-after-before (before-fn ctx)
          ;; Simulate some work (sleep a tiny bit)
          _ (Thread/sleep 1)
          ctx-after-after (after-fn ctx-after-before)
          m (ev/get-metrics)]
      (is (pos? (:timings-count m)) "Should have timing samples")
      (is (pos? (:avg-dispatch-ms m)) "Average dispatch time should be positive")
      (is (seq (get-in m [:timings-by-type :timed-event]))
          "Should have per-type timing samples"))))

(deftest metrics-interceptor-records-timing-per-type
  (testing "metrics interceptor tracks timing separately per event type"
    (ev/reset-metrics!)
    (let [before-fn (:before ev/metrics)
          after-fn (:after ev/metrics)
          ctx-fast {:coeffects {:event [:fast-event {}]} :effects {} :queue [] :stack []}
          ctx-slow {:coeffects {:event [:slow-event {}]} :effects {} :queue [] :stack []}
          ;; Fast event
          _ (-> ctx-fast before-fn after-fn)
          ;; Slow event (with artificial delay)
          ctx-slow-after-before (before-fn ctx-slow)
          _ (Thread/sleep 5)
          _ (after-fn ctx-slow-after-before)
          m (ev/get-metrics)]
      (is (contains? (:avg-by-type m) :fast-event) "Should have :fast-event avg")
      (is (contains? (:avg-by-type m) :slow-event) "Should have :slow-event avg")
      ;; Slow event should have higher average (not always guaranteed due to timing variance,
      ;; but with 5ms delay it should be reliably larger)
      (is (> (get-in m [:avg-by-type :slow-event])
             (get-in m [:avg-by-type :fast-event]))
          ":slow-event should have higher average time than :fast-event"))))

(deftest metrics-interceptor-rolling-window
  (testing "metrics uses rolling window of 100 samples"
    (ev/reset-metrics!)
    (let [before-fn (:before ev/metrics)
          after-fn (:after ev/metrics)
          ;; Dispatch 150 events
          _ (dotimes [_ 150]
              (let [ctx {:coeffects {:event [:window-test {}]}
                         :effects {}
                         :queue []
                         :stack []}]
                (-> ctx before-fn after-fn)))
          m (ev/get-metrics)]
      ;; Total count should be 150
      (is (= 150 (:events-dispatched m)) "Total dispatched should be 150")
      ;; But timing samples should be capped at 100
      (is (<= (:timings-count m) 100) "Timings should be capped at 100")
      (is (<= (count (get-in m [:timings-by-type :window-test])) 100)
          "Per-type timings should be capped at 100"))))

;; =============================================================================
;; Integration test: metrics with full dispatch
;; =============================================================================

(deftest metrics-works-with-dispatch
  (testing "metrics interceptor works in full dispatch chain"
    (ev/reset-metrics!)
    (ev/reset-all!)
    ;; Register a test event with metrics interceptor
    (ev/reg-event :metrics-test-event
                  [ev/metrics]
                  (fn [_coeffects _event]
                    {:log "Metrics test event executed"}))
    ;; Register a dummy :log effect handler
    (ev/reg-fx :log (fn [_] nil))
    ;; Dispatch the event
    (ev/dispatch [:metrics-test-event {:test true}])
    (let [m (ev/get-metrics)]
      (is (= 1 (:events-dispatched m)) "Should have 1 event dispatched")
      (is (= 1 (get-in m [:events-by-type :metrics-test-event]))
          "Should have 1 :metrics-test-event")
      (is (pos? (:timings-count m)) "Should have timing recorded"))))

;; =============================================================================
;; Prometheus Integration Tests (CLARITY-T: Telemetry first)
;; =============================================================================

(deftest dispatch-records-prometheus-events-total
  (testing "dispatch increments Prometheus events-total counter"
    (prom/init!)
    (ev/reset-all!)

    ;; Register a test event
    (ev/reg-event :prom-test-event
                  []
                  (fn [_coeffects _event]
                    {:log "Prometheus test"}))
    (ev/reg-fx :log (fn [_] nil))

    ;; Dispatch the event
    (ev/dispatch [:prom-test-event {:data "test"}])

    ;; Verify Prometheus metric
    (let [metrics (prom/metrics-response)]
      (is (str/includes? metrics "hive_mcp_events_total")
          "events-total counter present")
      (is (str/includes? metrics "type=\"prom-test-event\"")
          "event type label recorded"))))

(deftest dispatch-records-prometheus-request-duration
  (testing "dispatch records request duration to Prometheus histogram"
    (prom/init!)
    (ev/reset-all!)

    ;; Register a test event
    (ev/reg-event :prom-duration-test
                  []
                  (fn [_coeffects _event]
                    (Thread/sleep 5) ; Add small delay
                    {:log "Duration test"}))
    (ev/reg-fx :log (fn [_] nil))

    ;; Dispatch the event
    (ev/dispatch [:prom-duration-test {}])

    ;; Verify Prometheus histogram
    (let [metrics (prom/metrics-response)]
      (is (str/includes? metrics "hive_mcp_request_duration_seconds")
          "request-duration-seconds histogram present")
      (is (str/includes? metrics "tool=\"event-dispatch-prom-duration-test\"")
          "tool label with event-dispatch prefix recorded"))))
