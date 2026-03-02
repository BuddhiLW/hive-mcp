(ns hive-mcp.telemetry.bounded-atom-metrics-test
  "Tests for bounded atom Prometheus metrics (gc-fix-7).

   Validates:
   1. Metrics registry initialization
   2. Instrumented bounded atom auto-updates on bput!/eviction
   3. Sweep metrics (counter, gauge, histogram)
   4. instrument! for existing atoms
   5. snapshot-all! polling
   6. Export format validation"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [hive-dsl.bounded-atom :as ba]
            [hive-mcp.telemetry.bounded-atom-metrics :as bam]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn init-metrics-fixture
  "Initialize metrics registry and clear sweep registry before each test."
  [f]
  (bam/init!)
  (reset! @#'ba/sweepable-registry {})
  (f)
  (reset! @#'ba/sweepable-registry {}))

(use-fixtures :each init-metrics-fixture)

;;; =============================================================================
;;; Registry Initialization Tests
;;; =============================================================================

(deftest test-init-idempotent
  (testing "init! can be called multiple times safely"
    (is (true? (bam/init!)) "First init succeeds")
    (is (true? (bam/init!)) "Second init succeeds (idempotent)")))

(deftest test-registry-exists
  (testing "Registry is a delay that can be dereferenced"
    (is (delay? bam/registry) "Registry is a delay")
    (is (some? @bam/registry) "Registry can be dereferenced")))

;;; =============================================================================
;;; Instrumented Bounded Atom Tests
;;; =============================================================================

(deftest test-instrumented-atom-initial-metrics
  (testing "instrumented-bounded-atom sets initial capacity and zero entries"
    (let [_store (bam/instrumented-bounded-atom
                   {:max-entries 100 :name "test-initial"})]
      (let [metrics (bam/metrics-response)]
        (is (str/includes? metrics "hive_bounded_atom_capacity")
            "capacity gauge present")
        (is (str/includes? metrics "name=\"test-initial\"")
            "name label present")
        (is (str/includes? metrics "hive_bounded_atom_entries")
            "entries gauge present")
        (is (str/includes? metrics "hive_bounded_atom_utilization")
            "utilization gauge present")))))

(deftest test-instrumented-atom-requires-name
  (testing "instrumented-bounded-atom asserts :name is required"
    (is (thrown? AssertionError
                 (bam/instrumented-bounded-atom {:max-entries 10})))))

(deftest test-bput-updates-metrics-on-eviction
  (testing "bput! triggers metrics update when evictions occur"
    (let [store (bam/instrumented-bounded-atom
                  {:max-entries 3
                   :eviction-policy :lru
                   :name "evict-test"})]
      ;; Fill to capacity
      (ba/bput! store :k1 "a")
      (Thread/sleep 2)
      (ba/bput! store :k2 "b")
      (Thread/sleep 2)
      (ba/bput! store :k3 "c")
      (Thread/sleep 2)
      ;; This should trigger capacity eviction
      (ba/bput! store :k4 "d")

      (let [metrics (bam/metrics-response)]
        (is (str/includes? metrics "hive_bounded_atom_evictions_total")
            "evictions counter present after eviction")
        (is (str/includes? metrics "name=\"evict-test\"")
            "eviction has correct name label")
        (is (str/includes? metrics "reason=\"capacity\"")
            "eviction reason is capacity")))))

(deftest test-bput-no-eviction-no-callback
  (testing "bput! does not fire callback when no evictions"
    (let [callback-count (atom 0)
          store (ba/bounded-atom
                  {:max-entries 10
                   :name "no-evict-test"
                   :on-evict (fn [_] (swap! callback-count inc))})]
      (ba/bput! store :k1 "a")
      (ba/bput! store :k2 "b")
      (is (= 0 @callback-count)
          "callback should not fire when no evictions"))))

(deftest test-entries-gauge-tracks-count
  (testing "entries gauge reflects actual entry count after puts"
    (let [store (bam/instrumented-bounded-atom
                  {:max-entries 10 :name "count-test"})]
      (ba/bput! store :k1 "a")
      (ba/bput! store :k2 "b")
      (ba/bput! store :k3 "c")
      ;; Force a metrics snapshot
      (bam/set-entries! "count-test" (ba/bcount store))
      (let [metrics (bam/metrics-response)]
        (is (str/includes? metrics "hive_bounded_atom_entries")
            "entries gauge present")))))

;;; =============================================================================
;;; TTL Eviction Metrics Tests
;;; =============================================================================

(deftest test-ttl-eviction-updates-metrics
  (testing "TTL eviction during bput! updates eviction counter with reason=ttl"
    (let [store (bam/instrumented-bounded-atom
                  {:max-entries 5
                   :ttl-ms 50
                   :name "ttl-evict-test"})]
      ;; Add entries
      (ba/bput! store :k1 "old")
      (ba/bput! store :k2 "old-too")
      ;; Wait for TTL to expire
      (Thread/sleep 80)
      ;; This bput! should trigger TTL eviction
      (ba/bput! store :k3 "new")
      (ba/bput! store :k4 "newer")
      (ba/bput! store :k5 "newest")

      (let [metrics (bam/metrics-response)]
        (is (str/includes? metrics "hive_bounded_atom_evictions_total")
            "evictions counter present after TTL eviction")))))

;;; =============================================================================
;;; Sweep Metrics Tests
;;; =============================================================================

(deftest test-sweep-updates-metrics
  (testing "sweep! updates sweep counter, evicted gauge, and duration histogram"
    (let [store (bam/instrumented-bounded-atom
                  {:max-entries 10
                   :ttl-ms 50
                   :name "sweep-test"})]
      ;; Register for sweep
      (ba/register-sweepable! store :sweep-test)
      ;; Add entries
      (ba/bput! store :k1 "old")
      (ba/bput! store :k2 "old-too")
      ;; Wait for TTL
      (Thread/sleep 80)
      ;; Sweep
      (let [result (ba/sweep! store :sweep-test)]
        (is (= 2 (:evicted-count result)) "sweep evicted 2 entries")
        (is (= 0 (:remaining result)) "no entries remain"))

      (let [metrics (bam/metrics-response)]
        (is (str/includes? metrics "hive_lifecycle_sweep_total")
            "sweep total counter present")
        (is (str/includes? metrics "hive_lifecycle_sweep_last_evicted")
            "sweep last evicted gauge present")
        (is (str/includes? metrics "hive_lifecycle_sweep_duration_seconds")
            "sweep duration histogram present")))))

(deftest test-sweep-duration-recorded
  (testing "sweep! records duration in histogram"
    (let [store (bam/instrumented-bounded-atom
                  {:max-entries 10
                   :ttl-ms 50
                   :name "sweep-duration-test"})]
      (ba/bput! store :k1 "data")
      (Thread/sleep 80)
      (let [result (ba/sweep! store :sweep-duration)]
        (is (number? (:duration-ms result))
            "sweep result includes duration-ms")
        (is (>= (:duration-ms result) 0)
            "duration-ms is non-negative")))))

;;; =============================================================================
;;; Instrument! Tests (existing atom)
;;; =============================================================================

(deftest test-instrument-existing-atom
  (testing "instrument! adds metrics to an existing bounded atom"
    (let [store (ba/bounded-atom {:max-entries 5})
          _ (ba/bput! store :k1 "existing")
          instrumented (bam/instrument! store "retrofitted")]
      ;; Should still have the existing entry
      (is (= "existing" (ba/bget instrumented :k1))
          "existing data preserved after instrumentation")
      ;; Metrics should be initialized
      (let [metrics (bam/metrics-response)]
        (is (str/includes? metrics "name=\"retrofitted\"")
            "retrofitted name label present in metrics"))
      ;; Adding entries through instrumented should trigger callbacks
      (ba/bput! instrumented :k2 "new")
      (ba/bput! instrumented :k3 "new-2")
      (ba/bput! instrumented :k4 "new-3")
      (ba/bput! instrumented :k5 "new-4")
      ;; This should evict via capacity
      (ba/bput! instrumented :k6 "overflow")
      (let [metrics (bam/metrics-response)]
        (is (str/includes? metrics "hive_bounded_atom_evictions_total")
            "eviction counter updated through instrumented atom")))))

;;; =============================================================================
;;; Snapshot Tests
;;; =============================================================================

(deftest test-snapshot-all-updates-gauges
  (testing "snapshot-all! polls registry and updates all gauges"
    (let [store1 (ba/bounded-atom {:max-entries 10})
          store2 (ba/bounded-atom {:max-entries 20})]
      ;; Register with names
      (ba/register-sweepable! (assoc store1 :name "snap-1") :snap-1)
      (ba/register-sweepable! (assoc store2 :name "snap-2") :snap-2)
      ;; Add some entries
      (ba/bput! store1 :k1 "a")
      (ba/bput! store2 :k1 "b")
      (ba/bput! store2 :k2 "c")
      ;; Snapshot
      (bam/snapshot-all!)
      (let [metrics (bam/metrics-response)]
        (is (str/includes? metrics "name=\"snap-1\"")
            "snap-1 metrics present after snapshot")
        (is (str/includes? metrics "name=\"snap-2\"")
            "snap-2 metrics present after snapshot")))))

;;; =============================================================================
;;; Callback Safety Tests
;;; =============================================================================

(deftest test-callback-exception-does-not-break-bput
  (testing "exception in on-evict callback does not prevent bput! from completing"
    (let [store (ba/bounded-atom
                  {:max-entries 2
                   :name "err-test"
                   :on-evict (fn [_] (throw (RuntimeException. "boom!")))})]
      (ba/bput! store :k1 "a")
      (ba/bput! store :k2 "b")
      ;; This triggers eviction + broken callback — should NOT throw
      (ba/bput! store :k3 "c")
      (is (<= (ba/bcount store) 2)
          "capacity still enforced despite callback error"))))

(deftest test-callback-exception-does-not-break-sweep
  (testing "exception in on-sweep callback does not prevent sweep! from completing"
    (let [store (ba/bounded-atom
                  {:max-entries 10
                   :ttl-ms 50
                   :name "sweep-err-test"
                   :on-sweep (fn [_] (throw (RuntimeException. "sweep boom!")))})]
      (ba/bput! store :k1 "old")
      (Thread/sleep 80)
      ;; sweep! should complete without throwing
      (let [result (ba/sweep! store :sweep-err)]
        (is (= 1 (:evicted-count result))
            "sweep still works despite callback error")))))

;;; =============================================================================
;;; Export Format Tests
;;; =============================================================================

(deftest test-metrics-response-format
  (testing "metrics-response returns valid Prometheus format"
    (let [_ (bam/instrumented-bounded-atom {:max-entries 5 :name "format-test"})
          metrics (bam/metrics-response)]
      (is (string? metrics) "Response is a string")
      (is (str/includes? metrics "# HELP")
          "Contains HELP comments")
      (is (str/includes? metrics "# TYPE")
          "Contains TYPE comments")
      (is (str/includes? metrics "hive_bounded_atom")
          "Contains bounded atom metrics")
      (is (str/includes? metrics "hive_lifecycle_sweep")
          "Contains lifecycle sweep metrics"))))

;;; =============================================================================
;;; Integration Test — Full Flow
;;; =============================================================================

(deftest test-full-metrics-flow
  (testing "Complete flow: create, fill, evict, sweep, export"
    (let [store (bam/instrumented-bounded-atom
                  {:max-entries 3
                   :ttl-ms 100
                   :eviction-policy :fifo
                   :name "full-flow"})]
      ;; Register for sweep
      (ba/register-sweepable! store :full-flow)

      ;; Fill to capacity
      (ba/bput! store :k1 "first")
      (Thread/sleep 5)
      (ba/bput! store :k2 "second")
      (Thread/sleep 5)
      (ba/bput! store :k3 "third")
      (Thread/sleep 5)

      ;; Trigger capacity eviction (FIFO: k1 should be evicted)
      (ba/bput! store :k4 "fourth")

      ;; Wait for TTL to expire
      (Thread/sleep 120)

      ;; Sweep should evict remaining expired entries
      (ba/sweep! store :full-flow)

      ;; Verify all metric types are present
      (let [metrics (bam/metrics-response)]
        ;; Per-atom gauges
        (is (str/includes? metrics "hive_bounded_atom_entries{name=\"full-flow\"")
            "entries gauge with label")
        (is (str/includes? metrics "hive_bounded_atom_capacity{name=\"full-flow\"")
            "capacity gauge with label")
        (is (str/includes? metrics "hive_bounded_atom_utilization{name=\"full-flow\"")
            "utilization gauge with label")
        ;; Eviction counter
        (is (str/includes? metrics "hive_bounded_atom_evictions_total")
            "evictions counter present")
        ;; Sweep metrics
        (is (str/includes? metrics "hive_lifecycle_sweep_total")
            "sweep total counter")
        (is (str/includes? metrics "hive_lifecycle_sweep_last_evicted")
            "sweep last evicted gauge")
        (is (str/includes? metrics "hive_lifecycle_sweep_duration_seconds_bucket")
            "sweep duration histogram bucket")))))

(comment
  ;; Run tests from REPL
  (clojure.test/run-tests 'hive-mcp.telemetry.bounded-atom-metrics-test))
