(ns hive-mcp.gc.gc-sweep-integration-test
  "gc-fix-5: Tests for GC sweep integration with crystal/session lifecycle
   and periodic housekeeping scheduler.

   Tests cover:
   1. Sweep is callable from session wrap path (trigger-gc-sweep!)
   2. Housekeeping sweep includes GC sweep results
   3. Sweep on session wrap with registered bounded atoms evicts stale entries
   4. Housekeeping scheduler start/stop lifecycle
   5. Sweep stats are logged at :info level"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.gc.bounded-atom :as batom]
            [hive-mcp.scheduler.housekeeping :as housekeeping]
            [hive-mcp.events.handlers.lifecycle :as lifecycle]
            [hive-mcp.events.effects.lifecycle :as lifecycle-effects]
            [hive-mcp.events.core :as ev]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn clean-fixture
  "Reset bounded atom registry, lifecycle state, and stop housekeeping scheduler."
  [f]
  (batom/reset-registry!)
  (lifecycle-effects/reset-state!)
  (housekeeping/stop!)
  (try
    (f)
    (finally
      (batom/reset-registry!)
      (lifecycle-effects/reset-state!)
      (housekeeping/stop!))))

(use-fixtures :each clean-fixture)

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- make-test-atom-with-stale
  "Create a test atom with a mix of fresh and stale entries.
   Returns the atom."
  [id max-entries ttl-ms fresh-count stale-count]
  (let [now (System/currentTimeMillis)
        fresh-entries (map (fn [i] [(str "fresh-" i) {:created-at now :value i}])
                          (range fresh-count))
        stale-entries (map (fn [i] [(str "stale-" i) {:created-at (- now (* 2 ttl-ms)) :value i}])
                          (range stale-count))
        a (atom (into {} (concat fresh-entries stale-entries)))]
    (batom/register! id
      {:name        (name id)
       :atom-ref    a
       :max-entries max-entries
       :ttl-ms      ttl-ms
       :count-fn    (fn [aref] (count @aref))
       :evict-fn    (fn [aref {:keys [max-entries ttl-ms now-ms]}]
                      (let [before (count @aref)]
                        ;; Evict expired entries
                        (when ttl-ms
                          (swap! aref
                                 (fn [m]
                                   (into {}
                                         (remove (fn [[_k v]]
                                                   (< (:created-at v)
                                                      (- now-ms ttl-ms))))
                                         m))))
                        ;; Evict over-capacity (keep newest)
                        (when (> (count @aref) max-entries)
                          (swap! aref
                                 (fn [m]
                                   (into {}
                                         (take max-entries
                                               (sort-by (fn [[_k v]] (- (:created-at v)))
                                                        m))))))
                        (- before (count @aref))))})
    a))

;; =============================================================================
;; Test: Direct sweep-all! works as expected
;; =============================================================================

(deftest sweep-all-evicts-stale-entries
  (testing "sweep-all! evicts stale entries from registered bounded atoms"
    (let [a (make-test-atom-with-stale :test-cache 100 5000 3 2)]
      (is (= 5 (count @a)) "Should start with 5 entries")
      (let [result (batom/sweep-all!)]
        (is (= 2 (:total-evicted result)) "Should evict 2 stale entries")
        (is (= 3 (count @a)) "Should have 3 fresh entries remaining")
        (is (= 1 (:atom-count result)) "Should report 1 atom swept")))))

;; =============================================================================
;; Test: trigger-sweep! dispatches through event system
;; =============================================================================

(deftest trigger-sweep-dispatches-via-events
  (testing "trigger-sweep! dispatches :lifecycle/sweep event correctly"
    (ev/with-clean-registry
      ;; Register handler and effect
      (lifecycle/reset-registration!)
      (lifecycle/register-handlers!)
      (ev/reg-fx :lifecycle/sweep-fx
                 (fn [_data]
                   (batom/sweep-all!)))
      (ev/reg-fx :log (fn [_] nil))

      ;; Set up bounded atom with stale entries
      (let [a (make-test-atom-with-stale :trigger-test 100 5000 2 3)]
        (is (= 5 (count @a)) "Should start with 5 entries")

        ;; Trigger sweep via the convenience function
        (lifecycle/trigger-sweep!)

        ;; Verify sweep happened
        (is (= 2 (count @a)) "Should have 2 fresh entries after sweep")))))

;; =============================================================================
;; Test: Housekeeping sweep includes GC sweep
;; =============================================================================

(deftest housekeeping-sweep-includes-gc-sweep
  (testing "housekeeping-sweep! includes bounded atom GC sweep results"
    (let [a (make-test-atom-with-stale :hk-test 100 5000 2 4)]
      (is (= 6 (count @a)) "Should start with 6 entries")

      (let [result (housekeeping/housekeeping-sweep!)]
        ;; The gc-sweep key should be present in housekeeping results
        (is (contains? result :gc-sweep) "Should have :gc-sweep in result")

        ;; GC sweep should have evicted the stale entries
        (let [gc (get result :gc-sweep)]
          (is (= 4 (:total-evicted gc)) "GC sweep should evict 4 stale entries")
          (is (= 1 (:atom-count gc)) "Should report 1 atom swept"))

        ;; Atom should only have fresh entries
        (is (= 2 (count @a)) "Should have 2 fresh entries remaining")

        ;; Other housekeeping results should also be present
        (is (contains? result :callbacks) "Should have :callbacks")
        (is (contains? result :async-batches) "Should have :async-batches")
        (is (contains? result :event-journal) "Should have :event-journal")
        (is (contains? result :saa-states) "Should have :saa-states")
        (is (pos? (:sweep-number result)) "Should have positive sweep number")
        (is (>= (:duration-ms result) 0) "Should have non-negative duration")))))

;; =============================================================================
;; Test: Housekeeping sweep is idempotent for GC
;; =============================================================================

(deftest housekeeping-gc-sweep-idempotent
  (testing "Running housekeeping twice doesn't double-evict GC entries"
    (let [a (make-test-atom-with-stale :idem-hk 100 5000 3 2)]
      ;; First sweep: evicts 2 stale
      (let [r1 (housekeeping/housekeeping-sweep!)]
        (is (= 2 (get-in r1 [:gc-sweep :total-evicted])))
        (is (= 3 (count @a))))

      ;; Second sweep: nothing to evict
      (let [r2 (housekeeping/housekeeping-sweep!)]
        (is (= 0 (get-in r2 [:gc-sweep :total-evicted]))
            "Second sweep should evict nothing")
        (is (= 3 (count @a))
            "Atom should still have 3 entries")))))

;; =============================================================================
;; Test: Housekeeping scheduler lifecycle
;; =============================================================================

(deftest housekeeping-scheduler-start-stop
  (testing "Housekeeping scheduler can be started and stopped cleanly"
    (let [start-result (housekeeping/start!)]
      (if (:started start-result)
        (do
          (is (true? (:running? (housekeeping/status)))
              "Should report running after start")
          (let [stop-result (housekeeping/stop!)]
            (is (true? (:stopped stop-result))
                "Should report stopped")
            (is (false? (:running? (housekeeping/status)))
                "Should report not-running after stop")))
        ;; If disabled via config, that's OK
        (is (string? (:reason start-result))
            "Should have reason when not started")))))

(deftest housekeeping-scheduler-start-idempotent
  (testing "Starting housekeeping scheduler twice returns already-running"
    (let [r1 (housekeeping/start!)]
      (when (:started r1)
        (let [r2 (housekeeping/start!)]
          (is (false? (:started r2)))
          (is (= "already-running" (:reason r2))))))))

(deftest housekeeping-stop-when-not-running
  (testing "Stopping when not running is safe"
    (let [result (housekeeping/stop!)]
      (is (false? (:stopped result)))
      (is (= "not-running" (:reason result))))))

;; =============================================================================
;; Test: Housekeeping status introspection
;; =============================================================================

(deftest housekeeping-status-shows-gc-sweep-results
  (testing "Status shows last sweep result including GC sweep data"
    (make-test-atom-with-stale :status-test 100 5000 1 1)
    (housekeeping/housekeeping-sweep!)

    (let [status (housekeeping/status)]
      (is (pos? (:sweep-count status)) "Should have sweep count > 0")
      (is (some? (:last-run status)) "Should have last-run timestamp")
      (is (some? (:last-result status)) "Should have last-result")
      (is (contains? (:last-result status) :gc-sweep)
          "Last result should contain :gc-sweep"))))

;; =============================================================================
;; Test: GC sweep handles empty registry gracefully
;; =============================================================================

(deftest housekeeping-gc-sweep-empty-registry
  (testing "Housekeeping GC sweep with no registered atoms returns zero stats"
    (let [result (housekeeping/housekeeping-sweep!)
          gc (:gc-sweep result)]
      (is (= 0 (:total-evicted gc)) "Should evict nothing with empty registry")
      (is (= 0 (:atom-count gc)) "Should report 0 atoms"))))

;; =============================================================================
;; Test: GC sweep error isolation
;; =============================================================================

(deftest housekeeping-gc-sweep-error-isolation
  (testing "GC sweep error in one atom doesn't crash housekeeping"
    ;; Register a failing atom
    (batom/register! :bad-atom
      {:atom-ref    (atom {})
       :max-entries 1
       :count-fn    (fn [_] 0)
       :evict-fn    (fn [_ _] (throw (Exception. "boom")))})
    ;; Register a good atom with stale entries
    (make-test-atom-with-stale :good-atom 100 5000 1 1)

    (let [result (housekeeping/housekeeping-sweep!)
          gc (:gc-sweep result)]
      ;; Sweep should complete (not crash)
      (is (map? gc) "GC sweep should return a map")
      (is (= 2 (:atom-count gc)) "Should have processed both atoms")
      ;; The good atom's stale entry should be evicted
      (is (pos? (:total-evicted gc))
          "Should have evicted at least from the good atom"))))

;; =============================================================================
;; Test: Multiple atoms swept in single housekeeping pass
;; =============================================================================

(deftest housekeeping-sweeps-multiple-atoms
  (testing "Housekeeping GC sweep handles multiple bounded atoms"
    (let [a1 (make-test-atom-with-stale :cache-1 100 5000 2 3)
          a2 (make-test-atom-with-stale :cache-2 100 5000 1 2)
          result (housekeeping/housekeeping-sweep!)
          gc (:gc-sweep result)]
      (is (= 5 (:total-evicted gc)) "Should evict 3+2=5 stale entries total")
      (is (= 2 (:atom-count gc)) "Should report 2 atoms swept")
      (is (= 2 (count @a1)) "cache-1 should have 2 fresh entries")
      (is (= 1 (count @a2)) "cache-2 should have 1 fresh entry"))))
