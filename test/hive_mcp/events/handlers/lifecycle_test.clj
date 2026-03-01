(ns hive-mcp.events.handlers.lifecycle-test
  "Tests for :lifecycle/sweep event handler and bounded atom sweep.

   gc-fix-4: validates sweep dispatches correctly, stats are accurate,
   and sweep is idempotent."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.events.handlers.lifecycle :as lifecycle]
            [hive-mcp.events.effects.lifecycle :as lifecycle-effects]
            [hive-mcp.gc.bounded-atom :as batom]
            [hive-mcp.events.core :as ev]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn clean-registry-fixture
  "Reset bounded atom registry and lifecycle state before each test."
  [f]
  (batom/reset-registry!)
  (lifecycle-effects/reset-state!)
  (f)
  (batom/reset-registry!)
  (lifecycle-effects/reset-state!))

(use-fixtures :each clean-registry-fixture)

;; =============================================================================
;; Helper: Create test bounded atoms
;; =============================================================================

(defn- make-test-atom
  "Create a test atom with timestamped entries and register it.
   Returns the atom."
  [id max-entries entries]
  (let [now (System/currentTimeMillis)
        a   (atom (into {}
                        (map-indexed
                         (fn [i entry]
                           [(str "key-" i)
                            (merge {:created-at (- now (* i 1000))}
                                   entry)])
                         entries)))]
    (batom/register! id
      {:name        (name id)
       :atom-ref    a
       :max-entries max-entries
       :ttl-ms      5000  ; 5 second TTL for testing
       :count-fn    (fn [aref] (count @aref))
       :evict-fn    (fn [aref {:keys [max-entries ttl-ms now-ms]}]
                      (let [before (count @aref)
                            ;; Evict expired entries
                            _ (when ttl-ms
                                (swap! aref
                                       (fn [m]
                                         (into {}
                                               (remove (fn [[_k v]]
                                                         (< (:created-at v)
                                                            (- now-ms ttl-ms))))
                                               m))))
                            ;; Evict over-capacity (keep newest)
                            _ (when (> (count @aref) max-entries)
                                (swap! aref
                                       (fn [m]
                                         (into {}
                                               (take max-entries
                                                     (sort-by (fn [[_k v]] (- (:created-at v)))
                                                              m))))))
                            after (count @aref)]
                        (- before after)))})
    a))

;; =============================================================================
;; Handler Tests
;; =============================================================================

(deftest handle-lifecycle-sweep-produces-correct-effects
  (testing ":lifecycle/sweep handler produces :lifecycle/sweep-fx and :log effects"
    (let [coeffects {}
          event     [:lifecycle/sweep {}]
          result    (lifecycle/handle-lifecycle-sweep coeffects event)]
      (is (contains? result :lifecycle/sweep-fx)
          "Should produce :lifecycle/sweep-fx effect")
      (is (contains? result :log)
          "Should produce :log effect")
      (is (= :info (get-in result [:log :level]))
          "Log level should be :info"))))

(deftest handle-lifecycle-sweep-passes-atom-ids
  (testing ":lifecycle/sweep passes atom-ids filter to effect"
    (let [event  [:lifecycle/sweep {:atom-ids [:my-atom :other-atom]}]
          result (lifecycle/handle-lifecycle-sweep {} event)]
      (is (= [:my-atom :other-atom]
             (get-in result [:lifecycle/sweep-fx :atom-ids]))
          "Should pass atom-ids to effect"))))

(deftest handle-lifecycle-sweep-nil-atom-ids-for-sweep-all
  (testing ":lifecycle/sweep with empty data sweeps all"
    (let [event  [:lifecycle/sweep {}]
          result (lifecycle/handle-lifecycle-sweep {} event)]
      (is (nil? (get-in result [:lifecycle/sweep-fx :atom-ids]))
          "atom-ids should be nil for sweep-all"))))

;; =============================================================================
;; Bounded Atom Registry Tests
;; =============================================================================

(deftest register-and-deregister-bounded-atom
  (testing "Can register and deregister bounded atoms"
    (let [a (atom {})]
      (batom/register! :test-atom
        {:atom-ref    a
         :max-entries 100
         :count-fn    (fn [aref] (count @aref))
         :evict-fn    (fn [_aref _opts] 0)})
      (is (batom/registered? :test-atom))
      (is (contains? (batom/registered-ids) :test-atom))

      (batom/deregister! :test-atom)
      (is (not (batom/registered? :test-atom))))))

(deftest register-is-idempotent
  (testing "Re-registering with same id overwrites cleanly"
    (let [a1 (atom {})
          a2 (atom {})]
      (batom/register! :test-atom
        {:atom-ref a1 :max-entries 100
         :count-fn (fn [aref] (count @aref))
         :evict-fn (fn [_aref _opts] 0)})
      (batom/register! :test-atom
        {:atom-ref a2 :max-entries 200
         :count-fn (fn [aref] (count @aref))
         :evict-fn (fn [_aref _opts] 0)})
      (is (= 200 (:max-entries (batom/get-spec :test-atom)))
          "Should have updated max-entries"))))

;; =============================================================================
;; Sweep Stats Accuracy Tests
;; =============================================================================

(deftest sweep-empty-registry-returns-empty-stats
  (testing "Sweep with no registered atoms returns empty stats"
    (let [result (batom/sweep-all!)]
      (is (= 0 (:total-evicted result)))
      (is (= [] (:per-atom result)))
      (is (= 0 (:atom-count result)))
      (is (number? (:duration-ms result))))))

(deftest sweep-with-no-evictions-returns-zero
  (testing "Sweep with atoms under capacity and within TTL returns zero evictions"
    (make-test-atom :fresh-atom 100 [{:value "a"} {:value "b"}])
    (let [result (batom/sweep-all!)]
      (is (= 0 (:total-evicted result)))
      (is (= 1 (:atom-count result)))
      (is (= 1 (count (:per-atom result))))
      (let [atom-stats (first (:per-atom result))]
        (is (= :fresh-atom (:atom-id atom-stats)))
        (is (= "fresh-atom" (:name atom-stats)))
        (is (= 0 (:evicted atom-stats)))
        (is (= 2 (:remaining atom-stats)))))))

(deftest sweep-evicts-over-capacity-entries
  (testing "Sweep evicts entries when over max-entries capacity"
    (let [a (atom {})]
      ;; Add 10 entries
      (doseq [i (range 10)]
        (swap! a assoc (str "key-" i)
               {:created-at (- (System/currentTimeMillis) (* i 100))
                :value      i}))
      ;; Register with max 5
      (batom/register! :over-cap
        {:atom-ref    a
         :max-entries 5
         :count-fn    (fn [aref] (count @aref))
         :evict-fn    (fn [aref {:keys [max-entries]}]
                        (let [before (count @aref)]
                          (when (> before max-entries)
                            (swap! aref
                                   (fn [m]
                                     (into {}
                                           (take max-entries
                                                 (sort-by (fn [[_k v]] (- (:created-at v)))
                                                          m))))))
                          (- before (count @aref))))})
      (let [result (batom/sweep-all!)]
        (is (= 5 (:total-evicted result))
            "Should evict 5 entries (10 - 5 max)")
        (is (= 5 (count @a))
            "Atom should have 5 entries remaining")))))

(deftest sweep-evicts-expired-entries
  (testing "Sweep evicts TTL-expired entries"
    (let [now (System/currentTimeMillis)
          a   (atom {"fresh"   {:created-at now :value "new"}
                     "stale-1" {:created-at (- now 10000) :value "old1"}
                     "stale-2" {:created-at (- now 20000) :value "old2"}})]
      (batom/register! :ttl-test
        {:atom-ref    a
         :max-entries 100
         :ttl-ms      5000
         :count-fn    (fn [aref] (count @aref))
         :evict-fn    (fn [aref {:keys [ttl-ms now-ms]}]
                        (let [before (count @aref)]
                          (when ttl-ms
                            (swap! aref
                                   (fn [m]
                                     (into {}
                                           (remove (fn [[_k v]]
                                                     (< (:created-at v)
                                                        (- now-ms ttl-ms))))
                                           m))))
                          (- before (count @aref))))})
      (let [result (batom/sweep-all!)]
        (is (= 2 (:total-evicted result))
            "Should evict 2 expired entries")
        (is (= 1 (count @a))
            "Only fresh entry should remain")
        (is (contains? @a "fresh")
            "Fresh entry should survive")))))

(deftest sweep-stats-multi-atom
  (testing "Sweep collects per-atom stats from multiple registered atoms"
    (let [now (System/currentTimeMillis)
          a1  (atom {"k1" {:created-at now}
                     "k2" {:created-at (- now 20000)}})
          a2  (atom (into {} (map (fn [i] [(str "e-" i) {:created-at now}])
                                  (range 8))))]
      ;; a1: TTL eviction (1 stale)
      (batom/register! :atom-a
        {:name     "atom-alpha"
         :atom-ref a1
         :max-entries 100
         :ttl-ms   5000
         :count-fn (fn [aref] (count @aref))
         :evict-fn (fn [aref {:keys [ttl-ms now-ms]}]
                     (let [before (count @aref)]
                       (swap! aref
                              (fn [m]
                                (into {} (remove (fn [[_k v]]
                                                   (< (:created-at v)
                                                      (- now-ms ttl-ms)))
                                                 m))))
                       (- before (count @aref))))})
      ;; a2: capacity eviction (8 entries, max 3)
      (batom/register! :atom-b
        {:name     "atom-beta"
         :atom-ref a2
         :max-entries 3
         :count-fn (fn [aref] (count @aref))
         :evict-fn (fn [aref {:keys [max-entries]}]
                     (let [before (count @aref)]
                       (when (> before max-entries)
                         (swap! aref
                                (fn [m]
                                  (into {} (take max-entries m)))))
                       (- before (count @aref))))})

      (let [result  (batom/sweep-all!)
            by-id   (into {} (map (fn [s] [(:atom-id s) s]) (:per-atom result)))]
        (is (= 6 (:total-evicted result))
            "Total: 1 (TTL) + 5 (capacity) = 6")
        (is (= 2 (:atom-count result)))
        ;; atom-a
        (is (= 1 (:evicted (get by-id :atom-a))))
        (is (= 1 (:remaining (get by-id :atom-a))))
        (is (= "atom-alpha" (:name (get by-id :atom-a))))
        ;; atom-b
        (is (= 5 (:evicted (get by-id :atom-b))))
        (is (= 3 (:remaining (get by-id :atom-b))))
        (is (= "atom-beta" (:name (get by-id :atom-b))))))))

;; =============================================================================
;; Idempotency Tests
;; =============================================================================

(deftest sweep-is-idempotent
  (testing "Calling sweep twice does not double-evict"
    (let [now (System/currentTimeMillis)
          a   (atom {"fresh" {:created-at now}
                     "stale" {:created-at (- now 20000)}})]
      (batom/register! :idem-test
        {:atom-ref    a
         :max-entries 100
         :ttl-ms      5000
         :count-fn    (fn [aref] (count @aref))
         :evict-fn    (fn [aref {:keys [ttl-ms now-ms]}]
                        (let [before (count @aref)]
                          (when ttl-ms
                            (swap! aref
                                   (fn [m]
                                     (into {}
                                           (remove (fn [[_k v]]
                                                     (< (:created-at v)
                                                        (- now-ms ttl-ms))))
                                           m))))
                          (- before (count @aref))))})

      ;; First sweep: evicts 1 stale entry
      (let [r1 (batom/sweep-all!)]
        (is (= 1 (:total-evicted r1)))
        (is (= 1 (count @a))))

      ;; Second sweep: nothing to evict
      (let [r2 (batom/sweep-all!)]
        (is (= 0 (:total-evicted r2))
            "Second sweep should evict nothing")
        (is (= 1 (count @a))
            "Atom should still have 1 entry")))))

;; =============================================================================
;; Error Handling Tests
;; =============================================================================

(deftest sweep-handles-failing-evict-fn
  (testing "Sweep catches errors from individual atoms without stopping"
    (let [good-atom (atom {"k1" {:created-at (System/currentTimeMillis)}})]
      ;; Register an atom with a failing evict-fn
      (batom/register! :bad-atom
        {:atom-ref    (atom {})
         :max-entries 1
         :count-fn    (fn [_] 0)
         :evict-fn    (fn [_ _] (throw (Exception. "eviction boom")))})
      ;; Register a good atom
      (batom/register! :good-atom
        {:atom-ref    good-atom
         :max-entries 100
         :count-fn    (fn [aref] (count @aref))
         :evict-fn    (fn [_ _] 0)})

      (let [result (batom/sweep-all!)
            by-id  (into {} (map (fn [s] [(:atom-id s) s]) (:per-atom result)))]
        (is (= 2 (:atom-count result))
            "Should still process both atoms")
        (is (some? (:error (get by-id :bad-atom)))
            "Bad atom should have error")
        (is (nil? (:error (get by-id :good-atom)))
            "Good atom should not have error")))))

;; =============================================================================
;; Integration: Full dispatch via event system
;; =============================================================================

(deftest full-dispatch-integration
  (testing "Full event dispatch cycle: :lifecycle/sweep -> :lifecycle/sweep-fx"
    (ev/with-clean-registry
      ;; Register handler and effect
      (lifecycle/reset-registration!)
      (lifecycle/register-handlers!)
      (ev/reg-fx :lifecycle/sweep-fx
                 (fn [data]
                   ;; Use the real sweep
                   (batom/sweep-all!)))
      ;; Also register :log effect to avoid warnings
      (ev/reg-fx :log (fn [_] nil))

      ;; Setup: register a bounded atom with stale entries
      (let [now (System/currentTimeMillis)
            a   (atom {"fresh" {:created-at now}
                       "old"   {:created-at (- now 30000)}})]
        (batom/register! :dispatch-test
          {:atom-ref    a
           :max-entries 100
           :ttl-ms      5000
           :count-fn    (fn [aref] (count @aref))
           :evict-fn    (fn [aref {:keys [ttl-ms now-ms]}]
                          (let [before (count @aref)]
                            (when ttl-ms
                              (swap! aref
                                     (fn [m]
                                       (into {}
                                             (remove (fn [[_k v]]
                                                       (< (:created-at v)
                                                          (- now-ms ttl-ms))))
                                             m))))
                            (- before (count @aref))))})

        ;; Dispatch the sweep event
        (ev/dispatch [:lifecycle/sweep {}])

        ;; Verify the sweep happened
        (is (= 1 (count @a))
            "Stale entry should have been evicted via event dispatch")
        (is (contains? @a "fresh")
            "Fresh entry should survive")))))
