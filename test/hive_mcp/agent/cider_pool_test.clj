(ns hive-mcp.agent.cider-pool-test
  "Pool verification tests for CIDER session pooling.

   Validates:
   1. 3 connections handle 10 concurrent requests (contention + fair distribution)
   2. Recovery after session crash (degraded operation)
   3. High-contention stress (1000 cycles, 10 threads, race detection)
   4. Timeout behavior under exhaustion
   5. acquire-session! atomicity (swap!→get-in race check)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.cider :as cider])
  (:import [java.util.concurrent ArrayBlockingQueue CountDownLatch TimeUnit CyclicBarrier]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Infrastructure
;; =============================================================================

(def ^:private pool-atom
  "Access the private session-pool atom for test manipulation."
  @#'cider/session-pool)

(defn- fresh-pool!
  "Reset pool to empty state."
  []
  (reset! pool-atom {:sessions {} :available (ArrayBlockingQueue. 10)}))

(defn pool-fixture
  "Reset pool before and after each test."
  [f]
  (fresh-pool!)
  (try (f)
       (finally (fresh-pool!))))

(use-fixtures :each pool-fixture)

(defn- populate-pool!
  "Populate pool with n mock sessions (bypasses Emacs spawn).
   Uses ArrayBlockingQueue sized to n."
  [n]
  (let [q (ArrayBlockingQueue. (max n 1))]
    (reset! pool-atom {:sessions {} :available q})
    (doseq [i (range n)]
      (let [sname (str "test-pool-" i)]
        (swap! pool-atom update :sessions assoc sname
               {:name sname
                :port (+ 50000 i)
                :status :idle
                :spawned-at (System/currentTimeMillis)})
        (.offer q sname)))))

(defn- run-concurrent!
  "Run f on thread-count threads with synchronized start.
   Returns {:results [...] :errors [...] :wall-ms int}."
  [thread-count f]
  (let [results (atom [])
        errors (atom [])
        barrier (CyclicBarrier. thread-count)
        latch (CountDownLatch. thread-count)
        start (System/currentTimeMillis)]
    (doseq [tid (range thread-count)]
      (future
        (try
          (.await barrier 5 TimeUnit/SECONDS)
          (let [r (f tid)]
            (swap! results conj r))
          (catch Exception e
            (swap! errors conj {:thread tid :error (ex-message e)}))
          (finally
            (.countDown latch)))))
    (.await latch 30 TimeUnit/SECONDS)
    {:results @results
     :errors @errors
     :wall-ms (- (System/currentTimeMillis) start)}))

;; =============================================================================
;; Test 1: Concurrent Access (3 connections, 10 requests)
;; =============================================================================

(deftest concurrent-10-requests-3-connections-test
  (testing "10 concurrent requests with 3 pool connections all complete"
    (populate-pool! 3)
    (let [{:keys [results errors]}
          (run-concurrent! 10
                           (fn [tid]
                             (if-let [session (cider/acquire-session! 10000)]
                               (do
                                 (Thread/sleep 50) ;; simulate work
                                 (let [sname (:name session)]
                                   (cider/release-session! sname)
                                   {:thread tid :session sname}))
                               {:thread tid :session "TIMEOUT"})))
          sessions-used (frequencies (map :session results))]

      ;; All 10 complete without errors
      (is (= 10 (count results)) "All 10 requests should complete")
      (is (empty? errors) "No exceptions expected")

      ;; All 3 sessions utilized
      (is (= 3 (count sessions-used))
          "All 3 pool sessions should be used")

      ;; Fair distribution (each session used at least twice)
      (is (every? #(>= % 2) (vals sessions-used))
          (str "Each session should handle ≥2 requests: " sessions-used))

      ;; No timeouts
      (is (not-any? #(= "TIMEOUT" (:session %)) results)
          "No requests should timeout")))

  (testing "Pool fully available after concurrent burst"
    (let [status (cider/pool-status)]
      (is (= 3 (:total status)))
      (is (= 3 (:available status))))))

;; =============================================================================
;; Test 2: Recovery After Session Crash
;; =============================================================================

(deftest recovery-after-crash-test
  (testing "Pool degrades gracefully when session is lost"
    (populate-pool! 3)

    ;; Acquire all 3
    (let [s1 (cider/acquire-session! 1000)
          s2 (cider/acquire-session! 1000)
          s3 (cider/acquire-session! 1000)]
      (is (some? s1))
      (is (some? s2))
      (is (some? s3))

      ;; Pool exhausted
      (is (nil? (cider/acquire-session! 100))
          "Should timeout when pool exhausted")

      ;; Simulate crash: only return 2 of 3 sessions
      (cider/release-session! (:name s1))
      (cider/release-session! (:name s2))
      ;; s3 is "crashed" — never returned

      (is (= 2 (:available (cider/pool-status)))
          "Only 2 sessions available after crash")))

  (testing "Degraded pool still handles concurrent requests"
    ;; Pool has 2 available sessions from previous test block
    (let [{:keys [results errors]}
          (run-concurrent! 6
                           (fn [tid]
                             (if-let [session (cider/acquire-session! 5000)]
                               (do
                                 (Thread/sleep 30)
                                 (let [sname (:name session)]
                                   (cider/release-session! sname)
                                   {:thread tid :session sname}))
                               {:thread tid :session "TIMEOUT"})))
          sessions-used (set (map :session results))]

      (is (= 6 (count results)) "All 6 requests should complete")
      (is (empty? errors) "No exceptions in degraded mode")
      (is (<= (count sessions-used) 2)
          "Only 2 healthy sessions should be used"))))

;; =============================================================================
;; Test 3: Stress Test (Race Condition Detection)
;; =============================================================================

(deftest stress-1000-cycles-test
  (testing "1000 acquire/release cycles across 10 threads with 3 sessions"
    (populate-pool! 3)
    (let [iterations 100
          thread-count 10
          corruption-count (atom 0)
          cycle-count (atom 0)
          {:keys [errors]}
          (run-concurrent! thread-count
                           (fn [_tid]
                             (dotimes [_ iterations]
                               (when-let [session (cider/acquire-session! 5000)]
                  ;; Check session integrity
                                 (when (or (nil? (:name session))
                                           (nil? (:port session)))
                                   (swap! corruption-count inc))
                                 (Thread/sleep 1)
                                 (swap! cycle-count inc)
                                 (cider/release-session! (:name session))))
                             {:completed iterations}))]

      (is (= (* iterations thread-count) @cycle-count)
          "All 1000 cycles should complete")
      (is (zero? @corruption-count)
          "Zero session data corruption")
      (is (empty? errors)
          "Zero exceptions under contention"))

    ;; Pool should be fully available after stress
    (let [status (cider/pool-status)]
      (is (= 3 (:available status))
          "Pool fully recovered after stress test"))))

;; =============================================================================
;; Test 4: Timeout Behavior
;; =============================================================================

(deftest timeout-behavior-test
  (testing "acquire-session! returns nil on timeout when pool exhausted"
    (populate-pool! 1)
    (let [s1 (cider/acquire-session! 1000)]
      (is (some? s1) "Should acquire the one session")

      (let [start (System/currentTimeMillis)
            s2 (cider/acquire-session! 200)
            elapsed (- (System/currentTimeMillis) start)]
        (is (nil? s2) "Should return nil on timeout")
        (is (>= elapsed 150)
            (str "Should wait close to timeout (waited " elapsed "ms)")))

      (cider/release-session! (:name s1))))

  (testing "Session becomes available during wait"
    (populate-pool! 1)
    (let [s1 (cider/acquire-session! 1000)
          result (promise)]
      ;; Release after 100ms, acquire waits up to 2000ms
      (future
        (Thread/sleep 100)
        (cider/release-session! (:name s1)))
      (deliver result (cider/acquire-session! 2000))
      (is (some? @result) "Should acquire session released by other thread")
      (when @result
        (cider/release-session! (:name @result))))))

;; =============================================================================
;; Test 5: pool-status accuracy
;; =============================================================================

(deftest pool-status-accuracy-test
  (testing "pool-status reflects actual state"
    (populate-pool! 3)

    ;; All available
    (let [s (cider/pool-status)]
      (is (= 3 (:total s)))
      (is (= 3 (:available s))))

    ;; Acquire one
    (let [s1 (cider/acquire-session! 1000)
          s (cider/pool-status)]
      (is (= 3 (:total s)) "Total unchanged after acquire")
      (is (= 2 (:available s)) "Available decremented")

      ;; Release it back
      (cider/release-session! (:name s1))
      (let [s (cider/pool-status)]
        (is (= 3 (:available s)) "Available restored after release")))))

;; =============================================================================
;; Test 6: acquire-session! atomicity (code smell check)
;; =============================================================================

(deftest acquire-session-atomicity-test
  (testing "acquire-session! returns correct session data despite concurrent mutations"
    (populate-pool! 3)
    ;; Rapidly acquire/release from multiple threads to stress the
    ;; swap!→get-in window in acquire-session!
    (let [bad-data (atom 0)
          {:keys [errors]}
          (run-concurrent! 5
                           (fn [_tid]
                             (dotimes [_ 200]
                               (when-let [session (cider/acquire-session! 3000)]
                                 (when (not= (:name session)
                                             (some-> session :name))
                                   (swap! bad-data inc))
                  ;; Verify session has all expected keys
                                 (when-not (and (:name session)
                                                (:port session)
                                                (:spawned-at session))
                                   (swap! bad-data inc))
                                 (cider/release-session! (:name session))))
                             :ok))]
      (is (zero? @bad-data) "No inconsistent session data observed")
      (is (empty? errors) "No exceptions"))))
