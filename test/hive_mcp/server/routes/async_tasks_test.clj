(ns hive-mcp.server.routes.async-tasks-test
  "An async tool call must be listable, boundable and cancellable.

   The predecessor spawned a bare `(future ...)` and discarded the handle, so
   none of the three was possible: nothing could name a running call, nothing
   could stop it, and nothing could bound it. Every assertion here is
   unsatisfiable against that design, which is what makes them worth having."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.routes.async-tasks :as at]
            [hive-mcp.server.routes.async-task.state :as st])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- clean-registry [f]
  (at/reset-registry!)
  (try (f) (finally (at/reset-registry!))))

(use-fixtures :each clean-registry)

(defn- await-latch
  "Block on `latch` up to `ms`. Returns true when it opened.

   Every wait in this namespace is a CONDITION with a ceiling, never a bare
   sleep: a fixed pause either flakes on a slow box or wastes time on a fast
   one, and it cannot tell 'not yet' from 'never'."
  [^CountDownLatch latch ms]
  (.await latch ms TimeUnit/MILLISECONDS))

;; =============================================================================
;; It runs at all
;; =============================================================================

(deftest a-submitted-task-runs-and-completes
  (let [done (CountDownLatch. 1)
        ran  (atom false)]
    (at/submit! {:task-id "t-ok" :tool "memory" :caller-id "test"
                 :f (fn [] (reset! ran true) (.countDown done))})
    (is (await-latch done 5000) "the task should have run")
    (is (true? @ran))
    (is (= :task/done (:state (at/get-task "t-ok")))
        "a task that reached its own end is :done")))

;; =============================================================================
;; It can be seen while running
;; =============================================================================

(deftest a-running-task-is-listed
  (let [started (CountDownLatch. 1)
        release (CountDownLatch. 1)]
    (at/submit! {:task-id "t-visible" :tool "memory" :caller-id "test"
                 :f (fn [] (.countDown started) (await-latch release 5000))})
    (is (await-latch started 5000))
    (testing "a task in flight is reported, with the tool that owns it"
      (let [t (at/get-task "t-visible")]
        (is (= :task/running (:state t)))
        (is (= "memory" (:tool t)))
        (is (some #(= "t-visible" (:task-id %)) (at/running-tasks)))))
    (.countDown release)))

;; =============================================================================
;; It can be stopped
;; =============================================================================

(deftest cancel-interrupts-a-running-task
  (let [started     (CountDownLatch. 1)
        interrupted (CountDownLatch. 1)]
    (at/submit! {:task-id "t-cancel" :tool "memory" :caller-id "test"
                 :f (fn []
                      (.countDown started)
                      (try
                        ;; Stands in for a long call. Only an interrupt ends it.
                        (Thread/sleep 60000)
                        (catch InterruptedException _
                          (.countDown interrupted))))})
    (is (await-latch started 5000))
    (let [res (at/cancel! "t-cancel")]
      (is (true? (st/stops-the-work? res)))
      (is (= :cancel/interrupted (:adt/variant res))))
    (is (await-latch interrupted 5000)
        "cancellation must reach the task's own thread, not merely mark a flag")
    (is (= :task/cancelled (:state (at/get-task "t-cancel"))))))

(deftest cancelling-an-unknown-task-answers-rather-than-throws
  ;; Asking about a task that already finished, or never existed, is a
  ;; reasonable question. It gets an answer with a reason.
  (let [res (at/cancel! "t-nonexistent")]
    (is (false? (st/stops-the-work? res)))
    (is (= :cancel/unknown-task (:adt/variant res)))))

(deftest cancelling-a-finished-task-says-so
  (let [done (CountDownLatch. 1)]
    (at/submit! {:task-id "t-finished" :tool "memory" :caller-id "test"
                 :f (fn [] (.countDown done))})
    (is (await-latch done 5000))
    ;; The future is done the instant f returns; give the state write its turn
    ;; by asserting on the reason, which does not depend on that race.
    (let [res (at/cancel! "t-finished")]
      (is (false? (st/stops-the-work? res)))
      (is (= :cancel/already-finished (:adt/variant res))))))

;; =============================================================================
;; It can be bounded
;; =============================================================================

(deftest a-timeout-cancels-a-runaway-task
  (let [started     (CountDownLatch. 1)
        interrupted (CountDownLatch. 1)]
    (at/submit! {:task-id "t-bounded" :tool "memory" :caller-id "test"
                 :timeout-ms 300
                 :f (fn []
                      (.countDown started)
                      (try (Thread/sleep 60000)
                           (catch InterruptedException _
                             (.countDown interrupted))))})
    (is (await-latch started 5000))
    (is (await-latch interrupted 5000)
        "a task with a deadline must end WITHOUT anyone calling cancel!")
    (is (= :task/timed-out (:state (at/get-task "t-bounded")))
        "and it must say the bound is why it stopped, not report a plain cancel")))

(deftest a-task-inside-its-bound-is-untouched
  ;; The deadline must not be a fixed pause that fires regardless: a task that
  ;; finishes in time keeps its own outcome.
  (let [done (CountDownLatch. 1)]
    (at/submit! {:task-id "t-inside" :tool "memory" :caller-id "test"
                 :timeout-ms 10000
                 :f (fn [] (.countDown done))})
    (is (await-latch done 5000))
    (is (= :task/done (:state (at/get-task "t-inside"))))))

;; =============================================================================
;; The execution seam (DIP)
;; =============================================================================

(deftest the-runner-is-substitutable
  ;; The point of the port: the registry's rules can be exercised with no
  ;; threads at all. A runner that runs f on the calling thread is a legitimate
  ;; ITaskRunner, and everything above it must still behave.
  (let [ran (atom false)
        inline (reify at/ITaskRunner
                 (-run [_ f] (f) ::handle)
                 (-cancel [_ _] false)
                 (-view [_ h] {:present? (some? h) :done? true :cancelled? false}))]
    (reset! at/runner inline)
    (try
      (at/submit! {:task-id "t-inline" :tool "memory" :caller-id "test"
                   :f (fn [] (reset! ran true))})
      (is (true? @ran) "the injected runner executed the body")
      (is (= :task/done (:state (at/get-task "t-inline"))))
      (testing "cancelling work that already finished is refused, not attempted"
        (is (= :cancel/already-finished (:adt/variant (at/cancel! "t-inline")))))
      (finally (reset! at/runner nil)))))

;; =============================================================================
;; Housekeeping
;; =============================================================================

(deftest forget-drops-finished-tasks-and-keeps-running-ones
  (let [done    (CountDownLatch. 1)
        started (CountDownLatch. 1)
        release (CountDownLatch. 1)]
    (at/submit! {:task-id "t-gone" :tool "memory" :caller-id "test"
                 :f (fn [] (.countDown done))})
    (at/submit! {:task-id "t-stays" :tool "memory" :caller-id "test"
                 :f (fn [] (.countDown started) (await-latch release 5000))})
    (is (await-latch done 5000))
    (is (await-latch started 5000))
    (at/forget!)
    (is (nil? (at/get-task "t-gone")) "a finished task is dropped")
    (is (some? (at/get-task "t-stays")) "a running task is never dropped")
    (.countDown release)))
