(ns hive-mcp.concurrency.pool
  "Central bounded thread pool infrastructure for hive-mcp.

   Provides three named thread pools with bounded sizes to prevent
   unbounded thread creation throughout the codebase:

   1. :io-pool     — FixedThreadPool for blocking I/O (HTTP, file, DB, embeddings)
                      Size: (+ 2 (* 2 available-processors))
   2. :compute-pool — FixedThreadPool for CPU-bound work (KG traversal, compression)
                      Size: available-processors
   3. :event-pool   — FixedThreadPool for fire-and-forget event dispatch
                      Size: 4 (events should be fast; this bounds storm damage)

   All pools use LinkedBlockingQueue with bounded capacity (256) and
   CallerRunsPolicy for backpressure — if the queue is full, the submitting
   thread does the work instead of creating new threads.

   Thread names are prefixed for JVM diagnostics:
     hive-io-0, hive-compute-1, hive-event-2, etc.

   Usage:
     (require '[hive-mcp.concurrency.pool :as pool])
     (pool/submit-io! (fn [] (http-get url)))
     (pool/submit-compute! (fn [] (compress data)))
     (pool/submit-event! (fn [] (dispatch! event)))
     (pool/pool-stats)
     (pool/shutdown-pools!)"
  (:import [java.util.concurrent
            ThreadPoolExecutor
            LinkedBlockingQueue
            ThreadFactory
            TimeUnit
            ThreadPoolExecutor$CallerRunsPolicy
            Future
            RejectedExecutionException]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Thread Factory
;; =============================================================================

(defn- named-thread-factory
  "Create a ThreadFactory that names threads with the given prefix.
   Threads are daemons so they don't block JVM shutdown."
  ^ThreadFactory [^String prefix]
  (let [counter (atom 0)]
    (reify ThreadFactory
      (newThread [_ runnable]
        (doto (Thread. runnable (str prefix "-" (swap! counter inc)))
          (.setDaemon true))))))

;; =============================================================================
;; Pool Construction
;; =============================================================================

(def ^:private queue-capacity
  "Bounded queue capacity for all pools. Tasks beyond this trigger CallerRunsPolicy."
  256)

(defn- make-pool
  "Create a bounded ThreadPoolExecutor with CallerRunsPolicy.
   core-size = max-size (fixed pool), bounded queue, caller-runs rejection."
  ^ThreadPoolExecutor [^long pool-size ^String name-prefix]
  (ThreadPoolExecutor.
   pool-size                              ; core pool size
   pool-size                              ; max pool size (fixed)
   60                                     ; keep-alive for excess threads
   TimeUnit/SECONDS
   (LinkedBlockingQueue. (int queue-capacity))
   (named-thread-factory name-prefix)
   (ThreadPoolExecutor$CallerRunsPolicy.)))

;; =============================================================================
;; Pool Instances (lazy init via delay)
;; =============================================================================

(def ^:private available-processors
  (.availableProcessors (Runtime/getRuntime)))

(def ^:private io-pool-size
  (+ 2 (* 2 available-processors)))

(def ^:private compute-pool-size
  available-processors)

(def ^:private event-pool-size
  4)

(defonce ^:private io-pool
  (delay (make-pool io-pool-size "hive-io")))

(defonce ^:private compute-pool
  (delay (make-pool compute-pool-size "hive-compute")))

(defonce ^:private event-pool
  (delay (make-pool event-pool-size "hive-event")))

;; =============================================================================
;; Submit API
;; =============================================================================

(defn submit-io!
  "Submit a blocking I/O task to the IO pool. Returns a Future.
   Use for: HTTP calls, file I/O, database queries, embedding requests."
  ^Future [^Callable f]
  (try
    (.submit ^ThreadPoolExecutor @io-pool ^Callable f)
    (catch RejectedExecutionException _
      ;; Pool is shut down — run in caller thread
      (let [result (f)]
        (reify Future
          (get [_] result)
          (get [_ _timeout _unit] result)
          (isDone [_] true)
          (isCancelled [_] false)
          (cancel [_ _] false))))))

(defn submit-compute!
  "Submit a CPU-bound task to the compute pool. Returns a Future.
   Use for: KG traversal, data compression, batch transforms."
  ^Future [^Callable f]
  (try
    (.submit ^ThreadPoolExecutor @compute-pool ^Callable f)
    (catch RejectedExecutionException _
      (let [result (f)]
        (reify Future
          (get [_] result)
          (get [_ _timeout _unit] result)
          (isDone [_] true)
          (isCancelled [_] false)
          (cancel [_ _] false))))))

(defn submit-event!
  "Submit a fire-and-forget event dispatch task. Returns a Future.
   Use for: event chaining, KG mutation notifications, broadcast effects.
   Bounded to 4 threads — event storms queue instead of exploding."
  ^Future [^Callable f]
  (try
    (.submit ^ThreadPoolExecutor @event-pool ^Callable f)
    (catch RejectedExecutionException _
      (let [result (f)]
        (reify Future
          (get [_] result)
          (get [_ _timeout _unit] result)
          (isDone [_] true)
          (isCancelled [_] false)
          (cancel [_ _] false))))))

;; =============================================================================
;; DSL Macros — sugar over submit-*! fns
;; =============================================================================

(defmacro with-io
  "Submit body to the IO pool, returning a Future.
   Sugar for (submit-io! (fn [] body))."
  [& body]
  `(submit-io! (fn [] ~@body)))

(defmacro with-compute
  "Submit body to the compute pool, returning a Future.
   Sugar for (submit-compute! (fn [] body))."
  [& body]
  `(submit-compute! (fn [] ~@body)))

(defmacro with-event
  "Submit body to the event pool, returning a Future.
   Sugar for (submit-event! (fn [] body))."
  [& body]
  `(submit-event! (fn [] ~@body)))

(defmacro with-solo
  "Submit body to Clojure's solo executor (unbounded cached thread pool).
   Returns a Future (clojure.core/future).
   Use for coordinator tasks that internally spawn IO/compute pool work —
   avoids nested-pool contention where a bounded-pool thread blocks
   while spawning more work on the same pool."
  [& body]
  `(future ~@body))

;; =============================================================================
;; Monitoring
;; =============================================================================

(defn- pool-info
  "Extract stats from a ThreadPoolExecutor."
  [^ThreadPoolExecutor pool]
  {:active (.getActiveCount pool)
   :queued (.size (.getQueue pool))
   :pool-size (.getPoolSize pool)
   :max-pool-size (.getMaximumPoolSize pool)
   :completed-tasks (.getCompletedTaskCount pool)})

(defn pool-stats
  "Get current stats for all three pools.
   Returns: {:io {...} :compute {...} :event {...}}"
  []
  {:io (pool-info @io-pool)
   :compute (pool-info @compute-pool)
   :event (pool-info @event-pool)})

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn shutdown-pools!
  "Orderly shutdown of all pools. Waits up to 5 seconds for tasks to complete,
   then force-shuts. Call during JVM shutdown or REPL teardown."
  []
  (doseq [[name pool-delay] [["io" io-pool]
                             ["compute" compute-pool]
                             ["event" event-pool]]]
    (when (realized? pool-delay)
      (let [^ThreadPoolExecutor pool @pool-delay]
        (.shutdown pool)
        (when-not (.awaitTermination pool 5 TimeUnit/SECONDS)
          (.shutdownNow pool))))))
