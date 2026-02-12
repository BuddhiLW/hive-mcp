(ns hive-mcp.test-timing
  "Test timing constants and utilities for hive-mcp tests.

   DRY: Consolidates magic number Thread/sleep values.
   Reference: docs/DRY-AUDIT-REPORT.md - A6 Thread.sleep Magic Numbers")

;; =============================================================================
;; Timing Constants (A6)
;; =============================================================================

(def ^:dynamic *async-settle-ms*
  "Time for async operations to settle. Default 100ms.

   Bind to different value for slow CI environments:
     (binding [*async-settle-ms* 200] ...)"
  100)

(def ^:dynamic *channel-wait-ms*
  "Time to wait for channel operations. Default 100ms."
  100)

(def ^:dynamic *short-delay-ms*
  "Short delay for polling/retry loops. Default 10ms."
  10)

(def ^:dynamic *long-timeout-ms*
  "Long timeout for operations that may take time. Default 5000ms."
  5000)

;; =============================================================================
;; Wait Functions
;; =============================================================================

(defn wait-async
  "Wait for async operations to settle.

   DRY: Replaces (Thread/sleep 100) throughout tests.

   Usage:
     (do-async-thing)
     (wait-async)
     (check-result)"
  []
  (Thread/sleep *async-settle-ms*))

(defn wait-channel
  "Wait for channel operations to complete.

   Usage:
     (send-to-channel data)
     (wait-channel)
     (verify-received)"
  []
  (Thread/sleep *channel-wait-ms*))

(defn wait-short
  "Short wait for polling loops.

   Usage:
     (loop []
       (if (ready?)
         result
         (do (wait-short) (recur))))"
  []
  (Thread/sleep *short-delay-ms*))

(defn wait-ms
  "Wait for specific milliseconds.

   Usage:
     (wait-ms 250)"
  [ms]
  (Thread/sleep ms))

;; =============================================================================
;; Retry Utilities
;; =============================================================================

(defn with-retry
  "Retry a function until it returns truthy or timeout.

   Usage:
     (with-retry #(check-condition) :timeout 1000 :interval 50)"
  [f & {:keys [timeout interval]
        :or {timeout *long-timeout-ms*
             interval *short-delay-ms*}}]
  (let [start (System/currentTimeMillis)]
    (loop []
      (if-let [result (f)]
        result
        (if (> (- (System/currentTimeMillis) start) timeout)
          nil
          (do
            (Thread/sleep interval)
            (recur)))))))
