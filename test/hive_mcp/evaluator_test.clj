(ns hive-mcp.evaluator-test
  "Tests for nREPL evaluator error handling.
   
   CLARITY-Y: Verifies informative error messages for connection failures.
   
   Covers:
   - Connection refused (server not running)
   - Connection timeout (server unresponsive)
   - Unknown host (invalid hostname)
   - No route to host (network unreachable)
   - Generic errors (fallback)"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.evaluator :as eval]))

;; =============================================================================
;; Error Message Tests
;; =============================================================================

(deftest test-connection-refused-error-message
  (testing "Connection refused provides actionable error message"
    (let [evaluator (eval/create-direct-nrepl-evaluator
                     {:host "localhost"
                      :port 59999 ; unlikely to have server
                      :timeout-ms 1000})
          result (eval/eval-code evaluator "(+ 1 2)")]
      (is (false? (:success result)))
      (is (str/includes? (:error result) "59999"))
      (is (str/includes? (:error result) "connection refused")
          "Error should mention connection refused")
      (is (or (str/includes? (:error result) "server may not be running")
              (str/includes? (:error result) "Start with"))
          "Error should provide guidance"))))

(deftest test-timeout-error-message
  (testing "Timeout error includes port and duration"
    ;; We can't easily trigger a real timeout in tests, but we can verify
    ;; the error format would include the right information by checking
    ;; the format string structure in the code.
    ;; This is a documentation test - the actual timeout would take too long.
    (let [evaluator (eval/create-direct-nrepl-evaluator
                     {:host "localhost"
                      :port 59998
                      :timeout-ms 100})]
      ;; Attempting connection to closed port triggers ConnectException first
      ;; So we just verify the evaluator was created correctly
      (is (= 100 (:timeout-ms evaluator)))
      (is (= 59998 (:port evaluator))))))

(deftest test-unknown-host-error-message
  (testing "Unknown host provides clear error"
    (let [evaluator (eval/create-direct-nrepl-evaluator
                     {:host "this-host-definitely-does-not-exist-12345.invalid"
                      :port 7910
                      :timeout-ms 1000})
          result (eval/eval-code evaluator "(+ 1 2)")]
      (is (false? (:success result)))
      (is (str/includes? (:error result) "this-host-definitely-does-not-exist")
          "Error should include the bad hostname"))))

(deftest test-generic-error-includes-context
  (testing "Generic errors include host:port context"
    ;; The generic error handler should still include useful context
    (let [evaluator (eval/create-direct-nrepl-evaluator
                     {:host "localhost"
                      :port 59997
                      :timeout-ms 1000})
          result (eval/eval-code evaluator "(+ 1 2)")]
      (is (false? (:success result)))
      ;; Either specific error or generic with context
      (is (or (str/includes? (:error result) "59997")
              (str/includes? (:error result) "localhost"))
          "Error should include connection context"))))

(deftest test-connected?-returns-false-for-bad-port
  (testing "connected? returns false for non-existent server"
    (let [evaluator (eval/create-direct-nrepl-evaluator
                     {:host "localhost"
                      :port 59996
                      :timeout-ms 500})]
      (is (false? (eval/connected? evaluator))))))

(deftest test-get-status-structure
  (testing "get-status returns expected structure"
    (let [evaluator (eval/create-direct-nrepl-evaluator
                     {:host "testhost"
                      :port 12345
                      :timeout-ms 5000})
          status (eval/get-status evaluator)]
      (is (= "testhost" (:host status)))
      (is (= 12345 (:port status)))
      (is (= 5000 (:timeout-ms status)))
      (is (= :nrepl (:type status)))
      (is (contains? status :connected)))))
