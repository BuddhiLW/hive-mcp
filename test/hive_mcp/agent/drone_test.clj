(ns hive-mcp.agent.drone-test
  "Tests for drone delegation with structured nREPL error handling.

   TDD-first tests for:
   - nREPL error classification (:nrepl-connection, :nrepl-timeout, :nrepl-eval-error)
   - Structured error data (error-type, message, stacktrace)
   - Prometheus metric integration (drones_failed_total with error-type label)"
  (:require [clojure.test :refer :all]
            [hive-mcp.agent.drone :as drone]))

;; =============================================================================
;; Error Classification Tests
;; =============================================================================

(deftest classify-nrepl-connection-error
  (testing "Connection errors are classified as :nrepl-connection"
    ;; Test with actual ConnectException as cause
    (let [ex-with-cause (ex-info "Operation failed"
                                 {:cause (java.net.ConnectException. "Connection refused")})]
      (is (= :nrepl-connection (drone/classify-nrepl-error ex-with-cause))
          "Should classify ConnectException cause as :nrepl-connection"))

    ;; Test message-based detection
    (doseq [message ["Connection refused"
                     "CIDER not connected"
                     "Failed to connect to nREPL server at port 7888"
                     "java.net.ConnectException: Connection refused"
                     "No nREPL connection"]]
      (let [ex (ex-info message {})]
        (is (= :nrepl-connection (drone/classify-nrepl-error ex))
            (str "Should classify '" message "' as :nrepl-connection"))))))

(deftest classify-nrepl-timeout-error
  (testing "Timeout errors are classified as :nrepl-timeout"
    ;; Test with actual SocketTimeoutException as cause
    (let [ex-with-cause (ex-info "Operation failed"
                                 {:cause (java.net.SocketTimeoutException. "Read timed out")})]
      (is (= :nrepl-timeout (drone/classify-nrepl-error ex-with-cause))
          "Should classify SocketTimeoutException cause as :nrepl-timeout"))

    ;; Test message-based detection
    (doseq [message ["Read timed out"
                     "nREPL response timeout after 30000ms"
                     "Evaluation timed out"
                     "java.util.concurrent.TimeoutException"]]
      (let [ex (ex-info message {})]
        (is (= :nrepl-timeout (drone/classify-nrepl-error ex))
            (str "Should classify '" message "' as :nrepl-timeout"))))))

(deftest classify-nrepl-eval-error
  (testing "Evaluation errors are classified as :nrepl-eval-error"
    (doseq [message ["CompilerException java.lang.RuntimeException: Unable to resolve symbol: foo"
                     "Syntax error compiling at (REPL:1:1)"
                     "ClassNotFoundException: some.missing.Class"
                     "java.lang.ArithmeticException: Divide by zero"
                     "NullPointerException"]]
      (let [ex (ex-info message {})]
        (is (= :nrepl-eval-error (drone/classify-nrepl-error ex))
            (str "Should classify '" message "' as :nrepl-eval-error"))))))

(deftest classify-validation-error
  (testing "Validation errors are classified as :validation"
    (let [ex (ex-info "Invalid task parameter" {:type :validation})]
      (is (= :validation (drone/classify-nrepl-error ex))
          "Should classify validation errors as :validation"))))

(deftest classify-unknown-error
  (testing "Unknown errors fall back to :exception"
    (let [ex (ex-info "Some unknown error happened" {})]
      (is (= :exception (drone/classify-nrepl-error ex))
          "Should classify unknown errors as :exception"))))

;; =============================================================================
;; Structured Error Data Tests
;; =============================================================================

(deftest structure-error-includes-required-fields
  (testing "structure-error returns map with error-type, message, stacktrace"
    (let [ex (ex-info "Connection refused" {:cause (java.net.ConnectException. "Connection refused")})
          structured (drone/structure-error ex)]
      (is (map? structured) "Should return a map")
      (is (contains? structured :error-type) "Should contain :error-type")
      (is (contains? structured :message) "Should contain :message")
      (is (contains? structured :stacktrace) "Should contain :stacktrace")
      (is (keyword? (:error-type structured)) "error-type should be a keyword")
      (is (string? (:message structured)) "message should be a string")
      (is (or (nil? (:stacktrace structured))
              (string? (:stacktrace structured))) "stacktrace should be string or nil"))))

(deftest structure-error-preserves-exception-data
  (testing "structure-error preserves ex-data from original exception"
    (let [ex-data-map {:conflicts ["file1.clj" "file2.clj"]
                       :drone-id "drone-123"}
          ex (ex-info "File conflicts detected" ex-data-map)
          structured (drone/structure-error ex)]
      (is (= ex-data-map (:ex-data structured))
          "Should preserve original ex-data"))))

;; =============================================================================
;; Integration: Error Type to Prometheus Label
;; =============================================================================

(deftest error-type-valid-for-prometheus
  (testing "All error types are valid Prometheus label values"
    (let [valid-types #{:nrepl-connection :nrepl-timeout :nrepl-eval-error
                        :validation :exception :conflict :execution}
          test-cases [{:message "Connection refused" :expected :nrepl-connection}
                      {:message "Read timed out" :expected :nrepl-timeout}
                      {:message "Syntax error compiling" :expected :nrepl-eval-error}
                      {:message "Unknown error" :expected :exception}]]
      (doseq [{:keys [message expected]} test-cases]
        (let [ex (ex-info message {})
              error-type (drone/classify-nrepl-error ex)]
          (is (contains? valid-types error-type)
              (str "Error type " error-type " should be in valid-types set")))))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest classify-nested-cause-exception
  (testing "Classifies based on nested cause exception"
    (let [root-cause (java.net.ConnectException. "Connection refused")
          wrapper (ex-info "Operation failed" {:cause root-cause})]
      (is (= :nrepl-connection (drone/classify-nrepl-error wrapper))
          "Should detect ConnectException in nested cause"))))

(deftest classify-nil-message
  (testing "Handles nil exception message gracefully"
    (let [ex (ex-info nil {})]
      (is (keyword? (drone/classify-nrepl-error ex))
          "Should return a valid keyword even with nil message"))))

(deftest structure-error-with-nil-values
  (testing "structure-error handles exceptions with nil message"
    (let [ex (Exception.)
          structured (drone/structure-error ex)]
      (is (map? structured) "Should return a map")
      (is (contains? structured :error-type) "Should contain :error-type"))))

(comment
  ;; Run tests
  (run-tests 'hive-mcp.agent.drone-test))
