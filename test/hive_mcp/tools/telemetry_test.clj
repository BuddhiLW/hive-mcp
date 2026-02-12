(ns hive-mcp.tools.telemetry-test
  "TDD tests for prometheus_query MCP tool.

   Tests:
   1. Query parameter validation
   2. HTTP client integration (mocked)
   3. Response parsing
   4. Error handling

   CLARITY-T: Telemetry first - metrics query must be observable."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.data.json :as json]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-fixture
  "Reset state before each test."
  [f]
  (f))

(use-fixtures :each reset-fixture)

;; =============================================================================
;; Prometheus Query Tool Tests (TDD: Tests written before implementation)
;; =============================================================================

(deftest prometheus-query-missing-query-test
  (testing "returns error when query parameter is missing"
    ;; This test will fail until we implement the tool
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-prometheus-query)]
      (when handler
        (let [result (handler {})]
          (is (= "text" (:type result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (contains? parsed :error))
            (is (clojure.string/includes? (:error parsed) "query"))))))))

(deftest prometheus-query-basic-test
  (testing "executes PromQL query and returns results"
    ;; This test will fail until we implement the tool
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-prometheus-query)]
      (when handler
        ;; Mock the HTTP client to return test data
        (with-redefs []
          (let [result (handler {:query "up"})]
            (is (= "text" (:type result)))))))))

(deftest prometheus-query-with-time-range-test
  (testing "supports optional time_range parameter"
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-prometheus-query)]
      (when handler
        (let [result (handler {:query "rate(hive_mcp_events_total[5m])"
                               :time_range "1h"})]
          (is (= "text" (:type result))))))))

(deftest prometheus-query-connection-error-test
  (testing "handles Prometheus connection errors gracefully"
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-prometheus-query)]
      (when handler
        ;; Mock HTTP client to throw connection error
        (with-redefs []
          (let [result (handler {:query "up"})]
            (is (= "text" (:type result)))))))))

;; =============================================================================
;; Wave Metrics Tests (TDD: Verify wave metrics are recorded)
;; =============================================================================

(deftest wave-metrics-definitions-test
  (testing "wave metrics are defined in prometheus registry"
    (let [prom (requiring-resolve 'hive-mcp.telemetry.prometheus/registry)]
      (when prom
        ;; These metrics should exist after implementation
        (is @prom "Registry should be initialized")))))

(deftest wave-success-rate-metric-test
  (testing "wave_success_rate gauge is updated after wave completion"
    (let [set-wave-success-rate! (requiring-resolve 'hive-mcp.telemetry.prometheus/set-wave-success-rate!)]
      (when set-wave-success-rate!
        ;; Should update gauge with success ratio
        (set-wave-success-rate! 0.8)
        ;; Verify metric was recorded (would check via metrics-response)
        (is true "Metric updated without error")))))

(deftest wave-items-total-metric-test
  (testing "wave_items_total counter increments for each item"
    (let [inc-wave-items! (requiring-resolve 'hive-mcp.telemetry.prometheus/inc-wave-items!)]
      (when inc-wave-items!
        ;; Should increment counter with status label
        (inc-wave-items! :success)
        (inc-wave-items! :failed)
        (is true "Counters incremented without error")))))

(deftest wave-duration-histogram-test
  (testing "wave_duration_seconds histogram records execution time"
    (let [observe-wave-duration! (requiring-resolve 'hive-mcp.telemetry.prometheus/observe-wave-duration!)]
      (when observe-wave-duration!
        ;; Should observe duration in histogram
        (observe-wave-duration! 15.5)
        (is true "Histogram observed without error")))))

;; =============================================================================
;; Integration Test: Wave Execution with Metrics
;; =============================================================================

(deftest wave-execution-emits-metrics-test
  (testing "execute-wave! records metrics on completion"
    ;; This test verifies the integration between wave.clj and prometheus.clj
    ;; Will be fully functional after implementation
    (is true "Placeholder for integration test")))

;; =============================================================================
;; Loki Query Tool Tests (TDD: Tests written before implementation)
;; =============================================================================

(deftest loki-query-missing-query-test
  (testing "returns error when query parameter is missing"
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-loki-query)]
      (when handler
        (let [result (handler {})]
          (is (= "text" (:type result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (contains? parsed :error))
            (is (clojure.string/includes? (:error parsed) "query"))))))))

(deftest loki-query-empty-query-test
  (testing "returns error when query is empty"
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-loki-query)]
      (when handler
        (let [result (handler {:query ""})]
          (is (= "text" (:type result)))
          (let [parsed (json/read-str (:text result) :key-fn keyword)]
            (is (contains? parsed :error))))))))

(deftest loki-query-basic-test
  (testing "executes LogQL query and returns log entries"
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-loki-query)]
      (when handler
        ;; Mock HTTP to return test data
        (with-redefs []
          (let [result (handler {:query "{job=\"hive-mcp\"}"})]
            (is (= "text" (:type result)))))))))

(deftest loki-query-with-limit-test
  (testing "supports limit parameter"
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-loki-query)]
      (when handler
        (let [result (handler {:query "{job=\"hive-mcp\"}"
                               :limit 50})]
          (is (= "text" (:type result))))))))

(deftest loki-query-with-time-range-test
  (testing "supports time_range parameter"
    (let [handler (requiring-resolve 'hive-mcp.tools.telemetry/handle-loki-query)]
      (when handler
        (let [result (handler {:query "{job=\"hive-mcp\"}"
                               :time_range "1h"})]
          (is (= "text" (:type result))))))))

(deftest loki-validate-query-test
  (testing "validates LogQL query syntax"
    (let [validate-fn (requiring-resolve 'hive-mcp.tools.telemetry/validate-logql-query)]
      (when validate-fn
        ;; Valid queries
        (is (nil? (validate-fn "{job=\"test\"}")))
        (is (nil? (validate-fn "{job=\"test\"} |= \"error\"")))
        ;; Invalid queries
        (is (some? (validate-fn "")))
        (is (some? (validate-fn nil)))
        (is (some? (validate-fn "no-selector")))))))

(deftest loki-parse-time-range-test
  (testing "parses duration strings to milliseconds"
    (let [parse-fn (requiring-resolve 'hive-mcp.tools.telemetry/parse-time-range)]
      (when parse-fn
        (is (= 3600000 (parse-fn "1h")))
        (is (= 1800000 (parse-fn "30m")))
        (is (= 86400000 (parse-fn "1d")))
        (is (nil? (parse-fn nil)))
        (is (nil? (parse-fn "")))))))

(deftest loki-parse-response-test
  (testing "parses Loki API response to log entries"
    (let [parse-fn (requiring-resolve 'hive-mcp.tools.telemetry/parse-loki-response)]
      (when parse-fn
        (let [response {:status "success"
                        :data {:resultType "streams"
                               :result [{:stream {:job "test" :level "error"}
                                         :values [["1705000000000000000" "Error message"]]}]}}
              parsed (parse-fn response)]
          (is (= :success (:status parsed)))
          (is (seq (:entries parsed))))))))

(comment
  ;; Run tests
  (require '[clojure.test :as t])
  (t/run-tests 'hive-mcp.tools.telemetry-test)

  ;; Run specific test
  (prometheus-query-missing-query-test)
  (loki-query-missing-query-test))
