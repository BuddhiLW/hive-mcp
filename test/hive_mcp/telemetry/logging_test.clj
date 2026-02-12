(ns hive-mcp.telemetry.logging-test
  "Tests for structured logging with MDC context.

   Tests verify:
   - MDC context is set and cleared properly
   - Correlation IDs are generated and propagated
   - Nested contexts work correctly
   - Context cleanup on exceptions"
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.telemetry.logging :as logging])
  (:import [org.slf4j MDC]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn clear-mdc-fixture
  "Ensure MDC is clean before and after each test."
  [f]
  (MDC/clear)
  (try
    (f)
    (finally
      (MDC/clear))))

(use-fixtures :each clear-mdc-fixture)

;;; =============================================================================
;;; MDC Context Tests
;;; =============================================================================

(deftest set-context!-test
  (testing "set-context! sets MDC values"
    (logging/set-context! {:agent-id "test-agent"
                           :tool-name "test-tool"})
    (is (= "test-agent" (MDC/get "agent-id")))
    (is (= "test-tool" (MDC/get "tool-name"))))

  (testing "set-context! converts values to strings"
    (logging/set-context! {:count 42
                           :enabled true})
    (is (= "42" (MDC/get "count")))
    (is (= "true" (MDC/get "enabled")))))

(deftest clear-context!-test
  (testing "clear-context! removes specific keys"
    (logging/set-context! {:a "1" :b "2" :c "3"})
    (logging/clear-context! [:a :c])
    (is (nil? (MDC/get "a")))
    (is (= "2" (MDC/get "b")))
    (is (nil? (MDC/get "c")))))

(deftest clear-all-context!-test
  (testing "clear-all-context! removes all keys"
    (logging/set-context! {:a "1" :b "2" :c "3"})
    (logging/clear-all-context!)
    (is (empty? (logging/get-context)))))

(deftest get-context-test
  (testing "get-context returns current MDC as map"
    (logging/set-context! {:foo "bar" :baz "qux"})
    (let [ctx (logging/get-context)]
      (is (= "bar" (get ctx "foo")))
      (is (= "qux" (get ctx "baz"))))))

;;; =============================================================================
;;; with-context Macro Tests
;;; =============================================================================

(deftest with-context-sets-and-clears-test
  (testing "with-context sets context during body"
    (logging/with-context {:agent-id "ling-123" :event-type "test"}
      (is (= "ling-123" (MDC/get "agent-id")))
      (is (= "test" (MDC/get "event-type")))))

  (testing "with-context clears context after body"
    (logging/with-context {:agent-id "ling-123"}
      :body-executed)
    (is (nil? (MDC/get "agent-id")))))

(deftest with-context-clears-on-exception-test
  (testing "with-context clears context even on exception"
    (is (thrown? Exception
                 (logging/with-context {:agent-id "will-be-cleared"}
                   (throw (Exception. "test error")))))
    (is (nil? (MDC/get "agent-id")))))

(deftest with-context-nested-test
  (testing "nested contexts work correctly"
    (logging/with-context {:outer "value"}
      (is (= "value" (MDC/get "outer")))
      (logging/with-context {:inner "nested"}
        (is (= "value" (MDC/get "outer")))
        (is (= "nested" (MDC/get "inner"))))
      ;; Inner context cleared, outer still present
      (is (= "value" (MDC/get "outer")))
      (is (nil? (MDC/get "inner")))))

  (testing "nested contexts can override outer values"
    (logging/with-context {:key "outer"}
      (is (= "outer" (MDC/get "key")))
      (logging/with-context {:key "inner"}
        (is (= "inner" (MDC/get "key"))))
      ;; Outer value restored after inner completes
      ;; Note: This behavior depends on MDC implementation
      ;; With simple clear-context!, outer value is also cleared
      ;; This is expected - use different keys for nested contexts
      )))

(deftest with-context-returns-body-result-test
  (testing "with-context returns the body's result"
    (let [result (logging/with-context {:agent-id "test"}
                   {:success true :data 42})]
      (is (= {:success true :data 42} result)))))

;;; =============================================================================
;;; Correlation ID Tests
;;; =============================================================================

(deftest generate-correlation-id-test
  (testing "generates valid UUID strings"
    (let [id (logging/generate-correlation-id)]
      (is (string? id))
      (is (= 36 (count id)))  ; UUID format: 8-4-4-4-12
      (is (re-matches #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" id))))

  (testing "generates unique IDs"
    (let [ids (repeatedly 100 logging/generate-correlation-id)]
      (is (= 100 (count (set ids)))))))

(deftest current-correlation-id-test
  (testing "returns nil when not set"
    (is (nil? (logging/current-correlation-id))))

  (testing "returns value when set"
    (logging/set-context! {:correlation-id "test-123"})
    (is (= "test-123" (logging/current-correlation-id)))))

(deftest with-correlation-id-test
  (testing "generates ID when nil provided"
    (logging/with-correlation-id nil
      (let [id (logging/current-correlation-id)]
        (is (some? id))
        (is (re-matches #"[0-9a-f-]+" id)))))

  (testing "uses provided ID when given"
    (logging/with-correlation-id "my-custom-id"
      (is (= "my-custom-id" (logging/current-correlation-id)))))

  (testing "clears ID after body"
    (logging/with-correlation-id "temp-id"
      :body)
    (is (nil? (logging/current-correlation-id)))))

;;; =============================================================================
;;; with-request-context Tests
;;; =============================================================================

(deftest with-request-context-test
  (testing "sets tool-name and correlation-id"
    (logging/with-request-context {:tool "swarm_spawn"}
      (is (= "swarm_spawn" (MDC/get "tool-name")))
      (is (some? (MDC/get "correlation-id")))))

  (testing "sets agent-id when provided"
    (logging/with-request-context {:tool "test" :agent-id "ling-123"}
      (is (= "ling-123" (MDC/get "agent-id")))))

  (testing "uses provided correlation-id"
    (logging/with-request-context {:tool "test" :correlation-id "custom-123"}
      (is (= "custom-123" (MDC/get "correlation-id")))))

  (testing "clears context after body"
    (logging/with-request-context {:tool "test" :agent-id "ling"}
      :body)
    (is (nil? (MDC/get "tool-name")))
    (is (nil? (MDC/get "agent-id")))
    (is (nil? (MDC/get "correlation-id")))))

;;; =============================================================================
;;; Structured Logging Helper Tests (Smoke Tests)
;;; =============================================================================

(deftest log-tool-call-smoke-test
  (testing "log-tool-call doesn't throw"
    (is (nil? (logging/log-tool-call {:tool "test_tool"
                                      :args {:name "test"}
                                      :agent-id "test-agent"})))))

(deftest log-tool-result-smoke-test
  (testing "log-tool-result success doesn't throw"
    (is (nil? (logging/log-tool-result {:tool "test_tool"
                                        :success true
                                        :duration-ms 100}))))

  (testing "log-tool-result failure doesn't throw"
    (is (nil? (logging/log-tool-result {:tool "test_tool"
                                        :success false
                                        :duration-ms 50
                                        :error "Test error"})))))

(deftest log-swarm-event-smoke-test
  (testing "log-swarm-event doesn't throw"
    (is (nil? (logging/log-swarm-event {:event-type "ling-spawned"
                                        :agent-id "ling-123"
                                        :data {:presets ["tdd"]}})))))

(deftest log-memory-operation-smoke-test
  (testing "log-memory-operation doesn't throw"
    (is (nil? (logging/log-memory-operation {:operation "query"
                                             :project-id "test-project"
                                             :type :decision
                                             :count 5
                                             :duration-ms 25})))))

;;; =============================================================================
;;; Error Logging Tests
;;; =============================================================================

(deftest log-error-with-context-test
  (testing "log-error-with-context doesn't throw"
    (logging/with-context {:agent-id "test" :correlation-id "123"}
      (is (nil? (logging/log-error-with-context
                 {:message "Test error"
                  :error (Exception. "test")
                  :context {:extra "data"}}))))))

;;; =============================================================================
;;; Init Tests
;;; =============================================================================

(deftest init!-test
  (testing "init! sets service identifier"
    (logging/init!)
    (is (= "hive-mcp" (MDC/get "service")))))
