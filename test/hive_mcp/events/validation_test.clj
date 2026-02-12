(ns hive-mcp.events.validation-test
  "Tests for POC-14: validation interceptor.
   
   Verifies:
   - Valid events pass through
   - Invalid event structure throws
   - Invalid event data throws with schema
   - Error messages are descriptive"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.events.core :as core]))

;; =============================================================================
;; POC-14: Structure Validation Tests
;; =============================================================================

(deftest test-validate-event-passes-valid-event
  (testing "Valid event passes through validation interceptor"
    (let [interceptor (core/validate-event)
          context {:coeffects {:event [:test/event {:id "123"}]}
                   :effects {}
                   :queue []
                   :stack []}
          result ((:before interceptor) context)]
      (is (= context result) "Context should be unchanged for valid events"))))

(deftest test-validate-event-passes-minimal-event
  (testing "Minimal valid event (just keyword) passes"
    (let [interceptor (core/validate-event)
          context {:coeffects {:event [:noop]}
                   :effects {}
                   :queue []
                   :stack []}
          result ((:before interceptor) context)]
      (is (= context result)))))

(deftest test-validate-event-throws-on-non-vector
  (testing "Non-vector event throws with descriptive error"
    (let [interceptor (core/validate-event)
          context {:coeffects {:event "not-a-vector"}
                   :effects {}
                   :queue []
                   :stack []}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid event structure"
                            ((:before interceptor) context))))))

(deftest test-validate-event-throws-on-non-keyword-first
  (testing "Event with non-keyword first element throws"
    (let [interceptor (core/validate-event)
          context {:coeffects {:event ["string-not-keyword" {:data "foo"}]}
                   :effects {}
                   :queue []
                   :stack []}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid event structure"
                            ((:before interceptor) context))))))

(deftest test-validate-event-error-includes-event
  (testing "Error includes the invalid event in ex-data"
    (let [interceptor (core/validate-event)
          context {:coeffects {:event {:not "a-vector"}}
                   :effects {}
                   :queue []
                   :stack []}]
      (try
        ((:before interceptor) context)
        (is false "Should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= {:not "a-vector"} (:event (ex-data e))))
          (is (= :structure (:schema-type (ex-data e)))))))))

;; =============================================================================
;; POC-14: Data Schema Validation Tests
;; =============================================================================

(def TaskSchema
  "Example malli schema for task events"
  [:map
   [:id :string]
   [:title :string]])

(deftest test-validate-event-with-schema-passes-valid-data
  (testing "Event with valid data matching schema passes"
    (let [interceptor (core/validate-event TaskSchema)
          context {:coeffects {:event [:task/create {:id "123" :title "Test task"}]}
                   :effects {}
                   :queue []
                   :stack []}
          result ((:before interceptor) context)]
      (is (= context result)))))

(deftest test-validate-event-with-schema-throws-on-missing-field
  (testing "Event missing required schema field throws"
    (let [interceptor (core/validate-event TaskSchema)
          context {:coeffects {:event [:task/create {:id "123"}]} ; missing :title
                   :effects {}
                   :queue []
                   :stack []}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid event data"
                            ((:before interceptor) context))))))

(deftest test-validate-event-with-schema-throws-on-wrong-type
  (testing "Event with wrong type for field throws"
    (let [interceptor (core/validate-event TaskSchema)
          context {:coeffects {:event [:task/create {:id 123 :title "Test"}]} ; :id should be string
                   :effects {}
                   :queue []
                   :stack []}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid event data"
                            ((:before interceptor) context))))))

(deftest test-validate-event-schema-error-includes-humanized-message
  (testing "Schema validation error includes humanized message"
    (let [interceptor (core/validate-event TaskSchema)
          context {:coeffects {:event [:task/create {:wrong "keys"}]}
                   :effects {}
                   :queue []
                   :stack []}]
      (try
        ((:before interceptor) context)
        (is false "Should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :data (:schema-type (ex-data e))))
          (is (some? (:error (ex-data e))) "Should have humanized error")
          (is (= {:wrong "keys"} (:event-data (ex-data e)))))))))

(deftest test-validate-event-with-schema-validates-structure-first
  (testing "With schema, structure validation happens before data validation"
    (let [interceptor (core/validate-event TaskSchema)
          context {:coeffects {:event "not-even-a-vector"}
                   :effects {}
                   :queue []
                   :stack []}]
      (try
        ((:before interceptor) context)
        (is false "Should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :structure (:schema-type (ex-data e)))
              "Structure validation should fail first"))))))

;; =============================================================================
;; POC-14: Integration Tests
;; =============================================================================

(deftest test-validate-event-interceptor-has-correct-id
  (testing "Interceptor has :validate-event id"
    (is (= :validate-event (:id (core/validate-event))))
    (is (= :validate-event (:id (core/validate-event TaskSchema))))))

(deftest test-validate-event-interceptor-is-valid-interceptor
  (testing "validate-event returns valid interceptor structure"
    (let [interceptor (core/validate-event)]
      (is (core/interceptor? interceptor))
      (is (fn? (:before interceptor)))
      (is (fn? (:after interceptor))))))
