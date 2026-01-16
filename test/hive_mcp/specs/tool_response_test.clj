(ns hive-mcp.specs.tool-response-test
  "Tests for tool response specs with instrumentation.

   Tests validate:
   - Content item specs (text/image)
   - Tool response structure
   - Piggyback message format
   - Hivemind section detection
   - Function specs via instrumentation

   CLARITY: 'Inputs are guarded' - specs provide declarative validation"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [hive-mcp.specs.tool-response :as spec]))

;; =============================================================================
;; Test Fixture: Enable Instrumentation
;; =============================================================================

(defn instrument-fixture
  "Enable spec instrumentation for all fdef'd functions during tests."
  [f]
  ;; Instrument the specs
  (stest/instrument `hive-mcp.server.routes/make-tool)
  (stest/instrument `hive-mcp.tools.core/get-hivemind-piggyback)
  (try
    (f)
    (finally
      ;; Unstrument after tests
      (stest/unstrument `hive-mcp.server.routes/make-tool)
      (stest/unstrument `hive-mcp.tools.core/get-hivemind-piggyback))))

(use-fixtures :once instrument-fixture)

;; =============================================================================
;; Content Item Tests
;; =============================================================================

(deftest test-text-content-item-spec
  (testing "Valid text content items"
    (is (s/valid? ::spec/text-content-item {:type "text" :text "Hello"}))
    (is (s/valid? ::spec/text-content-item {:type "text" :text ""}))
    (is (s/valid? ::spec/text-content-item {:type "text" :text "Multi\nline\ntext"})))

  (testing "Invalid text content items"
    (is (not (s/valid? ::spec/text-content-item {:type "image" :text "hello"})))
    (is (not (s/valid? ::spec/text-content-item {:type "text"})))
    (is (not (s/valid? ::spec/text-content-item {:text "hello"})))
    (is (not (s/valid? ::spec/text-content-item {})))
    (is (not (s/valid? ::spec/text-content-item {:type "text" :text 123})))))

(deftest test-image-content-item-spec
  (testing "Valid image content items"
    (is (s/valid? ::spec/image-content-item
                  {:type "image" :data "base64data" :mimeType "image/png"}))
    (is (s/valid? ::spec/image-content-item
                  {:type "image" :data "" :mimeType "image/jpeg"})))

  (testing "Invalid image content items"
    (is (not (s/valid? ::spec/image-content-item {:type "text" :data "x" :mimeType "y"})))
    (is (not (s/valid? ::spec/image-content-item {:type "image" :data "x"})))
    (is (not (s/valid? ::spec/image-content-item {:type "image" :mimeType "y"})))))

(deftest test-content-item-union-spec
  (testing "Content item accepts text"
    (is (s/valid? ::spec/content-item {:type "text" :text "hello"})))

  (testing "Content item accepts image"
    (is (s/valid? ::spec/content-item
                  {:type "image" :data "base64" :mimeType "image/png"})))

  (testing "Content item rejects invalid types"
    (is (not (s/valid? ::spec/content-item {:type "video" :url "http://..."})))
    (is (not (s/valid? ::spec/content-item {})))))

(deftest test-content-array-spec
  (testing "Empty content array is valid"
    (is (s/valid? ::spec/content-array [])))

  (testing "Content array with text items"
    (is (s/valid? ::spec/content-array
                  [{:type "text" :text "a"}
                   {:type "text" :text "b"}])))

  (testing "Content array with mixed items"
    (is (s/valid? ::spec/content-array
                  [{:type "text" :text "hello"}
                   {:type "image" :data "x" :mimeType "image/png"}
                   {:type "text" :text "world"}])))

  (testing "Content array must be vector"
    (is (not (s/valid? ::spec/content-array
                       '({:type "text" :text "list"}))))))

;; =============================================================================
;; Tool Response Tests
;; =============================================================================

(deftest test-tool-response-spec
  (testing "Valid tool response with content"
    (is (s/valid? ::spec/tool-response
                  {:content [{:type "text" :text "result"}]})))

  (testing "Valid tool response with empty content"
    (is (s/valid? ::spec/tool-response
                  {:content []})))

  (testing "Valid tool response with isError"
    (is (s/valid? ::spec/tool-response
                  {:content [{:type "text" :text "error"}] :isError true}))
    (is (s/valid? ::spec/tool-response
                  {:content [{:type "text" :text "ok"}] :isError false})))

  (testing "Invalid tool response - missing content"
    (is (not (s/valid? ::spec/tool-response {}))))

  (testing "Invalid tool response - content not vector"
    (is (not (s/valid? ::spec/tool-response {:content "text"})))))

;; =============================================================================
;; Piggyback Message Tests
;; =============================================================================

(deftest test-piggyback-message-spec
  (testing "Valid piggyback message"
    (is (s/valid? ::spec/piggyback-message {:a "agent-1" :e "progress" :m "working"}))
    (is (s/valid? ::spec/piggyback-message {:a "worker" :e "completed" :m ""})))

  (testing "Invalid piggyback message - empty agent-id"
    (is (not (s/valid? ::spec/piggyback-message {:a "" :e "progress" :m "msg"}))))

  (testing "Invalid piggyback message - empty event-type"
    (is (not (s/valid? ::spec/piggyback-message {:a "agent" :e "" :m "msg"}))))

  (testing "Invalid piggyback message - missing keys"
    (is (not (s/valid? ::spec/piggyback-message {:a "agent" :e "progress"})))
    (is (not (s/valid? ::spec/piggyback-message {:a "agent" :m "msg"})))
    (is (not (s/valid? ::spec/piggyback-message {:e "progress" :m "msg"})))))

(deftest test-piggyback-messages-spec
  (testing "Nil is valid piggyback-messages"
    (is (s/valid? ::spec/piggyback-messages nil)))

  (testing "Empty vector is valid"
    (is (s/valid? ::spec/piggyback-messages [])))

  (testing "Vector of messages is valid"
    (is (s/valid? ::spec/piggyback-messages
                  [{:a "a1" :e "started" :m "msg1"}
                   {:a "a2" :e "progress" :m "msg2"}])))

  (testing "List is invalid (must be vector)"
    (is (not (s/valid? ::spec/piggyback-messages
                       '({:a "a1" :e "started" :m "msg1"}))))))

;; =============================================================================
;; Hivemind Section Tests
;; =============================================================================

(deftest test-hivemind-section-spec
  (testing "Valid hivemind section"
    (is (s/valid? ::spec/hivemind-section
                  "result\n\n---HIVEMIND---\n[{:a \"x\" :e \"y\" :m \"z\"}]\n---/HIVEMIND---"))
    (is (s/valid? ::spec/hivemind-section
                  "---HIVEMIND---\n[]\n---/HIVEMIND---")))

  (testing "Invalid hivemind section - missing opening"
    (is (not (s/valid? ::spec/hivemind-section
                       "result\n[{:a \"x\"}]\n---/HIVEMIND---"))))

  (testing "Invalid hivemind section - missing closing"
    (is (not (s/valid? ::spec/hivemind-section
                       "---HIVEMIND---\n[{:a \"x\"}]"))))

  (testing "Invalid hivemind section - not a string"
    (is (not (s/valid? ::spec/hivemind-section [{:a "x" :e "y" :m "z"}])))))

(deftest test-hivemind-section-helper
  (testing "valid-hivemind-section? helper"
    (is (spec/valid-hivemind-section?
         "---HIVEMIND---\n[]\n---/HIVEMIND---"))
    (is (not (spec/valid-hivemind-section? "plain text")))
    (is (not (spec/valid-hivemind-section? nil)))))

;; =============================================================================
;; Tool Definition Tests
;; =============================================================================

(deftest test-tool-def-spec
  (testing "Valid tool definition"
    (is (s/valid? ::spec/tool-def
                  {:name "test-tool"
                   :description "A test tool"
                   :inputSchema {:type "object"}
                   :handler (fn [_] {:type "text" :text "ok"})})))

  (testing "Invalid tool definition - missing name"
    (is (not (s/valid? ::spec/tool-def
                       {:description "test"
                        :inputSchema {}
                        :handler identity}))))

  (testing "Invalid tool definition - empty name"
    (is (not (s/valid? ::spec/tool-def
                       {:name ""
                        :description "test"
                        :inputSchema {}
                        :handler identity}))))

  (testing "Invalid tool definition - handler not a function"
    (is (not (s/valid? ::spec/tool-def
                       {:name "test"
                        :description "test"
                        :inputSchema {}
                        :handler "not-a-fn"})))))

;; =============================================================================
;; Validation Helper Tests
;; =============================================================================

(deftest test-valid-content-item-helper
  (testing "valid-content-item? helper"
    (is (spec/valid-content-item? {:type "text" :text "hello"}))
    (is (not (spec/valid-content-item? {:type "invalid"})))))

(deftest test-valid-tool-response-helper
  (testing "valid-tool-response? helper"
    (is (spec/valid-tool-response?
         {:content [{:type "text" :text "ok"}]}))
    (is (not (spec/valid-tool-response? {})))))

(deftest test-valid-piggyback-message-helper
  (testing "valid-piggyback-message? helper"
    (is (spec/valid-piggyback-message? {:a "x" :e "y" :m "z"}))
    (is (not (spec/valid-piggyback-message? {:a "x"})))))

(deftest test-explain-content-item
  (testing "explain-content-item returns nil for valid"
    (is (nil? (spec/explain-content-item {:type "text" :text "hello"}))))

  (testing "explain-content-item returns problems for invalid"
    (let [result (spec/explain-content-item {:type "invalid"})]
      (is (some? result))
      (is (contains? result ::s/problems)))))

(deftest test-explain-tool-response
  (testing "explain-tool-response returns nil for valid"
    (is (nil? (spec/explain-tool-response
               {:content [{:type "text" :text "ok"}]}))))

  (testing "explain-tool-response returns problems for invalid"
    (let [result (spec/explain-tool-response {})]
      (is (some? result))
      (is (contains? result ::s/problems)))))

(deftest test-valid-tool-def-helper
  (testing "valid-tool-def? helper"
    (is (spec/valid-tool-def?
         {:name "x" :description "y" :inputSchema {} :handler identity}))
    (is (not (spec/valid-tool-def? {})))))

;; =============================================================================
;; Instrumentation Verification Tests
;; =============================================================================

(deftest test-instrumentation-enabled
  (testing "Instrumentation is active for make-tool"
    ;; Verify the spec exists
    (is (some? (s/get-spec `hive-mcp.server.routes/make-tool))
        "make-tool should have an fdef spec"))

  (testing "Instrumentation is active for get-hivemind-piggyback"
    (is (some? (s/get-spec `hive-mcp.tools.core/get-hivemind-piggyback))
        "get-hivemind-piggyback should have an fdef spec")))

;; =============================================================================
;; Edge Case Tests
;; =============================================================================

(deftest test-content-item-with-extra-keys
  (testing "Content items allow extra keys (open spec)"
    ;; MCP spec allows extensions
    (is (s/valid? ::spec/text-content-item
                  {:type "text" :text "hello" :extra "ignored"}))))

(deftest test-deep-nested-hivemind
  (testing "Hivemind section with complex content"
    (let [complex-section "Result text\n\n---HIVEMIND---\n[{:a \"agent-with-long-id-123\" :e \"progress\" :m \"Step 1/10: Processing file.clj\"} {:a \"agent-2\" :e \"completed\" :m \"Done!\"}]\n---/HIVEMIND---"]
      (is (s/valid? ::spec/hivemind-section complex-section)))))
