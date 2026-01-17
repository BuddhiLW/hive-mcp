(ns hive-mcp.drone-feedback-test
  "Unit tests for drone_feedback MCP tool.

   Tests the feedback mechanism for drones to report:
   - Tool limitations they encounter
   - Workflow friction points
   - Successful patterns
   - Suggestions for improvement

   TDD: Tests written first, implementation follows."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.tools.drone-feedback :as df]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.test-fixtures :as fixtures]))

;; =============================================================================
;; Test Fixtures and Helpers
;; =============================================================================

(def ^:dynamic *test-collection* "hive-mcp-test-drone-feedback")

(defn with-mock-embedder
  "Fixture that sets up mock embedder for testing."
  [f]
  (let [original-provider @@#'chroma/embedding-provider]
    (chroma/set-embedding-provider! (fixtures/->MockEmbedder 384))
    (chroma/configure! {:host "localhost"
                        :port 8000
                        :collection-name *test-collection*})
    (try
      (f)
      (finally
        (chroma/reset-collection-cache!)
        (reset! @#'chroma/embedding-provider original-provider)))))

(use-fixtures :each with-mock-embedder)

(defn parse-mcp-response
  "Parse MCP response - extracts :text and parses as JSON."
  [response]
  (-> response :text (json/read-str :key-fn keyword)))

;; =============================================================================
;; Handler Tests
;; =============================================================================

(deftest handle-drone-feedback-basic
  (testing "Basic feedback submission stores entry in memory"
    (let [result (df/handle-drone-feedback
                  {:category "tool-missing"
                   :message "Cannot execute bash commands in drone context"})
          parsed (parse-mcp-response result)]
      (is (some? (:id parsed)) "Should return entry ID")
      (is (= "note" (:type parsed)) "Should be stored as note type")
      (is (str/includes? (str (:tags parsed)) "drone-feedback")
          "Should include drone-feedback tag")
      (is (str/includes? (str (:tags parsed)) "feedback-tool-missing")
          "Should include category-specific tag"))))

(deftest handle-drone-feedback-all-categories
  (testing "All category types are accepted"
    (doseq [category ["tool-missing" "tool-broken" "workflow-friction" "suggestion" "success"]]
      (let [result (df/handle-drone-feedback
                    {:category category
                     :message (str "Test message for " category)})
            parsed (parse-mcp-response result)]
        (is (some? (:id parsed)) (str "Category " category " should be accepted"))
        (is (str/includes? (str (:tags parsed)) (str "feedback-" category))
            (str "Should include feedback-" category " tag"))))))

(deftest handle-drone-feedback-with-optional-fields
  (testing "Optional tool field is included in content"
    (let [result (df/handle-drone-feedback
                  {:category "tool-broken"
                   :tool "cider_eval_silent"
                   :message "Timeout after 30s"})
          parsed (parse-mcp-response result)]
      (is (str/includes? (:content parsed) "cider_eval_silent")
          "Content should include tool name")))

  (testing "Optional context field is included in content"
    (let [result (df/handle-drone-feedback
                  {:category "workflow-friction"
                   :message "File operations require multiple steps"
                   :context "Implementing TDD flow"})
          parsed (parse-mcp-response result)]
      (is (str/includes? (:content parsed) "TDD")
          "Content should include context"))))

(deftest handle-drone-feedback-agent-tagging
  (testing "Agent ID from explicit param is tagged"
    (let [result (df/handle-drone-feedback
                  {:category "success"
                   :message "Found workaround using CIDER"
                   :agent_id "drone-test-123"})
          parsed (parse-mcp-response result)]
      (is (str/includes? (str (:tags parsed)) "agent:drone-test-123")
          "Should include agent tag"))))

(deftest handle-drone-feedback-validation
  (testing "Missing message field returns error"
    (let [result (df/handle-drone-feedback {:category "tool-missing"})
          parsed (parse-mcp-response result)]
      (is (:error parsed) "Should return error for missing message")))

  (testing "Missing category field returns error"
    (let [result (df/handle-drone-feedback {:message "Test"})
          parsed (parse-mcp-response result)]
      (is (:error parsed) "Should return error for missing category")))

  (testing "Invalid category returns error"
    (let [result (df/handle-drone-feedback
                  {:category "invalid-category"
                   :message "Test"})
          parsed (parse-mcp-response result)]
      (is (:error parsed) "Should return error for invalid category"))))

(deftest handle-drone-feedback-content-format
  (testing "Feedback content is structured for readability"
    (let [result (df/handle-drone-feedback
                  {:category "tool-broken"
                   :tool "mcp__emacs__grep"
                   :message "Returns empty results for valid patterns"
                   :context "Searching for function definitions"})
          parsed (parse-mcp-response result)
          content (:content parsed)]
      (is (str/includes? content "Category:") "Should have category header")
      (is (str/includes? content "Tool:") "Should have tool header")
      (is (str/includes? content "Message:") "Should have message header")
      (is (str/includes? content "Context:") "Should have context header"))))

;; =============================================================================
;; Tool Definition Tests
;; =============================================================================

(deftest tool-definition-structure
  (testing "Tool has correct schema structure"
    (let [tool (first df/tools)]
      (is (= "drone_feedback" (:name tool)))
      (is (string? (:description tool)))
      (is (fn? (:handler tool)))
      (let [schema (:inputSchema tool)
            props (:properties schema)]
        (is (= "object" (:type schema)))
        (is (contains? props "category"))
        (is (contains? props "message"))
        (is (contains? props "tool"))
        (is (contains? props "context"))
        (is (= ["category" "message"] (:required schema)))))))
