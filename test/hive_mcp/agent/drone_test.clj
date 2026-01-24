(ns hive-mcp.agent.drone-test
  "Tests for drone delegation with structured nREPL error handling.

   TDD-first tests for:
   - nREPL error classification (:nrepl-connection, :nrepl-timeout, :nrepl-eval-error)
   - Structured error data (error-type, message, stacktrace)
   - Prometheus metric integration (drones_failed_total with error-type label)
   - Preset selection and task classification
   - Parser code extraction and confidence scoring
   - Validation result structures"
  (:require [clojure.test :refer :all]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.agent.drone.parser :as parser]
            [hive-mcp.agent.drone.tools :as tools]
            [hive-mcp.agent.drone.validation :as validation]
            [hive-mcp.agent.drone.context :as context]))

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

;; =============================================================================
;; Preset Selection Tests (drone.preset)
;; =============================================================================

(deftest test-preset-selection
  (testing "Testing tasks select drone-test-writer preset"
    (is (= "drone-test-writer"
           (preset/select-drone-preset "write tests for foo" nil)))
    (is (= "drone-test-writer"
           (preset/select-drone-preset "add unit test coverage" nil)))
    (is (= "drone-test-writer"
           (preset/select-drone-preset "general task" ["test/foo_test.clj"]))))

  (testing "Bugfix tasks select drone-bugfix preset"
    (is (= "drone-bugfix"
           (preset/select-drone-preset "fix the NPE bug" nil)))
    (is (= "drone-bugfix"
           (preset/select-drone-preset "resolve null pointer exception" nil))))

  (testing "Refactoring tasks select drone-refactor preset"
    (is (= "drone-refactor"
           (preset/select-drone-preset "refactor the validation logic" nil)))
    (is (= "drone-refactor"
           (preset/select-drone-preset "extract method from process-order" nil))))

  (testing "Documentation tasks select drone-docs preset"
    (is (= "drone-docs"
           (preset/select-drone-preset "add docstring to foo" nil)))
    (is (= "drone-docs"
           (preset/select-drone-preset "general task" ["README.md"]))))

  (testing "General tasks fall back to drone-worker"
    (is (= "drone-worker"
           (preset/select-drone-preset "implement feature X" nil)))))

;; =============================================================================
;; Task Classification Tests (drone.preset, drone.tools)
;; =============================================================================

(deftest test-task-type-classification
  (testing "get-task-type classifies tasks correctly"
    (is (= :testing (preset/get-task-type "add unit tests" nil)))
    (is (= :bugfix (preset/get-task-type "fix null pointer" nil)))
    (is (= :refactoring (preset/get-task-type "refactor this function" nil)))
    (is (= :documentation (preset/get-task-type "add docstring" nil)))
    (is (= :general (preset/get-task-type "implement feature" nil)))))

(deftest test-tool-profiles
  (testing "Testing tasks get eval tools"
    (let [tools (set (tools/get-tools-for-drone :testing nil))]
      (is (contains? tools "propose_diff") "Core tool always included")
      (is (contains? tools "hivemind_shout") "Core tool always included")
      (is (contains? tools "cider_eval_silent") "Testing gets eval tool")))

  (testing "Documentation tasks get minimal tools"
    (let [tools (set (tools/get-tools-for-drone :documentation nil))]
      (is (contains? tools "propose_diff"))
      (is (contains? tools "read_file"))
      (is (not (contains? tools "grep")) "Docs don't need grep")))

  (testing "Nil task-type returns legacy full set"
    (let [tools (set (tools/get-tools-for-drone nil nil))]
      (is (> (count tools) 10) "Legacy set has many tools"))))

;; =============================================================================
;; Parser Extraction Tests (drone.parser)
;; =============================================================================

(deftest test-parser-code-extraction
  (testing "Extracts code from markdown blocks"
    (let [response "Here's the code:\n```clojure\n(defn foo [])\n```"
          blocks (parser/extract-code-blocks response)]
      (is (seq blocks) "Should extract blocks")
      (is (= "(defn foo [])" (first blocks)))))

  (testing "Extracts code from plain ``` blocks"
    (let [response "```\n(defn bar [x] x)\n```"
          blocks (parser/extract-code-blocks response)]
      (is (seq blocks))
      (is (= "(defn bar [x] x)" (first blocks)))))

  (testing "Extracts multiple code blocks"
    (let [response "First:\n```clj\n(defn a [])\n```\nSecond:\n```clojure\n(defn b [])\n```"
          blocks (parser/extract-code-blocks response)]
      (is (= 2 (count blocks)))))

  (testing "Falls back to raw response if looks like code"
    (let [response "(defn foo [x] (inc x))"
          blocks (parser/extract-code-blocks response)]
      (is (seq blocks))
      (is (= response (first blocks))))))

(deftest test-parser-looks-like-code
  (testing "Identifies Clojure code patterns"
    (is (parser/looks-like-code? "(defn foo [])"))
    (is (parser/looks-like-code? "(ns my.namespace (:require [foo]))"))
    (is (parser/looks-like-code? "(let [x 1] x)")))

  (testing "Rejects non-code text"
    (is (not (parser/looks-like-code? "Hello world")))
    (is (not (parser/looks-like-code? "This is a sentence.")))))

(deftest test-parser-valid-clojure
  (testing "Validates correct Clojure syntax"
    (is (parser/valid-clojure? "(defn foo [x] x)"))
    (is (parser/valid-clojure? "(+ 1 2 3)")))

  (testing "Rejects invalid Clojure syntax"
    (is (not (parser/valid-clojure? "(defn foo [x")))
    (is (not (parser/valid-clojure? "}{")))))

(deftest test-parser-confidence-scoring
  (testing "High confidence for valid code matching task"
    (let [blocks ["(defn process-order [order] order)"]
          task "implement process-order function"
          result (parser/extraction-confidence blocks task)]
      (is (:has-code? result))
      (is (:syntax-valid? result))
      (is (>= (:confidence result) 0.7))))

  (testing "Low confidence for no code"
    (let [result (parser/extraction-confidence [] "some task")]
      (is (not (:has-code? result)))
      (is (= 0.0 (:confidence result)))))

  (testing "recovery-action suggests appropriate action"
    (is (= :report-failure (parser/recovery-action {:has-code? false :confidence 0.0})))
    (is (= :accept (parser/recovery-action {:has-code? true :confidence 0.8 :block-count 1})))
    (is (= :ask-clarify (parser/recovery-action {:has-code? true :confidence 0.2 :block-count 1})))))

;; =============================================================================
;; Validation Structure Tests (drone.validation)
;; =============================================================================

(deftest test-validation-result-structure
  (testing "make-validation-result creates proper structure"
    (let [result (validation/make-validation-result
                  {:pre-valid? true
                   :post-valid? true
                   :lint-errors []
                   :diff-lines 10
                   :warnings []})]
      (is (:pre-valid? result))
      (is (:post-valid? result))
      (is (vector? (:lint-errors result)))
      (is (= 10 (:diff-lines result)))
      (is (vector? (:warnings result)))))

  (testing "Defaults are applied correctly"
    (let [result (validation/make-validation-result {})]
      (is (false? (:pre-valid? result)))
      (is (nil? (:post-valid? result)))
      (is (= [] (:lint-errors result)))
      (is (nil? (:diff-lines result)))
      (is (= [] (:warnings result))))))

(deftest test-validation-summarize
  (testing "summarize-validation aggregates results"
    (let [results {"file1.clj" {:pre-valid? true :post-valid? true
                                :lint-errors [] :diff-lines 5 :warnings []}
                   "file2.clj" {:pre-valid? true :post-valid? false
                                :lint-errors [{:level :error}] :diff-lines 3
                                :warnings ["warning1"]}}
          summary (validation/summarize-validation results)]
      (is (= 2 (:total-files summary)))
      (is (= 2 (:pre-valid summary)))
      (is (= 1 (:post-valid summary)))
      (is (= 1 (:post-invalid summary)))
      (is (= 1 (:total-lint-errors summary)))
      (is (= 8 (:total-diff-lines summary))))))

(deftest test-all-valid-check
  (testing "all-valid? correctly evaluates phase validity"
    (let [results {"a.clj" {:pre-valid? true :post-valid? true}
                   "b.clj" {:pre-valid? true :post-valid? true}}]
      (is (validation/all-valid? results :pre))
      (is (validation/all-valid? results :post)))

    (let [results {"a.clj" {:pre-valid? true :post-valid? false}
                   "b.clj" {:pre-valid? true :post-valid? true}}]
      (is (validation/all-valid? results :pre))
      (is (not (validation/all-valid? results :post))))))

;; =============================================================================
;; Context Module Tests (drone.context)
;; =============================================================================

(deftest test-context-surrounding-lines
  (testing "read-surrounding-lines handles missing files gracefully"
    (let [result (context/read-surrounding-lines "/nonexistent/file.clj" 10)]
      (is (nil? result) "Should return nil for missing files"))))

(comment
  ;; Run tests
  (run-tests 'hive-mcp.agent.drone-test))
