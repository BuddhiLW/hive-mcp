(ns hive-mcp.agent.drone-test
  "Tests for drone delegation with structured nREPL error handling.

   TDD-first tests for:
   - nREPL error classification (:nrepl-connection, :nrepl-timeout, :nrepl-eval-error)
   - Structured error data (error-type, message, stacktrace)
   - Prometheus metric integration (drones_failed_total with error-type label)
   - Preset selection and task classification
   - Parser code extraction and confidence scoring
   - Validation result structures
   - Sandbox path containment validation
   - KG-first context (consult KG before file reads)"
  (:require [clojure.test :refer [deftest is run-tests testing use-fixtures]]
            [hive-mcp.agent.drone.errors :as drone]
            [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.agent.drone.parser :as parser]
            [hive-mcp.agent.drone.tools :as tools]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.validation :as validation]
            [hive-mcp.agent.drone.context :as context]
            [hive-mcp.agent.drone.kg-context :as kg-context]
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [clojure.java.io :as io]))

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
      (doseq [{:keys [message]} test-cases]
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

;; =============================================================================
;; Sandbox Path Containment Tests (BUG FIX: drone path escapes project directory)
;; =============================================================================

(deftest test-sandbox-path-containment-validation
  (testing "validate-path-containment catches path traversal attacks"
    (let [project-root "/home/user/myproject"]
      ;; Paths that escape project directory should fail
      (is (not (:valid? (sandbox/validate-path-containment
                         "../../../etc/passwd" project-root)))
          "Should reject ../../../etc/passwd")
      (is (not (:valid? (sandbox/validate-path-containment
                         "/etc/passwd" project-root)))
          "Should reject absolute path /etc/passwd")
      (is (not (:valid? (sandbox/validate-path-containment
                         "src/../../../etc/passwd" project-root)))
          "Should reject src/../../../etc/passwd"))))

(deftest test-sandbox-path-containment-allows-valid-paths
  (testing "validate-path-containment allows valid project paths"
    (let [project-root (System/getProperty "user.dir")]
      ;; Relative paths within project should succeed
      (let [result (sandbox/validate-path-containment "src/foo.clj" project-root)]
        (is (:valid? result) "Should allow relative paths")
        (is (some? (:canonical-path result)) "Should return canonical path"))
      ;; Paths with ../ that still resolve inside project
      (let [result (sandbox/validate-path-containment "src/../src/bar.clj" project-root)]
        (is (:valid? result) "Should allow paths that resolve inside project")))))

(deftest test-create-sandbox-rejects-escaping-paths
  (testing "create-sandbox rejects files that escape project directory"
    (let [project-root "/home/user/myproject"
          files ["src/core.clj"          ; valid
                 "../../../etc/passwd"   ; escape attempt
                 "/etc/shadow"]          ; absolute path escape
          sandbox-spec (sandbox/create-sandbox files project-root)]
      ;; Should have rejected files
      (is (seq (:rejected-files sandbox-spec))
          "Should have rejected files list")
      (is (>= (count (:rejected-files sandbox-spec)) 2)
          "Should reject at least 2 escape attempts")
      ;; Rejected should include the escape attempts
      (let [rejected-paths (set (map :path (:rejected-files sandbox-spec)))]
        (is (contains? rejected-paths "../../../etc/passwd")
            "Should reject ../../../etc/passwd")
        (is (contains? rejected-paths "/etc/shadow")
            "Should reject /etc/shadow")))))

(deftest test-create-sandbox-allows-valid-paths-only
  (testing "create-sandbox only includes valid paths in allowed-files"
    (let [project-root (System/getProperty "user.dir")
          ;; Mix of valid and invalid paths
          files ["src/hive_mcp/core.clj"
                 "test/hive_mcp/core_test.clj"
                 "../../../etc/passwd"]
          sandbox-spec (sandbox/create-sandbox files project-root)]
      ;; Should have allowed files (the valid ones)
      (is (some? (:allowed-files sandbox-spec))
          "Should have allowed files")
      ;; Allowed files should NOT contain the escape path
      (let [allowed (set (map #(.getPath (java.io.File. %))
                              (:allowed-files sandbox-spec)))]
        (is (not (some #(clojure.string/includes? % "passwd") allowed))
            "Allowed files should not contain escaped paths")))))

(deftest test-sandbox-validates-against-project-root
  (testing "create-sandbox uses project-root for validation"
    ;; When no project-root provided, should use user.dir as fallback
    (let [sandbox-no-root (sandbox/create-sandbox ["../../../etc/passwd"])]
      (is (seq (:rejected-files sandbox-no-root))
          "Should reject paths even without explicit project-root"))
    ;; With explicit project-root
    (let [sandbox-with-root (sandbox/create-sandbox
                             ["../../../etc/passwd"]
                             "/tmp/test-project")]
      (is (seq (:rejected-files sandbox-with-root))
          "Should reject paths with explicit project-root"))))

;; =============================================================================
;; KG-First Context Tests (drone consults KG before file reads)
;; =============================================================================

(def ^:dynamic *test-project-root* nil)

(defn kg-first-fixture [f]
  ;; Initialize KG connection for tests
  (kg-conn/ensure-initialized!)
  ;; Create temp project directory
  (let [tmp-dir (io/file (System/getProperty "java.io.tmpdir")
                         (str "hive-kg-test-" (System/currentTimeMillis)))]
    (.mkdirs tmp-dir)
    (binding [*test-project-root* (.getAbsolutePath tmp-dir)]
      (try
        (f)
        (finally
          ;; Cleanup: remove temp files
          (doseq [f (file-seq tmp-dir)]
            (.delete f)))))))

(use-fixtures :each kg-first-fixture)

(deftest test-kg-first-context-classification
  (testing "kg-first-context classifies files correctly"
    (let [test-file (io/file *test-project-root* "src" "core.clj")]
      ;; Ensure parent directory exists
      (.mkdirs (.getParentFile test-file))
      ;; Create test file
      (spit test-file "(ns core)\n(defn foo [] 1)")
      (let [abs-path (.getAbsolutePath test-file)]
        ;; File with no disc entity should be :needs-read
        (let [{:keys [kg-known needs-read stale summary]}
              (disc/kg-first-context [abs-path])]
          (is (empty? kg-known) "New file should not be kg-known")
          (is (= [abs-path] needs-read) "New file should be in needs-read")
          (is (empty? stale) "New file should not be stale")
          (is (= 1 (:needs-read summary))))

        ;; Add disc entity (simulate prior analysis)
        (disc/add-disc! {:path abs-path
                         :content-hash (disc/compute-hash "(ns core)\n(defn foo [] 1)")})

        ;; Now file should be :kg-known (fresh)
        (let [{:keys [kg-known needs-read stale summary]}
              (disc/kg-first-context [abs-path])]
          (is (contains? kg-known abs-path) "Tracked file should be kg-known")
          (is (empty? needs-read) "Tracked file should not need read")
          (is (empty? stale) "Fresh file should not be stale")
          (is (= 1 (:known summary))))

        ;; Modify file content - should become :stale
        (spit test-file "(ns core)\n(defn foo [] 2)")

        (let [{:keys [kg-known needs-read stale]}
              (disc/kg-first-context [abs-path])]
          (is (empty? kg-known) "Changed file should not be kg-known")
          (is (empty? needs-read) "Changed file has disc, not needs-read")
          (is (= [abs-path] stale) "Changed file should be stale"))

        ;; Cleanup
        (disc/remove-disc! abs-path)))))

(deftest test-kg-context-build-summary
  (testing "build-kg-summary creates context for kg-known files"
    (let [test-file (io/file *test-project-root* "src" "util.clj")]
      (.mkdirs (.getParentFile test-file))
      (let [content "(ns util)\n(defn helper [x] (inc x))"
            _ (spit test-file content)
            abs-path (.getAbsolutePath test-file)]
        ;; Add disc with known hash
        (disc/add-disc! {:path abs-path
                         :content-hash (disc/compute-hash content)})
        ;; Touch to record read
        (disc/touch-disc! abs-path)

        ;; Build KG summary
        (let [{:keys [kg-known]} (disc/kg-first-context [abs-path])
              kg-info (get kg-known abs-path)
              summary (kg-context/build-kg-summary {abs-path kg-info})]
          (is (string? summary) "Should return string summary")
          (is (re-find #"util\.clj" summary) "Should mention file name")
          (is (re-find #"(?i)(fresh|stale)" summary) "Should indicate freshness"))

        ;; Cleanup
        (disc/remove-disc! abs-path)))))

(deftest test-kg-first-skips-file-read
  (testing "format-files-with-kg-context skips reading kg-known files"
    (let [known-file (io/file *test-project-root* "src" "known.clj")
          needs-read-file (io/file *test-project-root* "src" "unknown.clj")]
      (.mkdirs (.getParentFile known-file))

      ;; Create both files
      (spit known-file "(ns known)\n(defn a [] :a)")
      (spit needs-read-file "(ns unknown)\n(defn b [] :b)")

      (let [known-path (.getAbsolutePath known-file)
            needs-read-path (.getAbsolutePath needs-read-file)]
        ;; Add disc for known file only
        (disc/add-disc! {:path known-path
                         :content-hash (disc/compute-hash "(ns known)\n(defn a [] :a)")})

        ;; Call the KG-first context builder
        (let [{:keys [context files-read]}
              (kg-context/format-files-with-kg-context
               [known-path needs-read-path]
               {:project-root *test-project-root*})]

          ;; Known file should NOT be in files-read (used KG summary)
          (is (not (contains? (set files-read) known-path))
              "KG-known file should not be read from disk")
          ;; Unknown file SHOULD be in files-read
          (is (contains? (set files-read) needs-read-path)
              "Unknown file should be read from disk")
          ;; Context should mention both files
          (is (re-find #"known\.clj" context) "Context should reference known file")
          (is (re-find #"unknown\.clj" context) "Context should reference unknown file"))

        ;; Cleanup
        (disc/remove-disc! known-path)))))

(deftest test-kg-first-includes-staleness-warning
  (testing "Stale files include staleness warning in context"
    (let [test-file (io/file *test-project-root* "src" "stale.clj")]
      (.mkdirs (.getParentFile test-file))
      (let [old-content "(ns stale)\n(defn old [] 1)"
            new-content "(ns stale)\n(defn new [] 2)"
            _ (spit test-file old-content)
            abs-path (.getAbsolutePath test-file)]
        ;; Add disc with OLD hash
        (disc/add-disc! {:path abs-path
                         :content-hash (disc/compute-hash old-content)})

        ;; Modify file to make it stale
        (spit test-file new-content)

        ;; Build context - should include staleness warning
        (let [{:keys [context warnings]}
              (kg-context/format-files-with-kg-context
               [abs-path]
               {:project-root *test-project-root*})]
          (is (seq warnings) "Should have staleness warnings")
          (is (re-find #"(?i)stale" context) "Context should mention staleness")
          (is (re-find #"(?i)re-?read" context) "Context should suggest re-reading"))

        ;; Cleanup
        (disc/remove-disc! abs-path)))))

(comment
  ;; Run tests
  (run-tests 'hive-mcp.agent.drone-test))
