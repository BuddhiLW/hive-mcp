(ns hive-mcp.agent.drone.augment-test
  "TDD tests for drone task augmentation.

   Tests context preparation and task augmentation functions that were
   extracted from drone.clj to reduce complexity (SOLID-S).

   Key functions tested:
   - format-context-str: Format conventions/decisions as string (pure function)
   - format-file-contents: Pre-read files with path validation (I/O)
   - augment-task: Compose full augmented task with all context (integration)

   Note: augment-task tests use with-redefs to mock I/O dependencies
   (Chroma, KG, registry). Only AGPL layer tested."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-mcp.agent.drone.augment :as augment]
            [hive-mcp.agent.drone.unified-context :as unified-ctx]
            [hive-mcp.agent.drone.kg-context :as kg-ctx]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.context.budget :as budget]
            [hive-mcp.tools.diff :as diff]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:private test-dir (str (System/getProperty "java.io.tmpdir") "/drone-augment-test"))

(defn- setup-test-files!
  "Create temporary test files."
  []
  (let [dir (io/file test-dir)]
    (.mkdirs dir)
    ;; Create a test clojure file
    (spit (io/file dir "test.clj")
          "(ns test.core)\n\n(defn hello []\n  \"Hello, World!\")")
    ;; Create another file
    (spit (io/file dir "config.edn")
          "{:port 8080\n :host \"localhost\"}")))

(defn- cleanup-test-files!
  "Remove temporary test files."
  []
  (let [dir (io/file test-dir)]
    (when (.exists dir)
      (doseq [f (.listFiles dir)]
        (.delete f))
      (.delete dir))))

(defn test-files-fixture [f]
  (setup-test-files!)
  (try
    (f)
    (finally
      (cleanup-test-files!))))

(use-fixtures :each test-files-fixture)

;; =============================================================================
;; format-context-str Tests (Pure Function)
;; =============================================================================

(deftest test-format-context-str-empty
  (testing "Empty context returns nil"
    (is (nil? (augment/format-context-str {})))
    (is (nil? (augment/format-context-str nil)))))

(deftest test-format-context-str-with-conventions
  (testing "Conventions are formatted with header"
    (let [context {:conventions [{:content "Use kebab-case"}
                                 {:content "Prefer pure functions"}]}
          result (augment/format-context-str context)]
      (is (some? result))
      (is (str/includes? result "### Conventions"))
      (is (str/includes? result "Use kebab-case"))
      (is (str/includes? result "Prefer pure functions"))
      (is (str/includes? result "## Project Context")))))

(deftest test-format-context-str-with-decisions
  (testing "Decisions are formatted with header"
    (let [context {:decisions [{:content "Use DataScript for state"}]}
          result (augment/format-context-str context)]
      (is (some? result))
      (is (str/includes? result "### Decisions"))
      (is (str/includes? result "Use DataScript for state")))))

(deftest test-format-context-str-with-both
  (testing "Both conventions and decisions are included"
    (let [context {:conventions [{:content "Rule 1"}]
                   :decisions [{:content "Decision A"}]}
          result (augment/format-context-str context)]
      (is (str/includes? result "### Conventions"))
      (is (str/includes? result "### Decisions"))
      (is (str/includes? result "Rule 1"))
      (is (str/includes? result "Decision A")))))

(deftest test-format-context-str-empty-lists
  (testing "Empty convention/decision lists produce nil"
    (is (nil? (augment/format-context-str {:conventions [] :decisions []})))))

;; =============================================================================
;; format-file-contents Tests (I/O - uses test fixture files)
;; =============================================================================

(deftest test-format-file-contents-empty-files
  (testing "Empty file list returns nil"
    (is (nil? (augment/format-file-contents [] test-dir)))
    (is (nil? (augment/format-file-contents nil test-dir)))))

(deftest test-format-file-contents-valid-files
  (testing "Valid files are read and formatted"
    (let [files [(str test-dir "/test.clj")]
          result (augment/format-file-contents files test-dir)]
      (is (some? result))
      (is (str/includes? result "## Current File Contents"))
      (is (str/includes? result "test.clj"))
      (is (str/includes? result "(ns test.core)")))))

(deftest test-format-file-contents-missing-file
  (testing "Missing files show error message"
    (let [files [(str test-dir "/nonexistent.clj")]
          result (augment/format-file-contents files test-dir)]
      (is (some? result))
      (is (str/includes? result "nonexistent.clj")))))

(deftest test-format-file-contents-path-escape-blocked
  (testing "Path traversal attempts are blocked"
    (let [files [(str test-dir "/../../../etc/passwd")]
          result (augment/format-file-contents files test-dir)]
      (is (some? result))
      (is (str/includes? result "BLOCKED")))))

(deftest test-format-file-contents-multiple-files
  (testing "Multiple files are all included"
    (let [files [(str test-dir "/test.clj")
                 (str test-dir "/config.edn")]
          result (augment/format-file-contents files test-dir)]
      (is (str/includes? result "test.clj"))
      (is (str/includes? result "config.edn"))
      (is (str/includes? result "(ns test.core)"))
      (is (str/includes? result ":port 8080")))))

;; =============================================================================
;; augment-task Tests (Integration - with-redefs for I/O mocking)
;; =============================================================================

(deftest test-augment-task-basic
  (testing "Basic task augmentation includes task section"
    (with-redefs [unified-ctx/unified-context-available? (constantly false)
                  registry/get-tool (constantly nil)
                  diff/get-project-root (constantly test-dir)
                  kg-ctx/format-files-with-kg-context
                  (fn [_files _opts]
                    {:context nil :files-read [] :kg-skipped [] :summary {}})]
      (let [result (augment/augment-task "Fix the bug" [] {})]
        (is (string? result))
        (is (str/includes? result "## Task"))
        (is (str/includes? result "Fix the bug"))))))

(deftest test-augment-task-with-files
  (testing "Task with files includes file list"
    (with-redefs [unified-ctx/unified-context-available? (constantly false)
                  registry/get-tool (constantly nil)
                  diff/get-project-root (constantly test-dir)
                  kg-ctx/format-files-with-kg-context
                  (fn [files _opts]
                    {:context (str "## File Contents\n" (str/join "\n" files))
                     :files-read files
                     :kg-skipped []
                     :summary {:kg-known 0 :needs-read (count files) :stale 0}})]
      (let [files [(str test-dir "/test.clj")]
            result (augment/augment-task "Update function" files
                                         {:project-root test-dir})]
        (is (str/includes? result "## Files to modify"))
        (is (str/includes? result "test.clj"))))))

(deftest test-augment-task-injects-project-root
  (testing "Project root is injected for propose_diff"
    (with-redefs [unified-ctx/unified-context-available? (constantly false)
                  registry/get-tool (constantly nil)
                  diff/get-project-root (constantly test-dir)
                  kg-ctx/format-files-with-kg-context
                  (fn [_files _opts]
                    {:context nil :files-read [] :kg-skipped [] :summary {}})]
      (let [result (augment/augment-task "Fix bug" []
                                         {:project-root "/project/path"})]
        (is (str/includes? result "## Project Directory"))
        (is (str/includes? result "/project/path"))))))

(deftest test-augment-task-return-metadata
  (testing "Return metadata about context path and budget when requested"
    (with-redefs [unified-ctx/unified-context-available? (constantly false)
                  registry/get-tool (constantly nil)
                  diff/get-project-root (constantly test-dir)
                  kg-ctx/format-files-with-kg-context
                  (fn [_files _opts]
                    {:context nil :files-read ["a.clj"] :kg-skipped []
                     :summary {:kg-known 0 :needs-read 1 :stale 0}})]
      (let [result (augment/augment-task "Task" ["a.clj"]
                                         {:return-metadata true
                                          :project-root test-dir})]
        (is (map? result))
        (is (contains? result :task))
        (is (contains? result :files-read))
        (is (contains? result :unified?))
        (is (contains? result :compressed?))
        (is (false? (:compressed? result))
            "No ctx-refs → not compressed path")
        (is (false? (:unified? result))
            "Unified unavailable → legacy path")))))

(deftest test-augment-task-budget-metadata
  (testing "Budget metadata returned when token-budget is set"
    (with-redefs [unified-ctx/unified-context-available? (constantly false)
                  registry/get-tool (constantly nil)
                  diff/get-project-root (constantly test-dir)
                  kg-ctx/format-files-with-kg-context
                  (fn [_files _opts]
                    {:context nil :files-read [] :kg-skipped [] :summary {}})]
      (let [result (augment/augment-task "Task" []
                                         {:return-metadata true
                                          :token-budget 2000})]
        (is (some? (:budget result))
            "Budget metadata present when token-budget set")
        (is (= 2000 (get-in result [:budget :total-budget]))
            "Budget matches requested token-budget")))))

(deftest test-augment-task-nil-budget-unbounded
  (testing "nil token-budget produces unbounded output (backward compat)"
    (with-redefs [unified-ctx/unified-context-available? (constantly false)
                  registry/get-tool (constantly nil)
                  diff/get-project-root (constantly test-dir)
                  kg-ctx/format-files-with-kg-context
                  (fn [_files _opts]
                    {:context nil :files-read [] :kg-skipped [] :summary {}})]
      (let [result (augment/augment-task "Task" []
                                         {:return-metadata true
                                          :token-budget nil})]
        (is (nil? (:budget result))
            "No budget metadata when token-budget is nil (unbounded)")))))

;; =============================================================================
;; Integration: Full Augmentation Flow
;; =============================================================================

(deftest test-full-augmentation-flow
  (testing "Full augmentation combines unified context, files, and task with budget"
    (with-redefs [unified-ctx/unified-context-available? (constantly true)
                  unified-ctx/prepare-drone-context
                  (fn [_opts]
                    {:conventions [{:id "c1" :type "convention"
                                    :content "Use atoms for state"}]
                     :decisions [{:id "d1" :type "decision"
                                  :content "DataScript for KG"}]
                     :snippets [] :domain [] :edges []
                     :seed-count 2 :traversal-count 2 :node-ids #{}})
                  registry/get-tool (constantly nil)
                  diff/get-project-root (constantly test-dir)
                  kg-ctx/format-files-with-kg-context
                  (fn [files _opts]
                    {:context (str "## Files\n" (str/join "\n" files))
                     :files-read files :kg-skipped []
                     :summary {:kg-known 0 :needs-read (count files) :stale 0}})]
      (let [result (augment/augment-task
                    "Implement validation"
                    [(str test-dir "/test.clj")]
                    {:project-root test-dir
                     :return-metadata true
                     :token-budget 4000})]
        ;; Task section present
        (is (str/includes? (:task result) "## Task"))
        (is (str/includes? (:task result) "Implement validation"))
        ;; Unified context path used
        (is (true? (:unified? result))
            "Unified context path was used")
        (is (false? (:compressed? result))
            "Not compressed (no ctx-refs)")
        ;; Budget applied
        (is (some? (:budget result)))
        (is (<= (get-in result [:budget :total-tokens])
                (get-in result [:budget :total-budget]))
            "Budget guarantee holds in full augmentation flow")))))

;; =============================================================================
;; Run tests
;; =============================================================================

(comment
  ;; Run tests in REPL
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent.drone.augment-test))
