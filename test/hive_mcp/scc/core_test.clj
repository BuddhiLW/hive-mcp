(ns hive-mcp.scc.core-test
  "TDD tests for SCC shell wrapper.

   Tests cover:
   - run-scc: Execute scc with args, parse JSON output
   - analyze-project: Run scc on directory, return parsed metrics
   - get-complexity-hotspots: Filter files by complexity threshold
   - get-file-metrics: Get metrics for specific file"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.scc.core :as scc]))

;; =============================================================================
;; Test Data
;; =============================================================================

(def sample-scc-output
  "Sample scc JSON output for testing parse functions."
  [{:Name "Clojure"
    :Bytes 5000
    :Lines 150
    :Code 100
    :Comment 20
    :Blank 30
    :Complexity 12
    :Count 2
    :Files [{:Language "Clojure"
             :Filename "src/foo.clj"
             :Location "src/foo.clj"
             :Lines 80
             :Code 50
             :Comment 15
             :Blank 15
             :Complexity 5}
            {:Language "Clojure"
             :Filename "src/bar.clj"
             :Location "src/bar.clj"
             :Lines 70
             :Code 50
             :Comment 5
             :Blank 15
             :Complexity 7}]}
   {:Name "Markdown"
    :Bytes 1000
    :Lines 50
    :Code 40
    :Comment 0
    :Blank 10
    :Complexity 0
    :Count 1
    :Files [{:Language "Markdown"
             :Filename "README.md"
             :Location "README.md"
             :Lines 50
             :Code 40
             :Comment 0
             :Blank 10
             :Complexity 0}]}])

;; =============================================================================
;; Test: parse-scc-output
;; =============================================================================

(deftest test-parse-scc-output-extracts-summary
  (testing "parse-scc-output extracts summary statistics"
    (let [result (scc/parse-scc-output sample-scc-output)]
      (is (map? result))
      (is (contains? result :summary))
      (is (= 200 (get-in result [:summary :total-lines])))
      (is (= 140 (get-in result [:summary :total-code])))
      (is (= 20 (get-in result [:summary :total-comment])))
      (is (= 40 (get-in result [:summary :total-blank])))
      (is (= 12 (get-in result [:summary :total-complexity]))))))

(deftest test-parse-scc-output-extracts-by-language
  (testing "parse-scc-output groups metrics by language"
    (let [result (scc/parse-scc-output sample-scc-output)]
      (is (contains? result :by-language))
      (is (= 2 (count (:by-language result))))
      (is (= 100 (get-in result [:by-language "Clojure" :code])))
      (is (= 12 (get-in result [:by-language "Clojure" :complexity]))))))

(deftest test-parse-scc-output-extracts-files
  (testing "parse-scc-output includes all files"
    (let [result (scc/parse-scc-output sample-scc-output)]
      (is (contains? result :files))
      (is (= 3 (count (:files result))))
      (is (some #(= "src/foo.clj" (:filename %)) (:files result))))))

(deftest test-parse-scc-output-handles-empty
  (testing "parse-scc-output handles empty input"
    (let [result (scc/parse-scc-output [])]
      (is (= 0 (get-in result [:summary :total-lines])))
      (is (empty? (:files result))))))

;; =============================================================================
;; Test: run-scc
;; =============================================================================

(deftest test-run-scc-returns-parsed-result
  (testing "run-scc returns parsed result for valid path"
    (let [result (scc/run-scc "src/hive_mcp/tools/core.clj")]
      (is (map? result))
      (is (contains? result :summary))
      (is (contains? result :files))
      (is (pos? (get-in result [:summary :total-lines]))))))

(deftest test-run-scc-with-args
  (testing "run-scc accepts additional arguments"
    (let [result (scc/run-scc "src/hive_mcp/tools/core.clj" ["--no-cocomo"])]
      (is (map? result))
      (is (contains? result :summary)))))

(deftest test-run-scc-nonexistent-path
  (testing "run-scc returns error for nonexistent path"
    (let [result (scc/run-scc "/nonexistent/path/xyz")]
      (is (contains? result :error)))))

;; =============================================================================
;; Test: analyze-project
;; =============================================================================

(deftest test-analyze-project-returns-metrics
  (testing "analyze-project returns complete metrics for directory"
    (let [result (scc/analyze-project "src/hive_mcp")]
      (is (map? result))
      (is (contains? result :summary))
      (is (contains? result :by-language))
      (is (contains? result :files))
      (is (pos? (get-in result [:summary :total-lines]))))))

(deftest test-analyze-project-includes-complexity
  (testing "analyze-project includes complexity metrics"
    (let [result (scc/analyze-project "src/hive_mcp")]
      (is (contains? (:summary result) :total-complexity))
      (is (number? (get-in result [:summary :total-complexity]))))))

(deftest test-analyze-project-file-count
  (testing "analyze-project counts files correctly"
    (let [result (scc/analyze-project "src/hive_mcp/tools")]
      (is (pos? (get-in result [:summary :file-count]))))))

;; =============================================================================
;; Test: get-complexity-hotspots
;; =============================================================================

(deftest test-get-complexity-hotspots-filters-by-threshold
  (testing "get-complexity-hotspots returns files above threshold"
    (let [hotspots (scc/get-complexity-hotspots sample-scc-output 6)]
      (is (= 1 (count hotspots)))
      (is (= "src/bar.clj" (:filename (first hotspots))))
      (is (= 7 (:complexity (first hotspots)))))))

(deftest test-get-complexity-hotspots-includes-all-above
  (testing "get-complexity-hotspots includes all files above threshold"
    (let [hotspots (scc/get-complexity-hotspots sample-scc-output 4)]
      (is (= 2 (count hotspots))))))

(deftest test-get-complexity-hotspots-empty-when-none-match
  (testing "get-complexity-hotspots returns empty when no files match"
    (let [hotspots (scc/get-complexity-hotspots sample-scc-output 100)]
      (is (empty? hotspots)))))

(deftest test-get-complexity-hotspots-sorted-descending
  (testing "get-complexity-hotspots returns results sorted by complexity descending"
    (let [hotspots (scc/get-complexity-hotspots sample-scc-output 0)]
      (is (= [7 5 0] (mapv :complexity hotspots))))))

;; =============================================================================
;; Test: get-file-metrics
;; =============================================================================

(deftest test-get-file-metrics-returns-single-file
  (testing "get-file-metrics returns metrics for specific file"
    (let [result (scc/get-file-metrics "src/hive_mcp/tools/core.clj")]
      (is (map? result))
      (is (contains? result :filename))
      (is (contains? result :lines))
      (is (contains? result :code))
      (is (contains? result :complexity)))))

(deftest test-get-file-metrics-includes-language
  (testing "get-file-metrics includes language detection"
    (let [result (scc/get-file-metrics "src/hive_mcp/tools/core.clj")]
      (is (= "Clojure" (:language result))))))

(deftest test-get-file-metrics-nonexistent-file
  (testing "get-file-metrics returns error for nonexistent file"
    (let [result (scc/get-file-metrics "/nonexistent/file.clj")]
      (is (contains? result :error)))))

;; =============================================================================
;; Test: Integration - Real scc execution
;; =============================================================================

(deftest ^:integration test-scc-on-project-root
  (testing "scc can analyze the entire project"
    (let [result (scc/analyze-project ".")]
      (is (map? result))
      (is (> (get-in result [:summary :total-lines]) 1000))
      (is (contains? (:by-language result) "Clojure")))))

(deftest ^:integration test-complexity-hotspots-real
  (testing "Can find complexity hotspots in real codebase"
    (let [result (scc/analyze-project "src")
          hotspots (scc/get-complexity-hotspots (:raw result) 10)]
      (is (vector? hotspots))
      ;; Real codebase should have some complex files
      (is (seq hotspots)))))
