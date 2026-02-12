(ns hive-mcp.tools.scc-test
  "TDD tests for SCC MCP tool handlers.

   Tests cover:
   - handle-scc-analyze: Analyze project/directory
   - handle-scc-hotspots: Get complexity hotspots
   - handle-scc-file: Get metrics for specific file
   - handle-scc-compare: Compare two directories"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.tools.scc :as scc-tools]))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn parse-response
  "Parse JSON from MCP response text."
  [response]
  (json/read-str (:text response) :key-fn keyword))

(defn error-response?
  "Check if response is an error."
  [response]
  (:isError response))

;; =============================================================================
;; Test: handle-scc-analyze
;; =============================================================================

(deftest test-handle-scc-analyze-returns-mcp-response
  (testing "handle-scc-analyze returns valid MCP response structure"
    (let [response (scc-tools/handle-scc-analyze {:path "src/hive_mcp/tools/core.clj"})]
      (is (map? response))
      (is (= "text" (:type response)))
      (is (string? (:text response))))))

(deftest test-handle-scc-analyze-returns-summary
  (testing "handle-scc-analyze returns summary statistics"
    (let [response (scc-tools/handle-scc-analyze {:path "src/hive_mcp/tools"})
          data (parse-response response)]
      (is (contains? data :summary))
      (is (pos? (get-in data [:summary :total-lines])))
      (is (pos? (get-in data [:summary :total-code]))))))

(deftest test-handle-scc-analyze-returns-by-language
  (testing "handle-scc-analyze returns per-language breakdown"
    (let [response (scc-tools/handle-scc-analyze {:path "src/hive_mcp"})
          data (parse-response response)]
      (is (contains? data :by-language))
      (is (contains? (:by-language data) :Clojure)))))

(deftest test-handle-scc-analyze-returns-file-count
  (testing "handle-scc-analyze includes file count"
    (let [response (scc-tools/handle-scc-analyze {:path "src/hive_mcp/tools"})
          data (parse-response response)]
      (is (contains? data :file-count))
      (is (pos? (:file-count data))))))

(deftest test-handle-scc-analyze-returns-path
  (testing "handle-scc-analyze echoes the analyzed path"
    (let [response (scc-tools/handle-scc-analyze {:path "src/hive_mcp"})
          data (parse-response response)]
      (is (= "src/hive_mcp" (:path data))))))

(deftest test-handle-scc-analyze-invalid-path
  (testing "handle-scc-analyze returns error for invalid path"
    (let [response (scc-tools/handle-scc-analyze {:path "/nonexistent/path/xyz"})]
      (is (error-response? response)))))

;; =============================================================================
;; Test: handle-scc-hotspots
;; =============================================================================

(deftest test-handle-scc-hotspots-returns-mcp-response
  (testing "handle-scc-hotspots returns valid MCP response structure"
    (let [response (scc-tools/handle-scc-hotspots {:path "src/hive_mcp" :threshold 10})]
      (is (map? response))
      (is (= "text" (:type response)))
      (is (string? (:text response))))))

(deftest test-handle-scc-hotspots-returns-hotspots
  (testing "handle-scc-hotspots returns hotspot list"
    (let [response (scc-tools/handle-scc-hotspots {:path "src/hive_mcp" :threshold 10})
          data (parse-response response)]
      (is (contains? data :hotspots))
      (is (vector? (:hotspots data))))))

(deftest test-handle-scc-hotspots-returns-count
  (testing "handle-scc-hotspots includes count"
    (let [response (scc-tools/handle-scc-hotspots {:path "src/hive_mcp" :threshold 10})
          data (parse-response response)]
      (is (contains? data :count))
      (is (= (count (:hotspots data)) (:count data))))))

(deftest test-handle-scc-hotspots-default-threshold
  (testing "handle-scc-hotspots uses default threshold of 20"
    (let [response (scc-tools/handle-scc-hotspots {:path "src/hive_mcp"})
          data (parse-response response)]
      (is (= 20 (:threshold data))))))

(deftest test-handle-scc-hotspots-respects-threshold
  (testing "handle-scc-hotspots echoes threshold and returns filtered files"
    (let [response-high (scc-tools/handle-scc-hotspots {:path "src/hive_mcp" :threshold 80})
          response-low (scc-tools/handle-scc-hotspots {:path "src/hive_mcp" :threshold 5})
          data-high (parse-response response-high)
          data-low (parse-response response-low)]
      ;; Echoes threshold
      (is (= 80 (:threshold data-high)))
      (is (= 5 (:threshold data-low)))
      ;; Higher threshold = fewer hotspots
      (is (< (:count data-high) (:count data-low))))))

(deftest test-handle-scc-hotspots-invalid-path
  (testing "handle-scc-hotspots returns error for invalid path"
    (let [response (scc-tools/handle-scc-hotspots {:path "/nonexistent/path"})]
      (is (error-response? response)))))

;; =============================================================================
;; Test: handle-scc-file
;; =============================================================================

(deftest test-handle-scc-file-returns-mcp-response
  (testing "handle-scc-file returns valid MCP response structure"
    (let [response (scc-tools/handle-scc-file {:file_path "src/hive_mcp/tools/core.clj"})]
      (is (map? response))
      (is (= "text" (:type response)))
      (is (string? (:text response))))))

(deftest test-handle-scc-file-returns-metrics
  (testing "handle-scc-file returns file metrics"
    (let [response (scc-tools/handle-scc-file {:file_path "src/hive_mcp/tools/core.clj"})
          data (parse-response response)]
      (is (contains? data :filename))
      (is (contains? data :lines))
      (is (contains? data :code))
      (is (contains? data :complexity)))))

(deftest test-handle-scc-file-detects-language
  (testing "handle-scc-file includes language"
    (let [response (scc-tools/handle-scc-file {:file_path "src/hive_mcp/tools/core.clj"})
          data (parse-response response)]
      (is (= "Clojure" (:language data))))))

(deftest test-handle-scc-file-invalid-path
  (testing "handle-scc-file returns error for nonexistent file"
    (let [response (scc-tools/handle-scc-file {:file_path "/nonexistent/file.clj"})]
      (is (error-response? response)))))

;; =============================================================================
;; Test: handle-scc-compare
;; =============================================================================

(deftest test-handle-scc-compare-returns-mcp-response
  (testing "handle-scc-compare returns valid MCP response structure"
    (let [response (scc-tools/handle-scc-compare {:path_a "src/hive_mcp/tools"
                                                  :path_b "src/hive_mcp/scc"})]
      (is (map? response))
      (is (= "text" (:type response)))
      (is (string? (:text response))))))

(deftest test-handle-scc-compare-returns-both-summaries
  (testing "handle-scc-compare returns summaries for both directories"
    (let [response (scc-tools/handle-scc-compare {:path_a "src/hive_mcp/tools"
                                                  :path_b "src/hive_mcp/scc"})
          data (parse-response response)]
      (is (contains? data :path_a))
      (is (contains? data :path_b))
      (is (contains? (:path_a data) :summary))
      (is (contains? (:path_b data) :summary)))))

(deftest test-handle-scc-compare-returns-diff
  (testing "handle-scc-compare returns diff statistics"
    (let [response (scc-tools/handle-scc-compare {:path_a "src/hive_mcp/tools"
                                                  :path_b "src/hive_mcp/scc"})
          data (parse-response response)]
      (is (contains? data :diff))
      (is (contains? (:diff data) :lines))
      (is (contains? (:diff data) :code))
      (is (contains? (:diff data) :complexity)))))

(deftest test-handle-scc-compare-invalid-path-a
  (testing "handle-scc-compare returns error for invalid path_a"
    (let [response (scc-tools/handle-scc-compare {:path_a "/nonexistent/a"
                                                  :path_b "src/hive_mcp"})]
      (is (error-response? response)))))

(deftest test-handle-scc-compare-invalid-path-b
  (testing "handle-scc-compare returns error for invalid path_b"
    (let [response (scc-tools/handle-scc-compare {:path_a "src/hive_mcp"
                                                  :path_b "/nonexistent/b"})]
      (is (error-response? response)))))

;; =============================================================================
;; Test: Tool Definitions
;; =============================================================================

(deftest test-tools-vector-exists
  (testing "tools vector is defined"
    (is (vector? scc-tools/tools))
    (is (= 4 (count scc-tools/tools)))))

(deftest test-tools-have-required-keys
  (testing "All tools have required keys"
    (doseq [tool scc-tools/tools]
      (is (contains? tool :name))
      (is (contains? tool :description))
      (is (contains? tool :inputSchema))
      (is (contains? tool :handler)))))

(deftest test-tools-names
  (testing "Tools have correct names"
    (let [names (set (map :name scc-tools/tools))]
      (is (contains? names "scc_analyze"))
      (is (contains? names "scc_hotspots"))
      (is (contains? names "scc_file"))
      (is (contains? names "scc_compare")))))

(deftest test-tools-handlers-are-functions
  (testing "All tool handlers are functions"
    (doseq [tool scc-tools/tools]
      (is (fn? (:handler tool))))))
