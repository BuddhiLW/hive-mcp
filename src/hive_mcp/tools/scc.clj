(ns hive-mcp.tools.scc
  "MCP tool handlers for SCC code metrics.

   CLARITY: A - Architectural performance metrics
   SOLID: SRP - Only MCP tool handlers

   Provides MCP tools for:
   - scc_analyze: Analyze project/directory for code metrics
   - scc_hotspots: Get complexity hotspots above threshold
   - scc_file: Get metrics for specific file
   - scc_compare: Compare metrics between two directories"
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.scc.core :as scc]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Tool Handlers
;; =============================================================================

(defn handle-scc-analyze
  "Analyze project/directory for code metrics including LOC, complexity, and language breakdown."
  [{:keys [path]}]
  (log/info "scc-analyze" {:path path})
  (try
    (let [result (scc/analyze-project path)]
      (if (:error result)
        (mcp-error (:error result))
        (mcp-json {:summary (:summary result)
                   :by-language (:by-language result)
                   :file-count (count (:files result))
                   :path path})))
    (catch Exception e
      (log/error e "scc-analyze failed" {:path path})
      (mcp-error (str "Analysis failed: " (.getMessage e))))))

(defn handle-scc-hotspots
  "Get complexity hotspots - files exceeding a complexity threshold."
  [{:keys [path threshold]}]
  (let [threshold-val (or threshold 20)]
    (log/info "scc-hotspots" {:path path :threshold threshold-val})
    (try
      (let [result (scc/run-scc path)]
        (if (:error result)
          (mcp-error (:error result))
          (let [hotspots (scc/get-complexity-hotspots (:raw result) threshold-val)]
            (mcp-json {:hotspots hotspots
                       :count (count hotspots)
                       :threshold threshold-val
                       :path path}))))
      (catch Exception e
        (log/error e "scc-hotspots failed" {:path path :threshold threshold-val})
        (mcp-error (str "Hotspot analysis failed: " (.getMessage e)))))))

(defn handle-scc-file
  "Get detailed metrics for a specific file."
  [{:keys [file_path]}]
  (log/info "scc-file" {:file file_path})
  (try
    (let [result (scc/get-file-metrics file_path)]
      (if (:error result)
        (mcp-error (:error result))
        (mcp-json result)))
    (catch Exception e
      (log/error e "scc-file failed" {:file file_path})
      (mcp-error (str "File analysis failed: " (.getMessage e))))))

(defn handle-scc-compare
  "Compare metrics between two directories."
  [{:keys [path_a path_b]}]
  (log/info "scc-compare" {:path_a path_a :path_b path_b})
  (try
    (let [result-a (scc/analyze-project path_a)
          result-b (scc/analyze-project path_b)]
      (cond
        (:error result-a)
        (mcp-error (str "Error analyzing " path_a ": " (:error result-a)))

        (:error result-b)
        (mcp-error (str "Error analyzing " path_b ": " (:error result-b)))

        :else
        (let [summary-a (:summary result-a)
              summary-b (:summary result-b)
              diff (fn [k] (- (get summary-b k 0) (get summary-a k 0)))]
          (mcp-json {:path_a {:path path_a :summary summary-a}
                     :path_b {:path path_b :summary summary-b}
                     :diff {:lines (diff :total-lines)
                            :code (diff :total-code)
                            :complexity (diff :total-complexity)
                            :files (diff :file-count)}}))))
    (catch Exception e
      (log/error e "scc-compare failed" {:path_a path_a :path_b path_b})
      (mcp-error (str "Comparison failed: " (.getMessage e))))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  [{:name "scc_analyze"
    :description "Analyze a project/directory for code metrics using scc. Returns summary statistics including lines of code, complexity, comment ratio, and per-language breakdown."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "Path to file or directory to analyze"}}
                  :required ["path"]}
    :handler handle-scc-analyze}

   {:name "scc_hotspots"
    :description "Get complexity hotspots - files exceeding a complexity threshold. Useful for identifying code that may need refactoring. Returns files sorted by complexity descending."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "Path to directory to analyze"}
                               "threshold" {:type "number"
                                            :description "Minimum complexity to include (default: 20)"}}
                  :required ["path"]}
    :handler handle-scc-hotspots}

   {:name "scc_file"
    :description "Get detailed metrics for a specific file including lines, code, comments, blanks, complexity, and language."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Path to the file to analyze"}}
                  :required ["file_path"]}
    :handler handle-scc-file}

   {:name "scc_compare"
    :description "Compare metrics between two directories. Useful for measuring impact of refactoring or comparing codebases. Returns summary for each directory and the diff."
    :inputSchema {:type "object"
                  :properties {"path_a" {:type "string"
                                         :description "Path to first directory"}
                               "path_b" {:type "string"
                                         :description "Path to second directory"}}
                  :required ["path_a" "path_b"]}
    :handler handle-scc-compare}])
