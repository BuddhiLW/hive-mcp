(ns hive-mcp.scc.core
  "Shell wrapper for scc binary with JSON parsing.

   CLARITY: L - Pure infrastructure layer
   SOLID: SRP - Only handles scc execution

   Provides:
   - run-scc: Execute scc with args, parse JSON output
   - analyze-project: Run scc on directory, return parsed metrics
   - get-complexity-hotspots: Filter files by complexity threshold
   - get-file-metrics: Get metrics for specific file"
  (:require [clojure.java.shell :as shell]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Internal: JSON Parsing
;; =============================================================================

(defn- extract-files
  "Extract all files from scc language groups into flat vector."
  [scc-data]
  (->> scc-data
       (mapcat :Files)
       (mapv (fn [f]
               {:filename (:Filename f)
                :location (:Location f)
                :language (:Language f)
                :lines (:Lines f)
                :code (:Code f)
                :comment (:Comment f)
                :blank (:Blank f)
                :complexity (:Complexity f)
                :bytes (:Bytes f)}))))

(defn- build-summary
  "Build summary statistics from scc data."
  [scc-data files]
  {:total-lines (reduce + 0 (map :Lines scc-data))
   :total-code (reduce + 0 (map :Code scc-data))
   :total-comment (reduce + 0 (map :Comment scc-data))
   :total-blank (reduce + 0 (map :Blank scc-data))
   :total-complexity (reduce + 0 (map :Complexity scc-data))
   :total-bytes (reduce + 0 (map :Bytes scc-data))
   :file-count (count files)
   :language-count (count scc-data)})

(defn- build-by-language
  "Build per-language metrics map."
  [scc-data]
  (into {}
        (map (fn [lang]
               [(:Name lang)
                {:lines (:Lines lang)
                 :code (:Code lang)
                 :comment (:Comment lang)
                 :blank (:Blank lang)
                 :complexity (:Complexity lang)
                 :file-count (:Count lang)}])
             scc-data)))

;; =============================================================================
;; Public: Parse Functions
;; =============================================================================

(defn parse-scc-output
  "Parse raw scc JSON output into structured metrics.

   Input: Vector of language group maps from scc JSON
   Output: {:summary {...}
            :by-language {...}
            :files [...]}"
  [scc-data]
  (let [files (extract-files scc-data)]
    {:summary (build-summary scc-data files)
     :by-language (build-by-language scc-data)
     :files files}))

;; =============================================================================
;; Public: Shell Execution
;; =============================================================================

(defn run-scc
  "Execute scc on path with optional args, return parsed metrics.

   Arguments:
   - path: File or directory to analyze
   - args: Optional vector of additional scc arguments

   Returns parsed metrics map or {:error \"message\"}"
  ([path]
   (run-scc path []))
  ([path args]
   (log/debug "Running scc" {:path path :args args})
   (try
     (let [cmd-args (concat ["scc" "-f" "json" "--by-file"] args [path])
           {:keys [out err exit]} (apply shell/sh cmd-args)]
       (if (and (zero? exit) (not (str/blank? out)))
         (let [raw (json/read-str out :key-fn keyword)]
           (assoc (parse-scc-output raw) :raw raw))
         {:error (if (str/blank? err)
                   (str "scc returned no output for path: " path)
                   err)}))
     (catch Exception e
       (log/error e "scc execution failed" {:path path})
       {:error (.getMessage e)}))))

(defn analyze-project
  "Run scc on directory and return comprehensive metrics.

   Returns:
   {:summary {:total-lines N :total-code N :total-complexity N ...}
    :by-language {\"Clojure\" {:lines N :code N ...} ...}
    :files [{:filename \"...\" :complexity N ...} ...]
    :raw [original scc output]}"
  [directory]
  (log/info "Analyzing project" {:directory directory})
  (run-scc directory))

;; =============================================================================
;; Public: Complexity Analysis
;; =============================================================================

(defn get-complexity-hotspots
  "Filter files by complexity threshold, sorted descending.

   Arguments:
   - scc-data: Raw scc JSON output (vector of language groups)
   - threshold: Minimum complexity to include

   Returns vector of file maps with complexity > threshold."
  [scc-data threshold]
  (->> scc-data
       (mapcat :Files)
       (filter #(>= (:Complexity %) threshold))
       (sort-by :Complexity >)
       (mapv (fn [f]
               {:filename (:Filename f)
                :location (:Location f)
                :language (:Language f)
                :complexity (:Complexity f)
                :lines (:Lines f)
                :code (:Code f)}))))

(defn get-file-metrics
  "Get metrics for a specific file.

   Returns file metrics map or {:error \"message\"}"
  [file-path]
  (log/debug "Getting file metrics" {:file file-path})
  (let [result (run-scc file-path)]
    (if (:error result)
      result
      (if-let [file-data (first (:files result))]
        file-data
        {:error (str "No metrics found for file: " file-path)}))))
