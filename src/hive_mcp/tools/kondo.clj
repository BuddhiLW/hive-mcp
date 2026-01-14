(ns hive-mcp.tools.kondo
  "clj-kondo analysis tools for call graph and code intelligence.

   Provides static analysis via clj-kondo JVM API:
   - Project analysis with var/namespace definitions and usages
   - Find callers of a specific function
   - Find calls made by a specific function
   - Namespace dependency graph
   - Linting with configurable severity"
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [clj-kondo.core :as kondo]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Core Analysis
;; =============================================================================

(defn run-analysis
  "Run clj-kondo analysis on a path with full analysis config enabled."
  [path]
  (kondo/run! {:lint [path]
               :config {:analysis {:var-usages true
                                   :var-definitions true
                                   :namespace-usages true
                                   :namespace-definitions true
                                   :arglists true}}}))

;; =============================================================================
;; Tool Handlers
;; =============================================================================

(defn handle-kondo-analyze
  "Analyze a Clojure project/file and return summary statistics."
  [{:keys [path]}]
  (log/info "kondo-analyze" {:path path})
  (try
    (let [{:keys [analysis findings]} (run-analysis path)]
      (mcp-json {:var-definitions (count (:var-definitions analysis))
                 :var-usages (count (:var-usages analysis))
                 :namespaces (count (:namespace-definitions analysis))
                 :findings (count findings)
                 :path path}))
    (catch Exception e
      (log/error e "kondo-analyze failed" {:path path})
      (mcp-error (str "Analysis failed: " (.getMessage e))))))

(defn handle-kondo-find-callers
  "Find all locations that call a specific function."
  [{:keys [path ns var_name]}]
  (log/info "kondo-find-callers" {:path path :ns ns :var var_name})
  (try
    (let [{:keys [analysis]} (run-analysis path)
          target-ns (symbol ns)
          target-var (symbol var_name)
          callers (->> (:var-usages analysis)
                       (filter #(and (= (:to %) target-ns)
                                     (= (:name %) target-var)))
                       (mapv #(select-keys % [:filename :row :col :from :from-var :arity])))]
      (mcp-json {:target {:ns ns :var var_name}
                 :callers callers
                 :count (count callers)}))
    (catch Exception e
      (log/error e "kondo-find-callers failed" {:path path :ns ns :var var_name})
      (mcp-error (str "Find callers failed: " (.getMessage e))))))

(defn handle-kondo-find-calls
  "Find all functions called by a specific function."
  [{:keys [path ns var_name]}]
  (log/info "kondo-find-calls" {:path path :ns ns :var var_name})
  (try
    (let [{:keys [analysis]} (run-analysis path)
          source-ns (symbol ns)
          source-var (symbol var_name)
          calls (->> (:var-usages analysis)
                     (filter #(and (= (:from %) source-ns)
                                   (= (:from-var %) source-var)))
                     (mapv #(select-keys % [:filename :row :col :to :name :arity])))]
      (mcp-json {:source {:ns ns :var var_name}
                 :calls calls
                 :count (count calls)}))
    (catch Exception e
      (log/error e "kondo-find-calls failed" {:path path :ns ns :var var_name})
      (mcp-error (str "Find calls failed: " (.getMessage e))))))

(defn handle-kondo-namespace-graph
  "Generate a namespace dependency graph for the codebase."
  [{:keys [path]}]
  (log/info "kondo-namespace-graph" {:path path})
  (try
    (let [{:keys [analysis]} (run-analysis path)
          nodes (mapv #(select-keys % [:name :filename])
                      (:namespace-definitions analysis))
          edges (mapv #(hash-map :from (:from %)
                                 :to (:to %)
                                 :alias (:alias %))
                      (:namespace-usages analysis))]
      (mcp-json {:nodes nodes
                 :edges edges
                 :node-count (count nodes)
                 :edge-count (count edges)}))
    (catch Exception e
      (log/error e "kondo-namespace-graph failed" {:path path})
      (mcp-error (str "Namespace graph failed: " (.getMessage e))))))

(defn handle-kondo-lint
  "Lint a path and return findings filtered by severity level."
  [{:keys [path level]}]
  (log/info "kondo-lint" {:path path :level level})
  (try
    (let [{:keys [findings]} (run-analysis path)
          level-kw (keyword (or level "warning"))
          filtered (->> findings
                        (filter #(case level-kw
                                   :error (= (:level %) :error)
                                   :warning (#{:error :warning} (:level %))
                                   :info true))
                        (mapv #(select-keys % [:filename :row :col :level :type :message])))]
      (mcp-json {:findings filtered
                 :count (count filtered)
                 :level (name level-kw)}))
    (catch Exception e
      (log/error e "kondo-lint failed" {:path path})
      (mcp-error (str "Lint failed: " (.getMessage e))))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  [{:name "kondo_analyze"
    :description "Analyze a Clojure project/file using clj-kondo. Returns summary statistics including counts of var definitions, var usages, namespaces, and lint findings."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "Path to file or directory to analyze"}}
                  :required ["path"]}
    :handler handle-kondo-analyze}

   {:name "kondo_find_callers"
    :description "Find all locations that call a specific function. Returns filename, line, column, calling namespace and function for each call site."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "Path to file or directory to analyze"}
                               "ns" {:type "string"
                                     :description "Namespace of the target function (e.g., 'clojure.core')"}
                               "var_name" {:type "string"
                                           :description "Name of the function to find callers for (e.g., 'map')"}}
                  :required ["path" "ns" "var_name"]}
    :handler handle-kondo-find-callers}

   {:name "kondo_find_calls"
    :description "Find all functions called by a specific function. Returns filename, line, column, target namespace and function for each call."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "Path to file or directory to analyze"}
                               "ns" {:type "string"
                                     :description "Namespace of the source function"}
                               "var_name" {:type "string"
                                           :description "Name of the function to find calls from"}}
                  :required ["path" "ns" "var_name"]}
    :handler handle-kondo-find-calls}

   {:name "kondo_namespace_graph"
    :description "Generate a namespace dependency graph. Returns nodes (namespaces with filenames) and edges (require/use relationships with aliases)."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "Path to file or directory to analyze"}}
                  :required ["path"]}
    :handler handle-kondo-namespace-graph}

   {:name "kondo_lint"
    :description "Lint Clojure code and return findings. Filter by severity level: 'error' (errors only), 'warning' (errors + warnings), 'info' (all findings)."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "Path to file or directory to lint"}
                               "level" {:type "string"
                                        :description "Minimum severity level: 'error', 'warning' (default), or 'info'"
                                        :enum ["error" "warning" "info"]}}
                  :required ["path"]}
    :handler handle-kondo-lint}])
