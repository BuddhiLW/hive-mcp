(ns hive-mcp.tools.consolidated.analysis
  "Consolidated code analysis CLI tool combining clj-kondo and scc."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.kondo :as kondo-handlers]
            [hive-mcp.tools.scc :as scc-handlers]))

(def handlers
  {:lint     kondo-handlers/handle-kondo-lint
   :analyze  kondo-handlers/handle-kondo-analyze
   :callers  kondo-handlers/handle-kondo-find-callers
   :calls    kondo-handlers/handle-kondo-find-calls
   :graph    kondo-handlers/handle-kondo-namespace-graph
   :scc      scc-handlers/handle-scc-analyze
   :hotspots scc-handlers/handle-scc-hotspots
   :file     scc-handlers/handle-scc-file
   :compare  scc-handlers/handle-scc-compare})

(def handle-analysis
  (make-cli-handler handlers))

(def tool-def
  {:name "analysis"
   :consolidated true
   :description "Code analysis: lint (kondo errors), analyze (kondo summary), callers/calls/graph (kondo), scc (code metrics), hotspots (complexity), file (single file), compare (diff dirs). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["lint" "analyze" "callers" "calls" "graph"
                                                "scc" "hotspots" "file" "compare" "help"]
                                         :description "Analysis operation to perform"}
                              "path" {:type "string"
                                      :description "Path to file or directory to analyze"}
                              "level" {:type "string"
                                       :enum ["error" "warning" "info"]
                                       :description "Minimum severity level (lint)"}
                              "ns" {:type "string"
                                    :description "Namespace of the target/source function"}
                              "var_name" {:type "string"
                                          :description "Name of the function"}
                              "threshold" {:type "number"
                                           :description "Minimum complexity for hotspots (default: 20)"}
                              "file_path" {:type "string"
                                           :description "Path to specific file (file command)"}
                              "path_a" {:type "string"
                                        :description "First directory for comparison"}
                              "path_b" {:type "string"
                                        :description "Second directory for comparison"}}
                 :required ["command"]}
   :handler handle-analysis})

(def tools [tool-def])
