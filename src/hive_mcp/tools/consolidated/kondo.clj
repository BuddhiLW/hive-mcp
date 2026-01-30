(ns hive-mcp.tools.consolidated.kondo
  "Consolidated clj-kondo CLI tool.

   Subcommands: lint, analyze, callers, calls, graph

   Usage via MCP: kondo {\"command\": \"lint\", \"path\": \"src/\"}

   SOLID: Facade pattern - single tool entry point for kondo analysis.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.kondo :as kondo-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:lint    kondo-handlers/handle-kondo-lint
   :analyze kondo-handlers/handle-kondo-analyze
   :callers kondo-handlers/handle-kondo-find-callers
   :calls   kondo-handlers/handle-kondo-find-calls
   :graph   kondo-handlers/handle-kondo-namespace-graph})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-kondo
  "Unified CLI handler for clj-kondo operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated kondo command."
  {:name "kondo"
   :description "clj-kondo analysis: lint (check for errors), analyze (project summary), callers (find call sites), calls (find outgoing calls), graph (namespace deps). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["lint" "analyze" "callers" "calls" "graph" "help"]
                                         :description "Kondo operation to perform"}
                              ;; common params
                              "path" {:type "string"
                                      :description "Path to file or directory to analyze"}
                              ;; lint params
                              "level" {:type "string"
                                       :enum ["error" "warning" "info"]
                                       :description "Minimum severity level"}
                              ;; callers/calls params
                              "ns" {:type "string"
                                    :description "Namespace of the target/source function"}
                              "var_name" {:type "string"
                                          :description "Name of the function"}}
                 :required ["command"]}
   :handler handle-kondo})

(def tools
  "Tool definitions for registration."
  [tool-def])
