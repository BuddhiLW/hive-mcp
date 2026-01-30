(ns hive-mcp.tools.consolidated.kg
  "Consolidated Knowledge Graph CLI tool.

   Subcommands: traverse, edge, impact, subgraph, stats

   Usage via MCP: kg {\"command\": \"traverse\", \"start_node\": \"mem-123\"}

   SOLID: Facade pattern - single tool entry point for KG operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.kg :as kg-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:traverse kg-handlers/handle-kg-traverse
   :edge     kg-handlers/handle-kg-add-edge
   :impact   kg-handlers/handle-kg-impact-analysis
   :subgraph kg-handlers/handle-kg-subgraph
   :stats    kg-handlers/handle-kg-stats
   :path     kg-handlers/handle-kg-find-path
   :context  kg-handlers/handle-kg-node-context
   :promote  kg-handlers/handle-kg-promote
   :reground kg-handlers/handle-kg-reground})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-kg
  "Unified CLI handler for Knowledge Graph operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated kg command."
  {:name "kg"
   :description "Knowledge Graph operations: traverse (walk graph), edge (add relationship), impact (find dependents), subgraph (extract scope), stats (counts), path (shortest path), context (node details), promote (bubble up scope), reground (verify source). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["traverse" "edge" "impact" "subgraph" "stats" "path" "context" "promote" "reground" "help"]
                                         :description "KG operation to perform"}
                              ;; traverse params
                              "start_node" {:type "string"
                                            :description "Node ID to start traversal from"}
                              "direction" {:type "string"
                                           :enum ["outgoing" "incoming" "both"]
                                           :description "Edge direction for traversal"}
                              "max_depth" {:type "integer"
                                           :description "Maximum traversal/search depth"}
                              "relations" {:type "array"
                                           :items {:type "string"}
                                           :description "Relation types to follow"}
                              "scope" {:type "string"
                                       :description "Scope for filtering"}
                              ;; edge params
                              "from" {:type "string"
                                      :description "Source node ID for edge"}
                              "to" {:type "string"
                                    :description "Target node ID for edge"}
                              "relation" {:type "string"
                                          :enum ["implements" "supersedes" "refines" "contradicts" "depends-on" "derived-from" "applies-to"]
                                          :description "Relation type for edge"}
                              "confidence" {:type "number"
                                            :description "Confidence score 0.0-1.0"}
                              ;; impact/context params
                              "node_id" {:type "string"
                                         :description "Node ID for impact/context analysis"}
                              ;; path params
                              "from_node" {:type "string"
                                           :description "Source node for path finding"}
                              "to_node" {:type "string"
                                         :description "Target node for path finding"}
                              ;; promote params
                              "edge_id" {:type "string"
                                         :description "Edge ID to promote"}
                              "to_scope" {:type "string"
                                          :description "Target scope for promotion"}
                              ;; reground params
                              "entry_id" {:type "string"
                                          :description "Entry ID to reground"}
                              "force" {:type "boolean"
                                       :description "Force reground even if recent"}}
                 :required ["command"]}
   :handler handle-kg})

(def tools
  "Tool definitions for registration."
  [tool-def])
