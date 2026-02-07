(ns hive-mcp.tools.consolidated.kg
  "Consolidated Knowledge Graph CLI tool.

   Subcommands: traverse, edge, impact, subgraph, stats, path, context,
                promote, reground, batch-edge, batch-traverse

   Usage via MCP: kg {\"command\": \"traverse\", \"start_node\": \"mem-123\"}
   Batch usage:   kg {\"command\": \"batch-edge\", \"operations\": [{\"command\": \"edge\", ...}, ...]}

   SOLID: Facade pattern - single tool entry point for KG operations.
   CLARITY: L - Thin adapter delegating to domain handlers.
   CLARITY: C - Batch handlers composed via make-batch-handler HOF."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler make-batch-handler]]
            [hive-mcp.tools.kg :as kg-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def ^:private edge-handlers
  "Handlers subset for batch-edge operations."
  {:edge kg-handlers/handle-kg-add-edge})

(def ^:private traverse-handlers
  "Handlers subset for batch-traverse operations."
  {:traverse kg-handlers/handle-kg-traverse})

(def handle-batch-edge
  "Batch edge creation: accepts {:operations [{:command \"edge\", :from ..., :to ..., :relation ...}, ...], :parallel bool}.
   Each operation dispatches through the edge handler. Shared params (scope, confidence, created_by)
   from outer call merge with per-op params (per-op wins on conflict)."
  (make-batch-handler edge-handlers))

(def handle-batch-traverse
  "Batch traversal: accepts {:operations [{:command \"traverse\", :start_node ..., ...}, ...], :parallel bool}.
   Each operation dispatches through the traverse handler. Shared params (direction, max_depth, scope, relations)
   from outer call merge with per-op params (per-op wins on conflict)."
  (make-batch-handler traverse-handlers))

(def handlers
  "Map of command keywords to handler functions."
  {:traverse       kg-handlers/handle-kg-traverse
   :edge           kg-handlers/handle-kg-add-edge
   :impact         kg-handlers/handle-kg-impact-analysis
   :subgraph       kg-handlers/handle-kg-subgraph
   :stats          kg-handlers/handle-kg-stats
   :path           kg-handlers/handle-kg-find-path
   :context        kg-handlers/handle-kg-node-context
   :promote        kg-handlers/handle-kg-promote
   :reground       kg-handlers/handle-kg-reground
   :batch-edge     handle-batch-edge
   :batch-traverse handle-batch-traverse})

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
   :consolidated true
   :description "Knowledge Graph operations: traverse (walk graph), edge (add relationship), impact (find dependents), subgraph (extract scope), stats (counts), path (shortest path), context (node details), promote (bubble up scope), reground (verify source). Batch: batch-edge (multiple edges), batch-traverse (multiple traversals). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["traverse" "edge" "impact" "subgraph" "stats" "path" "context" "promote" "reground" "batch-edge" "batch-traverse" "help"]
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
                                       :description "Force reground even if recent"}
                              ;; batch params (batch-edge / batch-traverse)
                              "operations" {:type "array"
                                            :items {:type "object"}
                                            :description "Array of {command, ...} objects for batch-edge/batch-traverse. Each op needs its own :command ('edge' or 'traverse') plus per-op params."}
                              "parallel" {:type "boolean"
                                          :description "Run batch operations in parallel (default: false)"}}
                 :required ["command"]}
   :handler handle-kg})

(def tools
  "Tool definitions for registration."
  [tool-def])
