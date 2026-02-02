(ns hive-mcp.schema.tools
  "Malli schemas for MCP tool parameters.

   Defines input/output schemas for all MCP tools, enabling:
   - Runtime validation at tool boundaries (CLARITY-I)
   - Documentation generation
   - Test data generation with malli.generator

   Tool Categories:
   - Agent tools: spawn, dispatch, kill, status
   - Memory tools: add, query, query-metadata, get-full
   - KG tools: add-edge, traverse, impact-analysis

   SOLID: SRP - Schema definitions only.
   CLARITY: I - Inputs validated at tool boundary."
  (:require [malli.core :as m]
            [hive-mcp.schema.memory :as mem]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Common Types
;; =============================================================================

(def NonEmptyString
  "Non-empty string type."
  [:string {:min 1}])

(def OptionalString
  "Optional string (may be nil or string)."
  [:maybe :string])

(def PositiveInt
  "Positive integer (> 0)."
  [:int {:min 1}])

(def NonNegativeInt
  "Non-negative integer (>= 0)."
  [:int {:min 0}])

(def Confidence
  "Confidence score between 0.0 and 1.0."
  [:double {:min 0.0 :max 1.0}])

;; =============================================================================
;; Agent Tool Schemas
;; =============================================================================

(def AgentType
  "Agent type for spawning.
   - ling: Worker agent in the swarm
   - drone: Autonomous long-running agent"
  [:enum "ling" "drone"])

(def TerminalType
  "Terminal type for agent spawn."
  [:enum "vterm" "eat"])

(def AgentSpawnParams
  "Parameters for swarm_spawn tool.

   Spawns a new Claude agent (ling) in the swarm.

   Required: name
   Optional: presets, cwd, role, terminal, kanban_task_id"
  [:map
   [:name NonEmptyString]
   [:presets {:optional true} [:maybe [:vector NonEmptyString]]]
   [:cwd {:optional true} OptionalString]
   [:role {:optional true} OptionalString]
   [:terminal {:optional true} [:maybe TerminalType]]
   [:kanban_task_id {:optional true} OptionalString]])

(def AgentDispatchParams
  "Parameters for swarm_dispatch tool.

   Dispatches a task to an available ling.

   Required: task
   Optional: ling_id (specific target), priority, context"
  [:map
   [:task NonEmptyString]
   [:ling_id {:optional true} OptionalString]
   [:priority {:optional true} [:enum "high" "medium" "low"]]
   [:context {:optional true} OptionalString]])

(def AgentKillParams
  "Parameters for swarm_kill tool.

   Terminates a running agent.

   Required: slave_id (or 'all' for batch kill)
   Optional: force (bypass kill guard), directory (for project-scoped kill)"
  [:map
   [:slave_id NonEmptyString]
   [:force {:optional true} [:maybe :boolean]]
   [:directory {:optional true} OptionalString]])

(def AgentStatusParams
  "Parameters for swarm_status tool.

   Gets swarm status - no required params."
  [:map
   [:directory {:optional true} OptionalString]
   [:include_details {:optional true} [:maybe :boolean]]])

(def AgentCollectParams
  "Parameters for swarm_collect tool.

   Collects results from completed agents.

   Optional: ling_id (specific agent), timeout_ms"
  [:map
   [:ling_id {:optional true} OptionalString]
   [:timeout_ms {:optional true} PositiveInt]])

(def AgentBroadcastParams
  "Parameters for swarm_broadcast tool.

   Broadcasts a message to all lings.

   Required: message
   Optional: scope (project scope)"
  [:map
   [:message NonEmptyString]
   [:scope {:optional true} OptionalString]])

;; =============================================================================
;; Memory Tool Schemas
;; =============================================================================

(def MemoryAddParams
  "Parameters for mcp_memory_add tool.

   Creates a new memory entry in the knowledge base.

   Required: type, content
   Optional: tags, duration, directory, agent_id, kg_* relationships, abstraction_level"
  [:map
   [:type mem/MemoryType]
   [:content [:string {:min 1}]]
   [:tags {:optional true} [:maybe [:or mem/MemoryTags :string]]]  ; String for JSON array coercion
   [:duration {:optional true} [:maybe mem/MemoryDuration]]
   [:directory {:optional true} OptionalString]
   [:agent_id {:optional true} OptionalString]
   ;; KG relationship params
   [:kg_implements {:optional true} [:maybe [:or [:vector :string] :string]]]
   [:kg_supersedes {:optional true} [:maybe [:or [:vector :string] :string]]]
   [:kg_depends_on {:optional true} [:maybe [:or [:vector :string] :string]]]
   [:kg_refines {:optional true} [:maybe [:or [:vector :string] :string]]]
   [:abstraction_level {:optional true} [:maybe mem/AbstractionLevel]]])

(def MemoryQueryParams
  "Parameters for mcp_memory_query tool.

   Queries memory entries with filtering.

   Required: type
   Optional: tags, limit, duration, scope, directory"
  [:map
   [:type {:optional true} [:maybe mem/MemoryType]]
   [:tags {:optional true} [:maybe [:or mem/MemoryTags :string]]]
   [:limit {:optional true} [:maybe [:or PositiveInt :string]]]  ; String for JSON coercion
   [:duration {:optional true} [:maybe mem/MemoryDuration]]
   [:scope {:optional true} OptionalString]
   [:directory {:optional true} OptionalString]])

(def MemoryQueryMetadataParams
  "Parameters for mcp_memory_query_metadata tool.

   Queries memory entries returning only metadata (token-efficient).
   Same params as MemoryQueryParams."
  MemoryQueryParams)

(def MemoryGetFullParams
  "Parameters for mcp_memory_get_full tool.

   Gets complete entry by ID.

   Required: id"
  [:map
   [:id NonEmptyString]])

(def MemoryUpdateTagsParams
  "Parameters for mcp_memory_update_tags tool.

   Updates tags on an existing entry.

   Required: id, tags"
  [:map
   [:id NonEmptyString]
   [:tags mem/MemoryTags]])

(def MemoryCheckDuplicateParams
  "Parameters for mcp_memory_check_duplicate tool.

   Checks if content already exists.

   Required: type, content
   Optional: directory"
  [:map
   [:type mem/MemoryType]
   [:content NonEmptyString]
   [:directory {:optional true} OptionalString]])

(def MemorySearchSemanticParams
  "Parameters for mcp_memory_search_semantic tool.

   Semantic similarity search.

   Required: query
   Optional: type, limit, threshold, scope"
  [:map
   [:query NonEmptyString]
   [:type {:optional true} [:maybe mem/MemoryType]]
   [:limit {:optional true} [:maybe PositiveInt]]
   [:threshold {:optional true} [:maybe Confidence]]
   [:scope {:optional true} OptionalString]])

;; =============================================================================
;; Knowledge Graph Tool Schemas
;; =============================================================================

(def KGRelationType
  "Valid KG edge relation types."
  [:enum
   "implements" "supersedes" "refines" "contradicts"
   "depends-on" "derived-from" "applies-to" "co-accessed"
   "projects-to"])

(def KGSourceType
  "How an edge was established."
  [:enum "manual" "automated" "inferred" "co-access"])

(def KGDirection
  "Traversal direction."
  [:enum "outgoing" "incoming" "both"])

(def KGAddEdgeParams
  "Parameters for kg_add_edge tool.

   Creates a relationship between two knowledge nodes.

   Required: from, to, relation
   Optional: scope, confidence, created_by, source_type"
  [:map
   [:from NonEmptyString]
   [:to NonEmptyString]
   [:relation KGRelationType]
   [:scope {:optional true} OptionalString]
   [:confidence {:optional true} [:maybe Confidence]]
   [:created_by {:optional true} OptionalString]
   [:source_type {:optional true} [:maybe KGSourceType]]])

(def KGTraverseParams
  "Parameters for kg_traverse tool.

   Walks the graph from a starting node.

   Required: start_node
   Optional: direction, relations, max_depth, scope"
  [:map
   [:start_node NonEmptyString]
   [:direction {:optional true} [:maybe KGDirection]]
   [:relations {:optional true} [:maybe [:or [:vector KGRelationType] KGRelationType]]]
   [:max_depth {:optional true} [:maybe PositiveInt]]
   [:scope {:optional true} OptionalString]])

(def KGImpactAnalysisParams
  "Parameters for kg_impact_analysis tool.

   Finds all nodes that depend on a given node.

   Required: node_id
   Optional: max_depth, scope"
  [:map
   [:node_id NonEmptyString]
   [:max_depth {:optional true} [:maybe PositiveInt]]
   [:scope {:optional true} OptionalString]])

(def KGPromoteParams
  "Parameters for kg_promote tool.

   Promotes knowledge to a broader scope.

   Required: edge_id, to_scope"
  [:map
   [:edge_id NonEmptyString]
   [:to_scope NonEmptyString]])

(def KGFindPathParams
  "Parameters for kg_find_path tool.

   Finds shortest path between two nodes.

   Required: from_node, to_node
   Optional: max_depth, relations"
  [:map
   [:from_node NonEmptyString]
   [:to_node NonEmptyString]
   [:max_depth {:optional true} [:maybe PositiveInt]]
   [:relations {:optional true} [:maybe [:vector KGRelationType]]]])

(def KGSubgraphParams
  "Parameters for kg_subgraph tool.

   Extracts subgraph visible from a scope.

   Required: scope
   Optional: include_global"
  [:map
   [:scope NonEmptyString]
   [:include_global {:optional true} [:maybe :boolean]]])

(def KGNodeContextParams
  "Parameters for kg_node_context tool.

   Gets full context for a node.

   Required: node_id
   Optional: depth"
  [:map
   [:node_id NonEmptyString]
   [:depth {:optional true} [:maybe PositiveInt]]])

(def KGRegroundParams
  "Parameters for kg_reground tool.

   Re-verifies an entry against source.

   Required: entry_id
   Optional: source_path"
  [:map
   [:entry_id NonEmptyString]
   [:source_path {:optional true} OptionalString]])

(def KGContradictionsParams
  "Parameters for kg_contradictions tool.

   Finds conflicting knowledge in a scope.

   Required: scope"
  [:map
   [:scope NonEmptyString]])

(def KGStatsParams
  "Parameters for kg_stats tool.

   Gets KG statistics - no required params."
  [:map
   [:scope {:optional true} OptionalString]])

;; =============================================================================
;; Response Schemas
;; =============================================================================

(def MCPSuccessResponse
  "Standard MCP success response."
  [:map
   [:type [:= "text"]]
   [:text :string]
   [:isError {:optional true} [:= false]]])

(def MCPErrorResponse
  "Standard MCP error response."
  [:map
   [:type [:= "text"]]
   [:text :string]
   [:isError [:= true]]])

(def MCPResponse
  "MCP tool response - either success or error."
  [:or MCPSuccessResponse MCPErrorResponse])

;; =============================================================================
;; Schema Registry
;; =============================================================================

(def registry
  "Schema registry entries for tool parameters.

   Usage:
   ```clojure
   (mr/set-default-registry!
     (mr/composite-registry
       (m/default-schemas)
       hive-mcp.schema.tools/registry))
   ```"
  {;; Agent tools
   :tools/agent-spawn-params AgentSpawnParams
   :tools/agent-dispatch-params AgentDispatchParams
   :tools/agent-kill-params AgentKillParams
   :tools/agent-status-params AgentStatusParams
   :tools/agent-collect-params AgentCollectParams
   :tools/agent-broadcast-params AgentBroadcastParams

   ;; Memory tools
   :tools/memory-add-params MemoryAddParams
   :tools/memory-query-params MemoryQueryParams
   :tools/memory-get-full-params MemoryGetFullParams
   :tools/memory-update-tags-params MemoryUpdateTagsParams
   :tools/memory-check-duplicate-params MemoryCheckDuplicateParams
   :tools/memory-search-semantic-params MemorySearchSemanticParams

   ;; KG tools
   :tools/kg-add-edge-params KGAddEdgeParams
   :tools/kg-traverse-params KGTraverseParams
   :tools/kg-impact-analysis-params KGImpactAnalysisParams
   :tools/kg-promote-params KGPromoteParams
   :tools/kg-find-path-params KGFindPathParams
   :tools/kg-subgraph-params KGSubgraphParams
   :tools/kg-node-context-params KGNodeContextParams
   :tools/kg-reground-params KGRegroundParams

   ;; Common
   :tools/non-empty-string NonEmptyString
   :tools/confidence Confidence
   :tools/mcp-response MCPResponse})

;; =============================================================================
;; Validators
;; =============================================================================

(defn validate-params
  "Validate tool parameters against a schema.

   Returns {:valid true} or {:valid false :errors explanation}."
  [schema params]
  (if (m/validate schema params)
    {:valid true}
    {:valid false
     :errors (m/explain schema params)}))

(defn coerce-and-validate
  "Coerce and validate parameters with schema transformers.

   Handles common MCP coercions (string→int, string→array).
   Returns {:valid true :params coerced} or {:valid false :errors ...}."
  [schema params]
  ;; For now, just validate - coercion can be added with malli.transform
  (validate-params schema params))

(comment
  ;; Example usage

  (m/validate AgentSpawnParams
              {:name "worker-1"
               :cwd "/home/user/project"
               :presets ["research" "clojure"]})
  ;; => true

  (m/validate MemoryAddParams
              {:type "decision"
               :content "Use Malli for validation"
               :tags ["architecture"]
               :duration "long"})
  ;; => true

  (m/validate KGAddEdgeParams
              {:from "entry-1"
               :to "entry-2"
               :relation "implements"
               :confidence 0.95})
  ;; => true

  (m/explain AgentSpawnParams {:cwd "/tmp"})
  ;; => {:errors [{:path [:name] ...}]}
  )
