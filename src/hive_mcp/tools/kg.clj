(ns hive-mcp.tools.kg
  "MCP tool handlers for Knowledge Graph operations.

   Provides MCP tools for:
   - kg_add_edge: Create relationship between nodes
   - kg_traverse: Walk graph from starting node
   - kg_impact_analysis: Find dependents before modification
   - kg_promote: Bubble knowledge up scope hierarchy
   - kg_find_path: Find shortest path between nodes
   - kg_subgraph: Extract visible subgraph for a scope
   - kg_contradictions: Find conflicting knowledge
   - kg_node_context: Get full context for a node
   - kg_reground: Re-verify entry against source, detect drift

   SOLID-S: Single Responsibility - MCP tool handlers only.
   CLARITY-I: Inputs validated at tool boundary.
   CLARITY-Y: Graceful error handling."
  (:require [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.grounding :as grounding]
            [hive-mcp.knowledge-graph.queries :as queries]
            [hive-mcp.knowledge-graph.scope :as scope]
            [hive-mcp.knowledge-graph.schema :as schema]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Validation Helpers
;;; =============================================================================

(defn- validate-node-id
  "Validate node ID is a non-empty string."
  [node-id param-name]
  (when (or (nil? node-id) (not (string? node-id)) (empty? node-id))
    {:error (str param-name " must be a non-empty string")}))

(defn- validate-relation
  "Validate relation is a valid keyword."
  [relation]
  (cond
    (nil? relation)
    {:error "relation is required"}

    (and (string? relation) (not (empty? relation)))
    ;; Convert string to keyword and validate
    (let [rel-kw (keyword relation)]
      (when-not (schema/valid-relation? rel-kw)
        {:error (str "Invalid relation '" relation "'. Valid: "
                     (pr-str schema/relation-types))}))

    (keyword? relation)
    (when-not (schema/valid-relation? relation)
      {:error (str "Invalid relation. Valid: " (pr-str schema/relation-types))})

    :else
    {:error "relation must be a string or keyword"}))

(defn- parse-relations-filter
  "Parse relations filter from string or array to set of keywords."
  [relations]
  (cond
    (nil? relations) nil
    (set? relations) relations
    (coll? relations) (set (map keyword relations))
    (string? relations) #{(keyword relations)}
    :else nil))

;;; =============================================================================
;;; Tool Handlers
;;; =============================================================================

(defn handle-kg-add-edge
  "Create a relationship between two knowledge nodes.

   Arguments:
     from       - Source node ID (memory entry ID)
     to         - Target node ID (memory entry ID)
     relation   - Relation type (implements, supersedes, refines, contradicts,
                                 depends-on, derived-from, applies-to)
     scope      - Optional scope where edge was discovered
     confidence - Optional confidence score 0.0-1.0 (default: 1.0)
     created_by - Optional agent ID creating the edge"
  [{:keys [from to relation scope confidence created_by]}]
  (log/info "kg_add_edge" {:from from :to to :relation relation})
  (try
    ;; Validate inputs
    (or (validate-node-id from "from")
        (validate-node-id to "to")
        (validate-relation relation)
        ;; Execute
        (let [relation-kw (if (keyword? relation) relation (keyword relation))
              opts (cond-> {:from from
                            :to to
                            :relation relation-kw}
                     scope (assoc :scope scope)
                     confidence (assoc :confidence confidence)
                     created_by (assoc :created-by created_by))
              edge-id (edges/add-edge! opts)]
          (mcp-json {:success true
                     :edge-id edge-id
                     :message (str "Created edge " edge-id)})))
    (catch AssertionError e
      (log/warn "kg_add_edge validation failed:" (.getMessage e))
      (mcp-error (str "Validation error: " (.getMessage e))))
    (catch Exception e
      (log/error e "kg_add_edge failed")
      (mcp-error (str "Failed to add edge: " (.getMessage e))))))

(defn handle-kg-traverse
  "Walk graph from a starting node.

   Arguments:
     start_node - Node ID to start traversal from (required)
     direction  - 'outgoing' (default), 'incoming', or 'both'
     relations  - List of relation types to follow (optional, default: all)
     max_depth  - Maximum traversal depth (optional, default: 3)
     scope      - Limit to edges visible from this scope (optional)"
  [{:keys [start_node direction relations max_depth scope]}]
  (log/info "kg_traverse" {:start start_node :direction direction})
  (try
    (or (validate-node-id start_node "start_node")
        (let [dir-kw (case direction
                       "incoming" :incoming
                       "both" :both
                       :outgoing)  ;; default
              rel-set (parse-relations-filter relations)
              opts (cond-> {:direction dir-kw}
                     rel-set (assoc :relations rel-set)
                     max_depth (assoc :max-depth max_depth)
                     scope (assoc :scope scope))
              results (queries/traverse start_node opts)]
          (mcp-json {:success true
                     :start-node start_node
                     :result-count (count results)
                     :results (mapv (fn [{:keys [node-id edge depth path]}]
                                      {:node-id node-id
                                       :relation (:kg-edge/relation edge)
                                       :confidence (:kg-edge/confidence edge)
                                       :depth depth
                                       :path path})
                                    results)})))
    (catch Exception e
      (log/error e "kg_traverse failed")
      (mcp-error (str "Traversal failed: " (.getMessage e))))))

(defn handle-kg-impact-analysis
  "Find all nodes that depend on given node.
   Useful before modifying or deleting an entry.

   Arguments:
     node_id   - Node to analyze (required)
     max_depth - Maximum depth for transitive dependencies (optional, default: 5)
     scope     - Limit to visible scopes (optional)"
  [{:keys [node_id max_depth scope]}]
  (log/info "kg_impact_analysis" {:node node_id})
  (try
    (or (validate-node-id node_id "node_id")
        (let [opts (cond-> {}
                     max_depth (assoc :max-depth max_depth)
                     scope (assoc :scope scope))
              result (queries/impact-analysis node_id opts)]
          (mcp-json {:success true
                     :node-id node_id
                     :direct-count (count (:direct result))
                     :transitive-count (count (:transitive result))
                     :total-count (:total-count result)
                     :direct (:direct result)
                     :transitive (:transitive result)
                     :by-relation (:by-relation result)})))
    (catch Exception e
      (log/error e "kg_impact_analysis failed")
      (mcp-error (str "Impact analysis failed: " (.getMessage e))))))

(defn handle-kg-promote
  "Promote knowledge (edge) to a broader scope.

   This creates a new edge in the target scope, preserving the original.
   Used to 'bubble up' valuable knowledge from submodule to parent project.

   Arguments:
     edge_id  - Edge to promote (required)
     to_scope - Target scope to promote to (required)

   Returns:
     New edge ID in target scope"
  [{:keys [edge_id to_scope]}]
  (log/info "kg_promote" {:edge edge_id :to-scope to_scope})
  (try
    (cond
      (or (nil? edge_id) (empty? edge_id))
      (mcp-error "edge_id is required")

      (or (nil? to_scope) (empty? to_scope))
      (mcp-error "to_scope is required")

      :else
      (if-let [original-edge (edges/get-edge edge_id)]
        ;; Create new edge in target scope
        (let [new-edge-id (edges/add-edge!
                           {:from (:kg-edge/from original-edge)
                            :to (:kg-edge/to original-edge)
                            :relation (:kg-edge/relation original-edge)
                            :scope to_scope
                            :confidence (:kg-edge/confidence original-edge)
                            :created-by (str "promoted-from:" edge_id)})]
          (mcp-json {:success true
                     :original-edge-id edge_id
                     :promoted-edge-id new-edge-id
                     :to-scope to_scope
                     :message (str "Promoted edge to scope " to_scope)}))
        (mcp-error (str "Edge not found: " edge_id))))
    (catch Exception e
      (log/error e "kg_promote failed")
      (mcp-error (str "Promotion failed: " (.getMessage e))))))

(defn handle-kg-find-path
  "Find shortest path between two nodes.

   Arguments:
     from_node - Source node ID (required)
     to_node   - Target node ID (required)
     direction - 'outgoing', 'incoming', or 'both' (default: 'both')
     relations - List of relations to follow (optional)
     max_depth - Maximum search depth (optional, default: 10)
     scope     - Limit to visible scopes (optional)"
  [{:keys [from_node to_node direction relations max_depth scope]}]
  (log/info "kg_find_path" {:from from_node :to to_node})
  (try
    (or (validate-node-id from_node "from_node")
        (validate-node-id to_node "to_node")
        (let [dir-kw (case direction
                       "outgoing" :outgoing
                       "incoming" :incoming
                       :both)
              rel-set (parse-relations-filter relations)
              opts (cond-> {:direction dir-kw}
                     rel-set (assoc :relations rel-set)
                     max_depth (assoc :max-depth max_depth)
                     scope (assoc :scope scope))
              result (queries/find-path from_node to_node opts)]
          (if result
            (mcp-json {:success true
                       :path-exists true
                       :path (:path result)
                       :length (:length result)
                       :edges (mapv #(select-keys % [:kg-edge/id :kg-edge/relation
                                                     :kg-edge/confidence])
                                    (:edges result))})
            (mcp-json {:success true
                       :path-exists false
                       :message "No path found between nodes"}))))
    (catch Exception e
      (log/error e "kg_find_path failed")
      (mcp-error (str "Path finding failed: " (.getMessage e))))))

(defn handle-kg-subgraph
  "Extract subgraph visible from a scope.

   Arguments:
     scope - Scope string (required, e.g., 'hive-mcp:agora')"
  [{:keys [scope]}]
  (log/info "kg_subgraph" {:scope scope})
  (try
    (if (or (nil? scope) (empty? scope))
      (mcp-error "scope is required")
      (let [result (queries/subgraph scope)]
        (mcp-json {:success true
                   :scope scope
                   :visible-scopes (scope/visible-scopes scope)
                   :node-count (:node-count result)
                   :edge-count (:edge-count result)
                   :nodes (:nodes result)
                   :edges (mapv #(select-keys % [:kg-edge/id :kg-edge/from
                                                 :kg-edge/to :kg-edge/relation
                                                 :kg-edge/confidence :kg-edge/scope])
                                (:edges result))})))
    (catch Exception e
      (log/error e "kg_subgraph failed")
      (mcp-error (str "Subgraph extraction failed: " (.getMessage e))))))

(defn handle-kg-contradictions
  "Find edges with :contradicts relation in scope.

   Arguments:
     scope - Optional scope to limit search"
  [{:keys [scope]}]
  (log/info "kg_contradictions" {:scope scope})
  (try
    (let [results (queries/find-contradictions scope)]
      (mcp-json {:success true
                 :scope (or scope "global")
                 :count (count results)
                 :contradictions results}))
    (catch Exception e
      (log/error e "kg_contradictions failed")
      (mcp-error (str "Contradiction search failed: " (.getMessage e))))))

(defn handle-kg-node-context
  "Get full context for a node: incoming, outgoing, confidence stats.

   Arguments:
     node_id - Node to get context for (required)"
  [{:keys [node_id]}]
  (log/info "kg_node_context" {:node node_id})
  (try
    (or (validate-node-id node_id "node_id")
        (let [result (queries/get-node-context node_id)]
          (mcp-json {:success true
                     :node-id node_id
                     :incoming (-> (:incoming result)
                                   (update :edges #(mapv (fn [e]
                                                           (select-keys e [:kg-edge/id
                                                                           :kg-edge/from
                                                                           :kg-edge/relation
                                                                           :kg-edge/confidence]))
                                                         %)))
                     :outgoing (-> (:outgoing result)
                                   (update :edges #(mapv (fn [e]
                                                           (select-keys e [:kg-edge/id
                                                                           :kg-edge/to
                                                                           :kg-edge/relation
                                                                           :kg-edge/confidence]))
                                                         %)))
                     :confidence (:confidence result)
                     :scopes (:scopes result)})))
    (catch Exception e
      (log/error e "kg_node_context failed")
      (mcp-error (str "Context retrieval failed: " (.getMessage e))))))

(defn handle-kg-stats
  "Get statistics about the Knowledge Graph.

   Returns edge counts by relation type and scope."
  [_]
  (log/info "kg_stats")
  (try
    (let [stats (edges/edge-stats)]
      (mcp-json {:success true
                 :total-edges (:total-edges stats)
                 :by-relation (:by-relation stats)
                 :by-scope (:by-scope stats)}))
    (catch Exception e
      (log/error e "kg_stats failed")
      (mcp-error (str "Stats retrieval failed: " (.getMessage e))))))

(defn handle-kg-reground
  "Re-ground a knowledge entry by verifying against its source file.

   Arguments:
     entry_id - Entry ID to re-ground (required)
     force    - Force re-ground even if recently grounded (optional)"
  [{:keys [entry_id force]}]
  (log/info "kg_reground" {:entry-id entry_id :force force})
  (try
    (or (validate-node-id entry_id "entry_id")
        (let [result (grounding/reground-entry! entry_id)]
          (mcp-json {:success true
                     :status (name (:status result))
                     :drift? (:drift? result)
                     :entry-id entry_id
                     :source-file (:source-file result)
                     :updated? (:updated? result)})))
    (catch Exception e
      (log/error e "kg_reground failed")
      (mcp-error (str "Re-grounding failed: " (.getMessage e))))))

;;; =============================================================================
;;; Tool Definitions
;;; =============================================================================

(def tools
  [{:name "kg_add_edge"
    :description "Create a relationship (edge) between two knowledge nodes in the Knowledge Graph. Relations: implements (realizes principle), supersedes (replaces), refines (improves), contradicts (conflicts), depends-on (requires), derived-from (synthesis origin), applies-to (scope applicability)."
    :inputSchema {:type "object"
                  :properties {"from" {:type "string"
                                       :description "Source node ID (memory entry ID)"}
                               "to" {:type "string"
                                     :description "Target node ID (memory entry ID)"}
                               "relation" {:type "string"
                                           :enum ["implements" "supersedes" "refines"
                                                  "contradicts" "depends-on"
                                                  "derived-from" "applies-to"]
                                           :description "Relation type"}
                               "scope" {:type "string"
                                        :description "Scope where edge was discovered (optional)"}
                               "confidence" {:type "number"
                                             :description "Confidence score 0.0-1.0 (default: 1.0)"}
                               "created_by" {:type "string"
                                             :description "Agent ID creating edge (optional)"}}
                  :required ["from" "to" "relation"]}
    :handler handle-kg-add-edge}

   {:name "kg_traverse"
    :description "Walk the Knowledge Graph from a starting node using BFS. Returns nodes reachable within max_depth, with paths and edge information."
    :inputSchema {:type "object"
                  :properties {"start_node" {:type "string"
                                             :description "Node ID to start traversal from"}
                               "direction" {:type "string"
                                            :enum ["outgoing" "incoming" "both"]
                                            :description "Edge direction to follow (default: outgoing)"}
                               "relations" {:type "array"
                                            :items {:type "string"}
                                            :description "Relation types to follow (default: all)"}
                               "max_depth" {:type "integer"
                                            :description "Maximum traversal depth (default: 3)"}
                               "scope" {:type "string"
                                        :description "Limit to edges visible from this scope"}}
                  :required ["start_node"]}
    :handler handle-kg-traverse}

   {:name "kg_impact_analysis"
    :description "Find all nodes that depend on given node. Use before modifying or deleting a memory entry to understand impact. Returns direct and transitive dependents."
    :inputSchema {:type "object"
                  :properties {"node_id" {:type "string"
                                          :description "Node to analyze"}
                               "max_depth" {:type "integer"
                                            :description "Max depth for transitive deps (default: 5)"}
                               "scope" {:type "string"
                                        :description "Limit to visible scopes"}}
                  :required ["node_id"]}
    :handler handle-kg-impact-analysis}

   {:name "kg_promote"
    :description "Promote knowledge (edge) to a broader scope. Creates a new edge in target scope, preserving original. Use to 'bubble up' valuable knowledge from submodule to parent."
    :inputSchema {:type "object"
                  :properties {"edge_id" {:type "string"
                                          :description "Edge ID to promote"}
                               "to_scope" {:type "string"
                                           :description "Target scope (e.g., 'hive-mcp' or 'global')"}}
                  :required ["edge_id" "to_scope"]}
    :handler handle-kg-promote}

   {:name "kg_find_path"
    :description "Find shortest path between two nodes in the Knowledge Graph. Returns path and edges if exists."
    :inputSchema {:type "object"
                  :properties {"from_node" {:type "string"
                                            :description "Source node ID"}
                               "to_node" {:type "string"
                                          :description "Target node ID"}
                               "direction" {:type "string"
                                            :enum ["outgoing" "incoming" "both"]
                                            :description "Edge direction (default: both)"}
                               "relations" {:type "array"
                                            :items {:type "string"}
                                            :description "Relation types to follow"}
                               "max_depth" {:type "integer"
                                            :description "Max search depth (default: 10)"}
                               "scope" {:type "string"
                                        :description "Limit to visible scopes"}}
                  :required ["from_node" "to_node"]}
    :handler handle-kg-find-path}

   {:name "kg_subgraph"
    :description "Extract subgraph visible from a scope. Returns all nodes and edges accessible from that scope level."
    :inputSchema {:type "object"
                  :properties {"scope" {:type "string"
                                        :description "Scope to extract (e.g., 'hive-mcp:agora')"}}
                  :required ["scope"]}
    :handler handle-kg-subgraph}

   {:name "kg_contradictions"
    :description "Find knowledge contradictions in scope. Returns pairs of nodes connected by :contradicts edges."
    :inputSchema {:type "object"
                  :properties {"scope" {:type "string"
                                        :description "Scope to search (optional, default: all)"}}
                  :required []}
    :handler handle-kg-contradictions}

   {:name "kg_node_context"
    :description "Get full context for a node: incoming edges, outgoing edges, confidence stats, and scopes. Useful for understanding a node's role in the graph."
    :inputSchema {:type "object"
                  :properties {"node_id" {:type "string"
                                          :description "Node to get context for"}}
                  :required ["node_id"]}
    :handler handle-kg-node-context}

   {:name "kg_stats"
    :description "Get Knowledge Graph statistics: total edges, counts by relation type, counts by scope."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-kg-stats}

   {:name "kg_reground"
    :description "Re-verify a knowledge entry against its source file and update grounding timestamp. Detects drift when source content has changed since last grounding. Returns status: regrounded (success), needs-review (drift detected), source-missing (file not found)."
    :inputSchema {:type "object"
                  :properties {"entry_id" {:type "string"
                                           :description "Entry ID to re-ground"}
                               "force" {:type "boolean"
                                        :description "Force re-ground even if recently grounded (optional)"}}
                  :required ["entry_id"]}
    :handler handle-kg-reground}])
