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

   Versioning tools (Yggdrasil integration):
   - kg_branch: Create a new branch
   - kg_checkout: Switch to a branch
   - kg_branches: List all branches
   - kg_snapshot_id: Get current commit ID
   - kg_history: Get commit history

   SOLID-S: Single Responsibility - MCP tool handlers only.
   CLARITY-I: Inputs validated at tool boundary.
   CLARITY-Y: Graceful error handling."
  (:require [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.grounding :as grounding]
            [hive-mcp.knowledge-graph.queries :as queries]
            [hive-mcp.knowledge-graph.scope :as scope]
            [hive-mcp.knowledge-graph.schema :as schema]
            [hive-mcp.knowledge-graph.versioning :as versioning]
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

(defn handle-kg-backfill-grounding
  "Batch-discover and ground all Chroma entries with source-file metadata.

   Scans memory entries, finds those linked to source files, computes
   current content hashes, and sets grounded-at timestamps. Detects drift
   where source files have changed since knowledge was abstracted.

   Arguments:
     project_id   - Filter to specific project (optional, default: all)
     limit        - Max entries to process (optional, default: 500)
     force        - Re-ground even if already grounded (optional, default: false)
     max_age_days - Only re-ground if older than N days (optional, default: 7)"
  [{:keys [project_id limit force max_age_days]}]
  (log/info "kg_backfill_grounding" {:project-id project_id :limit limit :force force})
  (try
    (let [opts (cond-> {}
                 project_id (assoc :project-id project_id)
                 limit (assoc :limit limit)
                 force (assoc :force? force)
                 max_age_days (assoc :max-age-days max_age_days))
          result (grounding/backfill-grounding! opts)]
      (if (:error result)
        (mcp-error (str "Backfill failed: " (:error result)))
        (mcp-json {:success true
                   :total-scanned (:total-scanned result)
                   :with-source (:with-source result)
                   :processed (:processed result)
                   :by-status (:by-status result)
                   :drifted-entries (:drifted-entries result)})))
    (catch Exception e
      (log/error e "kg_backfill_grounding failed")
      (mcp-error (str "Backfill grounding failed: " (.getMessage e))))))

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
    :handler handle-kg-reground}

   {:name "kg_backfill_grounding"
    :description "Batch-discover and ground all memory entries with source-file metadata. Scans Chroma, computes content hashes, sets grounded-at timestamps, and detects drift. Use to bootstrap grounding for existing entries or periodically refresh staleness."
    :inputSchema {:type "object"
                  :properties {"project_id" {:type "string"
                                             :description "Filter to specific project (optional, default: all)"}
                               "limit" {:type "integer"
                                        :description "Max entries to process (optional, default: 500)"}
                               "force" {:type "boolean"
                                        :description "Re-ground even if already grounded (optional, default: false)"}
                               "max_age_days" {:type "integer"
                                               :description "Only re-ground entries older than N days (optional, default: 7)"}}
                  :required []}
    :handler handle-kg-backfill-grounding}])

;;; =============================================================================
;;; Versioning Tool Handlers (Yggdrasil Integration)
;;; =============================================================================

(defn handle-kg-branch
  "Create a new branch in the versioned Knowledge Graph.

   Arguments:
     name - Branch name (required, e.g., 'experiment', 'agent-1-exploration')
     from - Optional source branch or snapshot-id to branch from"
  [{:keys [name from]}]
  (log/info "kg_branch" {:name name :from from})
  (try
    (cond
      (not (versioning/versioning-available?))
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")

      (or (nil? name) (empty? name))
      (mcp-error "name is required")

      :else
      (let [branch-kw (keyword name)
            result (if from
                     (versioning/branch! branch-kw from)
                     (versioning/branch! branch-kw))]
        (if result
          (mcp-json {:success true
                     :branch (clojure.core/name branch-kw)
                     :from (or from "current")
                     :message (str "Created branch " name)})
          (mcp-error (str "Failed to create branch " name)))))
    (catch Exception e
      (log/error e "kg_branch failed")
      (mcp-error (str "Branch creation failed: " (.getMessage e))))))

(defn handle-kg-checkout
  "Switch to a different branch in the versioned Knowledge Graph.

   Arguments:
     name - Branch name to switch to (required)"
  [{:keys [name]}]
  (log/info "kg_checkout" {:name name})
  (try
    (cond
      (not (versioning/versioning-available?))
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")

      (or (nil? name) (empty? name))
      (mcp-error "name is required")

      :else
      (let [branch-kw (keyword name)
            result (versioning/checkout branch-kw)]
        (if result
          (mcp-json {:success true
                     :branch (clojure.core/name branch-kw)
                     :snapshot-id (versioning/snapshot-id)
                     :message (str "Switched to branch " name)})
          (mcp-error (str "Failed to checkout branch " name ". Branch may not exist.")))))
    (catch Exception e
      (log/error e "kg_checkout failed")
      (mcp-error (str "Checkout failed: " (.getMessage e))))))

(defn handle-kg-branches
  "List all branches in the versioned Knowledge Graph."
  [_]
  (log/info "kg_branches")
  (try
    (if-not (versioning/versioning-available?)
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")
      (let [branches (versioning/branches)
            current (versioning/current-branch)]
        (mcp-json {:success true
                   :current-branch (when current (clojure.core/name current))
                   :branches (if branches
                               (mapv clojure.core/name branches)
                               [])
                   :count (count (or branches []))})))
    (catch Exception e
      (log/error e "kg_branches failed")
      (mcp-error (str "Failed to list branches: " (.getMessage e))))))

(defn handle-kg-snapshot-id
  "Get the current commit/snapshot ID of the versioned Knowledge Graph."
  [_]
  (log/info "kg_snapshot_id")
  (try
    (if-not (versioning/versioning-available?)
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")
      (let [snap-id (versioning/snapshot-id)
            parent-ids (versioning/parent-ids)
            branch (versioning/current-branch)]
        (mcp-json {:success true
                   :snapshot-id snap-id
                   :parent-ids (vec (or parent-ids []))
                   :branch (when branch (clojure.core/name branch))})))
    (catch Exception e
      (log/error e "kg_snapshot_id failed")
      (mcp-error (str "Failed to get snapshot ID: " (.getMessage e))))))

(defn handle-kg-history
  "Get commit history for the current branch.

   Arguments:
     limit - Maximum number of commits to return (optional, default: 100)"
  [{:keys [limit]}]
  (log/info "kg_history" {:limit limit})
  (try
    (if-not (versioning/versioning-available?)
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")
      (let [opts (when limit {:limit limit})
            history (versioning/history opts)
            branch (versioning/current-branch)]
        (mcp-json {:success true
                   :branch (when branch (clojure.core/name branch))
                   :count (count history)
                   :commits (vec history)})))
    (catch Exception e
      (log/error e "kg_history failed")
      (mcp-error (str "Failed to get history: " (.getMessage e))))))

(defn handle-kg-merge
  "Merge a source branch or snapshot into the current branch.

   Arguments:
     source - Source branch name or snapshot-id to merge (required)"
  [{:keys [source]}]
  (log/info "kg_merge" {:source source})
  (try
    (cond
      (not (versioning/versioning-available?))
      (mcp-error "Versioning not available. Ensure Datahike backend is configured and Yggdrasil is on classpath.")

      (or (nil? source) (empty? source))
      (mcp-error "source is required")

      :else
      (let [;; Try as branch keyword first, fall back to snapshot-id string
            source-val (if (and (string? source)
                                (not (re-matches #"^[0-9a-f-]{36}$" source)))
                         (keyword source)
                         source)
            result (versioning/merge! source-val)]
        (if result
          (mcp-json {:success true
                     :source (str source)
                     :snapshot-id (versioning/snapshot-id)
                     :message (str "Merged " source " into current branch")})
          (mcp-error (str "Failed to merge " source)))))
    (catch Exception e
      (log/error e "kg_merge failed")
      (mcp-error (str "Merge failed: " (.getMessage e))))))

(defn handle-kg-versioning-status
  "Get the current versioning status of the Knowledge Graph."
  [_]
  (log/info "kg_versioning_status")
  (try
    (let [status (versioning/status)]
      (mcp-json {:success true
                 :available (:available status)
                 :system-id (:system-id status)
                 :branch (when (:branch status) (clojure.core/name (:branch status)))
                 :snapshot-id (:snapshot-id status)
                 :branches (when (:branches status)
                             (mapv clojure.core/name (:branches status)))}))
    (catch Exception e
      (log/error e "kg_versioning_status failed")
      (mcp-error (str "Failed to get versioning status: " (.getMessage e))))))

;;; =============================================================================
;;; Versioning Tool Definitions
;;; =============================================================================

(def versioning-tools
  [{:name "kg_branch"
    :description "Create a new branch in the versioned Knowledge Graph. Branches enable parallel exploration of knowledge without affecting the main timeline. Useful for multi-agent scenarios where each agent experiments independently."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Branch name (e.g., 'experiment', 'agent-1-exploration')"}
                               "from" {:type "string"
                                       :description "Source branch or snapshot-id to branch from (optional, default: current)"}}
                  :required ["name"]}
    :handler handle-kg-branch}

   {:name "kg_checkout"
    :description "Switch to a different branch in the versioned Knowledge Graph. Changes the active branch for all subsequent KG operations."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Branch name to switch to"}}
                  :required ["name"]}
    :handler handle-kg-checkout}

   {:name "kg_branches"
    :description "List all branches in the versioned Knowledge Graph. Shows which branches exist and which is currently active."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-kg-branches}

   {:name "kg_snapshot_id"
    :description "Get the current commit/snapshot ID of the versioned Knowledge Graph. Returns the unique identifier for the current state, useful for bookmarking or branching from specific points."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-kg-snapshot-id}

   {:name "kg_history"
    :description "Get commit history for the current branch. Returns chronological list of snapshot IDs, newest first."
    :inputSchema {:type "object"
                  :properties {"limit" {:type "integer"
                                        :description "Maximum commits to return (optional, default: 100)"}}
                  :required []}
    :handler handle-kg-history}

   {:name "kg_merge"
    :description "Merge a source branch or snapshot into the current branch. Combines knowledge from parallel exploration timelines."
    :inputSchema {:type "object"
                  :properties {"source" {:type "string"
                                         :description "Source branch name or snapshot-id to merge"}}
                  :required ["source"]}
    :handler handle-kg-merge}

   {:name "kg_versioning_status"
    :description "Get the current versioning status of the Knowledge Graph. Shows whether versioning is available, current branch, snapshot, and all branches."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-kg-versioning-status}])

;;; =============================================================================
;;; Migration Tool Handlers
;;; =============================================================================

(defn handle-kg-migrate
  "Migrate KG data from one backend to another.

   Arguments:
     source_backend - Current backend (:datascript, :datalevin, or :datahike)
     target_backend - Target backend to migrate to
     dry_run        - If true, show what would be migrated without doing it
     export_path    - Optional path to save EDN backup during migration
     target_db_path - Optional path for target backend storage (Datahike/Datalevin)"
  [{:keys [source_backend target_backend dry_run export_path target_db_path]}]
  (log/info "kg_migrate" {:source source_backend :target target_backend :dry-run dry_run})
  (try
    (require 'hive-mcp.knowledge-graph.migration)
    (let [migrate-fn (resolve 'hive-mcp.knowledge-graph.migration/migrate-store!)
          source-kw (keyword source_backend)
          target-kw (keyword target_backend)
          target-opts (when target_db_path {:db-path target_db_path})
          result (migrate-fn source-kw target-kw
                             {:target-opts target-opts
                              :dry-run (boolean dry_run)
                              :export-path export_path})]
      (mcp-json {:success true
                 :source-backend (name source-kw)
                 :target-backend (name target-kw)
                 :dry-run (:dry-run result)
                 :exported (:exported result)
                 :imported (:imported result)
                 :validation (:validation result)
                 :errors (when (seq (:errors result))
                           (count (:errors result)))}))
    (catch Exception e
      (log/error e "kg_migrate failed")
      (mcp-error (str "Migration failed: " (.getMessage e))))))

(defn handle-kg-export
  "Export KG data to EDN file for backup or migration.

   Arguments:
     path - File path to save the EDN export"
  [{:keys [path]}]
  (log/info "kg_export" {:path path})
  (try
    (require 'hive-mcp.knowledge-graph.migration)
    (let [export-fn (resolve 'hive-mcp.knowledge-graph.migration/export-to-file!)
          result (export-fn path)]
      (mcp-json {:success true
                 :path path
                 :counts (:counts result)
                 :exported-at (str (:exported-at result))}))
    (catch Exception e
      (log/error e "kg_export failed")
      (mcp-error (str "Export failed: " (.getMessage e))))))

(defn handle-kg-import
  "Import KG data from EDN file.

   Arguments:
     path - File path to the EDN export file"
  [{:keys [path]}]
  (log/info "kg_import" {:path path})
  (try
    (require 'hive-mcp.knowledge-graph.migration)
    (let [import-fn (resolve 'hive-mcp.knowledge-graph.migration/import-from-file!)
          result (import-fn path)]
      (mcp-json {:success true
                 :path path
                 :imported (:imported result)
                 :errors (when (seq (:errors result))
                           {:count (count (:errors result))
                            :first-error (first (:errors result))})}))
    (catch Exception e
      (log/error e "kg_import failed")
      (mcp-error (str "Import failed: " (.getMessage e))))))

(defn handle-kg-validate-migration
  "Validate migration by comparing expected vs actual entity counts.

   Arguments:
     expected_edges     - Expected number of edges
     expected_disc      - Expected number of disc entities
     expected_synthetic - Expected number of synthetic nodes"
  [{:keys [expected_edges expected_disc expected_synthetic]}]
  (log/info "kg_validate_migration" {:edges expected_edges :disc expected_disc :synthetic expected_synthetic})
  (try
    (require 'hive-mcp.knowledge-graph.migration)
    (let [validate-fn (resolve 'hive-mcp.knowledge-graph.migration/validate-migration)
          expected {:edges (or expected_edges 0)
                    :disc (or expected_disc 0)
                    :synthetic (or expected_synthetic 0)}
          result (validate-fn expected)]
      (mcp-json {:success true
                 :valid (:valid? result)
                 :expected (:expected result)
                 :actual (:actual result)
                 :missing (:missing result)}))
    (catch Exception e
      (log/error e "kg_validate_migration failed")
      (mcp-error (str "Validation failed: " (.getMessage e))))))

;;; =============================================================================
;;; Migration Tool Definitions
;;; =============================================================================

(def migration-tools
  [{:name "kg_migrate"
    :description "Migrate Knowledge Graph data from one backend to another. Supports DataScript (in-memory), Datalevin (persistent), and Datahike (time-travel). Use dry_run=true to preview migration without executing."
    :inputSchema {:type "object"
                  :properties {"source_backend" {:type "string"
                                                 :enum ["datascript" "datalevin" "datahike"]
                                                 :description "Current backend to migrate FROM"}
                               "target_backend" {:type "string"
                                                 :enum ["datascript" "datalevin" "datahike"]
                                                 :description "Target backend to migrate TO"}
                               "dry_run" {:type "boolean"
                                          :description "Preview migration without executing (default: false)"}
                               "export_path" {:type "string"
                                              :description "Optional path to save EDN backup during migration"}
                               "target_db_path" {:type "string"
                                                 :description "Optional storage path for target backend"}}
                  :required ["source_backend" "target_backend"]}
    :handler handle-kg-migrate}

   {:name "kg_export"
    :description "Export all Knowledge Graph data (edges, disc entities, synthetic nodes) to an EDN file. Useful for backup or migration between environments."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "File path to save the EDN export"}}
                  :required ["path"]}
    :handler handle-kg-export}

   {:name "kg_import"
    :description "Import Knowledge Graph data from an EDN file. Use after kg_export to restore or migrate data."
    :inputSchema {:type "object"
                  :properties {"path" {:type "string"
                                       :description "File path to the EDN export file"}}
                  :required ["path"]}
    :handler handle-kg-import}

   {:name "kg_validate_migration"
    :description "Validate that a migration completed successfully by comparing expected vs actual entity counts. Use after kg_import to verify data integrity."
    :inputSchema {:type "object"
                  :properties {"expected_edges" {:type "integer"
                                                 :description "Expected number of edges"}
                               "expected_disc" {:type "integer"
                                                :description "Expected number of disc entities"}
                               "expected_synthetic" {:type "integer"
                                                     :description "Expected number of synthetic nodes"}}
                  :required []}
    :handler handle-kg-validate-migration}])

;;; =============================================================================
;;; Combined Tool Export
;;; =============================================================================

(def all-tools
  "All KG tools including versioning and migration tools."
  (-> tools
      (into versioning-tools)
      (into migration-tools)))
