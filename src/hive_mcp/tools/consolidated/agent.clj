(ns hive-mcp.tools.consolidated.agent
  "Consolidated Agent CLI tool.

   Subcommands: spawn, status, kill, dispatch, claims, list, collect, broadcast

   Usage via MCP: agent {\"command\": \"spawn\", \"type\": \"drone\", \"cwd\": \"/project\"}

   SOLID: Facade pattern - single tool entry point for agent lifecycle.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.tools.swarm.collect :as swarm-collect]
            [hive-mcp.tools.swarm.status :as swarm-status]
            [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-mcp.emacsclient :as ec]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Lazy require to avoid circular dependency
;; =============================================================================

(defn- get-delegate-fn
  "Lazily resolve hive-mcp.agent/delegate! to avoid circular dep."
  []
  (require 'hive-mcp.agent)
  (resolve 'hive-mcp.agent/delegate!))

;; =============================================================================
;; Elisp Fallback for Lings (FIX: swarm_status only returning coordinator)
;; =============================================================================

(defn- query-elisp-lings
  "Query elisp for list of lings that may not be in DataScript.
   Lings spawned directly via elisp won't be in DataScript.

   Returns a seq of slave maps in DataScript format, or empty seq on failure."
  []
  (when (swarm-core/swarm-addon-available?)
    (let [{:keys [success result timed-out]}
          (ec/eval-elisp-with-timeout
           "(json-encode (hive-mcp-swarm-list-lings))" 3000)]
      (when (and success (not timed-out))
        (try
          (let [parsed (json/read-str result :key-fn keyword)]
            (when (sequential? parsed)
              ;; Convert elisp format to DataScript format
              (->> parsed
                   (map (fn [ling]
                          {:slave/id (or (:slave-id ling) (:slave_id ling))
                           :slave/name (:name ling)
                           :slave/status (keyword (or (:status ling) "idle"))
                           :slave/depth 1  ;; lings are depth 1
                           :slave/cwd (:cwd ling)
                           :slave/project-id (:project-id ling)
                           :slave/presets (:presets ling)}))
                   (filter :slave/id))))  ;; filter out invalid entries
          (catch Exception e
            (log/debug "Failed to parse elisp lings:" (ex-message e))
            []))))))

(defn- merge-with-elisp-lings
  "Merge DataScript agents with elisp lings.
   DataScript entries take precedence for duplicates (by ID).

   CLARITY: Y - Yield safe failure - returns at least DataScript data on error."
  [ds-agents]
  (try
    (let [;; Get elisp lings
          elisp-lings (or (query-elisp-lings) [])
          ;; Create set of IDs already in DataScript
          ds-ids (set (map :slave/id ds-agents))
          ;; Filter elisp lings to only those NOT in DataScript
          new-lings (remove #(ds-ids (:slave/id %)) elisp-lings)]
      (log/debug "Merging agents: DataScript=" (count ds-agents)
                 "elisp-only=" (count new-lings))
      (concat ds-agents new-lings))
    (catch Exception e
      (log/warn "Failed to merge elisp lings (returning DataScript only):" (ex-message e))
      ds-agents)))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn- generate-agent-id
  "Generate unique agent ID with type prefix."
  [agent-type]
  (str (name agent-type) "-" (java.util.UUID/randomUUID)))

(defn- format-agent
  "Format agent data for response."
  [agent-data]
  (when agent-data
    (let [base {:id (:slave/id agent-data)
                :status (:slave/status agent-data)
                :type (case (:slave/depth agent-data)
                        0 :coordinator
                        1 :ling
                        :drone)  ;; depth 2+ = drone
                :cwd (:slave/cwd agent-data)
                :project-id (:slave/project-id agent-data)}]
      (cond-> base
        (:slave/parent agent-data) (assoc :parent (:slave/parent agent-data))
        (:slave/presets agent-data) (assoc :presets (:slave/presets agent-data))
        (:slave/created-at agent-data) (assoc :created-at (:slave/created-at agent-data))))))

(defn- format-agents
  "Format a list of agents for response."
  [agents]
  (let [formatted (->> agents
                       (map format-agent)
                       (remove nil?)
                       vec)]
    {:agents formatted
     :count (count formatted)
     :by-type (frequencies (map :type formatted))
     :by-status (frequencies (map :status formatted))}))

;; =============================================================================
;; Spawn Handler
;; =============================================================================

(defn handle-spawn
  "Spawn a new agent (ling or drone).

   Parameters:
     type    - Agent type: 'ling' or 'drone' (required)
     name    - Optional agent name (auto-generated if not provided)
     cwd     - Working directory (required)
     presets - Preset names for ling (optional, ling only)
     model   - Model override for drone (optional, drone only)
     task    - Initial task to dispatch (optional)
     files   - Files for drone to work on (optional, drone only)
     parent  - Parent agent ID (optional)

   CLARITY: I - Validates type before dispatch."
  [{:keys [type name cwd presets model task files parent project_id]}]
  (let [agent-type (keyword type)]
    (if-not (#{:ling :drone} agent-type)
      (mcp-error "type must be 'ling' or 'drone'")
      (try
        (let [agent-id (or name (generate-agent-id agent-type))
              effective-project-id (or project_id
                                       (when cwd
                                         (last (clojure.string/split cwd #"/"))))]
          (case agent-type
            :ling
            ;; FIX: Ensure presets is always a vector, not a string
            ;; The shim may pass "explorer" instead of ["explorer"]
            (let [presets-vec (cond
                                (nil? presets) []
                                (string? presets) [presets]
                                (sequential? presets) (vec presets)
                                :else [presets])
                  ling-agent (ling/->ling agent-id {:cwd cwd
                                                    :presets presets-vec
                                                    :project-id effective-project-id})
                  ;; spawn! returns the actual elisp slave-id (may differ from agent-id)
                  elisp-slave-id (proto/spawn! ling-agent {:task task
                                                           :parent parent})]
              (log/info "Spawned ling" {:requested-id agent-id
                                        :elisp-slave-id elisp-slave-id
                                        :cwd cwd :presets presets-vec})
              (mcp-json {:success true
                         :agent-id elisp-slave-id
                         :type :ling
                         :cwd cwd
                         :presets presets-vec}))

            :drone
            (let [drone-agent (drone/->drone agent-id {:cwd cwd
                                                       :model model
                                                       :parent-id parent
                                                       :project-id effective-project-id})]
              (proto/spawn! drone-agent {:files files})
              (log/info "Spawned drone" {:id agent-id :cwd cwd :model model})
              (mcp-json {:success true
                         :agent-id agent-id
                         :type :drone
                         :cwd cwd
                         :files files}))))
        (catch Exception e
          (log/error "Failed to spawn agent" {:type agent-type :error (ex-message e)})
          (mcp-error (str "Failed to spawn " (name agent-type) ": " (ex-message e))))))))

;; =============================================================================
;; Status Handler
;; =============================================================================

(defn handle-status
  "Get agent status.

   Parameters:
     agent_id   - Specific agent ID to query (optional)
     type       - Filter by type: 'ling' or 'drone' (optional)
     project_id - Filter by project (optional)

   Returns all agents if no filters provided.

   FIX: Now merges elisp lings with DataScript agents.
   Lings spawned directly via elisp may not be in DataScript, so we
   query both sources and merge (DataScript takes precedence for duplicates).

   FIX: bb-mcp injects agent_id:'coordinator' for piggyback tracking.
   Ignore agent_id when it equals 'coordinator' to avoid false filtering.

   CLARITY: R - Returns formatted agent info with type inference."
  [{:keys [agent_id type project_id]}]
  ;; FIX: bb-mcp injects agent_id:"coordinator" on all calls - ignore it as a filter
  (let [eid (when (and agent_id (not= agent_id "coordinator")) agent_id)]
    (try
      (cond
        eid
        (if-let [agent-data (queries/get-slave eid)]
          (mcp-json {:agent (format-agent agent-data)})
          (mcp-error (str "Agent not found: " eid)))
        type
        (let [agent-type (keyword type)
              depth (case agent-type :ling 1 :drone 2 nil)
              all-agents (if project_id
                           (queries/get-slaves-by-project project_id)
                           (if (= agent-type :ling)
                             (merge-with-elisp-lings (queries/get-all-slaves))
                             (queries/get-all-slaves)))
              filtered (if depth
                         (filter #(= depth (:slave/depth %)) all-agents)
                         all-agents)]
          (mcp-json (format-agents filtered)))
        project_id
        (mcp-json (format-agents (queries/get-slaves-by-project project_id)))
        :else
        (mcp-json (format-agents (merge-with-elisp-lings (queries/get-all-slaves)))))
      (catch Exception e
        (log/error "Failed to get agent status" {:error (ex-message e)})
        (mcp-error (str "Failed to get status: " (ex-message e)))))))

;; =============================================================================
;; Kill Handler
;; =============================================================================

(defn handle-kill
  "Terminate an agent.

   Parameters:
     agent_id - Agent ID to kill (required)
     force    - Force kill even if critical ops in progress (default: false)

   CLARITY: Y - Safe failure, checks critical ops before killing."
  [{:keys [agent_id force]}]
  (if (empty? agent_id)
    (mcp-error "agent_id is required")
    (try
      (if-let [agent-data (queries/get-slave agent_id)]
        (let [agent-type (if (= 1 (:slave/depth agent-data)) :ling :drone)
              agent (case agent-type
                      :ling (ling/->ling agent_id {:cwd (:slave/cwd agent-data)
                                                   :presets (:slave/presets agent-data)
                                                   :project-id (:slave/project-id agent-data)})
                      :drone (drone/->drone agent_id {:cwd (:slave/cwd agent-data)
                                                      :parent-id (:slave/parent agent-data)
                                                      :project-id (:slave/project-id agent-data)}))
              result (proto/kill! agent)]
          (log/info "Kill agent result" {:agent_id agent_id :result result})
          (mcp-json result))
        (mcp-error (str "Agent not found: " agent_id)))
      (catch Exception e
        (log/error "Failed to kill agent" {:agent_id agent_id :error (ex-message e)})
        (mcp-error (str "Failed to kill agent: " (ex-message e)))))))

;; =============================================================================
;; Dispatch Handler
;; =============================================================================

(defn handle-dispatch
  "Dispatch a task to an agent.

   Parameters:
     agent_id - Target agent ID (required)
     prompt   - Task prompt/description (required)
     files    - Files to include (optional)
     priority - Task priority: normal, high, low (optional)

   CLARITY: I - Validates agent exists before dispatch."
  [{:keys [agent_id prompt files priority]}]
  (cond
    (empty? agent_id)
    (mcp-error "agent_id is required")

    (empty? prompt)
    (mcp-error "prompt is required")

    :else
    (try
      (if-let [agent-data (queries/get-slave agent_id)]
        (let [agent-type (if (= 1 (:slave/depth agent-data)) :ling :drone)
              agent (case agent-type
                      :ling (ling/->ling agent_id {:cwd (:slave/cwd agent-data)
                                                   :presets (:slave/presets agent-data)
                                                   :project-id (:slave/project-id agent-data)})
                      :drone (drone/->drone agent_id {:cwd (:slave/cwd agent-data)
                                                      :parent-id (:slave/parent agent-data)
                                                      :project-id (:slave/project-id agent-data)}))
              ;; Drones need delegate-fn for execution; lings use elisp dispatch
              task-opts (cond-> {:task prompt
                                 :files files
                                 :priority (keyword (or priority "normal"))}
                          (= agent-type :drone)
                          (assoc :delegate-fn (get-delegate-fn)))
              task-id (proto/dispatch! agent task-opts)]
          (log/info "Dispatched task to agent" {:agent_id agent_id :task-id task-id})
          (mcp-json {:success true
                     :agent-id agent_id
                     :task-id task-id
                     :files files}))
        (mcp-error (str "Agent not found: " agent_id)))
      (catch Exception e
        (log/error "Failed to dispatch to agent" {:agent_id agent_id :error (ex-message e)})
        (mcp-error (str "Failed to dispatch: " (ex-message e)))))))

;; =============================================================================
;; Claims Handler
;; =============================================================================

(defn handle-claims
  "Get file claims for an agent.

   Parameters:
     agent_id - Agent ID to query claims for (optional)

   Returns all claims if agent_id not provided.

   CLARITY: R - Shows both DataScript and logic claims for visibility."
  [{:keys [agent_id]}]
  (try
    (if agent_id
      ;; Get claims for specific agent
      (let [logic-claims (logic/get-all-claims)
            agent-claims (->> logic-claims
                              (filter #(= agent_id (:slave-id %)))
                              (mapv (fn [{:keys [file slave-id]}]
                                      {:file file :owner slave-id})))]
        (mcp-json {:agent-id agent_id
                   :claims agent-claims
                   :count (count agent-claims)}))
      ;; Get all claims with owner info
      (let [all-claims (logic/get-all-claims)
            formatted (->> all-claims
                           (mapv (fn [{:keys [file slave-id]}]
                                   {:file file :owner slave-id})))]
        (mcp-json {:claims formatted
                   :count (count formatted)
                   :by-owner (frequencies (map :owner formatted))})))
    (catch Exception e
      (log/error "Failed to get claims" {:error (ex-message e)})
      (mcp-error (str "Failed to get claims: " (ex-message e))))))

;; =============================================================================
;; List Handler
;; =============================================================================

(defn handle-list
  "List all agents (shorthand for status with no filters).

   Parameters:
     type       - Optional filter: 'ling' or 'drone'
     project_id - Optional filter by project

   CLARITY: R - Convenience wrapper around status."
  [params]
  (handle-status params))

;; =============================================================================
;; Collect Handler (delegates to swarm for backward compat)
;; =============================================================================

(defn handle-collect
  "Collect response from a dispatched task.

   Parameters:
     task_id    - ID of the task to collect results from (required)
     timeout_ms - How long to wait for completion (default: 300000 = 5min)

   CLARITY: L - Thin adapter delegating to swarm collect handler.
   DEPRECATED: Direct swarm_collect usage is preferred."
  [{:keys [task_id timeout_ms] :as params}]
  (if (empty? task_id)
    (mcp-error "task_id is required")
    (swarm-collect/handle-swarm-collect params)))

;; =============================================================================
;; Broadcast Handler (delegates to swarm for backward compat)
;; =============================================================================

(defn handle-broadcast
  "Broadcast a prompt to all active lings.

   Parameters:
     prompt - The prompt to broadcast to all lings (required)

   CLARITY: L - Thin adapter delegating to swarm broadcast handler.
   DEPRECATED: Direct swarm_broadcast usage is preferred."
  [{:keys [prompt] :as params}]
  (if (empty? prompt)
    (mcp-error "prompt is required")
    (swarm-status/handle-swarm-broadcast params)))

;; =============================================================================
;; Handlers Map
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:spawn     handle-spawn
   :status    handle-status
   :kill      handle-kill
   :dispatch  handle-dispatch
   :claims    handle-claims
   :list      handle-list
   :collect   handle-collect
   :broadcast handle-broadcast})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-agent
  "Unified CLI handler for agent lifecycle."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated agent command."
  {:name "agent"
   :description "Unified agent operations: spawn (create ling/drone), status (query agents), kill (terminate), dispatch (send task), claims (file ownership), list (all agents), collect (get task result), broadcast (prompt all). Type: 'ling' (Claude Code instance) or 'drone' (OpenRouter leaf worker). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["spawn" "status" "kill" "dispatch" "claims" "list" "collect" "broadcast" "help"]
                                         :description "Agent operation to perform"}
                              ;; spawn params
                              "type" {:type "string"
                                      :enum ["ling" "drone"]
                                      :description "Agent type to spawn (required for spawn)"}
                              "name" {:type "string"
                                      :description "Agent name/ID (auto-generated if not provided)"}
                              "cwd" {:type "string"
                                     :description "Working directory (required for spawn)"}
                              "presets" {:type "array"
                                         :items {:type "string"}
                                         :description "Preset names for ling (ling only)"}
                              "model" {:type "string"
                                       :description "Model override for drone (drone only)"}
                              "task" {:type "string"
                                      :description "Initial task to dispatch on spawn"}
                              ;; common params
                              "agent_id" {:type "string"
                                          :description "Agent ID for status/kill/dispatch/claims"}
                              "project_id" {:type "string"
                                            :description "Project ID filter for status/list"}
                              ;; dispatch params
                              "prompt" {:type "string"
                                        :description "Task prompt for dispatch or broadcast"}
                              "files" {:type "array"
                                       :items {:type "string"}
                                       :description "Files for drone or dispatch"}
                              "priority" {:type "string"
                                          :enum ["low" "normal" "high"]
                                          :description "Task priority for dispatch"}
                              "parent" {:type "string"
                                        :description "Parent agent ID for spawn"}
                              ;; kill params
                              "force" {:type "boolean"
                                       :description "Force kill even if critical ops in progress"}
                              ;; collect params
                              "task_id" {:type "string"
                                         :description "Task ID for collect operation"}
                              "timeout_ms" {:type "integer"
                                            :description "Timeout in ms for collect (default: 300000)"}}
                 :required ["command"]}
   :handler handle-agent})

(def tools
  "Tool definitions for registration."
  [tool-def])

(comment)
  ;; Usage examples

  ;; Spawn a ling
  ;; (handle-spawn {:type "ling" :cwd "/project" :presets ["coordinator"]})

  ;; Spawn a drone
  ;; (handle-spawn {:type "drone" :cwd "/project" :files ["src/core.clj"]})

  ;; Get status of all agents
  ;; (handle-status {})

  ;; Get status of specific agent
  ;; (handle-status {:agent_id "ling-123"})

  ;; Kill an agent
  ;; (handle-kill {:agent_id "drone-456"})

  ;; Dispatch a task
  ;; (handle-dispatch {:agent_id "ling-123" :prompt "Fix the bug" :files ["src/bug.clj"]})

  ;; Get claims for an agent
  ;; (handle-claims {:agent_id "drone-456"})

  ;; List all agents
  ;; (handle-list {})

