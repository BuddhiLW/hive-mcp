(ns hive-mcp.tools.consolidated.agent
  "Consolidated Agent CLI tool.

   Subcommands: spawn, status, kill, kill-batch, dispatch, claims, collect, broadcast, cleanup, dag
   Deprecated aliases: list → status

   Usage via MCP: agent {\"command\": \"spawn\", \"type\": \"drone\", \"cwd\": \"/project\"}

   SOLID: Facade pattern - single tool entry point for agent lifecycle.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler make-batch-handler]]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.registry :as registry]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.tools.swarm.collect :as swarm-collect]
            [hive-mcp.tools.swarm.status :as swarm-status]
            [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [hive-mcp.emacsclient :as ec]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]
            [clojure.string :as str]))
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

(defn- resolve-project-scope
  "Resolve effective project-id for a spawned agent.
   HCR Wave 4: Inherits project scope from parent agent's hierarchy.

   Resolution order:
   1. Explicit project_id parameter
   2. Inferred from cwd via kg-scope/infer-scope-from-path
   3. Parent agent's project-id (if parent specified)
   4. Fallback: last segment of cwd

   Also registers the resolved scope in kg-scope for hierarchy lookups."
  [project_id cwd parent]
  (or project_id
      ;; Try to infer from cwd using hierarchy-aware scope resolution
      (when cwd
        (let [inferred (kg-scope/infer-scope-from-path cwd)]
          (when (and inferred (not= inferred "global"))
            inferred)))
      ;; Inherit from parent agent's project scope
      (when parent
        (when-let [parent-data (queries/get-slave parent)]
          (:slave/project-id parent-data)))
      ;; Fallback to last path segment
      (when cwd
        (last (str/split cwd #"/")))))

(defn handle-spawn
  "Spawn a new agent (ling or drone).

   Parameters:
     type       - Agent type: 'ling' or 'drone' (required)
     name       - Optional agent name (auto-generated if not provided)
     cwd        - Working directory (required)
     presets    - Preset names for ling (optional, ling only)
     model      - Model override. For drones: OpenRouter model ID.
                  For lings: 'claude' (default, uses Claude Code CLI) or
                  OpenRouter model ID (auto-forces headless spawn mode).
     task       - Initial task to dispatch (optional)
     files      - Files for drone to work on (optional, drone only)
     parent     - Parent agent ID (optional)
     spawn_mode - Spawn mode for lings: 'vterm' (default) or 'headless' (optional)
                  'headless' maps to :agent-sdk (Claude Agent SDK, default since 0.12.0).
                  Non-claude models automatically force headless/openrouter mode.
     agents     - Subagent definitions map (optional, agent-sdk/headless mode only)
                  Map of agent-name -> {description, prompt, tools?, model?}
                  Passed to ClaudeAgentOptions.agents for custom subagent definitions.

   HCR Wave 4: Inherits project scope from parent hierarchy.
   CLARITY: I - Validates type before dispatch."
  [{:keys [type name cwd presets model task files parent project_id kanban_task_id spawn_mode agents]}]
  (let [agent-type (keyword type)]
    (if-not (#{:ling :drone} agent-type)
      (mcp-error "type must be 'ling' or 'drone'")
      (try
        (let [agent-id (or name (generate-agent-id agent-type))
              ;; HCR Wave 4: Use hierarchy-aware project scope resolution
              effective-project-id (resolve-project-scope project_id cwd parent)]
          (case agent-type
            :ling
            ;; FIX: Ensure presets is always a vector, not a string
            ;; The shim may pass "explorer" instead of ["explorer"]
            (let [presets-vec (cond
                                (nil? presets) []
                                (string? presets) [presets]
                                (sequential? presets) (vec presets)
                                :else [presets])
                  ;; Resolve spawn mode: 'headless' maps to :agent-sdk (default since 0.12.0)
                  ;; Non-claude models automatically force headless via ->ling
                  effective-spawn-mode (keyword (or spawn_mode "vterm"))
                  _ (when-not (#{:vterm :headless :agent-sdk} effective-spawn-mode)
                      (throw (ex-info "spawn_mode must be 'vterm', 'headless', or 'agent-sdk'"
                                      {:spawn-mode spawn_mode})))
                  ;; Normalize agents map: convert string keys to keyword keys for agent specs
                  ;; MCP JSON sends {"name": {"description": "...", "prompt": "...", "tools": [...]}}
                  ;; Clojure side expects {"name" {:description "..." :prompt "..." :tools [...]}}
                  normalized-agents (when (map? agents)
                                      (reduce-kv
                                       (fn [m agent-name agent-spec]
                                         (assoc m (clojure.core/name agent-name)
                                                (if (map? agent-spec)
                                                  (reduce-kv (fn [m2 k v]
                                                               (assoc m2 (keyword k) v))
                                                             {} agent-spec)
                                                  agent-spec)))
                                       {} agents))
                  ;; ->ling handles auto-forcing headless for non-claude models
                  ling-agent (ling/->ling agent-id (cond-> {:cwd cwd
                                                            :presets presets-vec
                                                            :project-id effective-project-id
                                                            :spawn-mode effective-spawn-mode
                                                            :model model}
                                                     normalized-agents (assoc :agents normalized-agents)))
                  ;; spawn! returns the actual slave-id
                  ;; For vterm: may differ from agent-id (elisp assigns)
                  ;; For headless: same as agent-id (we control it)
                  slave-id (proto/spawn! ling-agent {:task task
                                                     :parent parent
                                                     :kanban-task-id kanban_task_id
                                                     :spawn-mode (:spawn-mode ling-agent)
                                                     :model model})]
              (log/info "Spawned ling" {:requested-id agent-id
                                        :slave-id slave-id
                                        :spawn-mode (:spawn-mode ling-agent)
                                        :model (or model "claude")
                                        :cwd cwd :presets presets-vec
                                        :project-id effective-project-id})
              (mcp-json {:success true
                         :agent-id slave-id
                         :type :ling
                         :spawn-mode (:spawn-mode ling-agent)
                         :model (or model "claude")
                         :cwd cwd
                         :presets presets-vec
                         :project-id effective-project-id}))

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
;; Kill Handler (+ kill-batch)
;; =============================================================================

(defn- kill-one!
  "Core kill logic for a single agent. Returns plain data:
   - {:killed id :result proto-result} on success
   - {:error \"reason\" :id id} on failure

   Used by both handle-kill and handle-kill-batch to avoid duplicating
   ownership checks, agent construction, and proto/kill! dispatch.

   Ownership rules (HIL):
   - Caller without explicit directory (coordinator): can kill anything
   - Legacy lings without project-id: can be killed by anyone
   - Same project: kill proceeds normally
   - Different project: requires force_cross_project=true"
  [agent-id {:keys [directory force_cross_project]}]
  (try
    (if-let [agent-data (queries/get-slave agent-id)]
      (let [;; CRITICAL: Only use EXPLICIT directory param, not ctx fallback.
            caller-project-id (when directory
                                (scope/get-current-project-id directory))
            target-project-id (:slave/project-id agent-data)
            can-kill? (or force_cross_project
                          (nil? caller-project-id)
                          (nil? target-project-id)
                          (= caller-project-id target-project-id))]
        (if-not can-kill?
          {:error (format "Agent '%s' belongs to project '%s', not '%s'. Pass force_cross_project=true to kill cross-project."
                          agent-id target-project-id caller-project-id)
           :id agent-id}
          (let [agent-type (if (= 1 (:slave/depth agent-data)) :ling :drone)
                agent (case agent-type
                        :ling (ling/->ling agent-id {:cwd (:slave/cwd agent-data)
                                                     :presets (:slave/presets agent-data)
                                                     :project-id (:slave/project-id agent-data)
                                                     :spawn-mode (or (:ling/spawn-mode agent-data) :vterm)})
                        :drone (drone/->drone agent-id {:cwd (:slave/cwd agent-data)
                                                        :parent-id (:slave/parent agent-data)
                                                        :project-id (:slave/project-id agent-data)}))
                result (proto/kill! agent)]
            (log/info "Kill agent result" {:agent_id agent-id
                                           :result result
                                           :caller-project caller-project-id
                                           :target-project target-project-id})
            {:killed agent-id :result result})))
      {:error (str "Agent not found: " agent-id) :id agent-id})
    (catch Exception e
      (log/error "Failed to kill agent" {:agent_id agent-id :error (ex-message e)})
      {:error (str "Failed to kill agent: " (ex-message e)) :id agent-id})))

(defn handle-kill
  "Terminate an agent.

   Parameters:
     agent_id            - Agent ID to kill (required)
     directory           - Caller's working directory for ownership check (optional)
     force               - Force kill even if critical ops in progress (default: false)
     force_cross_project - Allow killing agents from different projects (default: false)

   HUMAN-IN-THE-LOOP (HIL): Cross-project kill prevention.
   If target agent's project differs from caller's project, kill is denied
   unless force_cross_project=true is explicitly passed.

   CLARITY: Y - Safe failure, checks ownership + critical ops before killing.
   CLARITY: I - Inputs guarded with HIL for cross-project safety.

   BUG FIX (2026-02): Only use EXPLICIT directory param for ownership check."
  [{:keys [agent_id] :as params}]
  (if (empty? agent_id)
    (mcp-error "agent_id is required")
    (let [result (kill-one! agent_id params)]
      (if (:error result)
        (mcp-error (:error result))
        (mcp-json (:result result))))))

(defn handle-kill-batch
  "Terminate multiple agents in a single call.

   Parameters:
     agent_ids           - Array of agent IDs to kill (required)
     force               - Force kill even if critical ops in progress (default: false)
     force_cross_project - Allow killing agents from different projects (default: false)
     directory           - Caller's working directory for ownership check (optional)

   Returns: {killed: [...], failed: [{id: ..., error: ...}], summary: {...}}

   Reuses kill-one! core logic — same ownership checks apply per agent.

   CLARITY: I - Validates agent_ids array before processing.
   CLARITY: R - Returns detailed per-agent results for transparency."
  [{:keys [agent_ids] :as params}]
  (if (or (nil? agent_ids) (empty? agent_ids))
    (mcp-error "agent_ids is required (array of agent ID strings)")
    (let [results (mapv #(kill-one! % params) agent_ids)
          killed  (filterv :killed results)
          failed  (filterv :error results)]
      (log/info "Kill-batch completed" {:total (count agent_ids)
                                        :killed (count killed)
                                        :failed (count failed)})
      (mcp-json {:killed  (mapv :killed killed)
                 :failed  (mapv #(select-keys % [:id :error]) failed)
                 :summary {:total  (count agent_ids)
                           :killed (count killed)
                           :failed (count failed)}}))))

;; =============================================================================
;; Dispatch Handler
;; =============================================================================

(defn- build-dispatch-context
  "Build an IDispatchContext from dispatch parameters.

   When ctx_refs are provided (pass-by-reference mode), creates a RefContext
   that carries lightweight context-store IDs + KG node IDs instead of full
   text blobs (~25x compression). Otherwise wraps prompt as TextContext.

   Arguments:
     prompt      - Base task prompt string (always present as fallback)
     ctx_refs    - Map of category->ctx-id for context-store lookups (optional)
     kg_node_ids - Vector of KG node IDs for graph traversal (optional)
     scope       - Project scope string for KG traversal (optional)

   Returns:
     IDispatchContext instance (RefContext or TextContext).

   SOLID-O: Open for extension - new context types via new factory fns.
   CLARITY-Y: Falls back to TextContext when no refs provided."
  [prompt ctx_refs kg_node_ids scope]
  (if (seq ctx_refs)
    ;; Pass-by-reference mode: create RefContext with context-store IDs
    (let [refs-map (reduce-kv (fn [m k v] (assoc m (keyword k) v)) {} ctx_refs)]
      (dispatch-ctx/->ref-context prompt
                                  {:ctx-refs    refs-map
                                   :kg-node-ids (vec (or kg_node_ids []))
                                   :scope       scope}))
    ;; Plain text mode: wrap string as TextContext (backward compatible)
    (dispatch-ctx/ensure-context prompt)))

(defn handle-dispatch
  "Dispatch a task to an agent.

   Parameters:
     agent_id    - Target agent ID (required)
     prompt      - Task prompt/description, or IDispatchContext (required)
     files       - Files to include (optional)
     priority    - Task priority: normal, high, low (optional)
     ctx_refs    - Map of category->ctx-id for KG-compressed context (optional)
                   e.g. {\"axioms\": \"ctx-123\", \"decisions\": \"ctx-456\"}
                   When provided, creates RefContext (~25x compression vs text)
     kg_node_ids - Vector of KG node IDs for graph traversal seeds (optional)
     scope       - Project scope for KG traversal (optional, auto-derived)

   Accepts plain string prompts (backward compat) or IDispatchContext instances.
   When ctx_refs is provided, creates RefContext for pass-by-reference dispatch.

   SOLID-D: Depends on IDispatchContext abstraction, not string concretion.
   SOLID-O: Open for RefContext extension without modifying TextContext path.
   CLARITY: I - Validates agent exists before dispatch."
  [{:keys [agent_id prompt files priority ctx_refs kg_node_ids scope]}]
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
                                                   :project-id (:slave/project-id agent-data)
                                                   :spawn-mode (or (:ling/spawn-mode agent-data) :vterm)})
                      :drone (drone/->drone agent_id {:cwd (:slave/cwd agent-data)
                                                      :parent-id (:slave/parent agent-data)
                                                      :project-id (:slave/project-id agent-data)}))
              ;; IDispatchContext: build RefContext when ctx_refs provided,
              ;; TextContext for plain strings (backward compatible)
              ctx (build-dispatch-context prompt ctx_refs kg_node_ids
                                          (or scope (:slave/project-id agent-data)))
              resolved-prompt (:prompt (dispatch-ctx/resolve-context ctx))
              ;; Drones need delegate-fn for execution; lings use elisp or stdin dispatch
              task-opts (cond-> {:task resolved-prompt
                                 :dispatch-context ctx
                                 :files files
                                 :priority (keyword (or priority "normal"))}
                          (= agent-type :drone)
                          (assoc :delegate-fn (get-delegate-fn)))
              task-id (proto/dispatch! agent task-opts)]
          (log/info "Dispatched task to agent" {:agent_id agent_id
                                                :task-id task-id
                                                :context-type (dispatch-ctx/context-type ctx)})
          (mcp-json {:success true
                     :agent-id agent_id
                     :task-id task-id
                     :context-type (name (dispatch-ctx/context-type ctx))
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
;; Collect Handler (delegates to swarm for backward compat)
;; =============================================================================

(defn handle-collect
  "Collect response from a dispatched task.

   Parameters:
     task_id    - ID of the task to collect results from (required)
     timeout_ms - How long to wait for completion (default: 300000 = 5min)

   CLARITY: L - Thin adapter delegating to swarm collect handler.
   DEPRECATED: Direct swarm_collect usage is preferred."
  [{:keys [task_id] :as params}]
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
;; Cleanup Handler (Emacs/DataScript Reconciliation)
;; =============================================================================

(defn handle-cleanup
  "Reconcile DataScript registry with actual Emacs state.

   Problem: When Emacs restarts, hive-mcp-swarm--slaves hash table is lost,
   but DataScript persists agent records. This leaves orphan agents in DataScript
   with no corresponding Emacs buffer.

   Solution: Query both DataScript and Emacs, remove orphans from DataScript.

   Algorithm:
   1. Get all agents from DataScript
   2. Query Emacs for live ling buffers
   3. For each DataScript ling (depth=1), check if Emacs has it
   4. Remove orphans (DataScript entries without Emacs buffer)

   Note: Drones (depth=2+) are JVM-side only, not affected by Emacs restart.

   Parameters: None required

   CLARITY: Y - Safe reconciliation, only removes clear orphans."
  [_params]
  (try
    (let [;; Get all slaves from DataScript
          ds-agents (queries/get-all-slaves)
          ;; Get live lings from Emacs
          elisp-lings (or (query-elisp-lings) [])
          elisp-ids (set (map :slave/id elisp-lings))
          ;; Find orphan lings (in DataScript but not in Emacs)
          ;; Only check lings (depth=1), drones are JVM-side
          orphan-lings (->> ds-agents
                            (filter #(= 1 (:slave/depth %)))
                            (filter #(not (elisp-ids (:slave/id %))))
                            (map :slave/id))
          ;; Remove orphans from DataScript
          removed (doall
                   (for [slave-id orphan-lings]
                     (do
                       (log/info "Removing orphan ling from DataScript" {:slave-id slave-id})
                       (registry/remove-slave! slave-id)
                       slave-id)))]
      (log/info "Cleanup completed" {:orphans-removed (count removed)
                                     :ds-total (count ds-agents)
                                     :elisp-lings (count elisp-lings)})
      (mcp-json {:success true
                 :orphans-removed (count removed)
                 :removed-ids (vec removed)
                 :ds-agents-before (count ds-agents)
                 :elisp-lings-found (count elisp-lings)}))
    (catch Exception e
      (log/error "Cleanup failed" {:error (ex-message e)})
      (mcp-error (str "Cleanup failed: " (ex-message e))))))

;; =============================================================================
;; DAG Scheduler Handlers (n-depth: "dag start", "dag stop", "dag status")
;; =============================================================================

(defn handle-dag-start
  "Start the DAG scheduler for a plan.

   Thin wrapper around hive-mcp.scheduler.dag-waves/start-dag!.
   Uses requiring-resolve to keep startup lazy (scheduler is optional subsystem).

   Parameters:
     plan_id    - Memory entry ID of the plan (required)
     cwd        - Working directory (required)
     max_slots  - Max concurrent lings (default: 5)
     presets    - Ling presets (default: [\"ling\"])
     project_id - Project ID (auto-detected from cwd if nil)

   CLARITY: I - Validates plan_id and cwd before delegating."
  [{:keys [plan_id cwd max_slots presets project_id]}]
  (cond
    (str/blank? plan_id)
    (mcp-error "plan_id is required for dag start")

    (str/blank? cwd)
    (mcp-error "cwd is required for dag start")

    :else
    (try
      (let [start! (requiring-resolve 'hive-mcp.scheduler.dag-waves/start-dag!)
            opts (cond-> {:cwd cwd}
                   max_slots  (assoc :max-slots (if (string? max_slots)
                                                  (parse-long max_slots)
                                                  max_slots))
                   presets    (assoc :presets (if (string? presets)
                                                [presets]
                                                (vec presets)))
                   project_id (assoc :project-id project_id))
            result (start! plan_id opts)]
        (mcp-json result))
      (catch Exception e
        (mcp-error (str "Failed to start DAG: " (ex-message e)))))))

(defn handle-dag-stop
  "Stop the DAG scheduler.

   Thin wrapper around hive-mcp.scheduler.dag-waves/stop-dag!.

   CLARITY: Y - Safe to call even when no DAG is active."
  [_params]
  (try
    (let [stop! (requiring-resolve 'hive-mcp.scheduler.dag-waves/stop-dag!)
          result (stop!)]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Failed to stop DAG: " (ex-message e))))))

(defn handle-dag-status
  "Get current DAG scheduler status.

   Thin wrapper around hive-mcp.scheduler.dag-waves/dag-status.

   Returns: active, plan-id, completed/failed/dispatched counts, wave-log.

   CLARITY: R - Read-only status query."
  [_params]
  (try
    (let [status-fn (requiring-resolve 'hive-mcp.scheduler.dag-waves/dag-status)
          result (status-fn)]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Failed to get DAG status: " (ex-message e))))))

;; =============================================================================
;; Deprecated Alias Support
;; =============================================================================

(def ^:private deprecated-aliases
  "Map of deprecated command keywords to their canonical replacements."
  {:list :status})

(defn- wrap-deprecated
  "Wrap a handler fn to emit a deprecation warning before delegating."
  [alias-kw canonical-kw handler-fn]
  (fn [params]
    (log/warn (str "DEPRECATED: command '" (name alias-kw)
                   "' is deprecated, use '" (name canonical-kw) "' instead."))
    (handler-fn params)))

;; =============================================================================
;; Batch-Spawn Handler (via make-batch-handler HOF)
;; =============================================================================

(def ^:private batch-spawn-handler
  "Batch spawn multiple agents in one call.
   Uses make-batch-handler HOF from cli.clj.

   Each operation in :operations is a spawn call.
   :command 'spawn' is auto-injected into each operation.

   Parameters:
     operations - Array of spawn parameter objects:
                  [{:type 'ling', :name 'a', :cwd '/path', :presets ['ling']}, ...]
     parallel   - Run spawns in parallel (default: false)

   Returns: {:results [...] :summary {:total N :success M :failed F}}"
  (let [spawn-handlers {:spawn handle-spawn}
        batch-fn (make-batch-handler spawn-handlers)]
    (fn [{:keys [operations] :as params}]
      (if (or (nil? operations) (empty? operations))
        (mcp-error "operations is required (array of spawn parameter objects)")
        (let [;; Auto-inject :command "spawn" into each operation
              ops-with-command (mapv #(assoc % :command "spawn") operations)]
          (batch-fn (assoc params :operations ops-with-command)))))))

;; =============================================================================
;; Handlers Map
;; =============================================================================

(def canonical-handlers
  "Map of canonical command keywords to handler functions.
   Supports n-depth dispatch via cli/make-cli-handler:
   - Flat: spawn, status, kill, kill-batch, dispatch, claims, collect, broadcast, cleanup
   - Nested: dag start, dag stop, dag status (dag alone defaults to status)"
  {:spawn       handle-spawn
   :status      handle-status
   :kill        handle-kill
   :kill-batch  handle-kill-batch
   :batch-spawn batch-spawn-handler
   :dispatch    handle-dispatch
   :claims     handle-claims
   :collect    handle-collect
   :broadcast  handle-broadcast
   :cleanup    handle-cleanup
   :dag        {:start    handle-dag-start
                :stop     handle-dag-stop
                :status   handle-dag-status
                :_handler handle-dag-status}})

(def handlers
  "Canonical handlers merged with deprecated aliases (with log warnings)."
  (merge canonical-handlers
         (reduce-kv (fn [m alias-kw canonical-kw]
                      (assoc m alias-kw
                             (wrap-deprecated alias-kw canonical-kw
                                              (get canonical-handlers canonical-kw))))
                    {} deprecated-aliases)))

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
   :consolidated true
   :description "Unified agent operations: spawn (create ling/drone), status (query agents), kill (terminate), kill-batch (terminate multiple agents in one call), batch-spawn (spawn multiple agents at once via operations array), dispatch (send task), claims (file ownership), list (deprecated alias for status), collect (get task result), broadcast (prompt all), cleanup (remove orphan agents after Emacs restart). Type: 'ling' (Claude Code instance) or 'drone' (OpenRouter leaf worker). Nested: dag (start/stop/status DAGWave scheduler). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["spawn" "status" "kill" "kill-batch" "batch-spawn" "dispatch" "claims" "list" "collect" "broadcast" "cleanup" "dag start" "dag stop" "dag status" "help"]
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
                                       :description "Model override for drone or ling. For drones: OpenRouter model ID. For lings: 'claude' (default, Claude Code CLI) or OpenRouter model ID (auto-forces headless spawn mode, e.g., 'deepseek/deepseek-v3.2')"}
                              "task" {:type "string"
                                      :description "Initial task to dispatch on spawn"}
                              "spawn_mode" {:type "string"
                                            :enum ["vterm" "headless"]
                                            :description "Spawn mode for lings: 'vterm' (default, Emacs buffer) or 'headless' (OS subprocess, no Emacs required). Headless mode captures stdout to ring buffer and supports stdin dispatch. Note: 'headless' maps to :agent-sdk (Claude Agent SDK) since 0.12.0."}
                              "agents" {:type "object"
                                        :description "[spawn] Subagent definitions for Claude Agent SDK sessions. Map of agent-name to agent definition object. Each definition: {description: string, prompt: string, tools?: string[], model?: 'sonnet'|'opus'|'haiku'|'inherit'}. Only effective with agent-sdk spawn mode (headless)."}
                              ;; common params
                              "agent_id" {:type "string"
                                          :description "Agent ID for status/kill/dispatch/claims"}
                              "agent_ids" {:type "array"
                                           :items {:type "string"}
                                           :description "Array of agent IDs for kill-batch"}
                              ;; batch-spawn params
                              "operations" {:type "array"
                                            :items {:type "object"}
                                            :description "Array of spawn parameter objects for batch-spawn. Each object: {type, name, cwd, presets, model, task, spawn_mode, parent, kanban_task_id}"}
                              "parallel" {:type "boolean"
                                          :description "Run batch operations in parallel (default: false)"}
                              "project_id" {:type "string"
                                            :description "Project ID filter for status"}
                              ;; dispatch params
                              "prompt" {:type "string"
                                        :description "Task prompt for dispatch or broadcast"}
                              "files" {:type "array"
                                       :items {:type "string"}
                                       :description "Files for drone or dispatch"}
                              "priority" {:type "string"
                                          :enum ["low" "normal" "high"]
                                          :description "Task priority for dispatch"}
                              ;; KG-compressed context params (dispatch)
                              "ctx_refs" {:type "object"
                                          :description "[dispatch] Map of category->ctx-id for KG-compressed context. When provided, creates RefContext (~25x compression vs text). E.g. {\"axioms\": \"ctx-123\", \"decisions\": \"ctx-456\"}"}
                              "kg_node_ids" {:type "array"
                                             :items {:type "string"}
                                             :description "[dispatch] KG node IDs for graph traversal seeds. Combined with ctx_refs for structural context reconstruction."}
                              "scope" {:type "string"
                                       :description "[dispatch] Project scope for KG traversal (auto-derived from agent if omitted)"}
                              "parent" {:type "string"
                                        :description "Parent agent ID for spawn"}
                              "kanban_task_id" {:type "string"
                                                :description "Kanban task ID to link to ling. On session_complete, linked task auto-moves to done."}
                              ;; kill params
                              "force" {:type "boolean"
                                       :description "Force kill even if critical ops in progress"}
                              "directory" {:type "string"
                                           :description "Caller's working directory (for cross-project ownership check)"}
                              "force_cross_project" {:type "boolean"
                                                     :description "HIL override: Allow killing agents from different projects (default: false). Required when target agent belongs to different project than caller."}
                              ;; collect params
                              "task_id" {:type "string"
                                         :description "Task ID for collect operation"}
                              "timeout_ms" {:type "integer"
                                            :description "Timeout in ms for collect (default: 300000)"}
                              ;; dag params
                              "plan_id" {:type "string"
                                         :description "Plan memory entry ID for dag start (required for 'dag start')"}
                              "max_slots" {:type "integer"
                                           :description "Max concurrent lings for dag scheduler (default: 5)"}}
                 :required ["command"]}
   :handler handle-agent})

(def tools
  "Tool definitions for registration."
  [tool-def])

(comment
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

  ;; Kill multiple agents in one call
  ;; (handle-kill-batch {:agent_ids ["ling-1" "ling-2" "drone-3"]})

  ;; Dispatch a task (plain text)
  ;; (handle-dispatch {:agent_id "ling-123" :prompt "Fix the bug" :files ["src/bug.clj"]})

  ;; Dispatch with KG-compressed context (pass-by-reference)
  ;; (handle-dispatch {:agent_id "ling-123"
  ;;                   :prompt "Fix the bug"
  ;;                   :ctx_refs {"axioms" "ctx-123" "decisions" "ctx-456"}
  ;;                   :kg_node_ids ["20260207-dec1"]
  ;;                   :scope "hive-mcp"})

  ;; Get claims for an agent
  ;; (handle-claims {:agent_id "drone-456"})

  ;; List all agents (deprecated — use status)
  ;; (handle-agent {:command "status"})

  ;; Batch spawn multiple agents at once
  ;; (handle-agent {:command "batch-spawn"
  ;;               :operations [{:type "ling" :name "worker-1" :cwd "/project" :presets ["ling"]}
  ;;                             {:type "ling" :name "worker-2" :cwd "/project" :presets ["ling"]}]
  ;;               :parallel true})

  ;; DAG Scheduler (n-depth subcommands)
  ;; (handle-agent {:command "dag status"})
  ;; (handle-agent {:command "dag start" :plan_id "mem-123" :cwd "/project"})
  ;; (handle-agent {:command "dag stop"})
  )

