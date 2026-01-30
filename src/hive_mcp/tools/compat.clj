(ns hive-mcp.tools.compat
  "Backward-compatibility shim infrastructure for deprecated tools.

   Provides a factory function to create deprecation shims that:
   - Log deprecation warnings with migration guidance
   - Translate old parameter names to new ones
   - Delegate to consolidated tool handlers

   SOLID: Open/Closed Principle - extend old tools without modifying them.
   CLARITY: L - Thin adapter layer between old and new tool interfaces."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Consolidated Handler Registry
;; =============================================================================

(def ^:private consolidated-handlers
  "Registry of consolidated tool handlers.
   Populated lazily on first access to avoid circular dependencies."
  (atom nil))

(defn- resolve-handler
  "Safely resolve a handler symbol, ensuring namespace is loaded.

   Parameters:
   - sym: Fully qualified symbol (e.g., 'ns/fn)

   Returns: Dereferenced var value, or nil if resolution fails."
  [sym]
  (try
    (require (symbol (namespace sym)))
    (if-let [v (resolve sym)]
      @v
      (do (log/error "Failed to resolve handler:" sym)
          nil))
    (catch Exception e
      (log/error "Exception resolving handler:" sym (.getMessage e))
      nil)))

(defn- load-consolidated-handlers!
  "Load consolidated tool handlers into registry.
   Called once on first access. Validates all handlers are non-nil."
  []
  (let [handlers-map
        {:kg       (resolve-handler 'hive-mcp.tools.consolidated.kg/handle-kg)
         :memory   (resolve-handler 'hive-mcp.tools.consolidated.memory/handle-memory)
         :kanban   (resolve-handler 'hive-mcp.tools.consolidated.kanban/handle-kanban)
         :cider    (resolve-handler 'hive-mcp.tools.consolidated.cider/handle-cider)
         :magit    (resolve-handler 'hive-mcp.tools.consolidated.magit/handle-magit)
         :olympus  (resolve-handler 'hive-mcp.tools.consolidated.olympus/handle-olympus)
         :preset   (resolve-handler 'hive-mcp.tools.consolidated.preset/handle-preset)
         :hivemind (resolve-handler 'hive-mcp.tools.consolidated.hivemind/handle-hivemind)
         :wave     (resolve-handler 'hive-mcp.tools.consolidated.wave/handle-wave)
         :agora    (resolve-handler 'hive-mcp.tools.consolidated.agora/handle-agora)
         :kondo    (resolve-handler 'hive-mcp.tools.consolidated.kondo/handle-kondo)
         :project  (resolve-handler 'hive-mcp.tools.consolidated.project/handle-project)
         :session  (resolve-handler 'hive-mcp.tools.consolidated.session/handle-session)
         :emacs    (resolve-handler 'hive-mcp.tools.consolidated.emacs/handle-emacs)
         :agent    (resolve-handler 'hive-mcp.tools.consolidated.agent/handle-agent)}
        nil-handlers (filterv (fn [[_ v]] (nil? v)) handlers-map)]
    ;; Validate all handlers loaded successfully
    (when (seq nil-handlers)
      (log/error "Consolidated handlers failed to load:"
                 (mapv first nil-handlers)))
    handlers-map))

(defn get-consolidated-handler
  "Look up a consolidated tool handler by name.

   Parameters:
   - tool-name: Keyword or string name of the consolidated tool
                (e.g. :kg, :memory, \"kanban\")

   Returns: Handler function or nil if not found."
  [tool-name]
  (when-not @consolidated-handlers
    (reset! consolidated-handlers (load-consolidated-handlers!)))
  (get @consolidated-handlers (keyword tool-name)))

;; =============================================================================
;; Shim Factory
;; =============================================================================

(defn make-shim
  "Factory that creates deprecation shims for old tool handlers.

   Parameters:
   - old-name:     String name of the deprecated tool
   - new-tool:     Keyword name of the consolidated tool (e.g. :kg, :memory)
   - new-cmd:      String name of the command in the new tool
   - param-rename: (optional) Map of {old-param-key new-param-key} for translation
   - static-params:(optional) Map of static params to merge into every call

   Returns: Handler function with deprecation metadata.

   Example:
   ```clojure
   (def kg-traverse-shim
     (make-shim \"kg_traverse\" :kg \"traverse\"
                :param-rename {:start_node :node_id}))
   ```"
  [old-name new-tool new-cmd & {:keys [param-rename static-params]
                                :or   {param-rename  {}
                                       static-params {}}}]
  (with-meta
    (fn [params]
      ;; Log deprecation warning with migration guidance
      (log/warn "DEPRECATED:" old-name "-> use" (name new-tool)
                "with command" new-cmd
                "(sunset: 2026-04-01)")

      ;; Normalize params to keyword keys (MCP may send string keys from JSON)
      ;; Then rename parameters using the param-rename map
      ;;
      ;; BUG FIX: bb-mcp injects agent_id:"coordinator" on all tool calls for
      ;; piggyback tracking. When we rename slave_id->agent_id, the injected
      ;; agent_id value ("coordinator") could overwrite our renamed value
      ;; depending on iteration order. Fix: remove target keys before renaming.
      (let [normalized (into {} (map (fn [[k v]] [(keyword k) v]) params))
            ;; Get the set of target keys that will be created by renames
            rename-targets (set (vals param-rename))
            ;; Remove any existing params that are rename targets BUT also have
            ;; a source key present. This prevents bb-mcp's agent_id from
            ;; overwriting the renamed slave_id value.
            ;; param-rename is {source-key target-key}, e.g. {:slave_id :agent_id}
            cleaned (reduce-kv
                     (fn [m source-key target-key]
                       (if (contains? normalized source-key)
                         ;; Source key exists, so remove the target to prevent overwrite
                         (dissoc m target-key)
                         m))
                     normalized
                     param-rename)
            renamed (reduce-kv
                     (fn [m k v]
                       (let [new-key (get param-rename k k)]
                         (assoc m new-key v)))
                     {}
                     cleaned)
            ;; Merge static params and add the command
            final (merge static-params renamed {:command new-cmd})]

        ;; Delegate to consolidated handler
        (if-let [handler (get-consolidated-handler new-tool)]
          (let [result (handler final)]
            (when (nil? result)
              (log/error "Consolidated handler returned nil"
                         {:tool new-tool :cmd new-cmd :params final}))
            result)
          {:isError true
           :text (str "Consolidated tool not found: " (name new-tool)
                      ". Migration may be incomplete.")})))

    ;; Metadata for deprecation tracking
    {:deprecated   true
     :sunset-date  "2026-04-01"
     :old-name     old-name
     :new-tool     new-tool
     :new-cmd      new-cmd
     :param-rename param-rename}))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn deprecated?
  "Check if a handler is a deprecation shim."
  [handler]
  (true? (:deprecated (meta handler))))

(defn sunset-date
  "Get the sunset date for a deprecated handler."
  [handler]
  (:sunset-date (meta handler)))

(defn migration-info
  "Get migration information for a deprecated handler.
   Returns map with :new-tool, :new-cmd, and :param-rename."
  [handler]
  (when-let [m (meta handler)]
    (select-keys m [:old-name :new-tool :new-cmd :param-rename])))

(defn list-consolidated-tools
  "List all available consolidated tool names."
  []
  (when-not @consolidated-handlers
    (reset! consolidated-handlers (load-consolidated-handlers!)))
  (keys @consolidated-handlers))

;; =============================================================================
;; Deprecation Shims - Agent/Swarm
;; =============================================================================

(def swarm-spawn-shim
  "DEPRECATED: Use `agent spawn` with type 'ling' instead."
  (make-shim "swarm_spawn" :agent "spawn"
             :param-rename {:directory :cwd
                            :preset :presets}
             :static-params {:type "ling"}))

(def swarm-status-shim
  "DEPRECATED: Use `agent status` instead."
  (make-shim "swarm_status" :agent "status"))

(def swarm-kill-shim
  "DEPRECATED: Use `agent kill` instead."
  (make-shim "swarm_kill" :agent "kill"
             :param-rename {:slave_id :agent_id}))

(def swarm-dispatch-shim
  "DEPRECATED: Use `agent dispatch` instead."
  (make-shim "swarm_dispatch" :agent "dispatch"
             :param-rename {:slave_id :agent_id
                            :message :prompt}))

(def lings-available-shim
  "DEPRECATED: Use `agent list` with type 'ling' instead."
  (make-shim "lings_available" :agent "list"
             :static-params {:type "ling"}))

(def swarm-list-presets-shim
  "DEPRECATED: Use `preset list` instead."
  (make-shim "swarm_list_presets" :preset "list"))

(def swarm-collect-shim
  "DEPRECATED: Use `agent collect` instead."
  (make-shim "swarm_collect" :agent "collect"))

(def swarm-broadcast-shim
  "DEPRECATED: Use `agent broadcast` instead."
  (make-shim "swarm_broadcast" :agent "broadcast"))

;; =============================================================================
;; Deprecation Shims - Memory
;; =============================================================================

(def mcp-memory-add-shim
  "DEPRECATED: Use `memory add` instead."
  (make-shim "mcp_memory_add" :memory "add"))

(def mcp-memory-query-shim
  "DEPRECATED: Use `memory query` instead."
  (make-shim "mcp_memory_query" :memory "query"))

(def mcp-memory-get-full-shim
  "DEPRECATED: Use `memory get` instead."
  (make-shim "mcp_memory_get_full" :memory "get"))

(def mcp-memory-search-semantic-shim
  "DEPRECATED: Use `memory search` instead."
  (make-shim "mcp_memory_search_semantic" :memory "search"))

(def mcp-memory-promote-shim
  "DEPRECATED: Use `memory promote` instead."
  (make-shim "mcp_memory_promote" :memory "promote"))

(def mcp-memory-demote-shim
  "DEPRECATED: Use `memory demote` instead."
  (make-shim "mcp_memory_demote" :memory "demote"))

(def mcp-memory-feedback-shim
  "DEPRECATED: Use `memory feedback` instead."
  (make-shim "mcp_memory_feedback" :memory "feedback"))

(def mcp-memory-update-tags-shim
  "DEPRECATED: Use `memory tags` instead."
  (make-shim "mcp_memory_update_tags" :memory "tags"))

(def mcp-memory-cleanup-expired-shim
  "DEPRECATED: Use `memory cleanup` instead."
  (make-shim "mcp_memory_cleanup_expired" :memory "cleanup"))

(def mcp-memory-expiring-soon-shim
  "DEPRECATED: Use `memory expiring` instead."
  (make-shim "mcp_memory_expiring_soon" :memory "expiring"))

(def mcp-memory-query-metadata-shim
  "DEPRECATED: Use `memory metadata` instead."
  (make-shim "mcp_memory_query_metadata" :memory "metadata"))

(def mcp-memory-set-duration-shim
  "DEPRECATED: Use `memory duration` instead."
  (make-shim "mcp_memory_set_duration" :memory "duration"))

(def mcp-memory-log-access-shim
  "DEPRECATED: Use `memory log_access` instead."
  (make-shim "mcp_memory_log_access" :memory "log_access"))

(def mcp-memory-helpfulness-ratio-shim
  "DEPRECATED: Use `memory helpfulness` instead."
  (make-shim "mcp_memory_helpfulness_ratio" :memory "helpfulness"))

(def mcp-memory-migrate-project-shim
  "DEPRECATED: Use `memory migrate` instead."
  (make-shim "mcp_memory_migrate_project" :memory "migrate"
             :param-rename {:old-project-id :old_project_id
                            :new-project-id :new_project_id
                            :update-scopes :update_scopes}))

(def mcp-memory-import-json-shim
  "DEPRECATED: Use `memory import` instead."
  (make-shim "mcp_memory_import_json" :memory "import"
             :param-rename {:project-id :project_id
                            :dry-run :dry_run}))

;; =============================================================================
;; Deprecation Shims - Knowledge Graph
;; =============================================================================

(def kg-traverse-shim
  "DEPRECATED: Use `kg traverse` instead."
  (make-shim "kg_traverse" :kg "traverse"))

(def kg-add-edge-shim
  "DEPRECATED: Use `kg edge` instead."
  (make-shim "kg_add_edge" :kg "edge"))

(def kg-impact-analysis-shim
  "DEPRECATED: Use `kg impact` instead."
  (make-shim "kg_impact_analysis" :kg "impact"))

(def kg-subgraph-shim
  "DEPRECATED: Use `kg subgraph` instead."
  (make-shim "kg_subgraph" :kg "subgraph"))

(def kg-stats-shim
  "DEPRECATED: Use `kg stats` instead."
  (make-shim "kg_stats" :kg "stats"))

(def kg-find-path-shim
  "DEPRECATED: Use `kg path` instead."
  (make-shim "kg_find_path" :kg "path"))

(def kg-node-context-shim
  "DEPRECATED: Use `kg context` instead."
  (make-shim "kg_node_context" :kg "context"))

(def kg-promote-shim
  "DEPRECATED: Use `kg promote` instead."
  (make-shim "kg_promote" :kg "promote"))

(def kg-reground-shim
  "DEPRECATED: Use `kg reground` instead."
  (make-shim "kg_reground" :kg "reground"))

(def kg-contradictions-shim
  "DEPRECATED: Use `kg contradictions` instead."
  (make-shim "kg_contradictions" :kg "contradictions"))

(def kg-backfill-grounding-shim
  "DEPRECATED: Use `kg backfill` instead."
  (make-shim "kg_backfill_grounding" :kg "backfill"))

;; =============================================================================
;; Deprecation Shims - Hivemind
;; =============================================================================

(def hivemind-shout-shim
  "DEPRECATED: Use `hivemind shout` instead."
  (make-shim "hivemind_shout" :hivemind "shout"))

(def hivemind-ask-shim
  "DEPRECATED: Use `hivemind ask` instead."
  (make-shim "hivemind_ask" :hivemind "ask"))

(def hivemind-status-shim
  "DEPRECATED: Use `hivemind status` instead."
  (make-shim "hivemind_status" :hivemind "status"))

(def hivemind-respond-shim
  "DEPRECATED: Use `hivemind respond` instead."
  (make-shim "hivemind_respond" :hivemind "respond"))

(def hivemind-messages-shim
  "DEPRECATED: Use `hivemind messages` instead."
  (make-shim "hivemind_messages" :hivemind "messages"))

;; =============================================================================
;; Deprecation Shims - Magit
;; =============================================================================

(def magit-status-shim
  "DEPRECATED: Use `magit status` instead."
  (make-shim "magit_status" :magit "status"))

(def magit-stage-shim
  "DEPRECATED: Use `magit stage` instead."
  (make-shim "magit_stage" :magit "stage"))

(def magit-commit-shim
  "DEPRECATED: Use `magit commit` instead."
  (make-shim "magit_commit" :magit "commit"))

(def magit-push-shim
  "DEPRECATED: Use `magit push` instead."
  (make-shim "magit_push" :magit "push"))

(def magit-branches-shim
  "DEPRECATED: Use `magit branches` instead."
  (make-shim "magit_branches" :magit "branches"))

(def magit-log-shim
  "DEPRECATED: Use `magit log` instead."
  (make-shim "magit_log" :magit "log"))

(def magit-diff-shim
  "DEPRECATED: Use `magit diff` instead."
  (make-shim "magit_diff" :magit "diff"))

(def magit-pull-shim
  "DEPRECATED: Use `magit pull` instead."
  (make-shim "magit_pull" :magit "pull"))

(def magit-fetch-shim
  "DEPRECATED: Use `magit fetch` instead."
  (make-shim "magit_fetch" :magit "fetch"))

(def magit-feature-branches-shim
  "DEPRECATED: Use `magit feature-branches` instead."
  (make-shim "magit_feature_branches" :magit "feature-branches"))

;; =============================================================================
;; Shim Registry - For Registration in tools.clj
;; =============================================================================

(def shims
  "Map of old tool names to their shim handlers.
   Use this map to register deprecated tools in the MCP tool registry."
  {;; Agent/Swarm shims
   "swarm_spawn"         swarm-spawn-shim
   "swarm_status"        swarm-status-shim
   "swarm_kill"          swarm-kill-shim
   "swarm_dispatch"      swarm-dispatch-shim
   "lings_available"     lings-available-shim
   "swarm_list_presets"  swarm-list-presets-shim
   "swarm_collect"       swarm-collect-shim
   "swarm_broadcast"     swarm-broadcast-shim

   ;; Memory shims
   "mcp_memory_add"              mcp-memory-add-shim
   "mcp_memory_query"            mcp-memory-query-shim
   "mcp_memory_get_full"         mcp-memory-get-full-shim
   "mcp_memory_search_semantic"  mcp-memory-search-semantic-shim
   "mcp_memory_promote"          mcp-memory-promote-shim
   "mcp_memory_demote"           mcp-memory-demote-shim
   "mcp_memory_feedback"         mcp-memory-feedback-shim
   "mcp_memory_update_tags"      mcp-memory-update-tags-shim
   "mcp_memory_cleanup_expired"  mcp-memory-cleanup-expired-shim
   "mcp_memory_expiring_soon"    mcp-memory-expiring-soon-shim
   "mcp_memory_query_metadata"   mcp-memory-query-metadata-shim
   "mcp_memory_set_duration"     mcp-memory-set-duration-shim
   "mcp_memory_log_access"       mcp-memory-log-access-shim
   "mcp_memory_helpfulness_ratio" mcp-memory-helpfulness-ratio-shim
   "mcp_memory_migrate_project"  mcp-memory-migrate-project-shim
   "mcp_memory_import_json"      mcp-memory-import-json-shim

   ;; Knowledge Graph shims
   "kg_traverse"          kg-traverse-shim
   "kg_add_edge"          kg-add-edge-shim
   "kg_impact_analysis"   kg-impact-analysis-shim
   "kg_subgraph"          kg-subgraph-shim
   "kg_stats"             kg-stats-shim
   "kg_find_path"         kg-find-path-shim
   "kg_node_context"      kg-node-context-shim
   "kg_promote"           kg-promote-shim
   "kg_reground"          kg-reground-shim
   "kg_contradictions"    kg-contradictions-shim
   "kg_backfill_grounding" kg-backfill-grounding-shim

   ;; Hivemind shims
   "hivemind_shout"   hivemind-shout-shim
   "hivemind_ask"     hivemind-ask-shim
   "hivemind_status"  hivemind-status-shim
   "hivemind_respond" hivemind-respond-shim
   "hivemind_messages" hivemind-messages-shim

   ;; Magit shims
   "magit_status"          magit-status-shim
   "magit_stage"           magit-stage-shim
   "magit_commit"          magit-commit-shim
   "magit_push"            magit-push-shim
   "magit_branches"        magit-branches-shim
   "magit_log"             magit-log-shim
   "magit_diff"            magit-diff-shim
   "magit_pull"            magit-pull-shim
   "magit_fetch"           magit-fetch-shim
   "magit_feature_branches" magit-feature-branches-shim})

(defn shim-count
  "Return the count of defined shims."
  []
  (count shims))

(defn list-deprecated-tools
  "List all deprecated tool names with their migration targets."
  []
  (for [[old-name handler] shims]
    (let [{:keys [new-tool new-cmd]} (migration-info handler)]
      {:old-name   old-name
       :new-tool   (name new-tool)
       :new-cmd    new-cmd
       :sunset     (sunset-date handler)})))

;; =============================================================================
;; MCP Tool Definitions for Deprecated Tools
;; =============================================================================
;;
;; These tool definitions expose the backward-compatibility shims via MCP.
;; Each tool has:
;; - :deprecated true metadata for tooling
;; - Deprecation warning in description
;; - Permissive inputSchema (validation handled by consolidated handlers)
;; - Handler pointing to the shim function
;;
;; CLARITY: O (Open/Closed) - shims extend behavior without modifying originals

(defn- make-tool-def
  "Generate MCP tool definition for a deprecated tool.

   Parameters:
   - old-name: String name of the deprecated tool
   - handler: The shim handler function

   Returns: MCP tool definition map with deprecation metadata."
  [old-name handler]
  (let [{:keys [new-tool new-cmd]} (migration-info handler)]
    {:name        old-name
     :deprecated  true
     :sunset-date "2026-04-01"
     :replacement {:tool (name new-tool) :command new-cmd}
     :description (format "DEPRECATED (sunset: 2026-04-01). Use `%s %s` instead. This tool will be removed after the sunset date."
                          (name new-tool) new-cmd)
     :inputSchema {:type "object"
                   :properties {}
                   :additionalProperties true
                   :description "All parameters passed through to replacement tool."}
     :handler     handler}))

(def tools
  "MCP tool definitions for all deprecated tools.

   Include this vector in hive-mcp.tools/tools to expose shims via MCP.
   Each tool logs a deprecation warning when called and delegates to
   the consolidated handler.

   Count: 50 deprecated tools covering:
   - swarm_* -> agent (8 tools)
   - mcp_memory_* -> memory (16 tools)
   - kg_* -> kg (11 tools)
   - hivemind_* -> hivemind (5 tools)
   - magit_* -> magit (10 tools)"
  (vec (for [[old-name handler] shims]
         (make-tool-def old-name handler))))
