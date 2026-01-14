(ns hive-mcp.agent.mcp
  "MCP tool definitions for agent delegation.
   
   Defines the MCP tools exposed for agent-related operations:
   - agent_delegate - delegate tasks to local/cloud models
   - delegate_drone - delegate to token-optimized drones
   - openrouter_* - model configuration
   - preset_* - preset configuration"
  (:require [hive-mcp.agent.config :as config]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [clojure.string :as str]))

;;; ============================================================
;;; Tool Handlers
;;; ============================================================

(defn handle-agent-delegate
  "MCP handler for agent.delegate tool.
   Requires delegate-fn to be passed in to avoid circular dependency."
  [delegate-fn {:keys [backend model task preset task_type api_key tools permissions max_steps trace]}]
  (try
    (let [result (delegate-fn {:backend (keyword (or backend "openrouter"))
                               :model model
                               :preset preset
                               :task-type (when task_type (keyword task_type))
                               :api-key api_key
                               :task task
                               :tools (when tools (set tools))
                               :permissions (set (map keyword (or permissions [])))
                               :max-steps (or max_steps 50)
                               :trace (boolean trace)})]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Delegation failed: " (ex-message e))))))

(defn handle-delegate-drone
  "MCP tool handler for delegate_drone.
   Requires delegate-drone-fn to be passed in to avoid circular dependency."
  [delegate-drone-fn {:keys [task files preset trace parent_id]}]
  (if (str/blank? task)
    (mcp-error "Task is required")
    (let [result (delegate-drone-fn {:task task
                                     :files files
                                     :preset (or preset "drone-worker")
                                     :trace (if (nil? trace) true trace)
                                     :parent-id parent_id})]
      (mcp-json result))))

;;; ============================================================
;;; Tool Definitions
;;; ============================================================

(defn make-tools
  "Create MCP tool definitions with injected handler functions.
   
   This avoids circular dependencies by receiving the actual delegate
   functions as parameters rather than requiring them."
  [delegate-fn delegate-drone-fn]
  [{:name "agent_delegate"
    :description "Delegate a task to a local LLM (Ollama) or cloud LLM (OpenRouter) with MCP tool access. The delegated model runs a tool-use loop until task completion. Use for implementation tasks to conserve coordinator context."
    :inputSchema {:type "object"
                  :properties {"backend" {:type "string"
                                          :enum ["ollama" "openrouter"]
                                          :description "Backend: 'ollama' (local, default) or 'openrouter' (cloud)"}
                               "model" {:type "string"
                                        :description "Model name (backend-specific)"}
                               "preset" {:type "string"
                                         :description "Swarm preset for auto model selection (e.g., 'tdd', 'reviewer', 'documenter')"}
                               "task_type" {:type "string"
                                            :enum ["coding" "coding-alt" "arch" "docs"]
                                            :description "OpenRouter task type (preset takes priority if both specified)"}
                               "api_key" {:type "string"
                                          :description "OpenRouter API key (or set OPENROUTER_API_KEY env)"}
                               "task" {:type "string"
                                       :description "Task description for the agent"}
                               "tools" {:type "array"
                                        :items {:type "string"}
                                        :description "Tool names to allow (nil = all registered)"}
                               "permissions" {:type "array"
                                              :items {:type "string"}
                                              :description "Permissions: 'auto-approve' skips human checks"}
                               "max_steps" {:type "integer"
                                            :description "Max tool-use iterations (default: 15)"}
                               "trace" {:type "boolean"
                                        :description "Emit progress events via channel"}}
                  :required ["task"]}
    :handler (partial handle-agent-delegate delegate-fn)}

   {:name "delegate_drone"
    :description "Delegate a task to a drone (token-optimized leaf agent). Drones use OpenRouter free-tier models and receive catchup context automatically. Use for file mutations to save coordinator tokens."
    :inputSchema {:type "object"
                  :properties {:task {:type "string"
                                      :description "Task description for the drone"}
                               :files {:type "array"
                                       :items {:type "string"}
                                       :description "List of files the drone will modify"}
                               :preset {:type "string"
                                        :description "Preset to use (default: drone-worker)"}
                               :trace {:type "boolean"
                                       :description "Enable progress events (default: true)"}
                               :parent_id {:type "string"
                                           :description "Parent ling's slave-id for swarm status sync. Pass your CLAUDE_SWARM_SLAVE_ID env var value."}}
                  :required ["task"]}
    :handler (partial handle-delegate-drone delegate-drone-fn)}

   ;; OpenRouter model configuration
   {:name "openrouter_list_models"
    :description "List all configured OpenRouter task-type to model mappings. Shows which models are used for :coding, :arch, :docs tasks."
    :inputSchema {:type "object" :properties {}}
    :handler (fn [_]
               (mcp-json {:models (config/list-models)
                          :task-types (keys (config/list-models))}))}

   {:name "openrouter_set_model"
    :description "Set the OpenRouter model for a specific task type. Task types: coding, coding-alt, arch, docs (or custom)."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :description "Task type (e.g., 'coding', 'arch', 'docs')"}
                               "model" {:type "string"
                                        :description "OpenRouter model ID (e.g., 'mistralai/devstral-2512:free')"}}
                  :required ["task_type" "model"]}
    :handler (fn [{:keys [task_type model]}]
               (let [task-key (keyword task_type)
                     updated (config/set-model! task-key model)]
                 (mcp-json {:success true
                            :message (format "Set %s → %s" task_type model)
                            :models updated})))}

   {:name "openrouter_remove_model"
    :description "Remove an OpenRouter task-type mapping."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :description "Task type to remove"}}
                  :required ["task_type"]}
    :handler (fn [{:keys [task_type]}]
               (let [task-key (keyword task_type)
                     updated (config/remove-model! task-key)]
                 (mcp-json {:success true
                            :message (format "Removed %s" task_type)
                            :models updated})))}

   ;; Preset to task-type mappings
   {:name "preset_list_mappings"
    :description "List all swarm preset to task-type mappings. Shows which presets map to :coding, :arch, :docs."
    :inputSchema {:type "object" :properties {}}
    :handler (fn [_]
               (let [mappings (config/list-preset-mappings)
                     by-type (group-by val mappings)]
                 (mcp-json {:mappings mappings
                            :by-task-type {:coding (keys (get by-type :coding))
                                           :arch (keys (get by-type :arch))
                                           :docs (keys (get by-type :docs))}})))}

   {:name "preset_set_task_type"
    :description "Set the task type for a swarm preset. This determines which OpenRouter model is used when delegating with that preset."
    :inputSchema {:type "object"
                  :properties {"preset" {:type "string"
                                         :description "Preset name (e.g., 'tdd', 'reviewer', 'documenter')"}
                               "task_type" {:type "string"
                                            :enum ["coding" "coding-alt" "arch" "docs"]
                                            :description "Task type to map to"}}
                  :required ["preset" "task_type"]}
    :handler (fn [{:keys [preset task_type]}]
               (let [updated (config/set-preset-task-type! preset task_type)]
                 (mcp-json {:success true
                            :message (format "Set preset %s → %s" preset task_type)
                            :mappings updated})))}])
