;; Cost tracking and budget management tools
(ns hive-mcp.tools.cost
  "MCP tool definitions for cost tracking and budget management.

   Provides tools for:
   - Querying usage summary and dashboard data
   - Managing budget limits
   - Identifying high-token tasks
   - Resetting tracking data"
  (:require [hive-mcp.agent.cost :as cost]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [taoensso.timbre :as log]))

;; Module for MCP cost tracking tools - handles budget management, usage reporting, and rate limiting
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Tool Handlers
;;; =============================================================================

(defn handle-get-usage-summary
  "Get comprehensive cost/usage summary for dashboard.

   Returns token usage, savings, budget status, and recent activity."
  [_params]
  (try
    (let [summary (cost/get-usage-summary)
          ;; Format Instant timestamps for JSON
          formatted (-> summary
                        (update-in [:hourly :window-start] str)
                        (update :recent-drones
                                (fn [drones]
                                  (mapv #(update % :timestamp str) drones)))
                        (update :recent-waves
                                (fn [waves]
                                  (mapv (fn [w]
                                          (-> w
                                              (update :started-at str)
                                              (update :completed-at str)))
                                        waves))))]
      (mcp-json formatted))
    (catch Exception e
      (log/error e "get_cost_summary failed")
      (mcp-error (str "Failed to get usage summary: " (ex-message e))))))

(defn handle-get-budgets
  "Get current budget configuration."
  [_params]
  (mcp-json (cost/get-budgets)))

(defn handle-set-budgets
  "Update budget configuration.

   Parameters:
     per_drone - Max tokens per drone task (optional)
     per_wave  - Max tokens per wave (optional)
     per_hour  - Hourly rate limit (optional)"
  [{:keys [per_drone per_wave per_hour]}]
  (try
    (let [new-budgets (cond-> {}
                        per_drone (assoc :per-drone per_drone)
                        per_wave (assoc :per-wave per_wave)
                        per_hour (assoc :per-hour per_hour))
          updated (cost/set-budgets! new-budgets)]
      (mcp-json {:success true
                 :message "Budgets updated"
                 :budgets updated}))
    (catch Exception e
      (log/error e "set_budgets failed")
      (mcp-error (str "Failed to set budgets: " (ex-message e))))))

(defn handle-check-budget
  "Check if a request would exceed budget limits.

   Parameters:
     estimated_tokens - Estimated tokens for the request
     drone_id         - Optional drone ID for context
     wave_id          - Optional wave ID for context"
  [{:keys [estimated_tokens drone_id wave_id]}]
  (try
    (if-not estimated_tokens
      (mcp-error "estimated_tokens is required")
      (let [result (cost/check-budget estimated_tokens
                                      {:drone-id drone_id
                                       :wave-id wave_id})]
        (mcp-json result)))
    (catch Exception e
      (log/error e "check_budget failed")
      (mcp-error (str "Failed to check budget: " (ex-message e))))))

(defn handle-get-high-token-tasks
  "Get tasks that consumed the most tokens.

   Parameters:
     limit - Max number of tasks to return (default: 10)"
  [{:keys [limit]}]
  (try
    (let [tasks (cost/identify-high-token-tasks (or limit 10))
          formatted (mapv #(update % :timestamp str) tasks)]
      (mcp-json {:high_token_tasks formatted
                 :count (count formatted)}))
    (catch Exception e
      (log/error e "get_high_token_tasks failed")
      (mcp-error (str "Failed to get high token tasks: " (ex-message e))))))

(defn handle-reset-hourly
  "Reset hourly rate limit tracking."
  [_params]
  (try
    (cost/reset-hourly!)
    (mcp-json {:success true
               :message "Hourly tracking reset"})
    (catch Exception e
      (log/error e "reset_hourly failed")
      (mcp-error (str "Failed to reset hourly: " (ex-message e))))))

;;; =============================================================================
;;; Budget Guardrail Status (P2-T4)
;;; =============================================================================

(defn handle-get-agent-budget
  "Get budget guardrail status for a specific agent.

   Parameters:
     agent_id - Agent identifier (required)"
  [{:keys [agent_id]}]
  (try
    (if-not agent_id
      (mcp-error "agent_id is required")
      (if-let [status-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/get-budget-status)]
        (if-let [status (status-fn agent_id)]
          (mcp-json status)
          (mcp-json {:agent-id agent_id :registered false :message "No budget registered for this agent"}))
        (mcp-json {:available false :message "Budget guardrail module not loaded"})))
    (catch Exception e
      (log/error e "get_agent_budget failed")
      (mcp-error (str "Failed to get agent budget: " (ex-message e))))))

(defn handle-get-all-agent-budgets
  "Get budget guardrail status for all registered agents."
  [_params]
  (try
    (if-let [all-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/get-all-budget-statuses)]
      (let [statuses (all-fn)
            total-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/get-total-spend)
            total (when total-fn (total-fn))]
        (mcp-json {:agents statuses
                   :total (or total {:total-spend-usd 0.0 :agent-count 0})}))
      (mcp-json {:available false :message "Budget guardrail module not loaded"}))
    (catch Exception e
      (log/error e "get_all_agent_budgets failed")
      (mcp-error (str "Failed to get agent budgets: " (ex-message e))))))

;;; =============================================================================
;;; Tool Definitions
;;; =============================================================================

(def tools
  "MCP tool definitions for cost tracking."
  [{:name "cost_get_summary"
    :description "Get comprehensive cost/usage summary including token usage, savings vs premium models, budget status, and recent drone/wave activity. Use for dashboard display and monitoring."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-get-usage-summary}

   {:name "cost_get_budgets"
    :description "Get current budget configuration including per-drone, per-wave, and hourly rate limits."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-get-budgets}

   {:name "cost_set_budgets"
    :description "Update budget configuration. Set token limits for drones, waves, and hourly rate limits. Omit a parameter to keep its current value."
    :inputSchema {:type "object"
                  :properties {:per_drone {:type "integer"
                                           :description "Max tokens per drone task (default: 8000)"}
                               :per_wave {:type "integer"
                                          :description "Max tokens per wave (default: 50000)"}
                               :per_hour {:type "integer"
                                          :description "Hourly rate limit (default: 200000)"}}
                  :required []}
    :handler handle-set-budgets}

   {:name "cost_check_budget"
    :description "Check if a request would exceed budget limits before dispatch. Returns allowed status and current usage levels."
    :inputSchema {:type "object"
                  :properties {:estimated_tokens {:type "integer"
                                                  :description "Estimated tokens for the request (required)"}
                               :drone_id {:type "string"
                                          :description "Drone ID for per-drone budget check (optional)"}
                               :wave_id {:type "string"
                                         :description "Wave ID for per-wave budget check (optional)"}}
                  :required ["estimated_tokens"]}
    :handler handle-check-budget}

   {:name "cost_high_token_tasks"
    :description "Identify tasks that consumed the most tokens. Use to find optimization opportunities."
    :inputSchema {:type "object"
                  :properties {:limit {:type "integer"
                                       :description "Max number of tasks to return (default: 10)"}}
                  :required []}
    :handler handle-get-high-token-tasks}

   {:name "cost_reset_hourly"
    :description "Reset hourly rate limit tracking. Use if you need to manually clear the rate limit window."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-reset-hourly}

   {:name "cost_agent_budget"
    :description "Get budget guardrail status for a specific agent. Shows max budget, spent USD, remaining, tool calls, and whether budget is exceeded."
    :inputSchema {:type "object"
                  :properties {:agent_id {:type "string"
                                          :description "Agent identifier to check budget for (required)"}}
                  :required ["agent_id"]}
    :handler handle-get-agent-budget}

   {:name "cost_all_agent_budgets"
    :description "Get budget guardrail status for all registered agents. Shows per-agent spend and total USD spend across the hive."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-get-all-agent-budgets}])
