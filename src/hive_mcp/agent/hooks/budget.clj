(ns hive-mcp.agent.hooks.budget
  "Budget guardrail hook for agent sessions.

   PreToolUse hook that tracks cumulative USD cost per agent and denies
   tool calls when the configured max-budget-usd is exceeded.

   Architecture:
   - Per-agent budget tracking in a shared atom (keyed by agent-id)
   - Model-based cost estimation (tokens → USD via pricing table)
   - Returns {:action :deny :message ... :interrupt? true} when over budget
   - Composable with permissions.clj via composite-handler

   Integration points:
   - register-budget!    — Set budget for an agent (called on spawn)
   - deregister-budget!  — Clean up on kill
   - record-usage!       — Track tool call cost (called by PostToolUse or observer)
   - budget-guardrail-handler — Handler factory for permissions.clj
   - get-budget-status   — Dashboard data

   Wiring:
   - Ling spawn: max-budget-usd from config → register-budget!
   - Permission check: budget-guardrail-handler composed via composite-handler
   - Session complete: deregister-budget!

   CLARITY-T: Telemetry — observable per-agent cost metrics.
   CLARITY-Y: Yield safe failure — deny with interrupt signal, never throw.
   CLARITY-I: Inputs guarded — all public fns validate args."
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Constants
;;; =============================================================================

(def ^:const chars-per-token
  "Approximation: 4 characters ≈ 1 token (matches agent/cost.clj)."
  4)

(def ^:const default-max-budget-usd
  "Default per-agent budget in USD. Conservative default for safety.
   Can be overridden per-agent via register-budget!."
  5.0)

;;; =============================================================================
;;; Model Pricing Table
;;; =============================================================================

(def model-pricing
  "USD cost per 1M tokens by model.
   Free-tier models have $0 cost.
   Prices sourced from provider pricing pages.

   Structure: {model-pattern {:input $/1M :output $/1M}}"
  {;; Claude models (Anthropic direct pricing)
   :claude-sonnet   {:input 3.0   :output 15.0}
   :claude-haiku    {:input 0.25  :output 1.25}
   :claude-opus     {:input 15.0  :output 75.0}

   ;; Free-tier models (OpenRouter)
   :free-tier       {:input 0.0   :output 0.0}

   ;; DeepSeek models
   :deepseek        {:input 0.14  :output 0.28}

   ;; Moonshot Kimi models
   :kimi            {:input 0.20  :output 0.60}

   ;; Default fallback (conservative estimate)
   :default         {:input 3.0   :output 15.0}})

(defn- resolve-model-pricing
  "Resolve pricing for a model identifier string.

   Arguments:
     model - Model string (e.g., 'claude', 'deepseek/deepseek-chat',
             'mistralai/devstral-2512:free')

   Returns:
     Pricing map {:input $/1M :output $/1M}"
  [model]
  (let [m (or model "claude")]
    (cond
      ;; Free-tier models
      (re-find #":free$" m)
      (:free-tier model-pricing)

      ;; Claude models
      (re-find #"(?i)haiku" m)
      (:claude-haiku model-pricing)

      (re-find #"(?i)opus" m)
      (:claude-opus model-pricing)

      (re-find #"(?i)sonnet|claude" m)
      (:claude-sonnet model-pricing)

      ;; DeepSeek
      (re-find #"(?i)deepseek" m)
      (:deepseek model-pricing)

      ;; Kimi / Moonshot
      (re-find #"(?i)kimi|moonshot" m)
      (:kimi model-pricing)

      ;; Default
      :else
      (:default model-pricing))))

(defn estimate-cost-usd
  "Estimate USD cost for a token count.

   Arguments:
     input-tokens  - Number of input tokens
     output-tokens - Number of output tokens
     model         - Model identifier string (optional, default: 'claude')

   Returns:
     Estimated cost in USD (double)"
  [input-tokens output-tokens & [model]]
  (let [pricing (resolve-model-pricing model)
        input-cost  (* (or input-tokens 0) (/ (:input pricing) 1000000.0))
        output-cost (* (or output-tokens 0) (/ (:output pricing) 1000000.0))]
    (+ input-cost output-cost)))

(defn estimate-tool-call-cost
  "Estimate the cost of a single tool call (rough approximation).

   Uses a heuristic: tool calls typically consume ~500 input + ~500 output tokens.
   This is a conservative estimate for PreToolUse budget checks.

   Arguments:
     tool-name - String name of the tool
     input     - Tool input data (map or string)
     model     - Model identifier string (optional)

   Returns:
     Estimated cost in USD (double)"
  [tool-name input & [model]]
  (let [;; Estimate input tokens from tool name + serialized input
        input-chars (+ (count (or tool-name ""))
                       (count (str input)))
        input-tokens (max 100 (int (Math/ceil (/ input-chars chars-per-token))))
        ;; Rough output estimate: most tool calls return ~500-2000 tokens
        ;; Use conservative estimate for budget checking
        output-tokens (case tool-name
                        ;; Read-heavy tools produce more output
                        ("read_file" "grep" "glob_files" "cider_doc" "cider_info")
                        1000
                        ;; Write tools produce less output
                        ("file_write" "bash" "magit_commit")
                        500
                        ;; Default
                        500)]
    (estimate-cost-usd input-tokens output-tokens model)))

;;; =============================================================================
;;; Per-Agent Budget State
;;; =============================================================================

;; Global registry of per-agent budget tracking.
;; Shape: {agent-id {:max-budget-usd   double
;;                   :spent-usd        double
;;                   :model            string
;;                   :tool-calls       long
;;                   :registered-at    Instant
;;                   :last-check-at    Instant
;;                   :exceeded?        boolean
;;                   :denied-calls     long}}
(defonce ^:private *agent-budgets
  (atom {}))

(defn register-budget!
  "Register a budget for an agent session.

   Called on ling spawn to set the max-budget-usd from config.

   Arguments:
     agent-id       - Agent identifier string
     max-budget-usd - Maximum USD spend allowed (double)
     opts           - Optional map:
                      :model - Model identifier for cost estimation

   Returns:
     Budget entry map"
  [agent-id max-budget-usd & [{:keys [model]}]]
  {:pre [(string? agent-id)
         (number? max-budget-usd)
         (pos? max-budget-usd)]}
  (let [entry {:max-budget-usd  (double max-budget-usd)
               :spent-usd       0.0
               :model           (or model "claude")
               :tool-calls      0
               :registered-at   (java.time.Instant/now)
               :last-check-at   nil
               :exceeded?       false
               :denied-calls    0}]
    (swap! *agent-budgets assoc agent-id entry)
    (log/info "[budget-hook] Budget registered"
              {:agent-id agent-id
               :max-budget-usd max-budget-usd
               :model (or model "claude")})
    entry))

(defn deregister-budget!
  "Remove budget tracking for an agent (cleanup on kill/complete).

   Arguments:
     agent-id - Agent identifier string

   Returns:
     Final budget entry (for logging/metrics) or nil if not found"
  [agent-id]
  (let [entry (get @*agent-budgets agent-id)]
    (swap! *agent-budgets dissoc agent-id)
    (when entry
      (log/info "[budget-hook] Budget deregistered"
                {:agent-id agent-id
                 :spent-usd (:spent-usd entry)
                 :max-budget-usd (:max-budget-usd entry)
                 :tool-calls (:tool-calls entry)
                 :denied-calls (:denied-calls entry)}))
    entry))

(defn record-usage!
  "Record USD cost for a completed tool call.

   Called by PostToolUse observer or after tool execution.

   Arguments:
     agent-id      - Agent identifier string
     cost-usd      - Cost of the tool call in USD
     opts          - Optional map:
                     :tool-name - Name of the tool (for logging)

   Returns:
     Updated budget entry or nil if agent not registered"
  [agent-id cost-usd & [{:keys [tool-name]}]]
  (when-let [_entry (get @*agent-budgets agent-id)]
    (let [updated (swap! *agent-budgets update agent-id
                         (fn [e]
                           (when e
                             (let [new-spent (+ (:spent-usd e) (double cost-usd))
                                   exceeded? (>= new-spent (:max-budget-usd e))]
                               (assoc e
                                      :spent-usd new-spent
                                      :tool-calls (inc (:tool-calls e))
                                      :exceeded? exceeded?)))))]
      (when-let [entry (get updated agent-id)]
        (when (>= (:spent-usd entry) (* 0.8 (:max-budget-usd entry)))
          (log/warn "[budget-hook] Agent approaching budget limit"
                    {:agent-id agent-id
                     :spent-usd (:spent-usd entry)
                     :max-budget-usd (:max-budget-usd entry)
                     :tool tool-name
                     :pct-used (* 100.0 (/ (:spent-usd entry) (:max-budget-usd entry)))}))
        entry))))

(defn record-tool-usage!
  "Record usage for a tool call by estimating cost from tool metadata.

   Convenience wrapper around record-usage! that estimates cost
   from tool name + input + model.

   Arguments:
     agent-id  - Agent identifier string
     tool-name - String name of the tool
     input     - Tool input data

   Returns:
     Updated budget entry or nil"
  [agent-id tool-name input]
  (when-let [entry (get @*agent-budgets agent-id)]
    (let [cost (estimate-tool-call-cost tool-name input (:model entry))]
      (record-usage! agent-id cost {:tool-name tool-name}))))

;;; =============================================================================
;;; Budget Check (PreToolUse Hook)
;;; =============================================================================

(defn check-budget
  "Check if an agent is within budget for a tool call.

   Arguments:
     agent-id  - Agent identifier string
     tool-name - String name of the tool being called
     input     - Tool input data

   Returns:
     Permission result map:
     {:action :allow}                                    - Within budget
     {:action :deny :message ... :interrupt? true}       - Over budget
     nil                                                 - Agent not registered (no budget)"
  [agent-id tool-name input]
  (when-let [entry (get @*agent-budgets agent-id)]
    (let [estimated-cost (estimate-tool-call-cost tool-name input (:model entry))
          projected-spend (+ (:spent-usd entry) estimated-cost)
          max-budget (:max-budget-usd entry)]

      ;; Update last-check timestamp
      (swap! *agent-budgets assoc-in [agent-id :last-check-at] (java.time.Instant/now))

      (if (> projected-spend max-budget)
        ;; OVER BUDGET: deny + interrupt
        (do
          (swap! *agent-budgets update-in [agent-id :denied-calls] inc)
          (swap! *agent-budgets assoc-in [agent-id :exceeded?] true)
          (log/warn "[budget-hook] Budget exceeded — denying tool call"
                    {:agent-id agent-id
                     :tool tool-name
                     :spent-usd (:spent-usd entry)
                     :estimated-cost estimated-cost
                     :projected-spend projected-spend
                     :max-budget-usd max-budget})
          {:action :deny
           :message (format "Budget exceeded: $%.4f spent + $%.4f estimated = $%.4f > $%.2f max. Agent %s interrupted."
                            (:spent-usd entry)
                            estimated-cost
                            projected-spend
                            max-budget
                            agent-id)
           :interrupt? true})

        ;; WITHIN BUDGET: allow
        {:action :allow}))))

;;; =============================================================================
;;; Permission Handler Factory (Composable with permissions.clj)
;;; =============================================================================

(defn budget-guardrail-handler
  "Create a PreToolUse permission handler that enforces USD budget limits.

   Returns a handler fn compatible with permissions.clj composite-handler.
   The handler checks per-agent cumulative cost and denies+interrupts when
   the budget is exceeded.

   The handler is a no-op (returns :allow) for agents without a registered budget.

   Returns:
     (fn [tool-name input context] -> permission-result)

   Context map should contain:
     :agent-id - Agent identifier (required for budget lookup)"
  []
  (fn [tool-name input context]
    (let [agent-id (:agent-id context)]
      (if-not agent-id
        ;; No agent-id in context → can't check budget → allow (safe default)
        {:action :allow}
        (or (check-budget agent-id tool-name input)
            ;; nil = agent not registered → no budget → allow
            {:action :allow})))))

;;; =============================================================================
;;; Query / Dashboard
;;; =============================================================================

(defn get-budget-status
  "Get budget status for a specific agent.

   Arguments:
     agent-id - Agent identifier string

   Returns:
     Budget status map or nil if not registered"
  [agent-id]
  (when-let [entry (get @*agent-budgets agent-id)]
    (let [{:keys [max-budget-usd spent-usd tool-calls denied-calls
                  model registered-at exceeded?]} entry]
      {:agent-id       agent-id
       :max-budget-usd max-budget-usd
       :spent-usd      spent-usd
       :remaining-usd  (max 0.0 (- max-budget-usd spent-usd))
       :pct-used       (if (pos? max-budget-usd)
                         (* 100.0 (/ spent-usd max-budget-usd))
                         0.0)
       :tool-calls     tool-calls
       :denied-calls   denied-calls
       :model          model
       :exceeded?      exceeded?
       :registered-at  (str registered-at)})))

(defn get-all-budget-statuses
  "Get budget status for all registered agents.

   Returns:
     Vector of budget status maps"
  []
  (->> (keys @*agent-budgets)
       (map get-budget-status)
       (remove nil?)
       vec))

(defn get-total-spend
  "Get total USD spend across all registered agents.

   Returns:
     {:total-spend-usd double
      :agent-count     int
      :agents          [{:agent-id :spent-usd}]}"
  []
  (let [entries @*agent-budgets]
    {:total-spend-usd (reduce + 0.0 (map :spent-usd (vals entries)))
     :agent-count     (count entries)
     :agents          (->> entries
                           (map (fn [[id e]] {:agent-id id :spent-usd (:spent-usd e)}))
                           (sort-by :spent-usd >)
                           vec)}))

;;; =============================================================================
;;; Reset / Testing
;;; =============================================================================

(defn reset-all-budgets!
  "Clear all budget tracking. For testing only."
  []
  (reset! *agent-budgets {})
  (log/info "[budget-hook] All budgets cleared"))

(defn reset-agent-spend!
  "Reset spend for a specific agent (keep budget limit).
   For testing or manual intervention.

   Arguments:
     agent-id - Agent identifier string"
  [agent-id]
  (swap! *agent-budgets update agent-id
         (fn [e]
           (when e
             (assoc e
                    :spent-usd 0.0
                    :tool-calls 0
                    :denied-calls 0
                    :exceeded? false))))
  (log/info "[budget-hook] Agent spend reset" {:agent-id agent-id}))

(comment
  ;; === Usage Examples ===

  ;; Register budget for a ling on spawn
  ;; (register-budget! "swarm-worker-123" 2.0 {:model "claude"})

  ;; Check budget before tool call (PreToolUse hook)
  ;; (check-budget "swarm-worker-123" "read_file" {:path "/src/foo.clj"})
  ;; => {:action :allow}

  ;; Record cost after tool call (PostToolUse observer)
  ;; (record-tool-usage! "swarm-worker-123" "bash" {:command "git status"})

  ;; Create composable handler for permissions.clj
  ;; (require '[hive-mcp.agent.permissions :as perms])
  ;; (def my-handler (perms/composite-handler
  ;;                   [(perms/logging-handler)
  ;;                    (budget-guardrail-handler)]))

  ;; Check status
  ;; (get-budget-status "swarm-worker-123")
  ;; => {:agent-id "swarm-worker-123"
  ;;     :max-budget-usd 2.0
  ;;     :spent-usd 0.0342
  ;;     :remaining-usd 1.9658
  ;;     :pct-used 1.71
  ;;     :tool-calls 12
  ;;     :denied-calls 0
  ;;     :model "claude"
  ;;     :exceeded? false}

  ;; Cleanup on kill
  ;; (deregister-budget! "swarm-worker-123")
  )
