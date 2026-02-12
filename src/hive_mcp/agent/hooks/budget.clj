(ns hive-mcp.agent.hooks.budget
  "Budget guardrail hook for agent sessions. Tracks cumulative USD cost per agent and denies tool calls when exceeded."
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const chars-per-token
  "Approximation: 4 characters per token."
  4)

(def ^:const default-max-budget-usd
  "Default per-agent budget in USD."
  5.0)

(def model-pricing
  "USD cost per 1M tokens by model."
  {:claude-sonnet   {:input 3.0   :output 15.0}
   :claude-haiku    {:input 0.25  :output 1.25}
   :claude-opus     {:input 15.0  :output 75.0}
   :free-tier       {:input 0.0   :output 0.0}
   :deepseek        {:input 0.14  :output 0.28}
   :kimi            {:input 0.20  :output 0.60}
   :default         {:input 3.0   :output 15.0}})

(defn- resolve-model-pricing
  "Resolve pricing for a model identifier string."
  [model]
  (let [m (or model "claude")]
    (cond
      (re-find #":free$" m)
      (:free-tier model-pricing)

      (re-find #"(?i)haiku" m)
      (:claude-haiku model-pricing)

      (re-find #"(?i)opus" m)
      (:claude-opus model-pricing)

      (re-find #"(?i)sonnet|claude" m)
      (:claude-sonnet model-pricing)

      (re-find #"(?i)deepseek" m)
      (:deepseek model-pricing)

      (re-find #"(?i)kimi|moonshot" m)
      (:kimi model-pricing)

      :else
      (:default model-pricing))))

(defn estimate-cost-usd
  "Estimate USD cost for given input/output token counts."
  [input-tokens output-tokens & [model]]
  (let [pricing (resolve-model-pricing model)
        input-cost  (* (or input-tokens 0) (/ (:input pricing) 1000000.0))
        output-cost (* (or output-tokens 0) (/ (:output pricing) 1000000.0))]
    (+ input-cost output-cost)))

(defn estimate-tool-call-cost
  "Estimate the cost of a single tool call using heuristic token counts."
  [tool-name input & [model]]
  (let [input-chars (+ (count (or tool-name ""))
                       (count (str input)))
        input-tokens (max 100 (int (Math/ceil (/ input-chars chars-per-token))))
        output-tokens (case tool-name
                        ("read_file" "grep" "glob_files" "cider_doc" "cider_info")
                        1000
                        ("file_write" "bash" "magit_commit")
                        500
                        500)]
    (estimate-cost-usd input-tokens output-tokens model)))

(defonce ^:private *agent-budgets
  (atom {}))

(defn register-budget!
  "Register a budget for an agent session."
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
  "Remove budget tracking for an agent."
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
  "Record USD cost for a completed tool call."
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
  "Record usage for a tool call by estimating cost from tool metadata."
  [agent-id tool-name input]
  (when-let [entry (get @*agent-budgets agent-id)]
    (let [cost (estimate-tool-call-cost tool-name input (:model entry))]
      (record-usage! agent-id cost {:tool-name tool-name}))))

(defn check-budget
  "Check if an agent is within budget for a tool call."
  [agent-id tool-name input]
  (when-let [entry (get @*agent-budgets agent-id)]
    (let [estimated-cost (estimate-tool-call-cost tool-name input (:model entry))
          projected-spend (+ (:spent-usd entry) estimated-cost)
          max-budget (:max-budget-usd entry)]

      (swap! *agent-budgets assoc-in [agent-id :last-check-at] (java.time.Instant/now))

      (if (> projected-spend max-budget)
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

        {:action :allow}))))

(defn budget-guardrail-handler
  "Create a PreToolUse permission handler that enforces USD budget limits."
  []
  (fn [tool-name input context]
    (let [agent-id (:agent-id context)]
      (if-not agent-id
        {:action :allow}
        (or (check-budget agent-id tool-name input)
            {:action :allow})))))

(defn get-budget-status
  "Get budget status for a specific agent."
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
  "Get budget status for all registered agents."
  []
  (->> (keys @*agent-budgets)
       (map get-budget-status)
       (remove nil?)
       vec))

(defn get-total-spend
  "Get total USD spend across all registered agents."
  []
  (let [entries @*agent-budgets]
    {:total-spend-usd (reduce + 0.0 (map :spent-usd (vals entries)))
     :agent-count     (count entries)
     :agents          (->> entries
                           (map (fn [[id e]] {:agent-id id :spent-usd (:spent-usd e)}))
                           (sort-by :spent-usd >)
                           vec)}))

(defn reset-all-budgets!
  "Clear all budget tracking."
  []
  (reset! *agent-budgets {})
  (log/info "[budget-hook] All budgets cleared"))

(defn reset-agent-spend!
  "Reset spend for a specific agent, keeping the budget limit."
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
