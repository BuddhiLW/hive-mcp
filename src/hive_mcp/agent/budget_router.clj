;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.agent.budget-router
  "Budget-aware model routing for agent spawn and task dispatch.

   Composes with agent.routing (task-type routing) and hooks.budget (USD tracking)
   to apply cost constraints on model selection. Routes agents to the cheapest
   model tier that can complete the task within budget.

   Model tiers (ascending cost):
     :free     — $0 (free-tier OpenRouter models)
     :economy  — sub-$1/M tokens (DeepSeek, Kimi, Haiku, Devstral)
     :standard — $3-15/M tokens (Sonnet, Grok)
     :premium  — $15-75/M tokens (Opus)

   Config integration:
     :services.forge.budget-routing  — enable/disable (default false)
     :services.forge.fleet-budget    — fleet-wide USD cap (default 20.0)"
  (:require [hive-mcp.config.core :as config]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))

;; ---------------------------------------------------------------------------
;; Config helpers
;; ---------------------------------------------------------------------------

(defn enabled?
  "Check whether budget routing is enabled in config.
   Reads :services.forge.budget-routing (default false)."
  []
  (boolean (config/get-service-value :forge :budget-routing :default false)))

(defn- fleet-budget-from-config
  "Read fleet budget cap from config, falling back to default-usd."
  [default-usd]
  (or (config/get-service-value :forge :fleet-budget :default nil)
      default-usd))

(def ^:private default-fleet-budget 20.0)

;; ---------------------------------------------------------------------------
;; Model tier definitions
;; ---------------------------------------------------------------------------

(def model-tiers
  "Ordered model tiers from cheapest to most expensive.
   Each tier has a representative cost-per-1M-tokens (blended input+output)
   and candidate model IDs."
  {:free     {:order          0
              :cost-per-1m    0.0
              :label          "Free tier"
              :models         #{"mistralai/devstral-2512:free"
                                "mistralai/devstral-small:free"
                                "google/gemma-3-4b-it:free"
                                "google/gemma-2-9b-it:free"}}
   :economy  {:order          1
              :cost-per-1m    0.40
              :label          "Economy (DeepSeek/Kimi/Haiku/Devstral)"
              :models         #{"deepseek/deepseek-v3.2"
                                "deepseek/deepseek-chat"
                                "moonshotai/kimi-k2.5"
                                "mistralai/devstral-small:24b"}}
   :standard {:order          2
              :cost-per-1m    9.0
              :label          "Standard (Sonnet/Grok)"
              :models         #{"anthropic/claude-sonnet"
                                "claude"
                                "x-ai/grok-code-fast-1"}}
   :premium  {:order          3
              :cost-per-1m    45.0
              :label          "Premium (Opus)"
              :models         #{"anthropic/claude-opus"}}})

(def tier-order
  "Tiers sorted cheapest-first. Useful for iteration and display."
  [:free :economy :standard :premium])

(defn classify-model-tier
  "Classify a model string into a cost tier keyword."
  [model]
  (let [m (or model "claude")]
    (cond
      (re-find #":free$" m)                        :free
      (re-find #"(?i)deepseek" m)                  :economy
      (re-find #"(?i)kimi|moonshot" m)             :economy
      (re-find #"(?i)devstral" m)                  :economy
      (re-find #"(?i)haiku" m)                     :economy
      (re-find #"(?i)opus" m)                      :premium
      (re-find #"(?i)sonnet|claude|grok" m)        :standard
      :else                                        :standard)))

(defn tier-cost-per-1m
  "Get the blended cost-per-1M tokens for a tier."
  [tier]
  (get-in model-tiers [tier :cost-per-1m] 9.0))

;; ---------------------------------------------------------------------------
;; Budget state resolution (reads from hooks.budget via requiring-resolve)
;; ---------------------------------------------------------------------------

(defn- resolve-budget-status
  "Resolve budget status for an agent via hooks.budget."
  [agent-id]
  (rescue nil
          (when-let [status-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/get-budget-status)]
            (status-fn agent-id))))

(defn- resolve-all-budget-statuses
  "Get all agent budget statuses."
  []
  (try
    (when-let [all-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/get-all-budget-statuses)]
      (all-fn))
    (catch Exception e
      (log/debug "[budget-router] Could not resolve all budgets" {:error (ex-message e)})
      [])))

(defn- resolve-total-spend
  "Get total spend across all agents."
  []
  (try
    (when-let [spend-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/get-total-spend)]
      (spend-fn))
    (catch Exception e
      (log/debug "[budget-router] Could not resolve total spend" {:error (ex-message e)})
      {:total-spend-usd 0.0 :agent-count 0 :agents []})))

;; ---------------------------------------------------------------------------
;; Cost estimation
;; ---------------------------------------------------------------------------

(def ^:const avg-task-tokens
  "Average tokens per task (input + output) for projection."
  5000)

(defn estimate-task-cost-usd
  "Estimate USD cost for a single task at a given tier."
  [tier & [{:keys [tokens] :or {tokens avg-task-tokens}}]]
  (let [cost-1m (tier-cost-per-1m tier)]
    (* tokens (/ cost-1m 1000000.0))))

(defn project-remaining-tasks
  "Estimate how many more tasks can be executed within remaining budget at a tier."
  [remaining-usd tier & [opts]]
  (let [per-task (estimate-task-cost-usd tier opts)
        clamped  (max 0.0 (double remaining-usd))]
    (if (pos? per-task)
      (long (Math/floor (/ clamped per-task)))
      Long/MAX_VALUE)))

;; ---------------------------------------------------------------------------
;; Budget thresholds
;; ---------------------------------------------------------------------------

(def ^:const budget-thresholds
  "Budget percentage thresholds that trigger tier downgrades.
   At each threshold, the router constrains to at-most that tier."
  [{:pct-used 0.0   :max-tier :premium}   ; 0-49%  → any tier allowed
   {:pct-used 50.0  :max-tier :standard}   ; 50-74% → standard or below
   {:pct-used 75.0  :max-tier :economy}    ; 75-89% → economy or below
   {:pct-used 90.0  :max-tier :free}])     ; 90%+   → free only

(defn max-tier-for-budget
  "Determine the maximum model tier allowed given budget utilization."
  [pct-used]
  (let [applicable (->> budget-thresholds
                        (filter #(<= (:pct-used %) pct-used))
                        last)]
    (or (:max-tier applicable) :premium)))

(defn tier-allowed?
  "Check if a model tier is within the budget-allowed ceiling."
  [tier max-tier]
  (let [tier-ord  (get-in model-tiers [tier :order] 2)
        max-ord   (get-in model-tiers [max-tier :order] 3)]
    (<= tier-ord max-ord)))

;; ---------------------------------------------------------------------------
;; Core routing logic
;; ---------------------------------------------------------------------------

(defn select-tier
  "Select the best tier for an agent given budget status.
   Returns {:tier :reason :max-tier :budget-status}."
  [budget-status & [{:keys [preferred-tier]}]]
  (let [preferred   (or preferred-tier :standard)
        pct-used    (or (:pct-used budget-status) 0.0)
        exceeded?   (:exceeded? budget-status)
        max-tier    (max-tier-for-budget pct-used)]
    (cond
      ;; Budget fully exhausted
      exceeded?
      {:tier          :free
       :reason        "Budget exhausted — free tier only"
       :max-tier      :free
       :downgraded?   (not= preferred :free)
       :budget-status budget-status}

      ;; Preferred tier is within budget ceiling
      (tier-allowed? preferred max-tier)
      {:tier          preferred
       :reason        (format "Within budget (%.0f%% used) — %s allowed"
                              pct-used (name preferred))
       :max-tier      max-tier
       :downgraded?   false
       :budget-status budget-status}

      ;; Preferred tier exceeds ceiling — downgrade to max allowed
      :else
      {:tier          max-tier
       :reason        (format "Budget pressure (%.0f%% used) — downgraded from %s to %s"
                              pct-used (name preferred) (name max-tier))
       :max-tier      max-tier
       :downgraded?   true
       :budget-status budget-status})))

(defn route-model
  "Route to a specific model within the selected tier.
   Prefers the requested model if it's in the allowed tier; otherwise picks
   a default from the target tier.

   When budget routing is disabled, returns the requested model as-is.

   Returns {:model :tier :reason :downgraded? :budget-status}."
  [agent-id requested-model & [{:keys [force?]}]]
  (cond
    ;; Budget routing disabled — pass through (force? irrelevant when gate off)
    (not (enabled?))
    {:model       requested-model
     :tier        (classify-model-tier requested-model)
     :reason      "Budget routing disabled"
     :downgraded? false}

    ;; Explicit override — bypass budget constraints
    force?
    {:model       requested-model
     :tier        (classify-model-tier requested-model)
     :reason      "Forced model override — budget routing bypassed"
     :downgraded? false}

    ;; Normal budget-aware routing
    :else
    (let [budget-status   (resolve-budget-status agent-id)
          requested-tier  (classify-model-tier requested-model)
          tier-selection  (select-tier budget-status {:preferred-tier requested-tier})
          target-tier     (:tier tier-selection)
          model-in-tier?  (= requested-tier target-tier)]

      (if model-in-tier?
        ;; Requested model is within budget — use it directly
        (assoc tier-selection :model requested-model)

        ;; Downgraded — pick a default model from the target tier
        (let [tier-models (get-in model-tiers [target-tier :models])
              fallback    (first tier-models)]
          (log/info "[budget-router] Model downgraded"
                    {:agent-id        agent-id
                     :requested       requested-model
                     :requested-tier  requested-tier
                     :target-tier     target-tier
                     :fallback        fallback
                     :pct-used        (:pct-used (:budget-status tier-selection))})
          (assoc tier-selection
                 :model           (or fallback requested-model)
                 :original-model  requested-model))))))

(defn suggest-model
  "Suggest the best model for a new agent spawn given a global budget constraint.

   Unlike route-model (per-agent), this considers total fleet spend and
   the number of active agents to recommend an appropriate tier for a
   new agent.

   When budget routing is disabled, returns a passthrough with the preferred
   model or default.

   Returns {:model :tier :reason :projected-tasks}."
  [& [{:keys [budget-usd model preferred-tier active-agents]}]]
  (if-not (enabled?)
    ;; Budget routing off — use requested model/tier as-is
    (let [tier (if model
                 (classify-model-tier model)
                 (or preferred-tier :standard))]
      {:tier            tier
       :model           (or model (first (get-in model-tiers [tier :models])))
       :reason          "Budget routing disabled"
       :max-tier        :premium
       :projected-tasks Long/MAX_VALUE
       :per-agent-share 0.0
       :fleet-status    {:spent 0.0 :budget 0.0 :remaining 0.0 :pct-used 0.0 :agents 0}})

    ;; Budget-aware fleet routing
    (let [total-spend     (resolve-total-spend)
          fleet-spent     (or (:total-spend-usd total-spend) 0.0)
          fleet-budget    (fleet-budget-from-config (or budget-usd default-fleet-budget))
          fleet-remaining (max 0.0 (- fleet-budget fleet-spent))
          fleet-pct       (if (pos? fleet-budget) (* 100.0 (/ fleet-spent fleet-budget)) 0.0)
          n-agents        (or active-agents (:agent-count total-spend) 1)
          per-agent-share (/ fleet-remaining (max 1 n-agents))
          max-tier        (max-tier-for-budget fleet-pct)
          preferred       (or preferred-tier :standard)
          effective-tier  (if (tier-allowed? preferred max-tier)
                            preferred
                            max-tier)
          projected       (project-remaining-tasks fleet-remaining effective-tier)
          ;; Select model: prefer requested if it fits the tier, else pick tier default
          effective-model (if (and model (= effective-tier (classify-model-tier model)))
                            model
                            (first (get-in model-tiers [effective-tier :models])))]

      (log/debug "[budget-router] Suggest model"
                 {:fleet-spent     fleet-spent
                  :fleet-remaining fleet-remaining
                  :fleet-pct       fleet-pct
                  :n-agents        n-agents
                  :per-agent-share per-agent-share
                  :effective-tier  effective-tier
                  :projected       projected})

      {:tier             effective-tier
       :model            effective-model
       :reason           (format "Fleet %.0f%% used ($%.2f/$%.2f). %s tier. ~%d tasks remaining."
                                 fleet-pct fleet-spent fleet-budget
                                 (name effective-tier) projected)
       :max-tier         max-tier
       :projected-tasks  projected
       :per-agent-share  per-agent-share
       :fleet-status     {:spent     fleet-spent
                          :budget    fleet-budget
                          :remaining fleet-remaining
                          :pct-used  fleet-pct
                          :agents    n-agents}})))

(defn recommend-budget
  "Recommend a per-agent max-budget-usd given fleet state.
   Divides remaining fleet budget evenly among planned agents."
  [& [{:keys [fleet-budget-usd planned-agents]}]]
  (let [total-spend     (resolve-total-spend)
        fleet-spent     (or (:total-spend-usd total-spend) 0.0)
        fleet-budget    (fleet-budget-from-config (or fleet-budget-usd default-fleet-budget))
        fleet-remaining (max 0.0 (- fleet-budget fleet-spent))
        n-planned       (or planned-agents 5)
        recommended     (/ fleet-remaining (max 1 n-planned))]

    {:recommended-budget-usd  (double recommended)
     :fleet-remaining-usd     fleet-remaining
     :fleet-spent-usd         fleet-spent
     :fleet-budget-usd        fleet-budget
     :planned-agents          n-planned
     :tier-at-budget          (let [tasks (project-remaining-tasks recommended :standard)]
                                (if (pos? tasks) :standard :economy))}))

;; ---------------------------------------------------------------------------
;; Fleet dashboard
;; ---------------------------------------------------------------------------

(defn fleet-budget-summary
  "Summary of budget state across all agents for monitoring.
   Returns per-agent breakdown with tier classification and projections."
  [& [{:keys [fleet-budget-usd]}]]
  (let [statuses        (resolve-all-budget-statuses)
        total           (resolve-total-spend)
        fleet-budget    (fleet-budget-from-config (or fleet-budget-usd default-fleet-budget))
        fleet-remaining (max 0.0 (- fleet-budget (or (:total-spend-usd total) 0.0)))
        agents-detail   (mapv (fn [s]
                                (let [tier (max-tier-for-budget (or (:pct-used s) 0.0))]
                                  (assoc s
                                         :current-max-tier tier
                                         :projected-tasks  (project-remaining-tasks
                                                            (or (:remaining-usd s) 0.0)
                                                            tier))))
                              statuses)]
    {:fleet-budget-usd    fleet-budget
     :fleet-spent-usd     (or (:total-spend-usd total) 0.0)
     :fleet-remaining-usd fleet-remaining
     :fleet-pct-used      (if (pos? fleet-budget)
                            (* 100.0 (/ (or (:total-spend-usd total) 0.0) fleet-budget))
                            0.0)
     :agent-count         (count statuses)
     :agents              agents-detail
     :health              (cond
                            (>= fleet-remaining (* 0.5 fleet-budget)) :healthy
                            (>= fleet-remaining (* 0.2 fleet-budget)) :caution
                            (pos? fleet-remaining)                    :critical
                            :else                                     :exhausted)}))
