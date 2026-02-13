(ns hive-mcp.agent.budget-router-test
  "Tests for the budget-aware model routing system.

   Covers:
   - Model tier classification (classify-model-tier)
   - Cost estimation (tier-cost-per-1m, estimate-task-cost-usd, project-remaining-tasks)
   - Budget thresholds (max-tier-for-budget, tier-allowed?)
   - Core routing (select-tier, route-model)
   - Fleet management (suggest-model, recommend-budget, fleet-budget-summary)
   - Config gate: :forge.budget-routing (default false) passthrough behavior
   - Integration with hooks.budget via requiring-resolve"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.budget-router :as br]
            [hive-mcp.agent.hooks.budget :as budget]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures & Helpers
;; =============================================================================

(defn clean-budgets-fixture [f]
  (budget/reset-all-budgets!)
  (f)
  (budget/reset-all-budgets!))

(use-fixtures :each clean-budgets-fixture)

;; Macro: run body with :forge.budget-routing gate ON (no disk writes).
(defmacro with-routing-enabled [& body]
  `(with-redefs [br/enabled? (constantly true)]
     ~@body))

;; =============================================================================
;; Model Tier Classification
;; =============================================================================

(deftest classify-model-tier-test
  (testing "free tier models (ending in :free)"
    (is (= :free (br/classify-model-tier "mistralai/devstral-2512:free")))
    (is (= :free (br/classify-model-tier "google/gemma-3-4b-it:free")))
    (is (= :free (br/classify-model-tier "google/gemma-2-9b-it:free"))))

  (testing "economy tier models"
    (is (= :economy (br/classify-model-tier "deepseek/deepseek-v3.2")))
    (is (= :economy (br/classify-model-tier "deepseek/deepseek-chat")))
    (is (= :economy (br/classify-model-tier "moonshotai/kimi-k2.5")))
    (is (= :economy (br/classify-model-tier "anthropic/claude-haiku"))))

  (testing "standard tier models"
    (is (= :standard (br/classify-model-tier "anthropic/claude-sonnet")))
    (is (= :standard (br/classify-model-tier "claude")))
    (is (= :standard (br/classify-model-tier "x-ai/grok-code-fast-1"))))

  (testing "premium tier models"
    (is (= :premium (br/classify-model-tier "anthropic/claude-opus"))))

  (testing "nil defaults to standard (matches 'claude')"
    (is (= :standard (br/classify-model-tier nil))))

  (testing "unknown model defaults to standard"
    (is (= :standard (br/classify-model-tier "some-unknown-model")))))

(deftest classify-model-tier-case-insensitive-test
  (testing "classification is case insensitive"
    (is (= :economy (br/classify-model-tier "DEEPSEEK/DEEPSEEK-V3")))
    (is (= :premium (br/classify-model-tier "Anthropic/Claude-Opus")))
    (is (= :standard (br/classify-model-tier "CLAUDE")))
    (is (= :economy (br/classify-model-tier "MoonshotAI/Kimi-K2.5")))))

;; =============================================================================
;; Tier Cost Lookup
;; =============================================================================

(deftest tier-cost-per-1m-test
  (testing "each tier has correct cost"
    (is (= 0.0 (br/tier-cost-per-1m :free)))
    (is (= 0.40 (br/tier-cost-per-1m :economy)))
    (is (= 9.0 (br/tier-cost-per-1m :standard)))
    (is (= 45.0 (br/tier-cost-per-1m :premium))))

  (testing "unknown tier falls back to 9.0 (standard)"
    (is (= 9.0 (br/tier-cost-per-1m :nonexistent)))))

;; =============================================================================
;; Cost Estimation
;; =============================================================================

(deftest estimate-task-cost-usd-test
  (testing "free tier costs nothing"
    (is (= 0.0 (br/estimate-task-cost-usd :free)))
    (is (= 0.0 (br/estimate-task-cost-usd :free {:tokens 100000}))))

  (testing "economy tier with default tokens (5000)"
    (let [cost (br/estimate-task-cost-usd :economy)]
      (is (pos? cost))
      ;; 5000 * (0.40 / 1_000_000) = 0.002
      (is (< (Math/abs (- cost 0.002)) 1e-9))))

  (testing "standard tier with custom tokens"
    (let [cost (br/estimate-task-cost-usd :standard {:tokens 10000})]
      ;; 10000 * (9.0 / 1_000_000) = 0.09
      (is (< (Math/abs (- cost 0.09)) 1e-9))))

  (testing "premium tier is the most expensive"
    (is (> (br/estimate-task-cost-usd :premium)
           (br/estimate-task-cost-usd :standard)))
    (is (> (br/estimate-task-cost-usd :standard)
           (br/estimate-task-cost-usd :economy)))))

(deftest project-remaining-tasks-test
  (testing "free tier projects infinite tasks"
    (is (= Long/MAX_VALUE (br/project-remaining-tasks 10.0 :free))))

  (testing "economy tier with $1 remaining"
    (let [tasks (br/project-remaining-tasks 1.0 :economy)]
      ;; $1 / (5000 * 0.40/1e6) = $1 / $0.002 = 500
      (is (= 500 tasks))))

  (testing "zero remaining means zero tasks"
    (is (= 0 (br/project-remaining-tasks 0.0 :standard))))

  (testing "negative remaining means zero tasks"
    (is (= 0 (br/project-remaining-tasks -1.0 :standard)))))

;; =============================================================================
;; Budget Thresholds
;; =============================================================================

(deftest max-tier-for-budget-test
  (testing "0% used allows premium"
    (is (= :premium (br/max-tier-for-budget 0.0))))

  (testing "under 50% allows premium"
    (is (= :premium (br/max-tier-for-budget 25.0)))
    (is (= :premium (br/max-tier-for-budget 49.9))))

  (testing "at 50% caps at standard"
    (is (= :standard (br/max-tier-for-budget 50.0))))

  (testing "between 50-75% caps at standard"
    (is (= :standard (br/max-tier-for-budget 60.0)))
    (is (= :standard (br/max-tier-for-budget 74.9))))

  (testing "at 75% caps at economy"
    (is (= :economy (br/max-tier-for-budget 75.0))))

  (testing "between 75-90% caps at economy"
    (is (= :economy (br/max-tier-for-budget 80.0)))
    (is (= :economy (br/max-tier-for-budget 89.9))))

  (testing "at 90% caps at free"
    (is (= :free (br/max-tier-for-budget 90.0))))

  (testing "above 90% caps at free"
    (is (= :free (br/max-tier-for-budget 95.0)))
    (is (= :free (br/max-tier-for-budget 100.0)))))

(deftest tier-allowed?-test
  (testing "free always allowed"
    (is (true? (br/tier-allowed? :free :free)))
    (is (true? (br/tier-allowed? :free :economy)))
    (is (true? (br/tier-allowed? :free :premium))))

  (testing "premium only under premium ceiling"
    (is (true? (br/tier-allowed? :premium :premium)))
    (is (false? (br/tier-allowed? :premium :standard)))
    (is (false? (br/tier-allowed? :premium :economy)))
    (is (false? (br/tier-allowed? :premium :free))))

  (testing "standard allowed under standard or higher"
    (is (true? (br/tier-allowed? :standard :standard)))
    (is (true? (br/tier-allowed? :standard :premium)))
    (is (false? (br/tier-allowed? :standard :economy)))
    (is (false? (br/tier-allowed? :standard :free))))

  (testing "economy allowed under economy or higher"
    (is (true? (br/tier-allowed? :economy :economy)))
    (is (true? (br/tier-allowed? :economy :standard)))
    (is (true? (br/tier-allowed? :economy :premium)))
    (is (false? (br/tier-allowed? :economy :free)))))

;; =============================================================================
;; Core Routing: select-tier
;; =============================================================================

(deftest select-tier-no-budget-test
  (testing "nil budget status defaults to preferred tier"
    (let [result (br/select-tier nil)]
      (is (= :standard (:tier result)))
      (is (false? (:downgraded? result)))
      (is (string? (:reason result))))))

(deftest select-tier-within-budget-test
  (testing "preferred tier allowed when under budget"
    (let [result (br/select-tier {:pct-used 30.0 :exceeded? false}
                                 {:preferred-tier :standard})]
      (is (= :standard (:tier result)))
      (is (false? (:downgraded? result)))
      (is (= :premium (:max-tier result))))))

(deftest select-tier-budget-pressure-test
  (testing "downgrades when preferred exceeds ceiling"
    (let [result (br/select-tier {:pct-used 80.0 :exceeded? false}
                                 {:preferred-tier :standard})]
      (is (= :economy (:tier result)))
      (is (true? (:downgraded? result)))
      (is (= :economy (:max-tier result)))))

  (testing "downgrades premium to standard at 50%"
    (let [result (br/select-tier {:pct-used 55.0 :exceeded? false}
                                 {:preferred-tier :premium})]
      (is (= :standard (:tier result)))
      (is (true? (:downgraded? result))))))

(deftest select-tier-exhausted-test
  (testing "exhausted budget forces free tier"
    (let [result (br/select-tier {:pct-used 100.0 :exceeded? true}
                                 {:preferred-tier :premium})]
      (is (= :free (:tier result)))
      (is (true? (:downgraded? result)))
      (is (= :free (:max-tier result)))
      (is (string? (:reason result))))))

(deftest select-tier-return-shape-test
  (testing "result always has required keys"
    (doseq [status [{:pct-used 0.0 :exceeded? false}
                    {:pct-used 50.0 :exceeded? false}
                    {:pct-used 100.0 :exceeded? true}
                    nil]]
      (let [result (br/select-tier status)]
        (is (contains? result :tier))
        (is (contains? result :reason))
        (is (contains? result :max-tier))
        (is (contains? result :downgraded?))
        (is (contains? result :budget-status))
        (is (keyword? (:tier result)))
        (is (string? (:reason result)))
        (is (boolean? (:downgraded? result)))))))

;; =============================================================================
;; Core Routing: route-model
;; =============================================================================

(deftest route-model-force-test
  (with-routing-enabled
    (testing "force bypasses budget routing"
      (let [result (br/route-model "agent-1" "anthropic/claude-opus" {:force? true})]
        (is (= "anthropic/claude-opus" (:model result)))
        (is (= :premium (:tier result)))
        (is (false? (:downgraded? result)))
        (is (re-find #"(?i)force" (:reason result)))))))

(deftest route-model-no-budget-test
  (with-routing-enabled
    (testing "no budget registered means full access"
      (let [result (br/route-model "unregistered-agent" "anthropic/claude-sonnet")]
        (is (= "anthropic/claude-sonnet" (:model result)))
        (is (= :standard (:tier result)))
        (is (false? (:downgraded? result)))))))

(deftest route-model-with-budget-test
  (with-routing-enabled
    (testing "model within budget passes through"
      (budget/register-budget! "agent-budget-1" 5.0)
      (let [result (br/route-model "agent-budget-1" "deepseek/deepseek-v3.2")]
        (is (= "deepseek/deepseek-v3.2" (:model result)))
        (is (= :economy (:tier result)))
        (is (false? (:downgraded? result)))))

    (testing "model downgraded when budget is tight"
      (budget/register-budget! "agent-budget-2" 0.01)
      ;; Spend almost all budget
      (budget/record-usage! "agent-budget-2" 0.009)
      (let [result (br/route-model "agent-budget-2" "anthropic/claude-opus")]
        ;; At 90% used, max tier = :free, so opus should be downgraded
        (is (not= "anthropic/claude-opus" (:model result)))
        (is (true? (:downgraded? result)))))))

(deftest route-model-return-shape-test
  (with-routing-enabled
    (testing "result has required keys (gate ON)"
      (let [result (br/route-model "test-agent" "claude")]
        (is (contains? result :model))
        (is (contains? result :tier))
        (is (contains? result :reason))
        (is (contains? result :downgraded?))
        (is (string? (:model result)))
        (is (keyword? (:tier result)))))))

;; =============================================================================
;; Fleet Management: suggest-model
;; =============================================================================

(deftest suggest-model-default-test
  (with-routing-enabled
    (testing "suggests standard tier with default budget"
      (let [result (br/suggest-model)]
        (is (keyword? (:tier result)))
        (is (string? (:model result)))
        (is (string? (:reason result)))
        (is (number? (:projected-tasks result)))
        (is (map? (:fleet-status result)))))))

(deftest suggest-model-fresh-budget-test
  (with-routing-enabled
    (testing "fresh budget suggests standard or better"
      (let [result (br/suggest-model {:budget-usd 50.0})]
        (is (#{:standard :premium} (:tier result)))
        (is (pos? (:projected-tasks result)))))))

(deftest suggest-model-tight-budget-test
  (with-routing-enabled
    (testing "tight fleet budget downgrades tier"
      ;; Register agents that have spent most of the budget
      (budget/register-budget! "fleet-1" 5.0)
      (budget/record-usage! "fleet-1" 4.5)
      (let [result (br/suggest-model {:budget-usd 5.0
                                      :preferred-tier :premium})]
        ;; At 90% fleet usage, max-tier is :free
        (is (= :free (:tier result)))))))

(deftest suggest-model-per-agent-share-test
  (with-routing-enabled
    (testing "per-agent share divides remaining evenly"
      (let [result (br/suggest-model {:budget-usd 20.0 :active-agents 4})]
        (is (number? (:per-agent-share result)))
        ;; $20 remaining / 4 agents = $5 each
        (is (< (Math/abs (- 5.0 (:per-agent-share result))) 0.1))))))

(deftest suggest-model-return-shape-test
  (with-routing-enabled
    (testing "result has all expected keys (gate ON)"
      (let [result (br/suggest-model)]
        (is (contains? result :tier))
        (is (contains? result :model))
        (is (contains? result :reason))
        (is (contains? result :max-tier))
        (is (contains? result :projected-tasks))
        (is (contains? result :per-agent-share))
        (is (contains? result :fleet-status))
        (let [fs (:fleet-status result)]
          (is (contains? fs :spent))
          (is (contains? fs :budget))
          (is (contains? fs :remaining))
          (is (contains? fs :pct-used))
          (is (contains? fs :agents)))))))

;; =============================================================================
;; Fleet Management: recommend-budget
;; =============================================================================

(deftest recommend-budget-default-test
  (testing "recommends a positive budget"
    (let [result (br/recommend-budget)]
      (is (pos? (:recommended-budget-usd result)))
      (is (number? (:fleet-remaining-usd result)))
      (is (keyword? (:tier-at-budget result))))))

(deftest recommend-budget-custom-fleet-test
  (testing "divides fleet budget evenly"
    (let [result (br/recommend-budget {:fleet-budget-usd 100.0 :planned-agents 10})]
      ;; $100 / 10 = $10 per agent
      (is (< (Math/abs (- 10.0 (:recommended-budget-usd result))) 0.1))
      (is (= 100.0 (:fleet-budget-usd result)))
      (is (= 10 (:planned-agents result))))))

(deftest recommend-budget-after-spending-test
  (testing "accounts for existing spend"
    (budget/register-budget! "rec-agent" 10.0)
    (budget/record-usage! "rec-agent" 8.0)
    (let [result (br/recommend-budget {:fleet-budget-usd 20.0 :planned-agents 5})]
      ;; Fleet remaining = 20 - 8 = $12, / 5 = $2.40
      (is (< (:recommended-budget-usd result) 4.0))
      (is (< (:fleet-remaining-usd result) 20.0)))))

;; =============================================================================
;; Fleet Dashboard: fleet-budget-summary
;; =============================================================================

(deftest fleet-budget-summary-empty-test
  (testing "empty fleet has healthy status"
    (let [result (br/fleet-budget-summary)]
      (is (= 0 (:agent-count result)))
      (is (= :healthy (:health result)))
      (is (= 0.0 (:fleet-spent-usd result)))
      (is (number? (:fleet-budget-usd result)))
      (is (vector? (:agents result))))))

(deftest fleet-budget-summary-with-agents-test
  (testing "summary includes agent details"
    (budget/register-budget! "sum-1" 5.0)
    (budget/register-budget! "sum-2" 3.0)
    (budget/record-usage! "sum-1" 1.0)
    (let [result (br/fleet-budget-summary {:fleet-budget-usd 20.0})]
      (is (= 2 (:agent-count result)))
      (is (pos? (:fleet-spent-usd result)))
      (is (< (:fleet-pct-used result) 50.0))
      (is (= :healthy (:health result)))
      ;; Each agent has tier and projection info
      (doseq [agent (:agents result)]
        (is (contains? agent :current-max-tier))
        (is (contains? agent :projected-tasks))))))

(deftest fleet-budget-summary-health-levels-test
  (testing "health degrades with spending"
    ;; Caution: 20-50% remaining
    (budget/register-budget! "health-1" 10.0)
    (budget/record-usage! "health-1" 7.0)
    (let [result (br/fleet-budget-summary {:fleet-budget-usd 10.0})]
      (is (= :caution (:health result))))

    (budget/reset-all-budgets!)

    ;; Critical: <20% remaining
    (budget/register-budget! "health-2" 10.0)
    (budget/record-usage! "health-2" 9.0)
    (let [result (br/fleet-budget-summary {:fleet-budget-usd 10.0})]
      (is (= :critical (:health result))))

    (budget/reset-all-budgets!)

    ;; Exhausted: 0 remaining
    (budget/register-budget! "health-3" 10.0)
    (budget/record-usage! "health-3" 10.0)
    (let [result (br/fleet-budget-summary {:fleet-budget-usd 10.0})]
      (is (= :exhausted (:health result))))))

;; =============================================================================
;; Tier order consistency
;; =============================================================================

(deftest tier-order-test
  (testing "tier-order matches model-tiers keys"
    (is (= (set br/tier-order) (set (keys br/model-tiers)))))

  (testing "tier-order is cheapest-first"
    (is (= [:free :economy :standard :premium] br/tier-order))
    ;; Verify cost ordering
    (let [costs (mapv br/tier-cost-per-1m br/tier-order)]
      (is (= costs (sort costs))))))

(deftest model-tiers-structure-test
  (testing "each tier has required fields"
    (doseq [[tier data] br/model-tiers]
      (is (number? (:order data)) (str tier " missing :order"))
      (is (number? (:cost-per-1m data)) (str tier " missing :cost-per-1m"))
      (is (string? (:label data)) (str tier " missing :label"))
      (is (set? (:models data)) (str tier " missing :models"))
      (is (seq (:models data)) (str tier " has empty :models")))))

;; =============================================================================
;; Config Gate: disabled passthrough (default behavior)
;; =============================================================================

(deftest route-model-disabled-gate-test
  (testing "route-model passes through when gate OFF (default)"
    (is (false? (br/enabled?)) "gate should be OFF by default")
    (let [result (br/route-model "any-agent" "anthropic/claude-opus")]
      (is (= "anthropic/claude-opus" (:model result)))
      (is (= :premium (:tier result)))
      (is (false? (:downgraded? result)))
      (is (re-find #"(?i)disabled" (:reason result)))))

  (testing "force? flag irrelevant when gate OFF — still passthrough"
    (let [result (br/route-model "any-agent" "anthropic/claude-opus" {:force? true})]
      (is (= "anthropic/claude-opus" (:model result)))
      (is (false? (:downgraded? result)))
      (is (re-find #"(?i)disabled" (:reason result))))))

(deftest suggest-model-disabled-gate-test
  (testing "suggest-model passes through when gate OFF"
    (is (false? (br/enabled?)))
    (let [result (br/suggest-model {:model "anthropic/claude-opus"
                                    :preferred-tier :premium})]
      (is (= :premium (:tier result)))
      (is (= "anthropic/claude-opus" (:model result)))
      (is (re-find #"(?i)disabled" (:reason result)))
      (is (= Long/MAX_VALUE (:projected-tasks result)))
      (is (map? (:fleet-status result)))))

  (testing "budget spending ignored when gate OFF"
    (budget/register-budget! "gate-off-agent" 1.0)
    (budget/record-usage! "gate-off-agent" 0.99)
    ;; Pass an explicit premium model so classify-model-tier returns :premium
    (let [result (br/suggest-model {:budget-usd 1.0
                                    :model "anthropic/claude-opus"
                                    :preferred-tier :premium})]
      ;; Gate OFF → no downgrade regardless of spend
      (is (= :premium (:tier result)))
      (is (= "anthropic/claude-opus" (:model result)))
      (is (= Long/MAX_VALUE (:projected-tasks result))))))
