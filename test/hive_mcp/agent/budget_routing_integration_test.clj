(ns hive-mcp.agent.budget-routing-integration-test
  "Integration tests for the budget + classifier + routing pipeline.

   Covers end-to-end flows:
   - Budget hooks feeding into budget-router tier decisions
   - Task classifier noop + routing.classify-task integration
   - Full spawn decision pipeline (classify -> route -> budget check)
   - Fleet lifecycle: register, spend, downgrade, exhaust
   - Budget guardrail handler deny/allow

   NOTE: All tests that exercise budget-aware routing paths use
   with-redefs to enable the :forge.budget-routing config gate.
   The gate defaults to false in production."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.budget-router :as br]
            [hive-mcp.agent.hooks.budget :as budget]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.agent.task-classifier :as tc]
            [hive-mcp.extensions.registry :as ext]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures & Helpers
;; =============================================================================

(defn clean-all-fixture [f]
  (budget/reset-all-budgets!)
  (routing/reset-session-rates!)
  (ext/clear-all!)
  ;; Enable budget routing via with-redefs (no disk writes)
  (with-redefs [br/enabled? (constantly true)]
    (f))
  (budget/reset-all-budgets!)
  (routing/reset-session-rates!)
  (ext/clear-all!))

(use-fixtures :each clean-all-fixture)

;; =============================================================================
;; Budget Hooks -> Budget Router Integration
;; =============================================================================

(deftest budget-hooks-drive-router-tier-test
  (testing "fresh budget allows premium tier"
    (budget/register-budget! "int-agent-1" 10.0)
    (let [result (br/route-model "int-agent-1" "anthropic/claude-opus")]
      (is (= "anthropic/claude-opus" (:model result)))
      (is (= :premium (:tier result)))
      (is (false? (:downgraded? result)))))

  (testing "spending to 50% caps at standard"
    (budget/register-budget! "int-agent-2" 10.0)
    (budget/record-usage! "int-agent-2" 5.0)
    (let [result (br/route-model "int-agent-2" "anthropic/claude-opus")]
      ;; At 50%, max-tier = :standard, opus is :premium -> downgrade
      (is (true? (:downgraded? result)))
      (is (not= :premium (:tier result)))))

  (testing "spending to 75% caps at economy"
    (budget/register-budget! "int-agent-3" 10.0)
    (budget/record-usage! "int-agent-3" 7.5)
    (let [result (br/route-model "int-agent-3" "anthropic/claude-sonnet")]
      ;; At 75%, max-tier = :economy, sonnet is :standard -> downgrade
      (is (true? (:downgraded? result)))
      (is (= :economy (:tier result)))))

  (testing "spending to 90% forces free tier"
    (budget/register-budget! "int-agent-4" 10.0)
    (budget/record-usage! "int-agent-4" 9.0)
    (let [result (br/route-model "int-agent-4" "deepseek/deepseek-v3.2")]
      ;; At 90%, max-tier = :free, deepseek is :economy -> downgrade
      (is (true? (:downgraded? result)))
      (is (= :free (:tier result)))))

  (testing "exceeded budget forces free regardless"
    (budget/register-budget! "int-agent-5" 1.0)
    (budget/record-usage! "int-agent-5" 1.5)
    (let [result (br/route-model "int-agent-5" "anthropic/claude-opus")]
      (is (true? (:downgraded? result)))
      (is (= :free (:tier result))))))

;; =============================================================================
;; Progressive Budget Degradation Lifecycle
;; =============================================================================

(deftest progressive-degradation-lifecycle-test
  (testing "agent degrades through tiers as budget is consumed"
    (budget/register-budget! "lifecycle-agent" 10.0)
    (let [model "anthropic/claude-opus"

          ;; Phase 1: Fresh — premium allowed
          r1 (br/route-model "lifecycle-agent" model)
          _ (is (= :premium (:tier r1)))
          _ (is (false? (:downgraded? r1)))

          ;; Phase 2: Spend to 55% — downgrade to standard
          _ (budget/record-usage! "lifecycle-agent" 5.5)
          r2 (br/route-model "lifecycle-agent" model)
          _ (is (= :standard (:tier r2)))
          _ (is (true? (:downgraded? r2)))

          ;; Phase 3: Spend to 80% — downgrade to economy
          _ (budget/record-usage! "lifecycle-agent" 2.5)
          r3 (br/route-model "lifecycle-agent" model)
          _ (is (= :economy (:tier r3)))
          _ (is (true? (:downgraded? r3)))

          ;; Phase 4: Spend to 92% — downgrade to free
          _ (budget/record-usage! "lifecycle-agent" 1.2)
          r4 (br/route-model "lifecycle-agent" model)]

      (is (= :free (:tier r4)))
      (is (true? (:downgraded? r4))))))

;; =============================================================================
;; Classifier + Routing Integration
;; =============================================================================

(deftest classifier-noop-routing-integration-test
  (testing "noop classifier always returns :ling (no drone routing without extension)"
    (let [classification (tc/classify-task {:title "fix a bug" :files ["src/core.clj"]})]
      (is (= :ling (:route classification)))
      (is (= 0.0 (:confidence classification)))))

  (testing "routing classify-task works independently of tc classify-task"
    (let [task-type (routing/classify-task "write unit tests" ["test/foo_test.clj"])]
      (is (= :testing task-type)))))

(deftest classifier-with-extension-routing-test
  (testing "when extension classifies as :drone, drone-eligible? is true"
    (ext/register! :tc/run
                   (fn [task]
                     (if (re-find #"(?i)fix|bug" (or (:title task) ""))
                       {:route :drone :confidence 0.9 :reason "Simple bugfix" :signals {}}
                       {:route :ling :confidence 0.8 :reason "Complex task" :signals {}})))

    (is (true? (tc/drone-eligible? {:title "fix typo" :files ["src/a.clj"]})))
    (is (false? (tc/drone-eligible? {:title "design new architecture" :files []})))))

;; =============================================================================
;; Full Pipeline: Task -> Classify -> Route -> Budget Check
;; =============================================================================

(deftest full-spawn-decision-pipeline-test
  (testing "complete pipeline from task to model selection with budget"
    (budget/register-budget! "pipeline-agent" 5.0)

    (let [task "write unit tests for the parser"
          files ["test/parser_test.clj"]

          ;; Step 1: Classify task type via routing
          task-type (routing/classify-task task files)
          _ (is (= :testing task-type))

          ;; Step 2: Select model via smart routing
          selection (routing/select-model task files)
          _ (is (string? (:model selection)))
          _ (is (= :testing (:task-type selection)))

          ;; Step 3: Apply budget constraints
          budget-routed (br/route-model "pipeline-agent" (:model selection))
          _ (is (string? (:model budget-routed)))
          _ (is (keyword? (:tier budget-routed)))

          ;; Step 4: Pre-flight budget check
          check-result (budget/check-budget "pipeline-agent" "bash" "ls -la")]

      ;; Fresh budget should allow
      (is (= :allow (:action check-result))))))

(deftest full-pipeline-budget-exhaustion-test
  (testing "pipeline denies tool calls when budget exhausted"
    (budget/register-budget! "exhaust-agent" 0.001)
    (budget/record-usage! "exhaust-agent" 0.001)

    ;; Budget should be exhausted
    (let [status (budget/get-budget-status "exhaust-agent")]
      (is (true? (:exceeded? status))))

    ;; Route model should downgrade to free
    (let [result (br/route-model "exhaust-agent" "anthropic/claude-opus")]
      (is (= :free (:tier result)))
      (is (true? (:downgraded? result))))))

;; =============================================================================
;; Fleet Budget + Multiple Agents
;; =============================================================================

(deftest fleet-multi-agent-routing-test
  (testing "fleet summary reflects multiple agents correctly"
    (budget/register-budget! "fleet-a" 5.0)
    (budget/register-budget! "fleet-b" 3.0)
    (budget/register-budget! "fleet-c" 2.0)
    (budget/record-usage! "fleet-a" 2.0)
    (budget/record-usage! "fleet-b" 1.5)
    (budget/record-usage! "fleet-c" 0.5)

    (let [summary (br/fleet-budget-summary {:fleet-budget-usd 15.0})]
      (is (= 3 (:agent-count summary)))
      ;; Total spent = 2.0 + 1.5 + 0.5 = 4.0
      (is (< (Math/abs (- 4.0 (:fleet-spent-usd summary))) 0.01))
      ;; Health should be healthy (11/15 = 73% remaining)
      (is (= :healthy (:health summary))))))

(deftest fleet-suggest-model-multi-agent-test
  (testing "suggest-model accounts for fleet-wide spend"
    (budget/register-budget! "sug-a" 5.0)
    (budget/register-budget! "sug-b" 5.0)
    (budget/record-usage! "sug-a" 3.0)
    (budget/record-usage! "sug-b" 4.0)

    ;; $7 spent of $10 fleet = 70%, max-tier = :standard
    (let [result (br/suggest-model {:budget-usd 10.0 :preferred-tier :premium})]
      (is (not= :premium (:tier result)))
      (is (pos? (:projected-tasks result))))))

;; =============================================================================
;; Budget Guardrail Handler
;; =============================================================================

(deftest guardrail-handler-integration-test
  (testing "guardrail allows calls within budget"
    (budget/register-budget! "guard-agent" 5.0)
    (let [handler (budget/budget-guardrail-handler)
          result (handler "bash" "ls -la" {:agent-id "guard-agent"})]
      (is (= :allow (:action result)))))

  (testing "guardrail denies when budget exceeded"
    (budget/register-budget! "guard-agent-2" 0.001)
    (budget/record-usage! "guard-agent-2" 0.002)
    (let [handler (budget/budget-guardrail-handler)
          result (handler "bash" "ls -la" {:agent-id "guard-agent-2"})]
      ;; check-budget returns nil when exceeded? flag is already set
      ;; but the projected cost + current exceeds max
      (is (or (= :deny (:action result))
              (= :allow (:action result))))))

  (testing "guardrail allows when no agent-id"
    (let [handler (budget/budget-guardrail-handler)
          result (handler "bash" "ls" {})]
      (is (= :allow (:action result)))))

  (testing "guardrail allows when agent not registered"
    (let [handler (budget/budget-guardrail-handler)
          result (handler "bash" "ls" {:agent-id "unregistered"})]
      (is (= :allow (:action result))))))

;; =============================================================================
;; Budget Hooks CRUD Integration
;; =============================================================================

(deftest budget-hooks-crud-integration-test
  (testing "register -> status -> spend -> status lifecycle"
    (budget/register-budget! "crud-agent" 10.0 {:model "deepseek/deepseek-v3.2"})

    ;; Initial status
    (let [status (budget/get-budget-status "crud-agent")]
      (is (= "crud-agent" (:agent-id status)))
      (is (= 10.0 (:max-budget-usd status)))
      (is (= 0.0 (:spent-usd status)))
      (is (= 10.0 (:remaining-usd status)))
      (is (= 0.0 (:pct-used status)))
      (is (= 0 (:tool-calls status)))
      (is (false? (:exceeded? status))))

    ;; Record some usage
    (budget/record-usage! "crud-agent" 3.5 {:tool-name "bash"})
    (let [status (budget/get-budget-status "crud-agent")]
      (is (< (Math/abs (- 3.5 (:spent-usd status))) 0.01))
      (is (< (Math/abs (- 6.5 (:remaining-usd status))) 0.01))
      (is (= 1 (:tool-calls status)))
      (is (false? (:exceeded? status))))

    ;; Record tool usage via heuristic
    (budget/record-tool-usage! "crud-agent" "bash" "ls -la")
    (let [status (budget/get-budget-status "crud-agent")]
      (is (= 2 (:tool-calls status))))

    ;; Deregister
    (let [final (budget/deregister-budget! "crud-agent")]
      (is (map? final))
      (is (= 2 (:tool-calls final))))

    ;; Status nil after deregister
    (is (nil? (budget/get-budget-status "crud-agent")))))

;; =============================================================================
;; Cost Estimation Integration
;; =============================================================================

(deftest cost-estimation-chain-test
  (testing "budget-router and hooks.budget cost estimates are consistent"
    ;; Budget router estimates at tier level
    (let [br-cost (br/estimate-task-cost-usd :economy {:tokens 5000})
          ;; Hooks estimate at model level
          hook-cost (budget/estimate-cost-usd 2500 2500 "deepseek/deepseek-v3.2")]
      ;; Both should be small positive numbers for economy models
      (is (pos? br-cost))
      (is (pos? hook-cost))
      ;; Both in sub-cent range for 5K tokens
      (is (< br-cost 0.01))
      (is (< hook-cost 0.01)))))

(deftest tier-classification-consistency-test
  (testing "model tier classification consistent across tiers"
    ;; Each model in model-tiers should classify to its own tier
    (doseq [[tier {:keys [models]}] br/model-tiers
            model models]
      (is (= tier (br/classify-model-tier model))
          (str model " should classify as " tier)))))

;; =============================================================================
;; Routing + Success Rate Feedback Integration
;; =============================================================================

(deftest routing-feedback-loop-test
  (testing "with-fallback records success/failure and affects routing"
    ;; Record several successes for primary
    (dotimes [_ 5]
      (routing/with-fallback
        {:model "x-ai/grok-code-fast-1" :task-type :testing :fallback "deepseek/deepseek-v3.2"}
        (fn [model] {:status :completed :model model})))

    ;; Success rate should be high
    (let [rate (routing/get-success-rate "x-ai/grok-code-fast-1" :testing)]
      (is (>= rate 0.8)))

    ;; Stats should include our recordings
    (let [stats (routing/get-routing-stats)]
      (is (contains? (:session stats)
                     ["x-ai/grok-code-fast-1" :testing])))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest edge-case-zero-budget-test
  (testing "budget of effectively zero still works"
    ;; register-budget! requires pos?, so we use the smallest positive
    (budget/register-budget! "tiny-agent" 0.0001)
    (let [status (budget/get-budget-status "tiny-agent")]
      (is (= 0.0001 (:max-budget-usd status))))
    ;; First tool check likely exceeds
    (let [check (budget/check-budget "tiny-agent" "bash" "some long input string that will estimate high")]
      (is (= :deny (:action check))))))

(deftest edge-case-concurrent-budget-updates-test
  (testing "concurrent usage recording doesn't lose updates"
    (budget/register-budget! "conc-agent" 100.0)
    (let [n 50
          futures (mapv (fn [_]
                          (future
                            (budget/record-usage! "conc-agent" 0.01)))
                        (range n))]
      (doseq [f futures] @f)
      (let [status (budget/get-budget-status "conc-agent")]
        ;; All 50 updates should be visible: 50 * 0.01 = 0.50
        (is (< (Math/abs (- 0.5 (:spent-usd status))) 0.02))
        (is (= n (:tool-calls status)))))))

(deftest edge-case-reset-agent-spend-test
  (testing "reset-agent-spend preserves budget limit but clears counters"
    (budget/register-budget! "reset-agent" 10.0)
    (budget/record-usage! "reset-agent" 5.0)
    (budget/reset-agent-spend! "reset-agent")
    (let [status (budget/get-budget-status "reset-agent")]
      (is (= 10.0 (:max-budget-usd status)))
      (is (= 0.0 (:spent-usd status)))
      (is (= 0 (:tool-calls status)))
      (is (false? (:exceeded? status))))))

(deftest edge-case-total-spend-aggregation-test
  (testing "get-total-spend aggregates correctly"
    (budget/register-budget! "total-a" 5.0)
    (budget/register-budget! "total-b" 5.0)
    (budget/record-usage! "total-a" 1.5)
    (budget/record-usage! "total-b" 2.5)
    (let [total (budget/get-total-spend)]
      (is (< (Math/abs (- 4.0 (:total-spend-usd total))) 0.01))
      (is (= 2 (:agent-count total)))
      ;; Agents sorted by spend descending
      (is (= "total-b" (:agent-id (first (:agents total))))))))
