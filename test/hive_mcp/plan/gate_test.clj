(ns hive-mcp.plan.gate-test
  "Tests for plan gate (FSM validation on memory write).

   Tests are designed to run via nREPL (not bash) per project axiom:
   'Clojure Tests Run via nREPL, Never Bash'

   Run: (require '[clojure.test :refer [run-tests]])
        (run-tests 'hive-mcp.plan.gate-test)"
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.plan.gate :as sut]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; plan-content? Detection Tests
;; =============================================================================

(deftest plan-content?-test
  (testing "detects EDN plan content"
    (is (sut/plan-content? "{:steps [{:id \"s1\" :title \"Do thing\"}]}"))
    (is (sut/plan-content? "{:plan/steps [{:step/id \"s1\"}]}"))
    (is (sut/plan-content? "```edn\n{:steps [{:id \"s1\" :title \"X\"}]}\n```")))

  (testing "detects markdown plan content (2+ headers)"
    (is (sut/plan-content? "## Step 1\nDo A\n## Step 2\nDo B")))

  (testing "rejects non-plan content"
    (is (not (sut/plan-content? "Just a note about the plan")))
    (is (not (sut/plan-content? "FRICTION: tool X returned unexpected result")))
    (is (not (sut/plan-content? "")))
    (is (not (sut/plan-content? nil))))

  (testing "rejects single-header markdown (not enough for a plan)"
    (is (not (sut/plan-content? "## Only One Section\nSome notes")))))

;; =============================================================================
;; validate-for-storage Tests - Valid Plans
;; =============================================================================

(deftest validate-valid-edn-plan-test
  (testing "valid minimal EDN plan passes gate"
    (let [content "{:id \"plan-test\" :title \"Test Plan\" :steps [{:id \"step-1\" :title \"Do thing\"}]}"
          result (sut/validate-for-storage content)]
      (is (:valid? result))
      (is (= 1 (get-in result [:metadata :steps-count])))
      (is (= :edn (get-in result [:metadata :source-format])))
      (is (false? (get-in result [:metadata :has-dependencies?])))))

  (testing "valid EDN plan with dependencies passes gate"
    (let [content "{:id \"plan-deps\" :title \"Dep Plan\"
                    :steps [{:id \"step-1\" :title \"First\" :depends-on []}
                            {:id \"step-2\" :title \"Second\" :depends-on [\"step-1\"]}]}"
          result (sut/validate-for-storage content)]
      (is (:valid? result))
      (is (= 2 (get-in result [:metadata :steps-count])))
      (is (true? (get-in result [:metadata :has-dependencies?])))))

  (testing "valid plan in code block passes gate"
    (let [content "# My Plan\n\n```edn\n{:id \"plan-cb\" :title \"Block Plan\" :steps [{:id \"s1\" :title \"Task 1\"}]}\n```"
          result (sut/validate-for-storage content)]
      (is (:valid? result))
      (is (= 1 (get-in result [:metadata :steps-count])))))

  (testing "valid plan with namespaced keys passes gate"
    (let [content "{:plan/id \"plan-ns\" :plan/title \"NS Plan\"
                    :plan/steps [{:step/id \"s1\" :step/title \"Thing\" :step/depends-on []}]}"
          result (sut/validate-for-storage content)]
      (is (:valid? result))
      (is (= 1 (get-in result [:metadata :steps-count]))))))

;; =============================================================================
;; validate-for-storage Tests - Invalid Plans
;; =============================================================================

(deftest validate-invalid-plan-test
  (testing "plan with no steps fails gate"
    (let [content "{:id \"plan-empty\" :title \"Empty\" :steps []}"
          result (sut/validate-for-storage content)]
      (is (not (:valid? result)))
      (is (seq (:errors result)))
      (is (string? (:hint result)))))

  (testing "plan with invalid dependency refs fails gate"
    (let [content "{:id \"plan-bad-dep\" :title \"Bad Deps\"
                    :steps [{:id \"step-1\" :title \"First\" :depends-on [\"step-99\"]}]}"
          result (sut/validate-for-storage content)]
      (is (not (:valid? result)))
      (is (= :dependencies (:phase result)))
      (is (some #(re-find #"step-99" %) (:errors result)))))

  (testing "unparseable content fails gate"
    (let [content "This is not a plan at all, just random text with {broken edn"
          result (sut/validate-for-storage content)]
      (is (not (:valid? result)))
      (is (= :parse (:phase result)))
      (is (string? (:hint result))))))

;; =============================================================================
;; validate-for-storage Tests - Markdown Plans
;; =============================================================================

(deftest validate-markdown-plan-test
  (testing "valid markdown plan passes gate"
    (let [content "# My Plan\n\n## Step 1: Setup\nConfigure things\n\n## Step 2: Build\nBuild the thing"
          result (sut/validate-for-storage content)]
      (is (:valid? result))
      (is (= 2 (get-in result [:metadata :steps-count])))
      (is (= :markdown (get-in result [:metadata :source-format]))))))

;; =============================================================================
;; format-gate-error Tests
;; =============================================================================

(deftest format-gate-error-test
  (testing "formats error with hint and contract"
    (let [gate-result {:valid? false
                       :errors ["Schema: missing :title"]
                       :hint "Fix the schema"
                       :phase :schema}
          formatted (sut/format-gate-error gate-result)]
      (is (string? formatted))
      (is (re-find #"Schema: missing :title" formatted))
      (is (re-find #"(?i)plan-to-kanban contract" formatted))
      (is (re-find #"phase: schema" formatted)))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest edge-case-test
  (testing "nil content returns parse error"
    (let [result (sut/validate-for-storage nil)]
      (is (not (:valid? result)))))

  (testing "empty string returns parse error"
    (let [result (sut/validate-for-storage "")]
      (is (not (:valid? result)))))

  (testing "plan with decision-id passes through metadata"
    (let [content "{:id \"plan-dec\" :title \"Dec Plan\" :decision-id \"dec-123\"
                    :steps [{:id \"s1\" :title \"Step\"}]}"
          result (sut/validate-for-storage content)]
      (is (:valid? result))
      (is (= "dec-123" (get-in result [:metadata :decision-id]))))))
