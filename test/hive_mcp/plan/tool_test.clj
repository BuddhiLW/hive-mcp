(ns hive-mcp.plan.tool-test
  "Tests for plan-to-kanban tool.

   Tests are designed to run via nREPL (not bash) per project axiom:
   'Clojure Tests Run via nREPL, Never Bash'

   Run: (require '[clojure.test :refer [run-tests]])
        (run-tests 'hive-mcp.plan.tool-test)"
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.plan.tool :as sut]
            [hive-mcp.plan.schema :as schema]
            [hive-mcp.plan.parser :as parser]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Integration with Schema Module Tests
;;; =============================================================================

(deftest schema-integration-test
  (testing "Tool uses schema for validation"
    (let [valid-step {:id "step-1" :title "Valid task" :depends-on [] :priority :high}]
      (is (schema/valid-step? (schema/normalize-step valid-step)))))

  (testing "Tool uses schema for dependency validation"
    (let [plan {:id "test-plan"
                :title "Test"
                :steps [{:id "step-1" :title "First" :depends-on []}
                        {:id "step-2" :title "Second" :depends-on ["step-1"]}]}
          result (schema/validate-dependencies plan)]
      (is (:valid result))))

  (testing "Invalid dependency detected"
    (let [plan {:id "test-plan"
                :title "Test"
                :steps [{:id "step-1" :title "First" :depends-on ["step-99"]}]}
          result (schema/validate-dependencies plan)]
      (is (not (:valid result)))
      (is (= "step-99" (get-in result [:invalid-refs 0 :missing-dep]))))))

;;; =============================================================================
;;; Integration with Parser Module Tests
;;; =============================================================================

(deftest parser-integration-test
  (testing "Parser handles EDN blocks in content"
    (let [content "# My Plan

```edn
{:steps [{:id \"step-1\" :title \"First task\" :depends-on [] :priority :high}
         {:id \"step-2\" :title \"Second task\" :depends-on [\"step-1\"] :priority :medium}]}
```

Some additional context here."
          result (parser/parse-plan content)]
      (is (:success result))
      (is (= 2 (count (get-in result [:plan :steps]))))
      (is (= :edn (get-in result [:plan :source-format])))))

  (testing "Parser handles markdown headers"
    (let [content "# Implementation Plan

## First task [priority: high]

Some details about the first task.

## Second task [depends: first-task-1] [priority: medium]

Some details about the second task."
          result (parser/parse-plan content)]
      (is (:success result))
      (is (= 2 (count (get-in result [:plan :steps]))))
      (is (= :markdown (get-in result [:plan :source-format])))))

  (testing "Parser converts steps to task specs"
    (let [plan {:id "test"
                :title "Test Plan"
                :steps [{:id "step-1" :title "Task" :priority :high :depends-on []}]}
          specs (parser/plan->task-specs plan)]
      (is (= 1 (count specs)))
      (is (= "Task" (:title (first specs))))
      (is (= "high" (:priority (first specs)))))))

;;; =============================================================================
;;; Circular Dependency Detection Tests
;;; =============================================================================

(deftest circular-dependency-detection-test
  (testing "No circular dependency"
    (let [plan {:id "test"
                :title "Test"
                :steps [{:id "step-1" :title "A" :depends-on []}
                        {:id "step-2" :title "B" :depends-on ["step-1"]}
                        {:id "step-3" :title "C" :depends-on ["step-2"]}]}
          result (schema/detect-cycles plan)]
      (is (:valid result))))

  (testing "Self-referential dependency detected"
    (let [plan {:id "test"
                :title "Test"
                :steps [{:id "step-1" :title "A" :depends-on ["step-1"]}]}
          result (schema/detect-cycles plan)]
      (is (not (:valid result)))))

  (testing "Two-node cycle detected"
    (let [plan {:id "test"
                :title "Test"
                :steps [{:id "step-1" :title "A" :depends-on ["step-2"]}
                        {:id "step-2" :title "B" :depends-on ["step-1"]}]}
          result (schema/detect-cycles plan)]
      (is (not (:valid result)))))

  (testing "Three-node cycle detected"
    (let [plan {:id "test"
                :title "Test"
                :steps [{:id "step-1" :title "A" :depends-on ["step-3"]}
                        {:id "step-2" :title "B" :depends-on ["step-1"]}
                        {:id "step-3" :title "C" :depends-on ["step-2"]}]}
          result (schema/detect-cycles plan)]
      (is (not (:valid result))))))

;;; =============================================================================
;;; Priority Normalization Tests
;;; =============================================================================

(deftest priority-normalization-test
  (testing "Keyword priorities preserved"
    (is (= :high (schema/normalize-priority :high)))
    (is (= :medium (schema/normalize-priority :medium)))
    (is (= :low (schema/normalize-priority :low))))

  (testing "String priorities converted to keywords"
    (is (= :high (schema/normalize-priority "high")))
    (is (= :medium (schema/normalize-priority "MEDIUM")))
    (is (= :low (schema/normalize-priority "Low"))))

  (testing "Invalid priorities default to medium"
    (is (= :medium (schema/normalize-priority nil)))
    (is (= :medium (schema/normalize-priority "invalid")))))

;;; =============================================================================
;;; Tool Definition Tests
;;; =============================================================================

(deftest tool-definition-test
  (testing "Tool is defined"
    (is (= 1 (count sut/tools))))

  (testing "Tool has required fields"
    (let [tool (first sut/tools)]
      (is (= "plan_to_kanban" (:name tool)))
      (is (string? (:description tool)))
      (is (map? (:inputSchema tool)))
      (is (fn? (:handler tool)))))

  (testing "Tool schema requires plan_id"
    (let [tool (first sut/tools)
          required (get-in tool [:inputSchema :required])]
      (is (some #{"plan_id"} required))))

  (testing "Tool schema has optional parameters"
    (let [tool (first sut/tools)
          properties (get-in tool [:inputSchema :properties])]
      (is (contains? properties "directory"))
      (is (contains? properties "auto_assign")))))

;;; =============================================================================
;;; Topological Sort / Wave Computation Tests
;;; =============================================================================

(deftest compute-waves-test
  (testing "Linear dependency chain"
    (let [steps [{:id "A" :title "First" :depends-on []}
                 {:id "B" :title "Second" :depends-on ["A"]}
                 {:id "C" :title "Third" :depends-on ["B"]}]
          waves (sut/compute-waves steps)]
      (is (= {"A" 0, "B" 1, "C" 2} waves))))

  (testing "Diamond pattern"
    (let [steps [{:id "A" :title "Root" :depends-on []}
                 {:id "B" :title "Left" :depends-on ["A"]}
                 {:id "C" :title "Right" :depends-on ["A"]}
                 {:id "D" :title "Tip" :depends-on ["B" "C"]}]
          waves (sut/compute-waves steps)]
      (is (= {"A" 0, "B" 1, "C" 1, "D" 2} waves))))

  (testing "Multiple independent roots"
    (let [steps [{:id "A" :title "Root1" :depends-on []}
                 {:id "B" :title "Root2" :depends-on []}
                 {:id "C" :title "Merge" :depends-on ["A" "B"]}]
          waves (sut/compute-waves steps)]
      (is (= {"A" 0, "B" 0, "C" 1} waves))))

  (testing "All independent tasks"
    (let [steps [{:id "A" :title "One" :depends-on []}
                 {:id "B" :title "Two" :depends-on []}
                 {:id "C" :title "Three" :depends-on []}]
          waves (sut/compute-waves steps)]
      (is (= {"A" 0, "B" 0, "C" 0} waves))))

  (testing "Empty steps"
    (let [waves (sut/compute-waves [])]
      (is (= {} waves))))

  (testing "Complex DAG with multiple levels"
    ;; A and B are roots (wave 0)
    ;; C depends on A (wave 1)
    ;; D depends on B (wave 1)
    ;; E depends on C and D (wave 2)
    ;; F depends on E (wave 3)
    (let [steps [{:id "A" :title "A" :depends-on []}
                 {:id "B" :title "B" :depends-on []}
                 {:id "C" :title "C" :depends-on ["A"]}
                 {:id "D" :title "D" :depends-on ["B"]}
                 {:id "E" :title "E" :depends-on ["C" "D"]}
                 {:id "F" :title "F" :depends-on ["E"]}]
          waves (sut/compute-waves steps)]
      (is (= {"A" 0, "B" 0, "C" 1, "D" 1, "E" 2, "F" 3} waves)))))

;;; =============================================================================
;;; End-to-End Parsing Tests (Unit level, no side effects)
;;; =============================================================================

(deftest end-to-end-parsing-test
  (testing "Full EDN plan parses and validates"
    (let [content "```edn
{:id \"plan-test\"
 :title \"Test Plan\"
 :steps [{:id \"step-1\" :title \"First\" :depends-on [] :priority :high}
         {:id \"step-2\" :title \"Second\" :depends-on [\"step-1\"]}]}
```"
          result (parser/parse-plan content)]
      (is (:success result))
      (let [plan (:plan result)]
        (is (schema/valid-plan? plan))
        (is (:valid (schema/validate-dependencies plan)))
        (is (:valid (schema/detect-cycles plan))))))

  (testing "Full markdown plan parses and validates"
    (let [content "# My Plan

## Step 1: First task [priority: high]

## Step 2: Second task [depends: step-1-first-task-1]"
          result (parser/parse-plan content)]
      (is (:success result))
      (is (= 2 (count (get-in result [:plan :steps])))))))
