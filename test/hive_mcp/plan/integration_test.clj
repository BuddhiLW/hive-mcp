(ns hive-mcp.plan.integration-test
  "End-to-end integration tests for the DAG-Wave (exploration-to-kanban) workflow.

   Tests the FULL pipeline:
   1. Memory entry with plan (EDN or markdown)
   2. plan_to_kanban tool parses and creates tasks
   3. KG edges link plan -> tasks and task -> task dependencies
   4. Wave computation respects topological order

   Test Scenarios:
   a) EDN plan to kanban - Full cycle with dependencies
   b) Markdown plan to kanban - Alternative format support
   c) Dependency cycle detection - Graceful rejection
   d) Wave computation - Topological sort validation

   AXIOM COMPLIANCE:
   - Tests run via nREPL, NOT bash (per 'Clojure Tests Run via nREPL, Never Bash')
   - TDD as Trust Bridge - tests validate the pipeline

   Run: (require '[clojure.test :refer [run-tests]])
        (run-tests 'hive-mcp.plan.integration-test)"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.plan.tool :as tool]
            [hive-mcp.plan.schema :as schema]
            [hive-mcp.plan.parser :as parser]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.tools.memory-kanban :as mem-kanban]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:dynamic *test-memory-ids*
  "Atom to track memory entries created during tests for cleanup."
  (atom []))

(def ^:dynamic *test-task-ids*
  "Atom to track kanban tasks created during tests for cleanup."
  (atom []))

(defn cleanup-test-data!
  "Clean up memory entries and tasks created during tests."
  []
  ;; Clean up memories
  (doseq [id @*test-memory-ids*]
    (try
      (chroma/delete-entry! id)
      (catch Exception _ nil)))
  (reset! *test-memory-ids* [])

  ;; Note: kanban tasks in DataScript - reset happens via kg-conn/reset-conn!
  (reset! *test-task-ids* []))

(defn integration-fixture
  "Reset state before/after each test."
  [f]
  ;; Reset KG DataScript connection
  (kg-conn/reset-conn!)
  (reset! *test-memory-ids* [])
  (reset! *test-task-ids* [])

  (f)

  ;; Cleanup
  (cleanup-test-data!)
  (kg-conn/reset-conn!))

(use-fixtures :each integration-fixture)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn create-test-memory!
  "Create a memory entry for testing. Returns the entry ID.
   Automatically tracked for cleanup."
  [content & {:keys [type tags project-id]
              :or {type "decision"
                   tags ["test" "plan"]
                   project-id "hive-mcp-test"}}]
  (let [entry-id (str (random-uuid))]
    (chroma/index-memory-entry! {:id entry-id
                                 :content content
                                 :type type
                                 :tags tags
                                 :project-id project-id})
    (swap! *test-memory-ids* conj entry-id)
    entry-id))

(defn parse-json-result
  "Parse JSON result from MCP tool response."
  [result]
  (when-not (:isError result)
    (try
      (clojure.data.json/read-str (:text result) :key-fn keyword)
      (catch Exception _ nil))))

(defn get-kg-edges-from
  "Get KG edges originating from a node."
  [node-id]
  (kg-edges/get-edges-from node-id))

(defn get-kg-edges-to
  "Get KG edges pointing to a node."
  [node-id]
  (kg-edges/get-edges-to node-id))

;; =============================================================================
;; Test Data: EDN Plans
;; =============================================================================

(def edn-plan-simple
  "Simple EDN plan with 2 steps, linear dependency."
  "# Implementation Plan

Here's the plan:

```edn
{:id \"plan-test-simple\"
 :title \"Simple Test Plan\"
 :steps [{:id \"step-1\"
          :title \"First task\"
          :depends-on []
          :priority :high}
         {:id \"step-2\"
          :title \"Second task\"
          :depends-on [\"step-1\"]
          :priority :medium}]}
```

This plan has a linear dependency chain.")

(def edn-plan-diamond
  "Diamond-shaped dependency graph (common parallel pattern).

   step-1 (root)
     |     \\
   step-2  step-3  (Wave 2 - parallel)
     \\     |
     step-4      (Wave 3 - waits for both)"
  "# Diamond Plan

```edn
{:id \"plan-test-diamond\"
 :title \"Diamond Dependency Plan\"
 :steps [{:id \"step-1\"
          :title \"Root task\"
          :depends-on []
          :priority :high}
         {:id \"step-2\"
          :title \"Left branch\"
          :depends-on [\"step-1\"]
          :priority :medium}
         {:id \"step-3\"
          :title \"Right branch\"
          :depends-on [\"step-1\"]
          :priority :medium}
         {:id \"step-4\"
          :title \"Final merge\"
          :depends-on [\"step-2\" \"step-3\"]
          :priority :high}]}
```")

(def edn-plan-with-cycle
  "Plan with circular dependency - should be rejected."
  "# Cyclic Plan (Invalid)

```edn
{:id \"plan-test-cycle\"
 :title \"Cyclic Plan\"
 :steps [{:id \"step-a\"
          :title \"Step A\"
          :depends-on [\"step-c\"]}
         {:id \"step-b\"
          :title \"Step B\"
          :depends-on [\"step-a\"]}
         {:id \"step-c\"
          :title \"Step C\"
          :depends-on [\"step-b\"]}]}
```")

(def edn-plan-invalid-deps
  "Plan with references to non-existent steps."
  "# Invalid Deps Plan

```edn
{:id \"plan-test-invalid\"
 :title \"Invalid Deps Plan\"
 :steps [{:id \"step-1\"
          :title \"Only step\"
          :depends-on [\"step-99\"]}]}
```")

;; =============================================================================
;; Test Data: Markdown Plans
;; =============================================================================

(def markdown-plan-simple
  "# Simple Markdown Plan

## First task [id: step-1] [priority: high]

This is the first task.

## Second task [id: step-2] [depends: step-1] [priority: medium]

This depends on the first task.")

(def markdown-plan-complex
  "# Complex Markdown Plan

## Setup environment [id: setup] [priority: high]

Install dependencies and configure.

## Write schema [id: schema] [depends: setup] [priority: high] [estimate: small]

Define the data schema.

## Write parser [id: parser] [depends: schema] [priority: medium] [estimate: medium]

Implement the parser.

## Write tests [id: tests] [depends: parser] [priority: medium]

Add test coverage.")

;; =============================================================================
;; Test a) EDN Plan to Kanban
;; =============================================================================

(deftest edn-plan-to-kanban-test
  (testing "Complete EDN plan -> kanban task pipeline"
    (let [;; Create memory with EDN plan
          memory-id (create-test-memory! edn-plan-simple)

          ;; Call plan_to_kanban tool
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})
          parsed (parse-json-result result)]

      ;; Assert: Not an error
      (is (not (:isError result)) "Should not return error")

      ;; Assert: Correct number of tasks created
      (is (= 2 (:task-count parsed)) "Should create 2 tasks")
      (is (= 2 (count (:task-ids parsed))) "Should have 2 task IDs")

      ;; Assert: KG edges created
      (is (pos? (:edge-count parsed)) "Should create KG edges")

      ;; Assert: step-mapping preserved
      (is (contains? (:step-mapping parsed) "step-1"))
      (is (contains? (:step-mapping parsed) "step-2"))))

  (testing "EDN diamond dependency creates correct KG structure"
    (let [memory-id (create-test-memory! edn-plan-diamond)
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})
          parsed (parse-json-result result)]

      ;; Assert: 4 tasks created (step-1 through step-4)
      (is (= 4 (:task-count parsed)) "Diamond plan should create 4 tasks")

      ;; Assert: KG edges exist
      ;; Plan -> Task edges (4) + Task dependency edges (4: 2->1, 3->1, 4->2, 4->3)
      (is (>= (:edge-count parsed) 4) "Should have dependency edges")

      ;; Assert: step-mapping has all steps
      (let [mapping (:step-mapping parsed)]
        (is (= 4 (count mapping)) "Mapping should have 4 entries")
        (is (contains? mapping "step-1"))
        (is (contains? mapping "step-4"))))))

;; =============================================================================
;; Test b) Markdown Plan to Kanban
;; =============================================================================

(deftest markdown-plan-to-kanban-test
  (testing "Simple markdown plan converts to kanban tasks"
    (let [memory-id (create-test-memory! markdown-plan-simple)
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})
          parsed (parse-json-result result)]

      ;; Assert: Success
      (is (not (:isError result)) "Should not return error")

      ;; Assert: 2 tasks created
      (is (= 2 (:task-count parsed)) "Should create 2 tasks from markdown")))

  (testing "Complex markdown plan with chained dependencies"
    (let [memory-id (create-test-memory! markdown-plan-complex)
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})
          parsed (parse-json-result result)]

      ;; Assert: 4 tasks created
      (is (= 4 (:task-count parsed)) "Should create 4 tasks from complex markdown")

      ;; Assert: step-mapping contains our IDs
      (let [mapping (:step-mapping parsed)]
        (is (contains? mapping "setup"))
        (is (contains? mapping "schema"))
        (is (contains? mapping "parser"))
        (is (contains? mapping "tests"))))))

;; =============================================================================
;; Test c) Dependency Cycle Detection
;; =============================================================================

(deftest cycle-detection-test
  (testing "Plan with circular dependencies is rejected"
    (let [memory-id (create-test-memory! edn-plan-with-cycle)
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})]

      ;; Assert: Returns error
      (is (:isError result) "Cyclic plan should return error")

      ;; Assert: Error mentions cycle
      (is (str/includes? (str (:text result)) "Circular")
          "Error should mention circular dependency")))

  (testing "Invalid dependency references are rejected"
    (let [memory-id (create-test-memory! edn-plan-invalid-deps)
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})]

      ;; Assert: Returns error
      (is (:isError result) "Invalid deps should return error")

      ;; Assert: Error mentions invalid reference
      (is (str/includes? (str (:text result)) "Invalid dependency")
          "Error should mention invalid dependency")))

  (testing "Self-referential dependency is detected"
    (let [self-ref-plan "```edn
{:id \"plan-self-ref\"
 :title \"Self Reference\"
 :steps [{:id \"step-x\"
          :title \"Self referencing\"
          :depends-on [\"step-x\"]}]}
```"
          memory-id (create-test-memory! self-ref-plan)
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})]

      (is (:isError result) "Self-referential dependency should be rejected"))))

;; =============================================================================
;; Test d) Wave Computation
;; =============================================================================

(defn compute-waves
  "Compute execution waves from plan steps using topological sort.

   Returns: Vector of waves, each wave is a vector of step IDs that can execute in parallel.
   Example: [[\"step-1\"] [\"step-2\" \"step-3\"] [\"step-4\"]]"
  [steps]
  (let [step-ids (set (map :id steps))
        ;; Build adjacency: step-id -> set of dependents (steps that depend on it)
        dependents-of (reduce (fn [acc step]
                                (reduce (fn [acc2 dep-id]
                                          (update acc2 dep-id (fnil conj #{}) (:id step)))
                                        acc
                                        (:depends-on step)))
                              {}
                              steps)
        ;; In-degree: how many dependencies each step has
        in-degree (into {} (map (fn [step]
                                  [(:id step) (count (:depends-on step))])
                                steps))]
    (loop [remaining step-ids
           in-deg in-degree
           waves []]
      (if (empty? remaining)
        waves
        ;; Find all steps with in-degree 0 (no unsatisfied deps)
        (let [ready (filter #(zero? (get in-deg %)) remaining)]
          (if (empty? ready)
            ;; No ready steps but still remaining = cycle (shouldn't happen if validated)
            waves
            (let [wave (vec ready)
                  ;; Decrement in-degree of dependents
                  new-in-deg (reduce (fn [deg step-id]
                                       (reduce (fn [d dependent]
                                                 (update d dependent dec))
                                               deg
                                               (get dependents-of step-id #{})))
                                     in-deg
                                     wave)
                  new-remaining (remove (set wave) remaining)]
              (recur new-remaining new-in-deg (conj waves wave)))))))))

(deftest wave-computation-test
  (testing "Linear dependency produces sequential waves"
    (let [plan {:steps [{:id "step-1" :title "A" :depends-on []}
                        {:id "step-2" :title "B" :depends-on ["step-1"]}
                        {:id "step-3" :title "C" :depends-on ["step-2"]}]}
          waves (compute-waves (:steps plan))]

      ;; Assert: 3 waves (one per step)
      (is (= 3 (count waves)) "Linear chain should have 3 waves")

      ;; Assert: Correct order
      (is (= ["step-1"] (first waves)))
      (is (= ["step-2"] (second waves)))
      (is (= ["step-3"] (nth waves 2)))))

  (testing "Diamond dependency produces correct parallel waves"
    (let [plan {:steps [{:id "step-1" :title "Root" :depends-on []}
                        {:id "step-2" :title "Left" :depends-on ["step-1"]}
                        {:id "step-3" :title "Right" :depends-on ["step-1"]}
                        {:id "step-4" :title "Merge" :depends-on ["step-2" "step-3"]}]}
          waves (compute-waves (:steps plan))]

      ;; Assert: 3 waves (not 4, because step-2 and step-3 are parallel)
      (is (= 3 (count waves)) "Diamond should have 3 waves")

      ;; Assert: Wave 1 is root
      (is (= ["step-1"] (first waves)))

      ;; Assert: Wave 2 has both branches (in some order)
      (is (= #{"step-2" "step-3"} (set (second waves)))
          "Wave 2 should have both parallel branches")

      ;; Assert: Wave 3 is merge
      (is (= ["step-4"] (nth waves 2)))))

  (testing "Independent steps form a single wave"
    (let [plan {:steps [{:id "a" :title "A" :depends-on []}
                        {:id "b" :title "B" :depends-on []}
                        {:id "c" :title "C" :depends-on []}]}
          waves (compute-waves (:steps plan))]

      ;; Assert: 1 wave with all 3 steps
      (is (= 1 (count waves)) "Independent steps should be 1 wave")
      (is (= #{"a" "b" "c"} (set (first waves))))))

  (testing "Complex DAG wave computation"
    ;; DAG:
    ;;   A
    ;;  /|\
    ;; B C D
    ;; | X |
    ;; E   F
    ;;  \ /
    ;;   G
    (let [plan {:steps [{:id "A" :title "A" :depends-on []}
                        {:id "B" :title "B" :depends-on ["A"]}
                        {:id "C" :title "C" :depends-on ["A"]}
                        {:id "D" :title "D" :depends-on ["A"]}
                        {:id "E" :title "E" :depends-on ["B" "C"]}
                        {:id "F" :title "F" :depends-on ["C" "D"]}
                        {:id "G" :title "G" :depends-on ["E" "F"]}]}
          waves (compute-waves (:steps plan))]

      ;; Assert: 4 waves
      (is (= 4 (count waves)))

      ;; Assert: A is in wave 1 alone
      (is (= ["A"] (first waves)))

      ;; Assert: B, C, D are in wave 2
      (is (= #{"B" "C" "D"} (set (second waves))))

      ;; Assert: E, F are in wave 3
      (is (= #{"E" "F"} (set (nth waves 2))))

      ;; Assert: G is in wave 4 alone
      (is (= ["G"] (nth waves 3))))))

;; =============================================================================
;; Test: Edge Linking Verification
;; =============================================================================

(deftest kg-edge-linking-test
  (testing "KG edges link plan to tasks"
    (let [memory-id (create-test-memory! edn-plan-simple)
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})
          parsed (parse-json-result result)]

      ;; Assert: Success
      (is (not (:isError result)))

      ;; Assert: Edges created
      (when-not (:isError result)
        (let [edges-from-plan (get-kg-edges-from memory-id)]
          ;; Plan should have edges to its tasks
          (is (>= (count edges-from-plan) 2)
              "Plan should have edges to at least 2 tasks")))))

  (testing "Inter-task dependency edges exist"
    (let [memory-id (create-test-memory! edn-plan-simple)
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})
          parsed (parse-json-result result)]

      (when-not (:isError result)
        (let [step-2-task-id (get-in parsed [:step-mapping "step-2"])
              edges-from-step-2 (when step-2-task-id (get-kg-edges-from step-2-task-id))]
          ;; step-2 depends on step-1, so should have edge to step-1's task
          (when step-2-task-id
            (is (>= (count edges-from-step-2) 1)
                "step-2 task should have dependency edge to step-1 task")))))))

;; =============================================================================
;; Test: Error Handling
;; =============================================================================

(deftest error-handling-test
  (testing "Non-existent memory ID returns error"
    (let [result (tool/handle-plan-to-kanban {:plan_id "non-existent-id-12345"
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})]
      (is (:isError result) "Non-existent memory should return error")))

  (testing "Memory without plan structure returns error"
    (let [memory-id (create-test-memory! "This is just regular text, no plan here.")
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})]
      (is (:isError result) "Non-plan content should return error")))

  (testing "Invalid EDN in plan returns error"
    (let [memory-id (create-test-memory! "```edn\n{:broken :edn without closing\n```")
          result (tool/handle-plan-to-kanban {:plan_id memory-id
                                              :directory "/home/lages/dotfiles/gitthings/hive-mcp"})]
      (is (:isError result) "Invalid EDN should return error"))))

;; =============================================================================
;; Test: Schema Validation
;; =============================================================================

(deftest schema-validation-test
  (testing "Parsed plans validate against Malli schema"
    (let [{:keys [success plan]} (parser/parse-plan edn-plan-diamond)]
      (is success "Diamond plan should parse successfully")
      (when success
        (is (schema/valid-plan? plan) "Parsed plan should be schema-valid"))))

  (testing "Markdown plans validate against schema"
    (let [{:keys [success plan]} (parser/parse-plan markdown-plan-complex)]
      (is success "Complex markdown should parse successfully")
      (when success
        (is (schema/valid-plan? plan) "Parsed markdown plan should be schema-valid")))))

;; =============================================================================
;; Run Tests Summary (for nREPL convenience)
;; =============================================================================

(comment
  ;; Run all tests in this namespace
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.plan.integration-test)

  ;; Run specific test
  (clojure.test/test-vars [#'edn-plan-to-kanban-test])
  (clojure.test/test-vars [#'wave-computation-test])
  (clojure.test/test-vars [#'cycle-detection-test]))
