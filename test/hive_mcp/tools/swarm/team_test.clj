(ns hive-mcp.tools.swarm.team-test
  "Tests for team_select tool handler.
   
   Covers:
   - All 6 task types return correct team composition
   - Context interpolation works correctly
   - Error handling for invalid task_type
   - auto_spawn returns error (not yet implemented)"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.tools.swarm.team :as team]
            [hive-mcp.tools.swarm.core :as core]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn mock-swarm-available
  "Mock hive-mcp-swarm as available for tests."
  [f]
  (with-redefs [core/swarm-addon-available? (constantly true)]
    (f)))

(use-fixtures :each mock-swarm-available)

;;; =============================================================================
;;; Helper Functions
;;; =============================================================================

(defn parse-response
  "Parse MCP response text as JSON."
  [response]
  (when (= "text" (:type response))
    (json/read-str (:text response) :key-fn keyword)))

(defn assert-team-structure
  "Assert that a team composition has all required fields."
  [result task-type]
  (is (map? result) "Result should be a map")
  (is (= (name task-type) (name (:task-type result))) "Task type should match")
  (is (string? (:team-pattern result)) "Should have team-pattern string")
  (is (vector? (:lings result)) "Should have lings vector")
  (is (pos? (count (:lings result))) "Should have at least one ling")
  (is (map? (:parallelization result)) "Should have parallelization map")
  (is (vector? (:coordinator-checklist result)) "Should have coordinator checklist"))

(defn assert-ling-structure
  "Assert that a ling spec has required fields."
  [ling]
  (is (string? (:name ling)) "Ling should have name")
  (is (vector? (:presets ling)) "Ling should have presets vector")
  (is (pos-int? (:sequence ling)) "Ling should have positive sequence number"))

;;; =============================================================================
;;; Implementation Task Type Tests
;;; =============================================================================

(deftest test-team-select-implementation
  (testing "implementation task type returns Explorer → Implementers team"
    (let [response (team/handle-team-select {:task_type "implementation"})
          result (parse-response response)]
      (assert-team-structure result :implementation)
      (is (= "Explorer → Implementers" (:team-pattern result)))
      (is (= 3 (count (:lings result))) "Implementation needs 3 lings")
      ;; Check explorer has planner-nih preset
      (let [explorer (first (filter #(str/includes? (:name %) "explore") (:lings result)))]
        (is (some? explorer) "Should have explorer ling")
        (is (some #{"planner-nih"} (:presets explorer)) "Explorer should have planner-nih preset"))
      ;; Check parallelization phases
      (is (= 1 (count (:phase-1 (:parallelization result)))) "Phase 1 should have 1 ling")
      (is (= 2 (count (:phase-2 (:parallelization result)))) "Phase 2 should have 2 lings"))))

;;; =============================================================================
;;; Refactoring Task Type Tests
;;; =============================================================================

(deftest test-team-select-refactoring
  (testing "refactoring task type returns Analyzer → Refactorer → Verifier team"
    (let [response (team/handle-team-select {:task_type "refactoring"})
          result (parse-response response)]
      (assert-team-structure result :refactoring)
      (is (= "Analyzer → Refactorer → Verifier" (:team-pattern result)))
      (is (= 3 (count (:lings result))) "Refactoring needs 3 lings")
      ;; Should have 3 phases
      (is (contains? (:parallelization result) :phase-1))
      (is (contains? (:parallelization result) :phase-2))
      (is (contains? (:parallelization result) :phase-3)))))

;;; =============================================================================
;;; Greenfield Task Type Tests
;;; =============================================================================

(deftest test-team-select-greenfield
  (testing "greenfield task type returns Architect → Scaffolder → Implementers team"
    (let [response (team/handle-team-select {:task_type "greenfield"})
          result (parse-response response)]
      (assert-team-structure result :greenfield)
      (is (= "Architect → Scaffolder → Implementers" (:team-pattern result)))
      (is (= 4 (count (:lings result))) "Greenfield needs 4 lings")
      ;; Check architect has hivemind preset
      (let [architect (first (filter #(str/includes? (:name %) "architect") (:lings result)))]
        (is (some? architect) "Should have architect ling")
        (is (some #{"hivemind"} (:presets architect)) "Architect should have hivemind preset")))))

;;; =============================================================================
;;; Simplification Task Type Tests
;;; =============================================================================

(deftest test-team-select-simplification
  (testing "simplification task type returns Metrics → Simplifier → Validator team"
    (let [response (team/handle-team-select {:task_type "simplification"})
          result (parse-response response)]
      (assert-team-structure result :simplification)
      (is (= "Metrics → Simplifier → Validator" (:team-pattern result)))
      (is (= 3 (count (:lings result))) "Simplification needs 3 lings"))))

;;; =============================================================================
;;; Quality Review Task Type Tests
;;; =============================================================================

(deftest test-team-select-quality-review
  (testing "quality-review task type returns Multi-Specialist Review Panel"
    (let [response (team/handle-team-select {:task_type "quality-review"})
          result (parse-response response)]
      (assert-team-structure result :quality-review)
      (is (= "Multi-Specialist Review Panel" (:team-pattern result)))
      (is (= 3 (count (:lings result))) "Quality review needs 3 specialists")
      ;; All reviewers should run in parallel (same phase)
      (is (= 3 (count (:phase-1 (:parallelization result)))) "All reviewers in phase 1"))))

;;; =============================================================================
;;; Documentation Task Type Tests
;;; =============================================================================

(deftest test-team-select-documentation
  (testing "documentation task type returns Reader → Documenter team"
    (let [response (team/handle-team-select {:task_type "documentation"})
          result (parse-response response)]
      (assert-team-structure result :documentation)
      (is (= "Reader → Documenter" (:team-pattern result)))
      (is (= 2 (count (:lings result))) "Documentation needs 2 lings")
      ;; Check documenter has documenter preset
      (let [documenter (first (filter #(str/includes? (:name %) "documenter") (:lings result)))]
        (is (some? documenter) "Should have documenter ling")
        (is (some #{"documenter"} (:presets documenter)) "Documenter should have documenter preset")))))

;;; =============================================================================
;;; Context Interpolation Tests
;;; =============================================================================

(deftest test-team-select-with-context
  (testing "context parameter interpolates into ling names"
    (let [response (team/handle-team-select {:task_type "implementation"
                                             :context "auth-feature"})
          result (parse-response response)]
      (assert-team-structure result :implementation)
      ;; Check names include context
      (is (some #(str/includes? (:name %) "auth-feature") (:lings result))
          "Ling names should include context")
      ;; Check parallelization includes context
      (is (some #(str/includes? % "auth-feature")
                (flatten (vals (:parallelization result))))
          "Parallelization should include context"))))

(deftest test-team-select-without-context
  (testing "without context, names have no placeholder artifacts"
    (let [response (team/handle-team-select {:task_type "implementation"})
          result (parse-response response)]
      (doseq [ling (:lings result)]
        (is (not (str/includes? (:name ling) "{context}"))
            "Names should not contain {context} placeholder")))))

;;; =============================================================================
;;; Error Handling Tests
;;; =============================================================================

(deftest test-team-select-invalid-task-type
  (testing "invalid task_type returns error"
    (let [response (team/handle-team-select {:task_type "invalid-type"})
          text (:text response)]
      (is (= "text" (:type response)))
      (is (str/includes? text "Invalid task_type"))
      (is (str/includes? text "implementation")
          "Error should mention valid types"))))

(deftest test-team-select-auto-spawn-not-implemented
  (testing "auto_spawn returns not-implemented error"
    (let [response (team/handle-team-select {:task_type "implementation"
                                             :auto_spawn true})
          text (:text response)]
      (is (= "text" (:type response)))
      (is (str/includes? text "not yet implemented")))))

;;; =============================================================================
;;; Ling Structure Validation Tests
;;; =============================================================================

(deftest test-all-lings-have-valid-structure
  (testing "all task types produce lings with valid structure"
    (doseq [task-type ["implementation" "refactoring" "greenfield"
                       "simplification" "quality-review" "documentation"]]
      (let [response (team/handle-team-select {:task_type task-type})
            result (parse-response response)]
        (doseq [ling (:lings result)]
          (assert-ling-structure ling))))))

;;; =============================================================================
;;; Keyword vs String Task Type Tests
;;; =============================================================================

(deftest test-team-select-keyword-task-type
  (testing "task_type works as keyword"
    (let [response (team/handle-team-select {:task_type :implementation})
          result (parse-response response)]
      (assert-team-structure result :implementation))))
