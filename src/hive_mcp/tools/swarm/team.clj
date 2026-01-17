(ns hive-mcp.tools.swarm.team
  "Team selection handler for automated swarm composition.

   ADR: Memory ID 20260116131527-5222f530
   Convention: Memory ID 20260116131031-76b1a490 (team compositions)

   Provides preset team patterns for common task types:
   - :implementation - Explorer → Implementers
   - :refactoring - Analyzer → Refactorer → Verifier
   - :greenfield - Architect → Scaffolder → Implementers
   - :simplification - Metrics → Simplifier → Validator
   - :quality-review - Multi-Specialist Review Panel
   - :documentation - Reader → Documenter

   SOLID: SRP - Single responsibility for team composition
   CLARITY: R - Represented intent via typed task selection"
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.specs.agent :as specs]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Team Composition Data
;; ============================================================

(def ^:private team-compositions
  "Pre-cooked team patterns by task type.
   Source: Memory convention 20260116131031-76b1a490"
  {:implementation
   {:team-pattern "Explorer → Implementers"
    :lings [{:name "explore-{context}" :presets ["planner-nih" "clarity"] :sequence 1}
            {:name "impl-{context}-1" :presets ["ling" "tdd" "clarity"] :sequence 2}
            {:name "impl-{context}-2" :presets ["ling" "tdd" "clarity"] :sequence 2}]
    :parallelization {:phase-1 ["explore-{context}"]
                      :phase-2 ["impl-{context}-1" "impl-{context}-2"]}
    :coordinator-checklist ["Wait for explorer NIH audit"
                            "Dispatch impl lings with non-overlapping files"
                            "Run integration tests after completion"]}

   :refactoring
   {:team-pattern "Analyzer → Refactorer → Verifier"
    :lings [{:name "analyze-{context}" :presets ["analyzer" "clarity"] :sequence 1}
            {:name "refactor-{context}-1" :presets ["ling" "refactorer" "clarity"] :sequence 2}
            {:name "verify-{context}" :presets ["verifier" "tdd"] :sequence 3}]
    :parallelization {:phase-1 ["analyze-{context}"]
                      :phase-2 ["refactor-{context}-1"]
                      :phase-3 ["verify-{context}"]}
    :coordinator-checklist ["Analyzer uses scc_hotspots"
                            "Refactorer maintains behavior"
                            "Verifier confirms no regression"]}

   :greenfield
   {:team-pattern "Architect → Scaffolder → Implementers"
    :lings [{:name "architect-{context}" :presets ["architect" "clarity"] :sequence 1}
            {:name "scaffold-{context}" :presets ["ling" "tdd"] :sequence 2}
            {:name "impl-{context}-1" :presets ["ling" "tdd" "clarity"] :sequence 3}
            {:name "impl-{context}-2" :presets ["ling" "tdd" "clarity"] :sequence 3}]
    :parallelization {:phase-1 ["architect-{context}"]
                      :phase-2 ["scaffold-{context}"]
                      :phase-3 ["impl-{context}-1" "impl-{context}-2"]}
    :coordinator-checklist ["Architect creates ADR"
                            "Scaffolder creates test stubs first"
                            "Implementers follow TDD"]}

   :simplification
   {:team-pattern "Metrics → Simplifier → Validator"
    :lings [{:name "metrics-{context}" :presets ["metrics" "clarity"] :sequence 1}
            {:name "simplify-{context}-1" :presets ["ling" "refactorer" "clarity"] :sequence 2}
            {:name "validate-{context}" :presets ["verifier" "tdd"] :sequence 3}]
    :parallelization {:phase-1 ["metrics-{context}"]
                      :phase-2 ["simplify-{context}-1"]
                      :phase-3 ["validate-{context}"]}
    :coordinator-checklist ["Run scc_analyze/scc_hotspots"
                            "Target complexity >20 to <15"
                            "Verify tests pass after"]}

   :quality-review
   {:team-pattern "Multi-Specialist Review Panel"
    :lings [{:name "review-compliance-{context}" :presets ["compliance" "clarity"] :sequence 1}
            {:name "review-tdd-{context}" :presets ["ling" "tdd" "reviewer"] :sequence 1}
            {:name "review-metrics-{context}" :presets ["analyzer" "clarity"] :sequence 1}]
    :parallelization {:phase-1 ["review-compliance-{context}"
                                "review-tdd-{context}"
                                "review-metrics-{context}"]}
    :coordinator-checklist ["All reviewers run in parallel"
                            "Collect findings as memory entries"
                            "Prioritize fixes by severity"]}

   :documentation
   {:team-pattern "Reader → Documenter"
    :lings [{:name "reader-{context}" :presets ["ling" "clarity"] :sequence 1}
            {:name "documenter-{context}" :presets ["ling" "documenter"] :sequence 2}]
    :parallelization {:phase-1 ["reader-{context}"]
                      :phase-2 ["documenter-{context}"]}
    :coordinator-checklist ["Reader identifies undocumented areas"
                            "Documenter writes docstrings/ADRs"]}})

;; ============================================================
;; Context Interpolation
;; ============================================================

(defn- interpolate-context
  "Replace {context} placeholders with actual context value."
  [s context]
  (if context
    (str/replace s "{context}" context)
    (str/replace s "-{context}" "")))

(defn- interpolate-ling
  "Interpolate context in a single ling spec."
  [ling context]
  (update ling :name interpolate-context context))

(defn- interpolate-parallelization
  "Interpolate context in parallelization map."
  [parallelization context]
  (into {}
        (map (fn [[phase names]]
               [phase (mapv #(interpolate-context % context) names)])
             parallelization)))

(defn- interpolate-composition
  "Interpolate context throughout a team composition."
  [composition context]
  (-> composition
      (update :lings #(mapv (fn [l] (interpolate-ling l context)) %))
      (update :parallelization interpolate-parallelization context)))

;; ============================================================
;; Handler
;; ============================================================

(defn handle-team-select
  "Select team composition for a task type.

   Returns spawn instructions based on pre-defined team patterns
   from memory convention (ID: 20260116131031-76b1a490).

   Parameters:
   - task_type: Enum - :implementation, :refactoring, :greenfield,
                       :simplification, :quality-review, :documentation
   - context: String for name interpolation (e.g., 'auth-feature')
   - auto_spawn: Boolean, execute spawns immediately (default false)

   Returns map with:
   - :task-type - The selected task type
   - :team-pattern - Human-readable pattern name
   - :lings - List of ling specs with name, presets, sequence
   - :parallelization - Which lings can run in parallel
   - :coordinator-checklist - Steps for coordinator to follow

   CLARITY: I - Input validated via spec
   CLARITY: R - Represented intent via typed selection"
  [{:keys [task_type context auto_spawn]}]
  (core/with-swarm
    (let [task-kw (if (string? task_type)
                    (keyword task_type)
                    task_type)]
      (cond
        ;; Validate task type
        (not (specs/valid-team-task-type? task-kw))
        (core/mcp-error
         (format "Invalid task_type '%s'. Valid types: implementation, refactoring, greenfield, simplification, quality-review, documentation"
                 task_type))

        ;; Auto-spawn not yet implemented
        auto_spawn
        (core/mcp-error "auto_spawn not yet implemented. Use returned ling specs with swarm_spawn manually.")

        :else
        (let [composition (get team-compositions task-kw)
              interpolated (interpolate-composition composition context)]
          (core/mcp-success
           (assoc interpolated :task-type task-kw)))))))
