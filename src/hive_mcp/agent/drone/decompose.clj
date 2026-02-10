(ns hive-mcp.agent.drone.decompose
  "Task decomposition for drone delegation.

   Large tasks overwhelm free-tier models. This module provides:
   - Complexity estimation based on file size, task length, operation count
   - Decomposition strategies (by-function, by-section, by-operation)
   - Auto-decomposition for delegate_drone

   SOLID: SRP - Single responsibility for task decomposition
   CLARITY: I - Inputs validated, complexity estimated before dispatch"
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Complexity Estimation
;;; ============================================================

(def ^:const complexity-thresholds
  "Thresholds for decomposition recommendations.
   :single-drone - task can be handled by one drone
   :split-2      - split into 2 subtasks
   :split-many   - split into multiple subtasks"
  {:single-drone 5
   :split-2 15})

(def ^:const step-budgets
  "Max steps based on task complexity (legacy OpenRouter drones).
   Simple tasks should complete in few steps, complex tasks get more headroom.

   FRICTION FIX: Drones were hitting 50 steps on trivial 'remove unused namespace'
   tasks. This maps complexity to appropriate step budgets:
   - :trivial (< 2): 8 steps - single read, lint, propose
   - :simple (2-5):  15 steps - typical single-file edit
   - :medium (5-15): 25 steps - multi-function changes
   - :complex (> 15): 40 steps - large refactoring"
  {:trivial 8
   :simple 15
   :medium 25
   :complex 40})

(defn estimate-complexity
  "Estimate task complexity based on file size, task description, and operations.

   Arguments:
     task      - Task description string
     file-path - Path to the file being modified

   Returns:
     {:score           - Numeric complexity score
      :file-size       - File size in bytes (0 if not found)
      :task-length     - Task description length
      :operation-count - Count of modification keywords
      :recommendation  - :single-drone, :split-2, or :split-many
      :complexity-tier - :trivial, :simple, :medium, or :complex
      :max-steps       - Recommended step budget for this complexity}"
  [task file-path]
  (let [file-size (try (count (slurp file-path)) (catch Exception _ 0))
        task-length (count (or task ""))
        ;; Count modification keywords
        keywords (count (re-seq #"(?i)(add|modify|refactor|implement|update|change|fix|create|delete|remove)" (or task "")))
        ;; Calculate score
        score (+ (/ file-size 1000.0)
                 (/ task-length 100.0)
                 (* keywords 2))
        recommendation (cond
                         (< score (:single-drone complexity-thresholds)) :single-drone
                         (< score (:split-2 complexity-thresholds)) :split-2
                         :else :split-many)
        ;; FRICTION FIX: Map complexity to step budget
        complexity-tier (cond
                          (< score 2) :trivial
                          (< score (:single-drone complexity-thresholds)) :simple
                          (< score (:split-2 complexity-thresholds)) :medium
                          :else :complex)
        max-steps (get step-budgets complexity-tier 25)]
    {:score score
     :file-size file-size
     :task-length task-length
     :operation-count keywords
     :recommendation recommendation
     :complexity-tier complexity-tier
     :max-steps max-steps}))

(defn get-step-budget
  "Get recommended max-steps for a task based on complexity.

   FRICTION FIX: Simple tasks like 'remove unused namespace' were exhausting
   50 steps. This function returns an appropriate step budget based on
   estimated complexity.

   Arguments:
     task  - Task description string
     files - List of file paths (uses first for complexity estimation)

   Returns:
     Integer max-steps value (8-40 based on complexity)"
  [task files]
  (let [file (first files)
        {:keys [max-steps complexity-tier score]} (when file
                                                    (estimate-complexity task file))]
    (log/debug "Step budget calculated" {:task (subs task 0 (min 50 (count task)))
                                         :complexity-tier complexity-tier
                                         :score score
                                         :max-steps max-steps})
    (or max-steps (:simple step-budgets))))

;;; ============================================================
;;; Decomposition Strategies
;;; ============================================================

(defn- extract-clojure-functions
  "Extract function definitions from Clojure source.
   Returns list of {:name :start-line :end-line}."
  [content]
  (let [lines (str/split-lines content)
        indexed (map-indexed vector lines)]
    (->> indexed
         (filter (fn [[_ line]] (re-find #"^\s*\(defn-?\s" line)))
         (map (fn [[idx line]]
                (let [[_ name] (re-find #"\(defn-?\s+([^\s\[]+)" line)]
                  {:name name :start-line idx})))
         (filter :name))))

(defn- extract-section-markers
  "Extract section markers (;;; comments) from Clojure source.
   Returns list of {:name :start-line}."
  [content]
  (let [lines (str/split-lines content)
        indexed (map-indexed vector lines)]
    (->> indexed
         (filter (fn [[_ line]] (re-find #"^;;;+\s*.+" line)))
         (map (fn [[idx line]]
                (let [name (str/trim (str/replace line #"^;;;+\s*" ""))]
                  {:name name :start-line idx}))))))

(defn split-task-by-functions
  "Split a task by function definitions in the file.
   Each function mentioned in the task becomes a separate subtask.

   Arguments:
     task      - Original task description
     file-path - Path to source file

   Returns:
     Vector of {:task :file :scope} maps"
  [task file-path]
  (try
    (let [content (slurp file-path)
          functions (extract-clojure-functions content)
          task-lower (str/lower-case task)
          ;; Find functions mentioned in task
          mentioned (filter (fn [{:keys [name]}]
                              (str/includes? task-lower (str/lower-case name)))
                            functions)]
      (if (seq mentioned)
        (mapv (fn [{:keys [name]}]
                {:task (str "In function `" name "`: " task)
                 :file file-path
                 :scope {:type :function :name name}})
              mentioned)
        ;; No specific functions mentioned - return original
        [{:task task :file file-path :scope {:type :whole-file}}]))
    (catch Exception e
      (log/warn e "Failed to split by functions, returning original task")
      [{:task task :file file-path :scope {:type :whole-file}}])))

(defn split-task-by-sections
  "Split a task by section markers (;;; comments) in the file.

   Arguments:
     task      - Original task description
     file-path - Path to source file
     n         - Target number of splits

   Returns:
     Vector of {:task :file :scope} maps"
  [task file-path n]
  (try
    (let [content (slurp file-path)
          sections (extract-section-markers content)
          section-count (count sections)]
      (if (>= section-count n)
        ;; Distribute sections across n subtasks
        (let [per-task (max 1 (quot section-count n))
              partitioned (partition-all per-task sections)]
          (mapv (fn [section-group]
                  (let [names (map :name section-group)]
                    {:task (str task "\n\nFocus on sections: " (str/join ", " names))
                     :file file-path
                     :scope {:type :sections :names names}}))
                partitioned))
        ;; Not enough sections - return original
        [{:task task :file file-path :scope {:type :whole-file}}]))
    (catch Exception e
      (log/warn e "Failed to split by sections, returning original task")
      [{:task task :file file-path :scope {:type :whole-file}}])))

(defn split-task-by-operations
  "Split a task by operation type (add/modify/delete).

   Arguments:
     task      - Original task description
     file-path - Path to source file

   Returns:
     Vector of {:task :file :scope} maps"
  [task file-path]
  (let [task-lower (str/lower-case task)
        has-add? (re-find #"\b(add|create|implement|new)\b" task-lower)
        has-modify? (re-find #"\b(modify|update|change|fix|refactor)\b" task-lower)
        has-delete? (re-find #"\b(delete|remove|drop)\b" task-lower)
        ops (cond-> []
              has-add? (conj {:op :add :desc "Add/create new code"})
              has-modify? (conj {:op :modify :desc "Modify/update existing code"})
              has-delete? (conj {:op :delete :desc "Delete/remove code"}))]
    (if (> (count ops) 1)
      (mapv (fn [{:keys [op desc]}]
              {:task (str "[" (name op) " operation only] " task "\n\nFocus only on: " desc)
               :file file-path
               :scope {:type :operation :op op}})
            ops)
      [{:task task :file file-path :scope {:type :whole-file}}])))

;;; ============================================================
;;; Auto-Decomposition
;;; ============================================================

(defn maybe-decompose
  "Auto-decompose task if complexity warrants it.

   Arguments:
     task    - Task description
     file    - File path (single file for now)
     opts    - Options map:
               :strategy - Override decomposition strategy
               :force?   - Force decomposition even if not recommended

   Returns:
     Vector of {:task :file :scope} maps.
     Returns single-element vector if no decomposition needed."
  [task file {:keys [strategy force?] :as _opts}]
  (let [{:keys [recommendation score]} (estimate-complexity task file)]
    (log/debug "Task complexity" {:score score :recommendation recommendation :file file})

    (if (or force? (not= recommendation :single-drone))
      (let [strategy (or strategy
                         (case recommendation
                           :split-2 :sections
                           :split-many :functions
                           :sections))
            subtasks (case strategy
                       :functions (split-task-by-functions task file)
                       :sections (split-task-by-sections task file
                                                         (if (= recommendation :split-2) 2 4))
                       :operations (split-task-by-operations task file)
                       ;; default
                       [{:task task :file file :scope {:type :whole-file}}])]
        (when (> (count subtasks) 1)
          (log/info "Decomposed task into" (count subtasks) "subtasks"
                    {:strategy strategy :recommendation recommendation}))
        subtasks)
      ;; No decomposition needed
      [{:task task :file file :scope {:type :whole-file}}])))

(defn recombine-results
  "Recombine results from parallel drone execution.

   Arguments:
     results - Sequence of drone result maps

   Returns:
     Merged result with combined files-modified, aggregated status"
  [results]
  (let [all-modified (mapcat :files-modified results)
        all-failed (mapcat :files-failed results)
        all-completed? (every? #(= :completed (:status %)) results)]
    {:status (if all-completed? :completed :partial)
     :subtask-count (count results)
     :files-modified (vec (distinct all-modified))
     :files-failed (vec all-failed)
     :results results}))
