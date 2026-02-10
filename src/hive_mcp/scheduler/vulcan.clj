(ns hive-mcp.scheduler.vulcan
  "Vulcan Mode: KG-aware task prioritization for the Forge Belt.

   Pure calculation module — all functions take dependencies as arguments,
   no atom coupling. Extracts and generalizes the dependency-checking logic
   from dag_waves.clj into reusable priority calculations.

   Key concepts:
   - A task is 'ready' when ALL its KG :depends-on targets are satisfied
   - Satisfied means: completed (deleted from Chroma) or in the completed set
   - Wave number = max dependency chain depth (0 = no deps, 1 = depends on leaf, etc.)
   - Vulcan sort: priority-tier DESC > wave-number ASC > creation-date ASC

   Used by the forge belt survey phase when vulcan-mode is enabled.

   SOLID: SRP — Pure priority calculations only.
   CLARITY: L — No I/O; callers provide dependency data.
   CLARITY: R — Domain intent clear: 'ready', 'blocked', 'wave-number'."
  (:require [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.chroma :as chroma]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; KG Dependency Queries (thin I/O wrappers — testable via with-redefs)
;; =============================================================================

(defn get-task-deps
  "Query KG for the task IDs that a given task depends on.

   Looks for outgoing :depends-on edges FROM this task-id.
   task-B --depends-on--> task-A means B requires A to complete first.

   Returns a set of task-ids (strings)."
  [task-id]
  (try
    (let [edges (kg-edges/get-edges-from task-id)
          dep-edges (filter #(= :depends-on (:kg-edge/relation %)) edges)]
      (set (map :kg-edge/to dep-edges)))
    (catch Exception e
      (log/debug "get-task-deps failed for" task-id (ex-message e))
      #{})))

(defn task-exists-in-chroma?
  "Check if a task still exists in Chroma (not yet completed/deleted).

   In the memory-kanban system, moving to 'done' DELETES the entry.
   So a missing entry means the task was completed."
  [task-id]
  (try
    (some? (chroma/get-entry-by-id task-id))
    (catch Exception _
      false)))

;; =============================================================================
;; Pure Readiness Calculations
;; =============================================================================

(defn dep-satisfied?
  "Check if a single dependency is satisfied.

   A dep is satisfied when:
   - It's in the completed-ids set, OR
   - It no longer exists in Chroma (deleted = done)

   Arguments:
     dep-id        - The dependency task ID
     completed-ids - Set of known-completed task IDs
     exists-fn     - (fn [task-id] -> bool) checks if task exists in store

   CLARITY: R — Named predicate makes intent clear."
  [dep-id completed-ids exists-fn]
  (or (contains? completed-ids dep-id)
      (not (exists-fn dep-id))))

(defn task-ready?
  "Check if a task has all its dependencies satisfied.

   A task with no dependencies is always ready.

   Arguments:
     task-id       - The task to check
     completed-ids - Set of known-completed task IDs
     deps-fn       - (fn [task-id] -> #{dep-ids}) returns dependency set
     exists-fn     - (fn [task-id] -> bool) checks if task exists in store

   Returns true if all deps are satisfied."
  [task-id completed-ids deps-fn exists-fn]
  (let [deps (deps-fn task-id)]
    (if (empty? deps)
      true
      (every? #(dep-satisfied? % completed-ids exists-fn) deps))))

(defn filter-ready-tasks
  "Filter a sequence of todo tasks to only those with all deps satisfied.

   Arguments:
     tasks         - Seq of task maps (must have :id key)
     completed-ids - Set of known-completed task IDs
     deps-fn       - (fn [task-id] -> #{dep-ids}) returns dependency set
     exists-fn     - (fn [task-id] -> bool) checks if task exists in store

   Returns vector of ready task maps."
  [tasks completed-ids deps-fn exists-fn]
  (filterv (fn [task]
             (task-ready? (:id task) completed-ids deps-fn exists-fn))
           tasks))

;; =============================================================================
;; Wave Number Computation
;; =============================================================================

(defn compute-wave-number
  "Compute the wave number (dependency depth) for a task.

   Wave 0 = no dependencies (frontier/leaf task)
   Wave 1 = depends only on wave-0 tasks
   Wave N = max(dep-wave-numbers) + 1

   Uses memoized DFS with cycle detection to handle DAGs safely.

   Arguments:
     task-id  - The task to compute wave for
     deps-fn  - (fn [task-id] -> #{dep-ids}) returns dependency set
     memo     - Atom of {task-id -> wave-number} for memoization (optional)

   Returns integer wave number (0-based)."
  ([task-id deps-fn]
   (compute-wave-number task-id deps-fn (atom {}) #{}))
  ([task-id deps-fn memo visiting]
   (if-let [cached (get @memo task-id)]
     cached
     (if (contains? visiting task-id)
       ;; Cycle detected — treat as wave 0 to avoid infinite recursion
       (do (log/warn "Cycle detected in task deps at" task-id)
           0)
       (let [deps (deps-fn task-id)
             wave (if (empty? deps)
                    0
                    (inc (apply max
                                (map #(compute-wave-number % deps-fn memo
                                                           (conj visiting task-id))
                                     deps))))]
         (swap! memo assoc task-id wave)
         wave)))))

(defn enrich-with-wave-numbers
  "Add :wave-number to each task map based on KG dependency depth.

   Arguments:
     tasks   - Seq of task maps (must have :id key)
     deps-fn - (fn [task-id] -> #{dep-ids}) returns dependency set

   Returns vector of task maps with :wave-number added."
  [tasks deps-fn]
  (let [memo (atom {})]
    (mapv (fn [task]
            (assoc task :wave-number
                   (compute-wave-number (:id task) deps-fn memo #{})))
          tasks)))

;; =============================================================================
;; Vulcan Sort (priority > wave-number > creation-date)
;; =============================================================================

(def ^:private priority-order
  "Priority ranking for sort: lower number = higher priority."
  {"high" 0 "priority-high" 0
   "medium" 1 "priority-medium" 1
   "low" 2 "priority-low" 2})

(defn sort-vulcan
  "Sort tasks by vulcan priority ordering:
   1. Priority tier DESC (high=0 > medium=1 > low=2)
   2. Wave number ASC (frontier tasks first, deeper deps later)
   3. Creation date ASC (older tasks first, by :id timestamp encoding)

   Arguments:
     tasks - Seq of task maps with :priority, :wave-number, :id

   Returns sorted vector of tasks."
  [tasks]
  (vec
   (sort (fn [a b]
           (let [pa (get priority-order (or (:priority a) "medium") 1)
                 pb (get priority-order (or (:priority b) "medium") 1)]
             (if (not= pa pb)
               (compare pa pb)
               (let [wa (or (:wave-number a) 0)
                     wb (or (:wave-number b) 0)]
                 (if (not= wa wb)
                   (compare wa wb)
                   (compare (str (:id a)) (str (:id b))))))))
         tasks)))

;; =============================================================================
;; High-Level API (composes the above)
;; =============================================================================

(defn prioritize-tasks
  "Apply full vulcan prioritization pipeline to a list of todo tasks.

   Pipeline:
   1. Filter to only ready tasks (all deps satisfied)
   2. Enrich with wave numbers
   3. Sort by vulcan ordering (priority > wave > creation-date)

   Arguments:
     tasks         - Seq of task maps from kanban query
     completed-ids - Set of known-completed task IDs (default: #{})
     opts          - Optional map:
                     :deps-fn   - Override dep query fn (default: get-task-deps)
                     :exists-fn - Override existence check (default: task-exists-in-chroma?)

   Returns {:tasks [sorted-ready-tasks] :count N :blocked-count N}."
  ([tasks] (prioritize-tasks tasks #{} {}))
  ([tasks completed-ids] (prioritize-tasks tasks completed-ids {}))
  ([tasks completed-ids {:keys [deps-fn exists-fn]
                         :or {deps-fn get-task-deps
                              exists-fn task-exists-in-chroma?}}]
   (let [ready (filter-ready-tasks tasks completed-ids deps-fn exists-fn)
         enriched (enrich-with-wave-numbers ready deps-fn)
         sorted (sort-vulcan enriched)
         blocked-count (- (count tasks) (count ready))]
     {:tasks sorted
      :count (count sorted)
      :blocked-count blocked-count})))
