(ns hive-mcp.scheduler.vulcan
  "KG-aware task prioritization for the Forge Belt."
  (:require [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.vectordb.facade :as facade]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]
            [hive-mcp.vectordb.kanban-facade :as kanban]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; KG Dependency Queries (thin I/O wrappers — testable via with-redefs)
;; =============================================================================

(defn get-task-deps
  "Query task dependency IDs. Lookup errors propagate so callers cannot mistake failure for no dependencies."
  [task-id]
  (let [edges (kg-edges/get-edges-from task-id)]
    (set (keep #(when (= :depends-on (:kg-edge/relation %)) (:kg-edge/to %)) edges))))

(defn task-exists?
  "Check if a task still exists in the memory store."
  [task-id]
  (rescue false
          (some? (facade/get-entry-by-id task-id))))

;; =============================================================================
;; Pure Readiness Calculations
;; =============================================================================

(defn entry-state
  "Classify a canonical kanban entry. Missing or malformed entries never prove completion."
  [entry]
  (if (nil? entry)
    :missing
    (try
      (let [raw (:content entry)
            content (if (string? raw) (json/read-str raw :key-fn keyword) raw)
            status (or (:status content) (get content "status"))
            status (if (keyword? status) (name status) status)]
        (cond
          (not= "kanban" (or (:task-type content) (get content "task-type"))) :invalid
          (= "done" status) :done
          (#{"todo" "doing" "review" "inprogress" "inreview"} status) :open
          :else :invalid))
      (catch Exception _ :invalid))))

(defn task-state
  "Read completion from the configured kanban store; lookup failures remain explicit and blocked."
  [task-id]
  (try
    (entry-state (kanban/get-entry-by-id task-id))
    (catch Exception _ :lookup-error)))

(defn dependency-state
  "Normalize a state lookup, including legacy exists-fn booleans. Absence is not completion."
  [dep-id completed-ids state-fn]
  (if (contains? completed-ids dep-id)
    :done
    (try
      (let [state (state-fn dep-id)]
        (cond
          (true? state) :open
          (or (false? state) (nil? state)) :missing
          (#{:done :open :missing :invalid :lookup-error} state) state
          :else :invalid))
      (catch Exception _ :lookup-error))))

(defn dep-satisfied?
  "A dependency is satisfied only by explicit completion or a stored done status."
  [dep-id completed-ids state-fn]
  (= :done (dependency-state dep-id completed-ids state-fn)))

(defn task-ready?
  "Check if a task has all its dependencies satisfied."
  [task-id completed-ids deps-fn exists-fn]
  (let [deps (deps-fn task-id)]
    (if (empty? deps)
      true
      (every? #(dep-satisfied? % completed-ids exists-fn) deps))))

(defn filter-ready-tasks
  "Filter todo tasks to only those with all deps satisfied."
  [tasks completed-ids deps-fn exists-fn]
  (filterv (fn [task]
             (task-ready? (:id task) completed-ids deps-fn exists-fn))
           tasks))

;; =============================================================================
;; Wave Number Computation
;; =============================================================================

(defn compute-wave-number
  "Compute the wave number (dependency depth) for a task."
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
  "Add :wave-number to each task map based on KG dependency depth."
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
  "Sort tasks by priority tier, wave number, then creation date."
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
  "Select the ready frontier. :state-fn reads :done/:open/:missing/:invalid/:lookup-error.
   Legacy :exists-fn remains injectable, but false no longer means completed.
   Dependency-query failures block the affected task and are returned in :blocked."
  ([tasks] (prioritize-tasks tasks #{} {}))
  ([tasks completed-ids] (prioritize-tasks tasks completed-ids {}))
  ([tasks completed-ids {:keys [deps-fn state-fn exists-fn task_ids]
                         :or {deps-fn get-task-deps}}]
   (let [scoped (if (some? task_ids)
                  (filterv #(contains? (set task_ids) (:id %)) tasks)
                  tasks)
         deps-fn (memoize deps-fn)
         state-fn (memoize (or state-fn exists-fn task-state))
         outcomes
         (mapv (fn [task]
                 (try
                   (let [states (into {} (map (fn [id]
                                               [id (dependency-state id completed-ids state-fn)]))
                                      (deps-fn (:id task)))
                         unmet (into {} (remove #(= :done (val %))) states)]
                     (if (seq unmet)
                       {:blocked {:task-id (:id task) :dependencies unmet}}
                       {:task (assoc task :wave-number
                                     (compute-wave-number (:id task) deps-fn))}))
                   (catch Exception e
                     {:blocked {:task-id (:id task) :state :lookup-error
                                :error (ex-message e)}})))
               scoped)
         ready (sort-vulcan (vec (keep :task outcomes)))
         blocked (vec (keep :blocked outcomes))]
     {:tasks ready :count (count ready)
      :blocked blocked :blocked-count (count blocked)
      :scoped-count (count scoped)})))
