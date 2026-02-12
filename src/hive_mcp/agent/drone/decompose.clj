(ns hive-mcp.agent.drone.decompose
  "Task processing delegates to extension if available.

   Extension points are resolved via the extensions registry at startup.
   When no extensions are registered, all functions gracefully degrade
   to noop results."
  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

;; =============================================================================
;; Noop Defaults
;; =============================================================================

(def ^:private simple-default
  "Default noop result for complexity estimation."
  {:score 3.0
   :file-size 0
   :task-length 0
   :operation-count 0
   :recommendation :single-drone
   :complexity :simple
   :complexity-tier :simple
   :max-steps 15})

(def ^:const step-budgets
  "Max steps based on task complexity tier."
  {:trivial 8
   :simple 15
   :medium 25
   :complex 40})

;; =============================================================================
;; Public API — delegates to extension or returns noop
;; =============================================================================

(defn estimate-complexity
  "Estimate task complexity.
   Delegates to extension if available."
  [task file-path]
  (delegate-or-noop :dd/est simple-default [task file-path]))

(defn get-step-budget
  "Return step budget for task.
   Delegates to extension if available."
  [task files]
  (delegate-or-noop :dd/budget
                    (:simple step-budgets)
                    [task files]))

(defn split-task-by-functions
  "Split task into subtasks.
   Delegates to extension if available."
  [task file-path]
  (delegate-or-noop :dd/split-fn
                    [{:task task :file file-path :scope {:type :whole-file}}]
                    [task file-path]))

(defn split-task-by-sections
  "Split task into subtasks by sections.
   Delegates to extension if available."
  [task file-path n]
  (delegate-or-noop :dd/split-sec
                    [{:task task :file file-path :scope {:type :whole-file}}]
                    [task file-path n]))

(defn split-task-by-operations
  "Split task into subtasks by operation type.
   Delegates to extension if available."
  [task file-path]
  (delegate-or-noop :dd/split-op
                    [{:task task :file file-path :scope {:type :whole-file}}]
                    [task file-path]))

(defn maybe-decompose
  "Auto-decompose task if extension available, returning subtask vector.
   Delegates to extension if available."
  [task file opts]
  (delegate-or-noop :dd/decomp
                    [{:task task :file file :scope {:type :whole-file}}]
                    [task file opts]))

(defn recombine-results
  "Recombine results from parallel drone subtask execution."
  [results]
  (let [all-modified (mapcat :files-modified results)
        all-failed (mapcat :files-failed results)
        all-completed? (every? #(= :completed (:status %)) results)]
    {:status (if all-completed? :completed :partial)
     :subtask-count (count results)
     :files-modified (vec (distinct all-modified))
     :files-failed (vec all-failed)
     :results results}))
