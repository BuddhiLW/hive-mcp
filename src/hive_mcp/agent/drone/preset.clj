(ns hive-mcp.agent.drone.preset
  "Drone preset selection logic.
   
   Auto-selects specialized drone presets based on task classification:
   - drone-test-writer: For test-related tasks
   - drone-refactor: For refactoring tasks
   - drone-bugfix: For bug fixing tasks
   - drone-docs: For documentation tasks
   - drone-worker: Default for general tasks")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Task Classification
;;; ============================================================

(def ^:private task-patterns
  "Pattern matching for task classification.
   Each entry is [pattern task-type] where pattern is a regex."
  [[#"(?i)\b(test|spec|coverage|edge.?case|assert)" :testing]
   [#"(?i)\b(refactor|extract|rename|slap|restructure|cleanup)" :refactoring]
   [#"(?i)\b(fix|bug|error|issue|npe|null|exception|crash)" :bugfix]
   [#"(?i)\b(doc|comment|docstring|readme|explain|describe)" :documentation]])

(def ^:private file-patterns
  "File path patterns for task classification."
  [[#"(?i)_test\.clj|test_|_spec\.clj|/test/" :testing]
   [#"(?i)\.md$|readme|doc/" :documentation]])

(defn- classify-task
  "Classify a task based on description and file paths.

   Arguments:
     task  - Task description string
     files - List of file paths (optional)

   Returns one of: :testing, :refactoring, :bugfix, :documentation, :general"
  [task files]
  (let [;; Check task patterns
        task-match (->> task-patterns
                        (filter (fn [[pattern _]] (re-find pattern (or task ""))))
                        first
                        second)
        ;; Check file patterns if no task match
        file-match (when (and (not task-match) (seq files))
                     (->> file-patterns
                          (filter (fn [[pattern _]]
                                    (some #(re-find pattern %) files)))
                          first
                          second))]
    (or task-match file-match :general)))

;;; ============================================================
;;; Public API
;;; ============================================================

(def preset-map
  "Mapping from task type to drone preset name."
  {:testing "drone-test-writer"
   :refactoring "drone-refactor"
   :bugfix "drone-bugfix"
   :documentation "drone-docs"
   :general "drone-worker"})

(defn select-drone-preset
  "Select appropriate drone preset based on task classification.

   Arguments:
     task  - Task description string
     files - List of file paths (optional)

   Returns preset name string.

   Example:
     (select-drone-preset \"Write tests for user validation\" nil)
     ;; => \"drone-test-writer\"

     (select-drone-preset \"Fix NPE in process-order\" nil)
     ;; => \"drone-bugfix\""
  [task files]
  (let [task-type (classify-task task files)]
    (get preset-map task-type "drone-worker")))

(defn get-task-type
  "Get the classified task type (useful for logging/debugging).

   Arguments:
     task  - Task description string
     files - List of file paths (optional)

   Returns keyword: :testing, :refactoring, :bugfix, :documentation, or :general"
  [task files]
  (classify-task task files))
