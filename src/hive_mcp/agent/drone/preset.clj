(ns hive-mcp.agent.drone.preset
  "Drone preset selection based on task classification.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private task-patterns
  "Pattern matching for task classification."
  [[#"(?i)\b(test|spec|coverage|edge.?case|assert)" :testing]
   [#"(?i)\b(refactor|extract|rename|slap|restructure|cleanup)" :refactoring]
   [#"(?i)\b(fix|bug|error|issue|npe|null|exception|crash)" :bugfix]
   [#"(?i)\b(doc|comment|docstring|readme|explain|describe)" :documentation]])

(def ^:private file-patterns
  "File path patterns for task classification."
  [[#"(?i)_test\.clj|test_|_spec\.clj|/test/" :testing]
   [#"(?i)\.md$|readme|doc/" :documentation]])

(defn- classify-task
  "Classify a task based on description and file paths."
  [task files]
  (let [task-match (->> task-patterns
                        (filter (fn [[pattern _]] (re-find pattern (or task ""))))
                        first
                        second)
        file-match (when (and (not task-match) (seq files))
                     (->> file-patterns
                          (filter (fn [[pattern _]]
                                    (some #(re-find pattern %) files)))
                          first
                          second))]
    (or task-match file-match :general)))

(def preset-map
  "Mapping from task type to drone preset name."
  {:testing "drone-test-writer"
   :refactoring "drone-refactor"
   :bugfix "drone-bugfix"
   :documentation "drone-docs"
   :general "drone-worker"})

(defn select-drone-preset
  "Select appropriate drone preset based on task classification."
  [task files]
  (let [task-type (classify-task task files)]
    (get preset-map task-type "drone-worker")))

(defn get-task-type
  "Get the classified task type for a given task and files."
  [task files]
  (classify-task task files))
