(ns hive-mcp.tools.swarm.validated-wave
  "Validated wave execution with self-healing loop.

   Extends dispatch_drone_wave with post-execution validation:
   - Runs kondo_lint after each wave iteration
   - Generates fix tasks from lint findings
   - Re-dispatches until validation passes or max retries reached

   Token Efficiency (ADR-004):
   - Lings NEVER implement directly (92% token savings)
   - Drones handle all file mutations
   - Self-healing loop reduces manual ling intervention

   SOLID: SRP - Validation loop orchestration only
   CLARITY: A - Architectural performance via bounded retries"
  (:require [hive-mcp.tools.swarm.wave :as wave]
            [hive-mcp.tools.kondo :as kondo]
            [hive-mcp.tools.core :refer [mcp-error]]
            [hive-mcp.events.core :as ev]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Constants
;;; =============================================================================

(def ^:const default-max-retries
  "Default maximum retry iterations for validation loop."
  3)

(def ^:const default-lint-level
  "Default severity threshold for validation."
  "error")

;;; =============================================================================
;;; Validation Functions
;;; =============================================================================

(defn- extract-modified-files
  "Extract list of modified files from wave execution result.

   Arguments:
     wave-status - Status map from get-wave-status

   Returns:
     Vector of file paths that were successfully modified."
  [plan-status]
  (->> (:items plan-status)
       (filter #(= :completed (:status %)))
       (mapv :file)))

(defn- lint-files
  "Run clj-kondo lint on files and filter by severity level.

   Arguments:
     files      - Collection of file paths to lint
     lint-level - Severity threshold (:error, :warning, :info)

   Returns:
     Map with :findings (filtered) and :files-with-errors"
  [files lint-level]
  (let [level-kw (keyword lint-level)
        all-findings (reduce
                      (fn [acc file]
                        (try
                          (let [{:keys [findings]} (kondo/run-analysis file)
                                filtered (->> findings
                                              (filter #(case level-kw
                                                         :error (= (:level %) :error)
                                                         :warning (#{:error :warning} (:level %))
                                                         :info true)))]
                            (concat acc filtered))
                          (catch Exception e
                            (log/warn "Failed to lint file:" file (.getMessage e))
                            acc)))
                      []
                      files)
        files-with-errors (->> all-findings
                               (group-by :filename)
                               (keys)
                               (vec))]
    {:findings (vec all-findings)
     :files-with-errors files-with-errors
     :count (count all-findings)}))

(defn- format-finding-for-task
  "Format a lint finding into a human-readable string for task description.

   Arguments:
     finding - Map with :filename :row :col :level :type :message"
  [{:keys [row col level type message]}]
  (format "- Line %d, Col %d [%s/%s]: %s"
          row col (name level) (name type) message))

(defn- generate-fix-tasks
  "Generate fix tasks from lint findings.

   Groups findings by file and creates one task per file with all its errors.

   Arguments:
     findings - Collection of lint findings
     original-tasks - Original tasks to extract context from

   Returns:
     Vector of {:file :task} maps for re-dispatch."
  [findings original-tasks]
  (let [;; Group findings by file
        by-file (group-by :filename findings)
        ;; Build task map from original for context
        original-by-file (into {} (map (juxt :file :task) original-tasks))]
    (mapv (fn [[file file-findings]]
            {:file file
             :task (str "Fix the following lint errors in this file:\n\n"
                        (str/join "\n" (map format-finding-for-task file-findings))
                        "\n\n"
                        (when-let [original-task (get original-by-file file)]
                          (str "Original task context: " original-task "\n\n"))
                        "IMPORTANT: Fix ALL listed errors. Ensure code compiles after changes.")})
          by-file)))

;;; =============================================================================
;;; Self-Healing Loop
;;; =============================================================================

(defn execute-validated-wave!
  "Execute a wave with validation loop and auto-retry.

   Execution Flow:
     DISPATCH → VALIDATE → [PASS] → COMPLETE
                    │
                 [FAIL]
                    │
                    ▼
              DIAGNOSE → GENERATE FIX TASKS → DISPATCH (loop)

   Arguments:
     tasks   - Collection of {:file :task} maps
     opts    - Options map:
               :validate     - Run validation (default: true)
               :max-retries  - Max retry iterations (default: 3)
               :lint-level   - Severity threshold (default: \"error\")
               :preset       - Drone preset (default: \"drone-worker\")
               :trace        - Emit events (default: true)
               :cwd          - Working directory override

   Returns:
     Map with:
       :status      - :success, :partial, or :failed
       :iterations  - Number of iterations executed
       :final-wave-id - Last wave ID
       :findings    - Remaining lint findings (if partial)
       :history     - Iteration history [{:iteration :wave-id :findings}]

   CLARITY-Y: Graceful degradation - returns partial on max retries."
  [tasks {:keys [validate max-retries lint-level preset trace cwd]
          :or {validate true
               max-retries default-max-retries
               lint-level default-lint-level
               preset "drone-worker"
               trace true}}]
  (let [start-time (System/nanoTime)]
    (log/info "Starting validated wave" {:task-count (count tasks)
                                         :validate validate
                                         :max-retries max-retries
                                         :lint-level lint-level})

    (when trace
      (ev/dispatch [:validated-wave/start {:task-count (count tasks)
                                           :max-retries max-retries
                                           :lint-level lint-level}]))

    (loop [current-tasks tasks
           iteration 1
           history []]
      (log/info "Validated wave iteration" iteration "with" (count current-tasks) "tasks")

      ;; Emit iteration start event
      (when trace
        (ev/dispatch [:validated-wave/iteration-start {:iteration iteration
                                                       :task-count (count current-tasks)}]))

      ;; Execute wave
      (let [plan-id (wave/create-plan! current-tasks preset)
            wave-id (wave/execute-wave! plan-id {:trace trace :cwd cwd})
            plan-status (wave/get-plan-status plan-id)
            modified-files (extract-modified-files plan-status)
            failed-items (->> (:items plan-status)
                              (filter #(= :failed (:status %))))]

        ;; Check for execution failures (not lint failures)
        (when (seq failed-items)
          (log/warn "Some tasks failed in iteration" iteration {:failed-count (count failed-items)}))

        ;; Run validation if enabled and we have modified files
        (let [validation-result (when (and validate (seq modified-files))
                                  (lint-files modified-files lint-level))
              findings (:findings validation-result)
              iteration-record {:iteration iteration
                                :wave-id wave-id
                                :plan-id plan-id
                                :modified-files modified-files
                                :finding-count (count findings)
                                :execution-failures (count failed-items)}
              updated-history (conj history iteration-record)
              ;; Calculate total execution failures across all iterations
              total-exec-failures (reduce + (map :execution-failures updated-history))]

          ;; Decision point: pass, retry, or give up
          (cond
              ;; No validation requested or no lint findings
              ;; BUT: check for execution failures - those make it partial, not success
            (or (not validate) (empty? findings))
            (if (pos? total-exec-failures)
                ;; Execution failures exist - return partial success, not full success
              (do
                (log/warn "Validated wave completed with execution failures"
                          {:iterations iteration
                           :execution-failures total-exec-failures})
                (when trace
                  (ev/dispatch [:validated-wave/partial
                                {:iterations iteration
                                 :execution-failures total-exec-failures
                                 :duration-ns (- (System/nanoTime) start-time)}]))
                {:status :partial
                 :iterations iteration
                 :final-wave-id wave-id
                 :final-plan-id plan-id
                 :modified-files modified-files
                 :execution-failures total-exec-failures
                 :history updated-history
                 :message (format "Lint passed but %d task(s) failed to execute." total-exec-failures)})
                ;; No execution failures, true success
              (do
                (log/info "Validated wave completed successfully" {:iterations iteration})
                (when trace
                  (ev/dispatch [:validated-wave/success
                                {:iterations iteration
                                 :wave-id wave-id
                                 :duration-ns (- (System/nanoTime) start-time)}]))
                {:status :success
                 :iterations iteration
                 :final-wave-id wave-id
                 :final-plan-id plan-id
                 :modified-files modified-files
                 :history updated-history}))

            ;; Max retries reached → partial success
            (>= iteration max-retries)
            (do
              (log/warn "Validated wave reached max retries" {:iterations iteration
                                                              :remaining-findings (count findings)
                                                              :execution-failures total-exec-failures})
              (when trace
                (ev/dispatch [:validated-wave/partial
                              {:iterations iteration
                               :remaining-findings (count findings)
                               :execution-failures total-exec-failures
                               :duration-ns (- (System/nanoTime) start-time)}]))
              {:status :partial
               :iterations iteration
               :final-wave-id wave-id
               :final-plan-id plan-id
               :modified-files modified-files
               :findings findings
               :files-with-errors (:files-with-errors validation-result)
               :execution-failures total-exec-failures
               :history updated-history
               :message (format "Validation failed after %d iterations. %d lint errors remain. %d task(s) failed to execute."
                                iteration (count findings) total-exec-failures)})

            ;; Findings present and retries remaining → generate fix tasks and retry
            :else
            (let [fix-tasks (generate-fix-tasks findings current-tasks)]
              (log/info "Generating fix tasks for iteration" (inc iteration)
                        {:fix-task-count (count fix-tasks)
                         :finding-count (count findings)})
              (when trace
                (ev/dispatch [:validated-wave/retry {:iteration iteration
                                                     :finding-count (count findings)
                                                     :fix-task-count (count fix-tasks)}]))
              (recur fix-tasks (inc iteration) updated-history))))))))

;;; =============================================================================
;;; MCP Handler
;;; =============================================================================

(defn handle-dispatch-validated-wave
  "Handle dispatch_validated_wave MCP tool call.

   Executes a wave with post-execution validation and auto-retry loop.

   Parameters:
     tasks       - Array of {:file :task} objects (required)
     validate    - Run clj-kondo lint after execution (default: true)
     max_retries - Max retry iterations (default: 3)
     lint_level  - Severity threshold: error, warning, info (default: error)
     preset      - Drone preset (default: drone-worker)
     trace       - Emit progress events (default: true)
     cwd         - Working directory override (optional)

   Returns:
     JSON with execution status, iteration count, and any remaining findings.

   Usage:
     dispatch_validated_wave(
       tasks: [{file: \"src/foo.clj\", task: \"Add docstrings\"}],
       validate: true,
       max_retries: 3,
       lint_level: \"error\"
     )"
  [{:keys [tasks validate max_retries lint_level preset trace cwd]}]
  (try
    (when (empty? tasks)
      (throw (ex-info "tasks array is required and must not be empty" {})))

    ;; Normalize task keys (MCP sends string keys)
    (let [normalized-tasks (mapv (fn [t]
                                   {:file (or (get t "file") (:file t))
                                    :task (or (get t "task") (:task t))})
                                 tasks)
          result (execute-validated-wave!
                  normalized-tasks
                  {:validate (if (false? validate) false true)
                   :max-retries (or max_retries default-max-retries)
                   :lint-level (or lint_level default-lint-level)
                   :preset (or preset "drone-worker")
                   :trace (if (nil? trace) true trace)
                   :cwd cwd})]

      {:type "text"
       :text (json/write-str
              (merge
               {:status (name (:status result))
                :iterations (:iterations result)
                :final_wave_id (:final-wave-id result)
                :final_plan_id (:final-plan-id result)
                :modified_files (:modified-files result)}
               ;; Include execution failures at top level when present
               (when-let [exec-failures (:execution-failures result)]
                 {:execution_failures exec-failures})
               ;; Include partial-specific fields (lint failures)
               (when (= :partial (:status result))
                 (merge
                  {:message (:message result)}
                  (when (:findings result)
                    {:remaining_findings (count (:findings result))
                     :files_with_errors (:files-with-errors result)})))
               ;; Always include iteration history for debugging
               (when (seq (:history result))
                 {:iteration_history
                  (mapv #(select-keys % [:iteration :wave-id :finding-count :execution-failures])
                        (:history result))})))})

    (catch Exception e
      (log/error e "dispatch_validated_wave failed")
      (mcp-error (str "Validated wave failed: " (.getMessage e))))))

;;; =============================================================================
;;; Tool Definition
;;; =============================================================================

(def tools
  "Tool definitions for validated wave execution."
  [{:name "dispatch_validated_wave"
    :description "Dispatch multiple drones with post-execution validation and self-healing. Runs kondo_lint after each iteration, generates fix tasks for errors, and re-dispatches until validation passes or max retries reached. Use this instead of dispatch_drone_wave when you need quality gates on drone output."
    :inputSchema {:type "object"
                  :properties {"tasks" {:type "array"
                                        :items {:type "object"
                                                :properties {"file" {:type "string"
                                                                     :description "File path to modify"}
                                                             "task" {:type "string"
                                                                     :description "Task description for this file"}}
                                                :required ["file" "task"]}
                                        :description "Array of {file, task} objects to execute"}
                               "validate" {:type "boolean"
                                           :description "Run clj-kondo lint after execution (default: true)"}
                               "max_retries" {:type "integer"
                                              :description "Max retry iterations for validation failures (default: 3)"}
                               "lint_level" {:type "string"
                                             :enum ["error" "warning" "info"]
                                             :description "Lint severity threshold (default: error)"}
                               "preset" {:type "string"
                                         :description "Drone preset (default: drone-worker)"}
                               "trace" {:type "boolean"
                                        :description "Emit progress events (default: true)"}
                               "cwd" {:type "string"
                                      :description "Working directory override for path resolution"}}
                  :required ["tasks"]}
    :handler handle-dispatch-validated-wave}])
