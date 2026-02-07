(ns hive-mcp.tools.swarm.wave.handlers
  "MCP handlers for wave operations.

   Thin adapter layer that parses MCP parameters and delegates
   to domain functions.

   SOLID-S: Single responsibility - MCP parameter handling only.
   CLARITY-L: Thin adapter, no business logic."
  (:require [hive-mcp.tools.swarm.wave.execution :as execution]
            [hive-mcp.tools.swarm.wave.validation :as validation]
            [hive-mcp.tools.swarm.wave.status :as status]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Plan Creation
;;; ============================================================

(defn create-plan!
  "Create a change plan with multiple tasks.

   Arguments:
     tasks  - Collection of {:file \"path\" :task \"description\"}
     preset - Optional drone preset (default: \"drone-worker\")

   Returns:
     The generated plan-id"
  [tasks & [preset]]
  (ds/create-plan! tasks (or preset "drone-worker")))

;;; ============================================================
;;; MCP Handlers
;;; ============================================================

(defn handle-get-wave-status
  "Handle get_wave_status MCP tool call.

   Parameters:
     wave_id - Wave ID to get status for (required)

   Returns:
     JSON with wave status including counts and item details."
  [{:keys [wave_id]}]
  (try
    (when-not wave_id
      (throw (ex-info "wave_id is required" {})))

    (if-let [wave-status (status/get-wave-status wave_id)]
      ;; Also get failed item details
      (let [failed-items (status/get-failed-items wave_id)]
        {:type "text"
         :text (json/write-str (merge wave-status
                                      {:failed_items failed-items}))})
      {:type "text"
       :text (json/write-str {:error "Wave not found"
                              :wave_id wave_id})})
    (catch Exception e
      (log/error e "get_wave_status failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)})})))

(defn handle-dispatch-drone-wave
  "Handle dispatch_drone_wave MCP tool call.

   Parameters:
     tasks          - Array of {:file :task} objects (required)
     preset         - Drone preset (default: \"drone-worker\")
     trace          - Emit progress events (default: true)
     cwd            - Working directory override (optional)
     ensure_dirs    - Create parent directories before dispatch (default: true)
     validate_paths - Fail fast if paths are invalid (default: true)
     mode           - Execution mode: \"delegate\" (default) or \"agentic\"
                      When \"agentic\", uses in-process agentic loop with session KG

   Returns:
     JSON with wave-id for immediate response.
     Actual execution happens asynchronously."
  [{:keys [tasks preset trace cwd ensure_dirs validate_paths mode]}]
  ;; DEPRECATION WARNING: Prefer unified 'delegate' tool
  (log/warn {:event :deprecation-warning
             :tool "dispatch_drone_wave"
             :message "DEPRECATED: Use 'wave' consolidated tool instead."})

  (try
    (when (empty? tasks)
      (throw (ex-info "tasks array is required and must not be empty" {})))

    ;; CTX Migration: Use request context fallback for cwd
    (let [effective-cwd (or cwd (ctx/current-directory))
          ;; Normalize mode: string from MCP → keyword
          effective-mode (if (= "agentic" (some-> mode name))
                           :agentic
                           :delegate)
          ;; Normalize task keys (MCP sends string keys)
          normalized-tasks (mapv (fn [t]
                                   {:file (or (get t "file") (:file t))
                                    :task (or (get t "task") (:task t))})
                                 tasks)
          ;; Pre-flight defaults
          do-validate (if (false? validate_paths) false true)
          do-ensure (if (false? ensure_dirs) false true)]

      ;; Pre-flight validation
      (when do-ensure
        (validation/ensure-parent-dirs! normalized-tasks))
      (when do-validate
        (validation/validate-task-paths normalized-tasks))

      ;; Create plan and execute async
      (let [plan-id (create-plan! normalized-tasks preset)
            {:keys [wave-id item-count]} (execution/execute-wave-async!
                                          plan-id
                                          {:trace (if (nil? trace) true trace)
                                           :cwd effective-cwd
                                           :mode effective-mode})]
        {:type "text"
         :text (json/write-str {:status "dispatched"
                                :plan_id plan-id
                                :wave_id wave-id
                                :item_count item-count
                                :mode (name effective-mode)
                                :message (str "Wave dispatched to background"
                                              (when (= :agentic effective-mode)
                                                " (agentic mode with session KG)")
                                              ". Poll get_wave_status(wave_id) for progress.")})}))

    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (log/error e "dispatch_drone_wave failed" {:error-type (:type data)})
        {:type "text"
         :text (json/write-str (merge {:error (.getMessage e)}
                                      (when (:hint data) {:hint (:hint data)})
                                      (when (:type data) {:error_type (name (:type data))})))}))
    (catch Exception e
      (log/error e "dispatch_drone_wave failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)})})))

(defn handle-dispatch-validated-wave
  "Handle dispatch_drone_wave with post-execution validation.

   Same as dispatch_drone_wave but runs lint and compile checks
   after applying diffs.

   Parameters:
     tasks      - Array of {:file :task} objects (required)
     preset     - Drone preset (default: \"drone-worker\")
     lint_level - Lint severity (:error :warning :info)
     trace      - Emit progress events (default: true)
     cwd        - Working directory override

   Returns:
     JSON with wave-id and validation results."
  [{:keys [lint_level] :as params}]
  (try
    ;; Dispatch wave first
    (let [dispatch-result (handle-dispatch-drone-wave params)
          dispatch-data (json/read-str (:text dispatch-result) :key-fn keyword)]

      (if (:error dispatch-data)
        dispatch-result
        ;; Add validation info to response
        {:type "text"
         :text (json/write-str (assoc dispatch-data
                                      :validation_mode true
                                      :lint_level (or lint_level :error)
                                      :message (str (:message dispatch-data)
                                                    " Post-apply validation enabled.")))}))

    (catch Exception e
      (log/error e "dispatch_validated_wave failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)})})))

;;; ============================================================
;;; Wave Cancellation
;;; ============================================================

(defn handle-cancel-wave
  "Handle wave cancellation request.

   Parameters:
     wave_id - Wave ID to cancel
     reason  - Cancellation reason

   Returns:
     JSON with cancellation result."
  [{:keys [wave_id reason]}]
  (try
    (when-not wave_id
      (throw (ex-info "wave_id is required" {})))

    (when-let [wave (ds/get-wave wave_id)]
      (let [plan-id (:wave/plan wave)]
        (ds/complete-wave! wave_id :cancelled)
        (ds/update-plan-status! plan-id :cancelled)
        {:type "text"
         :text (json/write-str {:cancelled true
                                :wave_id wave_id
                                :reason (or reason "User requested")})}))

    (catch Exception e
      (log/error e "cancel_wave failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)})})))
