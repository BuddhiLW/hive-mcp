(ns hive-mcp.tools.swarm.wave.phases
  "Phase-based wave execution orchestration.

   Extracted from wave.clj execute-wave! to reduce complexity
   (SOLID-S: Single Responsibility, cc < 15 per phase).

   Phases:
   1. pre-flight  - Validation, claim cleanup, cost tracking
   2. register    - Edit registration, dependency inference
   3. batching    - Compute conflict-free batches
   4. execute     - Batch-by-batch with bounded concurrency
   5. complete    - Metrics, status updates
   6. cleanup     - Always runs (transient state reset)

   Each phase is a pure function or has clear side-effect boundaries.
   The orchestrator (run-wave!) composes them with proper error handling."
  (:require [hive-mcp.tools.swarm.wave.domain :as domain]
            [hive-mcp.tools.swarm.wave.validation :as validation]
            [hive-mcp.tools.swarm.wave.batching :as batching]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.agent.cost :as cost]
            [hive-mcp.events.core :as ev]
            [hive-mcp.telemetry.prometheus :as prom]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Phase 1: Pre-Flight - Validation & Setup
;;; ============================================================

(defn phase:pre-flight!
  "Phase 1: Pre-flight validation and setup.

   Side effects:
   - Creates parent directories for task files
   - Cleans up stale file claims
   - Starts cost tracking for wave

   Arguments:
     wave-spec - WaveSpec record
     items     - Pending change items
     wave-id   - Wave identifier

   Returns:
     Map with :validation-result :stale-claims-cleaned

   Throws:
     ex-info on validation failure

   CLARITY-I: Guard inputs before processing."
  [_wave-spec items wave-id]
  (log/info "Phase 1: Pre-flight starting"
            {:wave-id wave-id :item-count (count items)})

  ;; 1. Start cost tracking (CLARITY-T: Budget management)
  (cost/start-wave-tracking! wave-id)

  ;; 2. Path validation and directory creation
  (let [tasks (mapv (fn [item]
                      {:file (:change-item/file item)
                       :task (:change-item/task item)})
                    items)
        validation-result (validation/pre-flight-validation! tasks)]

    (log/info "Pre-flight validation passed"
              {:dirs-created (:dirs-created validation-result)})

    ;; 3. Cleanup stale claims (CLARITY-Y: Unblock ghost claims)
    (let [stale-cleaned (try
                          (ds/cleanup-stale-claims!)
                          (catch Exception e
                            (log/warn "Stale claim cleanup failed (non-fatal):"
                                      (.getMessage e))
                            0))]
      (when (pos? stale-cleaned)
        (log/info "Cleaned up" stale-cleaned "stale claims before wave" wave-id))

      {:validation-result validation-result
       :stale-claims-cleaned stale-cleaned})))

;;; ============================================================
;;; Phase 2: Register - Edit & Dependency Setup
;;; ============================================================

(defn phase:register!
  "Phase 2: Register edits and infer dependencies.

   Side effects:
   - Registers edits in logic database
   - Infers test → source dependencies
   - Updates plan status to :in-progress

   Arguments:
     plan-id - Plan identifier
     items   - Change items

   Returns:
     Map with :edits-registered :dependencies-inferred

   CLARITY-L: Clear layer boundary for logic operations."
  [plan-id items]
  (log/info "Phase 2: Registering edits" {:plan-id plan-id})

  ;; 1. Reset stale edits before registering new ones
  (batching/reset-edits!)

  ;; 2. Register all edits
  (let [edits-count (batching/register-edits! items)]
    (log/info "Registered" edits-count "edits for batch computation")

    ;; 3. Infer test dependencies
    (let [deps-count (batching/infer-test-dependencies! items)]

      ;; 4. Update plan status
      (ds/update-plan-status! plan-id :in-progress)

      {:edits-registered edits-count
       :dependencies-inferred deps-count})))

;;; ============================================================
;;; Phase 3: Batching - Compute Safe Batches
;;; ============================================================

(defn phase:compute-batches
  "Phase 3: Compute conflict-free batches.

   Pure function - no side effects.

   Arguments:
     items - Change items

   Returns:
     Map with :batches :item-map :batch-count"
  [items]
  (log/info "Phase 3: Computing batches" {:item-count (count items)})

  (let [edit-ids (mapv :change-item/id items)
        batches (batching/compute-batches edit-ids)
        item-map (into {} (map (juxt :change-item/id identity) items))

        ;; Validation: Batches should not be empty if items exist
        _ (when (empty? batches)
            (log/warn "Empty batches computed for" (count items) "items - forcing single batch"))

        effective-batches (if (empty? batches) [edit-ids] batches)]

    (log/info "Computed" (count effective-batches) "batches for" (count items) "items")

    {:batches effective-batches
     :item-map item-map
     :batch-count (count effective-batches)}))

;;; ============================================================
;;; Phase 4: Execute - Batch-by-Batch Processing
;;; ============================================================

(defn phase:execute-batches!
  "Phase 4: Execute all batches sequentially.

   Side effects:
   - Executes drone tasks via execute-fn
   - Updates item statuses in DataScript
   - Emits wave/batch events

   Arguments:
     wave-spec  - WaveSpec record
     wave-id    - Wave identifier
     batches    - Computed batches
     item-map   - Edit-id → item map
     execute-fn - Function to execute each item

   Returns:
     Map with :total-completed :total-failed :batch-results"
  [wave-spec wave-id batches item-map execute-fn]
  (log/info "Phase 4: Executing batches"
            {:wave-id wave-id :batch-count (count batches)})

  ;; Emit wave start event
  (when (:trace wave-spec)
    (ev/dispatch [:wave/start {:plan-id (:plan-id wave-spec)
                               :wave-id wave-id
                               :item-count (count item-map)
                               :batch-count (count batches)}]))

  ;; Execute all batches
  (batching/execute-all-batches!
   batches item-map wave-spec wave-id execute-fn))

;;; ============================================================
;;; Phase 5: Complete - Metrics & Status Updates
;;; ============================================================

(defn phase:complete!
  "Phase 5: Complete wave with metrics and status updates.

   Side effects:
   - Updates wave and plan statuses
   - Records Prometheus metrics
   - Completes cost tracking
   - Emits wave/complete event

   Arguments:
     wave-spec       - WaveSpec record
     wave-id         - Wave identifier
     execution-result - Result from phase:execute-batches!
     start-time      - Wave start time (nanos)
     items           - Original items (for failure details)

   Returns:
     WaveResult record"
  [wave-spec wave-id execution-result start-time items]
  (let [{:keys [total-completed total-failed batch-results]} execution-result
        {:keys [plan-id trace]} wave-spec
        _total-items (+ total-completed total-failed)

        ;; Calculate metrics
        success-rate (domain/calculate-success-rate total-completed total-failed)
        status (domain/calculate-status total-completed total-failed)
        duration-seconds (/ (- (System/nanoTime) start-time) 1e9)

        ;; Gather failed item details
        failed-items (->> items
                          (filter #(= :failed (:change-item/status %)))
                          (mapv #(select-keys % [:change-item/id :change-item/file])))]

    (log/info "Phase 5: Completing wave"
              {:wave-id wave-id
               :status status
               :completed total-completed
               :failed total-failed
               :success-rate success-rate
               :duration-seconds duration-seconds})

    ;; CLEANUP: Reset all transient data (edits + task-files)
    (logic/reset-all-transient!)

    ;; CLEANUP: Release stale claims from this wave
    (try
      (ds/cleanup-stale-claims!)
      (catch Exception e
        (log/warn "Post-wave claim cleanup failed (non-fatal):" (.getMessage e))))

    ;; Record metrics (CLARITY-T)
    (prom/set-wave-success-rate! success-rate)
    (dotimes [_ total-completed] (prom/inc-wave-items! :success))
    (dotimes [_ total-failed] (prom/inc-wave-items! :failed))
    (prom/observe-wave-duration! duration-seconds)

    (when (pos? total-failed)
      (prom/inc-wave-failures! wave-id
                               (if (= total-completed 0) :all-failed :partial-failure)))

    ;; Update DataScript statuses
    (ds/complete-wave! wave-id status)
    (ds/update-plan-status! plan-id (if (pos? total-failed) :failed :completed))

    ;; Complete cost tracking
    (let [wave-cost (cost/complete-wave-tracking! wave-id)]
      (log/info {:event :wave/cost-summary
                 :wave-id wave-id
                 :total-tokens (:total-tokens wave-cost)
                 :drone-count (:drone-count wave-cost)}))

    ;; Emit completion event
    (when trace
      (ev/dispatch [:wave/complete {:plan-id plan-id
                                    :wave-id wave-id
                                    :results {:completed total-completed
                                              :failed total-failed
                                              :batches (count batch-results)}}]))

    ;; Invoke callback if provided
    (when-let [on-complete (:on-complete wave-spec)]
      (try
        (on-complete wave-id)
        (catch Exception e
          (log/warn "Wave on-complete callback failed:" (ex-message e)))))

    ;; Return WaveResult
    (domain/->wave-result
     {:wave-id wave-id
      :plan-id plan-id
      :status status
      :completed-count total-completed
      :failed-count total-failed
      :batch-count (count batch-results)
      :success-rate success-rate
      :duration-ms (long (* duration-seconds 1000))
      :failed-items failed-items})))

;;; ============================================================
;;; Phase 6: Cleanup - Always Runs
;;; ============================================================

(defn phase:cleanup!
  "Phase 6: Cleanup (always runs via finally).

   Side effects:
   - Resets transient logic state

   Arguments:
     wave-id - Wave identifier

   Note: Called in finally block, must not throw."
  [wave-id]
  (try
    (log/info "Phase 6: Cleanup" {:wave-id wave-id})
    (logic/reset-all-transient!)
    (catch Exception e
      (log/error "Cleanup phase failed (non-fatal):" (ex-message e)))))

;;; ============================================================
;;; Error Handling Phase
;;; ============================================================

(defn phase:handle-error!
  "Handle wave execution error.

   Side effects:
   - Marks wave as failed
   - Records failure metrics
   - Emits error event

   Arguments:
     wave-spec  - WaveSpec record
     wave-id    - Wave identifier
     exception  - Caught exception
     start-time - Wave start time (nanos)

   Returns:
     nil (rethrows after handling)"
  [wave-spec wave-id exception start-time]
  (let [{:keys [plan-id trace]} wave-spec
        duration-seconds (/ (- (System/nanoTime) start-time) 1e9)]

    (log/error {:event :wave/error
                :wave-id wave-id
                :plan-id plan-id
                :duration-seconds duration-seconds
                :error (ex-message exception)
                :error-type (type exception)})

    ;; Record failure
    (prom/inc-wave-failures! wave-id :error)

    ;; Update statuses
    (try
      (ds/complete-wave! wave-id :failed)
      (ds/update-plan-status! plan-id :failed)
      (catch Throwable t
        (log/error "Failed to mark wave as failed:" (ex-message t))))

    ;; Complete cost tracking (even on failure)
    (try
      (cost/complete-wave-tracking! wave-id)
      (catch Exception _))

    ;; Emit error event
    (when trace
      (ev/dispatch [:wave/error {:wave-id wave-id
                                 :plan-id plan-id
                                 :error (ex-message exception)}]))))
