(ns hive-mcp.tools.swarm.wave.phases
  "Phase-based wave execution orchestration with clear side-effect boundaries."
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

(defn phase:pre-flight!
  "Phase 1: Pre-flight validation, directory creation, and stale claim cleanup."
  [_wave-spec items wave-id]
  (log/info "Phase 1: Pre-flight starting"
            {:wave-id wave-id :item-count (count items)})

  (cost/start-wave-tracking! wave-id)

  (let [tasks (mapv (fn [item]
                      {:file (:change-item/file item)
                       :task (:change-item/task item)})
                    items)
        validation-result (validation/pre-flight-validation! tasks)]

    (log/info "Pre-flight validation passed"
              {:dirs-created (:dirs-created validation-result)})

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

(defn phase:register!
  "Phase 2: Register edits and infer dependencies."
  [plan-id items]
  (log/info "Phase 2: Registering edits" {:plan-id plan-id})

  (batching/reset-edits!)

  (let [edits-count (batching/register-edits! items)]
    (log/info "Registered" edits-count "edits for batch computation")

    (let [deps-count (batching/infer-test-dependencies! items)]

      (ds/update-plan-status! plan-id :in-progress)

      {:edits-registered edits-count
       :dependencies-inferred deps-count})))

(defn phase:compute-batches
  "Phase 3: Compute conflict-free batches (pure function)."
  [items]
  (log/info "Phase 3: Computing batches" {:item-count (count items)})

  (let [edit-ids (mapv :change-item/id items)
        batches (batching/compute-batches edit-ids)
        item-map (into {} (map (juxt :change-item/id identity) items))

        _ (when (empty? batches)
            (log/warn "Empty batches computed for" (count items) "items - forcing single batch"))

        effective-batches (if (empty? batches) [edit-ids] batches)]

    (log/info "Computed" (count effective-batches) "batches for" (count items) "items")

    {:batches effective-batches
     :item-map item-map
     :batch-count (count effective-batches)}))

(defn phase:execute-batches!
  "Phase 4: Execute all batches sequentially with bounded concurrency."
  [wave-spec wave-id batches item-map execute-fn]
  (log/info "Phase 4: Executing batches"
            {:wave-id wave-id :batch-count (count batches)})

  (when (:trace wave-spec)
    (ev/dispatch [:wave/start {:plan-id (:plan-id wave-spec)
                               :wave-id wave-id
                               :item-count (count item-map)
                               :batch-count (count batches)}]))

  (batching/execute-all-batches!
   batches item-map wave-spec wave-id execute-fn))

(defn phase:complete!
  "Phase 5: Complete wave with metrics, status updates, and cost tracking."
  [wave-spec wave-id execution-result start-time items]
  (let [{:keys [total-completed total-failed batch-results]} execution-result
        {:keys [plan-id trace]} wave-spec
        _total-items (+ total-completed total-failed)

        success-rate (domain/calculate-success-rate total-completed total-failed)
        status (domain/calculate-status total-completed total-failed)
        duration-seconds (/ (- (System/nanoTime) start-time) 1e9)

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

    (logic/reset-all-transient!)

    (try
      (ds/cleanup-stale-claims!)
      (catch Exception e
        (log/warn "Post-wave claim cleanup failed (non-fatal):" (.getMessage e))))

    (prom/set-wave-success-rate! success-rate)
    (dotimes [_ total-completed] (prom/inc-wave-items! :success))
    (dotimes [_ total-failed] (prom/inc-wave-items! :failed))
    (prom/observe-wave-duration! duration-seconds)

    (when (pos? total-failed)
      (prom/inc-wave-failures! wave-id
                               (if (= total-completed 0) :all-failed :partial-failure)))

    (ds/complete-wave! wave-id status)
    (ds/update-plan-status! plan-id (if (pos? total-failed) :failed :completed))

    (let [wave-cost (cost/complete-wave-tracking! wave-id)]
      (log/info {:event :wave/cost-summary
                 :wave-id wave-id
                 :total-tokens (:total-tokens wave-cost)
                 :drone-count (:drone-count wave-cost)}))

    (when trace
      (ev/dispatch [:wave/complete {:plan-id plan-id
                                    :wave-id wave-id
                                    :results {:completed total-completed
                                              :failed total-failed
                                              :batches (count batch-results)}}]))

    (when-let [on-complete (:on-complete wave-spec)]
      (try
        (on-complete wave-id)
        (catch Exception e
          (log/warn "Wave on-complete callback failed:" (ex-message e)))))

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

(defn phase:cleanup!
  "Phase 6: Cleanup transient state (always runs via finally, must not throw)."
  [wave-id]
  (try
    (log/info "Phase 6: Cleanup" {:wave-id wave-id})
    (logic/reset-all-transient!)
    (catch Exception e
      (log/error "Cleanup phase failed (non-fatal):" (ex-message e)))))

(defn phase:handle-error!
  "Handle wave execution error with status updates and metrics."
  [wave-spec wave-id exception start-time]
  (let [{:keys [plan-id trace]} wave-spec
        duration-seconds (/ (- (System/nanoTime) start-time) 1e9)]

    (log/error {:event :wave/error
                :wave-id wave-id
                :plan-id plan-id
                :duration-seconds duration-seconds
                :error (ex-message exception)
                :error-type (type exception)})

    (prom/inc-wave-failures! wave-id :error)

    (try
      (ds/complete-wave! wave-id :failed)
      (ds/update-plan-status! plan-id :failed)
      (catch Throwable t
        (log/error "Failed to mark wave as failed:" (ex-message t))))

    (try
      (cost/complete-wave-tracking! wave-id)
      (catch Exception _))

    (when trace
      (ev/dispatch [:wave/error {:wave-id wave-id
                                 :plan-id plan-id
                                 :error (ex-message exception)}]))))
