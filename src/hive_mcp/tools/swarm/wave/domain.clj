(ns hive-mcp.tools.swarm.wave.domain
  "Domain value objects and validated constructors for wave execution."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const default-concurrency
  "Default max concurrent drones."
  3)

(def ^:const drone-timeout-ms
  "Timeout for individual drone execution (10 minutes)."
  600000)

(def ^:const keepalive-interval-ms
  "Interval between keepalive messages during blocking batch execution."
  15000)

(def nrepl-retry-config
  "Retry configuration for nREPL transient failures."
  {:max-retries 2
   :initial-delay-ms 500
   :backoff-multiplier 2
   :max-delay-ms 5000})

(def conflict-retry-config
  "Retry configuration for file conflict errors with longer waits."
  {:max-retries 5
   :initial-delay-ms 2000
   :backoff-multiplier 1.5
   :max-delay-ms 10000})

(def openrouter-retry-config
  "Retry configuration for OpenRouter API errors with model fallback."
  {:max-retries 3
   :initial-delay-ms 1000
   :backoff-multiplier 2.0
   :max-delay-ms 30000
   :jitter-factor 0.2})

(defrecord WaveSpec
           [plan-id concurrency trace cwd skip-auto-apply wave-id on-complete backend model seeds
            ctx-refs kg-node-ids])

(defn ->wave-spec
  "Create a validated WaveSpec."
  [{:keys [plan-id concurrency trace cwd skip-auto-apply wave-id on-complete backend model seeds
           ctx-refs kg-node-ids]
    :or {concurrency default-concurrency
         trace true
         skip-auto-apply false}}]
  (when (str/blank? plan-id)
    (throw (ex-info "WaveSpec requires :plan-id"
                    {:error-type :validation
                     :field :plan-id})))
  (map->WaveSpec {:plan-id plan-id
                  :concurrency concurrency
                  :trace trace
                  :cwd cwd
                  :skip-auto-apply skip-auto-apply
                  :wave-id wave-id
                  :on-complete on-complete
                  :backend backend
                  :model model
                  :seeds (when (seq seeds) (vec seeds))
                  :ctx-refs (when (seq ctx-refs) ctx-refs)
                  :kg-node-ids (when (seq kg-node-ids) (vec kg-node-ids))}))

(defn wave-spec?
  "Check if value is a WaveSpec."
  [x]
  (instance? WaveSpec x))

(defrecord BatchSpec
           [items preset cwd wave-id skip-auto-apply trace batch-num total-batches])

(defn ->batch-spec
  "Create a validated BatchSpec for a single batch execution."
  [{:keys [items preset cwd wave-id skip-auto-apply trace batch-num total-batches]
    :or {skip-auto-apply false
         trace true
         batch-num 1
         total-batches 1}}]
  (when (empty? items)
    (throw (ex-info "BatchSpec requires non-empty :items"
                    {:error-type :validation
                     :field :items})))
  (when (str/blank? wave-id)
    (throw (ex-info "BatchSpec requires :wave-id"
                    {:error-type :validation
                     :field :wave-id})))
  (map->BatchSpec {:items (vec items)
                   :preset preset
                   :cwd cwd
                   :wave-id wave-id
                   :skip-auto-apply skip-auto-apply
                   :trace trace
                   :batch-num batch-num
                   :total-batches total-batches}))

(defrecord TaskResult
           [success item-id result error proposed-diff-ids retry-info])

(defn ->task-result
  "Create a TaskResult."
  [{:keys [success item-id result error proposed-diff-ids retry-info]}]
  (when (nil? success)
    (throw (ex-info "TaskResult requires :success"
                    {:error-type :validation
                     :field :success})))
  (map->TaskResult {:success success
                    :item-id item-id
                    :result result
                    :error error
                    :proposed-diff-ids (vec (or proposed-diff-ids []))
                    :retry-info retry-info}))

(defn success-result
  "Create a successful TaskResult."
  [item-id result & [proposed-diff-ids]]
  (->task-result {:success true
                  :item-id item-id
                  :result result
                  :proposed-diff-ids proposed-diff-ids}))

(defn failure-result
  "Create a failed TaskResult."
  [item-id error & [retry-info]]
  (->task-result {:success false
                  :item-id item-id
                  :error error
                  :retry-info retry-info}))

(defrecord WaveResult
           [wave-id plan-id status completed-count failed-count
            batch-count success-rate duration-ms failed-items])

(defn ->wave-result
  "Create a WaveResult."
  [{:keys [wave-id plan-id status completed-count failed-count
           batch-count success-rate duration-ms failed-items]}]
  (map->WaveResult {:wave-id wave-id
                    :plan-id plan-id
                    :status status
                    :completed-count (or completed-count 0)
                    :failed-count (or failed-count 0)
                    :batch-count (or batch-count 0)
                    :success-rate (or success-rate 1.0)
                    :duration-ms duration-ms
                    :failed-items (vec (or failed-items []))}))

(defn calculate-status
  "Calculate wave status from completed/failed counts."
  [completed-count failed-count]
  (cond
    (zero? failed-count) :completed
    (zero? completed-count) :failed
    :else :partial-failure))

(defn calculate-success-rate
  "Calculate success rate from counts."
  [completed-count failed-count]
  (let [total (+ completed-count failed-count)]
    (if (pos? total)
      (/ (double completed-count) total)
      1.0)))

(defrecord WaveContext
           [wave-id plan-id start-time items batches
            completed-count failed-count current-batch])

(defn ->wave-context
  "Create a mutable wave execution context."
  [wave-spec wave-id items]
  (map->WaveContext {:wave-id wave-id
                     :plan-id (:plan-id wave-spec)
                     :start-time (System/nanoTime)
                     :items items
                     :batches nil
                     :completed-count 0
                     :failed-count 0
                     :current-batch 0}))

(defn elapsed-seconds
  "Calculate elapsed seconds from context start."
  [ctx]
  (/ (- (System/nanoTime) (:start-time ctx)) 1e9))

(defn update-counts
  "Update completed/failed counts in context."
  [ctx {:keys [completed failed]}]
  (-> ctx
      (update :completed-count + (or completed 0))
      (update :failed-count + (or failed 0))))
