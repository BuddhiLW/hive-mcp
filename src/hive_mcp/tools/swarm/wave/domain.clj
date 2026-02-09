(ns hive-mcp.tools.swarm.wave.domain
  "Domain value objects for wave execution.

   Extracted from wave.clj to provide type-safe, immutable data structures
   with validated constructors (SOLID-O, CLARITY-I, CLARITY-R).

   Records:
   - WaveSpec: Immutable wave configuration
   - BatchSpec: Single batch execution spec
   - TaskResult: Individual task outcome
   - WaveResult: Overall wave outcome

   All constructors validate inputs (CLARITY-I: Inputs guarded)."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Constants - Retry Configurations
;;; ============================================================

(def ^:const default-concurrency
  "Default max concurrent drones."
  3)

(def ^:const drone-timeout-ms
  "Timeout for individual drone execution (10 minutes)."
  600000)

(def ^:const keepalive-interval-ms
  "Interval between keepalive messages during blocking batch execution.
   Prevents bb-mcp nREPL socket timeout by writing to *out* periodically."
  15000)

(def nrepl-retry-config
  "Retry configuration for nREPL transient failures.

   CLARITY-Y: Exponential backoff with bounded retries.
   Short delays since nREPL recovers quickly."
  {:max-retries 2          ; 1 initial + 2 retries = 3 total attempts
   :initial-delay-ms 500
   :backoff-multiplier 2
   :max-delay-ms 5000})

(def conflict-retry-config
  "Retry configuration for file conflict errors.

   File conflicts need LONGER waits than nREPL errors because we're waiting
   for another drone to finish its work. Uses longer delays and more retries.

   RACE CONDITION FIX: Gives holding drone time to complete and release claims."
  {:max-retries 5          ; More retries - conflicts can take time to clear
   :initial-delay-ms 2000  ; Start at 2s - give holding drone time
   :backoff-multiplier 1.5 ; Gentler backoff
   :max-delay-ms 10000})   ; Cap at 10s between retries

(def openrouter-retry-config
  "Retry configuration for OpenRouter API errors.

   Handles: rate limits, model unavailability, transient server errors.
   Uses model fallback strategy when available."
  {:max-retries 3
   :initial-delay-ms 1000
   :backoff-multiplier 2.0
   :max-delay-ms 30000
   :jitter-factor 0.2})

;;; ============================================================
;;; WaveSpec - Immutable Wave Configuration
;;; ============================================================

(defrecord WaveSpec
           [plan-id concurrency trace cwd skip-auto-apply wave-id on-complete backend model]
  ;; plan-id         - Plan identifier (required)
  ;; concurrency     - Max concurrent drones (default: 3)
  ;; trace           - Emit events (default: true)
  ;; cwd             - Working directory override
  ;; skip-auto-apply - Review mode (default: false)
  ;; wave-id         - Pre-created wave ID (optional)
  ;; on-complete     - Callback on completion (optional)
  ;; backend         - Drone execution backend keyword (optional, nil = mode-dependent default)
  ;; model           - Override model for all drones (optional, bypasses smart routing)
  )

(defn ->wave-spec
  "Create a validated WaveSpec.

   Arguments:
     opts - Map with:
       :plan-id         - Plan identifier (required)
       :concurrency     - Max concurrent drones (default: 3)
       :trace           - Emit events (default: true)
       :cwd             - Working directory override
       :skip-auto-apply - Review mode (default: false)
       :wave-id         - Pre-created wave ID
       :on-complete     - Callback fn
       :backend         - Drone execution backend keyword (optional)
                          One of: :openrouter, :hive-agent, :legacy-loop, :sdk-drone
                          nil = mode-dependent default
       :model           - Override model for all drones (optional)
                          Bypasses smart routing when provided

   Returns:
     WaveSpec record

   Throws:
     ex-info if :plan-id is missing (CLARITY-I)"
  [{:keys [plan-id concurrency trace cwd skip-auto-apply wave-id on-complete backend model]
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
                  :model model}))

(defn wave-spec?
  "Check if value is a WaveSpec."
  [x]
  (instance? WaveSpec x))

;;; ============================================================
;;; BatchSpec - Single Batch Configuration
;;; ============================================================

(defrecord BatchSpec
           [items preset cwd wave-id skip-auto-apply trace batch-num total-batches]
  ;; items           - Vector of change items for this batch
  ;; preset          - Drone preset
  ;; cwd             - Working directory
  ;; wave-id         - Parent wave ID
  ;; skip-auto-apply - Review mode
  ;; trace           - Emit events
  ;; batch-num       - Current batch number (1-indexed)
  ;; total-batches   - Total number of batches
  )

(defn ->batch-spec
  "Create a BatchSpec for a single batch execution.

   Arguments:
     opts - Map with:
       :items           - Vector of change items (required, non-empty)
       :preset          - Drone preset (required)
       :cwd             - Working directory
       :wave-id         - Parent wave ID (required)
       :skip-auto-apply - Review mode (default: false)
       :trace           - Emit events (default: true)
       :batch-num       - Batch number (default: 1)
       :total-batches   - Total batches (default: 1)

   Returns:
     BatchSpec record"
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

;;; ============================================================
;;; TaskResult - Individual Task Outcome
;;; ============================================================

(defrecord TaskResult
           [success item-id result error proposed-diff-ids retry-info]
  ;; success          - Boolean success flag
  ;; item-id          - Change item ID
  ;; result           - Result data on success
  ;; error            - Error message on failure
  ;; proposed-diff-ids - Diff IDs for review mode
  ;; retry-info       - Retry attempt details
  )

(defn ->task-result
  "Create a TaskResult.

   Arguments:
     opts - Map with:
       :success          - Boolean (required)
       :item-id          - Item identifier (required)
       :result           - Result data
       :error            - Error message
       :proposed-diff-ids - Diff IDs
       :retry-info       - Retry details

   Returns:
     TaskResult record"
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

;;; ============================================================
;;; WaveResult - Overall Wave Outcome
;;; ============================================================

(defrecord WaveResult
           [wave-id plan-id status completed-count failed-count
            batch-count success-rate duration-ms failed-items]
  ;; wave-id         - Wave identifier
  ;; plan-id         - Plan identifier
  ;; status          - :completed | :partial-failure | :failed | :cancelled
  ;; completed-count - Number of successful items
  ;; failed-count    - Number of failed items
  ;; batch-count     - Number of batches executed
  ;; success-rate    - Ratio of completed / total
  ;; duration-ms     - Total execution time
  ;; failed-items    - Details of failed items
  )

(defn ->wave-result
  "Create a WaveResult.

   Arguments:
     opts - Map with all fields

   Returns:
     WaveResult record"
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
  "Calculate wave status from counts.

   Returns:
     :completed       - All items succeeded
     :partial-failure - Some items failed
     :failed          - All items failed"
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

;;; ============================================================
;;; WaveContext - Mutable Runtime State
;;; ============================================================

(defrecord WaveContext
           [wave-id plan-id start-time items batches
            completed-count failed-count current-batch])

(defn ->wave-context
  "Create a mutable wave execution context.

   Arguments:
     wave-spec - WaveSpec record
     wave-id   - Assigned wave ID
     items     - All pending items

   Returns:
     WaveContext record with atom for mutable state"
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
