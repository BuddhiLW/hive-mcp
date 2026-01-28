(ns hive-mcp.tools.swarm.wave
  "Wave execution for batch drone dispatch.

   Executes multiple drone tasks in parallel with bounded concurrency.
   Integrates with DataScript for state tracking and hive-events for
   lifecycle events.

   Usage:
   1. Create plan with tasks
   2. Execute wave (async, bounded concurrency)
   3. Monitor via events or get-wave-status

   SOLID: SRP - Wave orchestration only
   CLARITY: A - Architectural performance via bounded concurrency"
  (:require [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.events.core :as ev]
            [hive-mcp.agent :as agent]
            [hive-mcp.agent.cost :as cost]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.telemetry.prometheus :as prom]
            [hive-mcp.telemetry.health :as health]
            [clojure.core.async :as async :refer [go go-loop <! >! <!! chan close!]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Constants
;;; =============================================================================

(def ^:const default-concurrency
  "Default max concurrent drones."
  3)

(def ^:const drone-timeout-ms
  "Timeout for individual drone execution (10 minutes)."
  600000)

;;; =============================================================================
;;; Pre-flight Validation (P0 + P2)
;;; =============================================================================

(defn ensure-parent-dirs!
  "Create parent directories for all task files.
   Called during pre-flight to prevent drone failures from missing directories.

   Arguments:
     tasks - Collection of maps with :file key

   CLARITY-I: Guard inputs before processing."
  [tasks]
  (doseq [{:keys [file]} tasks]
    (when file
      (io/make-parents file))))

(defn- valid-parent-path?
  "Check if parent directory exists.
   Returns true if:
   - file-path is nil (no validation needed)
   - parent directory exists

   Note: This is strict validation. Use ensure-parent-dirs! first
   to create directories, then validate remaining issues."
  [file-path]
  (if (nil? file-path)
    true
    (let [parent (.getParentFile (io/file file-path))]
      (or (nil? parent)
          (.exists parent)))))

(defn validate-task-paths
  "Validate all task paths have accessible parent directories.
   Throws ex-info with :invalid-paths if any paths are invalid.

   Arguments:
     tasks - Collection of maps with :file key

   CLARITY-I: Fail fast at boundaries."
  [tasks]
  (let [invalid (->> tasks
                     (map :file)
                     (remove nil?)
                     (remove valid-parent-path?)
                     (vec))]
    (when (seq invalid)
      (throw (ex-info "Invalid paths in wave"
                      {:invalid-paths invalid})))))

;;; =============================================================================
;;; Edit Registration and Dependency Inference
;;; =============================================================================

(defn- register-edits!
  "Register all plan items as edits in the logic database."
  [items]
  (doseq [{:keys [change-item/id change-item/file]} items]
    (logic/add-edit! id file :modify)))

(defn- infer-test-dependencies!
  "Infer dependencies between source and test files.

   Heuristics:
   - foo_test.clj depends on foo.clj
   - test/foo_test.clj depends on src/foo.clj"
  [items]
  (let [;; Build map: base-name → edit-id for source files
        source-map (->> items
                        (remove #(str/includes? (:change-item/file %) "_test"))
                        (reduce (fn [m {:keys [change-item/id change-item/file]}]
                                  (let [base (-> file
                                                 (str/replace #"^.*/src/" "")
                                                 (str/replace #"\.clj[sx]?$" ""))]
                                    (assoc m base id)))
                                {}))
        ;; Find test files
        test-items (filter #(str/includes? (:change-item/file %) "_test") items)]

    ;; Link test to source
    (doseq [{:keys [change-item/id change-item/file]} test-items]
      (let [base (-> file
                     (str/replace #"^.*/test/" "")
                     (str/replace #"_test\.clj[sx]?$" ""))]
        (when-let [source-id (get source-map base)]
          (logic/add-edit-dependency! source-id id)
          (log/debug "Inferred dependency:" source-id "→" id))))))

;;; =============================================================================
;;; Plan Management (delegates to datascript)
;;; =============================================================================

(defn create-plan!
  "Create a change plan with multiple tasks.

   Arguments:
     tasks  - Collection of {:file \"path\" :task \"description\"}
     preset - Optional drone preset (default: \"drone-worker\")

   Returns:
     The generated plan-id"
  [tasks & [preset]]
  (ds/create-plan! tasks (or preset "drone-worker")))

(defn get-pending-items
  "Get pending items for a plan."
  [plan-id]
  (ds/get-pending-items plan-id))

(defn get-plan-items
  "Get all items for a plan."
  [plan-id]
  (ds/get-plan-items plan-id))

;;; =============================================================================
;;; Drone Execution (with nREPL retry for transient failures)
;;; =============================================================================

(def ^:private nrepl-error-patterns
  "Patterns that indicate an nREPL transient failure (worth retrying).

   CLARITY-Y: Yield safe failure - distinguish transient from permanent errors."
  [#"(?i)connection.*refused"
   #"(?i)socket.*closed"
   #"(?i)nrepl.*not.*available"
   #"(?i)nrepl.*disconnect"
   #"(?i)timeout"
   #"(?i)network.*unreachable"
   #"(?i)connection.*reset"])

(defn- transient-nrepl-error?
  "Check if error message indicates a transient nREPL failure.

   Returns true if the error matches known transient patterns."
  [error-msg]
  (when error-msg
    (some #(re-find % error-msg) nrepl-error-patterns)))

(def ^:private drone-retry-config
  "Retry configuration for drone tasks.

   CLARITY-Y: Exponential backoff with bounded retries."
  {:max-retries 2    ; 1 initial + 2 retries = 3 total attempts
   :initial-delay-ms 500
   :backoff-multiplier 2
   :max-delay-ms 5000})

(defn- execute-drone-task-once
  "Execute a single drone task without retry.

   Arguments:
     item            - Change item map
     preset          - Drone preset
     cwd             - Optional working directory override for path resolution
     skip-auto-apply - When true, drone proposes diffs without auto-applying
     wave-id         - Wave ID for tagging proposed diffs

   Returns:
     Map with :success :result or :error, and :proposed-diff-ids when skip-auto-apply"
  [{:keys [change-item/id change-item/file change-item/task]} preset cwd skip-auto-apply wave-id]
  (let [;; L1 Disc: Proactively surface staleness warnings (zero noise for fresh files)
        disc-warnings (try (kg-disc/staleness-warnings [file]) (catch Exception _ nil))
        disc-notice (kg-disc/format-staleness-warnings disc-warnings)
        task-str (if disc-notice
                   (str disc-notice "File: " file "\n\nTask: " task)
                   (str "File: " file "\n\nTask: " task))
        result (agent/delegate-drone!
                {:task task-str
                 :files [file]
                 :preset preset
                 :trace true
                 :cwd cwd
                 :skip-auto-apply skip-auto-apply
                 :wave-id wave-id})]
    (if (= :completed (:status result))
      (cond-> {:success true
               :item-id id
               :result (:result result)}
        ;; Include proposed diff IDs when in review mode
        (seq (:proposed-diff-ids result))
        (assoc :proposed-diff-ids (:proposed-diff-ids result)))
      {:success false
       :item-id id
       :error (or (:result result) "Drone execution failed")})))

(defn- execute-drone-task
  "Execute a single drone task with retry for transient nREPL failures.

   Arguments:
     item            - Change item map
     preset          - Drone preset
     cwd             - Optional working directory override for path resolution
     skip-auto-apply - When true, drone proposes diffs without auto-applying
     wave-id         - Wave ID for tagging proposed diffs

   Retry behavior:
     - Retries up to max-retries times for transient nREPL errors
     - Exponential backoff between retries
     - Permanent errors fail immediately without retry

   Returns:
     Map with :success :result or :error, and :proposed-diff-ids when skip-auto-apply"
  [{:keys [change-item/id change-item/file change-item/task] :as item} preset cwd skip-auto-apply wave-id]
  (let [{:keys [max-retries initial-delay-ms backoff-multiplier max-delay-ms]} drone-retry-config]
    (loop [attempt 1
           delay-ms initial-delay-ms]
      (log/info "Executing drone task for item:" id "file:" file "cwd:" cwd
                {:attempt attempt :max-attempts (inc max-retries) :review-mode skip-auto-apply})

      ;; Update status to dispatched (only on first attempt)
      (when (= attempt 1)
        (ds/update-item-status! id :dispatched))

      (let [result (try
                     (execute-drone-task-once item preset cwd skip-auto-apply wave-id)
                     (catch Exception e
                       {:success false
                        :item-id id
                        :error (.getMessage e)}))]

        (cond
          ;; Success - return result
          (:success result)
          (do
            (when (> attempt 1)
              (log/info "Drone task succeeded after" attempt "attempts for item:" id))
            result)

          ;; Failure - check if transient and we have retries left
          (and (transient-nrepl-error? (:error result))
               (<= attempt max-retries))
          (do
            (log/warn "Transient nREPL error for item:" id
                      {:attempt attempt
                       :error (:error result)
                       :retry-delay-ms delay-ms})
            ;; Emit health event for telemetry
            (try
              (health/emit-health-event! {:type :nrepl-disconnect
                                          :severity :warn
                                          :message (str "Transient nREPL error, retrying: " (:error result))
                                          :context {:item-id id :attempt attempt}
                                          :recoverable? true})
              (catch Exception _))
            (Thread/sleep delay-ms)
            (recur (inc attempt)
                   (min (* delay-ms backoff-multiplier) max-delay-ms)))

          ;; Permanent failure or retries exhausted
          :else
          (do
            ;; CLARITY-T: Structured JSON logging for Loki
            (log/error {:event :wave/item-failed
                        :item-id id
                        :file file
                        :task task
                        :attempts attempt
                        :error-type (if (transient-nrepl-error? (:error result))
                                      :nrepl-transient
                                      :permanent)
                        :error (:error result)
                        :retries-exhausted (> attempt 1)})
            result))))))

;;; =============================================================================
;;; nREPL Keepalive (CLARITY-T: Progress visibility)
;;; =============================================================================

(def ^:const keepalive-interval-ms
  "Interval between keepalive messages during blocking batch execution.
   Prevents bb-mcp nREPL socket timeout by writing to *out* periodically."
  15000)

(defn- blocking-take-with-keepalive!
  "Block on channel take with periodic keepalive messages to *out*.
   Prevents nREPL socket timeout by emitting progress every interval-ms.

   Arguments:
     ch          - core.async channel to take from
     interval-ms - Milliseconds between keepalive messages
     progress-fn - Zero-arg function that returns a progress string

   Returns:
     The value taken from the channel.

   CLARITY-T: Progress visibility for long-running wave operations.
   Without this, bb-mcp's nREPL socket times out after 30s of silence."
  [ch interval-ms progress-fn]
  (loop []
    (let [timeout-ch (async/timeout interval-ms)
          [result port] (async/alts!! [ch timeout-ch])]
      (if (= port ch)
        result
        (do
          (println (progress-fn))
          (flush)
          (recur))))))

;;; =============================================================================
;;; Wave Execution (core.async bounded concurrency)
;;; =============================================================================

(defn- item->work-unit
  "Convert item to work unit for async processing.

   Arguments:
     item            - Change item map
     preset          - Drone preset
     cwd             - Working directory override
     skip-auto-apply - When true, drones propose diffs without auto-applying
     wave-id         - Wave ID for tagging proposed diffs"
  [item preset cwd skip-auto-apply wave-id]
  {:item item
   :preset preset
   :cwd cwd
   :skip-auto-apply skip-auto-apply
   :wave-id wave-id})

;;; =============================================================================
;;; Batch Execution Helpers (SLAP: Extract functions for single abstraction level)
;;; =============================================================================

(defn- update-item-state!
  "Update DataScript state and emit events for a completed item.

   Arguments:
     result   - Map with :success :item-id :result/:error
     wave-id  - Wave ID for count updates
     trace    - Whether to emit events

   CLARITY-Y: Graceful degradation - best-effort status update on failure."
  [{:keys [success item-id result error]} wave-id trace]
  (try
    (if success
      (do
        (ds/update-item-status! item-id :completed {:result (str result)})
        (ds/update-wave-counts! wave-id {:completed 1 :active -1}))
      (do
        (ds/update-item-status! item-id :failed {:result error})
        (ds/update-wave-counts! wave-id {:failed 1 :active -1})))

    (when trace
      (ev/dispatch [:wave/item-done {:item-id item-id
                                     :status (if success :completed :failed)
                                     :wave-id wave-id}]))
    (catch Exception e
      (log/error e "Failed to update item status, marking as failed:" item-id)
      ;; Best-effort status update
      (try
        (ds/update-item-status! item-id :failed
                                {:result (str "Post-execution error: " (.getMessage e))})
        (ds/update-wave-counts! wave-id {:failed 1 :active -1})
        (catch Exception _)))))

(defn- spawn-workers!
  "Spawn worker goroutines for bounded concurrency.

   Arguments:
     work-ch   - Channel providing work units (includes skip-auto-apply and wave-id)
     result-ch - Channel to send results to
     wave-id   - Wave ID for state updates
     trace     - Whether to emit events
     concurrency - Max concurrent workers
     item-count  - Number of items (for worker count calculation)

   SOLID-SRP: Single responsibility - worker spawning only."
  [work-ch result-ch _wave-id trace concurrency item-count]
  (dotimes [_ (min concurrency item-count)]
    (go-loop []
      (when-let [{:keys [item preset cwd skip-auto-apply wave-id]} (<! work-ch)]
        (let [item-id (:change-item/id item)
              exec-result (try
                            (execute-drone-task item preset cwd skip-auto-apply wave-id)
                            (catch Exception e
                              ;; CLARITY-T: Structured JSON logging for Loki
                              (log/error {:event :wave/worker-exception
                                          :item-id item-id
                                          :file (:change-item/file item)
                                          :wave-id wave-id
                                          :error-type :uncaught-exception
                                          :exception-class (.getName (class e))
                                          :message (.getMessage e)})
                              {:success false
                               :item-id item-id
                               :error (str "Worker exception: " (.getMessage e))}))]
          ;; Update state (SLAP: delegate to extracted function)
          (update-item-state! exec-result wave-id trace)
          (>! result-ch exec-result))
        (recur)))))

(defn- collect-results
  "Collect results from worker channels.

   Arguments:
     result-ch   - Channel receiving results
     item-count  - Expected number of results

   Returns:
     Map with :completed and :failed counts.

   SOLID-SRP: Single responsibility - result aggregation only."
  [result-ch item-count]
  (go
    (loop [completed 0 failed 0 n 0]
      (if (< n item-count)
        (let [r (<! result-ch)]
          (recur (if (:success r) (inc completed) completed)
                 (if-not (:success r) (inc failed) failed)
                 (inc n)))
        {:completed completed :failed failed}))))

(defn- execute-batch!
  "Execute a single batch of items with bounded concurrency.
   Returns channel that emits {:completed N :failed N} when batch is done.

   Arguments:
     batch-items     - Items to execute
     preset          - Drone preset
     cwd             - Working directory override
     concurrency     - Max concurrent workers
     wave-id         - Wave ID for state tracking
     trace           - Whether to emit events
     skip-auto-apply - When true, drones propose diffs without auto-applying

   SLAP: Uses extracted helper functions for single abstraction level:
   - spawn-workers! handles worker goroutine creation
   - collect-results handles result aggregation
   - update-item-state! handles DataScript + event updates"
  [batch-items preset cwd concurrency wave-id trace skip-auto-apply]
  (let [result-ch (chan)
        item-count (count batch-items)]
    (go
      (let [work-ch (chan)
            inner-result-ch (chan)]

        ;; Producer: push all items to work channel
        (go
          (doseq [item batch-items]
            (>! work-ch (item->work-unit item preset cwd skip-auto-apply wave-id)))
          (close! work-ch))

        ;; Spawn workers (SLAP: delegated to extracted function)
        (spawn-workers! work-ch inner-result-ch wave-id trace concurrency item-count)

        ;; Collect results and emit to result channel
        (let [counts (<! (collect-results inner-result-ch item-count))]
          (>! result-ch counts))))

    result-ch))

(defn execute-wave!
  "Execute a wave for a plan with bounded concurrency and conflict-aware batching.

   Arguments:
     plan-id     - Plan to execute
     opts        - Map with:
                   :concurrency     - Max concurrent drones (default 3)
                   :trace           - Emit events (default true)
                   :cwd             - Working directory override (optional)
                   :skip-auto-apply - When true, drones propose diffs without auto-applying.
                                      Use for review-before-apply workflow. (default false)

   Returns:
     Wave-id after execution completes.

   Batching behavior:
     - Items editing the same file are placed in separate batches (conflict prevention)
     - Test files wait for their source files (dependency inference)
     - Batches execute sequentially; items within a batch execute in parallel

   Review Mode (skip-auto-apply: true):
     - Drones propose diffs via propose_diff instead of file_write
     - Diffs are tagged with wave-id for batch review
     - Use review_wave_diffs, approve_wave_diffs, auto_approve_wave_diffs after execution

   CRITICAL FIX: Uses synchronous execution (<!! blocking take) to ensure
   all batches complete before returning. Previous async (go) implementation
   returned immediately while execution happened in detached context.

   The :wave-id option allows pre-creating the wave for async dispatch
   (caller creates wave-id, starts execute-wave! in a future, returns wave-id
   immediately to avoid nREPL socket timeout)."
  [plan-id & [{:keys [concurrency trace cwd skip-auto-apply wave-id]
               :or {concurrency default-concurrency trace true skip-auto-apply false}}]]
  (let [wave-start-time (System/nanoTime)  ;; CLARITY-T: Timing for wave duration metric
        plan (ds/get-plan plan-id)]
    (when-not plan
      (throw (ex-info "Plan not found" {:plan-id plan-id})))

    (let [items (get-pending-items plan-id)]
      ;; PHASE 1: Validation guards (CLARITY-I: Inputs are guarded)
      (when (empty? items)
        (throw (ex-info "No pending items for wave execution"
                        {:plan-id plan-id
                         :message "Plan has no items with :pending status"})))

      (let [preset (:change-plan/preset plan)
            wave-id (or wave-id (ds/create-wave! plan-id {:concurrency concurrency}))]

        ;; PHASE 0: START COST TRACKING (CLARITY-T: Budget management)
        (cost/start-wave-tracking! wave-id)

        ;; PHASE 0.5: CLEANUP STALE CLAIMS (CLARITY-Y: Unblock ghost claims)
        ;; Previous wave crashes or timeouts may leave orphaned claims
        (try
          (let [cleaned (ds/cleanup-stale-claims!)]
            (when (pos? cleaned)
              (log/info "Cleaned up" cleaned "stale claims before wave" wave-id)))
          (catch Exception e
            (log/warn "Stale claim cleanup failed (non-fatal):" (.getMessage e))))

        ;; Update plan status
        (ds/update-plan-status! plan-id :in-progress)

        ;; PHASE 2: REGISTER EDITS in logic db
        ;; CRITICAL: Reset stale edits before registering new ones
        (logic/reset-edits!)
        (register-edits! items)
        (log/info "Registered" (count items) "edits for batch computation")

        ;; PHASE 3: INFER DEPENDENCIES (test → source)
        (infer-test-dependencies! items)

        ;; PHASE 4: COMPUTE SAFE BATCHES
        (let [edit-ids (mapv :change-item/id items)
              batches (logic/compute-batches edit-ids)
              item-map (into {} (map (juxt :change-item/id identity) items))]

          ;; Validation: Batches should not be empty if items exist
          (when (empty? batches)
            (log/warn "Empty batches computed for" (count items) "items - forcing single batch"))
            ;; Fall back to treating all items as single batch

          (let [effective-batches (if (empty? batches) [edit-ids] batches)]
            (log/info "Computed" (count effective-batches) "batches for" (count items) "items")

            ;; Emit wave start event
            (when trace
              (ev/dispatch [:wave/start {:plan-id plan-id
                                         :wave-id wave-id
                                         :item-count (count items)
                                         :batch-count (count effective-batches)}]))

            ;; PHASE 5: Initialize active-count for the first batch
            ;; (items get tracked as they're dispatched within execute-batch!)

            ;; PHASE 6: EXECUTE BATCH-BY-BATCH (SYNCHRONOUS!)
            ;; CRITICAL: Use <!! to block until batch completes
            ;; This is the fix for fire-and-forget async bug
            (loop [remaining-batches effective-batches
                   total-completed 0
                   total-failed 0
                   batch-num 1]
              (if (empty? remaining-batches)
                ;; All batches done
                (let [final-status (if (pos? total-failed) :partial-failure :completed)
                      total-items (+ total-completed total-failed)
                      success-rate (if (pos? total-items)
                                     (/ (double total-completed) total-items)
                                     1.0)]
                  ;; CLEANUP: Reset all transient data (edits + task-files)
                  ;; CRITICAL: Use reset-all-transient! to prevent memory leak in task-files
                  (logic/reset-all-transient!)

                  ;; CLEANUP: Release stale claims from this wave
                  ;; CLARITY-Y: Prevent ghost claims from blocking subsequent waves
                  (try
                    (ds/cleanup-stale-claims!)
                    (catch Exception e
                      (log/warn "Post-wave claim cleanup failed (non-fatal):" (.getMessage e))))

                  ;; CLARITY-T: Structured JSON logging for Loki
                  (if (pos? total-failed)
                    (let [failed-items (->> items
                                            (filter #(= :failed (:change-item/status %)))
                                            (mapv #(select-keys % [:change-item/id :change-item/file])))]
                      (log/error {:event :wave/failure
                                  :wave-id wave-id
                                  :plan-id plan-id
                                  :failed-count total-failed
                                  :completed-count total-completed
                                  :batch-count (count effective-batches)
                                  :success-rate success-rate
                                  :failed-items failed-items
                                  :reason (if (= total-completed 0) "all-failed" "partial-failure")}))
                    (log/info {:event :wave/completed
                               :wave-id wave-id
                               :plan-id plan-id
                               :completed-count total-completed
                               :batch-count (count effective-batches)
                               :success-rate success-rate}))

                  ;; CLARITY-T: Record wave metrics
                  (let [wave-duration-seconds (/ (- (System/nanoTime) wave-start-time) 1e9)]
                    (prom/set-wave-success-rate! success-rate)
                    (dotimes [_ total-completed] (prom/inc-wave-items! :success))
                    (dotimes [_ total-failed] (prom/inc-wave-items! :failed))
                    (prom/observe-wave-duration! wave-duration-seconds)
                    ;; Record wave failures with granular reason
                    (when (pos? total-failed)
                      (prom/inc-wave-failures! wave-id
                                               (if (= total-completed 0)
                                                 :all-failed
                                                 :partial-failure))))

                  (ds/complete-wave! wave-id final-status)
                  (ds/update-plan-status! plan-id (if (pos? total-failed) :failed :completed))

                  ;; COMPLETE COST TRACKING (CLARITY-T: Budget management)
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
                                                            :batches (count effective-batches)}}]))
                  ;; Return wave-id after completion
                  wave-id)

                ;; Execute current batch
                (let [batch (first remaining-batches)
                      batch-items (mapv #(get item-map %) batch)]

                  (log/info "Executing batch" batch-num "of" (count effective-batches)
                            "with" (count batch-items) "items")

                  ;; Initialize active count for this batch BEFORE spawning
                  (ds/update-wave-counts! wave-id {:active (count batch-items)})

                  (when trace
                    (ev/dispatch [:wave/batch-start {:wave-id wave-id
                                                     :batch-num batch-num
                                                     :item-count (count batch-items)}]))

                  ;; Execute batch and BLOCK until completion with keepalive
                  ;; CLARITY-T: Periodic progress to *out* prevents nREPL socket timeout
                  (let [{:keys [completed failed]}
                        (blocking-take-with-keepalive!
                         (execute-batch! batch-items preset cwd
                                         concurrency wave-id trace
                                         skip-auto-apply)
                         keepalive-interval-ms
                         #(format "[wave:%s] batch %d/%d in progress (%d items)..."
                                  wave-id batch-num (count effective-batches) (count batch-items)))]
                    (recur (rest remaining-batches)
                           (+ total-completed completed)
                           (+ total-failed failed)
                           (inc batch-num))))))))))))

;;; =============================================================================
;;; Async Wave Execution (CLARITY-A: Non-blocking for nREPL transport)
;;; =============================================================================

(defn execute-wave-async!
  "Execute a wave asynchronously in a background thread.
   Returns immediately with {:wave-id :plan-id :item-count}.

   Creates the wave-id synchronously (so it's available for polling),
   then starts execution in a background future.

   The caller should use get-wave-status with the returned wave-id
   to poll for completion.

   Arguments:
     plan-id - Plan to execute
     opts    - Same options as execute-wave! plus:
               :on-complete - Optional callback fn called with wave-id on completion

   Returns:
     Map with :wave-id :plan-id :item-count for immediate response.

   CLARITY-A: Eliminates nREPL socket timeout class entirely.
   DIP: Transport layer (bb-mcp) no longer coupled to execution time."
  [plan-id & [{:keys [concurrency trace cwd skip-auto-apply on-complete]
               :or {concurrency default-concurrency trace true skip-auto-apply false}
               :as opts}]]
  ;; Validate synchronously so errors are reported immediately
  (let [plan (ds/get-plan plan-id)]
    (when-not plan
      (throw (ex-info "Plan not found" {:plan-id plan-id})))

    (let [items (get-pending-items plan-id)]
      (when (empty? items)
        (throw (ex-info "No pending items for wave execution"
                        {:plan-id plan-id
                         :message "Plan has no items with :pending status"})))

      ;; Create wave-id synchronously for immediate return
      (let [wave-id (ds/create-wave! plan-id {:concurrency concurrency})
            item-count (count items)]

        ;; Start execution in background thread
        (future
          (try
            (execute-wave! plan-id {:concurrency concurrency
                                    :trace trace
                                    :cwd cwd
                                    :skip-auto-apply skip-auto-apply
                                    :wave-id wave-id})
            (when on-complete
              (on-complete wave-id))
            (catch Exception e
              (log/error e "Async wave execution failed" {:wave-id wave-id :plan-id plan-id})
              ;; Mark wave as failed in DataScript
              (try
                (ds/complete-wave! wave-id :failed)
                (ds/update-plan-status! plan-id :failed)
                (catch Exception inner
                  (log/error inner "Failed to mark wave as failed" {:wave-id wave-id}))))))

        ;; Return immediately
        {:wave-id wave-id
         :plan-id plan-id
         :item-count item-count}))))

;;; =============================================================================
;;; Wave Cancellation
;;; =============================================================================

(defn cancel-wave!
  "Cancel a running wave.

   Arguments:
     wave-id - Wave ID to cancel
     reason  - Keyword reason (:timeout :explicit :error)
     message - Optional detail message

   Updates wave status in DataScript and emits :wave/cancelled event.
   Note: Does NOT interrupt running drones - they will complete.

   CLARITY-Y: Graceful degradation - running drones complete, no new ones start."
  [wave-id & [{:keys [reason message] :or {reason :explicit}}]]
  (when-let [wave (ds/get-wave wave-id)]
    (let [plan-id (:wave/plan wave)]
      ;; Update statuses
      (ds/complete-wave! wave-id :cancelled)
      (ds/update-plan-status! plan-id :cancelled)

      ;; CLARITY-T: Record wave failure metric for cancellation
      (prom/inc-wave-failures! wave-id (or reason :cancelled))

      ;; Emit cancellation event
      (ev/dispatch [:wave/cancelled {:plan-id plan-id
                                     :wave-id wave-id
                                     :reason reason
                                     :message message}])

      ;; Cleanup transient state
      (logic/reset-all-transient!)

      {:cancelled true
       :wave-id wave-id
       :reason reason})))

;;; =============================================================================
;;; Status Queries
;;; =============================================================================

(defn get-wave-status
  "Get current status of a wave execution.

   Returns map with:
     :wave-id :plan-id :status :active-count :completed-count :failed-count"
  [wave-id]
  (when-let [wave (ds/get-wave wave-id)]
    {:wave-id wave-id
     :plan-id (:wave/plan wave)
     :status (:wave/status wave)
     :active-count (:wave/active-count wave)
     :completed-count (:wave/completed-count wave)
     :failed-count (:wave/failed-count wave)
     :started-at (:wave/started-at wave)
     :completed-at (:wave/completed-at wave)}))

(defn get-plan-status
  "Get current status of a change plan.

   Returns map with plan info and all items."
  [plan-id]
  (when-let [plan (ds/get-plan plan-id)]
    {:plan-id plan-id
     :status (:change-plan/status plan)
     :preset (:change-plan/preset plan)
     :items (mapv (fn [item]
                    {:item-id (:change-item/id item)
                     :file (:change-item/file item)
                     :status (:change-item/status item)
                     :result (:change-item/result item)})
                  (get-plan-items plan-id))}))

;;; =============================================================================
;;; MCP Handler
;;; =============================================================================

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

    (if-let [status (get-wave-status wave_id)]
      ;; Also get item-level details for failed items
      (let [plan-status (get-plan-status (:plan-id status))
            failed-items (->> (:items plan-status)
                              (filter #(= :failed (:status %)))
                              (mapv #(select-keys % [:item-id :file :result])))]
        {:type "text"
         :text (json/write-str (merge status
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
     cwd            - Working directory override for path resolution (optional)
     ensure_dirs    - Create parent directories before dispatch (default: true)
     validate_paths - Fail fast if paths are invalid (default: true)

   Pre-flight behavior (P0/P2):
     1. If ensure_dirs is true, creates parent directories for all task files
     2. If validate_paths is true, validates all file paths have accessible parents
     3. Then proceeds with plan creation and wave execution

   Returns:
     JSON with wave-id for immediate response.
     Actual execution happens asynchronously."
  [{:keys [tasks preset trace cwd ensure_dirs validate_paths]}]
  ;; DEPRECATION WARNING: Prefer unified 'delegate' tool
  (log/warn {:event :deprecation-warning
             :tool "dispatch_drone_wave"
             :message "DEPRECATED: Use 'delegate' tool instead. delegate({tasks: [...]})"})

  (try
    (when (empty? tasks)
      (throw (ex-info "tasks array is required and must not be empty" {})))

    ;; Normalize task keys (MCP sends string keys)
    (let [normalized-tasks (mapv (fn [t]
                                   {:file (or (get t "file") (:file t))
                                    :task (or (get t "task") (:task t))})
                                 tasks)
          ;; Pre-flight defaults: both true unless explicitly false
          do-validate (if (false? validate_paths) false true)
          do-ensure (if (false? ensure_dirs) false true)]

      ;; P0: Create parent directories FIRST (so validation can pass)
      (when do-ensure
        (ensure-parent-dirs! normalized-tasks))

      ;; P2: Validate paths AFTER ensure-dirs (fail fast for remaining issues)
      (when do-validate
        (validate-task-paths normalized-tasks))

      ;; Proceed with plan and wave execution
      ;; Note: nREPL pre-flight check removed - drones use OpenRouter for LLM, not nREPL.
      ;; Clojure tools (cider_eval_silent, etc.) are optional; transient nREPL errors
      ;; handled gracefully at runtime via retry logic in execute-drone-task.
      (let [plan-id (create-plan! normalized-tasks preset)
            {:keys [wave-id item-count]} (execute-wave-async!
                                          plan-id
                                          {:trace (if (nil? trace) true trace)
                                           :cwd cwd})]
        {:type "text"
         :text (json/write-str {:status "dispatched"
                                :plan_id plan-id
                                :wave_id wave-id
                                :item_count item-count
                                :message "Wave dispatched to background. Poll get_wave_status(wave_id) for progress."})}))
    (catch clojure.lang.ExceptionInfo e
      ;; Structured error - preserve details
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
