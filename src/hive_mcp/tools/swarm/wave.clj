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
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.events.core :as ev]
            [hive-mcp.agent :as agent]
            [hive-mcp.telemetry.prometheus :as prom]
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
;;; Drone Execution
;;; =============================================================================

(defn- execute-drone-task
  "Execute a single drone task.

   Arguments:
     item   - Change item map
     preset - Drone preset
     cwd    - Optional working directory override for path resolution

   Returns:
     Map with :success :result or :error"
  [{:keys [change-item/id change-item/file change-item/task]} preset cwd]
  (try
    (log/info "Executing drone task for item:" id "file:" file "cwd:" cwd)
    ;; Update status to dispatched
    (ds/update-item-status! id :dispatched)

    ;; Delegate to drone
    (let [result (agent/delegate-drone!
                  {:task (str "File: " file "\n\nTask: " task)
                   :files [file]
                   :preset preset
                   :trace true
                   :cwd cwd})]
      (if (= :completed (:status result))
        {:success true
         :item-id id
         :result (:result result)}
        {:success false
         :item-id id
         :error (or (:result result) "Drone execution failed")}))
    (catch Exception e
      (log/error e "Drone task failed for item:" id)
      {:success false
       :item-id id
       :error (.getMessage e)})))

;;; =============================================================================
;;; Wave Execution (core.async bounded concurrency)
;;; =============================================================================

(defn- item->work-unit
  "Convert item to work unit for async processing."
  [item preset cwd]
  {:item item :preset preset :cwd cwd})

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
     work-ch   - Channel providing work units
     result-ch - Channel to send results to
     wave-id   - Wave ID for state updates
     trace     - Whether to emit events
     concurrency - Max concurrent workers
     item-count  - Number of items (for worker count calculation)

   SOLID-SRP: Single responsibility - worker spawning only."
  [work-ch result-ch wave-id trace concurrency item-count]
  (dotimes [_ (min concurrency item-count)]
    (go-loop []
      (when-let [{:keys [item preset cwd]} (<! work-ch)]
        (let [item-id (:change-item/id item)
              exec-result (try
                            (execute-drone-task item preset cwd)
                            (catch Exception e
                              (log/error e "Uncaught exception in drone worker")
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

   SLAP: Uses extracted helper functions for single abstraction level:
   - spawn-workers! handles worker goroutine creation
   - collect-results handles result aggregation
   - update-item-state! handles DataScript + event updates"
  [batch-items preset cwd concurrency wave-id trace]
  (let [result-ch (chan)
        item-count (count batch-items)]
    (go
      (let [work-ch (chan)
            inner-result-ch (chan)]

        ;; Producer: push all items to work channel
        (go
          (doseq [item batch-items]
            (>! work-ch (item->work-unit item preset cwd)))
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
     opts        - Map with :concurrency (default 3), :trace (default true), :cwd (optional)

   Returns:
     Wave-id after execution completes.

   Batching behavior:
     - Items editing the same file are placed in separate batches (conflict prevention)
     - Test files wait for their source files (dependency inference)
     - Batches execute sequentially; items within a batch execute in parallel

   CRITICAL FIX: Uses synchronous execution (<!! blocking take) to ensure
   all batches complete before returning. Previous async (go) implementation
   returned immediately while execution happened in detached context."
  [plan-id & [{:keys [concurrency trace cwd] :or {concurrency default-concurrency trace true}}]]
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
            wave-id (ds/create-wave! plan-id {:concurrency concurrency})]

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
                  (log/info "Wave complete. Batches:" (count effective-batches)
                            "Completed:" total-completed "Failed:" total-failed)

                  ;; CLARITY-T: Record wave metrics
                  (let [wave-duration-seconds (/ (- (System/nanoTime) wave-start-time) 1e9)]
                    (prom/set-wave-success-rate! success-rate)
                    (dotimes [_ total-completed] (prom/inc-wave-items! :success))
                    (dotimes [_ total-failed] (prom/inc-wave-items! :failed))
                    (prom/observe-wave-duration! wave-duration-seconds))

                  (ds/complete-wave! wave-id final-status)
                  (ds/update-plan-status! plan-id (if (pos? total-failed) :failed :completed))

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

                  ;; Execute batch and BLOCK until completion (<!! instead of <!)
                  (let [{:keys [completed failed]} (<!! (execute-batch! batch-items preset cwd
                                                                        concurrency wave-id trace))]
                    (recur (rest remaining-batches)
                           (+ total-completed completed)
                           (+ total-failed failed)
                           (inc batch-num))))))))))))

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
      (let [plan-id (create-plan! normalized-tasks preset)
            wave-id (execute-wave! plan-id {:trace (if (nil? trace) true trace)
                                            :cwd cwd})]
        {:type "text"
         :text (json/write-str {:status "wave_started"
                                :plan_id plan-id
                                :wave_id wave-id
                                :item_count (count tasks)
                                :message "Wave execution started. Monitor via HIVEMIND piggyback or get_wave_status."})}))
    (catch Exception e
      (log/error e "dispatch_drone_wave failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)})})))
