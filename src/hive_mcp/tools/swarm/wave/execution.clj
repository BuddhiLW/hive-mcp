(ns hive-mcp.tools.swarm.wave.execution
  "Wave execution orchestrator.

   Composes phases into a complete wave execution pipeline.
   This is the main entry point that replaces the monolithic execute-wave!.

   Architecture:
   1. pre-flight  - Validation, claim cleanup
   2. register    - Edit registration, dependency inference
   3. batching    - Compute safe batches
   4. execute     - Batch-by-batch with bounded concurrency
   5. complete    - Metrics, status updates
   6. cleanup     - Always runs (transient state reset)

   SOLID-S: Single responsibility - orchestration only.
   CLARITY-L: Clear layer for execution coordination."
  (:require [hive-mcp.tools.swarm.wave.domain :as domain]
            [hive-mcp.tools.swarm.wave.phases :as phases]
            [hive-mcp.tools.swarm.wave.retry :as retry]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.agent :as agent]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Drone Task Execution (with retry)
;;; ============================================================

(defn- build-task-str
  "Build task string with optional staleness warnings.
   Shared by both delegate and agentic execution paths."
  [file task]
  (let [disc-warnings (try (kg-disc/staleness-warnings [file]) (catch Exception _ nil))
        disc-notice (kg-disc/format-staleness-warnings disc-warnings)]
    (if disc-notice
      (str disc-notice "File: " file "\n\nTask: " task)
      (str "File: " file "\n\nTask: " task))))

(defn- task-result-from-drone
  "Convert drone execution result to TaskResult domain object."
  [id result]
  (if (= :completed (:status result))
    (cond-> (domain/success-result id (:result result))
      (seq (:proposed-diff-ids result))
      (assoc :proposed-diff-ids (:proposed-diff-ids result)))
    (domain/failure-result id (or (:result result) "Drone execution failed"))))

(defn- execute-drone-task-once
  "Execute a single drone task without retry (delegate-fn path).

   Arguments:
     item            - Change item map
     preset          - Drone preset
     cwd             - Working directory override
     skip-auto-apply - When true, drone proposes diffs without auto-applying
     wave-id         - Wave ID for tagging proposed diffs
     backend         - Drone execution backend keyword (optional)

   Returns:
     TaskResult record"
  [{:keys [change-item/id change-item/file change-item/task]} preset cwd skip-auto-apply wave-id
   & {:keys [backend model]}]
  (let [task-str (build-task-str file task)
        result (agent/delegate-drone!
                (cond-> {:task task-str
                         :files [file]
                         :preset preset
                         :trace true
                         :cwd cwd
                         :skip-auto-apply skip-auto-apply
                         :wave-id wave-id}
                  backend (assoc :backend backend)
                  model   (assoc :model model)))]
    (task-result-from-drone id result)))

(defn- execute-drone-task-once-agentic
  "Execute a single drone task without retry (in-process agentic loop path).

   Uses delegate-agentic-drone! which runs a multi-turn think-act-observe
   loop in-process with session KG (Datalevin) for context compression.
   No external delegate-fn needed.

   Arguments:
     item            - Change item map
     preset          - Drone preset
     cwd             - Working directory override
     skip-auto-apply - When true, drone proposes diffs without auto-applying
     wave-id         - Wave ID for tagging proposed diffs
     backend         - Drone execution backend keyword (optional)

   Returns:
     TaskResult record"
  [{:keys [change-item/id change-item/file change-item/task]} preset cwd skip-auto-apply wave-id
   & {:keys [backend model]}]
  (let [task-str (build-task-str file task)
        result (agent/delegate-agentic-drone!
                (cond-> {:task task-str
                         :files [file]
                         :preset preset
                         :trace true
                         :cwd cwd
                         :skip-auto-apply skip-auto-apply
                         :wave-id wave-id}
                  backend (assoc :backend backend)
                  model   (assoc :model model)))]
    (task-result-from-drone id result)))

(defn execute-drone-task
  "Execute a single drone task with unified retry logic.

   Uses retry/with-wave-retry for:
   - nREPL transient errors (short delays, 2 retries)
   - File conflicts (long delays, 5 retries)
   - OpenRouter errors (model fallback, exponential backoff)

   Arguments:
     item            - Change item map
     preset          - Drone preset
     cwd             - Working directory override
     skip-auto-apply - When true, drone proposes diffs without auto-applying
     wave-id         - Wave ID for tagging proposed diffs
     mode            - Execution mode: :delegate (default) or :agentic
     backend         - Drone execution backend keyword (optional)

   Returns:
     TaskResult record with optional :retry-info"
  [{:keys [change-item/id change-item/file] :as item} preset cwd skip-auto-apply wave-id
   & {:keys [mode backend model] :or {mode :delegate}}]
  (log/info "Executing drone task for item:" id "file:" file "cwd:" cwd
            {:review-mode skip-auto-apply :mode mode :backend backend :model model})

  ;; Update status to dispatched
  (ds/update-item-status! id :dispatched)

  ;; Route to appropriate execution function based on mode
  (let [execute-fn (if (= mode :agentic)
                     #(execute-drone-task-once-agentic item preset cwd skip-auto-apply wave-id
                                                       :backend backend :model model)
                     #(execute-drone-task-once item preset cwd skip-auto-apply wave-id
                                               :backend backend :model model))]
    ;; Execute with retry
    (retry/retry-task-execution
     execute-fn
     item
     {:item-id id :file file})))

;;; ============================================================
;;; Work Unit Executor (for batching)
;;; ============================================================

(defn- record-item-success!
  "Record successful drone execution in DataScript.
   Never throws — logs and attempts fallback on error."
  [item-id result-text wave-id]
  (ds/update-item-status! item-id :completed {:result result-text})
  (ds/update-wave-counts! wave-id {:completed 1 :active -1}))

(defn- record-item-failure!
  "Record failed drone execution in DataScript.
   Never throws — logs and attempts fallback on error."
  [item-id error-text wave-id]
  (ds/update-item-status! item-id :failed {:result error-text})
  (ds/update-wave-counts! wave-id {:failed 1 :active -1}))

(defn- update-item-state!
  "Update DataScript after drone execution completes.
   Dispatches to success/failure based on result.
   Catches Throwable — never lets state-update kill the result channel."
  [{:keys [item-id success result error] :as _task-result} wave-id]
  (try
    (if success
      (record-item-success! item-id (str result) wave-id)
      (record-item-failure! item-id error wave-id))
    (catch Throwable e
      (log/error {:event :wave/item-status-update-failed
                  :item-id item-id
                  :wave-id wave-id
                  :error (ex-message e)})
      (try
        (record-item-failure! item-id (str "Post-execution error: " (ex-message e)) wave-id)
        (catch Throwable _)))))

(defn make-work-executor
  "Create a work unit executor for batching.

   Composes: execute-drone-task → update-item-state! → return result.
   Each step is a named function at a single level of abstraction (SLAP).

   Options:
     :mode    - :delegate (default) or :agentic
     :backend - :openrouter, :hive-agent, :legacy-loop, :sdk-drone, or nil
     :model   - Override model for all drones (optional)

   Returns fn: work-unit → TaskResult."
  [& {:keys [mode backend model] :or {mode :delegate}}]
  (fn [{:keys [item preset cwd skip-auto-apply wave-id]}]
    (let [result (execute-drone-task item preset cwd skip-auto-apply wave-id
                                     :mode mode :backend backend :model model)]
      (update-item-state! result wave-id)
      result)))

;;; ============================================================
;;; Main Orchestrator
;;; ============================================================

(defn run-wave!
  "Orchestrate wave execution through all phases.

   This is the main entry point for wave execution.
   Composes phases with proper error handling and cleanup.

   Arguments:
     wave-spec - WaveSpec record
     items     - Pending change items

   Returns:
     WaveResult record on success, throws on failure."
  [wave-spec items]
  (let [{:keys [plan-id wave-id]} wave-spec
        ;; Execution mode: :delegate (default) or :agentic (in-process loop with session KG)
        mode (or (:mode wave-spec) :delegate)
        ;; Drone execution backend (nil = mode-dependent default)
        backend (:backend wave-spec)
        ;; Model override (nil = smart routing)
        model (:model wave-spec)
        ;; Create or use provided wave-id
        effective-wave-id (or wave-id (ds/create-wave! plan-id {:concurrency (:concurrency wave-spec)}))
        start-time (System/nanoTime)]

    (log/info "Starting wave execution"
              {:wave-id effective-wave-id
               :plan-id plan-id
               :item-count (count items)
               :mode mode
               :backend backend
               :model model})

    (try
      ;; Phase 1: Pre-flight
      (phases/phase:pre-flight! wave-spec items effective-wave-id)

      ;; Phase 2: Register edits
      (phases/phase:register! plan-id items)

      ;; Phase 3: Compute batches
      (let [{:keys [batches item-map]} (phases/phase:compute-batches items)

            ;; Initialize active-count for first batch
            _ (ds/update-wave-counts! effective-wave-id {:active (count (first batches))})

            ;; Phase 4: Execute batches (with mode-aware, backend-aware, model-aware executor)
            execution-result (phases/phase:execute-batches!
                              wave-spec effective-wave-id
                              batches item-map
                              (make-work-executor :mode mode :backend backend :model model))]

        ;; Phase 5: Complete
        (phases/phase:complete! wave-spec effective-wave-id execution-result start-time items))

      (catch Throwable e
        (phases/phase:handle-error! wave-spec effective-wave-id e start-time)
        (throw e))

      (finally
        ;; Phase 6: Cleanup (always runs)
        (phases/phase:cleanup! effective-wave-id)))))

;;; ============================================================
;;; Async Execution
;;; ============================================================

(defn run-wave-async!
  "Execute a wave asynchronously in a background thread.

   Creates the wave-id synchronously (so it's available for polling),
   then starts execution in a background future.

   Arguments:
     wave-spec - WaveSpec record
     items     - Pending change items

   Returns:
     Map with :wave-id :plan-id :item-count for immediate response.

   CLARITY-A: Eliminates nREPL socket timeout class entirely."
  [wave-spec items]
  (let [{:keys [plan-id concurrency]} wave-spec
        ;; Create wave-id synchronously for immediate return
        wave-id (ds/create-wave! plan-id {:concurrency concurrency})
        item-count (count items)
        spec-with-wave (assoc wave-spec :wave-id wave-id)]

    ;; Start execution in background thread
    (future
      (try
        (run-wave! spec-with-wave items)
        (catch Throwable e
          (log/error e "Async wave execution failed" {:wave-id wave-id :plan-id plan-id}))))

    ;; Return immediately
    {:wave-id wave-id
     :plan-id plan-id
     :item-count item-count}))

;;; ============================================================
;;; High-Level API (backwards compatible)
;;; ============================================================

(defn execute-wave!
  "Execute a wave for a plan (backwards-compatible API).

   Arguments:
     plan-id - Plan to execute
     opts    - Options map:
               :concurrency     - Max concurrent drones (default 3)
               :trace           - Emit events (default true)
               :cwd             - Working directory override
               :skip-auto-apply - Review mode (default false)
               :wave-id         - Pre-created wave ID
               :mode            - Execution mode: :delegate (default) or :agentic
                                  When :agentic, uses in-process agentic loop
                                  with session KG (Datalevin) for context compression
               :backend         - Drone execution backend keyword (optional)
                                  One of: :openrouter, :hive-agent, :legacy-loop, :sdk-drone
               :model           - Override model for all drones (optional)
                                  Bypasses smart routing when provided

   Returns:
     wave-id after execution completes."
  [plan-id & [{:keys [concurrency trace cwd skip-auto-apply wave-id mode backend model]
               :or {concurrency domain/default-concurrency
                    trace true
                    skip-auto-apply false
                    mode :delegate}}]]
  (let [plan (ds/get-plan plan-id)]
    (when-not plan
      (throw (ex-info "Plan not found" {:plan-id plan-id})))

    (let [items (ds/get-pending-items plan-id)]
      (when (empty? items)
        (throw (ex-info "No pending items for wave execution"
                        {:plan-id plan-id
                         :message "Plan has no items with :pending status"})))

      (let [preset (:change-plan/preset plan)
            wave-spec (domain/->wave-spec
                       (cond-> {:plan-id plan-id
                                :concurrency concurrency
                                :trace trace
                                :cwd cwd
                                :skip-auto-apply skip-auto-apply
                                :wave-id wave-id
                                :preset preset
                                :mode mode}
                         backend (assoc :backend backend)
                         model   (assoc :model model)))]

        ;; Run wave and return wave-id
        (:wave-id (run-wave! wave-spec items))))))

(defn execute-wave-async!
  "Execute a wave asynchronously (backwards-compatible API).

   Arguments:
     plan-id - Plan to execute
     opts    - Same options as execute-wave! plus:
               :on-complete - Optional callback
               :mode        - Execution mode: :delegate (default) or :agentic
               :backend     - Drone execution backend keyword (optional)
               :model       - Override model for all drones (optional)

   Returns:
     Map with :wave-id :plan-id :item-count for immediate response."
  [plan-id & [{:keys [concurrency trace cwd skip-auto-apply on-complete mode backend model]
               :or {concurrency domain/default-concurrency
                    trace true
                    skip-auto-apply false
                    mode :delegate}}]]
  (let [plan (ds/get-plan plan-id)]
    (when-not plan
      (throw (ex-info "Plan not found" {:plan-id plan-id})))

    (let [items (ds/get-pending-items plan-id)]
      (when (empty? items)
        (throw (ex-info "No pending items for wave execution"
                        {:plan-id plan-id
                         :message "Plan has no items with :pending status"})))

      (let [preset (:change-plan/preset plan)
            wave-spec (domain/->wave-spec
                       (cond-> {:plan-id plan-id
                                :concurrency concurrency
                                :trace trace
                                :cwd cwd
                                :skip-auto-apply skip-auto-apply
                                :on-complete on-complete
                                :preset preset
                                :mode mode}
                         backend (assoc :backend backend)
                         model   (assoc :model model)))]

        (run-wave-async! wave-spec items)))))

;;; ============================================================
;;; Wave Cancellation
;;; ============================================================

(defn cancel-wave!
  "Cancel a running wave.

   Arguments:
     wave-id - Wave ID to cancel
     opts    - Optional map with:
               :reason  - Keyword reason (:timeout :explicit :error)
               :message - Optional detail message

   Updates wave status in DataScript and cleans up transient state.
   Note: Does NOT interrupt running drones - they will complete.

   CLARITY-Y: Graceful degradation - running drones complete, no new ones start."
  [wave-id & [{:keys [reason message] :or {reason :explicit}}]]
  (when-let [wave (ds/get-wave wave-id)]
    (let [plan-id (:wave/plan wave)]
      ;; Update statuses
      (ds/complete-wave! wave-id :cancelled)
      (ds/update-plan-status! plan-id :cancelled)

      ;; Cleanup transient state
      (phases/phase:cleanup! wave-id)

      (log/info {:event :wave/cancelled
                 :wave-id wave-id
                 :plan-id plan-id
                 :reason reason
                 :message message})

      {:cancelled true
       :wave-id wave-id
       :reason reason})))
