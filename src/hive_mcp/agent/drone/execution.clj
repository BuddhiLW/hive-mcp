(ns hive-mcp.agent.drone.execution
  "Phase-based drone execution orchestration.

   Extracted from drone.clj delegate! function to reduce complexity
   (SOLID-S: Single Responsibility, cc < 15 per phase).

   Phases:
   1. prepare    - Infer task type, select model, create sandbox
   2. register   - Register drone in DataScript, claim files
   3. validate   - Pre-execution file validation
   4. execute    - Run task with sandbox constraints
   5. finalize   - Apply diffs, validate, record metrics
   6. cleanup    - Release locks, remove registration (always runs)

   Each phase is a pure function or has clear side-effect boundaries.
   The orchestrator (run-execution!) composes them with proper error handling."
  (:require [hive-mcp.agent.drone.domain :as domain]
            [hive-mcp.agent.drone.diff-mgmt :as diff-mgmt]
            [hive-mcp.agent.drone.augment :as augment]
            [hive-mcp.agent.drone.kg-factory :as kg-factory]
            [hive-mcp.agent.drone.session-kg :as session-kg]
            [hive-mcp.agent.drone.loop :as agentic-loop]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.tools :as drone-tools]
            [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.agent.drone.decompose :as decompose]
            [hive-mcp.agent.drone.validation :as validation]
            [hive-mcp.agent.drone.errors :as errors]
            [hive-mcp.agent.drone.tool-allowlist :as allowlist]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.cost :as cost]
            [hive-mcp.protocols.kg :as kg]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.events.core :as ev]
            [hive-mcp.telemetry.prometheus :as prom]
            [hive-mcp.tools.diff :as diff]
            [taoensso.timbre :as log]))

;;; ============================================================
;;; Phase 1: Prepare - Configuration & Tool Selection
;;; ============================================================

(defn phase:prepare
  "Phase 1: Prepare execution configuration.

   Pure function that computes:
   - Effective task type (inferred or explicit)
   - Minimal tool set for task
   - Preset selection
   - Model routing
   - Step budget

   Arguments:
     task-spec - TaskSpec record

   Returns:
     Map with :task-type, :tools, :preset, :model, :step-budget, etc."
  [task-spec]
  (let [{:keys [task files task-type preset cwd]} task-spec
        ;; Infer task-type if not explicitly provided
        effective-task-type (or task-type (preset/get-task-type task files))
        ;; Select minimal tools for this task type
        minimal-tools (drone-tools/get-tools-for-drone effective-task-type files)
        ;; Auto-select preset based on task type
        effective-preset (or preset (preset/select-drone-preset task files))
        ;; Smart model routing
        model-selection (routing/route-and-select task files {:directory cwd})
        ;; Calculate step budget based on task complexity
        step-budget (decompose/get-step-budget task files)]

    (log/info "Drone configuration prepared:"
              {:task-type effective-task-type
               :preset effective-preset
               :model (:model model-selection)
               :model-reason (:reason model-selection)
               :tool-count (count minimal-tools)
               :max-steps step-budget})

    {:task-type effective-task-type
     :tools minimal-tools
     :preset effective-preset
     :model (:model model-selection)
     :model-fallback (:fallback model-selection)
     :step-budget step-budget
     :model-selection model-selection}))

;;; ============================================================
;;; Phase 2: Register - DataScript & File Claims
;;; ============================================================

(defn phase:register!
  "Phase 2: Register drone and acquire file locks.

   Side effects:
   - Creates drone entity in DataScript
   - Atomically claims files via coordinator

   Arguments:
     ctx       - ExecutionContext
     task-spec - TaskSpec record

   Returns:
     Updated ctx or throws on failure.

   CLARITY-I: Validates registration before claiming."
  [ctx task-spec]
  (let [{:keys [drone-id task-id parent-id]} ctx
        {:keys [files]} task-spec]

    ;; 1. Register drone in DataScript
    (let [tx-result (ds/add-slave! drone-id {:slave/status :spawning
                                             :slave/name "drone"
                                             :slave/depth 2
                                             :slave/parent parent-id})]
      (when-not (and tx-result (seq (:tx-data tx-result)))
        (log/error {:event :drone/registration-failed
                    :drone-id drone-id
                    :tx-result tx-result})
        (throw (ex-info "Failed to register drone in DataScript"
                        {:drone-id drone-id}))))

    ;; 2. Atomic file claim acquisition
    (when (seq files)
      (let [result (coordinator/atomic-claim-files! task-id drone-id files)]
        (when-not (:acquired? result)
          ;; Cleanup registration on failure
          (try (ds/remove-slave! drone-id) (catch Exception _ nil))

          (log/error {:event :drone/error
                      :error-type :conflict
                      :drone-id drone-id
                      :task-id task-id
                      :files files
                      :conflicts (:conflicts result)})

          (ev/dispatch [:drone/failed {:drone-id drone-id
                                       :task-id task-id
                                       :parent-id parent-id
                                       :error "File conflicts detected"
                                       :error-type :conflict
                                       :files files}])

          (throw (ex-info "File conflicts detected - files locked by another drone"
                          {:conflicts (:conflicts result)
                           :drone-id drone-id
                           :files files})))
        (log/info "Drone acquired file locks:" drone-id "(" (:files-claimed result) "files)")))

    ctx))

;;; ============================================================
;;; Phase 3: Validate - Pre-Execution Checks
;;; ============================================================

(defn phase:validate
  "Phase 3: Pre-execution file validation.

   Validates files before mutation:
   - File exists
   - Not binary
   - Size limits

   Arguments:
     ctx       - ExecutionContext
     task-spec - TaskSpec record

   Returns:
     Updated ctx with :pre-validation and :file-contents-before.

   CLARITY-I: Inputs are guarded."
  [ctx task-spec]
  (let [{:keys [drone-id task-id]} ctx
        {:keys [files]} task-spec]

    (if (empty? files)
      ctx
      (let [pre-validation (validation/validate-files-pre files task-id)
            _ (when-not (validation/all-valid? pre-validation :pre)
                (let [invalid-files (->> pre-validation
                                         (filter (comp not :pre-valid? val))
                                         (map key))]
                  (log/warn {:event :drone/pre-validation-failed
                             :drone-id drone-id
                             :invalid-files invalid-files})))
            ;; Capture file contents for post-validation diff
            file-contents-before (into {}
                                       (for [f files]
                                         [f (try (slurp f) (catch Exception _ nil))]))]

        (-> ctx
            (domain/with-pre-validation pre-validation)
            (domain/with-file-contents-before file-contents-before))))))

;;; ============================================================
;;; Phase 4: Execute - Core Task Execution
;;; ============================================================

(defn phase:execute!
  "Phase 4: Execute task with sandbox constraints.

   Side effects:
   - Emits :drone/started event
   - Shouts to parent ling
   - Calls delegate-fn

   Arguments:
     ctx         - ExecutionContext
     task-spec   - TaskSpec record
     config      - Prepared config from phase:prepare
     delegate-fn - Execution function

   Returns:
     Raw execution result from delegate-fn."
  [ctx task-spec config delegate-fn]
  (let [{:keys [drone-id task-id parent-id project-root pre-validation]} ctx
        {:keys [task files options]} task-spec
        {:keys [tools preset model step-budget]} config
        cwd (or (:cwd task-spec) project-root)

        ;; Augment task with context
        augmented-task (augment/augment-task task files {:project-root cwd})

        ;; Create sandbox
        effective-root (or cwd (diff/get-project-root))
        drone-sandbox (sandbox/create-sandbox (or files []) effective-root)]

    ;; Security check: Reject path escape attempts
    (when (seq (:rejected-files drone-sandbox))
      (log/error {:event :drone/path-escape-blocked
                  :drone-id drone-id
                  :rejected-files (:rejected-files drone-sandbox)})
      (throw (ex-info "File paths escape project directory"
                      {:error-type :path-escape
                       :rejected-files (:rejected-files drone-sandbox)})))

    (log/info "Drone sandbox created"
              {:drone-id drone-id
               :allowed-files (count (:allowed-files drone-sandbox))
               :blocked-tools (count (:blocked-tools drone-sandbox))})

    ;; Emit started event
    (ev/dispatch [:drone/started {:drone-id drone-id
                                  :task-id task-id
                                  :parent-id parent-id
                                  :files files
                                  :task task
                                  :pre-validation (validation/summarize-validation (or pre-validation {}))}])

    ;; Shout to parent ling
    (when parent-id
      (hivemind/shout! parent-id :started
                       {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                        :message (format "Delegated drone %s working" drone-id)}))

    ;; Execute with per-drone KG isolation via dynamic binding.
    ;; This ensures any KG operations during execution use the drone's
    ;; isolated store instead of the global singleton.
    (binding [domain/*drone-kg-store* (:kg-store ctx)]
      (delegate-fn {:backend :openrouter
                    :preset preset
                    :model model
                    :task augmented-task
                    :tools tools
                    :max-steps step-budget
                    :trace (:trace options true)
                    :sandbox {:allowed-files (:allowed-files drone-sandbox)
                              :allowed-dirs (:allowed-dirs drone-sandbox)
                              :blocked-patterns (map str (:blocked-patterns drone-sandbox))
                              :blocked-tools (:blocked-tools drone-sandbox)}}))))

;;; ============================================================
;;; Phase 4b: Execute Agentic - In-Process Multi-Turn Loop
;;; ============================================================

(defn phase:execute-agentic!
  "Phase 4b: Execute task via in-process agentic loop with session KG.

   Unlike phase:execute! (which delegates to an external function),
   this creates an LLM backend and runs the agentic loop in-process:
   - Multi-turn think-act-observe loop
   - Session KG (Datalevin) for context compression
   - Tool execution via executor with allowlist enforcement
   - Termination heuristics (completion language, max turns, failures)

   Side effects:
   - Emits :drone/started event
   - Shouts to parent ling
   - Records observations in session KG
   - Executes tools via agent executor

   Arguments:
     ctx       - ExecutionContext (must have :kg-store for session KG)
     task-spec - TaskSpec record
     config    - Prepared config from phase:prepare

   Returns:
     Agentic loop result map with :status, :result, :steps, :tokens, etc."
  [ctx task-spec config]
  (let [{:keys [drone-id task-id parent-id project-root pre-validation]} ctx
        {:keys [task files options]} task-spec
        {:keys [tools preset model step-budget]} config
        cwd (or (:cwd task-spec) project-root)

        ;; Augment task with context
        augmented-task (augment/augment-task task files {:project-root cwd})

        ;; Create sandbox
        effective-root (or cwd (diff/get-project-root))
        drone-sandbox (sandbox/create-sandbox (or files []) effective-root)]

    ;; Security check: Reject path escape attempts
    (when (seq (:rejected-files drone-sandbox))
      (log/error {:event :drone/path-escape-blocked
                  :drone-id drone-id
                  :rejected-files (:rejected-files drone-sandbox)})
      (throw (ex-info "File paths escape project directory"
                      {:error-type :path-escape
                       :rejected-files (:rejected-files drone-sandbox)})))

    (log/info "Drone agentic sandbox created"
              {:drone-id drone-id
               :allowed-files (count (:allowed-files drone-sandbox))
               :blocked-tools (count (:blocked-tools drone-sandbox))
               :max-turns step-budget
               :model model})

    ;; Emit started event
    (ev/dispatch [:drone/started {:drone-id drone-id
                                  :task-id task-id
                                  :parent-id parent-id
                                  :files files
                                  :task task
                                  :mode :agentic
                                  :pre-validation (validation/summarize-validation (or pre-validation {}))}])

    ;; Shout to parent ling
    (when parent-id
      (hivemind/shout! parent-id :started
                       {:task (str "Agentic drone: " (subs task 0 (min 80 (count task))))
                        :message (format "Agentic drone %s starting multi-turn loop" drone-id)}))

    ;; Ensure tool registry is initialized
    (registry/ensure-registered!)

    ;; Create LLM backend
    (let [backend (try
                    (require 'hive-mcp.agent.config)
                    ((resolve 'hive-mcp.agent.config/openrouter-backend)
                     {:model model :preset preset})
                    (catch Exception e
                      (log/error {:event :drone/backend-creation-failed
                                  :drone-id drone-id
                                  :model model
                                  :error (.getMessage e)})
                      (throw (ex-info "Failed to create LLM backend"
                                      {:drone-id drone-id
                                       :model model
                                       :error (.getMessage e)}
                                      e))))

          ;; Resolve tool allowlist for this drone
          effective-tools (or tools
                              (let [al (allowlist/resolve-allowlist
                                        {:task-type (:task-type task-spec)})]
                                (vec al)))

          ;; Build permissions set from sandbox
          permissions (if (get options :auto-approve)
                        #{:auto-approve}
                        #{})]

      ;; Execute agentic loop with per-drone KG isolation
      (binding [domain/*drone-kg-store* (:kg-store ctx)]
        (agentic-loop/run-agentic-loop
         {:task augmented-task
          :files (or files [])
          :cwd cwd}
         {:drone-id drone-id
          :kg-store (:kg-store ctx)}
         {:max-turns (or step-budget 10)
          :backend backend
          :tools effective-tools
          :permissions permissions
          :trace? (get options :trace true)
          :agent-id drone-id})))))

;;; ============================================================
;;; Phase 5: Finalize - Diffs, Validation, Metrics
;;; ============================================================

(defn phase:finalize!
  "Phase 5: Handle diffs, validate, record metrics.

   Side effects:
   - Applies or tags diffs
   - Post-validates files
   - Records Prometheus metrics
   - Reports to routing
   - Records result to hivemind
   - Shouts completion to parent

   Arguments:
     ctx         - ExecutionContext
     task-spec   - TaskSpec record
     config      - Prepared config
     raw-result  - Result from execute phase
     diffs-before - Diff IDs before execution

   Returns:
     ExecutionResult record."
  [ctx task-spec config raw-result diffs-before]
  (let [{:keys [drone-id task-id parent-id pre-validation file-contents-before]} ctx
        {:keys [task files options]} task-spec
        {:keys [task-type model]} config
        {:keys [wave-id skip-auto-apply]} options

        ;; Calculate new diffs
        diffs-after (diff-mgmt/capture-diffs-before)
        new-diff-ids (diff-mgmt/get-new-diff-ids diffs-before diffs-after)

        ;; CLARITY-T: Warn if drone completed with 0 diffs (likely text-only response)
        _ (when (and (= :completed (:status raw-result))
                     (empty? new-diff-ids)
                     (seq files))
            (let [result-text (str (:result raw-result))
                  has-code-blocks? (re-find #"```" result-text)]
              (log/warn {:event :drone/zero-diff-completion
                         :drone-id drone-id
                         :files-expected (count files)
                         :has-code-blocks? (boolean has-code-blocks?)
                         :result-preview (subs result-text 0 (min 200 (count result-text)))
                         :message (str "Drone completed with 0 diffs but " (count files)
                                       " files expected. Model likely returned code as text "
                                       "instead of calling propose_diff."
                                       (when has-code-blocks?
                                         " CODE BLOCKS DETECTED in text response."))})))

        ;; Handle diffs
        diff-results (diff-mgmt/handle-diff-results!
                      drone-id new-diff-ids
                      {:wave-id wave-id :skip-auto-apply skip-auto-apply})

        duration-ms (domain/elapsed-ms ctx)

        ;; Post-validation
        post-validation (when (and (seq files)
                                   (= :completed (:status raw-result))
                                   (not skip-auto-apply))
                          (validation/validate-files-post
                           file-contents-before
                           (or pre-validation {})
                           {:lint-level :error
                            :require-modification false}))

        validation-summary (validation/summarize-validation
                            (merge (or pre-validation {}) (or post-validation {})))]

    ;; Log validation warnings
    (when (and post-validation (not (validation/all-valid? post-validation :post)))
      (log/warn {:event :drone/post-validation-warnings
                 :drone-id drone-id
                 :summary validation-summary}))

    ;; Shout completion to parent
    (when parent-id
      (if (= :completed (:status raw-result))
        (hivemind/shout! parent-id :completed
                         {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                          :message (format "Drone %s completed. Files: %s"
                                           drone-id
                                           (diff-mgmt/summarize-diff-results diff-results))})
        (hivemind/shout! parent-id :error
                         {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                          :message (format "Drone %s failed: %s" drone-id (:result raw-result))})))

    ;; Emit completion/failure event
    (if (= :completed (:status raw-result))
      (ev/dispatch [:drone/completed {:drone-id drone-id
                                      :task-id task-id
                                      :parent-id parent-id
                                      :files-modified (:applied diff-results)
                                      :files-failed (:failed diff-results)
                                      :proposed-diff-ids (:proposed diff-results)
                                      :duration-ms duration-ms
                                      :validation validation-summary}])
      (ev/dispatch [:drone/failed {:drone-id drone-id
                                   :task-id task-id
                                   :parent-id parent-id
                                   :error (str (:result raw-result))
                                   :error-type :execution
                                   :files files}]))

    ;; Record model metrics
    (let [model-name (or (:model raw-result) model)
          tokens (:tokens raw-result)
          input-tokens (or (:input-tokens tokens)
                           (cost/count-tokens (:task task-spec)))
          output-tokens (or (:output-tokens tokens)
                            (cost/count-tokens (str (:result raw-result))))]

      (prom/record-drone-result! {:model model-name
                                  :task-type (name task-type)
                                  :success? (= :completed (:status raw-result))
                                  :duration-ms duration-ms
                                  :tokens tokens})

      (cost/track-drone-usage! drone-id
                               {:input-tokens input-tokens
                                :output-tokens output-tokens
                                :task-preview task
                                :wave-id wave-id})

      (routing/report-execution! task-type model-name raw-result
                                 {:duration-ms duration-ms
                                  :directory (:cwd task-spec)
                                  :agent-id drone-id}))

    ;; Record to hivemind
    (hivemind/record-ling-result! drone-id
                                  {:task task
                                   :files files
                                   :result raw-result
                                   :diff-results diff-results
                                   :validation validation-summary
                                   :parent-id parent-id
                                   :timestamp (System/currentTimeMillis)})

    ;; Merge drone KG edges into global store (per-drone KG isolation)
    (when-let [drone-store (:kg-store ctx)]
      (try
        (when (kg/store-set?)
          (let [merge-result (kg-factory/merge-drone-results! drone-store (kg/get-store))]
            (log/info {:event :drone/kg-merge
                       :drone-id drone-id
                       :edges-found (:edges-found merge-result)
                       :edges-merged (:edges-merged merge-result)})))
        (catch Exception e
          (log/warn {:event :drone/kg-merge-failed
                     :drone-id drone-id
                     :error (.getMessage e)}))))

    ;; Build result
    (domain/success-result ctx {:result raw-result
                                :diff-results diff-results
                                :validation validation-summary})))

;;; ============================================================
;;; Phase 6: Cleanup - Always Runs
;;; ============================================================

(defn phase:cleanup!
  "Phase 6: Release resources (always runs).

   Side effects:
   - Releases file claims
   - Closes per-drone KG store (deregisters from factory)
   - Removes drone from DataScript

   Arguments:
     ctx       - ExecutionContext
     task-spec - TaskSpec record"
  [ctx task-spec]
  (let [{:keys [drone-id task-id]} ctx
        {:keys [files]} task-spec]
    (when (seq files)
      (coordinator/release-task-claims! task-id)
      (log/info "Drone released file locks:" drone-id))
    ;; Close per-drone KG store if one was created
    (when (:kg-store ctx)
      (try
        (kg-factory/close-drone-store! drone-id)
        (log/debug "Drone KG store closed:" drone-id)
        (catch Exception e
          (log/warn {:event :drone/kg-cleanup-failed
                     :drone-id drone-id
                     :error (.getMessage e)}))))
    (ds/remove-slave! drone-id)))

;;; ============================================================
;;; Phase: Handle Error
;;; ============================================================

(defn phase:handle-error!
  "Handle execution error with proper logging and metrics.

   Arguments:
     ctx       - ExecutionContext
     task-spec - TaskSpec record
     config    - Prepared config
     exception - The caught exception

   Returns:
     nil (re-throws after handling)."
  [ctx task-spec config exception]
  (let [{:keys [drone-id task-id parent-id]} ctx
        {:keys [files]} task-spec
        {:keys [model task-type]} config
        duration-ms (domain/elapsed-ms ctx)
        structured (errors/structure-error exception)]

    (log/error {:event :drone/error
                :error-type (:error-type structured)
                :drone-id drone-id
                :task-id task-id
                :parent-id parent-id
                :model model
                :files files
                :duration-ms duration-ms
                :message (:message structured)
                :stacktrace (subs (or (:stacktrace structured) "") 0
                                  (min 500 (count (or (:stacktrace structured) ""))))})

    (ev/dispatch [:drone/failed {:drone-id drone-id
                                 :task-id task-id
                                 :parent-id parent-id
                                 :error (:message structured)
                                 :error-type (:error-type structured)
                                 :stacktrace (:stacktrace structured)
                                 :files files}])

    (prom/record-drone-result! {:model model
                                :task-type (name task-type)
                                :success? false
                                :duration-ms duration-ms
                                :retry? (= :timeout (:error-type structured))
                                :retry-reason (:error-type structured)})

    (routing/report-execution! task-type model
                               {:status :failed :error (:message structured)}
                               {:duration-ms duration-ms
                                :directory (:cwd task-spec)
                                :agent-id drone-id})))

;;; ============================================================
;;; Orchestrator - Compose Phases
;;; ============================================================

(defn run-execution!
  "Orchestrate drone execution through all phases.

   This is the main entry point that composes all phases with
   proper error handling and cleanup.

   Arguments:
     task-spec   - TaskSpec record
     delegate-fn - Function to execute the task

   Returns:
     ExecutionResult record."
  [task-spec delegate-fn]
  (let [drone-id (domain/generate-drone-id)
        task-id (domain/generate-task-id drone-id)
        parent-id (or (:parent-id task-spec)
                      (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        cwd (or (:cwd task-spec) (diff/get-project-root))

        ;; Create per-drone isolated KG store
        drone-kg-store (try
                         (kg-factory/create-drone-store drone-id)
                         (catch Exception e
                           (log/warn {:event :drone/kg-store-creation-failed
                                      :drone-id drone-id
                                      :error (.getMessage e)})
                           nil))

        ;; Create execution context with per-drone KG store
        ctx (domain/->execution-context
             {:drone-id drone-id
              :task-id task-id
              :parent-id parent-id
              :project-root cwd
              :kg-store drone-kg-store})

        ;; Phase 1: Prepare (pure)
        config (phase:prepare task-spec)
        ctx (assoc ctx :model (:model config))]

    (try
      ;; Phase 2: Register
      (let [ctx (phase:register! ctx task-spec)
            ;; Capture diffs before execution
            diffs-before (diff-mgmt/capture-diffs-before)]

        ;; Phase 3: Validate
        (let [ctx (phase:validate ctx task-spec)]

          (try
            ;; Phase 4: Execute
            (let [raw-result (phase:execute! ctx task-spec config delegate-fn)]

              ;; Phase 5: Finalize
              (phase:finalize! ctx task-spec config raw-result diffs-before))

            (catch Exception e
              (phase:handle-error! ctx task-spec config e)
              (throw e)))))

      (finally
        ;; Phase 6: Cleanup (always)
        (phase:cleanup! ctx task-spec)))))

;;; ============================================================
;;; Agentic Orchestrator - In-Process Multi-Turn Execution
;;; ============================================================

(defn run-agentic-execution!
  "Orchestrate agentic drone execution through all phases.

   Like run-execution! but uses the in-process agentic loop (phase:execute-agentic!)
   instead of delegating to an external function. The agentic loop provides:
   - Multi-turn think-act-observe with session KG (Datalevin)
   - Context compression via KG reconstruction (~5-10x vs raw history)
   - Termination heuristics (completion language, max turns, failures)
   - Per-drone KG isolation with merge-back to global

   This is the primary entry point for in-process drone execution.

   Arguments:
     task-spec - TaskSpec record (or map coercible to one)

   Returns:
     ExecutionResult record."
  [task-spec]
  (let [drone-id (domain/generate-drone-id)
        task-id (domain/generate-task-id drone-id)
        parent-id (or (:parent-id task-spec)
                      (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        cwd (or (:cwd task-spec) (diff/get-project-root))

        ;; Create per-drone isolated session KG (Datalevin-backed)
        session-kg-store (try
                           (session-kg/create-session-kg! drone-id)
                           (catch Exception e
                             (log/warn {:event :drone/session-kg-creation-failed
                                        :drone-id drone-id
                                        :error (.getMessage e)})
                             ;; Fall back to DataScript in-memory KG
                             (try
                               (kg-factory/create-drone-store drone-id)
                               (catch Exception e2
                                 (log/warn {:event :drone/kg-store-fallback-failed
                                            :drone-id drone-id
                                            :error (.getMessage e2)})
                                 nil))))

        ;; Create execution context with session KG store
        ctx (domain/->execution-context
             {:drone-id drone-id
              :task-id task-id
              :parent-id parent-id
              :project-root cwd
              :kg-store session-kg-store})

        ;; Phase 1: Prepare (pure)
        config (phase:prepare task-spec)
        ctx (assoc ctx :model (:model config))]

    (try
      ;; Phase 2: Register
      (let [ctx (phase:register! ctx task-spec)
            ;; Capture diffs before execution
            diffs-before (diff-mgmt/capture-diffs-before)]

        ;; Phase 3: Validate
        (let [ctx (phase:validate ctx task-spec)]

          (try
            ;; Phase 4b: Execute Agentic (in-process multi-turn loop)
            (let [raw-result (phase:execute-agentic! ctx task-spec config)]

              ;; Phase 5: Finalize
              (phase:finalize! ctx task-spec config raw-result diffs-before))

            (catch Exception e
              (phase:handle-error! ctx task-spec config e)
              (throw e)))))

      (finally
        ;; Phase 6: Cleanup (always)
        ;; Close session KG and clean up temp directory
        (when session-kg-store
          (try
            (session-kg/close-session-kg! session-kg-store drone-id)
            (catch Exception e
              (log/warn {:event :drone/session-kg-cleanup-failed
                         :drone-id drone-id
                         :error (.getMessage e)}))))
        (phase:cleanup! ctx task-spec)))))
