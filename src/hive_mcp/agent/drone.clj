(ns hive-mcp.agent.drone
  "Drone delegation - token-optimized leaf agents.

   Drones are lightweight agents that:
   - Use OpenRouter free-tier models
   - Pre-inject file contents (no read tool calls needed)
   - Use propose_diff instead of direct file writes
   - Auto-apply proposed diffs on completion
   - Report status to parent lings for swarm sync
   - Receive smart context injection (imports, lint, conventions)

   Implements IAgent protocol for unified lifecycle management.

   Architecture (post-decomposition):
   - domain.clj      - Value objects (TaskSpec, ExecutionContext, ExecutionResult)
   - execution.clj   - Phase-based orchestration
   - augment.clj     - Task context preparation
   - diff-mgmt.clj   - Diff lifecycle management"
  (:require [hive-mcp.agent.protocol :refer [IAgent]]
            [hive-mcp.agent.drone.domain :as domain]
            [hive-mcp.agent.drone.execution :as execution]
            [hive-mcp.agent.drone.augment :as augment]
            [hive-mcp.agent.drone.diff-mgmt :as diff-mgmt]
            [hive-mcp.agent.drone.decompose :as decompose]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.retry :as retry]
            [hive-mcp.agent.config :as config]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.agent.drone.tools :as drone-tools]
            [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.tools.diff :as diff]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.events.core :as ev]
            [hive-mcp.telemetry.prometheus :as prom]
            [clojure.set]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Configuration - Tool Minimization
;;; ============================================================

;; Tool profiles and filtering are in hive-mcp.agent.drone.tools
;; See: drone-tools/tool-profiles for task-specific tool sets
;; See: drone-tools/get-tools-for-drone for tool selection

(def allowed-tools
  "DEPRECATED: Use drone-tools/get-tools-for-drone with task-type.
   Kept for backward compatibility - returns full legacy tool set."
  (vec drone-tools/legacy-allowed-tools))

;; Context preparation functions are now in hive-mcp.agent.drone.augment
;; Diff management functions are now in hive-mcp.agent.drone.diff-mgmt
;; See: augment/augment-task, augment/prepare-context, augment/format-file-contents
;; See: diff-mgmt/auto-apply-diffs!, diff-mgmt/tag-diffs-with-wave!

;;; ============================================================
;;; Public API
;;; ============================================================

(defn delegate!
  "Delegate a task to a drone (token-optimized leaf agent).

   Uses phase-based execution (see execution.clj):
   1. prepare    - Infer task type, select model, create sandbox
   2. register   - Register drone in DataScript, claim files
   3. validate   - Pre-execution file validation
   4. execute    - Run task with sandbox constraints
   5. finalize   - Apply diffs, validate, record metrics
   6. cleanup    - Release locks, remove registration

   Options:
     :task           - Task description (required)
     :files          - List of files the drone will modify (contents pre-injected)
     :task-type      - Explicit task type (:testing, :refactoring, :bugfix, :documentation, :general)
                       If nil, auto-inferred from task description
     :preset         - Override preset (default: auto-selected based on task-type)
     :trace          - Enable progress events (default: true)
     :parent-id      - Parent ling's slave-id (for swarm status sync)
     :cwd            - Working directory override for path resolution
     :skip-auto-apply - When true, don't auto-apply diffs (for validated wave mode)
     :wave-id        - Wave ID to tag proposed diffs for batch review

   Tool Minimization:
     Drones receive minimal tools based on task-type:
     - :testing       - read_file, kondo_lint, cider_eval_silent
     - :refactoring   - read_file, grep, glob_files, kondo_lint, kondo_analyze
     - :bugfix        - read_file, kondo_lint, magit_diff
     - :documentation - read_file (most restrictive)
     - :general       - read_file, kondo_lint (default)

   Returns:
     ExecutionResult record with :status, :agent-id, :files-modified, etc.

   Sandbox enforcement:
     - Drones can only read/write files in their :files list
     - Blocked: bash, memory writes, agent spawning
     - Blocked patterns: .env, credentials, secrets, keys

   Throws ex-info if file conflicts detected (files locked by another drone)."
  [{:keys [task files task-type preset trace parent-id cwd skip-auto-apply wave-id]
    :or {trace true
         skip-auto-apply false}}
   delegate-fn]
  ;; Create TaskSpec from options (CLARITY-R: Represented Intent)
  (let [task-spec (domain/->task-spec
                   {:task task
                    :files files
                    :task-type task-type
                    :preset preset
                    :cwd cwd
                    :parent-id parent-id
                    :wave-id wave-id
                    :trace trace
                    :skip-auto-apply skip-auto-apply})]
    ;; Delegate to phase-based execution orchestrator
    ;; All complexity is now in execution.clj phases
    (execution/run-execution! task-spec delegate-fn)))

;;; ============================================================
;;; Agentic Delegation (In-Process Multi-Turn Loop)
;;; ============================================================

(defn delegate-agentic!
  "Delegate a task to an in-process agentic drone with session KG.

   Unlike delegate! (which requires an external delegate-fn), this runs
   the full agentic loop in-process:
   - Creates an OpenRouter LLM backend
   - Runs a think-act-observe loop with tool calling
   - Uses a Datalevin-backed session KG for context compression
   - Terminates via structural heuristics (completion language, max turns)
   - Merges session KG edges to global KG on success

   This is the primary entry point for autonomous drone task execution.
   No external execution function needed — everything runs in-process.

   Options:
     :task           - Task description (required)
     :files          - List of files the drone will modify (contents pre-injected)
     :task-type      - Explicit task type (:testing, :refactoring, :bugfix, :documentation, :general)
                       If nil, auto-inferred from task description
     :preset         - Override preset (default: auto-selected based on task-type)
     :trace          - Enable progress events (default: true)
     :parent-id      - Parent ling's slave-id (for swarm status sync)
     :cwd            - Working directory override for path resolution
     :skip-auto-apply - When true, don't auto-apply diffs (for validated wave mode)
     :wave-id        - Wave ID to tag proposed diffs for batch review

   Returns:
     ExecutionResult record with :status, :agent-id, :files-modified, etc.

   Throws ex-info if file conflicts detected (files locked by another drone).

   Example:
     (delegate-agentic! {:task \"Fix the nil check in parse-config\"
                          :files [\"src/config.clj\"]
                          :cwd \"/home/user/project\"})"
  [{:keys [task files task-type preset trace parent-id cwd skip-auto-apply wave-id]
    :or {trace true
         skip-auto-apply false}}]
  ;; Create TaskSpec from options (CLARITY-R: Represented Intent)
  (let [task-spec (domain/->task-spec
                   {:task task
                    :files files
                    :task-type task-type
                    :preset preset
                    :cwd cwd
                    :parent-id parent-id
                    :wave-id wave-id
                    :trace trace
                    :skip-auto-apply skip-auto-apply})]
    ;; Delegate to agentic execution orchestrator
    ;; Uses in-process agentic loop with session KG (Datalevin)
    (execution/run-agentic-execution! task-spec)))

;;; ============================================================
;;; Retry-Enabled Delegation
;;; ============================================================

(defn delegate-with-retry!
  "Delegate a task to a drone with automatic retry on transient failures.

   Wraps delegate! with exponential backoff retry logic:
   - Rate limits: Wait longer + retry with alternative model
   - Timeouts: Retry with backoff
   - Model errors: Try fallback model (coding -> coding-alt -> docs)
   - Auth errors: Fail fast (permanent error)

   Options (in addition to delegate! options):
     :max-retries       - Maximum retry attempts (default: 3)
     :initial-delay-ms  - Initial backoff delay (default: 1000)
     :max-delay-ms      - Maximum backoff delay (default: 30000)
     :on-retry          - Callback fn [attempt ex strategy] called on each retry

   Returns:
     Same as delegate!, with additional :retry-info map:
       :retries      - Number of retries performed
       :models-tried - List of models attempted
       :total-duration-ms - Total time including retries

   CLARITY-Y: Graceful degradation through smart retries.
   CLARITY-T: All retries are logged for observability.

   Example:
     (delegate-with-retry!
       {:task \"Fix the bug\"
        :files [\"src/core.clj\"]
        :max-retries 3}
       agent/delegate!)"
  [{:keys [_task _files preset max-retries initial-delay-ms max-delay-ms on-retry]
    :or {max-retries 3
         initial-delay-ms 1000
         max-delay-ms 30000}
    :as opts}
   delegate-fn]
  ;; BUG FIX: Use UUID instead of millis to prevent collision in parallel retries
  (let [agent-id (str "drone-retry-" (java.util.UUID/randomUUID))
        current-model (config/resolve-model {:preset (or preset "drone-worker")})
        retry-opts {:max-retries max-retries
                    :initial-delay-ms initial-delay-ms
                    :max-delay-ms max-delay-ms
                    :model current-model
                    :preset preset
                    :drone-id agent-id
                    :task-id (str "task-" agent-id)
                    :on-retry on-retry}
        start-time (System/currentTimeMillis)
        retry-count (atom 0)
        models-tried (atom [current-model])]

    (try
      (let [result (retry/with-retry
                     (fn [exec-opts]
                       ;; Execute delegate! with potentially updated model
                       (let [effective-opts (if-let [new-model (:model exec-opts)]
                                              (assoc opts :model new-model)
                                              opts)]
                         (delegate! effective-opts delegate-fn)))
                     (assoc retry-opts
                            :on-retry (fn [attempt ex strategy]
                                        (swap! retry-count inc)
                                        (when-let [new-model (:model strategy)]
                                          (swap! models-tried conj new-model))
                                        ;; Call user callback if provided
                                        (when on-retry
                                          (on-retry attempt ex strategy)))))]

        ;; Add retry info to result
        (assoc result
               :retry-info {:retries @retry-count
                            :models-tried @models-tried
                            :total-duration-ms (- (System/currentTimeMillis) start-time)}))

      (catch Exception e
        ;; Log final failure with retry context
        (log/error {:event :drone/retry-exhausted
                    :drone-id agent-id
                    :retries @retry-count
                    :models-tried @models-tried
                    :total-duration-ms (- (System/currentTimeMillis) start-time)
                    :error (ex-message e)})
        (throw (ex-info "Drone execution failed after retries"
                        {:retries @retry-count
                         :models-tried @models-tried
                         :total-duration-ms (- (System/currentTimeMillis) start-time)
                         :original-error (ex-message e)}
                        e))))))

;;; ============================================================
;;; Drone Record - IAgent Implementation
;;; ============================================================

(defrecord Drone [id model task-type cwd max-steps parent-id project-id
                  ;; Internal state managed by lifecycle (atom for thread-safe mutability)
                  state-atom]
  IAgent

  (spawn! [_this opts]
    "Spawn the drone agent.

     Registers the drone in DataScript and optionally claims files.
     Does NOT execute the task - use dispatch! for that.

     Options:
       :files     - Files to claim for exclusive access
       :parent-id - Parent ling's slave-id (for swarm state sync)
       :task-id   - Optional explicit task-id

     Returns:
       The drone-id on success"
    (let [{:keys [files task-id]} opts
          effective-task-id (or task-id (str "task-" id))
          effective-parent (or (:parent-id opts) parent-id
                               (System/getenv "CLAUDE_SWARM_SLAVE_ID"))]

      ;; 1. Register in DataScript
      (let [tx-result (ds/add-slave! id {:slave/status :spawning
                                         :slave/name "drone"
                                         :slave/agent-type :drone
                                         :slave/depth 2
                                         :slave/parent effective-parent
                                         :slave/cwd cwd
                                         :slave/project-id project-id})]
        (when-not (and tx-result (seq (:tx-data tx-result)))
          (log/error {:event :drone/spawn-failed
                      :drone-id id
                      :reason "DataScript registration failed"})
          (throw (ex-info "Failed to register drone in DataScript"
                          {:drone-id id}))))

      ;; 2. Claim files if provided
      (when (seq files)
        (let [result (coordinator/atomic-claim-files! effective-task-id id files)]
          (if (:acquired? result)
            (do
              (swap! state-atom assoc
                     :claimed-files (vec files)
                     :current-task-id effective-task-id)
              (log/info "Drone spawned and claimed files"
                        {:drone-id id :files-claimed (:files-claimed result)}))
            (do
              ;; Rollback DataScript registration on claim failure
              (ds/remove-slave! id)
              (throw (ex-info "Failed to claim files during spawn"
                              {:drone-id id
                               :conflicts (:conflicts result)}))))))

      ;; 3. Emit started event
      (ev/dispatch [:drone/started {:drone-id id
                                    :parent-id effective-parent
                                    :files (or files [])
                                    :task-type task-type}])

      (log/info "Drone spawned" {:id id :task-type task-type :files (count (or files 0))})
      id))

  (dispatch! [_this task-opts]
    "Dispatch a task to this drone.

     Executes the task using the configured model and returns result.
     Uses the existing delegate! function under the hood.

     Options:
       :task            - Task description (required)
       :files           - Additional files to include (merged with claimed-files)
       :delegate-fn     - Custom delegation function (default: uses OpenRouter)
       :skip-auto-apply - When true, don't auto-apply diffs
       :wave-id         - Wave ID to tag proposed diffs

     Returns:
       Result map with :status, :result, :agent-id, :files-modified, etc."
    (let [{:keys [task files delegate-fn skip-auto-apply wave-id trace]
           :or {trace true}} task-opts
          {:keys [claimed-files current-task-id]} @state-atom
          effective-files (vec (distinct (concat (or claimed-files []) (or files []))))
          task-id (or current-task-id (str "task-" id "-" (System/currentTimeMillis)))]

      ;; Update status to :working
      (ds/update-slave! id {:slave/status :working})

      ;; Delegate using existing machinery
      ;; Note: We inline the core delegation logic here since delegate! manages its own lifecycle
      (let [;; Infer task-type if not explicitly provided
            effective-task-type (or task-type (preset/get-task-type task effective-files))
            minimal-tools (drone-tools/get-tools-for-drone effective-task-type effective-files)
            effective-preset (preset/select-drone-preset task effective-files)
            model-selection (routing/route-and-select task effective-files {:directory cwd})
            selected-model (or model (:model model-selection))
            step-budget (or max-steps (decompose/get-step-budget task effective-files))
            augmented-task (augment/augment-task task effective-files {:project-root cwd})
            diffs-before (set (keys @diff/pending-diffs))
            effective-root (or cwd (diff/get-project-root))
            drone-sandbox (sandbox/create-sandbox (or effective-files []) effective-root)

            ;; Execute via delegate-fn or default OpenRouter
            execution-fn (or delegate-fn
                             (fn [_opts]
                               ;; This should be injected or use a default agent impl
                               (throw (ex-info "No delegate-fn provided - use delegate! for standalone execution"
                                               {:drone-id id}))))
            start-time (System/currentTimeMillis)
            result (execution-fn {:backend :openrouter
                                  :preset effective-preset
                                  :model selected-model
                                  :task augmented-task
                                  :tools minimal-tools
                                  :max-steps step-budget
                                  :trace trace
                                  :sandbox {:allowed-files (:allowed-files drone-sandbox)
                                            :allowed-dirs (:allowed-dirs drone-sandbox)
                                            :blocked-patterns (map str (:blocked-patterns drone-sandbox))
                                            :blocked-tools (:blocked-tools drone-sandbox)}})
            diffs-after (set (keys @diff/pending-diffs))
            new-diff-ids (clojure.set/difference diffs-after diffs-before)
            duration-ms (- (System/currentTimeMillis) start-time)

            ;; Handle diff application
            _ (when (and wave-id (seq new-diff-ids))
                (diff-mgmt/tag-diffs-with-wave! new-diff-ids wave-id))
            diff-results (if skip-auto-apply
                           {:applied [] :failed [] :proposed (vec new-diff-ids)}
                           (diff-mgmt/auto-apply-diffs! id new-diff-ids))]

        ;; Emit completion event
        (if (= :completed (:status result))
          (ev/dispatch [:drone/completed {:drone-id id
                                          :task-id task-id
                                          :parent-id parent-id
                                          :files-modified (:applied diff-results)
                                          :duration-ms duration-ms}])
          (ev/dispatch [:drone/failed {:drone-id id
                                       :task-id task-id
                                       :parent-id parent-id
                                       :error (str (:result result))
                                       :error-type :execution}]))

        ;; Record model metrics
        (prom/record-drone-result! {:model selected-model
                                    :task-type (name effective-task-type)
                                    :success? (= :completed (:status result))
                                    :duration-ms duration-ms})

        (assoc result
               :agent-id id
               :task-id task-id
               :files-modified (:applied diff-results)
               :proposed-diff-ids (:proposed diff-results)
               :duration-ms duration-ms))))

  (status [_this]
    "Get current drone status from DataScript.

     Returns:
       Status map with :slave/id, :slave/status, :slave/cwd, etc.
       or nil if drone not found"
    (let [slave-info (ds/get-slave id)
          {:keys [claimed-files current-task-id]} @state-atom]
      (when slave-info
        (assoc slave-info
               :claimed-files (or claimed-files [])
               :current-task-id current-task-id))))

  (kill! [this]
    "Terminate the drone and release all resources.

     - Releases file claims
     - Removes from DataScript
     - Emits :drone/failed event

     Returns:
       {:killed? bool :id drone-id}"
    (try
      ;; 1. Release claims
      (.release-claims! this)

      ;; 2. Remove from DataScript
      (ds/remove-slave! id)

      ;; 3. Emit event
      (ev/dispatch [:drone/failed {:drone-id id
                                   :error "Killed by request"
                                   :error-type :killed}])

      (log/info "Drone killed" {:id id})
      {:killed? true :id id}

      (catch Exception e
        (log/error "Error killing drone" {:id id :error (ex-message e)})
        {:killed? false :id id :error (ex-message e)})))

  (agent-type [_]
    :drone)

  (can-chain-tools? [_]
    "Drones cannot chain multiple tool calls - they are single-shot executors."
    false)

  (claims [_this]
    "Get list of files currently claimed by this drone.

     Queries core.logic pldb for claims associated with this drone-id."
    (or (:claimed-files @state-atom)
        (->> (logic/get-all-claims)
             (filter #(= id (:slave-id %)))
             (map :file)
             vec)))

  (claim-files! [_this files task-id]
    "Claim files for exclusive access during task.

     Uses coordinator/atomic-claim-files! for race-free claiming.

     Returns:
       {:acquired? bool :conflicts [...] :files-claimed N}"
    (when (seq files)
      (let [{:keys [claimed-files current-task-id]} @state-atom
            effective-task-id (or task-id current-task-id (str "task-" id))
            result (coordinator/atomic-claim-files! effective-task-id id files)]
        (when (:acquired? result)
          (swap! state-atom assoc
                 :claimed-files (vec (distinct (concat (or claimed-files []) files)))
                 :current-task-id effective-task-id))
        (log/info "Drone claim-files!" {:drone-id id
                                        :acquired? (:acquired? result)
                                        :files-claimed (:files-claimed result)})
        result)))

  (release-claims! [_this]
    "Release all file claims held by this drone.

     Uses coordinator/release-task-claims! if task-id is known,
     otherwise falls back to logic/release-claims-for-slave!."
    (let [{:keys [claimed-files current-task-id]} @state-atom
          files-count (count (or claimed-files []))]
      (if current-task-id
        (coordinator/release-task-claims! current-task-id)
        (logic/release-claims-for-slave! id))
      (swap! state-atom assoc :claimed-files nil :current-task-id nil)
      (log/info "Drone released claims" {:id id :count files-count})
      files-count))

  (upgrade! [_this]
    "Upgrade drone to a ling when task requires tool chaining.

     Spawns a new ling with the same cwd and transfers claims.
     Marks this drone as :upgraded.

     Returns:
       {:ling-id new-id :inherited-claims [...]} or nil on failure"
    (try
      (let [ling-id (str "ling-" (java.util.UUID/randomUUID))
            current-claims (or (:claimed-files @state-atom) [])]

        ;; Mark drone as upgraded
        (ds/update-slave! id {:slave/status :upgraded})

        ;; Note: Actual ling creation requires emacs integration
        ;; This provides the specification for the upgrade
        (log/info "Drone upgrade requested" {:drone-id id
                                             :ling-id ling-id
                                             :claims current-claims})

        {:ling-id ling-id
         :cwd cwd
         :inherited-claims current-claims
         :parent-id parent-id
         :project-id project-id})

      (catch Exception e
        (log/error "Drone upgrade failed" {:id id :error (ex-message e)})
        nil))))

;;; ============================================================
;;; Factory Functions
;;; ============================================================

(defn ->drone
  "Create a new Drone agent instance.

   Arguments:
     id   - Unique identifier for this drone
     opts - Map with optional keys:
            :model      - Override model (default: auto-selected)
            :task-type  - Task type hint (:testing, :refactoring, etc.)
            :cwd        - Working directory
            :max-steps  - Maximum execution steps
            :parent-id  - Parent ling's slave-id
            :project-id - Project ID for scoping

   Returns:
     Drone record implementing IAgent protocol

   Example:
     (->drone \"drone-123\" {:cwd \"/project\"
                             :task-type :testing
                             :parent-id \"ling-001\"})"
  [id opts]
  (map->Drone {:id id
               :model (:model opts)
               :task-type (:task-type opts)
               :cwd (:cwd opts)
               :max-steps (:max-steps opts)
               :parent-id (:parent-id opts)
               :project-id (:project-id opts)
               :state-atom (atom {:claimed-files nil
                                  :current-task-id nil})}))

(defn create-drone!
  "Create and spawn a new drone agent.

   Convenience function that creates the Drone record and spawns it.

   Arguments:
     id   - Unique identifier
     opts - Spawn options (see spawn! and ->drone)

   Returns:
     The drone ID on success, throws on failure"
  [id opts]
  (let [drone (->drone id opts)]
    (.spawn! drone opts)))

;;; ============================================================
;;; Drone Query Functions
;;; ============================================================

(defn get-drone
  "Get a drone by ID as a Drone record.

   Reconstitutes the Drone record from DataScript state.

   Returns:
     Drone record or nil if not found"
  [id]
  (when-let [slave (ds/get-slave id)]
    (when (= :drone (:slave/agent-type slave))
      (->drone id {:cwd (:slave/cwd slave)
                   :parent-id (when-let [p (:slave/parent slave)]
                                (:slave/id p))
                   :project-id (:slave/project-id slave)}))))

(defn list-drones
  "List all active drones, optionally filtered by project-id.

   Arguments:
     project-id - Optional project ID filter

   Returns:
     Seq of Drone records"
  [& [project-id]]
  (let [slaves (if project-id
                 (ds/get-slaves-by-project project-id)
                 (ds/get-all-slaves))]
    (->> slaves
         ;; Filter to depth 2 (drones) or agent-type :drone
         (filter #(or (= 2 (:slave/depth %))
                      (= :drone (:slave/agent-type %))))
         (map (fn [s]
                (->drone (:slave/id s)
                         {:cwd (:slave/cwd s)
                          :parent-id (when-let [p (:slave/parent s)]
                                       (:slave/id p))
                          :project-id (:slave/project-id s)}))))))
