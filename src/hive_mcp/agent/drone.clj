(ns hive-mcp.agent.drone
  "Drone delegation - token-optimized leaf agents.

   Drones are lightweight agents that:
   - Use OpenRouter free-tier models
   - Pre-inject file contents (no read tool calls needed)
   - Use propose_diff instead of direct file writes
   - Auto-apply proposed diffs on completion
   - Report status to parent lings for swarm sync
   - Receive smart context injection (imports, lint, conventions)"
  (:require [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.drone.errors :as errors]
            [hive-mcp.agent.drone.context :as ctx]
            [hive-mcp.agent.drone.decompose :as decompose]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.validation :as validation]
            [hive-mcp.agent.drone.retry :as retry]
            [hive-mcp.agent.config :as config]
            [hive-mcp.agent.routing :as routing]
            [hive-mcp.agent.cost :as cost]
            [hive-mcp.agent.drone.tools :as drone-tools]
            [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.tools.diff :as diff]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.events.core :as ev]
            [hive-mcp.telemetry.prometheus :as prom]
            [clojure.data.json :as json]
            [clojure.string :as str]
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

;;; ============================================================
;;; Context Preparation
;;; ============================================================

(defn- prepare-context
  "Prepare context for drone delegation by gathering catchup data."
  []
  (try
    (let [catchup-handler (registry/get-tool "mcp_get_context")
          context (when (:handler catchup-handler)
                    ((:handler catchup-handler) {}))]
      (if (and context (:text context))
        (let [parsed (json/read-str (:text context) :key-fn keyword)]
          {:conventions (get-in parsed [:memory :conventions] [])
           :decisions (get-in parsed [:memory :decisions] [])
           :snippets (get-in parsed [:memory :snippets] [])
           :project (get parsed :project {})})
        {}))
    (catch Exception e
      (log/warn e "Failed to gather ling context")
      {})))

(defn- format-context-str
  "Format context data as string for task augmentation."
  [context]
  (when (seq context)
    (str "## Project Context\n"
         (when (seq (:conventions context))
           (str "### Conventions\n"
                (str/join "\n" (map :content (:conventions context)))
                "\n\n"))
         (when (seq (:decisions context))
           (str "### Decisions\n"
                (str/join "\n" (map :content (:decisions context)))
                "\n\n")))))

(defn- format-file-contents
  "Pre-read file contents so drone has exact content for propose_diff.
   Records disc entity reads for L1 file state tracking.

   Arguments:
     files        - List of file paths to read
     project-root - Optional project root override (defaults to diff/get-project-root)

   CLARITY-I: Validates paths don't escape project directory before reading."
  [files & [project-root-override]]
  (when (seq files)
    (let [project-root (or project-root-override (diff/get-project-root) "")
          contents (for [f files]
                     ;; SECURITY FIX: Validate path containment before reading
                     (let [validation (sandbox/validate-path-containment f project-root)]
                       (if (:valid? validation)
                         (try
                           (let [content (slurp (:canonical-path validation))]
                             ;; Track disc read (async, non-blocking)
                             (future
                               (try
                                 (kg-disc/touch-disc! (:canonical-path validation))
                                 (catch Exception e
                                   (log/debug "Disc touch failed (non-fatal):" (.getMessage e)))))
                             (str "### " f "\n```\n" content "```\n"))
                           (catch Exception e
                             (str "### " f "\n(File not found or unreadable: " (.getMessage e) ")\n")))
                         ;; Path escapes project - reject with security warning
                         (do
                           (log/warn "Path validation failed in format-file-contents"
                                     {:file f :error (:error validation)})
                           (str "### " f "\n(BLOCKED: " (:error validation) ")\n")))))]
      (str "## Current File Contents\n"
           "IMPORTANT: Use this EXACT content as old_content in propose_diff.\n"
           "Do NOT guess or assume file content - use what is provided below.\n\n"
           (str/join "\n" contents)))))

(defn- build-smart-context
  "Build smart context for each target file.

   Gathers:
   - Imports/requires from ns form
   - Related function signatures
   - Existing lint warnings
   - Relevant conventions from memory

   Arguments:
     files        - List of file paths
     task         - Task description
     project-root - Project root directory
     project-id   - Project ID for memory scoping

   Returns:
     Formatted context string"
  [files task project-root project-id]
  (when (seq files)
    (let [contexts (for [f files]
                     (try
                       (let [ctx-data (ctx/build-drone-context
                                       {:file-path f
                                        :task task
                                        :project-root project-root
                                        :project-id project-id})]
                         (when (:formatted ctx-data)
                           (str "## Smart Context for " f "\n"
                                (:formatted ctx-data))))
                       (catch Exception e
                         (log/debug "Could not build smart context for" f (.getMessage e))
                         nil)))
          non-nil-contexts (remove nil? contexts)]
      (when (seq non-nil-contexts)
        (str/join "\n\n" non-nil-contexts)))))

(defn- augment-task
  "Augment task with context and file contents.

   Arguments:
     task         - Task description
     files        - List of files to include
     project-root - Optional project root override for path resolution
     project-id   - Optional project ID for memory scoping"
  [task files & [{:keys [project-root project-id]}]]
  (let [effective-root (or project-root (diff/get-project-root) "")
        effective-project-id (or project-id "hive-mcp")
        context (prepare-context)
        context-str (format-context-str context)
        smart-ctx-str (build-smart-context files task effective-root effective-project-id)
        file-contents-str (format-file-contents files project-root)]
    (str context-str
         (when smart-ctx-str
           (str "\n" smart-ctx-str "\n"))
         ;; CRITICAL: Inject project root so drones use correct directory in propose_diff
         (when (seq effective-root)
           (str "## Project Directory\n"
                "IMPORTANT: When calling propose_diff, you MUST include:\n"
                "  directory: \"" effective-root "\"\n"
                "This ensures paths are validated against YOUR project, not the MCP server.\n\n"))
         "## Task\n" task
         (when (seq files)
           (str "\n\n## Files to modify\n"
                (str/join "\n" (map #(str "- " %) files))))
         (when file-contents-str
           (str "\n\n" file-contents-str)))))

;;; ============================================================
;;; Diff Management
;;; ============================================================

;; LOGGING STRATEGY (CLARITY-T):
;; This module uses two complementary logging approaches:
;; 1. Direct log/info/warn for operational details (lock acquisition, diff application)
;; 2. Events (:drone/*) for lifecycle telemetry (started, completed, failed)
;; These are NOT redundant - operational logs aid debugging while events provide
;; structured telemetry for monitoring dashboards and swarm coordination.

(defn- auto-apply-diffs
  "Auto-apply diffs proposed during drone execution.

   Returns map with :applied [files] and :failed [{:file :error}].

   CLARITY-T: Logs operational details directly for debugging."
  [drone-id new-diff-ids]
  (when (seq new-diff-ids)
    (let [results (for [diff-id new-diff-ids]
                    (let [diff-info (get @diff/pending-diffs diff-id)
                          response (diff/handle-apply-diff {:diff_id diff-id})
                          parsed (try (json/read-str (:text response) :key-fn keyword)
                                      (catch Exception _ nil))]
                      (if (:isError response)
                        {:status :failed :file (:file-path diff-info) :error (:error parsed)}
                        {:status :applied :file (:file-path diff-info)})))
          {applied :applied failed :failed} (group-by :status results)]
      (when (seq applied)
        (log/info "Auto-applied drone diffs" {:drone drone-id :files (mapv :file applied)}))
      (when (seq failed)
        (log/warn "Some drone diffs failed to apply" {:drone drone-id :failures failed}))
      {:applied (mapv :file applied)
       :failed (mapv #(select-keys % [:file :error]) failed)})))

(defn- tag-diffs-with-wave!
  "Tag newly proposed diffs with wave-id for batch review tracking.

   Used by validated wave execution to associate diffs with their wave
   so they can be reviewed together before applying."
  [new-diff-ids wave-id]
  (when (and (seq new-diff-ids) wave-id)
    (doseq [diff-id new-diff-ids]
      (swap! diff/pending-diffs update diff-id assoc :wave-id wave-id))
    (log/debug "Tagged diffs with wave-id" {:wave-id wave-id :count (count new-diff-ids)})))

;;; ============================================================
;;; Public API
;;; ============================================================

(defn delegate!
  "Delegate a task to a drone (token-optimized leaf agent).

   Automatically:
   - Acquires file locks via coordinator (conflict prevention)
   - Pre-injects file contents (drone doesn't need to read)
   - Injects catchup context (conventions, decisions, snippets)
   - Creates sandbox enforcing file scope (CLARITY-I)
   - Uses drone-worker preset for OpenRouter
   - Auto-applies any diffs proposed by the drone (unless :skip-auto-apply)
   - Records results to hivemind for review
   - Reports status to parent ling (if parent-id provided) for swarm state sync
   - Releases file locks on completion (even on error)

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

   Returns result map with :status, :result, :agent-id, :files-modified, :proposed-diff-ids

   Sandbox enforcement:
     - Drones can only read/write files in their :files list
     - Blocked: bash, memory writes, agent spawning
     - Blocked patterns: .env, credentials, secrets, keys

   Throws ex-info if file conflicts detected (files locked by another drone)."
  [{:keys [task files task-type preset trace parent-id cwd skip-auto-apply wave-id]
    :or {trace true
         skip-auto-apply false}}
   delegate-fn]
  (let [;; Infer task-type if not explicitly provided
        effective-task-type (or task-type (preset/get-task-type task files))
        ;; Select minimal tools for this task type
        minimal-tools (drone-tools/get-tools-for-drone effective-task-type files)
        ;; Auto-select preset based on task type if not explicitly provided
        effective-preset (or preset (preset/select-drone-preset task files))
        ;; SMART MODEL ROUTING: Select optimal model based on task + files
        ;; Uses routing module which considers: task classification, success rates, cooldowns
        model-selection (routing/route-and-select task files {:directory cwd})
        selected-model (:model model-selection)
        model-fallback (:fallback model-selection)
        effective-parent-id (or parent-id
                                (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        ;; BUG FIX: Use UUID instead of millis to prevent collision in parallel waves
        agent-id (str "drone-" (java.util.UUID/randomUUID))
        task-id (str "task-" agent-id)
        ;; FRICTION FIX: Calculate step budget based on task complexity
        ;; Simple tasks like "remove unused namespace" get fewer steps to avoid exhaustion
        step-budget (decompose/get-step-budget task files)
        ;; Log tool reduction, model selection, and step budget for observability
        _ (log/info "Drone configuration:"
                    {:drone-id agent-id
                     :task-type effective-task-type
                     :preset effective-preset
                     :model selected-model
                     :model-reason (:reason model-selection)
                     :model-fallback model-fallback
                     :tool-count (count minimal-tools)
                     :max-steps step-budget
                     :reduction (drone-tools/tool-reduction-summary effective-task-type)})]

    ;; 0. REGISTER DRONE IN DATASCRIPT
    ;; CRITICAL: Must register before claiming files because claim-file! uses
    ;; [:slave/id agent-id] lookup ref which requires the entity to exist.
    ;; Without this, coordinator/atomic-claim-files! fails with "entity not found".
    ;; BUG FIX: Capture transaction result and verify it completed before read.
    (let [tx-result (ds/add-slave! agent-id {:slave/status :spawning
                                             :slave/name "drone"
                                             :slave/depth 2
                                             :slave/parent effective-parent-id})]
      ;; Verify transaction succeeded - ds/add-slave! returns tx report with :tx-data
      (when-not (and tx-result (seq (:tx-data tx-result)))
        (log/error {:event :drone/registration-failed
                    :drone-id agent-id
                    :tx-result tx-result})
        (throw (ex-info "Failed to register drone in DataScript"
                        {:drone-id agent-id}))))

    ;; 1. ATOMIC ACQUIRE LOCKS (Race-Free)
    ;; CRITICAL: Uses atomic-claim-files! to prevent race condition where
    ;; two drones could both pass pre-flight check before either claims.
    ;; This combines check + claim into a single atomic operation.
    (when (seq files)
      (let [result (coordinator/atomic-claim-files! task-id agent-id files)]
        (when-not (:acquired? result)
          ;; CLARITY-T: Structured JSON logging for Loki
          (log/error {:event :drone/error
                      :error-type :conflict
                      :drone-id agent-id
                      :task-id task-id
                      :parent-id effective-parent-id
                      :files files
                      :conflicts (:conflicts result)
                      :message "File conflicts detected - files locked by another drone"})
          ;; Emit failure event for conflict (CLARITY-T)
          (ev/dispatch [:drone/failed {:drone-id agent-id
                                       :task-id task-id
                                       :parent-id effective-parent-id
                                       :error "File conflicts detected - files locked by another drone"
                                       :error-type :conflict
                                       :files files}])
          (throw (ex-info "File conflicts detected - files locked by another drone"
                          {:conflicts (:conflicts result)
                           :drone-id agent-id
                           :files files})))
        (log/info "Drone acquired file locks:" agent-id "(" (:files-claimed result) "files)")))

    ;; 1.5 PRE-EXECUTION VALIDATION (CLARITY-I: Inputs are guarded)
    ;; Validate files before mutation: exists, not binary, size limits
    (let [pre-validation (when (seq files)
                           (validation/validate-files-pre files task-id))
          _ (when (and (seq files) (not (validation/all-valid? pre-validation :pre)))
              (let [invalid-files (->> pre-validation
                                       (filter (comp not :pre-valid? val))
                                       (map key))]
                (log/warn {:event :drone/pre-validation-failed
                           :drone-id agent-id
                           :invalid-files invalid-files
                           :validation pre-validation})))
          ;; Track start time for duration metrics (CLARITY-T)
          start-time (System/currentTimeMillis)
          ;; Capture file contents before mutation for post-validation diff
          file-contents-before (when (seq files)
                                 (into {}
                                       (for [f files]
                                         [f (try (slurp f) (catch Exception _ nil))])))]

      ;; 2. EMIT STARTED EVENT (CLARITY-T: Telemetry first)
      (ev/dispatch [:drone/started {:drone-id agent-id
                                    :task-id task-id
                                    :parent-id effective-parent-id
                                    :files files
                                    :task task
                                    :pre-validation (validation/summarize-validation (or pre-validation {}))}])

      (try
        (let [augmented-task (augment-task task files {:project-root cwd})
              diffs-before (set (keys @diff/pending-diffs))
              ;; Create sandbox for file scope enforcement (CLARITY-I)
              ;; BUG FIX: Pass project-root for path containment validation
              effective-root (or cwd (diff/get-project-root))
              drone-sandbox (sandbox/create-sandbox (or files []) effective-root)]

          ;; SECURITY: Fail if any paths were rejected for escaping project directory
          (when (seq (:rejected-files drone-sandbox))
            (let [rejected (:rejected-files drone-sandbox)]
              (log/error {:event :drone/path-escape-blocked
                          :drone-id agent-id
                          :project-root effective-root
                          :rejected-files rejected})
              (throw (ex-info "File paths escape project directory - blocked for security"
                              {:error-type :path-escape
                               :drone-id agent-id
                               :project-root effective-root
                               :rejected-files rejected}))))

          ;; Log sandbox creation
          (log/info "Drone sandbox created"
                    {:drone-id agent-id
                     :allowed-files (count (:allowed-files drone-sandbox))
                     :blocked-tools (count (:blocked-tools drone-sandbox))})

          ;; Shout started to parent ling
          (when effective-parent-id
            (hivemind/shout! effective-parent-id :started
                             {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                              :message (format "Delegated drone %s working" agent-id)}))

          ;; 3. EXECUTE (with sandbox constraints + minimal tools + routed model + step budget)
          (let [result (delegate-fn {:backend :openrouter
                                     :preset effective-preset
                                     :model selected-model  ; Smart-routed model selection
                                     :task augmented-task
                                     :tools minimal-tools  ; Task-specific minimal tool set
                                     :max-steps step-budget  ; FRICTION FIX: Complexity-based step budget
                                     :trace trace
                                     ;; Sandbox config for runtime enforcement
                                     :sandbox {:allowed-files (:allowed-files drone-sandbox)
                                               :allowed-dirs (:allowed-dirs drone-sandbox)
                                               :blocked-patterns (map str (:blocked-patterns drone-sandbox))
                                               :blocked-tools (:blocked-tools drone-sandbox)}})
                diffs-after (set (keys @diff/pending-diffs))
                new-diff-ids (clojure.set/difference diffs-after diffs-before)
                ;; Handle diff application based on mode
                _ (when (and wave-id (seq new-diff-ids))
                    (tag-diffs-with-wave! new-diff-ids wave-id))
                diff-results (if skip-auto-apply
                               ;; In validated mode, don't apply - just track proposed diffs
                               {:applied []
                                :failed []
                                :proposed (vec new-diff-ids)}
                               ;; Normal mode - auto-apply diffs
                               (auto-apply-diffs agent-id new-diff-ids))
                duration-ms (- (System/currentTimeMillis) start-time)
                ;; 3.5 POST-EXECUTION VALIDATION (CLARITY-I)
                ;; Validate modified files: lint check, diff stats
                post-validation (when (and (seq files) (= :completed (:status result)) (not skip-auto-apply))
                                  (validation/validate-files-post
                                   file-contents-before
                                   (or pre-validation {})
                                   {:lint-level :error
                                    :require-modification false}))
                validation-summary (validation/summarize-validation
                                    (merge (or pre-validation {}) (or post-validation {})))]

            ;; Log validation results
            (when (and post-validation (not (validation/all-valid? post-validation :post)))
              (log/warn {:event :drone/post-validation-warnings
                         :drone-id agent-id
                         :summary validation-summary}))

            ;; Shout completion to parent ling
            (when effective-parent-id
              (if (= :completed (:status result))
                (hivemind/shout! effective-parent-id :completed
                                 {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                                  :message (format "Drone %s completed. Files: %s"
                                                   agent-id
                                                   (if skip-auto-apply
                                                     (str (count new-diff-ids) " diffs proposed for review")
                                                     (str/join ", " (or (:applied diff-results) []))))})
                (hivemind/shout! effective-parent-id :error
                                 {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                                  :message (format "Drone %s failed: %s" agent-id (:result result))})))

            ;; 4. EMIT COMPLETION/FAILURE EVENT (CLARITY-T)
            (if (= :completed (:status result))
              (ev/dispatch [:drone/completed {:drone-id agent-id
                                              :task-id task-id
                                              :parent-id effective-parent-id
                                              :files-modified (:applied diff-results)
                                              :files-failed (:failed diff-results)
                                              :proposed-diff-ids (:proposed diff-results)
                                              :duration-ms duration-ms
                                              :validation validation-summary}])
              (ev/dispatch [:drone/failed {:drone-id agent-id
                                           :task-id task-id
                                           :parent-id effective-parent-id
                                           :error (str (:result result))
                                           :error-type :execution
                                           :files files}]))

            ;; 4.5 RECORD MODEL METRICS (CLARITY-T: OpenRouter model performance tracking)
            ;; Captures model, task-type, duration, and token usage for Prometheus
            (let [model-name (or (:model result) selected-model)
                  tokens (:tokens result)
                  ;; 4.5.1 ESTIMATE TOKEN USAGE (CLARITY-T: Cost tracking)
                  input-tokens (or (:input-tokens tokens)
                                   (cost/count-tokens augmented-task))
                  output-tokens (or (:output-tokens tokens)
                                    (cost/count-tokens (str (:result result))))]
              (prom/record-drone-result! {:model model-name
                                          :task-type (name effective-task-type)
                                          :success? (= :completed (:status result))
                                          :duration-ms duration-ms
                                          :tokens tokens})
              ;; 4.5.2 TRACK COST (CLARITY-T: Budget management)
              (cost/track-drone-usage! agent-id
                                       {:input-tokens input-tokens
                                        :output-tokens output-tokens
                                        :task-preview task
                                        :wave-id (:wave-id result)})
              ;; 4.5.3 REPORT TO ROUTING (CLARITY-T: Smart model selection feedback)
              ;; Updates routing success rates for future model selection
              (routing/report-execution! effective-task-type model-name result
                                         {:duration-ms duration-ms
                                          :directory cwd
                                          :agent-id agent-id}))

            ;; Record result for coordinator review
            (hivemind/record-ling-result! agent-id
                                          {:task task
                                           :files files
                                           :result result
                                           :diff-results diff-results
                                           :validation validation-summary
                                           :parent-id effective-parent-id
                                           :timestamp (System/currentTimeMillis)})
            (assoc result
                   :agent-id agent-id
                   :task-id task-id
                   :parent-id effective-parent-id
                   :files-modified (:applied diff-results)
                   :files-failed (:failed diff-results)
                   :proposed-diff-ids (:proposed diff-results)
                   :duration-ms duration-ms
                   :validation validation-summary)))

        (catch Exception e
          ;; 5. EMIT FAILURE EVENT ON EXCEPTION (CLARITY-T)
          ;; Use structured error classification for proper Prometheus labeling
          ;; JSON-structured logging for Loki ingestion
          (let [duration-ms (- (System/currentTimeMillis) start-time)
                structured (errors/structure-error e)]
            ;; CLARITY-T: Structured JSON logging for Loki
            (log/error {:event :drone/error
                        :error-type (:error-type structured)
                        :drone-id agent-id
                        :task-id task-id
                        :parent-id effective-parent-id
                        :model selected-model
                        :files files
                        :duration-ms duration-ms
                        :message (:message structured)
                        :stacktrace (subs (or (:stacktrace structured) "") 0
                                          (min 500 (count (or (:stacktrace structured) ""))))})
            (ev/dispatch [:drone/failed {:drone-id agent-id
                                         :task-id task-id
                                         :parent-id effective-parent-id
                                         :error (:message structured)
                                         :error-type (:error-type structured)
                                         :stacktrace (:stacktrace structured)
                                         :files files}])
            ;; Record failure metrics (CLARITY-T: Model performance tracking)
            (prom/record-drone-result! {:model selected-model
                                        :task-type (name effective-task-type)
                                        :success? false
                                        :duration-ms duration-ms
                                        :retry? (= :timeout (:error-type structured))
                                        :retry-reason (:error-type structured)})
            ;; Report failure to routing for future model selection optimization
            (routing/report-execution! effective-task-type selected-model
                                       {:status :failed :error (:message structured)}
                                       {:duration-ms duration-ms
                                        :directory cwd
                                        :agent-id agent-id})
            (throw e)))

        (finally
          ;; 6. RELEASE LOCKS (always, even on error)
          (when (seq files)
            (coordinator/release-task-claims! task-id)
            (log/info "Drone released file locks:" agent-id))
          ;; 7. CLEANUP DRONE REGISTRATION
          ;; Remove ephemeral drone from DataScript after completion
          (ds/remove-slave! agent-id))))))

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
