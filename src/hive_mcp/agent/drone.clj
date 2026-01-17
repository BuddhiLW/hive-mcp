(ns hive-mcp.agent.drone
  "Drone delegation - token-optimized leaf agents.
   
   Drones are lightweight agents that:
   - Use OpenRouter free-tier models
   - Pre-inject file contents (no read tool calls needed)
   - Use propose_diff instead of direct file writes
   - Auto-apply proposed diffs on completion
   - Report status to parent lings for swarm sync"
  (:require [hive-mcp.agent.registry :as registry]
            [hive-mcp.tools.diff :as diff]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.events.core :as ev]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.set]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Error Classification (CLARITY-T: Structured nREPL error telemetry)
;;; ============================================================

(defn- get-root-cause
  "Extract root cause from nested exceptions.
   Checks both Java .getCause() and ex-data :cause key."
  [ex]
  (loop [e ex]
    (let [;; Check Java cause first
          java-cause (when (instance? Throwable e) (.getCause e))
          ;; Fall back to ex-data :cause
          ex-data-cause (when (instance? clojure.lang.IExceptionInfo e)
                          (:cause (ex-data e)))
          cause (or java-cause ex-data-cause)]
      (if cause
        (recur cause)
        e))))

(defn- exception-type-name
  "Get the simple class name of an exception."
  [ex]
  (when ex
    (.getSimpleName (class ex))))

(defn classify-nrepl-error
  "Classify an exception into structured error categories.

   Returns one of:
   - :nrepl-connection - Connection failures (refused, not connected)
   - :nrepl-timeout    - Timeouts (socket timeout, evaluation timeout)
   - :nrepl-eval-error - Evaluation errors (syntax, runtime, compiler)
   - :validation       - Input validation errors
   - :conflict         - File conflict errors
   - :execution        - General execution errors
   - :exception        - Unknown/fallback category

   Used by Prometheus drones_failed_total metric with error-type label."
  [ex]
  (let [ex-data-map (ex-data ex)
        message (or (ex-message ex) "")
        message-lower (str/lower-case message)
        root-cause (get-root-cause ex)
        root-type (exception-type-name root-cause)]
    (cond
      ;; Check ex-data for explicit type
      (= :validation (:type ex-data-map))
      :validation

      (= :conflict (:type ex-data-map))
      :conflict

      ;; Connection errors
      (or (= "ConnectException" root-type)
          (str/includes? message-lower "connection refused")
          (str/includes? message-lower "cider not connected")
          (str/includes? message-lower "no nrepl connection")
          (str/includes? message-lower "failed to connect"))
      :nrepl-connection

      ;; Timeout errors
      (or (= "SocketTimeoutException" root-type)
          (= "TimeoutException" root-type)
          (str/includes? message-lower "timed out")
          (str/includes? message-lower "timeout"))
      :nrepl-timeout

      ;; Evaluation/compilation errors
      (or (str/includes? message-lower "compilerexception")
          (str/includes? message-lower "syntax error")
          (str/includes? message-lower "classnotfoundexception")
          (str/includes? message-lower "nullpointerexception")
          (str/includes? message-lower "arithmeticexception")
          (str/includes? message-lower "unable to resolve symbol"))
      :nrepl-eval-error

      ;; Fallback
      :else
      :exception)))

(defn structure-error
  "Wrap an exception with structured error data for telemetry.

   Returns a map with:
   - :error-type  - Classified error category (keyword)
   - :message     - Human-readable error message
   - :stacktrace  - Stack trace as string (truncated to 2000 chars)
   - :ex-data     - Original ex-data from the exception (if any)

   Used for Prometheus metrics and debugging."
  [ex]
  (let [error-type (classify-nrepl-error ex)
        message (or (ex-message ex) (str ex))
        stacktrace (when ex
                     (let [sw (java.io.StringWriter.)
                           pw (java.io.PrintWriter. sw)]
                       (.printStackTrace ex pw)
                       (let [trace (str sw)]
                         ;; Truncate to avoid metric cardinality explosion
                         (subs trace 0 (min 2000 (count trace))))))
        ex-data-map (ex-data ex)]
    {:error-type error-type
     :message message
     :stacktrace stacktrace
     :ex-data ex-data-map}))

;;; ============================================================
;;; Configuration
;;; ============================================================

(def allowed-tools
  "Safe tools for drone agents. Drones cannot write files directly - they must use propose_diff."
  ["read_file" "grep" "glob_files" "clojure_eval" "clojure_inspect_project"
   "magit_status" "magit_diff" "magit_log" "magit_branches"
   "propose_diff" "hivemind_shout"])

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

   Arguments:
     files        - List of file paths to read
     project-root - Optional project root override (defaults to diff/get-project-root)"
  [files & [project-root-override]]
  (when (seq files)
    (let [project-root (or project-root-override (diff/get-project-root) "")
          contents (for [f files]
                     (let [abs-path (if (str/starts-with? f "/")
                                      f
                                      (str project-root "/" f))]
                       (try
                         (let [content (slurp abs-path)]
                           (str "### " f "\n```\n" content "```\n"))
                         (catch Exception e
                           (str "### " f "\n(File not found or unreadable: " (.getMessage e) ")\n")))))]
      (str "## Current File Contents\n"
           "IMPORTANT: Use this EXACT content as old_content in propose_diff.\n"
           "Do NOT guess or assume file content - use what is provided below.\n\n"
           (str/join "\n" contents)))))

(defn- augment-task
  "Augment task with context and file contents.

   Arguments:
     task         - Task description
     files        - List of files to include
     project-root - Optional project root override for path resolution"
  [task files & [project-root]]
  (let [context (prepare-context)
        context-str (format-context-str context)
        file-contents-str (format-file-contents files project-root)]
    (str context-str
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

;;; ============================================================
;;; Public API
;;; ============================================================

(defn delegate!
  "Delegate a task to a drone (token-optimized leaf agent).

   Automatically:
   - Acquires file locks via coordinator (conflict prevention)
   - Pre-injects file contents (drone doesn't need to read)
   - Injects catchup context (conventions, decisions, snippets)
   - Uses drone-worker preset for OpenRouter
   - Auto-applies any diffs proposed by the drone
   - Records results to hivemind for review
   - Reports status to parent ling (if parent-id provided) for swarm state sync
   - Releases file locks on completion (even on error)

   Options:
     :task      - Task description (required)
     :files     - List of files the drone will modify (contents pre-injected)
     :preset    - Override preset (default: drone-worker)
     :trace     - Enable progress events (default: true)
     :parent-id - Parent ling's slave-id (for swarm status sync)
     :cwd       - Working directory override for path resolution

   Returns result map with :status, :result, :agent-id, :files-modified

   Throws ex-info if file conflicts detected (files locked by another drone)."
  [{:keys [task files preset trace parent-id cwd]
    :or {preset "drone-worker"
         trace true}}
   delegate-fn]
  (let [effective-parent-id (or parent-id
                                (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
        agent-id (str "drone-" (System/currentTimeMillis))
        task-id (str "task-" agent-id)]

    ;; 1. ATOMIC ACQUIRE LOCKS (Race-Free)
    ;; CRITICAL: Uses atomic-claim-files! to prevent race condition where
    ;; two drones could both pass pre-flight check before either claims.
    ;; This combines check + claim into a single atomic operation.
    (when (seq files)
      (let [result (coordinator/atomic-claim-files! task-id agent-id files)]
        (when-not (:acquired? result)
          (log/warn "Drone file conflicts detected:" (:conflicts result))
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

    ;; Track start time for duration metrics (CLARITY-T)
    (let [start-time (System/currentTimeMillis)]
      ;; 2. EMIT STARTED EVENT (CLARITY-T: Telemetry first)
      (ev/dispatch [:drone/started {:drone-id agent-id
                                    :task-id task-id
                                    :parent-id effective-parent-id
                                    :files files
                                    :task task}])

      (try
        (let [augmented-task (augment-task task files cwd)
              diffs-before (set (keys @diff/pending-diffs))]

          ;; Shout started to parent ling
          (when effective-parent-id
            (hivemind/shout! effective-parent-id :started
                             {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                              :message (format "Delegated drone %s working" agent-id)}))

          ;; 3. EXECUTE
          (let [result (delegate-fn {:backend :openrouter
                                     :preset preset
                                     :task augmented-task
                                     :tools allowed-tools
                                     :trace trace})
                diffs-after (set (keys @diff/pending-diffs))
                new-diff-ids (clojure.set/difference diffs-after diffs-before)
                diff-results (auto-apply-diffs agent-id new-diff-ids)
                duration-ms (- (System/currentTimeMillis) start-time)]

            ;; Shout completion to parent ling
            (when effective-parent-id
              (if (= :completed (:status result))
                (hivemind/shout! effective-parent-id :completed
                                 {:task (str "Drone: " (subs task 0 (min 80 (count task))))
                                  :message (format "Drone %s completed. Files: %s"
                                                   agent-id
                                                   (str/join ", " (or (:applied diff-results) [])))})
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
                                              :duration-ms duration-ms}])
              (ev/dispatch [:drone/failed {:drone-id agent-id
                                           :task-id task-id
                                           :parent-id effective-parent-id
                                           :error (str (:result result))
                                           :error-type :execution
                                           :files files}]))

            ;; Record result for coordinator review
            (hivemind/record-ling-result! agent-id
                                          {:task task
                                           :files files
                                           :result result
                                           :diff-results diff-results
                                           :parent-id effective-parent-id
                                           :timestamp (System/currentTimeMillis)})
            (assoc result
                   :agent-id agent-id
                   :task-id task-id
                   :parent-id effective-parent-id
                   :files-modified (:applied diff-results)
                   :files-failed (:failed diff-results)
                   :duration-ms duration-ms)))

        (catch Exception e
          ;; 5. EMIT FAILURE EVENT ON EXCEPTION (CLARITY-T)
          ;; Use structured error classification for proper Prometheus labeling
          (let [duration-ms (- (System/currentTimeMillis) start-time)
                structured (structure-error e)]
            (log/warn "Drone execution failed"
                      {:drone-id agent-id
                       :error-type (:error-type structured)
                       :message (:message structured)})
            (ev/dispatch [:drone/failed {:drone-id agent-id
                                         :task-id task-id
                                         :parent-id effective-parent-id
                                         :error (:message structured)
                                         :error-type (:error-type structured)
                                         :stacktrace (:stacktrace structured)
                                         :files files}])
            (throw e)))

        (finally
          ;; 6. RELEASE LOCKS (always, even on error)
          (when (seq files)
            (coordinator/release-task-claims! task-id)
            (log/info "Drone released file locks:" agent-id)))))))
