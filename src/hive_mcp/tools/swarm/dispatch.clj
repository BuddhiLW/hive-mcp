(ns hive-mcp.tools.swarm.dispatch
  "Swarm dispatch handler - send prompts to slaves.

   Runs pre-flight conflict checks via coordinator before dispatch.
   Integrates with hive-mcp.swarm.coordinator for task queueing.

   Context Injection Layers:
   - Layer 3.5: Staleness warnings (KG-first context)
   - Layer 3.6: Recent file changes (CC.7) - shows what other agents modified
   - Layer 3: Shout reminder (mandatory completion notification)

   SOLID: SRP - Single responsibility for dispatch operations.
   CLARITY: I - Inputs validated, conflicts checked before dispatch.
   CLARITY: A - Architectural context via recent changes injection."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [clojure.data.json :as json]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Layer 3: Shout Reminder Injection
;; ============================================================

(def ^:const shout-reminder-suffix
  "Mandatory suffix appended to ALL dispatch prompts.
   Ensures lings always know to report completion status.

   This is Layer 3 of the 4-Layer Convergence Pattern:
   - Layer 1: Preset footer (system prompt)
   - Layer 2: Terminal introspection (detect idle)
   - Layer 3: Dispatch wrapper (THIS - inject reminder)
   - Layer 4: Sync hooks (auto-check on MCP calls)"
  "\n\n---\nREMINDER: When task is complete, call hivemind_shout with event_type 'completed' and include your task summary in the message. This is MANDATORY for hivemind coordination.")

(defn inject-shout-reminder
  "Append shout reminder to prompt.
   Preserves original prompt content.

   CLARITY: R - Represented intent via explicit suffix."
  [prompt]
  (str prompt shout-reminder-suffix))

;; ============================================================
;; Layer 3.5: Staleness Warning Injection (KG-First)
;; ============================================================

(defn- extract-file-paths
  "Extract file paths from prompt text.
   Matches patterns like:
   - /absolute/path/to/file.clj
   - src/relative/path.clj
   - Files in `backticks`

   CLARITY: R - Heuristic extraction, not exhaustive."
  [prompt]
  (when (seq prompt)
    (let [;; Match file paths: starts with / or word, ends with extension
          path-pattern #"(?:^|[\s`\"'\(\[])(/[^\s`\"'\)\]]+\.[a-z]+|[a-z][^\s`\"'\)\]]*\.[a-z]+)"
          matches (re-seq path-pattern prompt)]
      (->> matches
           (map second)
           (filter #(and % (re-find #"\.(clj|cljs|edn|md|json|yaml|yml|js|ts|py|rs|go)$" %)))
           distinct
           vec))))

(defn- inject-staleness-warnings
  "Inject staleness warnings for files mentioned in prompt.
   Consults KG for file freshness and prepends warnings if stale.

   Arguments:
     prompt - The dispatch prompt
     files  - Optional explicit file list (if nil, extracts from prompt)

   Returns:
     Prompt with staleness warnings prepended (if any).

   CLARITY: A - KG-first lookup minimizes redundant reads.
   CLARITY: I - Guards against stale knowledge."
  [prompt files]
  (let [effective-files (or (seq files) (extract-file-paths prompt))]
    (if (empty? effective-files)
      prompt
      (let [{:keys [stale]} (kg-disc/kg-first-context effective-files)
            warnings (when (seq stale)
                       (kg-disc/staleness-warnings stale))
            warning-text (when (seq warnings)
                           (kg-disc/format-staleness-warnings warnings))]
        (if warning-text
          (str warning-text "\n" prompt)
          prompt)))))

;; ============================================================
;; Layer 3.6: Recent File Changes Injection (CC.7)
;; ============================================================

(def ^:private recent-changes-window-ms
  "Time window for recent changes (30 minutes in milliseconds)."
  (* 30 60 1000))

(defn- format-time-ago
  "Format a timestamp as relative time (e.g., '5m ago', '2h ago')."
  [^java.util.Date timestamp]
  (when timestamp
    (let [now-ms (System/currentTimeMillis)
          then-ms (.getTime timestamp)
          diff-ms (- now-ms then-ms)
          minutes (quot diff-ms 60000)
          hours (quot minutes 60)]
      (cond
        (< minutes 1) "just now"
        (< minutes 60) (str minutes "m ago")
        (< hours 24) (str hours "h ago")
        :else (str (quot hours 24) "d ago")))))

(defn- format-lines-delta
  "Format lines added/removed as compact string (e.g., '+12/-3')."
  [{:keys [lines-added lines-removed]}]
  (let [added (or lines-added 0)
        removed (or lines-removed 0)]
    (if (and (zero? added) (zero? removed))
      "(no line changes)"
      (str "+" added "/-" removed))))

(defn- format-recent-change
  "Format a single recent change entry as a table row."
  [entry]
  (let [file-name (last (str/split (or (:file entry) "") #"/"))
        delta (format-lines-delta entry)
        agent (or (:slave-id entry) "unknown")
        time-ago (format-time-ago (:released-at entry))]
    (str "| " file-name " | " delta " | " agent " | " time-ago " |")))

(defn- get-recent-file-changes
  "Query recent file changes from claim history.

   CC.7: Provides context about what files were recently modified
   by other agents, helping new dispatches avoid conflicts.

   Arguments:
     since-ms - Time window in milliseconds (default: 30 minutes)

   Returns:
     Vector of {:file :lines-added :lines-removed :slave-id :released-at}"
  [& {:keys [since-ms] :or {since-ms recent-changes-window-ms}}]
  (let [since (java.util.Date. (- (System/currentTimeMillis) since-ms))]
    (queries/get-recent-claim-history :since since :limit 10)))

(defn- build-recent-changes-section
  "Build the '## Recent File Changes' markdown section.

   CC.7: Enriches drone prompts with context about what files
   were recently modified by other agents.

   Format: file, lines +/-, agent, time

   Returns:
     Formatted markdown string or nil if no recent changes."
  []
  (let [changes (get-recent-file-changes)]
    (when (seq changes)
      (str "## Recent File Changes\n"
           "Other agents recently modified these files:\n\n"
           "| File | Lines | Agent | Time |\n"
           "|------|-------|-------|------|\n"
           (str/join "\n" (map format-recent-change changes))
           "\n\n"))))

(defn- inject-recent-changes
  "Inject recent file changes context into prompt.

   Prepends a summary of recently modified files if any exist.
   This helps the dispatched agent understand what has changed.

   CLARITY: A - Architectural awareness via context propagation."
  [prompt]
  (if-let [changes-section (build-recent-changes-section)]
    (str changes-section prompt)
    prompt))

;; ============================================================
;; Pre-flight Check Results
;; ============================================================

(defn- handle-blocked-dispatch
  "Handle blocked dispatch due to circular dependency.

   CLARITY: R - Clear error response for blocked state"
  [preflight slave_id]
  (core/mcp-error-json
   {:error "Dispatch blocked: circular dependency detected"
    :status "blocked"
    :would_deadlock (:would-deadlock preflight)
    :slave_id slave_id}))

(defn- handle-queued-dispatch
  "Handle queued dispatch due to file conflicts.

   CLARITY: R - Clear response for queued state"
  [preflight slave_id]
  (core/mcp-success
   {:status "queued"
    :task_id (:task-id preflight)
    :queue_position (:position preflight)
    :conflicts (:conflicts preflight)
    :slave_id slave_id
    :message "Task queued - waiting for file conflicts to clear"}))

(defn- execute-dispatch
  "Execute actual dispatch after pre-flight approval.

   Returns MCP response with task_id on success.
   Injects context layers before dispatch:
   - Layer 3.5: Staleness warnings (KG-first context)
   - Layer 3.6: Recent file changes (CC.7)
   - Layer 3: Shout reminder

   CLARITY: Y - Yield safe failure with timeout handling"
  [slave_id prompt timeout_ms effective-files]
  (let [;; Layer 3.5: Inject staleness warnings (KG-first context)
        warned-prompt (inject-staleness-warnings prompt effective-files)
        ;; Layer 3.6: Inject recent file changes (CC.7)
        contextualized-prompt (inject-recent-changes warned-prompt)
        ;; Layer 3: Inject shout reminder into prompt
        enhanced-prompt (inject-shout-reminder contextualized-prompt)
        elisp (format "(json-encode (hive-mcp-swarm-api-dispatch \"%s\" \"%s\" %s))"
                      (v/escape-elisp-string slave_id)
                      (v/escape-elisp-string enhanced-prompt)
                      (or timeout_ms "nil"))
        ;; Dispatch should be quick - 5s default timeout
        {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 5000)]
    (cond
      timed-out
      (core/mcp-timeout-error "Dispatch operation" :extra-data {:slave_id slave_id})

      success
      (do
        ;; Register file claims for successful dispatch
        (when-let [task-id (try
                             (:task-id (json/read-str result :key-fn keyword))
                             (catch Exception _ nil))]
          (when (seq effective-files)
            (coord/register-task-claims! task-id slave_id effective-files)))
        (core/mcp-success result))

      :else
      (core/mcp-error (str "Error: " error)))))

;; ============================================================
;; Dispatch Handler
;; ============================================================

(defn handle-swarm-dispatch
  "Dispatch a prompt to a slave.
   Runs pre-flight conflict checks before dispatch.
   Uses timeout to prevent MCP blocking.

   Parameters:
   - slave_id: Target slave for dispatch (required)
   - prompt: The prompt/task to send (required)
   - timeout_ms: Optional timeout in milliseconds
   - files: Optional explicit list of files task will modify

   CLARITY: I - Inputs validated via coordinator pre-flight
   SOLID: OCP - Open for extension via coordinator actions"
  [{:keys [slave_id prompt timeout_ms files]}]
  (core/with-swarm
    ;; Pre-flight check: detect conflicts before dispatch
    (let [preflight (coord/dispatch-or-queue!
                     {:slave-id slave_id
                      :prompt prompt
                      :files files
                      :timeout-ms timeout_ms})]
      (case (:action preflight)
        ;; Blocked due to circular dependency - cannot proceed
        :blocked
        (handle-blocked-dispatch preflight slave_id)

        ;; Queued due to file conflicts - will dispatch when conflicts clear
        :queued
        (handle-queued-dispatch preflight slave_id)

        ;; Approved - proceed with dispatch
        :dispatch
        (execute-dispatch slave_id prompt timeout_ms (:files preflight))

        ;; Fallback for unknown action
        (core/mcp-error-json {:error "Unknown pre-flight result"
                              :preflight preflight})))))
