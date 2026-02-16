(ns hive-mcp.tools.swarm.dispatch
  "Swarm dispatch handler — send prompts to slaves via terminal-registry strategies.

   Architecture (Stratified Design):
   ┌─────────────────────────────────────────────────────┐
   │ Layer 3: MCP Handler (handle-swarm-dispatch)        │ ← entry point
   │   - context resolution, pre-flight conflict checks  │
   ├─────────────────────────────────────────────────────┤
   │ Layer 2: Effectful Boundary (execute-dispatch!)     │ ← effects
   │   - slave lookup, strategy resolution, dispatch I/O │
   ├─────────────────────────────────────────────────────┤
   │ Layer 1: Pure Computation (enhance-prompt, etc.)    │ ← testable
   │   - prompt pipeline, spawn-mode resolution, format  │
   └─────────────────────────────────────────────────────┘

   FP Principles applied:
   - Effect Boundary: pure prompt computation separated from effectful dispatch
   - Pipeline Decomposition: enhance-prompt as composable pure pipeline
   - Stratified Design: pure bottom layer, effectful top layer"
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [hive-mcp.agent.ling.terminal-registry :as terminal-reg]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-dsl.result :as result]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Layer 1: Pure Computation (no side effects, fully testable)
;; =============================================================================

(def ^:const shout-reminder-suffix
  "Mandatory suffix appended to ALL dispatch prompts for hivemind coordination."
  "\n\n---\nREMINDER: When task is complete, call hivemind_shout with event_type 'completed' and include your task summary in the message. This is MANDATORY for hivemind coordination.")

(defn inject-shout-reminder
  "Append shout reminder to prompt. Pure: string -> string."
  [prompt]
  (str prompt shout-reminder-suffix))

(defn- extract-file-paths
  "Extract file paths from prompt text. Pure: string -> [string]."
  [prompt]
  (when (seq prompt)
    (let [path-pattern #"(?:^|[\s`\"'\(\[])(/[^\s`\"'\)\]]+\.[a-z]+|[a-z][^\s`\"'\)\]]*\.[a-z]+)"
          matches (re-seq path-pattern prompt)]
      (->> matches
           (map second)
           (filter #(and % (re-find #"\.(clj|cljs|edn|md|json|yaml|yml|js|ts|py|rs|go)$" %)))
           distinct
           vec))))

(defn resolve-spawn-mode
  "Pure: determine spawn mode from slave data map (or nil).
   Returns keyword — defaults to :claude when data is absent."
  [slave-data]
  (or (:ling/spawn-mode slave-data) :claude))

(defn- format-time-ago
  "Format a timestamp as relative time. Pure: Date -> string."
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
  "Format lines added/removed as compact string. Pure."
  [{:keys [lines-added lines-removed]}]
  (let [added (or lines-added 0)
        removed (or lines-removed 0)]
    (if (and (zero? added) (zero? removed))
      "(no line changes)"
      (str "+" added "/-" removed))))

(defn- format-recent-change
  "Format a single recent change entry as a table row. Pure."
  [entry]
  (let [file-name (last (str/split (or (:file entry) "") #"/"))
        delta (format-lines-delta entry)
        agent (or (:slave-id entry) "unknown")
        time-ago (format-time-ago (:released-at entry))]
    (str "| " file-name " | " delta " | " agent " | " time-ago " |")))

(defn build-changes-section
  "Build markdown table from change entries. Pure: [entry] -> string|nil."
  [changes]
  (when (seq changes)
    (str "## Recent File Changes\n"
         "Other agents recently modified these files:\n\n"
         "| File | Lines | Agent | Time |\n"
         "|------|-------|-------|------|\n"
         (str/join "\n" (map format-recent-change changes))
         "\n\n")))

(defn inject-changes-section
  "Prepend changes section to prompt. Pure: string * string|nil -> string."
  [prompt changes-section]
  (if changes-section
    (str changes-section prompt)
    prompt))

(defn inject-staleness-context
  "Inject staleness warnings given disc context. Pure: string * map -> string."
  [prompt {:keys [stale]}]
  (if (seq stale)
    (let [warnings (kg-disc/staleness-warnings stale)
          warning-text (when (seq warnings)
                         (kg-disc/format-staleness-warnings warnings))]
      (if warning-text
        (str warning-text "\n" prompt)
        prompt))
    prompt))

;; =============================================================================
;; Layer 2: Effectful Boundary (I/O, DB lookups, strategy dispatch)
;; =============================================================================

(def ^:private recent-changes-window-ms
  "Time window for recent changes (30 minutes in milliseconds)."
  (* 30 60 1000))

(defn- get-recent-file-changes
  "Effect: query recent file changes from claim history."
  [& {:keys [since-ms] :or {since-ms recent-changes-window-ms}}]
  (let [since (java.util.Date. (- (System/currentTimeMillis) since-ms))]
    (queries/get-recent-claim-history :since since :limit 10)))

(defn- get-disc-context
  "Effect: query disc staleness context for files."
  [prompt files]
  (let [effective-files (or (seq files) (extract-file-paths prompt))]
    (if (empty? effective-files)
      {}
      (kg-disc/kg-first-context effective-files))))

(defn enhance-prompt
  "Prompt enhancement pipeline: staleness → recent changes → shout reminder.
   Pure data transformations composed with effectful context lookups.

   The pipeline stages:
   1. inject-staleness-context  — warn about stale KG disc entries
   2. inject-changes-section    — show recent file modifications by other agents
   3. inject-shout-reminder     — append mandatory hivemind coordination suffix"
  [prompt files]
  (let [disc-ctx (get-disc-context prompt files)
        changes (get-recent-file-changes)]
    (-> prompt
        (inject-staleness-context disc-ctx)
        (inject-changes-section (build-changes-section changes))
        inject-shout-reminder)))

(defn- resolve-strategy
  "Effect: look up slave in DataScript, resolve terminal strategy.
   Returns Result — (ok {:strategy strat :mode mode}) or (err :dispatch/no-strategy ...)."
  [slave_id]
  (let [slave-data (queries/get-slave slave_id)
        spawn-mode (resolve-spawn-mode slave-data)
        strat (terminal-reg/resolve-terminal-strategy spawn-mode)]
    (if strat
      (result/ok {:strategy strat :mode spawn-mode})
      (result/err :dispatch/no-strategy {:mode spawn-mode :slave_id slave_id}))))

(defn- execute-dispatch!
  "Effectful boundary: enhance prompt, resolve strategy, dispatch via terminal addon.

   Returns MCP response (success or error). All effects are contained here —
   callers above only route pre-flight results."
  [slave_id prompt timeout_ms effective-files]
  (let [enhanced-prompt (enhance-prompt prompt effective-files)
        resolution (resolve-strategy slave_id)]
    (if (result/err? resolution)
      (core/mcp-error (str "No terminal strategy for mode: " (:mode resolution)))
      (let [{:keys [strategy]} (:ok resolution)
            dispatch-result (result/try-effect* :dispatch/strategy-failed
                                                (strategy/strategy-dispatch! strategy
                                                                             {:id slave_id}
                                                                             {:task enhanced-prompt
                                                                              :timeout-ms timeout_ms})
                                                (when (seq effective-files)
                                                  (coord/register-task-claims!
                                                   (str "task-" (System/currentTimeMillis))
                                                   slave_id effective-files)))]
        (if (result/ok? dispatch-result)
          (core/mcp-success {:status "dispatched" :slave_id slave_id})
          (core/mcp-error (str "Dispatch failed: " (:message dispatch-result))))))))

;; =============================================================================
;; Layer 3: MCP Handler (pre-flight conflict checks + routing)
;; =============================================================================

(defn- handle-blocked-dispatch
  "Handle blocked dispatch due to circular dependency."
  [preflight slave_id]
  (core/mcp-error-json
   {:error "Dispatch blocked: circular dependency detected"
    :status "blocked"
    :would_deadlock (:would-deadlock preflight)
    :slave_id slave_id}))

(defn- handle-queued-dispatch
  "Handle queued dispatch due to file conflicts."
  [preflight slave_id]
  (core/mcp-success
   {:status "queued"
    :task_id (:task-id preflight)
    :queue_position (:position preflight)
    :conflicts (:conflicts preflight)
    :slave_id slave_id
    :message "Task queued - waiting for file conflicts to clear"}))

(defn handle-swarm-dispatch
  "Dispatch a prompt to a slave with pre-flight conflict checks."
  [{:keys [slave_id prompt timeout_ms files]}]
  (core/with-swarm
    (let [ctx (dispatch-ctx/ensure-context prompt)
          resolved-prompt (:prompt (dispatch-ctx/resolve-context ctx))
          preflight (coord/dispatch-or-queue!
                     {:slave-id slave_id
                      :prompt resolved-prompt
                      :files files
                      :timeout-ms timeout_ms})]
      (case (:action preflight)
        :blocked
        (handle-blocked-dispatch preflight slave_id)

        :queued
        (handle-queued-dispatch preflight slave_id)

        :dispatch
        (execute-dispatch! slave_id resolved-prompt timeout_ms (:files preflight))

        (core/mcp-error-json {:error "Unknown pre-flight result"
                              :preflight preflight})))))
