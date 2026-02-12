(ns hive-mcp.tools.swarm.dispatch
  "Swarm dispatch handler - send prompts to slaves with pre-flight conflict checks and context injection."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.dns.validation :as v]
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [clojure.data.json :as json]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const shout-reminder-suffix
  "Mandatory suffix appended to ALL dispatch prompts for hivemind coordination."
  "\n\n---\nREMINDER: When task is complete, call hivemind_shout with event_type 'completed' and include your task summary in the message. This is MANDATORY for hivemind coordination.")

(defn inject-shout-reminder
  "Append shout reminder to prompt."
  [prompt]
  (str prompt shout-reminder-suffix))

(defn- extract-file-paths
  "Extract file paths from prompt text."
  [prompt]
  (when (seq prompt)
    (let [path-pattern #"(?:^|[\s`\"'\(\[])(/[^\s`\"'\)\]]+\.[a-z]+|[a-z][^\s`\"'\)\]]*\.[a-z]+)"
          matches (re-seq path-pattern prompt)]
      (->> matches
           (map second)
           (filter #(and % (re-find #"\.(clj|cljs|edn|md|json|yaml|yml|js|ts|py|rs|go)$" %)))
           distinct
           vec))))

(defn- inject-staleness-warnings
  "Inject staleness warnings for files mentioned in prompt."
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

(def ^:private recent-changes-window-ms
  "Time window for recent changes (30 minutes in milliseconds)."
  (* 30 60 1000))

(defn- format-time-ago
  "Format a timestamp as relative time."
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
  "Format lines added/removed as compact string."
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
  "Query recent file changes from claim history."
  [& {:keys [since-ms] :or {since-ms recent-changes-window-ms}}]
  (let [since (java.util.Date. (- (System/currentTimeMillis) since-ms))]
    (queries/get-recent-claim-history :since since :limit 10)))

(defn- build-recent-changes-section
  "Build the '## Recent File Changes' markdown section, or nil if no recent changes."
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
  "Inject recent file changes context into prompt."
  [prompt]
  (if-let [changes-section (build-recent-changes-section)]
    (str changes-section prompt)
    prompt))

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

(defn- execute-dispatch
  "Execute actual dispatch after pre-flight approval."
  [slave_id prompt timeout_ms effective-files]
  (let [warned-prompt (inject-staleness-warnings prompt effective-files)
        contextualized-prompt (inject-recent-changes warned-prompt)
        enhanced-prompt (inject-shout-reminder contextualized-prompt)
        elisp (format "(json-encode (hive-mcp-swarm-api-dispatch \"%s\" \"%s\" %s))"
                      (v/escape-elisp-string slave_id)
                      (v/escape-elisp-string enhanced-prompt)
                      (or timeout_ms "nil"))
        {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 5000)]
    (cond
      timed-out
      (core/mcp-timeout-error "Dispatch operation" :extra-data {:slave_id slave_id})

      success
      (let [parsed (try
                     (json/read-str result :key-fn keyword)
                     (catch Exception _ {:status "error" :error "json-parse-failed"}))
            status (:status parsed)]
        (case status
          "dispatched"
          (do
            (when-let [task-id (:task-id parsed)]
              (when (seq effective-files)
                (coord/register-task-claims! task-id slave_id effective-files)))
            (core/mcp-success parsed))

          "queued"
          (core/mcp-success (assoc parsed
                                   :message "Dispatch queued - ling terminal not ready. Will retry automatically."
                                   :retry_interval_ms 500
                                   :max_retries 20))

          "error"
          (core/mcp-error-json {:error (or (:error parsed) "dispatch-failed")
                                :slave_id slave_id
                                :details parsed})

          (core/mcp-success parsed)))

      :else
      (core/mcp-error (str "Error: " error)))))

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
        (execute-dispatch slave_id resolved-prompt timeout_ms (:files preflight))

        (core/mcp-error-json {:error "Unknown pre-flight result"
                              :preflight preflight})))))
