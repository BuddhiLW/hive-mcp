(ns hive-mcp.crystal.core
  "Progressive crystallization of ephemeral knowledge."
  (:require [clojure.string :as str]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.extensions.registry :as ext]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Forward declarations used in should-promote?
(declare scope-boost)

;; =============================================================================
;; Recall Context Weights
;; =============================================================================

(def recall-weights
  "Weights for different recall contexts.
   Higher = more meaningful signal for promotion."
  {:catchup-structural 0.1 ; Always loaded at catchup - noise
   :wrap-structural 0.1 ; Always checked at wrap - noise
   :explicit-reference 1.0 ; LLM explicitly cited in reasoning
   :cross-session 2.0 ; Referenced from different session
   :cross-project 3.0 ; Referenced from different project
   :user-feedback 5.0 ; Human marked as helpful
   ;; Behavioral signals (outcome tracking)
   :behavioral-success 2.0 ; Task completed successfully after recall
   :behavioral-failure 0.0 ; Task failed after recall (no penalty, just no boost)
   :behavioral-correction -2.0}) ; User had to correct agent work (demote signal)

(def promotion-thresholds
  "Score thresholds for promotion between durations."
  {:ephemeral->short 5.0
   :short->medium 10.0
   :medium->long 15.0
   :long->permanent 25.0})

;; =============================================================================
;; Score Calculation
;; =============================================================================

(defn calculate-promotion-score
  "Calculate promotion score from recall history."
  [recalls]
  (let [breakdown (for [{:keys [context count] :or {count 1}} recalls
                        :let [weight (get recall-weights context 1.0)
                              contribution (* weight count)]]
                    {:context context
                     :weight weight
                     :count count
                     :contribution contribution})
        total-score (reduce + 0.0 (map :contribution breakdown))]
    {:score total-score
     :breakdown (vec breakdown)}))

(defn current-duration->next
  "Map current duration to next tier."
  [duration]
  (case (keyword duration)
    :ephemeral :short
    :short :medium
    :medium :long
    :long :permanent
    :permanent :permanent
    ;; Handle string versions
    (case (str duration)
      "ephemeral" :short
      "short-term" :medium
      "short" :medium
      "medium" :long
      "long-term" :permanent
      "long" :permanent
      "permanent" :permanent
      :medium))) ; default

(defn threshold-for-duration
  "Get promotion threshold for current duration."
  [duration]
  (case (keyword duration)
    :ephemeral (:ephemeral->short promotion-thresholds)
    :short (:short->medium promotion-thresholds)
    :short-term (:short->medium promotion-thresholds)
    :medium (:medium->long promotion-thresholds)
    :long (:long->permanent promotion-thresholds)
    :long-term (:long->permanent promotion-thresholds)
    :permanent Double/MAX_VALUE
    10.0)) ; default

(defn should-promote?
  "Determine if a memory entry should be promoted."
  ([entry] (should-promote? entry {}))
  ([{:keys [duration recalls] :as entry} {:keys [behavioral-adjustment scope-boost-override]
                                          :or {behavioral-adjustment 0.0
                                               scope-boost-override 0.0}}]
   (let [{:keys [score]} (calculate-promotion-score recalls)
         ;; Auto-compute scope boost from tags if not explicitly provided
         xpoll-boost (if (zero? scope-boost-override)
                       (scope-boost entry)
                       scope-boost-override)
         ;; Apply behavioral adjustment + scope boost
         adjusted-score (+ score behavioral-adjustment xpoll-boost)
         threshold (threshold-for-duration duration)
         should? (>= adjusted-score threshold)]
     {:promote? should?
      :current-score adjusted-score
      :base-score score
      :behavioral-adjustment behavioral-adjustment
      :scope-boost-override xpoll-boost
      :threshold threshold
      :next-duration (when should? (current-duration->next duration))})))

(defn should-demote?
  "Determine if a memory entry should be demoted."
  [{:keys [duration] :as _entry} behavioral-adjustment]
  (let [demote? (< behavioral-adjustment -3.0)
        prev-duration (case (keyword duration)
                        :permanent :long
                        :long :medium
                        :medium :short
                        :short :ephemeral
                        :ephemeral :ephemeral
                        :short)]
    {:demote? demote?
     :reason (when demote? :behavioral-corrections)
     :behavioral-adjustment behavioral-adjustment
     :prev-duration (when demote? prev-duration)}))

;; =============================================================================
;; Session Tagging
;; =============================================================================

(defn session-id
  "Generate a session identifier for today."
  []
  (let [now (java.time.LocalDateTime/now)
        fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd")]
    (.format now fmt)))

(defn session-tag
  "Create a session scope tag."
  ([]
   (str "session:" (session-id)))
  ([date-str]
   (str "session:" date-str)))

(defn extract-session-from-tags
  "Extract session identifier from tags."
  [tags]
  (some #(when (str/starts-with? % "session:")
           (subs % 8))
        tags))

;; =============================================================================
;; Session Timestamp Tracking (per-agent)
;; =============================================================================

(defonce ^{:private true
           :doc "Per-agent session start times. Map of agent-id -> java.time.Instant.
                 Set on first tool call per agent (idempotent CAS per key). Reset on wrap/complete.
                 Global key \"_global\" used as fallback when agent-id is unknown."}
  session-start-tracker
  (atom {}))

(def ^:private default-agent-key "_global")

(defn record-session-start!
  "Record session start timestamp for an agent. Idempotent per agent-id —
   only records the first call for each agent.
   Returns the recorded start time (existing or new)."
  ([] (record-session-start! nil))
  ([agent-id]
   (let [k (or agent-id default-agent-key)
         now (java.time.Instant/now)]
     (swap! session-start-tracker
            (fn [m] (if (contains? m k) m (assoc m k now))))
     (get @session-start-tracker k))))

(defn get-session-start
  "Get the recorded session start time for an agent, or nil if not yet started.
   Falls back to _global if agent-specific start not found."
  ([] (get-session-start nil))
  ([agent-id]
   (let [k (or agent-id default-agent-key)]
     (or (get @session-start-tracker k)
         (when-not (= k default-agent-key)
           (get @session-start-tracker default-agent-key))))))

(defn reset-session-start!
  "Reset session start tracker for an agent. Called after crystallization completes.
   With no args, resets ALL agent timestamps (full reset)."
  ([] (reset! session-start-tracker {}))
  ([agent-id]
   (let [k (or agent-id default-agent-key)]
     (swap! session-start-tracker dissoc k))))

(defn session-timing-metadata
  "Compute session timing metadata from start and end instants.
   Returns {:session-start ISO-string, :session-end ISO-string, :duration-minutes long}.
   If start is nil, returns metadata with :session-start nil and :duration-minutes 0."
  [start-instant end-instant]
  (let [duration-minutes (if (and start-instant end-instant)
                           (.between java.time.temporal.ChronoUnit/MINUTES
                                     start-instant end-instant)
                           0)]
    {:session-start (some-> start-instant .toString)
     :session-end (.toString end-instant)
     :duration-minutes duration-minutes}))

;; =============================================================================
;; Crystallization Rules (Pure Predicates)
;; =============================================================================

(defn mechanical-recall?
  "Is this recall context mechanical/structural (low signal)?"
  [context]
  (contains? #{:catchup-structural :wrap-structural} context))

(defn meaningful-recalls
  "Filter to only meaningful recalls."
  [recalls]
  (remove #(mechanical-recall? (:context %)) recalls))

(defn cross-boundary-recalls
  "Get recalls that cross session/project boundaries."
  [recalls]
  (filter #(contains? #{:cross-session :cross-project} (:context %)) recalls))

(defn has-user-endorsement?
  "Check if any recall has user feedback."
  [recalls]
  (some #(= :user-feedback (:context %)) recalls))

(defn behavioral-recall?
  "Is this recall context a behavioral signal (from outcome tracking)?"
  [context]
  (contains? #{:behavioral-success :behavioral-failure :behavioral-correction} context))

(defn behavioral-recalls
  "Filter to only behavioral signal recalls."
  [recalls]
  (filter #(behavioral-recall? (:context %)) recalls))

(defn has-behavioral-signal?
  "Check if any recall has behavioral outcome data."
  [recalls]
  (some behavioral-recall? (map :context recalls)))

;; =============================================================================
;; Staleness Decay (W2: Scheduled Decay Lifecycle)
;; =============================================================================

(defn days-since
  "Calculate days elapsed since a timestamp string.
   Returns nil if timestamp is nil/empty or unparseable."
  [timestamp-str]
  (when (and timestamp-str (not (str/blank? (str timestamp-str))))
    (rescue nil
            (let [then (java.time.ZonedDateTime/parse (str timestamp-str))
                  now (java.time.ZonedDateTime/now)]
              (.between java.time.temporal.ChronoUnit/DAYS then now)))))

(defn decay-candidate?
  "Check if entry should be considered for staleness decay."
  ([entry] (decay-candidate? entry {}))
  ([{:keys [access-count duration type] :as _entry}
    {:keys [access-threshold] :or {access-threshold 3}}]
   (and (< (or access-count 0) access-threshold)
        (not= duration "permanent")
        (not= type "axiom"))))

(def ^:private duration-decay-rate
  "Decay rate multiplier per duration tier.
   Shorter durations decay faster (they're meant to be ephemeral)."
  {"ephemeral" 2.0
   "short"     1.5
   "medium"    1.0
   "long"      0.5})

(defn calculate-decay-delta
  "Calculate staleness-beta increase for a decay cycle."
  ([entry] (calculate-decay-delta entry {}))
  ([{:keys [access-count last-accessed duration] :as _entry}
    {:keys [recency-days] :or {recency-days 7}}]
   (let [days-idle (or (days-since last-accessed)
                       30) ;; Never accessed → treat as 30 days idle
         access (max 1 (or access-count 0))]
     (if (< days-idle recency-days)
       0.0 ;; Recently accessed - no decay
       (let [time-factor (/ (double days-idle) 30.0) ;; Normalize to ~monthly
             access-dampening (/ 1.0 (Math/log (+ access 2))) ;; log dampening
             rate (get duration-decay-rate (or duration "medium") 1.0)]
         (* time-factor access-dampening rate))))))

;; =============================================================================
;; Scope Detection (Auto-Promotion)
;; =============================================================================

(defn extract-xpoll-projects
  "Extract distinct project IDs from scope tags."
  [entry]
  (let [tags (or (:tags entry) [])]
    (->> tags
         (filter #(str/starts-with? % "xpoll:project:"))
         (map #(subs % (count "xpoll:project:")))
         set)))

(defn scope-count
  "Count distinct scope accesses for this entry."
  [entry]
  (count (extract-xpoll-projects entry)))

(defn scope-boost
  "Compute promotion score boost from scope breadth.
   Delegates to extension if available. Returns 0.0 otherwise."
  [entry]
  (if-let [f (ext/get-extension :gx/score)]
    (f entry)
    0.0))

(defn scope-eligible?
  "Predicate: is this entry eligible for scope-based auto-promotion?
   Delegates to extension if available. Returns false otherwise."
  ([entry] (scope-eligible? entry {}))
  ([entry opts]
   (if-let [f (ext/get-extension :gx/eligible?)]
     (f entry opts)
     false)))

(defn scope-tiers
  "Compute tiers to promote based on scope breadth.
   Delegates to extension if available. Returns 0 otherwise."
  [entry]
  (if-let [f (ext/get-extension :gx/tiers)]
    (f entry)
    0))

;; =============================================================================
;; Progress Note Generation
;; =============================================================================

(defn task-to-progress-note
  "Convert a completed kanban task to a progress note."
  [{:keys [title context priority started] :as task}]
  (let [completed-at (or (:completed-at task)
                         (.toString (java.time.Instant/now)))
        duration-str (when started
                       (str " (started: " started ")"))
        content (str "## Completed: " title "\n\n"
                     (when context (str context "\n\n"))
                     "Priority: " (or priority "medium")
                     duration-str
                     "\nCompleted: " completed-at)]
    {:type :note
     :content content
     :tags [(session-tag) "session-progress" "completed-task"
            (str "priority-" (or priority "medium"))]
     :duration :ephemeral}))

(defn- extract-content-summary
  "Extract a one-line summary from note content."
  [content]
  (cond
    (nil? content) "(no content)"
    (string? content) (or (first (str/split-lines content)) "(empty)")
    (map? content) (or (:title content)
                       (:task-type content)
                       (str (keys content)))
    :else (str content)))

(defn summarize-session-progress
  "Summarize multiple progress notes into a session summary.

   notes: seq of progress note maps (handles nil, empty, or malformed)
   git-commits: seq of commit strings (handles nil)

   Returns: session summary map suitable for crystallization, or nil if no content.

   'Key must be integer' error when accessing (:tags item) on vectors.
   Also filters out notes with nil/empty content to prevent '(no content)' entries."
  [notes git-commits]
  (let [;; Guard: ensure notes is a sequence of maps only
        notes (->> (or notes [])
                   (filter map?))
        git-commits (or git-commits [])
        ;; Filter to only notes with actual content before processing
        notes-with-content (->> notes
                                (filter #(let [c (:content %)]
                                           (and (some? c)
                                                (if (string? c)
                                                  (not (str/blank? c))
                                                  true)))))
        task-count (count (filter #(some #{"completed-task"} (:tags %)) notes-with-content))
        session (session-id)
        note-summaries (->> notes-with-content
                            (map #(extract-content-summary (:content %)))
                            ;; Extra safety: filter out placeholder values
                            (remove #(contains? #{"(no content)" "(empty)"} %))
                            (map #(str "- " %))
                            (str/join "\n"))
        commit-summaries (->> git-commits
                              (map #(str "- " %))
                              (str/join "\n"))
        ;; Only create summary if there's actual content
        has-content? (or (seq notes-with-content) (seq git-commits))]
    (when has-content?
      {:type :note
       :content (str "## Session Summary: " session "\n\n"
                     "### Completed Tasks: " task-count "\n"
                     note-summaries
                     "\n\n### Commits: " (count git-commits) "\n"
                     commit-summaries)
       :tags [(session-tag) "session-summary" "wrap-generated"]
       :duration :short})))

(defn summarize-memory-activity
  "Produce a session summary from memory activity alone.
   For coordinator sessions that create/access memories without
   generating progress-notes, tasks, or git commits.

   Returns summary map or nil if no memory activity."
  [{:keys [created accessed]}]
  (when (pos? (+ (or created 0) (or accessed 0)))
    {:type :note
     :content (str "## Session Summary: " (session-id) "\n\n"
                   "### Memory Activity\n"
                   "- Memories created: " (or created 0) "\n"
                   "- Memories accessed: " (or accessed 0) "\n")
     :tags [(session-tag) "session-summary" "wrap-generated" "coordinator"]
     :duration :short}))

(comment
  ;; Example usage
  (calculate-promotion-score
   [{:context :explicit-reference :count 2}
    {:context :cross-session :count 1}
    {:context :catchup-structural :count 5}])
  ;; => {:score 7.5, :breakdown [...]}

  (should-promote? {:duration :ephemeral
                    :recalls [{:context :explicit-reference :count 3}
                              {:context :cross-session :count 1}]})
  ;; => {:promote? true, :current-score 5.0, :threshold 5.0, :next-duration :short}
  )
