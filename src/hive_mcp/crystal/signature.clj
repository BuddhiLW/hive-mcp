(ns hive-mcp.crystal.signature
  "Behavioral signature tracking for hive-knowledge feedback loop.

   Two levels of tracking:

   ## 1. Outcome Signatures (recall-level)
   Tracks the outcome of memory recalls to learn which memories are actually
   useful vs. just frequently accessed. This enables the knowledge system to:
   - Distinguish between 'recalled' and 'useful' (behavioral validation)
   - Learn from task outcomes (did the recalled memory lead to success?)
   - Track user corrections (indicates low-quality memories)
   - Measure time-to-completion (efficiency signal)

   ## 2. Behavioral Signatures (agent-level)
   Tracks agent behavioral patterns over time:
   - Tool-use frequency (which tools does this agent prefer?)
   - Task completion rates (how often does this agent succeed?)
   - Error patterns (what types of errors does this agent encounter?)
   - Session activity (duration, tool count, task count per session)

   NOTE: This module contains SCHEMA + CRUD tracking only (L1/L2 open).
   Behavioral ANALYSIS algorithms (similarity scoring, clustering, anomaly
   detection) are L3+ closed IP in hive-knowledge.

   Integration points:
   - crystal/recall.clj: Links to recall events via recall-id
   - crystal/core.clj: Feeds promotion scoring with outcome weights
   - crystal/hooks.clj: Triggered on task completion/session end

   SOLID: Single responsibility - behavioral tracking (outcome + agent).
   DDD: Bounded context for outcome validation and agent profiling."
  (:require [taoensso.timbre :as log]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schema Definitions
;; =============================================================================

(def outcome-schema
  "Schema for behavioral outcome tracking.

   :task-completed? - Did the task succeed after recall?
   :tools-used - Which tools were invoked during task execution
   :user-correction? - Did the user have to correct the agent's work?
   :time-to-completion-ms - Time from recall to task completion"
  {:task-completed? :boolean
   :tools-used [:vector :string]
   :user-correction? :boolean
   :time-to-completion-ms :long})

(def signature-schema
  "Full behavioral signature schema.

   :signature/id - Unique signature identifier
   :signature/recall-id - Links to the recall event that preceded this outcome
   :signature/entries-recalled - Memory entry IDs that were recalled
   :signature/outcome - The behavioral outcome map
   :signature/created-at - Timestamp of signature creation
   :signature/session-id - Session where this occurred
   :signature/project-id - Project context"
  {:signature/id :string
   :signature/recall-id :string
   :signature/entries-recalled [:vector :string]
   :signature/outcome :map
   :signature/created-at :instant
   :signature/session-id :string
   :signature/project-id :string})

;; =============================================================================
;; Signature State (Buffer for batching before persistence)
;; =============================================================================

(defonce ^{:private true
           :doc "Buffer for batching signatures before persistence.
                 Maps recall-id to partial signature (entries captured at recall time)."}
  signature-buffer
  (atom {}))

(defonce ^{:private true
           :doc "Completed signatures awaiting persistence."}
  completed-signatures
  (atom []))

;; =============================================================================
;; ID Generation
;; =============================================================================

(defn- generate-signature-id
  "Generate a unique signature ID.
   Format: sig-<timestamp>-<random>"
  []
  (str "sig-"
       (.toEpochMilli (java.time.Instant/now))
       "-"
       (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn- generate-recall-id
  "Generate a unique recall ID for linking signatures.
   Format: rcl-<timestamp>-<random>"
  []
  (str "rcl-"
       (.toEpochMilli (java.time.Instant/now))
       "-"
       (subs (str (java.util.UUID/randomUUID)) 0 8)))

;; =============================================================================
;; Signature Creation
;; =============================================================================

(defn start-signature
  "Start tracking a behavioral signature when memories are recalled.

   Called when memory query returns results - captures the 'before' state.

   entry-ids: seq of memory entry IDs that were recalled
   opts: {:session-id string, :project-id string, :source string}

   Returns: recall-id (use this to complete the signature later)"
  [entry-ids {:keys [session-id project-id source] :as _opts}]
  (let [recall-id (generate-recall-id)
        now (java.time.Instant/now)]
    (swap! signature-buffer assoc recall-id
           {:recall-id recall-id
            :entries-recalled (vec entry-ids)
            :session-id session-id
            :project-id project-id
            :source source
            :started-at now})
    (log/trace "Started signature tracking:" recall-id
               "entries:" (count entry-ids)
               "source:" source)
    recall-id))

(defn complete-signature
  "Complete a behavioral signature with outcome data.

   Called when task completes (success or failure) - captures the 'after' state.

   recall-id: The ID returned from start-signature
   outcome: {:task-completed? bool
             :tools-used [string]
             :user-correction? bool
             :time-to-completion-ms long}

   Returns: completed signature map, or nil if recall-id not found"
  [recall-id {:keys [task-completed? tools-used user-correction? time-to-completion-ms]
              :or {task-completed? false
                   tools-used []
                   user-correction? false}}]
  (if-let [pending (get @signature-buffer recall-id)]
    (let [now (java.time.Instant/now)
          ;; Calculate time-to-completion if not provided
          elapsed-ms (or time-to-completion-ms
                         (when-let [started (:started-at pending)]
                           (- (.toEpochMilli now)
                              (.toEpochMilli started))))
          completed-sig {:signature/id (generate-signature-id)
                         :signature/recall-id recall-id
                         :signature/entries-recalled (:entries-recalled pending)
                         :signature/outcome {:task-completed? (boolean task-completed?)
                                             :tools-used (vec (or tools-used []))
                                             :user-correction? (boolean user-correction?)
                                             :time-to-completion-ms elapsed-ms}
                         :signature/created-at now
                         :signature/session-id (:session-id pending)
                         :signature/project-id (:project-id pending)}]
      ;; Move from buffer to completed
      (swap! signature-buffer dissoc recall-id)
      (swap! completed-signatures conj completed-sig)
      (log/debug "Completed signature:" (:signature/id completed-sig)
                 "task-completed?" task-completed?
                 "correction?" user-correction?
                 "elapsed-ms:" elapsed-ms)
      completed-sig)
    (do
      (log/warn "Signature not found for recall-id:" recall-id)
      nil)))

(defn abandon-signature
  "Abandon a pending signature (e.g., user cancelled task).

   recall-id: The ID returned from start-signature
   reason: Why the signature was abandoned

   Returns: true if found and removed, false otherwise"
  [recall-id reason]
  (if (contains? @signature-buffer recall-id)
    (do
      (swap! signature-buffer dissoc recall-id)
      (log/debug "Abandoned signature:" recall-id "reason:" reason)
      true)
    false))

;; =============================================================================
;; Outcome Calculation Helpers
;; =============================================================================

(defn calculate-outcome-weight
  "Calculate a weight for this outcome to feed back into promotion scoring.

   Higher weights = better outcome = memory was more useful.

   outcome: {:task-completed? bool, :user-correction? bool, :time-to-completion-ms long}

   Returns: float weight (0.0 to 5.0)"
  [{:keys [task-completed? user-correction? time-to-completion-ms]}]
  (cond
    ;; User had to correct = memory was wrong/misleading
    user-correction?
    -1.0

    ;; Task failed after recall = memory wasn't helpful
    (not task-completed?)
    0.0

    ;; Task completed without correction
    :else
    (let [base-weight 2.0
          ;; Bonus for fast completion (< 30 seconds)
          speed-bonus (if (and time-to-completion-ms
                               (< time-to-completion-ms 30000))
                        1.0
                        0.0)]
      (+ base-weight speed-bonus))))

(defn outcome->promotion-signal
  "Convert an outcome to a promotion signal that can be used by crystal/core.

   outcome: behavioral outcome map

   Returns: {:context keyword, :weight float} suitable for promotion scoring"
  [{:keys [task-completed? user-correction?] :as outcome}]
  (cond
    user-correction?
    {:context :behavioral-correction
     :weight -2.0}

    task-completed?
    {:context :behavioral-success
     :weight (calculate-outcome-weight outcome)}

    :else
    {:context :behavioral-failure
     :weight 0.0}))

;; =============================================================================
;; Signature Buffer Management
;; =============================================================================

(defn get-pending-signatures
  "Get all pending signatures (started but not completed)."
  []
  @signature-buffer)

(defn get-completed-signatures
  "Get all completed signatures awaiting persistence."
  []
  @completed-signatures)

(defn flush-completed-signatures!
  "Get and clear completed signatures.
   Returns: vector of completed signature maps"
  []
  (let [completed @completed-signatures]
    (reset! completed-signatures [])
    completed))

(defn clear-stale-signatures!
  "Clear signatures that have been pending too long (orphaned).

   max-age-ms: Maximum age in milliseconds (default: 30 minutes)

   Returns: count of cleared signatures"
  [& {:keys [max-age-ms] :or {max-age-ms 1800000}}]
  (let [now-ms (.toEpochMilli (java.time.Instant/now))
        stale? (fn [[_recall-id sig]]
                 (when-let [started (:started-at sig)]
                   (> (- now-ms (.toEpochMilli started)) max-age-ms)))
        stale-ids (->> @signature-buffer
                       (filter stale?)
                       (map first))]
    (doseq [id stale-ids]
      (swap! signature-buffer dissoc id))
    (when (seq stale-ids)
      (log/info "Cleared" (count stale-ids) "stale signatures"))
    (count stale-ids)))

;; =============================================================================
;; Aggregation & Analysis
;; =============================================================================

(defn aggregate-entry-outcomes
  "Aggregate outcomes for a specific memory entry.

   entry-id: Memory entry ID
   signatures: seq of completed signatures

   Returns: {:recalls int
             :successes int
             :failures int
             :corrections int
             :avg-completion-ms float
             :effectiveness-ratio float}"
  [entry-id signatures]
  (let [relevant (->> signatures
                      (filter #(some #{entry-id} (:signature/entries-recalled %))))
        outcomes (map :signature/outcome relevant)
        successes (count (filter :task-completed? outcomes))
        corrections (count (filter :user-correction? outcomes))
        completion-times (->> outcomes
                              (map :time-to-completion-ms)
                              (filter some?))]
    {:recalls (count relevant)
     :successes successes
     :failures (- (count relevant) successes)
     :corrections corrections
     :avg-completion-ms (when (seq completion-times)
                          (/ (reduce + completion-times) (count completion-times)))
     :effectiveness-ratio (when (pos? (count relevant))
                            (/ (- successes corrections) (count relevant)))}))

(defn compute-promotion-adjustment
  "Compute promotion score adjustment based on behavioral signatures.

   entry-id: Memory entry ID
   signatures: seq of completed signatures

   Returns: float adjustment to add to promotion score (can be negative)"
  [entry-id signatures]
  (let [{:keys [effectiveness-ratio corrections recalls]}
        (aggregate-entry-outcomes entry-id signatures)]
    (cond
      ;; Not enough data
      (or (nil? recalls) (< recalls 2))
      0.0

      ;; Many corrections = demote
      (and (pos? corrections) (> (/ corrections recalls) 0.3))
      (* -2.0 corrections)

      ;; High effectiveness = boost
      (and effectiveness-ratio (> effectiveness-ratio 0.7))
      (* 2.0 effectiveness-ratio recalls)

      ;; Low effectiveness = penalty
      (and effectiveness-ratio (< effectiveness-ratio 0.3))
      (* -1.0 (- 1.0 effectiveness-ratio) recalls)

      ;; Neutral
      :else
      0.0)))

;; =============================================================================
;; Integration Hooks
;; =============================================================================

(defn on-memory-query-complete
  "Hook to call after a memory query returns results.

   Starts signature tracking for behavioral validation.

   entry-ids: seq of memory entry IDs returned by query
   context: {:session-id :project-id :source :query-type}

   Returns: recall-id for completing the signature later"
  [entry-ids context]
  (when (seq entry-ids)
    (start-signature entry-ids context)))

(defn on-task-complete
  "Hook to call when a task completes (success or failure).

   Completes any pending signature with outcome data.

   recall-id: ID from on-memory-query-complete (or nil to complete most recent)
   outcome: {:task-completed? :tools-used :user-correction?}

   Returns: completed signature or nil"
  [recall-id outcome]
  (if recall-id
    (complete-signature recall-id outcome)
    ;; If no recall-id, try to complete the most recent pending signature
    (when-let [[most-recent-id _] (first (sort-by #(get-in % [1 :started-at]) > @signature-buffer))]
      (complete-signature most-recent-id outcome))))

(defn on-user-correction
  "Hook to call when user corrects agent work.

   Marks any pending signatures as having required correction.

   Returns: count of signatures marked"
  []
  (let [pending-ids (keys @signature-buffer)]
    (doseq [recall-id pending-ids]
      (complete-signature recall-id {:task-completed? true
                                     :user-correction? true}))
    (count pending-ids)))

;; =============================================================================
;; Behavioral Signature Schema (Agent-Level)
;; =============================================================================
;; NOTE: Schema + CRUD = L1/L2 open (AGPL). Analysis algorithms = L3+ closed.

(def behavior-event-schema
  "Schema for a single behavioral event recorded for an agent.

   :event/type - Category of the event (:tool-use, :task-complete, :task-fail, :error, :session-start, :session-end)
   :event/agent-id - The agent that produced this event
   :event/timestamp - When the event occurred (Instant)
   :event/project-id - Project context
   :event/session-id - Session context
   :event/data - Type-specific payload map"
  {:event/type :keyword
   :event/agent-id :string
   :event/timestamp :instant
   :event/project-id :string
   :event/session-id :string
   :event/data :map})

(def tool-use-data-schema
  "Payload schema for :tool-use events.

   :tool-name - Name of the tool invoked (e.g. \"read_file\", \"grep\", \"cider_eval\")
   :duration-ms - How long the tool call took (nil if unknown)
   :success? - Whether the tool call succeeded"
  {:tool-name :string
   :duration-ms :long
   :success? :boolean})

(def task-outcome-data-schema
  "Payload schema for :task-complete and :task-fail events.

   :task-id - Kanban task ID (if applicable)
   :task-type - Category of task (e.g. \"implementation\", \"review\", \"research\")
   :tools-used - Tools invoked during this task
   :duration-ms - Total task duration
   :error-count - Number of errors encountered during task"
  {:task-id :string
   :task-type :string
   :tools-used [:vector :string]
   :duration-ms :long
   :error-count :long})

(def error-data-schema
  "Payload schema for :error events.

   :error-type - Category (:nrepl-connection, :timeout, :eval-error, :file-conflict, :tool-error)
   :error-message - Human-readable error message (truncated to 200 chars)
   :tool-name - Tool that caused the error (if applicable)
   :recoverable? - Whether the agent recovered from this error"
  {:error-type :keyword
   :error-message :string
   :tool-name :string
   :recoverable? :boolean})

(def agent-profile-schema
  "Schema for an agent's behavioral profile (aggregated view).

   This is a DERIVED structure computed from raw events.
   L1/L2: Basic counts and ratios only.
   L3+: Similarity scoring, clustering, anomaly detection → hive-knowledge.

   :profile/agent-id - The agent this profile describes
   :profile/total-tasks - Total tasks attempted
   :profile/completed-tasks - Tasks completed successfully
   :profile/failed-tasks - Tasks that failed
   :profile/total-tool-calls - Total tool invocations
   :profile/tool-frequency - Map of {tool-name count}
   :profile/error-frequency - Map of {error-type count}
   :profile/total-errors - Total errors encountered
   :profile/avg-task-duration-ms - Average task duration
   :profile/sessions - Number of sessions observed
   :profile/last-active - Timestamp of most recent activity
   :profile/computed-at - When this profile was last computed"
  {:profile/agent-id :string
   :profile/total-tasks :long
   :profile/completed-tasks :long
   :profile/failed-tasks :long
   :profile/total-tool-calls :long
   :profile/tool-frequency :map
   :profile/error-frequency :map
   :profile/total-errors :long
   :profile/avg-task-duration-ms :double
   :profile/sessions :long
   :profile/last-active :instant
   :profile/computed-at :instant})

;; =============================================================================
;; Behavioral Event Store (Atom-based, same pattern as signature-buffer)
;; =============================================================================

(defonce ^{:private true
           :doc "In-memory store for behavioral events.
                 Maps agent-id to vector of behavior events.
                 Bounded: max-events-per-agent prevents unbounded growth."}
  behavior-store
  (atom {}))

(def ^:private max-events-per-agent
  "Maximum behavioral events retained per agent before FIFO eviction.
   Keeps memory bounded. Old events are dropped when limit is reached."
  500)

;; =============================================================================
;; Behavioral Event ID Generation
;; =============================================================================

(defn- generate-event-id
  "Generate a unique behavioral event ID.
   Format: bev-<timestamp>-<random>"
  []
  (str "bev-"
       (.toEpochMilli (java.time.Instant/now))
       "-"
       (subs (str (java.util.UUID/randomUUID)) 0 8)))

;; =============================================================================
;; Behavioral Event Recording (CRUD: Create)
;; =============================================================================

(defn record-behavior!
  "Record a behavioral event for an agent.

   This is the primary write operation for behavioral tracking.
   Events are stored in-memory (atom) with FIFO eviction when
   max-events-per-agent is exceeded.

   agent-id: String identifying the agent
   event-type: Keyword - one of #{:tool-use :task-complete :task-fail :error :session-start :session-end}
   data: Map with type-specific payload (see *-data-schema)
   opts: {:project-id string, :session-id string}

   Returns: The recorded event map (with generated ID and timestamp)"
  [agent-id event-type data & {:keys [project-id session-id]}]
  {:pre [(string? agent-id) (keyword? event-type) (map? data)]}
  (let [event {:event/id (generate-event-id)
               :event/type event-type
               :event/agent-id agent-id
               :event/timestamp (java.time.Instant/now)
               :event/project-id project-id
               :event/session-id session-id
               :event/data data}]
    (swap! behavior-store
           (fn [store]
             (let [existing (get store agent-id [])
                   ;; FIFO eviction: drop oldest when over limit
                   trimmed (if (>= (count existing) max-events-per-agent)
                             (subvec existing (- (count existing) (dec max-events-per-agent)))
                             existing)]
               (assoc store agent-id (conj trimmed event)))))
    (log/trace "Recorded behavior:" event-type "for agent:" agent-id
               "data-keys:" (keys data))
    event))

;; =============================================================================
;; Behavioral Event Querying (CRUD: Read)
;; =============================================================================

(defn query-behaviors
  "Query behavioral events for an agent with optional filters.

   agent-id: String identifying the agent
   opts: {:event-type keyword   - Filter by event type
          :project-id string    - Filter by project
          :session-id string    - Filter by session
          :since Instant        - Only events after this time
          :limit int            - Max events to return (default: all)}

   Returns: Vector of matching event maps, newest first."
  [agent-id & {:keys [event-type project-id session-id since limit]}]
  (let [events (get @behavior-store agent-id [])
        filtered (cond->> events
                   event-type (filter #(= event-type (:event/type %)))
                   project-id (filter #(= project-id (:event/project-id %)))
                   session-id (filter #(= session-id (:event/session-id %)))
                   since (filter #(let [ts (:event/timestamp %)]
                                    (and ts (.isAfter ts since)))))]
    (cond->> (vec (reverse filtered))
      limit (take limit)
      limit vec)))

(defn query-all-behaviors
  "Query behavioral events across ALL agents with optional filters.

   opts: {:event-type keyword   - Filter by event type
          :project-id string    - Filter by project
          :since Instant        - Only events after this time
          :limit int            - Max events to return}

   Returns: Vector of matching event maps, newest first."
  [& {:keys [event-type project-id since limit]}]
  (let [all-events (->> (vals @behavior-store)
                        (apply concat)
                        vec)
        filtered (cond->> all-events
                   event-type (filter #(= event-type (:event/type %)))
                   project-id (filter #(= project-id (:event/project-id %)))
                   since (filter #(let [ts (:event/timestamp %)]
                                    (and ts (.isAfter ts since)))))]
    (cond->> (vec (sort-by :event/timestamp #(compare %2 %1) filtered))
      limit (take limit)
      limit vec)))

;; =============================================================================
;; Agent Profile Computation (CRUD: Read - derived/aggregate)
;; =============================================================================

(defn agent-profile
  "Compute a behavioral profile for an agent from their recorded events.

   This is a L1/L2 aggregation: basic counts, ratios, and frequency maps.
   L3+ analysis (similarity scoring, clustering) belongs in hive-knowledge.

   agent-id: String identifying the agent
   opts: {:since Instant - Only consider events after this time}

   Returns: Agent profile map matching agent-profile-schema, or nil if no data."
  [agent-id & {:keys [since]}]
  (let [events (if since
                 (query-behaviors agent-id :since since)
                 (get @behavior-store agent-id []))]
    (when (seq events)
      (let [tool-events (filter #(= :tool-use (:event/type %)) events)
            task-completes (filter #(= :task-complete (:event/type %)) events)
            task-fails (filter #(= :task-fail (:event/type %)) events)
            error-events (filter #(= :error (:event/type %)) events)
            session-starts (filter #(= :session-start (:event/type %)) events)
            ;; Tool frequency: count of each tool name
            tool-freq (->> tool-events
                           (map #(get-in % [:event/data :tool-name]))
                           (filter some?)
                           frequencies)
            ;; Error frequency: count of each error type
            error-freq (->> error-events
                            (map #(get-in % [:event/data :error-type]))
                            (filter some?)
                            frequencies)
            ;; Average task duration from completed tasks
            task-durations (->> task-completes
                                (map #(get-in % [:event/data :duration-ms]))
                                (filter some?))
            avg-duration (when (seq task-durations)
                           (double (/ (reduce + task-durations)
                                      (count task-durations))))
            ;; Most recent activity
            last-active (->> events
                             (map :event/timestamp)
                             (filter some?)
                             sort
                             last)]
        {:profile/agent-id agent-id
         :profile/total-tasks (+ (count task-completes) (count task-fails))
         :profile/completed-tasks (count task-completes)
         :profile/failed-tasks (count task-fails)
         :profile/total-tool-calls (count tool-events)
         :profile/tool-frequency tool-freq
         :profile/error-frequency error-freq
         :profile/total-errors (count error-events)
         :profile/avg-task-duration-ms avg-duration
         :profile/sessions (count session-starts)
         :profile/last-active last-active
         :profile/computed-at (java.time.Instant/now)}))))

;; =============================================================================
;; Behavioral Similarity (L1/L2 - basic Jaccard, schema only)
;; =============================================================================
;; NOTE: Advanced similarity algorithms (structural, GNN-based) are L3+ closed IP.
;; This provides only a basic tool-overlap metric for internal use.

(defn behavior-similarity
  "Compute basic behavioral similarity between two agents.

   Uses Jaccard index over tool-use sets as a simple L1/L2 metric.
   L3+ algorithms (structural similarity, embedding-based) are in hive-knowledge.

   agent-id-a: First agent ID
   agent-id-b: Second agent ID

   Returns: {:jaccard float       - Tool-set overlap (0.0 to 1.0)
             :shared-tools set    - Tools used by both agents
             :unique-a set        - Tools only used by agent A
             :unique-b set        - Tools only used by agent B}
            or nil if either agent has no data."
  [agent-id-a agent-id-b]
  (let [profile-a (agent-profile agent-id-a)
        profile-b (agent-profile agent-id-b)]
    (when (and profile-a profile-b)
      (let [tools-a (set (keys (:profile/tool-frequency profile-a)))
            tools-b (set (keys (:profile/tool-frequency profile-b)))
            shared (clojure.set/intersection tools-a tools-b)
            union (clojure.set/union tools-a tools-b)
            jaccard (if (empty? union)
                      0.0
                      (double (/ (count shared) (count union))))]
        {:jaccard jaccard
         :shared-tools shared
         :unique-a (clojure.set/difference tools-a tools-b)
         :unique-b (clojure.set/difference tools-b tools-a)}))))

;; =============================================================================
;; Behavioral Store Management
;; =============================================================================

(defn get-tracked-agents
  "Get the set of agent IDs that have behavioral events recorded.
   Returns: set of agent-id strings."
  []
  (set (keys @behavior-store)))

(defn get-behavior-event-count
  "Get the total number of behavioral events stored for an agent.
   Returns: int (0 if agent has no data)."
  [agent-id]
  (count (get @behavior-store agent-id [])))

(defn clear-agent-behaviors!
  "Clear all behavioral events for a specific agent.
   Returns: count of events cleared."
  [agent-id]
  (let [count-before (get-behavior-event-count agent-id)]
    (swap! behavior-store dissoc agent-id)
    (when (pos? count-before)
      (log/debug "Cleared" count-before "behavioral events for agent:" agent-id))
    count-before))

(defn clear-all-behaviors!
  "Clear ALL behavioral events for ALL agents.
   Returns: count of agents cleared."
  []
  (let [count-before (count @behavior-store)]
    (reset! behavior-store {})
    (when (pos? count-before)
      (log/info "Cleared behavioral events for" count-before "agents"))
    count-before))

(defn evict-old-behaviors!
  "Evict behavioral events older than max-age for all agents.

   max-age-ms: Maximum age in milliseconds (default: 24 hours)

   Returns: {:agents-affected int, :events-evicted int}"
  [& {:keys [max-age-ms] :or {max-age-ms 86400000}}]
  (let [cutoff (java.time.Instant/ofEpochMilli
                (- (.toEpochMilli (java.time.Instant/now)) max-age-ms))
        result (atom {:agents-affected 0 :events-evicted 0})]
    (swap! behavior-store
           (fn [store]
             (reduce-kv
              (fn [acc agent-id events]
                (let [kept (filterv #(let [ts (:event/timestamp %)]
                                       (and ts (.isAfter ts cutoff)))
                                    events)
                      evicted (- (count events) (count kept))]
                  (when (pos? evicted)
                    (swap! result update :agents-affected inc)
                    (swap! result update :events-evicted + evicted))
                  (if (seq kept)
                    (assoc acc agent-id kept)
                    acc)))
              {}
              store)))
    (let [r @result]
      (when (pos? (:events-evicted r))
        (log/info "Evicted" (:events-evicted r) "old behavioral events from"
                  (:agents-affected r) "agents"))
      r)))

;; =============================================================================
;; REPL / Debug
;; =============================================================================

(comment
  ;; === Outcome Signatures (recall-level) ===

  ;; 1. Memory query happens, returns entry IDs
  (def recall-id
    (start-signature ["entry-1" "entry-2" "entry-3"]
                     {:session-id "2026-02-05"
                      :project-id "hive-mcp"
                      :source "agent"}))

  ;; 2. Agent does work using the recalled memories...
  ;; (time passes)

  ;; 3. Task completes successfully
  (complete-signature recall-id
                      {:task-completed? true
                       :tools-used ["file_write" "cider_eval"]
                       :user-correction? false})

  ;; 4. Check completed signatures
  (get-completed-signatures)

  ;; 5. Aggregate outcomes for an entry
  (aggregate-entry-outcomes "entry-1" (get-completed-signatures))

  ;; 6. Compute promotion adjustment
  (compute-promotion-adjustment "entry-1" (get-completed-signatures))

  ;; Clear stale signatures (> 30 min old)
  (clear-stale-signatures!)

  ;; Flush for persistence
  (flush-completed-signatures!)

  ;; === Behavioral Signatures (agent-level) ===

  ;; Record tool usage
  (record-behavior! "swarm-worker-123" :tool-use
                    {:tool-name "read_file" :duration-ms 150 :success? true}
                    :project-id "hive-mcp" :session-id "2026-02-05")

  ;; Record task completion
  (record-behavior! "swarm-worker-123" :task-complete
                    {:task-id "task-42" :task-type "implementation"
                     :tools-used ["read_file" "file_write" "grep"]
                     :duration-ms 300000 :error-count 0}
                    :project-id "hive-mcp")

  ;; Record an error
  (record-behavior! "swarm-worker-123" :error
                    {:error-type :nrepl-connection
                     :error-message "Connection refused on port 7888"
                     :tool-name "cider_eval" :recoverable? true}
                    :project-id "hive-mcp")

  ;; Query behaviors for an agent
  (query-behaviors "swarm-worker-123")
  (query-behaviors "swarm-worker-123" :event-type :tool-use)
  (query-behaviors "swarm-worker-123" :event-type :error :limit 5)

  ;; Get agent profile
  (agent-profile "swarm-worker-123")

  ;; Compare two agents
  (behavior-similarity "swarm-worker-123" "swarm-worker-456")

  ;; Management
  (get-tracked-agents)
  (get-behavior-event-count "swarm-worker-123")
  (evict-old-behaviors! :max-age-ms 3600000) ;; 1 hour
  (clear-agent-behaviors! "swarm-worker-123")
  (clear-all-behaviors!))
