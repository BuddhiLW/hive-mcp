(ns hive-mcp.crystal.recall
  "Recall tracking with context-aware weighting.
   
   Tracks when and how memories are accessed, distinguishing between:
   - Mechanical recalls (catchup/wrap structural queries)
   - Meaningful recalls (LLM explicitly references)
   - Cross-boundary recalls (different session/project)
   - User endorsements (human feedback)
   
   SOLID: Single responsibility - recall tracking only.
   DDD: Bounded context for access patterns."
  (:require [hive-mcp.crystal.core :as crystal]
            [clojure.string :as str]))

;; =============================================================================
;; Recall Context Detection
;; =============================================================================

(defn detect-recall-context
  "Detect the context type of a recall event.
   
   params: {:source :string        ; 'catchup', 'wrap', 'agent', 'user'
            :session :string       ; current session id
            :project :string       ; current project
            :entry-session :string ; session that created the entry
            :entry-project :string ; project the entry belongs to
            :explicit? :boolean}   ; LLM explicitly cited this
   
   Returns: keyword from crystal/recall-weights"
  [{:keys [source session project entry-session entry-project explicit?]}]
  (cond
    ;; User explicitly marked as helpful
    (= source "feedback")
    :user-feedback

    ;; LLM explicitly cited this entry
    explicit?
    :explicit-reference

    ;; Cross-project access
    (and project entry-project
         (not= project entry-project))
    :cross-project

    ;; Cross-session access
    (and session entry-session
         (not= session entry-session))
    :cross-session

    ;; Structural catchup query
    (= source "catchup")
    :catchup-structural

    ;; Structural wrap query
    (= source "wrap")
    :wrap-structural

    ;; Default: treat as explicit reference
    :else
    :explicit-reference))

;; =============================================================================
;; Recall Event Creation
;; =============================================================================

(defn create-recall-event
  "Create a recall event record.
   
   Returns: {:context :keyword
             :timestamp :string
             :source :string
             :session :string}"
  [context-params]
  (let [context (detect-recall-context context-params)]
    {:context context
     :timestamp (.toString (java.time.Instant/now))
     :source (:source context-params)
     :session (or (:session context-params) (crystal/session-id))}))

(defn batch-recall-events
  "Create recall events for multiple entries queried together.
   
   entry-ids: seq of memory entry IDs
   context-params: shared context for all entries
   
   Returns: map of {entry-id [recall-event]}"
  [entry-ids context-params]
  (let [event (create-recall-event context-params)]
    (into {} (map (fn [id] [id [event]]) entry-ids))))

;; =============================================================================
;; Recall Aggregation
;; =============================================================================

(defn aggregate-recalls
  "Aggregate recall events by context type.
   
   recalls: seq of {:context :keyword ...}
   
   Returns: seq of {:context :keyword :count int}"
  [recalls]
  (->> recalls
       (group-by :context)
       (map (fn [[ctx events]]
              {:context ctx
               :count (count events)}))
       (sort-by :count >)))

(defn merge-recall-histories
  "Merge new recalls with existing recall history.
   
   existing: seq of {:context :count}
   new-recalls: seq of {:context ...}
   
   Returns: merged and re-aggregated recalls"
  [existing new-recalls]
  (let [new-aggregated (aggregate-recalls new-recalls)
        ;; Convert existing to map for merging
        existing-map (into {} (map (juxt :context :count) existing))
        new-map (into {} (map (juxt :context :count) new-aggregated))]
    ;; Merge counts
    (->> (merge-with + existing-map new-map)
         (map (fn [[ctx cnt]] {:context ctx :count cnt}))
         (sort-by :count >)
         vec)))

;; =============================================================================
;; Smart Recall Classification
;; =============================================================================

(defn classify-query-intent
  "Classify a memory query to determine recall context.
   
   query-type: 'note', 'decision', 'convention', 'snippet'
   tags: seq of query tags
   caller: which system made the query
   
   Returns: :catchup-structural | :wrap-structural | :explicit-reference"
  [query-type tags caller]
  (cond
    ;; Catchup workflow queries
    (and (= caller "catchup")
         (or (some #{"session-summary"} tags)
             (some #{"session-notes"} tags)))
    :catchup-structural

    ;; Wrap workflow queries
    (and (= caller "wrap")
         (or (some #{"session-progress"} tags)
             (some #{"ephemeral"} tags)))
    :wrap-structural

    ;; Workflow queries for conventions/decisions are still meaningful
    (and (contains? #{"decision" "convention"} query-type)
         (contains? #{"catchup" "wrap"} caller))
    :explicit-reference

    ;; Everything else is meaningful
    :else
    :explicit-reference))

;; =============================================================================
;; Session Boundary Detection
;; =============================================================================

(defn crosses-session-boundary?
  "Check if accessing entry crosses session boundary."
  [current-session entry-tags]
  (when-let [entry-session (crystal/extract-session-from-tags entry-tags)]
    (not= current-session entry-session)))

(defn crosses-project-boundary?
  "Check if accessing entry crosses project boundary."
  [current-project entry-tags]
  (when-let [entry-project (some #(when (str/starts-with? % "scope:project:")
                                    (subs % 14))
                                 entry-tags)]
    (not= current-project entry-project)))

;; =============================================================================
;; Recall Tracking State (Optional - for hot-path optimization)
;; =============================================================================

(defonce ^{:private true
           :doc "Buffer for batching recall events before persistence."}
  recall-buffer
  (atom {}))

(defn buffer-recall!
  "Buffer a recall event for later persistence.
   
   entry-id: memory entry ID
   event: recall event map"
  [entry-id event]
  (swap! recall-buffer update entry-id (fnil conj []) event))

(defn flush-recall-buffer!
  "Get and clear buffered recalls.
   Returns: map of {entry-id [events]}"
  []
  (let [buffered @recall-buffer]
    (reset! recall-buffer {})
    buffered))

(defn get-buffered-recalls
  "Get buffered recalls without clearing."
  []
  @recall-buffer)

(comment
  ;; Example usage
  (detect-recall-context
   {:source "agent"
    :session "2026-01-04"
    :project "hive-mcp"
    :entry-session "2026-01-03"
    :entry-project "hive-mcp"
    :explicit? false})
  ;; => :cross-session

  (aggregate-recalls
   [{:context :explicit-reference}
    {:context :explicit-reference}
    {:context :cross-session}
    {:context :catchup-structural}
    {:context :catchup-structural}
    {:context :catchup-structural}])
  ;; => [{:context :catchup-structural :count 3}
  ;;     {:context :explicit-reference :count 2}
  ;;     {:context :cross-session :count 1}]
  )
