(ns hive-mcp.crystal.recall
  "Recall tracking with context-aware weighting."
  (:require [hive-mcp.crystal.core :as crystal]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Recall Context Detection
;; =============================================================================

(defn detect-recall-context
  "Detect the context type of a recall event."
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
  "Create a recall event record."
  [context-params]
  (let [context (detect-recall-context context-params)]
    {:context context
     :timestamp (.toString (java.time.Instant/now))
     :source (:source context-params)
     :session (or (:session context-params) (crystal/session-id))}))

(defn batch-recall-events
  "Create recall events for multiple entries queried together."
  [entry-ids context-params]
  (let [event (create-recall-event context-params)]
    (into {} (map (fn [id] [id [event]]) entry-ids))))

;; =============================================================================
;; Recall Aggregation
;; =============================================================================

(defn aggregate-recalls
  "Aggregate recall events by context type."
  [recalls]
  (->> recalls
       (group-by :context)
       (map (fn [[ctx events]]
              {:context ctx
               :count (count events)}))
       (sort-by :count >)))

(defn merge-recall-histories
  "Merge new recalls with existing recall history."
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
  "Classify a memory query to determine recall context."
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
  "Buffer a recall event for later persistence."
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
