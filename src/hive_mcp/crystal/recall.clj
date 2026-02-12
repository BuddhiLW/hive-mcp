(ns hive-mcp.crystal.recall
  "Recall tracking with context-aware weighting.

   Classification intelligence delegates to extension if available.
   Returns noop defaults otherwise.

   Extension points are resolved via the extensions registry at startup.
   When no extensions are registered, classification functions gracefully
   degrade to safe default results."
  (:require [hive-mcp.crystal.core :as crystal]
            [hive-mcp.extensions.registry :as ext]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

;; =============================================================================
;; Recall Context Detection — delegates to extension
;; =============================================================================

(defn detect-recall-context
  "Delegates to extension if available."
  [context-params]
  (delegate-or-noop :cl/a :explicit-reference [context-params]))

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
;; Recall Aggregation — delegates to extension
;; =============================================================================

(defn aggregate-recalls
  "Delegates to extension if available."
  [recalls]
  (delegate-or-noop :cl/c [] [recalls]))

(defn merge-recall-histories
  "Delegates to extension if available."
  [existing new-recalls]
  (delegate-or-noop :cl/d (vec existing) [existing new-recalls]))

;; =============================================================================
;; Recall Classification — delegates to extension
;; =============================================================================

(defn classify-query-intent
  "Delegates to extension if available."
  [query-type tags caller]
  (delegate-or-noop :cl/b :explicit-reference [query-type tags caller]))

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

;; =============================================================================
;; Created IDs Tracking (Session-scoped)
;; =============================================================================

(defonce ^{:private true
           :doc "Buffer for tracking memory IDs created during this session."}
  created-ids-buffer
  (atom []))

(defn register-created-id!
  "Register a newly created memory entry ID for session tracking.
   Called after successful chroma/index-memory-entry! in write path."
  [entry-id]
  (when entry-id
    (swap! created-ids-buffer conj
           {:id entry-id
            :timestamp (.toString (java.time.Instant/now))})))

(defn get-created-ids
  "Get all created IDs without clearing."
  []
  @created-ids-buffer)

(defn flush-created-ids!
  "Get and clear all created IDs.
   Returns: vector of {:id entry-id :timestamp iso-str}"
  []
  (let [ids @created-ids-buffer]
    (reset! created-ids-buffer [])
    ids))

(comment
  ;; Example usage
  (detect-recall-context
   {:source "agent"
    :session "2026-01-04"
    :project "hive-mcp"
    :entry-session "2026-01-03"
    :entry-project "hive-mcp"
    :explicit? false})
  ;; => :explicit-reference (noop) or classified context (with extension)

  (aggregate-recalls
   [{:context :explicit-reference}
    {:context :explicit-reference}
    {:context :cross-session}
    {:context :catchup-structural}
    {:context :catchup-structural}
    {:context :catchup-structural}])
  ;; => [] (noop) or [{:context :catchup-structural :count 3} ...] (with extension)
  )
