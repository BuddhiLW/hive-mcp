(ns hive-mcp.tools.memory.analytics
  "Analytics handlers for memory feedback and usage tracking.

   SOLID: SRP - Single responsibility for usage analytics.
   CLARITY: T - Telemetry first with access tracking.

   Handlers:
   - log-access: Track when entries are accessed
   - feedback: Record helpfulness feedback
   - helpfulness-ratio: Calculate entry usefulness ratio"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.chroma :as chroma]
            [taoensso.timbre :as log])
  (:import [java.time ZonedDateTime]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; ============================================================
;; Access Tracking Handler
;; ============================================================

(defn handle-log-access
  "Log access to a memory entry (Chroma-only).
   Increments access-count and updates last-accessed timestamp."
  [{:keys [id]}]
  (log/info "mcp-memory-log-access:" id)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      (let [new-count (inc (or (:access-count entry) 0))
            updated (chroma/update-entry! id {:access-count new-count
                                              :last-accessed (str (ZonedDateTime/now))})]
        (mcp-json (fmt/entry->json-alist updated)))
      (mcp-json {:error "Entry not found"}))))

;; ============================================================
;; Feedback Handler
;; ============================================================

(defn handle-feedback
  "Submit helpfulness feedback for a memory entry (Chroma-only).
   feedback should be 'helpful' or 'unhelpful'."
  [{:keys [id feedback]}]
  (log/info "mcp-memory-feedback:" id feedback)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      (let [updates (case feedback
                      "helpful" {:helpful-count (inc (or (:helpful-count entry) 0))}
                      "unhelpful" {:unhelpful-count (inc (or (:unhelpful-count entry) 0))}
                      (throw (ex-info "Invalid feedback type" {:feedback feedback})))
            updated (chroma/update-entry! id updates)]
        (mcp-json (fmt/entry->json-alist updated)))
      (mcp-json {:error "Entry not found"}))))

;; ============================================================
;; Helpfulness Ratio Handler
;; ============================================================

(defn handle-helpfulness-ratio
  "Get helpfulness ratio for a memory entry (Chroma-only).
   Returns helpful/(helpful+unhelpful) or null if no feedback."
  [{:keys [id]}]
  (log/info "mcp-memory-helpfulness-ratio:" id)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      (let [helpful (or (:helpful-count entry) 0)
            unhelpful (or (:unhelpful-count entry) 0)
            total (+ helpful unhelpful)
            ratio (when (pos? total) (/ (double helpful) total))]
        (mcp-json {:ratio ratio
                   :helpful helpful
                   :unhelpful unhelpful}))
      (mcp-json {:error "Entry not found"}))))
