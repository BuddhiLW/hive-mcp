(ns hive-mcp.tools.memory.lifecycle
  "Lifecycle handlers for memory entry duration management.

   SOLID: SRP - Single responsibility for entry lifecycle.
   CLARITY: Y - Yield safe failure with boundary handling.

   Handlers:
   - set-duration: Explicitly set duration category
   - promote: Move to longer duration
   - demote: Move to shorter duration
   - cleanup-expired: Remove expired entries
   - expiring-soon: List entries expiring within N days"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma with-entry]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; ============================================================
;; Set Duration Handler
;; ============================================================

(defn handle-set-duration
  "Set duration category for a memory entry (Chroma-only)."
  [{:keys [id duration]}]
  (log/info "mcp-memory-set-duration:" id duration)
  (with-chroma
    (let [expires (dur/calculate-expires duration)
          updated (chroma/update-entry! id {:duration duration
                                            :expires (or expires "")})]
      (if updated
        {:type "text" :text (json/write-str (fmt/entry->json-alist updated))}
        {:type "text" :text (json/write-str {:error "Entry not found"}) :isError true}))))

;; ============================================================
;; Promote/Demote Handlers
;; ============================================================

(defn- shift-entry-duration
  "Shift entry duration by delta steps. Returns MCP response.
   SOLID: DRY - Unified promote/demote logic."
  [id delta boundary-msg]
  (with-entry [entry id]
    (let [{:keys [new-duration changed?]} (dur/shift-duration (:duration entry) delta)]
      (if-not changed?
        {:type "text" :text (json/write-str {:message boundary-msg
                                             :duration new-duration})}
        (let [expires (dur/calculate-expires new-duration)
              updated (chroma/update-entry! id {:duration new-duration
                                                :expires (or expires "")})]
          {:type "text" :text (json/write-str (fmt/entry->json-alist updated))})))))

(defn handle-promote
  "Promote memory entry to longer duration (Chroma-only)."
  [{:keys [id]}]
  (log/info "mcp-memory-promote:" id)
  (shift-entry-duration id +1 "Already at maximum duration"))

(defn handle-demote
  "Demote memory entry to shorter duration (Chroma-only)."
  [{:keys [id]}]
  (log/info "mcp-memory-demote:" id)
  (shift-entry-duration id -1 "Already at minimum duration"))

;; ============================================================
;; Cleanup Handler
;; ============================================================

(defn handle-cleanup-expired
  "Remove all expired memory entries (Chroma-only)."
  [_]
  (log/info "mcp-memory-cleanup-expired")
  (with-chroma
    (let [count (chroma/cleanup-expired!)]
      {:type "text" :text (json/write-str {:deleted count})})))

;; ============================================================
;; Expiring Soon Handler
;; ============================================================

(defn handle-expiring-soon
  "List memory entries expiring within N days (Chroma-only)."
  [{:keys [days]}]
  (log/info "mcp-memory-expiring-soon:" (or days 7))
  (with-chroma
    (let [project-id (scope/get-current-project-id)
          entries (chroma/entries-expiring-soon (or days 7) :project-id project-id)]
      {:type "text" :text (json/write-str (mapv fmt/entry->json-alist entries))})))
