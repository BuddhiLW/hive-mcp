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
            [hive-mcp.tools.core :refer [mcp-error coerce-int!]]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.agent.context :as ctx]
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
  "Remove all expired memory entries (Chroma-only).
   Also cleans up KG edges for deleted entries to maintain referential integrity."
  [_]
  (log/info "mcp-memory-cleanup-expired")
  (with-chroma
    (let [{:keys [count deleted-ids]} (chroma/cleanup-expired!)
          ;; Clean up KG edges for all deleted entries
          edges-removed (when (seq deleted-ids)
                          (reduce (fn [total id]
                                    (+ total (kg-edges/remove-edges-for-node! id)))
                                  0 deleted-ids))]
      (when (pos? (or edges-removed 0))
        (log/info "Cleaned up" edges-removed "KG edges for" count "deleted entries"))
      {:type "text" :text (json/write-str {:deleted count
                                           :kg_edges_removed (or edges-removed 0)})})))

;; ============================================================
;; Expire (Delete) Handler
;; ============================================================

(defn handle-expire
  "Force-expire (delete) a memory entry by ID.
   Also cleans up KG edges for the deleted entry."
  [{:keys [id]}]
  (log/info "mcp-memory-expire:" id)
  (with-entry [_entry id]
    (let [edges-removed (kg-edges/remove-edges-for-node! id)]
      (chroma/delete-entry! id)
      (when (pos? edges-removed)
        (log/info "Cleaned up" edges-removed "KG edges for expired entry" id))
      {:type "text" :text (json/write-str {:expired id
                                           :kg_edges_removed edges-removed})})))

;; ============================================================
;; Expiring Soon Handler
;; ============================================================

(defn handle-expiring-soon
  "List memory entries expiring within N days (Chroma-only).
   When directory is provided, filters results to that project scope.
   Without directory, returns all expiring entries (no scope filter)."
  [{:keys [days directory]}]
  (try
    (let [days-val (coerce-int! days :days 7)
          directory (or directory (ctx/current-directory))]
      (log/info "mcp-memory-expiring-soon:" days-val "directory:" directory)
      (with-chroma
        (let [project-id (scope/get-current-project-id directory)
              ;; Fetch all expiring entries (don't filter by project-id in query
              ;; because we need scope tag filtering, not metadata field matching)
              all-entries (chroma/entries-expiring-soon days-val)
              ;; Apply scope filter using tag matching (same pattern as handle-query)
              scope-filter (when (and project-id (not= project-id "global"))
                             (scope/make-scope-tag project-id))
              filtered (if scope-filter
                         (filter #(scope/matches-scope? % scope-filter) all-entries)
                         all-entries)]
          {:type "text" :text (json/write-str (mapv fmt/entry->json-alist filtered))})))
    (catch clojure.lang.ExceptionInfo e
      (if (= :coercion-error (:type (ex-data e)))
        (mcp-error (.getMessage e))
        (throw e)))))
