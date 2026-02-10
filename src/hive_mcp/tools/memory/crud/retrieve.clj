(ns hive-mcp.tools.memory.crud.retrieve
  "Retrieval operations for memory: get-full, batch-get, check-duplicate, update-tags.

   SOLID: SRP - Handles only single-entry and batch retrieval operations.
   CLARITY: L - Layers stay pure with clear domain separation.

   Handlers:
   - handle-get-full: Get full entry by ID (with optional KG edges)
   - handle-batch-get: Get multiple entries by IDs
   - handle-check-duplicate: Check for existing content
   - handle-update-tags: Update tags on existing entry

   Knowledge Graph Integration:
   - get-full returns kg_outgoing and kg_incoming edges when present
   - batch-get includes KG edges per entry"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; KG Edge Formatting
;; ============================================================

(defn- edge->json-map
  "Convert KG edge to JSON-safe map format."
  [edge]
  (cond-> {:id (:kg-edge/id edge)
           :from (:kg-edge/from edge)
           :to (:kg-edge/to edge)
           :relation (name (:kg-edge/relation edge))
           :confidence (:kg-edge/confidence edge)
           :scope (:kg-edge/scope edge)
           :created_by (:kg-edge/created-by edge)
           :created_at (str (:kg-edge/created-at edge))}
    (:kg-edge/last-verified edge) (assoc :last_verified (str (:kg-edge/last-verified edge)))
    (:kg-edge/source-type edge) (assoc :source_type (name (:kg-edge/source-type edge)))))

(defn- get-kg-edges-for-entry
  "Get KG edges for a memory entry.
   Returns map with :outgoing and :incoming edge lists."
  [entry-id]
  (let [outgoing (kg-edges/get-edges-from entry-id)
        incoming (kg-edges/get-edges-to entry-id)]
    {:outgoing (mapv edge->json-map outgoing)
     :incoming (mapv edge->json-map incoming)}))

;; ============================================================
;; Get Full Handler (with KG Edge Inclusion)
;; ============================================================

(defn handle-get-full
  "Get full content of a memory entry by ID.
   Searches both hive-mcp-memory and hive-mcp-plans collections.
   Use after mcp_memory_query_metadata to fetch specific entries.

   Transparent fallback: If not found in memory collection, tries plans collection.
   This allows get-full to work regardless of which collection stores the entry.

   Knowledge Graph Integration:
   Automatically includes KG edges when present:
     - kg_outgoing: Edges where this entry is the source
     - kg_incoming: Edges where this entry is the target"
  [{:keys [id]}]
  (log/info "mcp-memory-get-full:" id)
  (with-chroma
    ;; Try memory collection first, then fall back to plans collection
    (if-let [entry (or (chroma/get-entry-by-id id)
                       (plans/get-plan id))]
      (let [base-result (fmt/entry->json-alist entry)
            ;; Include KG edges (graceful degradation if KG backend is broken)
            {:keys [outgoing incoming]}
            (try (get-kg-edges-for-entry id)
                 (catch Exception e
                   (log/warn "KG edge lookup failed for" id ":" (.getMessage e))
                   {:outgoing [] :incoming []}))
            result (cond-> base-result
                     (seq outgoing) (assoc :kg_outgoing outgoing)
                     (seq incoming) (assoc :kg_incoming incoming))]
        (mcp-json result))
      (mcp-json {:error "Entry not found" :id id}))))

;; ============================================================
;; Batch Get Handler
;; ============================================================

(defn handle-batch-get
  "Get full content of multiple memory entries by IDs in a single call.
   Returns all found entries with KG edges. Missing IDs reported in :missing."
  [{:keys [ids]}]
  (if (or (nil? ids) (empty? ids))
    (mcp-error "ids is required (array of memory entry ID strings)")
    (with-chroma
      (let [results (mapv (fn [id]
                            (if-let [entry (or (chroma/get-entry-by-id id)
                                               (plans/get-plan id))]
                              (let [base (fmt/entry->json-alist entry)
                                    {:keys [outgoing incoming]}
                                    (try (get-kg-edges-for-entry id)
                                         (catch Exception e
                                           (log/warn "KG edge lookup failed for" id ":" (.getMessage e))
                                           {:outgoing [] :incoming []}))]
                                (cond-> base
                                  (seq outgoing) (assoc :kg_outgoing outgoing)
                                  (seq incoming) (assoc :kg_incoming incoming)))
                              {:error "Entry not found" :id id}))
                          ids)
            found   (filterv #(not (:error %)) results)
            missing (filterv :error results)]
        (mcp-json (cond-> {:entries found :count (count found)}
                    (seq missing) (assoc :missing (mapv :id missing))))))))

;; ============================================================
;; Check Duplicate Handler
;; ============================================================

(defn handle-check-duplicate
  "Check if content already exists in memory (Chroma-only).

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer."
  [{:keys [type content directory]}]
  (log/info "mcp-memory-check-duplicate:" type "directory:" directory)
  (with-chroma
    (let [project-id (scope/get-current-project-id directory)
          hash (chroma/content-hash content)
          existing (chroma/find-duplicate type hash :project-id project-id)]
      (mcp-json {:exists (some? existing)
                 :entry (when existing (fmt/entry->json-alist existing))
                 :content_hash hash}))))

;; ============================================================
;; Update Tags Handler
;; ============================================================

(defn handle-update-tags
  "Update tags on an existing memory entry (Chroma-only).
   Replaces existing tags with the new tags list.
   Returns the updated entry or error if not found."
  [{:keys [id tags]}]
  (log/info "mcp-memory-update-tags:" id "tags:" tags)
  (with-chroma
    (if-let [_existing (chroma/get-entry-by-id id)]
      (let [updated (chroma/update-entry! id {:tags (or tags [])})]
        (log/info "Updated tags for entry:" id)
        (mcp-json (fmt/entry->json-alist updated)))
      (mcp-json {:error "Entry not found" :id id}))))
