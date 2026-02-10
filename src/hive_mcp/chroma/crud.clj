(ns hive-mcp.chroma.crud
  "Core CRUD operations for Chroma memory entries.

   Provides create, read, update, delete, batch operations,
   and collection statistics."
  (:require [clojure-chroma-client.api :as chroma]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.chroma.embeddings :as emb]
            [hive-mcp.chroma.helpers :as h]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Async Eviction
;;; ============================================================

(defn- evict-expired-async!
  "Fire-and-forget deletion of expired entry IDs from Chroma.
   Opportunistic lazy eviction: clean up expired entries discovered during reads.
   Non-blocking — errors are logged but never propagate to callers."
  [expired-ids]
  (when (seq expired-ids)
    (future
      (try
        (let [coll (conn/get-or-create-collection)]
          @(chroma/delete coll :ids (vec expired-ids))
          (log/info "Lazy eviction: deleted" (count expired-ids) "expired entries"))
        (catch Exception e
          (log/warn "Lazy eviction failed (non-blocking):" (.getMessage e)))))))

;;; ============================================================
;;; Create / Index
;;; ============================================================

(defn index-memory-entry!
  "Index a memory entry in Chroma (full storage, not just search).
   Entry should have :id, :content, :type, and optionally :tags, :created, etc.
   Returns the entry ID on success."
  [{:keys [id content type tags created updated duration expires
           content-hash access-count helpful-count unhelpful-count project-id
           kg-outgoing kg-incoming abstraction-level
           grounded-at grounded-from knowledge-gaps source-hash source-file
           staleness-alpha staleness-beta staleness-source staleness-depth]
    :as entry}]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        entry-id (or id (h/generate-id))
        now (h/iso-timestamp)
        doc-text (h/memory-to-document entry)
        embedding (emb/embed-text (emb/get-embedding-provider) doc-text)
        provided {:type type :tags (h/join-tags tags) :content (h/serialize-content content)
                  :content-hash content-hash :created (or created now) :updated (or updated now)
                  :duration duration :expires expires :access-count access-count
                  :helpful-count helpful-count :unhelpful-count unhelpful-count
                  :project-id project-id
                  :kg-outgoing (h/join-tags kg-outgoing)
                  :kg-incoming (h/join-tags kg-incoming)
                  :abstraction-level abstraction-level
                  :grounded-at grounded-at
                  :grounded-from grounded-from
                  :knowledge-gaps (h/join-tags knowledge-gaps)
                  :source-hash source-hash
                  :source-file source-file
                  :staleness-alpha staleness-alpha
                  :staleness-beta staleness-beta
                  :staleness-source (when staleness-source (name staleness-source))
                  :staleness-depth staleness-depth}
        meta (merge h/metadata-defaults (into {} (remove (comp nil? val) provided)))]
    @(chroma/add coll [{:id entry-id :embedding embedding :document doc-text :metadata meta}]
                 :upsert? true)
    (log/debug "Indexed memory entry:" entry-id)
    entry-id))

(defn index-memory-entries!
  "Index multiple memory entries in batch.
   More efficient than calling index-memory-entry! repeatedly."
  [entries]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        docs (mapv h/memory-to-document entries)
        embeddings (emb/embed-batch (emb/get-embedding-provider) docs)
        records (mapv (fn [entry doc emb-vec]
                        {:id (:id entry)
                         :embedding emb-vec
                         :document doc
                         :metadata {:type (:type entry)
                                    :tags (or (h/join-tags (:tags entry)) "")
                                    :created (or (:created entry) "")}})
                      entries docs embeddings)]
    @(chroma/add coll records :upsert? true)
    (log/info "Indexed" (count entries) "memory entries")
    (mapv :id entries)))

;;; ============================================================
;;; Read / Query
;;; ============================================================

(defn get-entry-by-id
  "Get a specific memory entry by ID from Chroma.
   Returns the full entry as a map, or nil if not found."
  [id]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        results @(chroma/get coll :ids [id] :include #{:documents :metadatas})]
    (some-> (first results) h/metadata->entry)))

(defn query-entries
  "Query memory entries from Chroma with filtering.
   Options:
     :type - Filter by type (note, snippet, convention, decision)
     :project-id - Filter by single project (backward compat)
     :project-ids - Filter by multiple projects using Chroma $in operator
     :limit - Max results (default: 100)
     :include-expired? - Include expired entries (default: false)
   Returns seq of entry maps."
  [& {:keys [type project-id project-ids limit include-expired?]
      :or {limit 100 include-expired? false}}]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        where-clause (cond-> {}
                       type (assoc :type type)
                       project-ids (assoc :project-id {:$in (vec project-ids)})
                       (and project-id (not project-ids)) (assoc :project-id project-id))
        where (when (seq where-clause) where-clause)
        ;; Over-fetch to compensate for expired entries that will be filtered out
        fetch-limit (if include-expired? limit (+ limit 50))
        results @(chroma/get coll
                             :where where
                             :include #{:documents :metadatas}
                             :limit fetch-limit)
        entries (map h/metadata->entry results)
        {expired true live false} (group-by #(boolean (h/expired? %)) entries)]
    ;; Lazy eviction: async-delete expired entries discovered during read
    (when-not include-expired?
      (evict-expired-async! (mapv :id expired)))
    (->> (if include-expired? entries live)
         (sort-by :created #(compare %2 %1))
         (take limit)
         vec)))

(defn query-grounded-from
  "Query Chroma for entries grounded from a specific disc path.
   Returns seq of entry maps."
  [disc-path]
  (emb/require-embedding!)
  (let [coll (conn/get-or-create-collection)
        results @(chroma/get coll
                             :where {:grounded-from disc-path}
                             :include #{:documents :metadatas})]
    (map h/metadata->entry results)))

;;; ============================================================
;;; Update
;;; ============================================================

(defn update-entry!
  "Update a memory entry in Chroma.
   ID is required. Updates only provided fields.
   Returns the updated entry."
  [id updates]
  (emb/require-embedding!)
  (when-let [existing (get-entry-by-id id)]
    (let [merged (merge existing updates {:updated (h/iso-timestamp)})
          _ (index-memory-entry! merged)]
      (get-entry-by-id id))))

(defn update-staleness!
  "Update staleness fields for a Chroma entry.
   Options: :alpha, :beta, :source (keyword), :depth (int).
   Returns updated entry."
  [entry-id {:keys [alpha beta source depth]}]
  (emb/require-embedding!)
  (let [updates (-> {}
                    (cond-> alpha (assoc :staleness-alpha alpha))
                    (cond-> beta (assoc :staleness-beta beta))
                    (cond-> source (assoc :staleness-source source))
                    (cond-> depth (assoc :staleness-depth depth)))]
    (update-entry! entry-id updates)))

;;; ============================================================
;;; Delete / Find / Stats
;;; ============================================================

(defn delete-entry!
  "Delete a memory entry from the Chroma index."
  [id]
  (let [coll (conn/get-or-create-collection)]
    @(chroma/delete coll :ids [id])
    (log/debug "Deleted entry from Chroma:" id)
    id))

(defn find-duplicate
  "Find entry with matching content-hash in the given type.
   Returns the existing entry or nil."
  [type content-hash & {:keys [project-id]}]
  (emb/require-embedding!)
  (let [entries (query-entries :type type :project-id project-id :limit 1000)]
    (first (filter #(= (:content-hash %) content-hash) entries))))

(defn collection-stats
  "Get statistics about the Chroma collection."
  []
  (try
    (let [coll (conn/get-or-create-collection)
          all-entries @(chroma/get coll :include [:metadatas])]
      {:count (count all-entries)
       :types (frequencies (map #(get-in % [:metadata :type]) all-entries))})
    (catch Exception e
      {:error (str e)})))
