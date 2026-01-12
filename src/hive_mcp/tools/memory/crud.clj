(ns hive-mcp.tools.memory.crud
  "CRUD handlers for memory operations.

   SOLID: SRP - Single responsibility for create/read/update/delete.
   CLARITY: L - Layers stay pure with clear domain separation.

   Handlers:
   - add: Create new memory entry
   - query: Query entries with filtering
   - query-metadata: Query returning metadata only
   - get-full: Get full entry by ID
   - check-duplicate: Check for existing content"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; ============================================================
;; Add Handler
;; ============================================================

(defn handle-add
  "Add an entry to project memory (Chroma-only storage).
   Stores full entry in Chroma with content, metadata, and embedding."
  [{:keys [type content tags duration]}]
  (log/info "mcp-memory-add:" type)
  (with-chroma
    (let [project-id (scope/get-current-project-id)
          tags-with-scope (scope/inject-project-scope (or tags []) project-id)
          content-hash (chroma/content-hash content)
          duration-str (or duration "long")
          expires (dur/calculate-expires duration-str)
          ;; Check for duplicate
          existing (chroma/find-duplicate type content-hash :project-id project-id)]
      (if existing
        ;; Duplicate found - merge tags and return existing
        (let [merged-tags (distinct (concat (:tags existing) tags-with-scope))
              updated (chroma/update-entry! (:id existing) {:tags merged-tags})]
          (log/info "Duplicate found, merged tags:" (:id existing))
          {:type "text" :text (json/write-str (fmt/entry->json-alist updated))})
        ;; Create new entry
        (let [entry-id (chroma/index-memory-entry!
                        {:type type
                         :content content
                         :tags tags-with-scope
                         :content-hash content-hash
                         :duration duration-str
                         :expires (or expires "")
                         :project-id project-id})
              created (chroma/get-entry-by-id entry-id)]
          (log/info "Created memory entry:" entry-id)
          {:type "text" :text (json/write-str (fmt/entry->json-alist created))})))))

;; ============================================================
;; Query Handler
;; ============================================================

(defn- apply-tag-filter
  "Filter entries by required tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

(defn- apply-duration-filter
  "Filter entries by duration."
  [entries duration]
  (if duration
    (filter #(= (:duration %) duration) entries)
    entries))

(defn handle-query
  "Query project memory by type with scope filtering (Chroma-only).
   SCOPE controls which memories are returned:
     - nil/omitted: auto-filter by current project + global
     - \"all\": return all entries regardless of scope
     - \"global\": return only scope:global entries
     - specific scope tag: filter by that scope"
  [{:keys [type tags limit duration scope]}]
  (log/info "mcp-memory-query:" type "scope:" scope)
  (with-chroma
    (let [project-id (scope/get-current-project-id)
          limit-val (or limit 20)
          ;; Query from Chroma
          entries (chroma/query-entries :type type
                                        :project-id (when (nil? scope) project-id)
                                        :limit (* limit-val 5)) ; Over-fetch for filtering
          ;; Apply scope filter
          scope-filter (scope/derive-scope-filter scope project-id)
          filtered (if scope-filter
                     (filter #(scope/matches-scope? % scope-filter) entries)
                     entries)
          ;; Apply tag filter
          tag-filtered (apply-tag-filter filtered tags)
          ;; Apply duration filter
          dur-filtered (apply-duration-filter tag-filtered duration)
          ;; Apply limit
          results (take limit-val dur-filtered)]
      {:type "text" :text (json/write-str (mapv fmt/entry->json-alist results))})))

;; ============================================================
;; Query Metadata Handler
;; ============================================================

(defn handle-query-metadata
  "Query project memory by type, returning only metadata (Chroma-only).
   Returns id, type, preview, tags, created - ~10x fewer tokens than full query.
   Follow up with mcp_memory_get_full to fetch specific entries."
  [{:keys [type tags limit scope]}]
  (log/info "mcp-memory-query-metadata:" type "scope:" scope)
  (with-chroma
    (let [;; Reuse query logic
          {:keys [text isError]} (handle-query
                                  {:type type :tags tags :limit limit :scope scope})]
      (if isError
        {:type "text" :text text :isError true}
        (let [entries (json/read-str text :key-fn keyword)
              metadata (mapv fmt/entry->metadata entries)]
          {:type "text" :text (json/write-str metadata)})))))

;; ============================================================
;; Get Full Handler
;; ============================================================

(defn handle-get-full
  "Get full content of a memory entry by ID (Chroma-only).
   Use after mcp_memory_query_metadata to fetch specific entries."
  [{:keys [id]}]
  (log/info "mcp-memory-get-full:" id)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      {:type "text" :text (json/write-str (fmt/entry->json-alist entry))}
      {:type "text" :text (json/write-str {:error "Entry not found" :id id}) :isError true})))

;; ============================================================
;; Check Duplicate Handler
;; ============================================================

(defn handle-check-duplicate
  "Check if content already exists in memory (Chroma-only)."
  [{:keys [type content]}]
  (log/info "mcp-memory-check-duplicate:" type)
  (with-chroma
    (let [project-id (scope/get-current-project-id)
          hash (chroma/content-hash content)
          existing (chroma/find-duplicate type hash :project-id project-id)]
      {:type "text" :text (json/write-str {:exists (some? existing)
                                           :entry (when existing (fmt/entry->json-alist existing))
                                           :content_hash hash})})))
