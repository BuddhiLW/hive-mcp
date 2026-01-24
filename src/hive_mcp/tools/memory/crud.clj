(ns hive-mcp.tools.memory.crud
  "CRUD handlers for memory operations.

   SOLID: SRP - Single responsibility for create/read/update/delete.
   CLARITY: L - Layers stay pure with clear domain separation.

   Handlers:
   - add: Create new memory entry (with optional KG edge creation)
   - query: Query entries with filtering
   - query-metadata: Query returning metadata only
   - get-full: Get full entry by ID (with optional KG edges)
   - check-duplicate: Check for existing content

   Knowledge Graph Integration:
   - add supports kg_implements, kg_supersedes, kg_depends_on, kg_refines
   - get-full returns kg_outgoing and kg_incoming edges when present"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Add Handler
;; ============================================================

;; ============================================================
;; Knowledge Graph Edge Creation
;; ============================================================

(defn- update-target-incoming!
  "Update target entry's kg-incoming field with the new edge ID.
   Appends to existing incoming edges if present."
  [target-id edge-id]
  (when-let [target-entry (chroma/get-entry-by-id target-id)]
    (let [existing-incoming (or (:kg-incoming target-entry) [])
          updated-incoming (conj existing-incoming edge-id)]
      (chroma/update-entry! target-id {:kg-incoming updated-incoming}))))

(defn- create-kg-edges!
  "Create KG edges for the given relationships.

   Arguments:
     entry-id    - The newly created memory entry ID
     kg-params   - Map with :kg_implements, :kg_supersedes, :kg_depends_on, :kg_refines
     project-id  - Project scope for edge attribution
     agent-id    - Agent creating the edges (for attribution)

   Returns:
     Vector of created edge IDs

   Side effects:
     - Creates edges in DataScript KG store
     - Updates target entries' kg-incoming field in Chroma (bidirectional lookup)"
  [entry-id {:keys [kg_implements kg_supersedes kg_depends_on kg_refines]} project-id agent-id]
  (let [created-by (when agent-id (str "agent:" agent-id))
        create-edges (fn [targets relation]
                       (when (seq targets)
                         (mapv (fn [target-id]
                                 (let [edge-id (kg-edges/add-edge!
                                                {:from entry-id
                                                 :to target-id
                                                 :relation relation
                                                 :scope project-id
                                                 :confidence 1.0
                                                 :created-by created-by})]
                                   ;; Update target's kg-incoming for bidirectional lookup
                                   (update-target-incoming! target-id edge-id)
                                   edge-id))
                               targets)))]
    (vec (concat
          (create-edges kg_implements :implements)
          (create-edges kg_supersedes :supersedes)
          (create-edges kg_depends_on :depends-on)
          (create-edges kg_refines :refines)))))

(defn handle-add
  "Add an entry to project memory (Chroma-only storage).
   Stores full entry in Chroma with content, metadata, and embedding.

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue).

   When agent_id is provided (or CLAUDE_SWARM_SLAVE_ID env var is set),
   adds an 'agent:{id}' tag for tracking which ling created the entry.

   Knowledge Graph Integration:
   When kg_* parameters are provided, creates corresponding KG edges:
     - kg_implements: List of entry IDs this implements
     - kg_supersedes: List of entry IDs this supersedes
     - kg_depends_on: List of entry IDs this depends on
     - kg_refines: List of entry IDs this refines

   Edges are stored in DataScript with full provenance (scope, agent, timestamp)."
  [{:keys [type content tags duration directory agent_id
           kg_implements kg_supersedes kg_depends_on kg_refines] :as params}]
  (let [directory (or directory (ctx/current-directory))]
    (log/info "mcp-memory-add:" type "directory:" directory "agent_id:" agent_id)
    (with-chroma
      (let [project-id (scope/get-current-project-id directory)
            ;; Agent tagging for convergence tracking
            agent-id (or agent_id (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
            agent-tag (when agent-id (str "agent:" agent-id))
            base-tags (or tags [])
            tags-with-agent (if agent-tag (conj base-tags agent-tag) base-tags)
            ;; Add KG relationship metadata as tags for Chroma filtering
            kg-tags (cond-> []
                      (seq kg_implements) (conj "kg:has-implements")
                      (seq kg_supersedes) (conj "kg:has-supersedes")
                      (seq kg_depends_on) (conj "kg:has-depends-on")
                      (seq kg_refines) (conj "kg:has-refines"))
            tags-with-kg (into tags-with-agent kg-tags)
            tags-with-scope (scope/inject-project-scope tags-with-kg project-id)
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
            (mcp-json (fmt/entry->json-alist updated)))
          ;; Create new entry
          (let [entry-id (chroma/index-memory-entry!
                          {:type type
                           :content content
                           :tags tags-with-scope
                           :content-hash content-hash
                           :duration duration-str
                           :expires (or expires "")
                           :project-id project-id})
                ;; Create KG edges if any relationships specified
                edge-ids (create-kg-edges! entry-id params project-id agent-id)
                ;; Store outgoing edge IDs in Chroma for bidirectional lookup
                _ (when (seq edge-ids)
                    (chroma/update-entry! entry-id {:kg-outgoing edge-ids}))
                created (chroma/get-entry-by-id entry-id)]
            (log/info "Created memory entry:" entry-id
                      (when (seq edge-ids) (str " with " (count edge-ids) " KG edges")))
            (mcp-json (cond-> (fmt/entry->json-alist created)
                        (seq edge-ids) (assoc :kg_edges_created edge-ids)))))))))

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
     - specific scope tag: filter by that scope

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue)."
  [{:keys [type tags limit duration scope directory]}]
  (let [directory (or directory (ctx/current-directory))]
    (log/info "mcp-memory-query:" type "scope:" scope "directory:" directory)
    (with-chroma
      (let [project-id (scope/get-current-project-id directory)
            limit-val (or limit 20)
            ;; Query from Chroma
            entries (chroma/query-entries :type type
                                          :project-id (when (nil? scope) project-id)
                                          :limit (* limit-val 5)) ; Over-fetch for filtering
            ;; Apply hierarchical scope filter
            scope-filter (scope/derive-hierarchy-scope-filter scope)
            filtered (if scope-filter
                       (filter #(scope/matches-hierarchy-scopes? % scope-filter) entries)
                       entries)
            ;; Apply tag filter
            tag-filtered (apply-tag-filter filtered tags)
            ;; Apply duration filter
            dur-filtered (apply-duration-filter tag-filtered duration)
            ;; Apply limit
            results (take limit-val dur-filtered)]
        (mcp-json (mapv fmt/entry->json-alist results))))))

;; ============================================================
;; Query Metadata Handler
;; ============================================================

(defn handle-query-metadata
  "Query project memory by type, returning only metadata (Chroma-only).
   Returns id, type, preview, tags, created - ~10x fewer tokens than full query.
   Follow up with mcp_memory_get_full to fetch specific entries.

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue)."
  [{:keys [type tags limit scope directory]}]
  (let [directory (or directory (ctx/current-directory))]
    (log/info "mcp-memory-query-metadata:" type "scope:" scope "directory:" directory)
    (with-chroma
      (let [;; Reuse query logic - pass directory through
            {:keys [text isError]} (handle-query
                                    {:type type :tags tags :limit limit :scope scope :directory directory})]
        (if isError
          {:type "text" :text text :isError true}
          (let [entries (json/read-str text :key-fn keyword)
                metadata (mapv fmt/entry->metadata entries)]
            (mcp-json metadata)))))))

;; ============================================================
;; Get Full Handler (with KG Edge Inclusion)
;; ============================================================

(defn- edge->json-map
  "Convert KG edge to JSON-safe map format."
  [edge]
  {:id (:kg-edge/id edge)
   :from (:kg-edge/from edge)
   :to (:kg-edge/to edge)
   :relation (name (:kg-edge/relation edge))
   :confidence (:kg-edge/confidence edge)
   :scope (:kg-edge/scope edge)
   :created_by (:kg-edge/created-by edge)
   :created_at (str (:kg-edge/created-at edge))})

(defn- get-kg-edges-for-entry
  "Get KG edges for a memory entry.
   Returns map with :outgoing and :incoming edge lists."
  [entry-id]
  (let [outgoing (kg-edges/get-edges-from entry-id)
        incoming (kg-edges/get-edges-to entry-id)]
    {:outgoing (mapv edge->json-map outgoing)
     :incoming (mapv edge->json-map incoming)}))

(defn handle-get-full
  "Get full content of a memory entry by ID (Chroma-only).
   Use after mcp_memory_query_metadata to fetch specific entries.

   Knowledge Graph Integration:
   Automatically includes KG edges when present:
     - kg_outgoing: Edges where this entry is the source
     - kg_incoming: Edges where this entry is the target"
  [{:keys [id]}]
  (log/info "mcp-memory-get-full:" id)
  (with-chroma
    (if-let [entry (chroma/get-entry-by-id id)]
      (let [base-result (fmt/entry->json-alist entry)
            ;; Include KG edges
            {:keys [outgoing incoming]} (get-kg-edges-for-entry id)
            result (cond-> base-result
                     (seq outgoing) (assoc :kg_outgoing outgoing)
                     (seq incoming) (assoc :kg_incoming incoming))]
        (mcp-json result))
      (mcp-json {:error "Entry not found" :id id}))))

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
