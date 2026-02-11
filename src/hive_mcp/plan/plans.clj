(ns hive-mcp.plan.plans
  "Chroma vector database integration for plan memory entries.

   Plans are large memory entries (1000-5000+ chars) that exceed the
   Ollama embedding limit (~1500 chars). They get their own Chroma
   collection backed by OpenRouter embeddings (4096 dims, ~8K+ context).

   Architecture:
   ```
   hive-mcp-memory  -> Ollama (768 dims, ~1500 char, free)
   hive-mcp-plans   -> OpenRouter (4096 dims, ~8K+ context, cheap)
   hive-mcp-presets -> OpenRouter (4096 dims, already working)
   ```

   Collection Schema:
     id: memory entry ID (timestamp-based, same as hive-mcp-memory)
     content: full plan text (can be 1000-5000+ chars)
     metadata:
       - type: always 'plan'
       - tags: comma-separated tags
       - project-id: project scope
       - duration: TTL category
       - expires: ISO timestamp
       - content-hash: SHA256 of content
       - steps-count: number of plan steps
       - decision-id: linked decision entry ID (optional)
       - wave-count: number of wave dispatches planned (optional)
       - plan-status: draft | active | completed | superseded
       - abstraction-level: always 4 (Intent level)

   Usage:
     ;; Index a plan
     (index-plan! {:type \"plan\" :content \"...\" :tags [...] :project-id \"hive-mcp\"})

     ;; Semantic search
     (search-plans \"authentication refactoring\" :limit 3)

     ;; Get specific plan
     (get-plan \"20260206110839-4b3dadd6\")

     ;; Query plans by project
     (query-plans :project-id \"hive-mcp\" :limit 10)"
  (:require [hive-mcp.chroma :as chroma]
            [clojure-chroma-client.api :as chroma-api]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(def ^:private collection-name "hive-mcp-plans")

(def ^:private valid-plan-statuses
  "Valid plan-status values."
  #{"draft" "active" "completed" "superseded"})


(defonce ^:private collection-cache (atom nil))

(defn- try-get-existing-collection
  "Try to get existing collection. Returns nil on failure."
  []
  (try
    @(chroma-api/get-collection collection-name)
    (catch Exception _ nil)))

(defn- delete-collection!
  "Delete the plans collection. Returns true on success."
  []
  (try
    (when-let [coll (try-get-existing-collection)]
      @(chroma-api/delete-collection coll)
      (Thread/sleep 50))
    true
    (catch Exception _ false)))

(defn- create-collection-with-dimension
  "Create a new collection with the given dimension.
   Returns fresh collection reference to avoid stale cache issues."
  [dim]
  ;; Verify collection doesn't exist before creating
  (when-let [_stale (try-get-existing-collection)]
    (log/warn "Stale plans collection found after delete, forcing re-delete")
    (delete-collection!))
  ;; Create the collection
  @(chroma-api/create-collection
    collection-name
    {:metadata {:dimension dim
                :created-by "hive-mcp"
                :purpose "plan-memory-entries"}})
  ;; Get fresh reference
  (Thread/sleep 50)
  (or (try-get-existing-collection)
      (throw (ex-info "Failed to get plans collection after creation"
                      {:collection collection-name :dimension dim}))))

(defn- get-or-create-collection
  "Get existing plans collection or create new one.

   Flex Embedding Dimensions:
   If the existing collection's dimension doesn't match the current provider,
   the collection is automatically recreated with the correct dimension.
   This handles provider switches (e.g., Ollama 768 -> OpenRouter 4096).

   COLLECTION-AWARE: Uses chroma/get-provider-for to get the provider
   configured specifically for this collection (if any), falling back
   to global provider."
  []
  (if-let [coll @collection-cache]
    coll
    (let [provider (chroma/get-provider-for collection-name)]
      (when-not provider
        (throw (ex-info "Embedding provider not configured for plans collection."
                        {:type :no-embedding-provider
                         :collection collection-name})))
      (let [required-dim (chroma/embedding-dimension provider)
            existing (try-get-existing-collection)]
        (if existing
          ;; Check dimension match
          (let [existing-dim (get-in existing [:metadata :dimension])]
            (if (= existing-dim required-dim)
              ;; Dimension matches - reuse existing collection
              (do
                (reset! collection-cache existing)
                (log/info "Using existing plans collection:" collection-name "dimension:" existing-dim)
                existing)
              ;; Dimension mismatch - recreate collection!
              (do
                (log/warn "Plans embedding dimension changed:" existing-dim "->" required-dim ". Recreating collection.")
                (delete-collection!)
                (reset! collection-cache nil)
                (let [new-coll (create-collection-with-dimension required-dim)]
                  (reset! collection-cache new-coll)
                  (log/info "Recreated plans collection:" collection-name "dimension:" required-dim)
                  new-coll))))
          ;; No existing collection - create new
          (let [new-coll (create-collection-with-dimension required-dim)]
            (reset! collection-cache new-coll)
            (log/info "Created plans collection:" collection-name "dimension:" required-dim)
            new-coll))))))

(defn reset-collection-cache!
  "Reset the collection cache. For testing."
  []
  (reset! collection-cache nil))


(defn- plan-to-document
  "Convert plan entry to searchable document string.
   Includes structured metadata header for better semantic search."
  [{:keys [type content tags project-id plan-status steps-count decision-id]}]
  (str "Plan Entry [" (or plan-status "draft") "]\n"
       "Type: " (or type "plan") "\n"
       "Project: " (or project-id "unknown") "\n"
       (when steps-count (str "Steps: " steps-count "\n"))
       (when decision-id (str "Decision: " decision-id "\n"))
       "Tags: " (if (sequential? tags) (str/join ", " tags) (or tags "")) "\n\n"
       content))


(defn- count-plan-steps
  "Heuristic to count steps in plan content.
   Looks for numbered lists, ## Step headers, etc."
  [content]
  (when content
    (let [numbered (count (re-seq #"(?m)^\s*\d+[\.\)]\s+" content))
          step-headers (count (re-seq #"(?m)^#+\s*[Ss]tep\s+\d+" content))]
      (max numbered step-headers))))

(defn- extract-plan-metadata
  "Extract plan-specific metadata from entry map.
   Returns metadata map suitable for Chroma storage."
  [{:keys [type content tags project-id duration expires content-hash
           steps-count decision-id wave-count plan-status
           abstraction-level knowledge-gaps agent-id]}]
  (let [auto-steps (or steps-count (count-plan-steps content))]
    (cond-> {:type (or type "plan")
             :tags (if (sequential? tags) (str/join "," tags) (or tags ""))
             :project-id (or project-id "")
             :duration (or duration "long")
             :expires (or expires "")
             :content-hash (or content-hash "")
             :plan-status (or plan-status "draft")
             :abstraction-level (or abstraction-level 4)}
      auto-steps        (assoc :steps-count (str auto-steps))
      decision-id       (assoc :decision-id decision-id)
      wave-count        (assoc :wave-count (str wave-count))
      agent-id          (assoc :agent-id agent-id)
      knowledge-gaps    (assoc :knowledge-gaps
                               (if (sequential? knowledge-gaps)
                                 (str/join "|" knowledge-gaps)
                                 (or knowledge-gaps ""))))))


(defn index-plan!
  "Index a plan entry in the plans Chroma collection.

   Entry map keys:
     :type       - Always 'plan' (auto-set if missing)
     :content    - Full plan text (can be 1000-5000+ chars)
     :tags       - Vector of tag strings
     :project-id - Project scope
     :duration   - TTL category (default: 'long')
     :expires    - ISO timestamp for expiration
     :content-hash - SHA256 of content
     :steps-count  - Number of plan steps (auto-detected if nil)
     :decision-id  - Linked decision entry ID (optional)
     :wave-count   - Number of wave dispatches planned (optional)
     :plan-status  - draft | active | completed | superseded (default: draft)
     :id           - Pre-generated ID (optional, auto-generated if nil)

   Returns: entry ID string on success.

   COLLECTION-AWARE: Uses collection-specific embedding provider."
  [{:keys [id content] :as entry}]
  (try
    (let [coll (get-or-create-collection)
          provider (chroma/get-provider-for collection-name)
          entry-id (or id (str (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")
                               "-" (subs (str (java.util.UUID/randomUUID)) 0 8)))
          doc-text (plan-to-document entry)
          embedding (chroma/embed-text provider doc-text)
          metadata (extract-plan-metadata entry)]
      @(chroma-api/add coll [{:id entry-id
                              :embedding embedding
                              :document doc-text
                              :metadata metadata}]
                       :upsert? true)
      (log/info "Indexed plan:" entry-id "status:" (:plan-status metadata)
                "steps:" (:steps-count metadata))
      entry-id)
    (catch Exception e
      (log/error "Failed to index plan:" (.getMessage e))
      (throw (ex-info (str "Plan indexing failed: " (.getMessage e))
                      {:type :plan-index-error
                       :content-length (count (str content))})))))


(defn search-plans
  "Search plans using semantic similarity.

   Options:
     :limit       - Max results (default: 5)
     :project-id  - Filter by project
     :plan-status - Filter by status (draft, active, completed, superseded)

   COLLECTION-AWARE: Uses collection-specific embedding provider.

   Returns seq of {:id, :type, :tags, :project-id, :plan-status, :distance, :preview}"
  [query-text & {:keys [limit project-id plan-status] :or {limit 5}}]
  (let [coll (get-or-create-collection)
        provider (chroma/get-provider-for collection-name)
        query-embedding (chroma/embed-text provider query-text)
        where-clause (cond-> nil
                       project-id (assoc :project-id project-id)
                       plan-status (assoc :plan-status plan-status))
        results @(chroma-api/query coll query-embedding
                                   :num-results limit
                                   :where where-clause
                                   :include #{:documents :metadatas :distances})]
    (log/debug "Plan search for:" (subs query-text 0 (min 50 (count query-text)))
               "found:" (count results))
    (mapv (fn [{:keys [id document metadata distance]}]
            {:id id
             :type (get metadata :type "plan")
             :tags (when-let [t (get metadata :tags)]
                     (when (not= t "")
                       (str/split t #",")))
             :project-id (get metadata :project-id)
             :plan-status (get metadata :plan-status)
             :steps-count (when-let [s (get metadata :steps-count)]
                            (when (not= s "") (parse-long s)))
             :decision-id (get metadata :decision-id)
             :distance distance
             :preview (when document
                        (subs document 0 (min 300 (count document))))})
          results)))


(defn get-plan
  "Get a specific plan by ID from the plans collection.
   Returns full plan map or nil if not found."
  [plan-id]
  (try
    (let [coll (get-or-create-collection)
          results @(chroma-api/get coll :ids [plan-id] :include #{:documents :metadatas})]
      (when-let [{:keys [id document metadata]} (first results)]
        {:id id
         :type (get metadata :type "plan")
         :content document
         :tags (when-let [t (get metadata :tags)]
                 (when (not= t "")
                   (str/split t #",")))
         :project-id (get metadata :project-id)
         :duration (get metadata :duration)
         :expires (get metadata :expires)
         :plan-status (get metadata :plan-status)
         :steps-count (when-let [s (get metadata :steps-count)]
                        (when (not= s "") (parse-long s)))
         :decision-id (get metadata :decision-id)
         :wave-count (when-let [w (get metadata :wave-count)]
                       (when (not= w "") (parse-long w)))
         :abstraction-level (when-let [a (get metadata :abstraction-level)]
                              (if (string? a) (parse-long a) a))
         :content-hash (get metadata :content-hash)}))
    (catch Exception e
      (log/debug "Failed to get plan from Chroma:" plan-id (.getMessage e))
      nil)))


(defn query-plans
  "Query plans with metadata filtering.

   Options:
     :project-id  - Filter by project
     :plan-status - Filter by status
     :limit       - Max results (default: 20)
     :tags        - Filter by tags (entries must contain ALL tags)

   Returns seq of plan maps (without full content for efficiency)."
  [& {:keys [project-id plan-status limit tags] :or {limit 20}}]
  (try
    (let [coll (get-or-create-collection)
          where-clause (cond-> {:type "plan"}
                         project-id (assoc :project-id project-id)
                         plan-status (assoc :plan-status plan-status))
          results @(chroma-api/get coll
                                   :where where-clause
                                   :include #{:metadatas :documents}
                                   :limit limit)]
      (->> results
           (map (fn [{:keys [id metadata document]}]
                  {:id id
                   :type (get metadata :type "plan")
                   :tags (when-let [t (get metadata :tags)]
                           (when (not= t "")
                             (str/split t #",")))
                   :project-id (get metadata :project-id)
                   :plan-status (get metadata :plan-status)
                   :steps-count (when-let [s (get metadata :steps-count)]
                                  (when (not= s "") (parse-long s)))
                   :decision-id (get metadata :decision-id)
                   :duration (get metadata :duration)
                   :preview (when document
                              (subs document 0 (min 200 (count document))))}))
           ;; Apply tag filter in memory (Chroma where doesn't support substring matching on tags)
           (filter (fn [entry]
                     (if (seq tags)
                       (let [entry-tags (set (:tags entry))]
                         (every? #(contains? entry-tags %) tags))
                       true)))
           (take limit)
           (vec)))
    (catch Exception e
      (log/warn "Failed to query plans:" (.getMessage e))
      [])))


(defn update-plan-status!
  "Update the status of a plan.
   Valid statuses: draft, active, completed, superseded."
  [plan-id new-status]
  (when-not (contains? valid-plan-statuses new-status)
    (throw (ex-info (str "Invalid plan status: " new-status
                         ". Valid: " (str/join ", " valid-plan-statuses))
                    {:type :invalid-plan-status
                     :status new-status})))
  (try
    (let [coll (get-or-create-collection)]
      @(chroma-api/update coll [{:id plan-id
                                 :metadata {:plan-status new-status}}])
      (log/info "Updated plan status:" plan-id "->" new-status)
      plan-id)
    (catch Exception e
      (log/error "Failed to update plan status:" (.getMessage e))
      nil)))


(defn delete-plan!
  "Delete a plan from the plans collection."
  [plan-id]
  (let [coll (get-or-create-collection)]
    @(chroma-api/delete coll :ids [plan-id])
    (log/debug "Deleted plan from Chroma:" plan-id)
    plan-id))


(defn status
  "Get plans collection integration status."
  []
  (let [base {:collection collection-name
              :chroma-configured? (chroma/embedding-configured?)}]
    (if (chroma/embedding-configured?)
      (try
        (let [plans (query-plans)]
          (assoc base
                 :count (count plans)
                 :statuses (frequencies (map :plan-status plans))
                 :projects (frequencies (map :project-id plans))))
        (catch Exception e
          (assoc base :error (str e))))
      base)))
