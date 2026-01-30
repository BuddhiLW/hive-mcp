(ns hive-mcp.chroma
  "Chroma vector database integration for semantic memory search.
   
   Provides vector-based similarity search for hive-mcp memory entries.
   Uses clojure-chroma-client for Chroma DB operations.
   
   Embedding providers are pluggable - configure via `set-embedding-provider!`
   
   Configuration via environment variables:
     CHROMA_HOST - Chroma server host (required)
     CHROMA_PORT - Chroma server port (default: 8000)
     
   Or call (configure! {:host \"localhost\" :port 8000})
   
   Usage:
     ;; Initialize with embedding provider
     ;; For testing, use test-fixtures/->MockEmbedder
     
     ;; Index a memory entry
     (index-memory-entry! {:id \"123\" :content \"My note\" :type \"note\"})
     
     ;; Semantic search
     (search-similar \"find notes about Clojure\" :limit 5)"
  (:require [clojure-chroma-client.api :as chroma]
            [clojure-chroma-client.config :as chroma-config]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

(defn answer [] 42)

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Configuration
;;; ============================================================

(def ^:private default-config
  {:host "localhost"
   :port 8000
   :collection-name "hive-mcp-memory"})

(defonce ^:private config (atom default-config))

(defn configure!
  "Configure Chroma connection settings.
   
   Options:
     :host - Chroma server host (default: localhost)
     :port - Chroma server port (default: 8000)
     :collection-name - Collection for memory entries (default: hive-mcp-memory)
   
   Note: For Chroma Cloud, set CHROMA_API_KEY, CHROMA_TENANT, CHROMA_DATABASE env vars."
  [opts]
  (swap! config merge opts)
  ;; Use library's configure function (fixed in our local fork)
  (chroma-config/configure (select-keys opts [:host :port :api-version :tenant :database]))
  (log/info "Chroma configured:" (select-keys @config [:host :port :collection-name])))

;;; ============================================================
;;; Embedding Provider Protocol
;;; ============================================================

(defprotocol EmbeddingProvider
  "Protocol for generating text embeddings.
   Implement this to add support for different embedding services."
  (embed-text [this text]
    "Generate embedding vector for text. Returns vector of floats.")
  (embed-batch [this texts]
    "Generate embeddings for multiple texts. Returns seq of vectors.")
  (embedding-dimension [this]
    "Return the dimension of embeddings produced by this provider."))

;; Current provider (nil means not configured)
(defonce ^:private embedding-provider (atom nil))

(defn set-embedding-provider!
  "Set the embedding provider to use for vectorization.
   Provider must implement EmbeddingProvider protocol."
  [provider]
  (reset! embedding-provider provider)
  (log/info "Embedding provider set:" (type provider)))

(defn embedding-configured?
  "Check if an embedding provider is configured."
  []
  (some? @embedding-provider))

(defn get-embedding-provider
  "Get the current embedding provider. Returns nil if not configured."
  []
  @embedding-provider)

(defn reset-embedding-provider!
  "Reset the embedding provider atom. Use when hot-reload causes protocol mismatch.

   Hot-reload can cause 'No implementation of method' errors when:
   1. Protocol is redefined but provider instance was created with old protocol
   2. Namespace load order changes during development

   Fix: Call this, then set-embedding-provider! with a fresh instance.

   Example:
     (reset-embedding-provider!)
     (set-embedding-provider! (ollama/->provider))"
  []
  (reset! embedding-provider nil)
  (log/info "Embedding provider reset (was:" (type @embedding-provider) ")"))

;;; ============================================================
;;; Collection-Aware Embedding API
;;; ============================================================
;; These functions delegate to the EmbeddingService for per-collection routing.
;; They require (embeddings.service/init!) to be called first.
;; For backward compatibility, they fall back to the global provider.

(defn get-provider-for
  "Get embedding provider for a specific collection.
   Delegates to EmbeddingService if initialized, otherwise returns global provider.

   This enables different collections to use different embedding dimensions:
   - hive-mcp-memory: Ollama (768 dims, fast)
   - hive-mcp-presets: OpenRouter (4096 dims, accurate)"
  [collection-name]
  (try
    (require 'hive-mcp.embeddings.service)
    (let [get-provider (resolve 'hive-mcp.embeddings.service/get-provider-for)]
      (get-provider collection-name))
    (catch Exception _
      ;; Fallback to global provider if service not initialized
      (or @embedding-provider
          (throw (ex-info "No embedding provider available" {:collection collection-name}))))))

(defn embed-text-for
  "Embed text using the provider configured for the collection.
   Falls back to global provider if collection not configured."
  [collection-name text]
  (let [provider (get-provider-for collection-name)]
    (embed-text provider text)))

(defn embed-batch-for
  "Embed multiple texts using the provider configured for the collection."
  [collection-name texts]
  (let [provider (get-provider-for collection-name)]
    (embed-batch provider texts)))

(defn get-dimension-for
  "Get embedding dimension for a collection's provider."
  [collection-name]
  (let [provider (get-provider-for collection-name)]
    (embedding-dimension provider)))

;;; ============================================================
;;; DRY Helpers (CLARITY: Composition over modification)
;;; ============================================================

(defn- require-embedding!
  "Guard: throws if embedding provider not configured.
   CLARITY: Inputs are guarded - validate at boundaries."
  []
  (when-not @embedding-provider
    (throw (ex-info "Embedding provider not configured. Call set-embedding-provider! first."
                    {:type :no-embedding-provider}))))

(defn- parse-zoned-datetime
  "Parse ISO string to ZonedDateTime, returns nil on failure.
   CLARITY: Yield safe failure."
  [s]
  (when (and (string? s) (seq s))
    (try
      (java.time.ZonedDateTime/parse s)
      (catch Exception _ nil))))

(defn- json-string?
  "Check if string looks like JSON (starts with { or [)."
  [s]
  (and s (or (str/starts-with? s "{") (str/starts-with? s "["))))

(defn- try-parse-json
  "Parse JSON string, return original on failure.
   CLARITY: Yield safe failure."
  [s]
  (if (json-string? s)
    (try (json/read-str s :key-fn keyword) (catch Exception _ s))
    s))

(defn- split-tags
  "Split comma-separated tags string into vector."
  [tags-str]
  (when (seq tags-str)
    (str/split tags-str #",")))

(defn- serialize-content
  "Serialize content to string for storage."
  [content]
  (if (string? content) content (json/write-str content)))

(defn- join-tags
  "Join tags vector to comma-separated string."
  [tags]
  (when (seq tags) (str/join "," tags)))

(defn- metadata->entry
  "Convert Chroma metadata format to domain entry map.
   DRY: Single conversion point for all entry retrieval.

   Knowledge Graph fields (kg-outgoing, kg-incoming) store edge IDs
   as comma-separated strings for bidirectional lookup.

   Grounding fields track abstraction level and verification status
   per Korzybski's Structural Differential model.

   Staleness fields track Bayesian freshness evidence for L1 Phase 2
   transitive staleness propagation."
  [{:keys [id document metadata]}]
  {:id id
   :type (:type metadata)
   :content (try-parse-json (:content metadata))
   :tags (split-tags (:tags metadata))
   :content-hash (:content-hash metadata)
   :created (:created metadata)
   :updated (:updated metadata)
   :duration (:duration metadata)
   :expires (:expires metadata)
   :access-count (:access-count metadata)
   :helpful-count (:helpful-count metadata)
   :unhelpful-count (:unhelpful-count metadata)
   :project-id (:project-id metadata)
   ;; Knowledge Graph edge references for bidirectional lookup
   :kg-outgoing (split-tags (:kg-outgoing metadata))
   :kg-incoming (split-tags (:kg-incoming metadata))
   ;; Knowledge abstraction and grounding fields
   :abstraction-level (:abstraction-level metadata)
   :grounded-at (:grounded-at metadata)
   :grounded-from (:grounded-from metadata)
   :knowledge-gaps (split-tags (:knowledge-gaps metadata))
   :source-hash (:source-hash metadata)
   :source-file (:source-file metadata)
   ;; Staleness tracking (Bayesian Beta model for L1 Phase 2)
   :staleness-alpha (:staleness-alpha metadata)
   :staleness-beta (:staleness-beta metadata)
   :staleness-source (when-let [s (:staleness-source metadata)]
                       (if (string? s) (keyword s) s))
   :staleness-depth (:staleness-depth metadata)
   :document document})

;;; ============================================================
;;; Collection Management
;;; ============================================================

(defonce ^:private collection-cache (atom nil))

(defn- try-get-collection
  "Try to get existing collection, returns nil on failure."
  [coll-name]
  (try @(chroma/get-collection coll-name)
       (catch Exception _ nil)))

(defn- create-new-collection
  "Create a new Chroma collection with dimension metadata."
  [coll-name dim]
  @(chroma/create-collection coll-name {:metadata {:dimension dim :created-by "hive-mcp"}}))

(defn- cache-collection!
  "Cache and return collection, logging action."
  [coll log-msg]
  (reset! collection-cache coll)
  (log/info log-msg)
  coll)

(defn- get-or-create-collection
  "Get existing collection or create new one."
  []
  (or @collection-cache
      (let [coll-name (:collection-name @config)
            _ (require-embedding!)
            dim (embedding-dimension @embedding-provider)]
        (if-let [existing (try-get-collection coll-name)]
          (cache-collection! existing (str "Using existing Chroma collection: " coll-name))
          (cache-collection! (create-new-collection coll-name dim)
                             (str "Created Chroma collection: " coll-name " dimension: " dim))))))

(defn reset-collection-cache!
  "Reset the collection cache (for testing/reconnection)."
  []
  (reset! collection-cache nil))

;;; ============================================================
;;; Memory Indexing & Full CRUD (Chroma as Source of Truth)
;;; ============================================================

(defn- memory-to-document
  "Convert memory entry to searchable document string."
  [{:keys [content type tags]}]
  (str "Type: " type "\n"
       (when-let [t (join-tags tags)] (str "Tags: " (str/replace t "," ", ") "\n"))
       "Content: " (serialize-content content)))

(defn- generate-id
  "Generate a unique ID for memory entries (timestamp + random hex)."
  []
  (let [ts (java.time.LocalDateTime/now)
        fmt (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")
        random-hex (format "%08x" (rand-int Integer/MAX_VALUE))]
    (str (.format ts fmt) "-" random-hex)))

(defn- iso-timestamp
  "Return current ISO 8601 timestamp."
  []
  (str (java.time.ZonedDateTime/now
        (java.time.ZoneId/systemDefault))))

(defn- expired?
  "Check if an entry has expired based on :expires metadata.
   CLARITY: Yield safe failure - returns false on parse errors."
  [metadata]
  (when-let [expires-instant (parse-zoned-datetime (:expires metadata))]
    (.isBefore expires-instant (java.time.ZonedDateTime/now))))

(def ^:private metadata-defaults
  "Default values for entry metadata fields."
  {:type "note" :tags "" :content-hash "" :duration "long-term"
   :expires "" :access-count 0 :helpful-count 0 :unhelpful-count 0
   :project-id "global"
   ;; Knowledge Graph edge references (empty = no edges)
   :kg-outgoing "" :kg-incoming ""
   ;; Knowledge abstraction and grounding fields (Korzybski Structural Differential)
   :abstraction-level nil      ; Integer 1-4: L1=Disc, L2=Semantic, L3=Pattern, L4=Intent
   :grounded-at ""             ; ISO timestamp of last verification
   :grounded-from ""           ; Disc entity ID verified against
   :knowledge-gaps ""          ; Comma-separated gap keywords
   :source-hash ""             ; Content hash when abstracted (for drift detection)
   :source-file ""
   ;; Staleness tracking for L1 Phase 2 transitive propagation (Bayesian Beta model)
   :staleness-alpha 1          ; Bayesian α parameter (positive evidence / freshness)
   :staleness-beta 1           ; Bayesian β parameter (negative evidence / staleness)
   :staleness-source nil       ; Source: :hash-mismatch, :git-commit, :time-decay, :transitive
   :staleness-depth 0})

(defn index-memory-entry!
  "Index a memory entry in Chroma (full storage, not just search).
   Entry should have :id, :content, :type, and optionally :tags, :created, etc.
   Returns the entry ID on success.

   Full metadata stored: type, tags, content, content-hash, created, updated,
   duration, expires, access-count, helpful-count, unhelpful-count, project-id,
   kg-outgoing, kg-incoming, abstraction-level, grounded-at, grounded-from,
   knowledge-gaps, source-hash, source-file, staleness-alpha, staleness-beta,
   staleness-source, staleness-depth.

   Note: Tags, KG edge IDs, and knowledge-gaps are stored as comma-separated
   strings since Chroma metadata only supports scalar values (string, int, float, bool).
   Keywords (like staleness-source) are stored as strings."
  [{:keys [id content type tags created updated duration expires
           content-hash access-count helpful-count unhelpful-count project-id
           kg-outgoing kg-incoming abstraction-level
           grounded-at grounded-from knowledge-gaps source-hash source-file
           staleness-alpha staleness-beta staleness-source staleness-depth]
    :as entry}]
  (require-embedding!)
  (let [coll (get-or-create-collection)
        entry-id (or id (generate-id))
        now (iso-timestamp)
        doc-text (memory-to-document entry)
        embedding (embed-text @embedding-provider doc-text)
        provided {:type type :tags (join-tags tags) :content (serialize-content content)
                  :content-hash content-hash :created (or created now) :updated (or updated now)
                  :duration duration :expires expires :access-count access-count
                  :helpful-count helpful-count :unhelpful-count unhelpful-count
                  :project-id project-id
                  ;; Knowledge Graph edge references (stored as comma-separated IDs)
                  :kg-outgoing (join-tags kg-outgoing)
                  :kg-incoming (join-tags kg-incoming)
                  ;; Knowledge abstraction and grounding fields (Korzybski Structural Differential)
                  :abstraction-level abstraction-level
                  :grounded-at grounded-at
                  :grounded-from grounded-from
                  :knowledge-gaps (join-tags knowledge-gaps)
                  :source-hash source-hash
                  :source-file source-file
                  ;; Staleness tracking (Bayesian Beta model for L1 Phase 2)
                  :staleness-alpha staleness-alpha
                  :staleness-beta staleness-beta
                  :staleness-source (when staleness-source (name staleness-source))
                  :staleness-depth staleness-depth}
        meta (merge metadata-defaults (into {} (remove (comp nil? val) provided)))]
    @(chroma/add coll [{:id entry-id :embedding embedding :document doc-text :metadata meta}]
                 :upsert? true)
    (log/debug "Indexed memory entry:" entry-id)
    entry-id))

(defn get-entry-by-id
  "Get a specific memory entry by ID from Chroma.
   Returns the full entry as a map with :id, :content, :type, :tags, etc.
   Returns nil if not found."
  [id]
  (require-embedding!)
  (let [coll (get-or-create-collection)
        results @(chroma/get coll :ids [id] :include #{:documents :metadatas})]
    (some-> (first results) metadata->entry)))

(defn query-entries
  "Query memory entries from Chroma with filtering.
   Options:
     :type - Filter by type (note, snippet, convention, decision)
     :project-id - Filter by project
     :limit - Max results (default: 100)
     :include-expired? - Include expired entries (default: false)

   Returns seq of entry maps."
  [& {:keys [type project-id limit include-expired?]
      :or {limit 100 include-expired? false}}]
  (require-embedding!)
  (let [coll (get-or-create-collection)
        where-clause (cond-> {}
                       type (assoc :type type)
                       project-id (assoc :project-id project-id))
        where (when (seq where-clause) where-clause)
        results @(chroma/get coll
                             :where where
                             :include #{:documents :metadatas}
                             :limit limit)]
    (->> results
         (map metadata->entry)
         (filter #(or include-expired? (not (expired? %))))
         (sort-by :created #(compare %2 %1))
         (take limit)
         vec)))

(defn query-grounded-from
  "Query Chroma for entries grounded from a specific disc path.
   Returns seq of entry maps."
  [disc-path]
  (require-embedding!)
  (let [coll (get-or-create-collection)
        results @(chroma/get coll
                             :where {:grounded-from disc-path}
                             :include #{:documents :metadatas})]
    (map metadata->entry results)))

(defn update-entry!
  "Update a memory entry in Chroma.
   ID is required. Updates only provided fields.
   Returns the updated entry."
  [id updates]
  (require-embedding!)
  (when-let [existing (get-entry-by-id id)]
    (let [merged (merge existing updates {:updated (iso-timestamp)})
          ;; Re-index with merged data
          _ (index-memory-entry! merged)]
      (get-entry-by-id id))))

(defn update-staleness!
  "Update staleness fields for a Chroma entry.
   Options: :alpha (float), :beta (float), :source (keyword like :hash-mismatch), :depth (int).
   Returns updated entry."
  [entry-id {:keys [alpha beta source depth]}]
  (require-embedding!)
  (let [updates (-> {}
                    (cond-> alpha (assoc :staleness-alpha alpha))
                    (cond-> beta (assoc :staleness-beta beta))
                    (cond-> source (assoc :staleness-source source))
                    (cond-> depth (assoc :staleness-depth depth)))]
    (update-entry! entry-id updates)))

(defn find-duplicate
  "Find entry with matching content-hash in the given type.
   Returns the existing entry or nil."
  [type content-hash & {:keys [project-id]}]
  (require-embedding!)
  (let [entries (query-entries :type type :project-id project-id :limit 1000)]
    (first (filter #(= (:content-hash %) content-hash) entries))))

(defn- normalize-whitespace
  "Normalize whitespace for hashing: trim, collapse spaces and newlines."
  [s]
  (-> s str/trim (str/replace #"[ \t]+" " ") (str/replace #"\n+" "\n")))

(defn content-hash
  "Compute SHA-256 hash of content for deduplication.
   Normalizes content (trim, collapse whitespace) before hashing."
  [content]
  (let [normalized (normalize-whitespace (serialize-content content))
        md (java.security.MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest md (.getBytes normalized "UTF-8"))]
    (apply str (map #(format "%02x" %) hash-bytes))))

(defn cleanup-expired!
  "Delete all expired entries from Chroma.
   Returns map with :count and :deleted-ids for KG edge cleanup."
  []
  (require-embedding!)
  (let [coll (get-or-create-collection)
        ;; Get all entries including expired
        all-entries @(chroma/get coll :include #{:metadatas} :limit 10000)
        expired-ids (->> all-entries
                         (filter #(expired? (:metadata %)))
                         (map :id)
                         vec)]
    (when (seq expired-ids)
      @(chroma/delete coll :ids expired-ids)
      (log/info "Cleaned up" (count expired-ids) "expired entries"))
    {:count (count expired-ids)
     :deleted-ids expired-ids}))

(defn- time-between?
  "Check if time is between start (exclusive) and end (exclusive)."
  [time start end]
  (and (.isAfter time start) (.isBefore time end)))

(defn entries-expiring-soon
  "Get entries expiring within the given number of days.
   Returns seq of entry maps."
  [days & {:keys [project-id]}]
  (require-embedding!)
  (let [now (java.time.ZonedDateTime/now)
        threshold (.plusDays now days)
        entries (query-entries :project-id project-id :limit 10000 :include-expired? false)]
    (->> entries
         (filter (fn [entry]
                   (when-let [exp-time (parse-zoned-datetime (:expires entry))]
                     (time-between? exp-time now threshold))))
         (sort-by :expires)
         vec)))

(defn index-memory-entries!
  "Index multiple memory entries in batch.
   More efficient than calling index-memory-entry! repeatedly."
  [entries]
  (require-embedding!)
  (let [coll (get-or-create-collection)
        docs (mapv memory-to-document entries)
        embeddings (embed-batch @embedding-provider docs)
        records (mapv (fn [entry doc emb]
                        {:id (:id entry)
                         :embedding emb
                         :document doc
                         :metadata {:type (:type entry)
                                    :tags (or (join-tags (:tags entry)) "")
                                    :created (or (:created entry) "")}})
                      entries docs embeddings)]
    @(chroma/add coll records :upsert? true)
    (log/info "Indexed" (count entries) "memory entries")
    (mapv :id entries)))

;;; ============================================================
;;; Semantic Search
;;; ============================================================

(defn search-similar
  "Search for memory entries similar to the query text.
   Options:
     :limit - Max results to return (default: 10)
     :type - Filter by memory type (note, snippet, convention, decision)

   Returns seq of {:id, :document, :metadata, :distance}"
  [query-text & {:keys [limit type] :or {limit 10}}]
  (require-embedding!)
  (let [coll (get-or-create-collection)
        query-embedding (embed-text @embedding-provider query-text)
        where-clause (when type {:type type})
        results @(chroma/query coll query-embedding
                               :num-results limit
                               :where where-clause
                               :include #{:documents :metadatas :distances})]
    (log/debug "Semantic search for:" (subs query-text 0 (min 50 (count query-text))) "..."
               "found:" (count results))
    results))

(defn search-by-id
  "Get a specific entry by ID from Chroma."
  [id]
  (let [coll (get-or-create-collection)
        results @(chroma/get coll :ids [id] :include #{:documents :metadatas})]
    (first results)))

;;; ============================================================
;;; Maintenance
;;; ============================================================

(defn delete-entry!
  "Delete a memory entry from the Chroma index."
  [id]
  (let [coll (get-or-create-collection)]
    @(chroma/delete coll :ids [id])
    (log/debug "Deleted entry from Chroma:" id)
    id))

(defn collection-stats
  "Get statistics about the Chroma collection."
  []
  (try
    (let [coll (get-or-create-collection)
          all-entries @(chroma/get coll :include [:metadatas])]
      {:count (count all-entries)
       :types (frequencies (map #(get-in % [:metadata :type]) all-entries))})
    (catch Exception e
      {:error (str e)})))

;;; ============================================================
;;; Status & Health
;;; ============================================================

(defn status
  "Get Chroma integration status."
  []
  {:configured? (embedding-configured?)
   :provider (when-let [p @embedding-provider] (str (type p)))
   :collection (:collection-name @config)
   :host (:host @config)
   :port (:port @config)})

(defn chroma-available?
  "Check if Chroma is configured AND reachable.
   Returns true only if:
   1. Embedding provider is configured
   2. Chroma server responds to health check

   Used for capability-based tool switching (e.g., mem-kanban vs org-kanban fallback).

   CLARITY: Yield safe failure - returns false on any error, never throws."
  []
  (when (embedding-configured?)
    (try
      ;; Try to get/create collection as health check
      (get-or-create-collection)
      true
      (catch Exception e
        (log/debug "Chroma availability check failed:" (.getMessage e))
        false))))

(defn reinitialize-embeddings!
  "Fix hot-reload protocol mismatch by reloading namespaces and reinitializing.

   Call this when you see:
     'No implementation of method: :embed-text of protocol: EmbeddingProvider'

   This happens when the protocol is redefined but cached provider instances
   were created with the old protocol definition.

   Options:
     :provider-type - :ollama (default), :openai, or :openrouter

   Returns status map on success."
  [& {:keys [provider-type] :or {provider-type :ollama}}]
  (log/info "Reinitializing embeddings due to protocol mismatch...")

  ;; 1. Remove provider namespaces (not service - it has alias to this ns)
  (remove-ns 'hive-mcp.embeddings.ollama)
  (remove-ns 'hive-mcp.embeddings.openai)
  (remove-ns 'hive-mcp.embeddings.openrouter)
  (remove-ns 'hive-mcp.embeddings.registry)

  ;; 2. Reload implementors in correct order
  (require 'hive-mcp.embeddings.ollama :reload)
  (require 'hive-mcp.embeddings.openai :reload)
  (require 'hive-mcp.embeddings.openrouter :reload)
  (require 'hive-mcp.embeddings.registry :reload)

  ;; 3. Clear all caches
  (reset-collection-cache!)
  (reset-embedding-provider!)

  ;; 4. Reinitialize registry (service uses resolve, no reload needed)
  (let [registry-init (resolve 'hive-mcp.embeddings.registry/init!)
        registry-clear (resolve 'hive-mcp.embeddings.registry/clear-cache!)]
    (registry-clear)
    (registry-init))

  ;; 5. Create fresh provider based on type
  (let [provider (case provider-type
                   :ollama ((resolve 'hive-mcp.embeddings.ollama/->provider))
                   :openai ((resolve 'hive-mcp.embeddings.openai/->provider))
                   :openrouter ((resolve 'hive-mcp.embeddings.openrouter/->provider)))]
    (set-embedding-provider! provider)

    ;; 6. Verify fix
    (let [fixed? (satisfies? EmbeddingProvider provider)]
      (if fixed?
        (log/info "Embeddings reinitialized successfully")
        (log/error "Reinitialization failed - protocol still mismatched"))
      {:fixed? fixed?
       :provider-type provider-type
       :dimension (when fixed? (embedding-dimension provider))})))
