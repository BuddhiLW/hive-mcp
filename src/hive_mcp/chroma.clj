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
     (set-embedding-provider! (->MockEmbedder))  ; for testing
     
     ;; Index a memory entry
     (index-memory-entry! {:id \"123\" :content \"My note\" :type \"note\"})
     
     ;; Semantic search
     (search-similar \"find notes about Clojure\" :limit 5)"
  (:require [clojure-chroma-client.api :as chroma]
            [clojure-chroma-client.config :as chroma-config]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

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

;;; ============================================================
;;; Mock Embedding Provider (for testing)
;;; ============================================================

(defrecord MockEmbedder [dimension]
  EmbeddingProvider
  (embed-text [_ text]
    ;; Generate deterministic pseudo-random embedding based on text hash
    (let [h (hash text)]
      (vec (for [i (range dimension)]
             (-> (bit-xor h i)
                 (mod 1000)
                 (/ 1000.0)
                 (* 2)
                 (- 1))))))
  (embed-batch [this texts]
    (mapv #(embed-text this %) texts))
  (embedding-dimension [_] dimension))

(defn ->MockEmbedder
  "Create a mock embedder for testing (not for production use).
   Generates deterministic embeddings based on text hash."
  ([] (->MockEmbedder 384))
  ([dimension] (MockEmbedder. dimension)))

;;; ============================================================
;;; Collection Management
;;; ============================================================

(defonce ^:private collection-cache (atom nil))

(defn- get-or-create-collection
  "Get existing collection or create new one."
  []
  (if-let [coll @collection-cache]
    coll
    (let [coll-name (:collection-name @config)
          provider @embedding-provider]
      (when-not provider
        (throw (ex-info "Embedding provider not configured. Call set-embedding-provider! first."
                        {:type :no-embedding-provider})))
      (let [dim (embedding-dimension provider)
            ;; Try to get existing collection first
            existing (try
                       @(chroma/get-collection coll-name)
                       (catch Exception _ nil))]
        (if existing
          (do
            (reset! collection-cache existing)
            (log/info "Using existing Chroma collection:" coll-name)
            existing)
          (let [new-coll @(chroma/create-collection
                           coll-name
                           {:metadata {:dimension dim
                                       :created-by "hive-mcp"}})]
            (reset! collection-cache new-coll)
            (log/info "Created Chroma collection:" coll-name "dimension:" dim)
            new-coll))))))

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
       (when (seq tags) (str "Tags: " (str/join ", " tags) "\n"))
       "Content: " (if (string? content)
                     content
                     (json/write-str content))))

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
  "Check if an entry has expired based on :expires metadata."
  [metadata]
  (when-let [expires (:expires metadata)]
    (when (and (string? expires) (not (empty? expires)))
      (try
        (let [expires-instant (java.time.ZonedDateTime/parse expires)
              now (java.time.ZonedDateTime/now)]
          (.isBefore expires-instant now))
        (catch Exception _ false)))))

(defn index-memory-entry!
  "Index a memory entry in Chroma (full storage, not just search).
   Entry should have :id, :content, :type, and optionally :tags, :created, etc.
   Returns the entry ID on success.

   Full metadata stored: type, tags, content, content-hash, created, updated,
   duration, expires, access-count, helpful-count, unhelpful-count, project-id.

   Note: Tags are stored as comma-separated string since Chroma metadata
   only supports scalar values (string, int, float, bool)."
  [{:keys [id content type tags created updated duration expires
           content-hash access-count helpful-count unhelpful-count project-id]
    :as entry}]
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        provider @embedding-provider
        entry-id (or id (generate-id))
        now (iso-timestamp)
        doc-text (memory-to-document entry)
        embedding (embed-text provider doc-text)
        ;; Chroma metadata only supports scalar values, convert tags to string
        tags-str (when (seq tags) (str/join "," tags))
        ;; Serialize content if it's not a string
        content-str (if (string? content) content (json/write-str content))]
    @(chroma/add coll [{:id entry-id
                        :embedding embedding
                        :document doc-text
                        :metadata {:type (or type "note")
                                   :tags (or tags-str "")
                                   :content content-str
                                   :content-hash (or content-hash "")
                                   :created (or created now)
                                   :updated (or updated now)
                                   :duration (or duration "long-term")
                                   :expires (or expires "")
                                   :access-count (or access-count 0)
                                   :helpful-count (or helpful-count 0)
                                   :unhelpful-count (or unhelpful-count 0)
                                   :project-id (or project-id "global")}}]
                 :upsert? true)
    (log/debug "Indexed memory entry:" entry-id)
    entry-id))

(defn get-entry-by-id
  "Get a specific memory entry by ID from Chroma.
   Returns the full entry as a map with :id, :content, :type, :tags, etc.
   Returns nil if not found."
  [id]
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        results @(chroma/get coll :ids [id] :include #{:documents :metadatas})]
    (when-let [entry (first results)]
      (let [metadata (:metadata entry)
            tags-str (:tags metadata)
            content-str (:content metadata)]
        {:id (:id entry)
         :type (:type metadata)
         :content (if (and content-str
                           (or (str/starts-with? content-str "{")
                               (str/starts-with? content-str "[")))
                    (try (json/read-str content-str :key-fn keyword)
                         (catch Exception _ content-str))
                    content-str)
         :tags (when (and tags-str (not (empty? tags-str)))
                 (str/split tags-str #","))
         :content-hash (:content-hash metadata)
         :created (:created metadata)
         :updated (:updated metadata)
         :duration (:duration metadata)
         :expires (:expires metadata)
         :access-count (:access-count metadata)
         :helpful-count (:helpful-count metadata)
         :unhelpful-count (:unhelpful-count metadata)
         :project-id (:project-id metadata)
         :document (:document entry)}))))

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
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        ;; Build where clause
        where-clause (cond-> {}
                       type (assoc :type type)
                       project-id (assoc :project-id project-id))
        where (when (seq where-clause) where-clause)
        ;; Get all matching entries
        results @(chroma/get coll
                             :where where
                             :include #{:documents :metadatas}
                             :limit limit)]
    (->> results
         (map (fn [entry]
                (let [metadata (:metadata entry)
                      tags-str (:tags metadata)
                      content-str (:content metadata)]
                  {:id (:id entry)
                   :type (:type metadata)
                   :content (if (and content-str
                                     (or (str/starts-with? content-str "{")
                                         (str/starts-with? content-str "[")))
                              (try (json/read-str content-str :key-fn keyword)
                                   (catch Exception _ content-str))
                              content-str)
                   :tags (when (and tags-str (not (empty? tags-str)))
                           (str/split tags-str #","))
                   :content-hash (:content-hash metadata)
                   :created (:created metadata)
                   :updated (:updated metadata)
                   :duration (:duration metadata)
                   :expires (:expires metadata)
                   :access-count (:access-count metadata)
                   :helpful-count (:helpful-count metadata)
                   :unhelpful-count (:unhelpful-count metadata)
                   :project-id (:project-id metadata)})))
         ;; Filter expired unless include-expired?
         (remove #(and (not include-expired?) (expired? %)))
         ;; Sort by created desc
         (sort-by :created #(compare %2 %1))
         (take limit)
         vec)))

(defn update-entry!
  "Update a memory entry in Chroma.
   ID is required. Updates only provided fields.
   Returns the updated entry."
  [id updates]
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (when-let [existing (get-entry-by-id id)]
    (let [merged (merge existing updates {:updated (iso-timestamp)})
          ;; Re-index with merged data
          _ (index-memory-entry! merged)]
      (get-entry-by-id id))))

(defn find-duplicate
  "Find entry with matching content-hash in the given type.
   Returns the existing entry or nil."
  [type content-hash & {:keys [project-id]}]
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [entries (query-entries :type type :project-id project-id :limit 1000)]
    (first (filter #(= (:content-hash %) content-hash) entries))))

(defn content-hash
  "Compute SHA-256 hash of content for deduplication.
   Normalizes content (trim, collapse whitespace) before hashing."
  [content]
  (let [text (if (string? content)
               content
               (json/write-str content))
        normalized (-> text
                       str/trim
                       (str/replace #"[ \t]+" " ")
                       (str/replace #"\n+" "\n"))]
    (let [md (java.security.MessageDigest/getInstance "SHA-256")
          hash-bytes (.digest md (.getBytes normalized "UTF-8"))]
      (apply str (map #(format "%02x" %) hash-bytes)))))

(defn cleanup-expired!
  "Delete all expired entries from Chroma.
   Returns count of deleted entries."
  []
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        ;; Get all entries including expired
        all-entries @(chroma/get coll :include #{:metadatas} :limit 10000)
        expired-ids (->> all-entries
                         (filter #(expired? (:metadata %)))
                         (map :id))]
    (when (seq expired-ids)
      @(chroma/delete coll :ids expired-ids)
      (log/info "Cleaned up" (count expired-ids) "expired entries"))
    (count expired-ids)))

(defn entries-expiring-soon
  "Get entries expiring within the given number of days.
   Returns seq of entry maps."
  [days & {:keys [project-id]}]
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [threshold (-> (java.time.ZonedDateTime/now)
                      (.plusDays days))
        entries (query-entries :project-id project-id :limit 10000 :include-expired? false)]
    (->> entries
         (filter (fn [entry]
                   (when-let [expires (:expires entry)]
                     (when (and (string? expires) (not (empty? expires)))
                       (try
                         (let [exp-time (java.time.ZonedDateTime/parse expires)]
                           (and (.isAfter exp-time (java.time.ZonedDateTime/now))
                                (.isBefore exp-time threshold)))
                         (catch Exception _ false))))))
         (sort-by :expires)
         vec)))

(defn index-memory-entries!
  "Index multiple memory entries in batch.
   More efficient than calling index-memory-entry! repeatedly."
  [entries]
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        provider @embedding-provider
        docs (mapv memory-to-document entries)
        embeddings (embed-batch provider docs)
        records (mapv (fn [entry doc emb]
                        (let [tags-str (when (seq (:tags entry))
                                         (str/join "," (:tags entry)))]
                          {:id (:id entry)
                           :embedding emb
                           :document doc
                           :metadata {:type (:type entry)
                                      :tags (or tags-str "")
                                      :created (or (:created entry) "")}}))
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
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        provider @embedding-provider
        query-embedding (embed-text provider query-text)
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
