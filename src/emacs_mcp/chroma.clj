(ns emacs-mcp.chroma
  "Chroma vector database integration for semantic memory search.
   
   Provides vector-based similarity search for emacs-mcp memory entries.
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
            [taoensso.timbre :as log]
            [clojure.data.json :as json]))

;;; ============================================================
;;; Configuration
;;; ============================================================

(def ^:private default-config
  {:host "localhost"
   :port 8000
   :collection-name "emacs-mcp-memory"})

(defonce ^:private config (atom default-config))

(defn configure!
  "Configure Chroma connection settings.
   
   Options:
     :host - Chroma server host (default: localhost)
     :port - Chroma server port (default: 8000)
     :collection-name - Collection for memory entries (default: emacs-mcp-memory)
   
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
                                       :created-by "emacs-mcp"}})]
            (reset! collection-cache new-coll)
            (log/info "Created Chroma collection:" coll-name "dimension:" dim)
            new-coll))))))

(defn reset-collection-cache!
  "Reset the collection cache (for testing/reconnection)."
  []
  (reset! collection-cache nil))

;;; ============================================================
;;; Memory Indexing
;;; ============================================================

(defn- memory-to-document
  "Convert memory entry to searchable document string."
  [{:keys [content type tags]}]
  (str "Type: " type "\n"
       (when (seq tags) (str "Tags: " (clojure.string/join ", " tags) "\n"))
       "Content: " (if (string? content)
                     content
                     (json/write-str content))))

(defn index-memory-entry!
  "Index a memory entry in Chroma for semantic search.
   Entry should have :id, :content, :type, and optionally :tags, :created.
   Returns the entry ID on success.
   
   Note: Tags are stored as comma-separated string since Chroma metadata
   only supports scalar values (string, int, float, bool)."
  [{:keys [id content type tags created] :as entry}]
  (when-not (embedding-configured?)
    (throw (ex-info "Embedding provider not configured" {:type :no-embedding-provider})))
  (let [coll (get-or-create-collection)
        provider @embedding-provider
        doc-text (memory-to-document entry)
        embedding (embed-text provider doc-text)
        ;; Chroma metadata only supports scalar values, convert tags to string
        tags-str (when (seq tags) (clojure.string/join "," tags))]
    @(chroma/add coll [{:id id
                        :embedding embedding
                        :document doc-text
                        :metadata {:type type
                                   :tags (or tags-str "")
                                   :created (or created "")}}]
                 :upsert? true)
    (log/debug "Indexed memory entry:" id)
    id))

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
                                         (clojure.string/join "," (:tags entry)))]
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
