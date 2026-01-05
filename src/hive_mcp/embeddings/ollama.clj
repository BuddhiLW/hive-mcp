(ns hive-mcp.embeddings.ollama
  "Ollama embedding provider for semantic memory search.
   
   Uses local Ollama server for free, private embeddings.
   No API key required - all data stays on your machine.
   
   Recommended model: nomic-embed-text (768 dimensions, fast, good quality)
   
   Setup:
     ollama pull nomic-embed-text
   
   Usage:
     (require '[hive-mcp.embeddings.ollama :as ollama])
     (require '[hive-mcp.chroma :as chroma])
     
     ;; Create provider with default model
     (chroma/set-embedding-provider! (ollama/->provider))
     
     ;; Or specify model
     (chroma/set-embedding-provider! 
       (ollama/->provider {:model \"mxbai-embed-large\"}))"
  (:require [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [taoensso.timbre :as log])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

;;; ============================================================
;;; Configuration
;;; ============================================================

(def ^:private models
  "Supported Ollama embedding models with their dimensions.
   Run `ollama pull <model>` to download."
  {"nomic-embed-text" 768 ; Good balance of speed/quality
   "mxbai-embed-large" 1024 ; Higher quality, slower
   "all-minilm" 384 ; Fastest, lower quality
   "snowflake-arctic-embed" 1024})

(def ^:private default-model "nomic-embed-text")
(def ^:private default-host "http://localhost:11434")

;;; ============================================================
;;; HTTP Client
;;; ============================================================

(defonce ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.connectTimeout (Duration/ofSeconds 30))
        (.build))))

(defn- make-request
  "Make HTTP POST request to Ollama API."
  [host endpoint body]
  (let [url (str host endpoint)
        request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.header "Content-Type" "application/json")
                    (.POST (HttpRequest$BodyPublishers/ofString (json/write-str body)))
                    (.timeout (Duration/ofSeconds 120)) ; Embeddings can be slow on first run
                    (.build))
        response (.send @http-client request (HttpResponse$BodyHandlers/ofString))
        status (.statusCode response)
        body-str (.body response)]
    (if (= status 200)
      (json/read-str body-str :key-fn keyword)
      (throw (ex-info "Ollama API error"
                      {:status status
                       :body body-str
                       :url url})))))

;;; ============================================================
;;; Embedding Functions
;;; ============================================================

(defn- get-embedding
  "Get embedding for a single text from Ollama."
  [host model text]
  (let [response (make-request host "/api/embeddings"
                               {:model model
                                :prompt text})]
    (:embedding response)))

(defn- get-embeddings-batch
  "Get embeddings for multiple texts from Ollama.
   Ollama doesn't have native batch API, so we parallelize requests."
  [host model texts]
  (let [futures (mapv #(future (get-embedding host model %)) texts)]
    (mapv deref futures)))

;;; ============================================================
;;; Provider Implementation
;;; ============================================================

(defrecord OllamaEmbedder [host model dimension]
  chroma/EmbeddingProvider
  (embed-text [_ text]
    (get-embedding host model text))
  (embed-batch [_ texts]
    (get-embeddings-batch host model texts))
  (embedding-dimension [_] dimension))

;;; ============================================================
;;; Provider Factory
;;; ============================================================

(defn ->provider
  "Create an Ollama embedding provider.
   
   Options:
     :host - Ollama server URL (default: http://localhost:11434)
     :model - Embedding model (default: nomic-embed-text)
   
   Available models (run `ollama pull <model>` first):
     - nomic-embed-text (768 dims, recommended - good balance)
     - mxbai-embed-large (1024 dims, higher quality)
     - all-minilm (384 dims, fastest)
     - snowflake-arctic-embed (1024 dims)"
  ([] (->provider {}))
  ([{:keys [host model] :or {host default-host model default-model}}]
   (let [dimension (get models model)]
     (when-not dimension
       (throw (ex-info (str "Unknown model: " model ". Supported: " (keys models)
                            "\nYou can also add custom models to the `models` map.")
                       {:type :unknown-model
                        :model model
                        :supported (keys models)})))
     ;; Test connection
     (try
       (let [test-result (make-request host "/api/tags" nil)]
         (log/info "Connected to Ollama at" host)
         (log/debug "Available models:" (mapv :name (:models test-result))))
       (catch Exception e
         (log/warn "Could not connect to Ollama at" host "- ensure ollama is running")))
     (log/info "Created Ollama embedder with model:" model "dimension:" dimension)
     (->OllamaEmbedder host model dimension))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defn list-models
  "List available models on the Ollama server."
  ([] (list-models default-host))
  ([host]
   (let [response (make-request host "/api/tags" nil)]
     (mapv :name (:models response)))))

(defn pull-model
  "Pull a model from Ollama (downloads if not present).
   This is a convenience wrapper - you can also run `ollama pull <model>` in terminal."
  ([model] (pull-model default-host model))
  ([host model]
   (log/info "Pulling model:" model "(this may take a while...)")
   (make-request host "/api/pull" {:name model :stream false})))
