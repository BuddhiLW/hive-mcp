(ns hive-mcp.embeddings.config
  "EmbeddingConfig value object for per-collection embedding configuration.

   SOLID: SRP - Single responsibility for embedding configuration.
   CLARITY: R - Represented intent via explicit config records.

   An EmbeddingConfig describes how a specific collection should embed its content:
   - provider-type: :ollama, :openai, :openrouter
   - model: Model name for the provider
   - dimension: Embedding dimension (for Chroma collection creation)
   - options: Provider-specific options (host, api-key, etc.)

   Factory functions provide easy config creation:
     (ollama-config)                     ; Default Ollama config
     (openrouter-config)                 ; OpenRouter with env API key
     (openai-config {:model \"text-embedding-3-large\"})

   Usage with EmbeddingService:
     (service/configure-collection! \"my-collection\" (config/ollama-config))")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; EmbeddingConfig Value Object
;;; ============================================================

(defrecord EmbeddingConfig
           [provider-type  ; :ollama, :openai, :openrouter
            model          ; e.g., "nomic-embed-text", "qwen/qwen3-embedding-8b"
            dimension      ; 768, 1536, 4096
            options])      ; {:host "..." :api-key "..."}

(defn valid-config?
  "Check if an EmbeddingConfig is valid."
  [config]
  (and (instance? EmbeddingConfig config)
       (#{:ollama :openai :openrouter} (:provider-type config))
       (string? (:model config))
       (pos-int? (:dimension config))))

(defn config->map
  "Convert EmbeddingConfig to plain map (for serialization/logging)."
  [^EmbeddingConfig config]
  {:provider-type (:provider-type config)
   :model (:model config)
   :dimension (:dimension config)
   :options (select-keys (:options config) [:host])}) ; Don't log api-key

;;; ============================================================
;;; Provider-Specific Model Definitions
;;; ============================================================

(def ^:private ollama-models
  "Ollama embedding models with dimensions."
  {"nomic-embed-text" 768
   "mxbai-embed-large" 1024
   "all-minilm" 384
   "snowflake-arctic-embed" 1024})

(def ^:private openai-models
  "OpenAI embedding models with dimensions."
  {"text-embedding-3-small" 1536
   "text-embedding-3-large" 3072
   "text-embedding-ada-002" 1536})

(def ^:private openrouter-models
  "OpenRouter embedding models with dimensions."
  {"qwen/qwen3-embedding-8b" 4096
   "openai/text-embedding-3-small" 1536
   "openai/text-embedding-3-large" 3072
   "cohere/embed-english-v3.0" 1024
   "cohere/embed-multilingual-v3.0" 1024})

(defn get-dimension
  "Get embedding dimension for a provider/model pair.
   Returns nil if model is unknown."
  [provider-type model]
  (case provider-type
    :ollama (get ollama-models model)
    :openai (get openai-models model)
    :openrouter (get openrouter-models model 4096) ; Default for unknown OpenRouter models
    nil))

;;; ============================================================
;;; Factory Functions (CLARITY: C - Composition over modification)
;;; ============================================================

(defn ollama-config
  "Create Ollama embedding configuration.

   Options:
     :model - Embedding model (default: nomic-embed-text)
     :host - Ollama server URL (default: from OLLAMA_HOST or localhost)

   Returns EmbeddingConfig record."
  ([] (ollama-config {}))
  ([{:keys [model host]
     :or {model "nomic-embed-text"}}]
   (let [host (or host (System/getenv "OLLAMA_HOST") "http://localhost:11434")
         dimension (or (get ollama-models model)
                       (throw (ex-info (str "Unknown Ollama model: " model
                                            ". Supported: " (keys ollama-models))
                                       {:model model :supported (keys ollama-models)})))]
     (->EmbeddingConfig :ollama model dimension {:host host}))))

(defn openai-config
  "Create OpenAI embedding configuration.

   Options:
     :model - Embedding model (default: text-embedding-3-small)
     :api-key - API key (default: from OPENAI_API_KEY env)

   Returns EmbeddingConfig record."
  ([] (openai-config {}))
  ([{:keys [model api-key]
     :or {model "text-embedding-3-small"}}]
   (let [api-key (or api-key (System/getenv "OPENAI_API_KEY"))
         dimension (or (get openai-models model)
                       (throw (ex-info (str "Unknown OpenAI model: " model
                                            ". Supported: " (keys openai-models))
                                       {:model model :supported (keys openai-models)})))]
     (when-not api-key
       (throw (ex-info "OpenAI API key required. Set OPENAI_API_KEY env var or pass :api-key option."
                       {:type :missing-api-key})))
     (->EmbeddingConfig :openai model dimension {:api-key api-key}))))

(defn openrouter-config
  "Create OpenRouter embedding configuration.

   Options:
     :model - Embedding model (default: qwen/qwen3-embedding-8b - FREE!)
     :api-key - API key (default: from OPENROUTER_API_KEY env)

   Returns EmbeddingConfig record."
  ([] (openrouter-config {}))
  ([{:keys [model api-key]
     :or {model "qwen/qwen3-embedding-8b"}}]
   (let [api-key (or api-key (System/getenv "OPENROUTER_API_KEY"))
         dimension (get openrouter-models model 4096)] ; Default dimension for unknown models
     (when-not api-key
       (throw (ex-info "OpenRouter API key required. Set OPENROUTER_API_KEY env var or pass :api-key option."
                       {:type :missing-api-key})))
     (->EmbeddingConfig :openrouter model dimension {:api-key api-key}))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defn same-dimension?
  "Check if two configs have the same embedding dimension."
  [config1 config2]
  (= (:dimension config1) (:dimension config2)))

(defn describe
  "Human-readable description of an EmbeddingConfig."
  [config]
  (format "%s/%s (%d dims)"
          (name (:provider-type config))
          (:model config)
          (:dimension config)))
