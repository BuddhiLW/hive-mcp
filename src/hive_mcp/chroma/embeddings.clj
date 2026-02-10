(ns hive-mcp.chroma.embeddings
  "Embedding provider protocol and management.

   Defines the EmbeddingProvider protocol and manages the global provider atom.
   Supports collection-aware routing via EmbeddingService."
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

(defn require-embedding!
  "Guard: throws if embedding provider not configured."
  []
  (when-not @embedding-provider
    (throw (ex-info "Embedding provider not configured. Call set-embedding-provider! first."
                    {:type :no-embedding-provider}))))

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
