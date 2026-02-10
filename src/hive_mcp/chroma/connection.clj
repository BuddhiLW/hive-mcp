(ns hive-mcp.chroma.connection
  "Chroma connection configuration, collection management, and health status.

   Manages connection config, collection cache, availability checks,
   and embedding reinitialization for hot-reload recovery."
  (:require [clojure-chroma-client.api :as chroma]
            [clojure-chroma-client.config :as chroma-config]
            [hive-mcp.chroma.embeddings :as emb]
            [taoensso.timbre :as log]))

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

(defn get-config
  "Get current Chroma configuration map."
  []
  @config)

(defn configure!
  "Configure Chroma connection settings.

   Options:
     :host - Chroma server host (default: localhost)
     :port - Chroma server port (default: 8000)
     :collection-name - Collection for memory entries (default: hive-mcp-memory)

   Note: For Chroma Cloud, set CHROMA_API_KEY, CHROMA_TENANT, CHROMA_DATABASE env vars."
  [opts]
  (swap! config merge opts)
  (chroma-config/configure (select-keys opts [:host :port :api-version :tenant :database]))
  (log/info "Chroma configured:" (select-keys @config [:host :port :collection-name])))

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

(defn get-or-create-collection
  "Get existing collection or create new one."
  []
  (or @collection-cache
      (let [coll-name (:collection-name @config)
            _ (emb/require-embedding!)
            dim (emb/embedding-dimension (emb/get-embedding-provider))]
        (if-let [existing (try-get-collection coll-name)]
          (cache-collection! existing (str "Using existing Chroma collection: " coll-name))
          (cache-collection! (create-new-collection coll-name dim)
                             (str "Created Chroma collection: " coll-name " dimension: " dim))))))

(defn reset-collection-cache!
  "Reset the collection cache (for testing/reconnection)."
  []
  (reset! collection-cache nil))

;;; ============================================================
;;; Status & Health
;;; ============================================================

(defn status
  "Get Chroma integration status."
  []
  {:configured? (emb/embedding-configured?)
   :provider (when-let [p (emb/get-embedding-provider)] (str (type p)))
   :collection (:collection-name @config)
   :host (:host @config)
   :port (:port @config)})

(defn chroma-available?
  "Check if Chroma is configured AND reachable.
   Returns true only if:
   1. Embedding provider is configured
   2. Chroma server responds to health check

   Used for capability-based tool switching."
  []
  (when (emb/embedding-configured?)
    (try
      (get-or-create-collection)
      true
      (catch Exception e
        (log/debug "Chroma availability check failed:" (.getMessage e))
        false))))

(defn reinitialize-embeddings!
  "Fix hot-reload protocol mismatch by reloading namespaces and reinitializing.

   Call this when you see:
     'No implementation of method: :embed-text of protocol: EmbeddingProvider'

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
  (emb/reset-embedding-provider!)

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
    (emb/set-embedding-provider! provider)

    ;; 6. Verify fix
    (let [fixed? (satisfies? emb/EmbeddingProvider provider)]
      (if fixed?
        (log/info "Embeddings reinitialized successfully")
        (log/error "Reinitialization failed - protocol still mismatched"))
      {:fixed? fixed?
       :provider-type provider-type
       :dimension (when fixed? (emb/embedding-dimension provider))})))
