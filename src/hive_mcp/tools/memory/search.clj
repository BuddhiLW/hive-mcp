(ns hive-mcp.tools.memory.search
  "Semantic search handler for memory operations.

   SOLID: SRP - Single responsibility for vector search.
   CLARITY: A - Architectural performance with vector similarity.

   Handlers:
   - search-semantic: Vector similarity search using Chroma embeddings"
  (:require [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; ============================================================
;; Result Formatting
;; ============================================================

(defn- format-search-result
  "Format a single search result for user-friendly output."
  [{:keys [id document metadata distance]}]
  {:id id
   :type (get metadata :type)
   :tags (when-let [t (get metadata :tags)]
           (when (not= t "")
             (str/split t #",")))
   :distance distance
   :preview (when document
              (subs document 0 (min 200 (count document))))})

;; ============================================================
;; Search Handler
;; ============================================================

(defn handle-search-semantic
  "Search project memory using semantic similarity (vector search).
   Requires Chroma to be configured with an embedding provider."
  [{:keys [query limit type]}]
  (log/info "mcp-memory-search-semantic:" query)
  (let [status (chroma/status)]
    (if-not (:configured? status)
      {:type "text"
       :text (json/write-str
              {:error "Chroma semantic search not configured"
               :message "To enable semantic search, configure Chroma with an embedding provider. See hive-mcp.chroma namespace."
               :status status})
       :isError true}
      (try
        (let [results (chroma/search-similar query
                                             :limit (or limit 10)
                                             :type type)
              formatted (mapv format-search-result results)]
          {:type "text"
           :text (json/write-str {:results formatted
                                  :count (count formatted)
                                  :query query})})
        (catch Exception e
          {:type "text"
           :text (json/write-str {:error (str "Semantic search failed: " (.getMessage e))
                                  :status status})
           :isError true})))))
