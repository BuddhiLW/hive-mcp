(ns hive-mcp.tools.memory.search
  "Semantic search handler for memory operations.

   SOLID: SRP - Single responsibility for vector search.
   CLARITY: A - Architectural performance with vector similarity.

   Handlers:
   - search-semantic: Vector similarity search using Chroma embeddings"
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.core :refer [mcp-error coerce-int!]]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

(defn- matches-scope-filter?
  "Check if a search result matches the scope filter.
   Search results have different structure than entries - tags are in metadata."
  [result scope-filter]
  (if (nil? scope-filter)
    true
    (let [tags-str (get-in result [:metadata :tags] "")
          tags (when (and tags-str (not= tags-str ""))
                 (set (str/split tags-str #",")))]
      (or (contains? tags scope-filter)
          (contains? tags "scope:global")))))

(defn handle-search-semantic
  "Search project memory using semantic similarity (vector search).
   Requires Chroma to be configured with an embedding provider.
   When directory is provided, filters results to that project scope."
  [{:keys [query limit type directory]}]
  (let [directory (or directory (ctx/current-directory))]
    (log/info "mcp-memory-search-semantic:" query "directory:" directory)
    (try
      (let [limit-val (coerce-int! limit :limit 10)
            status (chroma/status)]
        (if-not (:configured? status)
          {:type "text"
           :text (json/write-str
                  {:error "Chroma semantic search not configured"
                   :message "To enable semantic search, configure Chroma with an embedding provider. See hive-mcp.chroma namespace."
                   :status status})
           :isError true}
          (try
            (let [project-id (scope/get-current-project-id directory)
                  ;; Over-fetch to allow for scope filtering
                  results (chroma/search-similar query
                                                 :limit (* limit-val 3)
                                                 :type type)
                  ;; Apply scope filter
                  scope-filter (when (and project-id (not= project-id "global"))
                                 (scope/make-scope-tag project-id))
                  filtered (if scope-filter
                             (filter #(matches-scope-filter? % scope-filter) results)
                             results)
                  ;; Take requested limit after filtering
                  limited (take limit-val filtered)
                  formatted (mapv format-search-result limited)]
              ;; Record co-access pattern for semantic search results (async, non-blocking)
              (when (>= (count formatted) 2)
                (future
                  (try
                    (kg-edges/record-co-access!
                     (mapv :id formatted)
                     {:scope project-id :created-by "system:semantic-search"})
                    (catch Exception e
                      (log/debug "Co-access recording failed (non-fatal):" (.getMessage e))))))
              {:type "text"
               :text (json/write-str {:results formatted
                                      :count (count formatted)
                                      :query query
                                      :scope project-id})})
            (catch Exception e
              {:type "text"
               :text (json/write-str {:error (str "Semantic search failed: " (.getMessage e))
                                      :status status})
               :isError true}))))
      (catch clojure.lang.ExceptionInfo e
        (if (= :coercion-error (:type (ex-data e)))
          (mcp-error (.getMessage e))
          (throw e))))))
