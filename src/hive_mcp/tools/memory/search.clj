(ns hive-mcp.tools.memory.search
  "Semantic search handler for memory operations.

   SOLID: SRP - Single responsibility for vector search.
   CLARITY: A - Architectural performance with vector similarity.

   Handlers:
   - search-semantic: Vector similarity search using Chroma embeddings"
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.plans :as plans]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
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
   Search results have different structure than entries - tags are in metadata.

   HCR Wave 5: scope-filter can be a string (single tag) or a set of tags.
   Matches if any entry tag is in the filter set."
  [result scope-filter]
  (if (nil? scope-filter)
    true
    (let [tags-str (get-in result [:metadata :tags] "")
          tags (when (and tags-str (not= tags-str ""))
                 (set (str/split tags-str #",")))
          scope-set (if (set? scope-filter) scope-filter #{scope-filter})]
      (or (some tags scope-set)
          (contains? tags "scope:global")))))

(defn handle-search-semantic
  "Search project memory using semantic similarity (vector search).
   Requires Chroma to be configured with an embedding provider.
   When directory is provided, filters results to that project scope.

   Plan routing: When type='plan', searches the plans collection
   (OpenRouter embeddings, 4096 dims) instead of the memory collection."
  [{:keys [query limit type directory]}]
  (let [directory (or directory (ctx/current-directory))
        plan? (= type "plan")]
    (log/info "mcp-memory-search-semantic:" query "type:" type "directory:" directory)
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
                  in-project? (and project-id (not= project-id "global"))]
              (if plan?
                ;; Route to plans collection (OpenRouter, 4096 dims)
                (let [results (plans/search-plans query
                                                  :limit limit-val
                                                  :project-id (when in-project? project-id))
                      formatted (mapv (fn [{:keys [id type tags project-id plan-status distance preview]}]
                                        {:id id
                                         :type (or type "plan")
                                         :tags tags
                                         :distance distance
                                         :preview preview})
                                      results)]
                  ;; Record co-access for plan search results
                  (when (>= (count formatted) 2)
                    (future
                      (try
                        (kg-edges/record-co-access!
                         (mapv :id formatted)
                         {:scope project-id :created-by "system:plan-search"})
                        (catch Exception e
                          (log/debug "Co-access recording failed (non-fatal):" (.getMessage e))))))
                  {:type "text"
                   :text (json/write-str {:results formatted
                                          :count (count formatted)
                                          :query query
                                          :scope project-id})})
                ;; Default: search memory collection (Ollama, 768 dims)
                (let [;; HCR Wave 5: Compute visible + descendant project-ids for DB-level $in
                      ;; Includes ancestors (upward) AND descendants (downward) using
                      ;; cached hierarchy tree for O(1) descendant resolution.
                      visible-ids (when in-project?
                                    (let [visible (kg-scope/visible-scopes project-id)
                                          descendants (kg-scope/descendant-scopes project-id)
                                          all-ids (distinct (concat visible descendants))]
                                      (vec (remove #(= "global" %) all-ids))))
                      ;; Reduced over-fetch with DB-level filtering
                      results (chroma/search-similar query
                                                     :limit (* limit-val 2)
                                                     :type type
                                                     :project-ids visible-ids)
                      ;; Safety net: apply scope filter using full hierarchy tags
                      ;; HCR Wave 5: Uses set of scope tags (self + ancestors + descendants)
                      scope-filter (when in-project?
                                     (let [full-tags (kg-scope/full-hierarchy-scope-tags project-id)]
                                       (disj full-tags "scope:global")))
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
                                          :scope project-id})})))
            (catch Exception e
              {:type "text"
               :text (json/write-str {:error (str "Semantic search failed: " (.getMessage e))
                                      :status status})
               :isError true}))))
      (catch clojure.lang.ExceptionInfo e
        (if (= :coercion-error (:type (ex-data e)))
          (mcp-error (.getMessage e))
          (throw e))))))
