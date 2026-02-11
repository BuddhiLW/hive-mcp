(ns hive-mcp.tools.memory.search
  "Semantic search handler for memory operations."
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.core :refer [mcp-error coerce-int!]]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [clojure.set]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

(defn- matches-scope-filter?
  "Check if a search result matches the scope filter."
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
   HCR Wave 4: Pass include_descendants=true to include child project memories."
  [{:keys [query limit type directory include_descendants]}]
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
                (let [visible-ids (when in-project?
                                    (let [visible (kg-scope/visible-scopes project-id)
                                          ;; HCR Wave 4: Only include descendants when explicitly requested
                                          descendants (when include_descendants
                                                        (kg-scope/descendant-scopes project-id))
                                          all-ids (distinct (concat visible descendants))]
                                      (vec (remove #(= "global" %) all-ids))))
                      results (chroma/search-similar query
                                                     :limit (* limit-val 2)
                                                     :type type
                                                     :project-ids visible-ids)
                      scope-filter (when in-project?
                                     (let [base-tags (kg-scope/visible-scope-tags project-id)
                                           desc-tags (when include_descendants
                                                       (kg-scope/descendant-scope-tags project-id))
                                           all-tags (if desc-tags
                                                      (clojure.set/union base-tags desc-tags)
                                                      base-tags)]
                                       (disj all-tags "scope:global")))
                      filtered (if scope-filter
                                 (filter #(matches-scope-filter? % scope-filter) results)
                                 results)
                      limited (take limit-val filtered)
                      formatted (mapv format-search-result limited)]
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
