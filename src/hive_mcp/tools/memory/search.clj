(ns hive-mcp.tools.memory.search
  "Semantic search handler for memory operations."
  (:require [hive-mcp.chroma.core :as chroma]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.core :refer [coerce-int!]]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.dns.result :as result :refer [rescue]]
            [hive-mcp.agent.context :as ctx]
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

(defn- record-co-access!
  "Fire-and-forget co-access recording for search results."
  [formatted project-id created-by]
  (when (>= (count formatted) 2)
    (future
      (rescue nil
              (kg-edges/record-co-access!
               (mapv :id formatted)
               {:scope project-id :created-by created-by})))))

(defn- search-plans*
  "Search high-abstraction plans. Returns Result."
  [query limit-val type project-id in-project?]
  (let [results (plans/search-plans query
                                    :limit limit-val
                                    :type type
                                    :project-id (when in-project? project-id))
        formatted (mapv (fn [{:keys [id type tags distance preview]}]
                          {:id id
                           :type (or type "plan")
                           :tags tags
                           :distance distance
                           :preview preview})
                        results)]
    (record-co-access! formatted project-id "system:high-abstraction-search")
    (result/ok {:results formatted
                :count (count formatted)
                :query query
                :scope project-id})))

(defn- search-chroma*
  "Search Chroma vector store with scope filtering. Returns Result."
  [query limit-val type project-id in-project? include_descendants]
  (let [visible-ids (when in-project?
                      (let [visible (kg-scope/visible-scopes project-id)
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
    (record-co-access! formatted project-id "system:semantic-search")
    (result/ok {:results formatted
                :count (count formatted)
                :query query
                :scope project-id})))

(defn- search-semantic*
  "Pure search logic returning Result. Validates inputs and dispatches to appropriate search backend."
  [{:keys [query limit type directory include_descendants]}]
  (let [directory (or directory (ctx/current-directory))
        openrouter? (plans/high-abstraction-type? type)
        limit-val (coerce-int! limit :limit 10)
        status (chroma/status)]
    (log/info "mcp-memory-search-semantic:" query "type:" type "directory:" directory)
    (if-not (:configured? status)
      (result/err :memory/chroma-not-configured
                  {:message (str "Chroma semantic search not configured. "
                                 "Configure Chroma with an embedding provider.")})
      (let [project-id (scope/get-current-project-id directory)
            in-project? (and project-id (not= project-id "global"))]
        (if openrouter?
          (search-plans* query limit-val type project-id in-project?)
          (search-chroma* query limit-val type project-id in-project? include_descendants))))))

(defn handle-search-semantic
  "Search project memory using semantic similarity (vector search).
   HCR Wave 4: Pass include_descendants=true to include child project memories."
  [params]
  (rb/result->mcp (rb/try-result :memory/search #(search-semantic* params))))
