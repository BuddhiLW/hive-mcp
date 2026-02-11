(ns hive-mcp.tools.catchup.scope
  "Scope resolution and query functions for catchup workflow."
  (:require [hive-mcp.emacs.client :as ec]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [clojure.string :as str]
            [clojure.set :as set]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn distinct-by
  "Return distinct elements from coll by the value of (f item)."
  [f coll]
  (let [seen (volatile! #{})]
    (filterv (fn [item]
               (let [key (f item)]
                 (if (contains? @seen key)
                   false
                   (do (vswap! seen conj key) true))))
             coll)))

(defn- newest-first
  "Sort entries by :created timestamp, newest first."
  [entries]
  (sort-by :created #(compare %2 %1) entries))

(defn get-current-project-name
  "Get current project name from Emacs."
  ([] (get-current-project-name nil))
  ([directory]
   (try
     (let [elisp (if directory
                   (format "(hive-mcp-memory--get-project-name %s)" (pr-str directory))
                   "(hive-mcp-memory--get-project-name)")
           {:keys [success result timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
       (if (and success result (not= result "nil") (not timed-out))
         (str/replace result #"\"" "")
         nil))
     (catch Exception _
       nil))))

(defn filter-by-tags
  "Filter entries to only those containing all specified tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

(defn- compute-hierarchy-project-ids
  "Compute the full set of visible project IDs for DB-level filtering."
  [project-id]
  (let [in-project? (and project-id (not= project-id "global"))]
    (when in-project?
      (let [visible (kg-scope/visible-scopes project-id)
            descendants (kg-scope/descendant-scopes project-id)
            all-ids (distinct (concat visible descendants))]
        (vec (remove #(= "global" %) all-ids))))))

(defn- compute-full-scope-tags
  "Compute full hierarchy scope tags for in-memory safety-net filtering."
  [project-id]
  (let [in-project? (and project-id (not= project-id "global"))]
    (cond-> (kg-scope/full-hierarchy-scope-tags project-id)
      in-project? (disj "scope:global"))))

(defn- scope-filter-entries
  "Apply in-memory scope filter as safety net."
  [entries scope-tags visible-ids]
  (filter (fn [entry]
            (let [entry-tags (set (or (:tags entry) []))]
              (or
               (some entry-tags scope-tags)
               (contains? visible-ids (:project-id entry)))))
          entries))

(defn- scope-pierce-entries
  "Extract axioms and catchup-priority entries that pierce scope boundaries."
  [entries project-id]
  (let [in-project? (and project-id (not= project-id "global"))]
    (when in-project?
      (filter (fn [entry]
                (let [entry-tags (set (or (:tags entry) []))
                      entry-type (str (or (:type entry) ""))]
                  (or (= entry-type "axiom")
                      (contains? entry-tags "catchup-priority"))))
              entries))))

(defn query-scoped-entries
  "Query Chroma entries filtered by project scope with hierarchy and scope-piercing."
  [entry-type tags project-id limit]
  (when (chroma/embedding-configured?)
    (let [limit-val (or limit 20)
          in-project? (and project-id (not= project-id "global"))
          hierarchy-ids (compute-hierarchy-project-ids project-id)
          over-fetch-factor (if hierarchy-ids 3 4)
          entries (chroma/query-entries :type entry-type
                                        :project-ids hierarchy-ids
                                        :limit (min (* limit-val over-fetch-factor) 500))
          full-scope-tags (compute-full-scope-tags project-id)
          all-visible-ids (set (or hierarchy-ids ["global"]))
          scoped (scope-filter-entries entries full-scope-tags all-visible-ids)
          scope-piercing (when in-project?
                           (let [global-entries (chroma/query-entries :type entry-type
                                                                      :project-id "global"
                                                                      :limit 100)]
                             (scope-pierce-entries global-entries project-id)))
          scoped (distinct-by :id (concat scoped scope-piercing))
          filtered (filter-by-tags scoped tags)]
      (->> filtered
           newest-first
           (take limit-val)))))

(defn entry-expiring-soon?
  "Check if entry expires within 7 days."
  [entry]
  (when-let [exp (:expires entry)]
    (try
      (let [exp-time (java.time.ZonedDateTime/parse exp)
            now (java.time.ZonedDateTime/now)
            week-later (.plusDays now 7)]
        (.isBefore exp-time week-later))
      (catch Exception _ false))))

(defn query-expiring-entries
  "Query entries expiring within 7 days, scoped to project with scope-piercing."
  [project-id limit]
  (let [in-project? (and project-id (not= project-id "global"))
        hierarchy-ids (compute-hierarchy-project-ids project-id)
        entries (chroma/query-entries :project-ids hierarchy-ids
                                      :limit 200)
        full-scope-tags (compute-full-scope-tags project-id)
        all-visible-ids (set (or hierarchy-ids ["global"]))
        scoped (scope-filter-entries entries full-scope-tags all-visible-ids)
        scope-piercing (when in-project?
                         (let [global-entries (chroma/query-entries :project-id "global"
                                                                    :limit 100)]
                           (scope-pierce-entries global-entries project-id)))
        scoped (distinct-by :id (concat scoped scope-piercing))]
    (->> scoped
         (filter entry-expiring-soon?)
         newest-first
         (take (or limit 20)))))

(defn query-axioms
  "Query axiom entries (both formal type and legacy tagged conventions)."
  [project-id]
  (let [formal (query-scoped-entries "axiom" nil project-id 100)
        legacy (query-scoped-entries "convention" ["axiom"] project-id 100)]
    (distinct-by :id (concat formal legacy))))

(defn query-regular-conventions
  "Query conventions excluding axioms and priority ones."
  [project-id axiom-ids priority-ids]
  (let [all-conventions (query-scoped-entries "convention" nil project-id 50)
        excluded-ids (set/union axiom-ids priority-ids)]
    (remove #(contains? excluded-ids (:id %)) all-conventions)))
