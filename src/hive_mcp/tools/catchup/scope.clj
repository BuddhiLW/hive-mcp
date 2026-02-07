(ns hive-mcp.tools.catchup.scope
  "Scope resolution and query functions for catchup workflow.

   SOLID: SRP - Single responsibility for scope-filtered memory queries.
   Extracted from hive-mcp.tools.catchup (Sprint 1 refactoring).

   Contains:
   - distinct-by utility (used by scope-piercing logic)
   - Project name lookup (Emacs integration)
   - Tag filtering
   - Scope-filtered Chroma queries (HCR Wave 4, scope-piercing)
   - Expiring entry detection
   - Composed query helpers (axioms, regular conventions)"
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [clojure.string :as str]
            [clojure.set :as set]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Utility Helpers
;; =============================================================================

(defn distinct-by
  "Return distinct elements from coll by the value of (f item).
   Keeps the first occurrence of each unique (f item) value."
  [f coll]
  (let [seen (volatile! #{})]
    (filterv (fn [item]
               (let [key (f item)]
                 (if (contains? @seen key)
                   false
                   (do (vswap! seen conj key) true))))
             coll)))

(defn- newest-first
  "Sort entries by :created timestamp, newest first.
   SLAP helper — keeps pipelines at a single abstraction level."
  [entries]
  (sort-by :created #(compare %2 %1) entries))

;; =============================================================================
;; Project Context Helpers
;; =============================================================================

;; NOTE: get-current-project-id is provided by scope/get-current-project-id
;; which implements the Go Context Pattern: context flows down explicitly,
;; never derived from ambient Emacs state.

(defn get-current-project-name
  "Get current project name from Emacs.
   When directory is provided, uses that path to determine project context."
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

;; =============================================================================
;; Tag Filtering
;; =============================================================================

(defn filter-by-tags
  "Filter entries to only those containing all specified tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

;; =============================================================================
;; Hierarchy Project ID Helpers (HCR Wave 5)
;; =============================================================================

(defn- compute-hierarchy-project-ids
  "Compute the full set of visible project IDs for DB-level $in filtering.
   Includes ancestors (upward), self, and descendants (downward).

   HCR Wave 5: Pushes scope filtering to Chroma via :project-ids,
   replacing the over-fetch-all + in-memory-filter strategy.
   Uses cached hierarchy tree for descendant resolution (O(1) via tree-cache).

   When in project context, excludes 'global' to prevent scope leak.
   Returns nil for global scope (no DB-level filter needed)."
  [project-id]
  (let [in-project? (and project-id (not= project-id "global"))]
    (when in-project?
      (let [visible (kg-scope/visible-scopes project-id)
            descendants (kg-scope/descendant-scopes project-id)
            all-ids (distinct (concat visible descendants))]
        ;; Exclude "global" when in project context (scope leak fix)
        (vec (remove #(= "global" %) all-ids))))))

(defn- compute-full-scope-tags
  "Compute full hierarchy scope tags for in-memory safety-net filtering.
   When in project context, excludes scope:global to prevent scope leak."
  [project-id]
  (let [in-project? (and project-id (not= project-id "global"))]
    (cond-> (kg-scope/full-hierarchy-scope-tags project-id)
      in-project? (disj "scope:global"))))

(defn- scope-filter-entries
  "Apply in-memory scope filter as belt-and-suspenders safety net.
   Matches entries by scope tags OR project-id metadata."
  [entries scope-tags visible-ids]
  (filter (fn [entry]
            (let [entry-tags (set (or (:tags entry) []))]
              (or
               ;; Match any full hierarchy scope tag
               (some entry-tags scope-tags)
               ;; Also match by project-id metadata for backward compat
               (contains? visible-ids (:project-id entry)))))
          entries))

(defn- scope-pierce-entries
  "Extract axioms and catchup-priority entries that pierce scope boundaries.
   Axioms are INVIOLABLE and must always be visible.
   catchup-priority conventions are explicitly marked for cross-scope visibility.

   Only applies when in project context (global sees everything)."
  [entries project-id]
  (let [in-project? (and project-id (not= project-id "global"))]
    (when in-project?
      (filter (fn [entry]
                (let [entry-tags (set (or (:tags entry) []))
                      entry-type (str (or (:type entry) ""))]
                  (or (= entry-type "axiom")
                      (contains? entry-tags "catchup-priority"))))
              entries))))

;; =============================================================================
;; Scope-Filtered Queries
;; =============================================================================

(defn query-scoped-entries
  "Query Chroma entries filtered by project scope.
   Uses DB-level :project-ids $in filtering AND in-memory scope matching.

   HCR Wave 5: Pushes hierarchy filtering to Chroma via :project-ids,
   dramatically reducing over-fetch. Uses cached hierarchy tree for
   descendant resolution (O(1) via tree-cache).

   Catchup uses full-hierarchy-scope-tags by default, so coordinator
   gets visibility into child project memories too.

   SCOPE LEAK FIX: When in project context, excludes scope:global from
   matching to prevent global memories from leaking into project catchup.
   Global entries are only included when project-id IS 'global'."
  [entry-type tags project-id limit]
  (when (chroma/embedding-configured?)
    (let [limit-val (or limit 20)
          in-project? (and project-id (not= project-id "global"))
          ;; HCR Wave 5: DB-level $in filtering via hierarchy project-ids
          hierarchy-ids (compute-hierarchy-project-ids project-id)
          ;; Reduced over-fetch factor: DB-level filter means less waste
          over-fetch-factor (if hierarchy-ids 3 4)
          entries (chroma/query-entries :type entry-type
                                        :project-ids hierarchy-ids
                                        :limit (min (* limit-val over-fetch-factor) 500))
          ;; Belt-and-suspenders: in-memory scope filter
          full-scope-tags (compute-full-scope-tags project-id)
          all-visible-ids (set (or hierarchy-ids ["global"]))
          scoped (scope-filter-entries entries full-scope-tags all-visible-ids)
          ;; Scope-piercing: axioms and catchup-priority always visible.
          ;; Axioms with project-id "global" won't be in the DB-level results
          ;; when in-project?, so we fetch global entries separately for piercing.
          scope-piercing (when in-project?
                           (let [global-entries (chroma/query-entries :type entry-type
                                                                      :project-id "global"
                                                                      :limit 100)]
                             (scope-pierce-entries global-entries project-id)))
          scoped (distinct-by :id (concat scoped scope-piercing))
          ;; NOTE: filter-by-tags has signature [entries tags], so we can't use ->>
          filtered (filter-by-tags scoped tags)]
      (->> filtered
           newest-first
           (take limit-val)))))

;; =============================================================================
;; Expiring Entries
;; =============================================================================

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
  "Query entries expiring within 7 days, scoped to project.

   HCR Wave 5: Uses DB-level :project-ids $in filtering with cached
   hierarchy tree for descendant resolution. Consistent with
   query-scoped-entries optimization.

   Scope-piercing: Axioms and catchup-priority entries always included
   regardless of scope, since expiring axioms need urgent attention."
  [project-id limit]
  (let [in-project? (and project-id (not= project-id "global"))
        ;; HCR Wave 5: DB-level $in filtering via hierarchy project-ids
        hierarchy-ids (compute-hierarchy-project-ids project-id)
        entries (chroma/query-entries :project-ids hierarchy-ids
                                      :limit 200)
        ;; Belt-and-suspenders: in-memory scope filter
        full-scope-tags (compute-full-scope-tags project-id)
        all-visible-ids (set (or hierarchy-ids ["global"]))
        scoped (scope-filter-entries entries full-scope-tags all-visible-ids)
        ;; Scope-piercing: axioms and catchup-priority always visible for expiring check
        scope-piercing (when in-project?
                         (let [global-entries (chroma/query-entries :project-id "global"
                                                                    :limit 100)]
                           (scope-pierce-entries global-entries project-id)))
        scoped (distinct-by :id (concat scoped scope-piercing))]
    (->> scoped
         (filter entry-expiring-soon?)
         newest-first
         (take (or limit 20)))))

;; =============================================================================
;; Composed Query Helpers
;; =============================================================================

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
