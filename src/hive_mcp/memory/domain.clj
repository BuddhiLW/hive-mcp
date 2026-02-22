(ns hive-mcp.memory.domain
  "ScopeFilter ADT — closed sum type for memory scope filtering.

   Replaces stringly-typed scope params ('all', 'global', nil, project-id)
   with an exhaustive ADT. Parse once at IO boundary, dispatch via adt-case.

   Variants:
     :scope/all     — no filtering (show everything)
     :scope/global  — only scope:global entries
     :scope/project — specific project (+ hierarchy)
     :scope/auto    — auto-derived from directory"
  (:require [hive-dsl.adt :refer [defadt adt-case]]
            [hive-mcp.knowledge-graph.scope :as kg-scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defadt ScopeFilter
  "Scope filter for memory queries — closed sum type."
  :scope/all
  :scope/global
  [:scope/project {:project-id string?}]
  [:scope/auto    {:project-id string?}])

;; =============================================================================
;; Boundary Parsing
;; =============================================================================

(defn parse-scope
  "Parse raw scope string + project-id into a ScopeFilter ADT value.
   Called once at IO boundary (handle-query, handle-search-semantic)."
  [scope-str project-id]
  (cond
    (= scope-str "all")    (scope-filter :scope/all)
    (= scope-str "global") (scope-filter :scope/global)
    (some? scope-str)       (scope-filter :scope/project {:project-id scope-str})
    :else                   (scope-filter :scope/auto {:project-id project-id})))

;; =============================================================================
;; Projection Helpers
;; =============================================================================

(defn scope->project-ids
  "Compute visible project-ids for DB-level filtering.
   Returns [string?] or nil (nil = no filtering)."
  [sf include-descendants?]
  (adt-case ScopeFilter sf
    :scope/all     nil
    :scope/global  ["global"]
    :scope/project (let [pid (:project-id sf)
                         visible (kg-scope/visible-scopes pid)
                         descendants (when include-descendants?
                                       (kg-scope/descendant-scopes pid))]
                     (vec (distinct (concat visible descendants))))
    :scope/auto    (let [pid (:project-id sf)
                         in-project? (and pid (not= pid "global"))
                         visible (kg-scope/visible-scopes pid)
                         filtered (if in-project?
                                    (vec (remove #(= "global" %) visible))
                                    visible)
                         descendants (when include-descendants?
                                       (kg-scope/descendant-scopes pid))]
                     (vec (distinct (concat filtered descendants))))))

(defn scope->tag-set
  "Compute visible scope tags for in-memory filtering.
   Returns #{string?} or nil (nil = no filtering / pass all)."
  [sf include-descendants?]
  (adt-case ScopeFilter sf
    :scope/all     nil
    :scope/global  #{"scope:global"}
    :scope/project (if include-descendants?
                     (kg-scope/full-hierarchy-scope-tags (:project-id sf))
                     (kg-scope/visible-scope-tags (:project-id sf)))
    :scope/auto    (if include-descendants?
                     (kg-scope/full-hierarchy-scope-tags (:project-id sf))
                     (kg-scope/visible-scope-tags (:project-id sf)))))

(defn scope->effective
  "Compute [effective-pid in-project?] for search dispatch.
   Used by search-semantic* to determine filtering mode."
  [sf]
  (adt-case ScopeFilter sf
    :scope/all     [nil false]
    :scope/global  ["global" false]
    :scope/project [(:project-id sf) true]
    :scope/auto    (let [pid (:project-id sf)]
                     [pid (and pid (not= pid "global"))])))
