(ns hive-mcp.tools.kanban.list.plan
  "Pure planner + shaper for `kanban list` (CPPB Promote stratum).

   `plan`  ListRequest -> FetchPlan. Decides what is pushed to the store
           (required tags), how wide the store window must be, which
           filters run client-side and which page is cut.
   `shape` FetchPlan x Board x multi-project? -> SlimPage. Applies the
           post-filters, projects to the slim shape, sorts, paginates.

   Contract of the window: the store must return the WHOLE scoped board.
   The final order is priority-then-id, not the store's created-desc, so
   any page of the answer may need any row of the board. `whole-board` is
   the one constant list, stats and catchup share for that.
   Rationale: hive memory 20260907074339-258aae45."
  (:require [hive-mcp.tools.kanban.filters :as kf]
            [hive-mcp.tools.kanban.list.schema :as s]
            [hive-mcp.tools.kanban.predicates :as kp]
            [hive-mcp.tools.kanban.transitions :as kt]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def whole-board
  "Store fetch window that covers every scoped board. Shared by list,
   stats and catchup so no caller re-invents a smaller one."
  20000)

(def bare-page-cap
  "Rows a bare, unfiltered, unlimited list answers with (token budget)."
  100)

(defn tag-mode
  "Normalise `tag_match` (string, keyword or nil) to :all | :any."
  [tag_match]
  (if (= :any (some-> tag_match name keyword)) :any :all))

(defn required-tags
  "Tags pushed into the store query: kanban, the normalised status, and
   the AND-tags. These filter the whole scoped board server-side."
  [{:keys [status tags tag_match]}]
  (vec (concat ["kanban"]
               (when status [(kp/normalize-status status)])
               (when (and (= :all (tag-mode tag_match)) (seq tags)) tags))))

(defn page-cap
  "Rows the answer is cut to: the caller's limit; nil (all matches) when a
   status or any post-filter narrows the request; else `bare-page-cap`."
  [{:keys [status limit] :as req}]
  (or limit
      (when-not (or status (kf/post-filters? req)) bare-page-cap)))

(defn window
  "Store window: never below `whole-board`, never below the last row the
   caller can address (offset + limit)."
  [{:keys [offset limit]}]
  (max whole-board (+ (or offset 0) (or limit 0))))

(defn plan
  "Derive the FetchPlan for a ListRequest."
  [{:keys [tags tag_match query priority created_after updated_after
           offset fields]
    :as req}]
  {:required-tags (required-tags req)
   :window        (window req)
   :post-filters  {:query         query
                   :priority      priority
                   :created_after created_after
                   :updated_after updated_after
                   :or-tags       (when (and (= :any (tag-mode tag_match)) (seq tags))
                                    (vec tags))}
   :page          {:offset offset
                   :limit  (page-cap req)
                   :fields (when (seq fields) (vec fields))}})

(defn tagged-with?
  "True iff the entry carries every tag in `required`."
  [entry required]
  (let [ts (set (:tags entry))]
    (every? #(contains? ts %) required)))

(defn select-tagged
  "Kanban entries carrying every required tag."
  [entries required]
  (->> entries
       (filter #(tagged-with? % required))
       (filter kp/kanban-entry?)))

(defn shape
  "Apply the plan's post-filters and page to a fetched board."
  [{:keys [required-tags post-filters page]} entries multi-project?]
  (let [{:keys [query priority created_after updated_after or-tags]} post-filters
        {:keys [offset limit fields]} page]
    (->> (select-tagged entries required-tags)
         (filter #(kf/entry-tags-match? % or-tags :any))
         (filter #(kf/entry-matches-query? % query))
         (filter #(kf/entry-priority? % priority))
         (filter #(kf/entry-after-ts? % :created created_after))
         (filter #(kf/entry-after-ts? % :updated updated_after))
         (mapv #(kt/task->slim % multi-project?))
         kt/sort-by-priority-then-created
         (#(kf/paginate % offset limit))
         (mapv #(kf/project-fields % fields)))))

(m/=> required-tags [:=> [:cat s/ListRequest] [:vector {:min 1} s/Tag]])
(m/=> page-cap      [:=> [:cat s/ListRequest] [:maybe s/PageIndex]])
(m/=> window        [:=> [:cat s/ListRequest] s/Window])
(m/=> plan          [:=> [:cat s/ListRequest] s/FetchPlan])
(m/=> shape         [:=> s/ShapeArgs s/SlimPage])
