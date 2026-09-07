(ns hive-mcp.tools.kanban.list.schema
  "Malli value objects for the kanban list pipeline.

   ListRequest  what an MCP caller sends (boundary shape: every key optional,
                nil tolerated).
   FetchPlan    what the pure planner derives from it: tags pushed to the
                store, the store window, the client-side post-filters and
                the page to cut.
   Entry        a kanban row as the store returns it.
   SlimTask     a row as `kanban list` answers it.

   Generators are bounded (`:max` on ints, vectors and strings) so the
   synthesized suites stay fast.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Status
  "MCP enum spellings plus the canonical internal tags."
  [:enum "todo" "inprogress" "inreview" "done" "doing" "review"])

(def Priority [:enum "high" "medium" "low"])

(def TagMode [:enum "all" "any"])

(def Tag [:string {:min 1 :max 24}])

(def Tags [:vector {:max 4} Tag])

(def Timestamp
  "ISO-8601-ish string; the post-filters compare these lexically."
  [:string {:min 1 :max 24}])

(def Field [:enum "id" "title" "status" "priority" "project"])

(def Fields [:vector {:max 5} Field])

(def PageIndex [:int {:min 0 :max 10000}])

(def ListRequest
  [:map
   [:status        {:optional true} [:maybe Status]]
   [:tags          {:optional true} [:maybe Tags]]
   [:tag_match     {:optional true} [:maybe TagMode]]
   [:query         {:optional true} [:maybe [:string {:max 12}]]]
   [:priority      {:optional true} [:maybe Priority]]
   [:created_after {:optional true} [:maybe Timestamp]]
   [:updated_after {:optional true} [:maybe Timestamp]]
   [:limit         {:optional true} [:maybe PageIndex]]
   [:offset        {:optional true} [:maybe PageIndex]]
   [:fields        {:optional true} [:maybe Fields]]])

(def Window [:int {:min 1}])

(def PostFilters
  [:map {:closed true}
   [:query         [:maybe :string]]
   [:priority      [:maybe Priority]]
   [:created_after [:maybe Timestamp]]
   [:updated_after [:maybe Timestamp]]
   [:or-tags       [:maybe Tags]]])

(def Page
  [:map {:closed true}
   [:offset [:maybe PageIndex]]
   [:limit  [:maybe PageIndex]]
   [:fields [:maybe Fields]]])

(def FetchPlan
  [:map {:closed true}
   [:required-tags [:vector {:min 1} Tag]]
   [:window        Window]
   [:post-filters  PostFilters]
   [:page          Page]])

(def Content
  [:map
   [:task-type   [:= "kanban"]]
   [:title       [:maybe [:string {:max 12}]]]
   [:status      [:maybe Status]]
   [:priority    [:maybe Priority]]
   [:description {:optional true} [:maybe [:string {:max 12}]]]
   [:created     {:optional true} [:maybe Timestamp]]])

(def Entry
  [:map
   [:id      [:string {:min 1 :max 24}]]
   [:tags    [:vector {:max 6} Tag]]
   [:content Content]])

(def Board [:vector {:max 24} Entry])

(def SlimTask
  [:map
   [:id       {:optional true} :string]
   [:title    {:optional true} [:maybe :string]]
   [:status   {:optional true} [:maybe :string]]
   [:priority {:optional true} [:maybe :string]]
   [:project  {:optional true} [:maybe :string]]])

(def SlimPage [:vector SlimTask])

(def ShapeArgs
  "Arglist of `plan/shape`: the plan, the fetched board, the multi-project flag."
  [:cat FetchPlan Board :boolean])
