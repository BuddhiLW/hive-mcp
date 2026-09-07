(ns hive-mcp.tools.kanban.list.plan-trifecta-test
  "Schema-synthesized suites for the pure kanban list planner.

   `plan`  : property (conformance + relation), schema-derived mutants,
             golden lock of the seeded cases, input-strength vacuity guard.
   `shape` : same, over a generated board.
   `page-cap`: property + a hand classification table (the three branches
             are gated on key ABSENCE, which a generator reaches too rarely
             for a distribution facet)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-mcp.tools.kanban.filters :as kf]
            [hive-mcp.tools.kanban.list.plan :as plan]
            [hive-mcp.tools.kanban.list.schema :as s]
            [hive-mcp.tools.kanban.predicates :as kp]
            [hive-mcp.tools.kanban.transitions :as kt]))

;; =============================================================================
;; plan : ListRequest -> FetchPlan
;; =============================================================================

(deftrifecta-from-schema plan-trifecta
  hive-mcp.tools.kanban.list.plan/plan
  {:in  s/ListRequest
   :out s/FetchPlan
   :rel (fn [{:keys [status offset limit query tags tag_match] :as req}
             {:keys [required-tags window page post-filters]}]
          (and (= "kanban" (first required-tags))
               (or (nil? status)
                   (some #{(kp/normalize-status status)} required-tags))
               ;; AND-tags are pushed down; OR-tags stay client-side.
               (if (and (seq tags) (= :any (plan/tag-mode tag_match)))
                 (and (= (vec tags) (:or-tags post-filters))
                      (not-any? (set tags) (rest required-tags)))
                 (and (nil? (:or-tags post-filters))
                      (every? (set required-tags) (or tags []))))
               ;; the window theorem: never below the board, never below
               ;; the last row the caller can address.
               (>= window plan/whole-board)
               (>= window (+ (or offset 0) (or limit 0)))
               (= (:offset page) offset)
               (= (:limit page) (plan/page-cap req))
               (= (:query post-filters) query)))
   :golden-path "test/golden/kanban/list/plan.edn"
   :strict-in   true
   :num-tests   150})

;; =============================================================================
;; page-cap : ListRequest -> nil | int
;; =============================================================================

(deftrifecta-from-schema page-cap-trifecta
  hive-mcp.tools.kanban.list.plan/page-cap
  {:in  s/ListRequest
   :out [:maybe s/PageIndex]
   :rel (fn [{:keys [limit status] :as req} cap]
          (cond limit                            (= cap limit)
                (or status (kf/post-filters? req)) (nil? cap)
                :else                            (= cap plan/bare-page-cap)))
   :mutation  false
   :num-tests 150})

(deftest page-cap-classification-table
  (testing "explicit limit wins"
    (is (= 7 (plan/page-cap {:limit 7})))
    (is (= 7 (plan/page-cap {:limit 7 :status "todo"}))))
  (testing "a status or any narrowing filter lifts the cap: the answer is the full match"
    (is (nil? (plan/page-cap {:status "todo"})))
    (is (nil? (plan/page-cap {:query "auth"})))
    (is (nil? (plan/page-cap {:priority "high"})))
    (is (nil? (plan/page-cap {:tags ["x"] :tag_match "any"})))
    (is (nil? (plan/page-cap {:offset 3}))))
  (testing "a bare list is capped for the token budget"
    (is (= plan/bare-page-cap (plan/page-cap {})))
    (is (= plan/bare-page-cap (plan/page-cap {:tags ["x"] :tag_match "all"})))))

;; =============================================================================
;; window : the store window is a theorem, not a tunable
;; =============================================================================

(deftrifecta-from-schema window-trifecta
  hive-mcp.tools.kanban.list.plan/window
  {:in  s/ListRequest
   :out s/Window
   :rel (fn [{:keys [offset limit]} w]
          (and (>= w plan/whole-board)
               (>= w (+ (or offset 0) (or limit 0)))))
   :mutation  false
   :num-tests 150})

;; =============================================================================
;; shape : FetchPlan x Board x multi? -> SlimPage
;; =============================================================================

(deftrifecta-from-schema shape-trifecta
  hive-mcp.tools.kanban.list.plan/shape
  {:in  s/ShapeArgs
   :out s/SlimPage
   :rel (fn [[fetch-plan board multi?] out]
          (let [ids    (set (map :id board))
                {:keys [limit fields]} (:page fetch-plan)
                required (set (:required-tags fetch-plan))]
            (and (<= (count out) (count board))
                 (or (not (and (number? limit) (pos? limit)))
                     (<= (count out) limit))
                 (if (seq fields)
                   (every? #(every? (set (map keyword fields)) (keys %)) out)
                   (and (every? #(contains? ids (:id %)) out)
                        (= out (vec (kt/sort-by-priority-then-created out)))
                        (or (not multi?) (every? #(contains? % :project) out))
                        ;; every answered row carried every required tag
                        (every? (fn [{:keys [id]}]
                                  (let [e (first (filter #(= id (:id %)) board))]
                                    (every? (set (:tags e)) required)))
                                out))))))
   :golden-path "test/golden/kanban/list/shape.edn"
   :num-tests   150})
