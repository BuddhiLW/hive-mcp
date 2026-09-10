(ns hive-mcp.tools.consolidated.workflow.forge-survey-verb-test
  "forge-ops/survey computes plan membership, per-card states and dependency
   readiness, and for a long time NOTHING public called it: its two non-test
   callers are both on the strike path. So the only way to ask 'what would this
   plan select' was to run a strike, and `forge survey` silently fell through
   the :forge :_handler to the belt dashboard, answering with global kanban
   totals. Every attempt to verify plan scoping read-only therefore looked like
   a deployment gap when it was a missing verb.

   These tests pin the verb, and pin that adding it did not move `forge status`."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.workflow :as wf]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest survey-is-its-own-verb-not-the-belt-dashboard
  (let [forge (:forge wf/canonical-handlers)]
    (is (contains? forge :survey)
        "without this key the dispatcher falls through :_handler to status, and
         a plan-scoped question comes back answered with global totals")
    (is (not= (:survey forge) (:status forge))
        "status reports the BELT, survey reports the SELECTION")
    (is (= (:status forge) (:_handler forge))
        "the fallback stays what it was: adding a verb must not move the
         meaning of a bare `forge` call")))

(deftest the-verb-is-advertised
  (let [enum (get-in wf/tool-def [:inputSchema :properties "command" :enum])]
    (is (some #{"forge survey"} enum)
        "a verb absent from the schema is unreachable through the tool surface
         even when the handler exists")
    (testing "the verbs that were already there are still there"
      (is (some #{"forge status"} enum))
      (is (some #{"forge strike"} enum))
      (is (some #{"forge quench"} enum)))))

(deftest survey-reports-a-failure-as-a-result-rather-than-throwing
  (testing "forge-ops/survey THROWS when a plan_id cannot be resolved, by design
            (a plan survey that silently selects nothing is worse than an
            error). The handler must turn that into an MCP error result, not
            let it escape into the tool loop."
    (let [handler (get-in wf/canonical-handlers [:forge :survey])
          res     (handler {:plan_id "no-such-plan-id-20260910"
                            :directory "/home/leibniz/PP/hive"})]
      (is (map? res))
      (is (contains? res :isError)
          "an unresolvable plan is reported, and reported as an error"))))
