(ns hive-mcp.workflows.forge-belt-test
  "Tests for the Forge Belt FSM spec.

   Tests the PURE handler layer — no side effects, no DataScript, no Emacs.
   All external dependencies are injected via mock resources.

   Test categories:
   1. Dispatch predicates — pure boolean functions of state data
   2. Handler unit tests — each handler with mock resources
   3. FSM integration — full compile+run with mock resource chain
   4. EDN spec — round-trip through EDN with handler map

   SOLID: D — Tests depend on abstractions (resources), not concretions.
   CLARITY: T — Telemetry via test assertions."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.workflows.forge-belt :as belt]
            [hive.events.fsm :as fsm]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Helpers — Mock Resources
;; =============================================================================

(def fixed-clock
  "Deterministic clock for reproducible tests."
  #(java.time.Instant/parse "2026-02-07T12:00:00Z"))

(defn mock-resources
  "Build a mock resources map for handler tests.

   Options:
     :kill-result    — return value for agent-ops :kill-fn
     :survey-result  — return value for kanban-ops :list-fn
     :spawn-result   — return value for agent-ops :spawn-fn
     :directory      — working directory (default: /tmp/test)
     :max-slots      — max concurrent lings (default: 5)
     :presets        — ling presets (default: [\"ling\"])"
  ([] (mock-resources {}))
  ([{:keys [kill-result survey-result spawn-result
            directory max-slots presets]
     :or {kill-result   {:smited [] :failed [] :count 0}
          survey-result {:tasks [] :count 0}
          spawn-result  {:spawned [] :failed [] :count 0}
          directory     "/tmp/test"
          max-slots     5
          presets       ["ling"]}}]
   {:ds-conn    nil
    :directory  directory
    :config     {:max-slots max-slots :presets presets}
    :agent-ops  {:kill-fn      (fn [_dir _pid] kill-result)
                 :spawn-fn     (fn [_opts] spawn-result)
                 :dispatch-fn  (fn [_id _prompt] nil)
                 :wait-ready-fn (fn [_id] {:ready? true})}
    :kanban-ops {:list-fn   (fn [_dir] survey-result)
                 :update-fn (fn [_opts] nil)}
    :scope-fn   (fn [_dir] "test-project")
    :clock-fn   fixed-clock}))

;; =============================================================================
;; 1. Dispatch Predicate Tests
;; =============================================================================

(deftest test-quenched?
  (testing "quenched? returns true when :quenched? is true"
    (is (true? (belt/quenched? {:quenched? true})))
    (is (false? (belt/quenched? {:quenched? false})))
    (is (false? (belt/quenched? {})))))

(deftest test-has-tasks?
  (testing "has-tasks? checks survey-result :count"
    (is (true? (belt/has-tasks? {:survey-result {:count 3}})))
    (is (false? (belt/has-tasks? {:survey-result {:count 0}})))
    (is (false? (belt/has-tasks? {})))))

(deftest test-no-tasks?
  (testing "no-tasks? is the inverse of has-tasks?"
    (is (true? (belt/no-tasks? {:survey-result {:count 0}})))
    (is (true? (belt/no-tasks? {})))
    (is (false? (belt/no-tasks? {:survey-result {:count 1}})))))

(deftest test-continuous?
  (testing "continuous? checks :continuous? flag"
    (is (true? (belt/continuous? {:continuous? true})))
    (is (false? (belt/continuous? {:continuous? false})))
    (is (false? (belt/continuous? {})))))

(deftest test-single-shot?
  (testing "single-shot? is the inverse of continuous?"
    (is (true? (belt/single-shot? {:continuous? false})))
    (is (true? (belt/single-shot? {})))
    (is (false? (belt/single-shot? {:continuous? true})))))

(deftest test-always
  (testing "always returns true for any input"
    (is (true? (belt/always nil)))
    (is (true? (belt/always {})))
    (is (true? (belt/always {:anything true})))))

;; =============================================================================
;; 2. Handler Unit Tests
;; =============================================================================

(deftest test-handle-start
  (testing "handle-start initializes cycle state"
    (let [resources (mock-resources)
          data {:quenched? false :strike-count 0}
          result (belt/handle-start resources data)]
      (is (= ::belt/smite (:phase result)) "Sets phase to ::smite")
      (is (some? (:cycle-start result)) "Sets cycle-start timestamp")
      (is (= "2026-02-07T12:00:00Z" (:cycle-start result)) "Uses clock-fn")
      (is (nil? (:smite-result result)) "Clears smite-result")
      (is (nil? (:survey-result result)) "Clears survey-result")
      (is (nil? (:spark-result result)) "Clears spark-result")
      (is (nil? (:error result)) "Clears error"))))

(deftest test-handle-start-preserves-existing-state
  (testing "handle-start preserves fields it doesn't touch"
    (let [resources (mock-resources)
          data {:quenched? false :strike-count 5 :total-smited 10}
          result (belt/handle-start resources data)]
      (is (= 5 (:strike-count result)) "Preserves strike-count")
      (is (= 10 (:total-smited result)) "Preserves total-smited"))))

(deftest test-handle-smite
  (testing "handle-smite calls kill-fn and records results"
    (let [kill-result {:smited [{:id "a1" :killed true}] :failed [] :count 1}
          resources (mock-resources {:kill-result kill-result})
          data {:phase ::belt/smite :total-smited 3}
          result (belt/handle-smite resources data)]
      (is (= ::belt/survey (:phase result)) "Advances phase to ::survey")
      (is (= kill-result (:smite-result result)) "Records smite result")
      (is (= 4 (:total-smited result)) "Increments total-smited"))))

(deftest test-handle-smite-zero-kills
  (testing "handle-smite with no kills increments by 0"
    (let [resources (mock-resources {:kill-result {:smited [] :failed [] :count 0}})
          data {:phase ::belt/smite :total-smited 5}
          result (belt/handle-smite resources data)]
      (is (= 5 (:total-smited result)) "total-smited unchanged"))))

(deftest test-handle-survey
  (testing "handle-survey calls list-fn and records tasks"
    (let [tasks [{:id "t1" :title "Task 1"} {:id "t2" :title "Task 2"}]
          survey-result {:tasks tasks :count 2}
          resources (mock-resources {:survey-result survey-result})
          data {:phase ::belt/survey}
          result (belt/handle-survey resources data)]
      (is (= ::belt/spark (:phase result)) "Advances phase to ::spark")
      (is (= survey-result (:survey-result result)) "Records survey result")
      (is (= 2 (get-in result [:survey-result :count])) "Has correct task count"))))

(deftest test-handle-survey-empty
  (testing "handle-survey with no tasks returns count 0"
    (let [resources (mock-resources {:survey-result {:tasks [] :count 0}})
          result (belt/handle-survey resources {:phase ::belt/survey})]
      (is (= 0 (get-in result [:survey-result :count]))))))

(deftest test-handle-spark
  (testing "handle-spark calls spawn-fn and records results"
    (let [spawn-result {:spawned [{:agent-id "f1"}] :failed [] :count 1}
          resources (mock-resources {:spawn-result spawn-result})
          data {:phase ::belt/spark
                :survey-result {:tasks [{:id "t1" :title "Task 1"}] :count 1}
                :total-sparked 2
                :strike-count 0}
          result (belt/handle-spark resources data)]
      (is (= ::belt/cycle-complete (:phase result)) "Advances phase to ::cycle-complete")
      (is (= spawn-result (:spark-result result)) "Records spark result")
      (is (= 3 (:total-sparked result)) "Increments total-sparked")
      (is (= 1 (:strike-count result)) "Increments strike-count")
      (is (= "2026-02-07T12:00:00Z" (:last-strike result)) "Records last-strike time"))))

(deftest test-handle-spark-no-tasks
  (testing "handle-spark with empty survey still produces valid output"
    (let [resources (mock-resources {:spawn-result {:spawned [] :failed [] :count 0}})
          data {:phase ::belt/spark
                :survey-result {:tasks [] :count 0}
                :total-sparked 0
                :strike-count 0}
          result (belt/handle-spark resources data)]
      (is (= 0 (:total-sparked result)) "total-sparked unchanged")
      (is (= 1 (:strike-count result)) "strike-count still increments"))))

(deftest test-handle-end
  (testing "handle-end returns summary subset of data"
    (let [data {:strike-count 3
                :total-smited 5
                :total-sparked 8
                :last-strike "2026-02-07T12:00:00Z"
                :smite-result {:count 2}
                :survey-result {:count 3}
                :spark-result {:count 2}
                :quenched? false
                :continuous? false
                ;; Extra fields that should NOT be in output
                :phase ::belt/cycle-complete
                :cycle-start "earlier"
                :trace-log []}
          result (belt/handle-end nil {:data data})]
      (is (= 3 (:strike-count result)))
      (is (= 5 (:total-smited result)))
      (is (= 8 (:total-sparked result)))
      (is (= "2026-02-07T12:00:00Z" (:last-strike result)))
      (is (contains? result :smite-result))
      (is (contains? result :survey-result))
      (is (contains? result :spark-result))
      (is (contains? result :quenched?))
      (is (contains? result :continuous?))
      ;; Should NOT contain internal fields
      (is (not (contains? result :phase)))
      (is (not (contains? result :cycle-start)))
      (is (not (contains? result :trace-log))))))

(deftest test-handle-halt
  (testing "handle-halt sets phase to ::halted and removes :fsm key"
    (let [fsm {:data {:phase ::belt/smite :quenched? true}
               :fsm {:some :graph}
               :current-state-id ::belt/halt}
          result (belt/handle-halt nil fsm)]
      (is (= ::belt/halted (get-in result [:data :phase])) "Sets phase to ::halted")
      (is (not (contains? result :fsm)) "Removes :fsm key for serialization"))))

(deftest test-handle-error
  (testing "handle-error throws ex-info with context"
    (let [fsm {:data {:phase ::belt/smite}
               :error (Exception. "test boom")}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Forge belt error"
                            (belt/handle-error nil fsm))))))

;; =============================================================================
;; 3. FSM Integration Tests (compile + run with mocks)
;; =============================================================================

(deftest test-single-strike-cycle
  (testing "Single strike: start → smite → survey → spark → end"
    (let [resources (mock-resources
                     {:kill-result   {:smited [{:id "old"}] :failed [] :count 1}
                      :survey-result {:tasks [{:id "t1" :title "Build X"}] :count 1}
                      :spawn-result  {:spawned [{:agent-id "forja-1"}] :failed [] :count 1}})
          result (belt/run-single-strike resources)]
      (is (= 1 (:strike-count result)) "One strike completed")
      (is (= 1 (:total-smited result)) "One ling smited")
      (is (= 1 (:total-sparked result)) "One ling sparked")
      (is (some? (:last-strike result)) "Records last strike time")
      (is (false? (:continuous? result)) "Single-shot mode"))))

(deftest test-single-strike-no-tasks
  (testing "Single strike with no tasks ends early after survey"
    (let [resources (mock-resources
                     {:kill-result   {:smited [] :failed [] :count 0}
                      :survey-result {:tasks [] :count 0}})
          result (belt/run-single-strike resources)]
      ;; When no tasks, survey dispatches to ::end, never reaches ::spark
      (is (= 0 (:total-sparked result)) "No lings sparked")
      (is (= 0 (:total-smited result)) "No lings smited"))))

(deftest test-continuous-belt-exhausts-tasks
  (testing "Continuous belt loops until no tasks remain"
    (let [call-count (atom 0)
          ;; Survey returns tasks for first 2 calls, then empty
          dynamic-survey (fn [_dir]
                           (let [n (swap! call-count inc)]
                             (if (<= n 2)
                               {:tasks [{:id (str "t" n) :title (str "Task " n)}] :count 1}
                               {:tasks [] :count 0})))
          resources (assoc (mock-resources {:kill-result {:smited [] :failed [] :count 0}
                                            :spawn-result {:spawned [{:agent-id "f1"}] :failed [] :count 1}})
                           :kanban-ops {:list-fn   dynamic-survey
                                        :update-fn (fn [_] nil)})
          result (belt/run-continuous-belt resources)]
      ;; Should have run 2 full cycles, then ended when survey returned 0
      (is (= 2 (:strike-count result)) "Two strikes before exhaustion")
      (is (= 2 (:total-sparked result)) "Two lings sparked total")
      (is (true? (:continuous? result)) "Was in continuous mode"))))

(deftest test-quench-halts-belt
  (testing "Quenched belt halts at start of next cycle"
    (let [resources (mock-resources)
          compiled (belt/compile-belt)
          ;; Start already quenched
          result (belt/run-belt compiled resources {:quenched? true :continuous? true})]
      ;; When quenched? is true at ::start, dispatches to ::fsm/halt
      ;; The FSM engine's built-in halt handler returns FSM state minus :fsm key
      (is (map? result) "Returns halted FSM state")
      (is (not (contains? result :fsm)) "No :fsm key (stripped by halt handler)")
      (is (some? (:data result)) "Has :data key with last state")
      (is (true? (get-in result [:data :quenched?])) "Data preserves quenched flag"))))

;; =============================================================================
;; 4. FSM Spec Structure Tests
;; =============================================================================

(deftest test-forge-belt-spec-structure
  (testing "forge-belt-spec has correct shape"
    (let [spec belt/forge-belt-spec]
      (is (map? (:fsm spec)) "Has :fsm key")
      (is (map? (:opts spec)) "Has :opts key")
      (is (contains? (:fsm spec) ::fsm/start) "Has ::start state")
      (is (contains? (:fsm spec) ::belt/smite) "Has ::smite state")
      (is (contains? (:fsm spec) ::belt/survey) "Has ::survey state")
      (is (contains? (:fsm spec) ::belt/spark) "Has ::spark state")
      (is (contains? (:fsm spec) ::fsm/end) "Has ::end state")
      (is (contains? (:fsm spec) ::fsm/halt) "Has ::halt state")
      (is (contains? (:fsm spec) ::fsm/error) "Has ::error state"))))

(deftest test-compile-belt-succeeds
  (testing "compile-belt produces a valid compiled FSM"
    (let [compiled (belt/compile-belt)]
      (is (map? compiled) "Returns a map")
      (is (contains? compiled :fsm) "Has :fsm key")
      (is (contains? compiled :opts) "Has :opts key"))))

(deftest test-subscriptions-in-spec
  (testing "Spec has subscriptions for total-smited and total-sparked"
    (let [subs (get-in belt/forge-belt-spec [:opts :subscriptions])]
      (is (contains? subs [:total-smited]) "Watches :total-smited")
      (is (contains? subs [:total-sparked]) "Watches :total-sparked"))))

;; =============================================================================
;; 5. Pre/Post Hook Tests
;; =============================================================================

(deftest test-pre-hook-adds-trace-log
  (testing "Pre hook appends trace-log entries"
    (let [pre-fn (get-in belt/forge-belt-spec [:opts :pre])
          fsm {:current-state-id ::belt/smite
               :data {:trace-log []}}
          result (pre-fn fsm nil)]
      (is (= 1 (count (get-in result [:data :trace-log]))) "Added one trace entry")
      (is (= ::belt/smite (-> result :data :trace-log first :state)) "Recorded state")
      (is (= :enter (-> result :data :trace-log first :direction)) "Direction is :enter"))))

(deftest test-post-hook-adds-trace-log
  (testing "Post hook appends trace-log entries"
    (let [post-fn (get-in belt/forge-belt-spec [:opts :post])
          fsm {:current-state-id ::belt/spark
               :data {:trace-log [{:state ::belt/smite :direction :enter}]}}
          result (post-fn fsm nil)]
      (is (= 2 (count (get-in result [:data :trace-log]))) "Added second trace entry")
      (is (= :exit (-> result :data :trace-log second :direction)) "Direction is :exit"))))
