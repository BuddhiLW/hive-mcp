(ns hive-mcp.agent.saa.fsm-orchestrator-test
  "Tests for FSMSAAOrchestrator — the FSM-backed ISAAOrchestrator bridge.

   Covers:
   - Individual phase methods (run-silence!, run-abstract!, run-act!)
   - Full SAA cycle (run-full-saa!)
   - Error handling (missing resources, handler exceptions)
   - Protocol compliance (returns channels, correct message shapes)
   - Silence loop (grounding retry)
   - Abstract retry (plan validation retry)

   Tests run via nREPL per project axiom:
   (require '[clojure.test :refer [run-tests]])
   (run-tests 'hive-mcp.agent.saa.fsm-orchestrator-test)"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.async :as async :refer [<!! close!]]
            [hive-mcp.agent.saa.fsm-orchestrator :as sut]
            [hive-mcp.protocols.agent-bridge :as bridge]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures & Helpers
;; =============================================================================

(def ^:private fixed-clock
  (java.time.Instant/parse "2026-02-08T12:00:00Z"))

(defn- mock-config
  "Build a mock config map with sensible defaults for FSMSAAOrchestrator."
  ([] (mock-config {}))
  ([overrides]
   (merge
    {:scope-fn           (constantly "test-project")
     :catchup-fn         (fn [_agent-id _dir]
                           {:axioms      [{:id "ax-1" :content "Test"}]
                            :conventions []
                            :decisions   []})
     :explore-fn         (fn [_task _agent-id existing-obs]
                           {:observations (conj (or existing-obs [])
                                                {:type :file :content "foo.clj"})
                            :files-read  2
                            :discoveries 1})
     :score-grounding-fn (fn [obs files]
                           (min 1.0
                                (+ (if (seq obs) 0.3 0.0)
                                   (min 0.4 (* 0.1 (count obs)))
                                   (if (pos? (or files 0)) 0.3 0.0))))
     :synthesize-fn      (fn [task _obs _ctx]
                           {:id    "plan-test"
                            :title (str "Plan: " task)
                            :steps [{:id "s1" :title "Step 1" :files ["a.clj"] :wave 1}]
                            :waves {:wave-1 {:steps ["s1"] :parallel false}}})
     :validate-plan-fn   (fn [plan]
                           (if (seq (:steps plan))
                             {:valid? true :errors []}
                             {:valid? false :errors ["No steps"]}))
     :store-plan-fn      (fn [_plan _agent-id _dir]
                           {:memory-id "mem-test" :kanban-ids ["t-1"] :kg-edges 2})
     :dispatch-fn        (fn [_plan _mode _agent-id]
                           {:wave-id "wave-test" :result {:status :completed}})
     :verify-fn          (fn [_result _plan]
                           {:passed? true :details {:tests-run 3 :tests-passed 3}})
     :shout-fn           nil
     :clock-fn           (constantly fixed-clock)
     :directory          "/test/project"}
    overrides)))

(defn- mock-session
  "Create a NoopAgentSession for testing."
  ([] (mock-session "test-agent"))
  ([id] (bridge/->noop-session id)))

(defn- drain-ch
  "Drain all messages from a channel, return as vector."
  [ch]
  (loop [msgs []]
    (if-let [msg (<!! ch)]
      (recur (conj msgs msg))
      msgs)))

(defn- last-msg
  "Get the final message from a channel (drains all)."
  [ch]
  (last (drain-ch ch)))

;; =============================================================================
;; Factory Tests
;; =============================================================================

(deftest factory-test
  (testing "->fsm-saa-orchestrator creates valid instance"
    (let [orch (sut/->fsm-saa-orchestrator)]
      (is (instance? hive_mcp.agent.saa.fsm_orchestrator.FSMSAAOrchestrator orch))
      (is (satisfies? bridge/ISAAOrchestrator orch))))

  (testing "->fsm-saa-orchestrator with config preserves config"
    (let [cfg  (mock-config)
          orch (sut/->fsm-saa-orchestrator cfg)]
      (is (= cfg (:config orch))))))

;; =============================================================================
;; run-silence! Tests
;; =============================================================================

(deftest run-silence-happy-path-test
  (testing "run-silence! returns channel with phase-complete message"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          ch   (bridge/run-silence! orch (mock-session) "Fix auth bug" {})
          msg  (last-msg ch)]
      (is (= :phase-complete (:type msg)))
      (is (= :silence (:saa-phase msg)))
      (is (seq (:observations msg)))
      (is (number? (:grounding-score msg)))
      (is (pos? (:silence-iterations msg))))))

(deftest run-silence-emits-progress-test
  (testing "run-silence! emits progress messages via shout-fn"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          ch   (bridge/run-silence! orch (mock-session) "Explore codebase" {})
          msgs (drain-ch ch)
          progress-msgs (filter #(= :progress (:type %)) msgs)]
      ;; Should have progress shouts from handlers
      (is (pos? (count progress-msgs))))))

(deftest run-silence-error-missing-task-test
  (testing "run-silence! returns error when task causes handler failure"
    (let [cfg  (mock-config {:explore-fn (fn [_ _ _] (throw (Exception. "boom")))})
          orch (sut/->fsm-saa-orchestrator cfg)
          ch   (bridge/run-silence! orch (mock-session) "Bad task" {})
          msg  (last-msg ch)]
      (is (= :error (:type msg)))
      (is (= :silence (:saa-phase msg))))))

(deftest run-silence-grounding-loop-test
  (testing "run-silence! retries when grounding insufficient"
    (let [call-count (atom 0)
          cfg (mock-config
               {:explore-fn (fn [_ _ obs]
                              (swap! call-count inc)
                              {:observations (conj (or obs []) {:type :file})
                               :files-read @call-count
                               :discoveries 1})
                :score-grounding-fn (fn [_ _] 0.2)}) ;; always low
          orch (sut/->fsm-saa-orchestrator cfg)
          ch   (bridge/run-silence! orch (mock-session) "Deep explore" {})
          msg  (last-msg ch)]
      ;; Should loop to max iterations (3) then proceed
      (is (= :phase-complete (:type msg)))
      (is (>= @call-count 3)))))

;; =============================================================================
;; run-abstract! Tests
;; =============================================================================

(deftest run-abstract-happy-path-test
  (testing "run-abstract! returns channel with plan"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          obs  [{:type :file :content "foo.clj"} {:type :pattern :content "ring"}]
          ch   (bridge/run-abstract! orch (mock-session) obs {:task "Fix auth"})
          msg  (last-msg ch)]
      (is (= :phase-complete (:type msg)))
      (is (= :abstract (:saa-phase msg)))
      (is (some? (:plan msg)))
      (is (true? (:plan-valid? msg)))
      (is (= "mem-test" (:plan-memory-id msg))))))

(deftest run-abstract-retry-then-succeed-test
  (testing "run-abstract! retries on invalid plan, succeeds on second try"
    (let [synth-count (atom 0)
          cfg (mock-config
               {:synthesize-fn (fn [task _ _]
                                 (swap! synth-count inc)
                                 (if (= 1 @synth-count)
                                   {:id "bad" :title "Bad" :steps []}
                                   {:id "good" :title "Good"
                                    :steps [{:id "s1" :title "OK"}]}))})
          orch (sut/->fsm-saa-orchestrator cfg)
          ch   (bridge/run-abstract! orch (mock-session) [{:type :file}] {:task "T"})
          msg  (last-msg ch)]
      (is (= :phase-complete (:type msg)))
      (is (= 2 @synth-count)))))

(deftest run-abstract-max-retries-error-test
  (testing "run-abstract! errors after max retries with invalid plan"
    (let [cfg (mock-config
               {:synthesize-fn (fn [_ _ _]
                                 {:id "bad" :title "Bad" :steps []})})
          orch (sut/->fsm-saa-orchestrator cfg)
          ch   (bridge/run-abstract! orch (mock-session) [{:type :file}] {:task "T"})
          msg  (last-msg ch)]
      (is (= :error (:type msg)))
      (is (= :abstract (:saa-phase msg))))))

(deftest run-abstract-no-synthesize-fn-test
  (testing "run-abstract! errors when no synthesize-fn"
    (let [cfg  (mock-config {:synthesize-fn nil})
          orch (sut/->fsm-saa-orchestrator cfg)
          ch   (bridge/run-abstract! orch (mock-session) [{:type :file}] {:task "T"})
          msg  (last-msg ch)]
      (is (= :error (:type msg))))))

;; =============================================================================
;; run-act! Tests
;; =============================================================================

(deftest run-act-happy-path-test
  (testing "run-act! dispatches and verifies"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          plan {:id "p1" :steps [{:id "s1"}]}
          ch   (bridge/run-act! orch (mock-session) plan {:task "Execute"})
          msg  (last-msg ch)]
      (is (= :phase-complete (:type msg)))
      (is (= :act (:saa-phase msg)))
      (is (true? (:tests-passed? msg)))
      (is (some? (:result msg))))))

(deftest run-act-dispatch-error-test
  (testing "run-act! returns error when dispatch fails"
    (let [cfg  (mock-config {:dispatch-fn (fn [_ _ _] (throw (Exception. "dispatch boom")))})
          orch (sut/->fsm-saa-orchestrator cfg)
          ch   (bridge/run-act! orch (mock-session) {:id "p1"} {:task "T"})
          msg  (last-msg ch)]
      (is (= :error (:type msg)))
      (is (= :act (:saa-phase msg))))))

(deftest run-act-verify-failure-test
  (testing "run-act! reports test failure"
    (let [cfg  (mock-config {:verify-fn (fn [_ _] {:passed? false :details {:tests-run 3 :tests-passed 1}})})
          orch (sut/->fsm-saa-orchestrator cfg)
          ch   (bridge/run-act! orch (mock-session) {:id "p1" :steps [{:id "s1"}]} {:task "T"})
          msg  (last-msg ch)]
      (is (= :phase-complete (:type msg)))
      (is (false? (:tests-passed? msg))))))

;; =============================================================================
;; run-full-saa! Tests
;; =============================================================================

(deftest run-full-saa-happy-path-test
  (testing "run-full-saa! completes full cycle via FSM engine"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          ch   (bridge/run-full-saa! orch (mock-session) "Fix auth bug" {})
          msgs (drain-ch ch)
          final (last msgs)]
      (is (= :saa-complete (:type final)))
      (is (= :complete (:saa-phase final)))
      (is (map? (:result final)))
      ;; Result should have plan and test results
      (is (true? (get-in final [:result :plan-valid?])))
      (is (true? (get-in final [:result :tests-passed?]))))))

(deftest run-full-saa-plan-only-test
  (testing "run-full-saa! in plan-only mode skips Act phase"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          ch   (bridge/run-full-saa! orch (mock-session) "Explore auth"
                                     {:plan-only? true})
          msgs (drain-ch ch)
          final (last msgs)]
      (is (= :saa-complete (:type final)))
      (is (true? (get-in final [:result :plan-only?])))
      ;; No execution results in plan-only
      (is (nil? (get-in final [:result :execution-result]))))))

(deftest run-full-saa-error-no-task-test
  (testing "run-full-saa! handles missing task gracefully"
    (let [cfg  (mock-config)
          orch (sut/->fsm-saa-orchestrator cfg)
          ch   (bridge/run-full-saa! orch (mock-session) nil {})
          msgs (drain-ch ch)
          final (last msgs)]
      ;; Should get error (missing task triggers FSM error state)
      (is (some? final)))))

(deftest run-full-saa-emits-progress-test
  (testing "run-full-saa! emits progress messages during execution"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          ch   (bridge/run-full-saa! orch (mock-session) "Fix bug" {})
          msgs (drain-ch ch)
          progress-msgs (filter #(= :progress (:type %)) msgs)]
      ;; Should have progress shouts from FSM handlers
      (is (pos? (count progress-msgs))))))

;; =============================================================================
;; Agent-ID Resolution Tests
;; =============================================================================

(deftest agent-id-from-session-test
  (testing "agent-id resolved from session"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          ch   (bridge/run-full-saa! orch (mock-session "my-agent") "Task" {})
          msgs (drain-ch ch)
          final (last msgs)]
      (is (= "my-agent" (get-in final [:result :agent-id]))))))

(deftest agent-id-from-opts-test
  (testing "agent-id from opts overrides session"
    (let [orch (sut/->fsm-saa-orchestrator (mock-config))
          ch   (bridge/run-full-saa! orch (mock-session "session-agent") "Task"
                                     {:agent-id "opts-agent"})
          msgs (drain-ch ch)
          final (last msgs)]
      (is (= "opts-agent" (get-in final [:result :agent-id]))))))

;; =============================================================================
;; Integration: ISAAOrchestrator Protocol Satisfaction
;; =============================================================================

(deftest satisfies-protocol-test
  (testing "FSMSAAOrchestrator satisfies ISAAOrchestrator"
    (let [orch (sut/->fsm-saa-orchestrator)]
      (is (satisfies? bridge/ISAAOrchestrator orch))))

  (testing "all protocol methods return channels"
    (let [orch    (sut/->fsm-saa-orchestrator (mock-config))
          session (mock-session)]
      (let [ch (bridge/run-silence! orch session "T" {})]
        (is (some? ch))
        (drain-ch ch))
      (let [ch (bridge/run-abstract! orch session [] {:task "T"})]
        (is (some? ch))
        (drain-ch ch))
      (let [ch (bridge/run-act! orch session {:id "p"} {:task "T"})]
        (is (some? ch))
        (drain-ch ch))
      (let [ch (bridge/run-full-saa! orch session "T" {})]
        (is (some? ch))
        (drain-ch ch)))))
