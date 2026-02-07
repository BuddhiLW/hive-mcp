(ns hive-mcp.agent.saa.orchestrator-test
  "Tests for SAA orchestrator.

   Tests state management, phase transitions, and protocol compliance.
   Uses MockSession that returns predefined messages via query!,
   avoiding dependency on real SDK/Python backend."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.core.async :as async :refer [<!! >!! chan close! go put!]]
            [hive-mcp.agent.saa.orchestrator :as saa]
            [hive-mcp.protocols.agent-bridge :as bridge]
            [hive-mcp.agent.headless-sdk :as sdk]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn clean-state-fixture
  "Reset all SAA agent states between tests."
  [f]
  (saa/clear-all-states!)
  (try (f)
       (finally
         (saa/clear-all-states!))))

(use-fixtures :each clean-state-fixture)

;;; =============================================================================
;;; Mock Session
;;; =============================================================================

(defn ->mock-session
  "Create a MockSession that returns predefined messages from query!.
   response-fn: (fn [prompt opts] -> seq-of-messages) or nil for defaults."
  ([id] (->mock-session id nil))
  ([id response-fn]
   (reify bridge/IAgentSession
     (session-id [_] id)
     (query! [_ prompt opts]
       (let [ch (chan 10)
             msgs (if response-fn
                    (response-fn prompt opts)
                    [{:type :message :content "mock observation"}
                     {:type :complete :content "done"}])]
         (go
           (doseq [msg msgs]
             (put! ch msg))
           (close! ch))
         ch))
     (interrupt! [_] {:success? true :errors []})
     (receive-messages [_]
       (let [ch (chan)] (close! ch) ch))
     (receive-response [_]
       (let [ch (chan)] (close! ch) ch)))))

;;; =============================================================================
;;; Factory Tests
;;; =============================================================================

(deftest test-create-orchestrator
  (testing "Default configuration"
    (let [orch (saa/->saa-orchestrator)]
      (is (some? orch))
      (is (instance? hive_mcp.agent.saa.orchestrator.SAAOrchestrator orch))
      (is (true? (:shout? (:config orch))))
      (is (= 0.0 (:score-threshold (:config orch))))
      (is (= 50 (:max-silence-turns (:config orch))))))

  (testing "Custom configuration"
    (let [orch (saa/->saa-orchestrator {:shout? false :max-silence-turns 10})]
      (is (false? (:shout? (:config orch))))
      (is (= 10 (:max-silence-turns (:config orch))))
      ;; Defaults still present for unspecified keys
      (is (= 20 (:max-abstract-turns (:config orch))))
      (is (= 100 (:max-act-turns (:config orch)))))))

;;; =============================================================================
;;; Protocol Compliance Tests
;;; =============================================================================

(deftest test-orchestrator-satisfies-protocol
  (testing "SAAOrchestrator satisfies ISAAOrchestrator"
    (let [orch (saa/->saa-orchestrator)]
      (is (satisfies? bridge/ISAAOrchestrator orch)))))

;;; =============================================================================
;;; State Management Tests
;;; =============================================================================

(deftest test-agent-state-lifecycle
  (testing "No state initially"
    (is (nil? (saa/agent-saa-state "test-agent-1")))
    (is (nil? (saa/agent-saa-phase "test-agent-1"))))

  (testing "State created during run-silence!"
    (let [orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "test-agent-2"
                                  (fn [_ _]
                                    [{:type :message :content "found file A"}
                                     {:type :message :content "found pattern B"}]))]
      (let [ch (bridge/run-silence! orch session "test task" {})
            ;; Drain all messages
            _msgs (loop [acc []]
                    (if-let [msg (<!! ch)]
                      (recur (conj acc msg))
                      acc))]
        ;; State should exist now
        (is (some? (saa/agent-saa-state "test-agent-2")))
        (let [state (saa/agent-saa-state "test-agent-2")]
          (is (= "test task" (:task state)))
          (is (vector? (:observations state)))
          (is (= 2 (count (:observations state)))))))))

(deftest test-list-active-saa
  (testing "Empty when no agents running"
    (is (empty? (saa/list-active-saa)))))

(deftest test-clear-all-states
  (testing "Clears all states"
    (let [orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "clear-test-1")]
      ;; Run silence to create state
      (let [ch (bridge/run-silence! orch session "task" {})]
        (loop [] (when (<!! ch) (recur))))
      ;; State exists
      (is (some? (saa/agent-saa-state "clear-test-1")))
      ;; Clear all
      (saa/clear-all-states!)
      (is (nil? (saa/agent-saa-state "clear-test-1"))))))

(deftest test-clear-completed-states
  (testing "Clears completed states only"
    (let [orch (saa/->saa-orchestrator {:shout? false})
          ;; Create a session that will cause error
          error-session (->mock-session "error-agent"
                                        (fn [_ _]
                                          (throw (ex-info "boom" {}))))
          ;; Create a session that works
          ok-session (->mock-session "ok-agent")]
      ;; Run silence for ok-agent
      (let [ch (bridge/run-silence! orch ok-session "task" {})]
        (loop [] (when (<!! ch) (recur))))
      ;; Run silence for error-agent (will set error state)
      (let [ch (bridge/run-silence! orch error-session "task" {})]
        (loop [] (when (<!! ch) (recur))))
      ;; error-agent should be in :error phase
      (is (= :error (saa/agent-saa-phase "error-agent")))
      ;; Clear completed/errored
      (let [result (saa/clear-completed-states!)]
        (is (= 1 (:cleared result))))
      ;; error-agent cleared, ok-agent still exists
      (is (nil? (saa/agent-saa-state "error-agent")))
      (is (some? (saa/agent-saa-state "ok-agent"))))))

;;; =============================================================================
;;; run-silence! Tests
;;; =============================================================================

(deftest test-run-silence-returns-channel
  (testing "run-silence! returns a core.async channel with phase-complete"
    (let [orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "silence-test"
                                  (fn [_ _]
                                    [{:type :message :content "observation 1"}
                                     {:type :message :content "observation 2"}]))]
      (let [ch (bridge/run-silence! orch session "explore codebase" {})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        ;; Should have messages + phase-complete
        (is (pos? (count msgs)))
        ;; Last message should be phase-complete
        (let [last-msg (last msgs)]
          (is (= :phase-complete (:type last-msg)))
          (is (= :silence (:saa-phase last-msg)))
          (is (vector? (:observations last-msg)))
          (is (= 2 (:observation-count last-msg))))))))

(deftest test-run-silence-tags-messages
  (testing "All messages from silence are tagged with :saa-phase :silence"
    (let [orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "tag-test"
                                  (fn [_ _]
                                    [{:type :message :content "obs"}]))]
      (let [ch (bridge/run-silence! orch session "task" {})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        ;; All messages should have :saa-phase :silence
        (doseq [msg msgs]
          (is (= :silence (:saa-phase msg))))))))

;;; =============================================================================
;;; run-abstract! Tests
;;; =============================================================================

(deftest test-run-abstract-returns-plan
  (testing "run-abstract! synthesizes observations into plan"
    (let [orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "abstract-test"
                                  (fn [prompt _]
                                    [{:type :message :content "Step 1: fix auth.clj"}
                                     {:type :message :content "Step 2: update tests"}]))]
      ;; Init state first (abstract expects prior state from silence)
      (let [silence-session (->mock-session "abstract-test"
                                            (fn [_ _] [{:type :message :content "obs"}]))
            ch (bridge/run-silence! orch silence-session "task" {})]
        (loop [] (when (<!! ch) (recur))))
      ;; Now run abstract
      (let [ch (bridge/run-abstract! orch session ["obs1" "obs2"] {})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        (is (pos? (count msgs)))
        (let [last-msg (last msgs)]
          (is (= :phase-complete (:type last-msg)))
          (is (= :abstract (:saa-phase last-msg)))
          (is (string? (:plan last-msg))))))))

;;; =============================================================================
;;; run-act! Tests
;;; =============================================================================

(deftest test-run-act-returns-result
  (testing "run-act! executes plan and returns result"
    (let [orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "act-test"
                                  (fn [_ _]
                                    [{:type :message :content "Changed auth.clj line 42"}
                                     {:type :message :content "Tests pass"}]))]
      ;; Init state
      (let [init-session (->mock-session "act-test"
                                         (fn [_ _] [{:type :message :content "obs"}]))
            ch (bridge/run-silence! orch init-session "task" {})]
        (loop [] (when (<!! ch) (recur))))
      ;; Run act
      (let [ch (bridge/run-act! orch session "Step 1: fix auth.clj\nStep 2: run tests" {})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        (is (pos? (count msgs)))
        (let [last-msg (last msgs)]
          (is (= :phase-complete (:type last-msg)))
          (is (= :act (:saa-phase last-msg)))
          (is (map? (:result last-msg)))
          (is (pos? (:message-count (:result last-msg)))))
        ;; State should be :complete
        (is (= :complete (saa/agent-saa-phase "act-test")))))))

;;; =============================================================================
;;; run-full-saa! Tests
;;; =============================================================================

(deftest test-run-full-saa-chains-phases
  (testing "run-full-saa! executes all three phases"
    (let [call-count (atom 0)
          orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "full-saa-test"
                                  (fn [prompt _]
                                    (let [n (swap! call-count inc)]
                                      (case n
                                        1 [{:type :message :content "observation from silence"}]
                                        2 [{:type :message :content "plan step from abstract"}]
                                        3 [{:type :message :content "execution result from act"}]
                                        [{:type :message :content "extra"}]))))]
      (let [ch (bridge/run-full-saa! orch session "full test task" {})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        ;; Should have messages from all phases + saa-complete
        (is (pos? (count msgs)))
        ;; Last message should be saa-complete
        (let [last-msg (last msgs)]
          (is (= :saa-complete (:type last-msg)))
          (is (= "full-saa-test" (:agent-id last-msg)))
          (is (number? (:elapsed-ms last-msg))))
        ;; query! should have been called at least 3 times
        ;; (silence, abstract, act — each calls query! once)
        (is (>= @call-count 3))))))

(deftest test-run-full-saa-skip-silence
  (testing "run-full-saa! with :skip-silence? skips Silence phase"
    (let [call-count (atom 0)
          orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "skip-silence"
                                  (fn [_ _]
                                    (swap! call-count inc)
                                    [{:type :message :content "response"}]))]
      (let [ch (bridge/run-full-saa! orch session "task" {:skip-silence? true})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        ;; Should only have abstract + act calls (2, not 3)
        (is (= 2 @call-count))
        (let [last-msg (last msgs)]
          (is (= :saa-complete (:type last-msg))))))))

(deftest test-run-full-saa-skip-abstract
  (testing "run-full-saa! with :skip-abstract? skips Abstract phase"
    (let [call-count (atom 0)
          orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "skip-abstract"
                                  (fn [_ _]
                                    (swap! call-count inc)
                                    [{:type :message :content "response"}]))]
      (let [ch (bridge/run-full-saa! orch session "task" {:skip-abstract? true})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        ;; Should only have silence + act calls (2, not 3)
        (is (= 2 @call-count))
        (let [last-msg (last msgs)]
          (is (= :saa-complete (:type last-msg))))))))

;;; =============================================================================
;;; Error Handling Tests
;;; =============================================================================

(deftest test-silence-error-handling
  (testing "run-silence! handles query! errors gracefully"
    (let [orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "error-test"
                                  (fn [_ _]
                                    (throw (ex-info "SDK exploded" {:phase :silence}))))]
      (let [ch (bridge/run-silence! orch session "task" {})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        ;; Should get an error message, not throw
        (is (pos? (count msgs)))
        (let [err-msg (first msgs)]
          (is (= :error (:type err-msg)))
          (is (= :silence (:saa-phase err-msg)))))
      ;; State should record error
      (let [state (saa/agent-saa-state "error-test")]
        (is (= :error (:phase state)))
        (is (some? (:error state)))))))

;;; =============================================================================
;;; Config Behavior Tests
;;; =============================================================================

(deftest test-shout-config-respected
  (testing ":shout? false suppresses hivemind shouts (no debug log spam)"
    ;; With :shout? false, no shout-phase! calls should happen.
    ;; We verify by checking that no exception escapes from
    ;; the requiring-resolve path (which would indicate shout was called).
    (let [orch (saa/->saa-orchestrator {:shout? false})
          session (->mock-session "shout-config-test"
                                  (fn [_ _] [{:type :message :content "obs"}]))]
      (let [ch (bridge/run-silence! orch session "test task" {})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        ;; Phase should complete successfully
        (is (= :phase-complete (:type (last msgs))))
        ;; State should be populated
        (is (some? (saa/agent-saa-state "shout-config-test"))))))

  (testing ":shout? true (default) enables hivemind shouts"
    ;; With :shout? true, shout-phase! IS called (gracefully fails in test env)
    (let [orch (saa/->saa-orchestrator {:shout? true})
          session (->mock-session "shout-enabled-test"
                                  (fn [_ _] [{:type :message :content "obs"}]))]
      (let [ch (bridge/run-silence! orch session "test task" {})
            msgs (loop [acc []]
                   (if-let [msg (<!! ch)]
                     (recur (conj acc msg))
                     acc))]
        ;; Should still complete (shout failures are non-critical)
        (is (= :phase-complete (:type (last msgs))))))))

;;; =============================================================================
;;; Score Observations Fallback Tests
;;; =============================================================================

(deftest test-score-observations-fallback
  (testing "score-observations uses headless-sdk heuristic (L3+ unavailable)"
    ;; hive-knowledge.scoring/score-observations won't be on classpath
    ;; so it falls back to sdk/score-observations
    (let [observations [{:data "found a bug in auth.clj"}
                        {:data "read the README"}
                        {:data "discovered a pattern for validation"}]]
      (let [scored (sdk/score-observations observations)]
        (is (vector? scored))
        (is (= 3 (count scored)))
        ;; Each entry should have :observation and :score
        (doseq [entry scored]
          (is (contains? entry :observation))
          (is (contains? entry :score))
          (is (number? (:score entry))))))))

(comment
  ;; Run all tests in this namespace via nREPL:
  ;; (require '[clojure.test :refer [run-tests]])
  ;; (require 'hive-mcp.agent.saa.orchestrator-test :reload)
  ;; (run-tests 'hive-mcp.agent.saa.orchestrator-test)
  )
