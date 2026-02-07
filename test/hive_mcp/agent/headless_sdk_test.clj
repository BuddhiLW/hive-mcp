(ns hive-mcp.agent.headless-sdk-test
  "Tests for headless SDK ling management (SAA strategy).

   Tests cover:
   1. SDK availability detection (graceful degradation)
   2. Session lifecycle (spawn, status, kill)
   3. SAA phase configuration
   4. Session registry operations
   5. Observation scoring (L1/L2 heuristic)
   6. Silence tracking integration

   Note: Python/SDK calls are tested via availability checks only.
   Integration tests requiring actual Python + claude-agent-sdk
   would be tagged :integration."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.core.async :as async :refer [<!! >!! chan close!]]
            [hive-mcp.agent.headless-sdk :as sdk]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn with-fresh-state [f]
  "Reset SDK session registry between tests."
  (sdk/kill-all-sdk!)
  (sdk/reset-availability!)
  (f)
  (sdk/kill-all-sdk!))

(use-fixtures :each with-fresh-state)

(defn gen-ling-id []
  (str "test-sdk-" (java.util.UUID/randomUUID)))

;; =============================================================================
;; SAA Phase Configuration Tests
;; =============================================================================

(deftest saa-phases-defined-test
  (testing "All three SAA phases are defined"
    (is (contains? sdk/saa-phases :silence))
    (is (contains? sdk/saa-phases :abstract))
    (is (contains? sdk/saa-phases :act))))

(deftest silence-phase-config-test
  (testing "Silence phase has read-only tools"
    (let [phase (get sdk/saa-phases :silence)]
      (is (= :silence (:name phase)))
      (is (some #{"Read"} (:allowed-tools phase)))
      (is (some #{"Glob"} (:allowed-tools phase)))
      (is (some #{"Grep"} (:allowed-tools phase)))
      ;; Should NOT have edit/write/bash
      (is (not (some #{"Edit"} (:allowed-tools phase))))
      (is (not (some #{"Write"} (:allowed-tools phase))))
      (is (not (some #{"Bash"} (:allowed-tools phase))))
      (is (= "bypassPermissions" (:permission-mode phase))))))

(deftest abstract-phase-config-test
  (testing "Abstract phase has read-only tools"
    (let [phase (get sdk/saa-phases :abstract)]
      (is (= :abstract (:name phase)))
      (is (some #{"Read"} (:allowed-tools phase)))
      (is (not (some #{"Edit"} (:allowed-tools phase))))
      (is (= "bypassPermissions" (:permission-mode phase))))))

(deftest act-phase-config-test
  (testing "Act phase has full tool access"
    (let [phase (get sdk/saa-phases :act)]
      (is (= :act (:name phase)))
      (is (some #{"Read"} (:allowed-tools phase)))
      (is (some #{"Edit"} (:allowed-tools phase)))
      (is (some #{"Write"} (:allowed-tools phase)))
      (is (some #{"Bash"} (:allowed-tools phase)))
      (is (= "acceptEdits" (:permission-mode phase))))))

(deftest saa-phase-system-prompts-test
  (testing "Each phase has a system prompt suffix"
    (doseq [[phase-key phase-config] sdk/saa-phases]
      (is (string? (:system-prompt-suffix phase-config))
          (str "Phase " phase-key " should have string system-prompt-suffix"))
      (is (pos? (count (:system-prompt-suffix phase-config)))
          (str "Phase " phase-key " system-prompt-suffix should not be empty")))))

(deftest saa-phase-tool-lists-test
  (testing "Each phase has a non-empty tool list"
    (doseq [[phase-key phase-config] sdk/saa-phases]
      (is (vector? (:allowed-tools phase-config))
          (str "Phase " phase-key " tools should be a vector"))
      (is (pos? (count (:allowed-tools phase-config)))
          (str "Phase " phase-key " should have at least one tool")))))

(deftest saa-phase-permission-modes-test
  (testing "Each phase has a valid permission mode"
    (doseq [[phase-key phase-config] sdk/saa-phases]
      (is (contains? #{"bypassPermissions" "acceptEdits" "default"}
                     (:permission-mode phase-config))
          (str "Phase " phase-key " should have a valid permission mode")))))

(deftest silence-phase-is-read-only-test
  (testing "Silence phase tools are all read-only (no mutation)"
    (let [read-only-tools #{"Read" "Glob" "Grep" "WebSearch" "WebFetch"}
          phase-tools (set (:allowed-tools (:silence sdk/saa-phases)))]
      (is (every? read-only-tools phase-tools)
          "All silence tools should be read-only"))))

(deftest act-phase-has-mutation-tools-test
  (testing "Act phase includes mutation tools"
    (let [mutation-tools #{"Edit" "Write" "Bash"}
          phase-tools (set (:allowed-tools (:act sdk/saa-phases)))]
      (is (some mutation-tools phase-tools)
          "Act phase should have at least one mutation tool"))))

;; =============================================================================
;; SDK Availability Tests (Graceful Degradation)
;; =============================================================================

(deftest sdk-status-returns-keyword-test
  (testing "sdk-status returns a keyword status"
    (let [status (sdk/sdk-status)]
      (is (keyword? status))
      (is (contains? #{:available :no-libpython :no-sdk :not-initialized} status)))))

(deftest available-predicate-test
  (testing "available? returns boolean"
    (is (boolean? (sdk/available?)))))

(deftest sdk-status-caching-test
  (testing "sdk-status is cached after first call"
    (let [status1 (sdk/sdk-status)
          status2 (sdk/sdk-status)]
      (is (= status1 status2)
          "Repeated calls should return same cached value"))))

(deftest reset-availability-test
  (testing "reset-availability! clears cached status"
    ;; Call once to cache
    (sdk/sdk-status)
    ;; Reset
    (sdk/reset-availability!)
    ;; Should still work (re-checks)
    (is (keyword? (sdk/sdk-status)))))

;; =============================================================================
;; Session Registry Tests (No Python Required)
;; =============================================================================

(deftest session-registry-empty-test
  (testing "Empty registry returns empty list"
    (is (= [] (sdk/list-sdk-sessions)))))

(deftest get-session-nonexistent-test
  (testing "Get nonexistent session returns nil"
    (is (nil? (sdk/get-session "nonexistent-ling")))))

(deftest sdk-session-predicate-test
  (testing "sdk-session? returns false for nonexistent"
    (is (false? (sdk/sdk-session? "nonexistent")))))

(deftest kill-nonexistent-session-test
  (testing "Kill nonexistent session throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sdk/kill-headless-sdk! "nonexistent")))))

(deftest kill-all-empty-test
  (testing "Kill all with no sessions succeeds"
    (let [result (sdk/kill-all-sdk!)]
      (is (zero? (:killed result)))
      (is (zero? (:errors result))))))

;; =============================================================================
;; Spawn Tests (SDK availability dependent)
;; =============================================================================

(deftest spawn-without-sdk-throws-test
  (testing "Spawn when SDK unavailable throws with helpful error"
    ;; This test verifies graceful degradation
    ;; If SDK IS available on test system, this becomes a positive test
    (if (sdk/available?)
      ;; SDK available - test spawn succeeds
      (let [ling-id (gen-ling-id)
            result (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})]
        (is (= ling-id (:ling-id result)))
        (is (= :spawned (:status result)))
        (is (= :agent-sdk (:backend result)))
        (sdk/kill-headless-sdk! ling-id))
      ;; SDK not available - test error path
      (let [ling-id (gen-ling-id)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Claude Agent SDK not available"
                              (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})))))))

(deftest spawn-preconditions-test
  (testing "Spawn requires ling-id and cwd"
    (is (thrown? AssertionError
                 (sdk/spawn-headless-sdk! nil {:cwd "/tmp"})))
    (is (thrown? AssertionError
                 (sdk/spawn-headless-sdk! "test" {:cwd nil})))))

;; =============================================================================
;; Status Tests
;; =============================================================================

(deftest sdk-status-for-nonexistent-test
  (testing "Status for nonexistent returns nil"
    (is (nil? (sdk/sdk-status-for "nonexistent")))))

;; =============================================================================
;; Dispatch Precondition Tests
;; =============================================================================

(deftest dispatch-without-session-throws-test
  (testing "Dispatch to nonexistent session throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sdk/dispatch-headless-sdk! "nonexistent" "do something")))))

(deftest dispatch-preconditions-test
  (testing "Dispatch requires ling-id and task"
    (is (thrown? AssertionError
                 (sdk/dispatch-headless-sdk! nil "task")))
    (is (thrown? AssertionError
                 (sdk/dispatch-headless-sdk! "id" nil)))))

;; =============================================================================
;; Observation Scoring Tests (L1/L2 heuristic - no Python needed)
;; =============================================================================

(deftest score-observations-empty-test
  (testing "Scoring empty observations returns empty vector"
    (is (= [] (sdk/score-observations [])))))

(deftest score-observations-basic-test
  (testing "Scoring ranks observations by relevance"
    (let [observations [{:data "found a bug in auth.clj"}
                        {:data "read the README file"}
                        {:data "discovered a pattern for validation"}]
          scored (sdk/score-observations observations)]
      ;; Should return sorted by score descending
      (is (= 3 (count scored)))
      (is (every? :score scored))
      (is (every? :observation scored))
      ;; Bug-related should score higher than generic
      (let [scores (map :score scored)]
        (is (>= (first scores) (last scores))
            "Scores should be in descending order")))))

(deftest score-observations-bug-boost-test
  (testing "Bug/error/issue observations get a boost"
    (let [bug-obs [{:data "found a critical bug"}]
          generic-obs [{:data "read some code"}]
          bug-scored (first (sdk/score-observations bug-obs))
          generic-scored (first (sdk/score-observations generic-obs))]
      (is (> (:score bug-scored) (:score generic-scored))
          "Bug observations should score higher"))))

(deftest score-observations-pattern-boost-test
  (testing "Pattern/convention observations get a boost"
    (let [pattern-obs [{:data "discovered a convention for naming"}]
          generic-obs [{:data "read some code"}]
          pattern-scored (first (sdk/score-observations pattern-obs))
          generic-scored (first (sdk/score-observations generic-obs))]
      (is (> (:score pattern-scored) (:score generic-scored))
          "Pattern observations should score higher"))))

(deftest score-observations-test-boost-test
  (testing "Test-related observations get a boost"
    (let [test-obs [{:data "found test assertions"}]
          generic-obs [{:data "read some code"}]
          test-scored (first (sdk/score-observations test-obs))
          generic-scored (first (sdk/score-observations generic-obs))]
      (is (> (:score test-scored) (:score generic-scored))
          "Test observations should score higher"))))

(deftest score-observations-combined-boost-test
  (testing "Observations with multiple signals get combined boost"
    (let [multi-obs [{:data "found a bug pattern in test code"}]
          single-obs [{:data "found a bug"}]
          multi-scored (first (sdk/score-observations multi-obs))
          single-scored (first (sdk/score-observations single-obs))]
      (is (> (:score multi-scored) (:score single-scored))
          "Multi-signal observations should score highest"))))

(deftest score-observations-string-input-test
  (testing "Scoring handles plain string observations"
    (let [observations ["found a bug" "read some code"]
          scored (sdk/score-observations observations)]
      (is (= 2 (count scored)))
      (is (every? :score scored)))))

;; =============================================================================
;; Subagent (agents) Field Threading Tests
;; =============================================================================

(deftest spawn-stores-agents-in-session-test
  (testing "spawn-headless-sdk! stores agents map in session registry"
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            test-agents {"code-reviewer" {:description "Reviews code"
                                          :prompt "You review code"
                                          :tools ["Read" "Grep"]
                                          :model "sonnet"}}
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"
                                                :agents test-agents})
            session (sdk/get-session ling-id)]
        (is (some? session) "Session should be registered")
        (is (= test-agents (:agents session))
            "Session should store agents map from spawn opts")
        (sdk/kill-headless-sdk! ling-id)))))

(deftest spawn-without-agents-stores-nil-test
  (testing "spawn-headless-sdk! without agents stores nil in session"
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})
            session (sdk/get-session ling-id)]
        (is (some? session))
        (is (nil? (:agents session))
            "Session agents should be nil when not provided")
        (sdk/kill-headless-sdk! ling-id)))))

(deftest spawn-agents-multiple-definitions-test
  (testing "spawn-headless-sdk! stores multiple agent definitions"
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            test-agents {"analyzer" {:description "Analyzes code"
                                     :prompt "You analyze code"
                                     :tools ["Read" "Grep" "Glob"]}
                         "tester" {:description "Writes tests"
                                   :prompt "You write tests"
                                   :tools ["Read" "Write" "Bash"]
                                   :model "sonnet"}
                         "documenter" {:description "Writes docs"
                                       :prompt "You write documentation"}}
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"
                                                :agents test-agents})
            session (sdk/get-session ling-id)]
        (is (= 3 (count (:agents session)))
            "Session should store all 3 agent definitions")
        (is (= #{"analyzer" "tester" "documenter"}
               (set (keys (:agents session))))
            "All agent names should be preserved")
        (sdk/kill-headless-sdk! ling-id)))))

;; =============================================================================
;; Integration Test Placeholder (requires actual Python + SDK)
;; =============================================================================

(deftest ^:integration sdk-full-cycle-test
  (testing "Full SAA cycle with actual SDK (requires Python + claude-agent-sdk)"
    ;; This test only runs when tagged :integration
    ;; It requires: pip install claude-agent-sdk + ANTHROPIC_API_KEY
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})
            ch (sdk/dispatch-headless-sdk! ling-id "List files in the current directory"
                                           {:skip-abstract? true})]
        ;; Consume all messages
        (loop [msgs []]
          (if-let [msg (<!! ch)]
            (recur (conj msgs msg))
            ;; Done - verify we got messages
            (do
              (is (pos? (count msgs)))
              (is (= :saa-complete (:type (last msgs)))))))
        ;; Cleanup
        (sdk/kill-headless-sdk! ling-id)))))

(comment
  ;; Run tests
  ;; (clojure.test/run-tests 'hive-mcp.agent.headless-sdk-test)
  )
