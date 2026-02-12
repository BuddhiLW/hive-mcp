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
            [clojure.set]
            [hive-mcp.agent.headless-sdk :as sdk]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.agent-sdk-strategy :as sdk-strat]))

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
;; SAA Gating Hook Contract Tests (No Python Required)
;; =============================================================================

(deftest saa-phases-have-hook-required-fields-test
  (testing "Each phase has :name and :allowed-tools needed by build-saa-hooks"
    (doseq [[phase-key phase-config] sdk/saa-phases]
      (is (keyword? (:name phase-config))
          (str "Phase " phase-key " must have a :name keyword for hook phase_name"))
      (is (vector? (:allowed-tools phase-config))
          (str "Phase " phase-key " must have :allowed-tools vector for hook filtering"))
      (is (pos? (count (:allowed-tools phase-config)))
          (str "Phase " phase-key " must have at least one allowed tool")))))

(deftest silence-phase-denies-mutation-tools-test
  (testing "Silence phase allowed-tools excludes all mutation tools"
    (let [silence-tools (set (:allowed-tools (:silence sdk/saa-phases)))
          mutation-tools #{"Edit" "Write" "Bash" "NotebookEdit"}]
      (is (empty? (clojure.set/intersection silence-tools mutation-tools))
          "Silence phase must not include any mutation tools"))))

(deftest abstract-phase-denies-mutation-tools-test
  (testing "Abstract phase allowed-tools excludes all mutation tools"
    (let [abstract-tools (set (:allowed-tools (:abstract sdk/saa-phases)))
          mutation-tools #{"Edit" "Write" "Bash" "NotebookEdit"}]
      (is (empty? (clojure.set/intersection abstract-tools mutation-tools))
          "Abstract phase must not include any mutation tools"))))

(deftest act-phase-allows-mutation-tools-test
  (testing "Act phase allowed-tools includes mutation tools for execution"
    (let [act-tools (set (:allowed-tools (:act sdk/saa-phases)))]
      (is (contains? act-tools "Edit")
          "Act phase must allow Edit for file changes")
      (is (contains? act-tools "Write")
          "Act phase must allow Write for new files")
      (is (contains? act-tools "Bash")
          "Act phase must allow Bash for commands"))))

(deftest saa-phase-tool-sets-are-progressively-permissive-test
  (testing "Each successive SAA phase allows >= tools than previous"
    (let [silence-tools (set (:allowed-tools (:silence sdk/saa-phases)))
          abstract-tools (set (:allowed-tools (:abstract sdk/saa-phases)))
          act-tools (set (:allowed-tools (:act sdk/saa-phases)))]
      ;; Act must be a superset of abstract
      (is (clojure.set/subset? abstract-tools act-tools)
          "Act phase must allow all abstract phase tools plus more"))))

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
;; P3-T2: Persistent Client / Multi-Turn Tests
;; =============================================================================

(deftest spawn-creates-persistent-client-test
  (testing "spawn-headless-sdk! creates persistent loop + client refs in session"
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            result (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})
            session (sdk/get-session ling-id)]
        (is (= :spawned (:status result)))
        ;; P3-T2: Session must have persistent client refs
        (is (string? (:client-ref session))
            "Session should have client-ref (Python global name)")
        (is (string? (:py-loop-var session))
            "Session should have py-loop-var (Python global name)")
        (is (string? (:py-safe-id session))
            "Session should have py-safe-id")
        (is (zero? (:turn-count session))
            "Initial turn count should be 0")
        ;; Client-ref should follow naming convention
        (is (re-matches #"_hive_client_.*" (:client-ref session))
            "Client ref should follow _hive_client_<safe-id> pattern")
        (is (re-matches #"_hive_loop_.*" (:py-loop-var session))
            "Loop var should follow _hive_loop_<safe-id> pattern")
        (sdk/kill-headless-sdk! ling-id)))))

(deftest status-shows-persistent-client-info-test
  (testing "sdk-status-for shows multi-turn tracking fields"
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})
            status (sdk/sdk-status-for ling-id)]
        (is (true? (:has-persistent-client? status))
            "Status should show has-persistent-client? true")
        (is (zero? (:turn-count status))
            "Status should show initial turn count of 0")
        (is (true? (:interruptable? status))
            "Persistent client should be interruptable")
        (sdk/kill-headless-sdk! ling-id)))))

(deftest dispatch-requires-persistent-client-test
  (testing "dispatch to session without client-ref throws"
    ;; Use var access to private register fn for test setup
    (let [ling-id (gen-ling-id)
          register! @#'sdk/register-session!
          unregister! @#'sdk/unregister-session!]
      (register! ling-id {:ling-id ling-id
                          :phase :idle
                          :cwd "/tmp"
                          :client-ref nil
                          :py-loop-var nil})
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"No persistent client"
                            (sdk/dispatch-headless-sdk! ling-id "test task")))
      (unregister! ling-id))))

(deftest kill-gracefully-disconnects-test
  (testing "kill-headless-sdk! disconnects client and removes session"
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})
            ;; Verify session exists with client
            session-before (sdk/get-session ling-id)]
        (is (some? (:client-ref session-before)))
        ;; Kill should succeed (graceful disconnect)
        (let [result (sdk/kill-headless-sdk! ling-id)]
          (is (true? (:killed? result)))
          ;; Session should be gone
          (is (nil? (sdk/get-session ling-id))))))))

(deftest ling-id-safe-id-conversion-test
  (testing "ling-id->safe-id replaces non-alphanumeric chars"
    ;; Test via spawn — the safe-id is derived internally
    (when (sdk/available?)
      (let [ling-id "swarm-test-agent-123"
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})
            session (sdk/get-session ling-id)]
        (is (= "swarm_test_agent_123" (:py-safe-id session))
            "Hyphens should be replaced with underscores")
        (sdk/kill-headless-sdk! ling-id)))))

(deftest dispatch-raw-mode-returns-channel-test
  (testing "Dispatch with :raw? true returns a channel"
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})
            ;; Raw dispatch — returns channel even if SDK call may fail
            ch (sdk/dispatch-headless-sdk! ling-id "echo hello" {:raw? true})]
        (is (some? ch) "Raw dispatch should return a channel")
        ;; Drain channel to prevent hanging
        (async/thread (loop [] (when (<!! ch) (recur))))
        (Thread/sleep 500)
        (sdk/kill-headless-sdk! ling-id)))))

(deftest kill-all-with-persistent-clients-test
  (testing "kill-all-sdk! cleans up all persistent clients"
    (when (sdk/available?)
      (let [ids (mapv (fn [_] (gen-ling-id)) (range 2))]
        ;; Spawn two lings
        (doseq [id ids]
          (sdk/spawn-headless-sdk! id {:cwd "/tmp"}))
        ;; Verify both registered
        (is (= 2 (count (sdk/list-sdk-sessions))))
        ;; Kill all
        (let [result (sdk/kill-all-sdk!)]
          (is (= 2 (:killed result)))
          (is (zero? (:errors result))))
        ;; Verify all gone
        (is (= 0 (count (sdk/list-sdk-sessions))))))))

;; =============================================================================
;; P3-T2: Strategy Layer :raw? Threading Tests
;; =============================================================================

(deftest strategy-dispatch-threads-raw-option-test
  (testing "AgentSDKStrategy.strategy-dispatch! threads :raw? to SDK dispatch"
    ;; Setup: register a mock session with client-ref so dispatch doesn't throw
    (let [ling-id (gen-ling-id)
          register! @#'sdk/register-session!
          unregister! @#'sdk/unregister-session!
          captured-opts (atom nil)]
      (register! ling-id {:ling-id ling-id
                          :phase :idle
                          :cwd "/tmp"
                          :client-ref (str "_hive_client_test")
                          :py-loop-var (str "_hive_loop_test")
                          :turn-count 0})
      ;; Mock dispatch-headless-sdk! to capture the opts passed
      (with-redefs [sdk/dispatch-headless-sdk!
                    (fn [id task & [opts]]
                      (reset! captured-opts {:id id :task task :opts opts})
                      ;; Return a closed channel (simulates instant completion)
                      (let [ch (chan 1)]
                        (close! ch)
                        ch))]
        (let [strategy-inst (sdk-strat/->agent-sdk-strategy)]
          ;; Dispatch with :raw? true
          (strategy/strategy-dispatch!
           strategy-inst
           {:id ling-id}
           {:task "Test raw dispatch" :raw? true})
          ;; Verify :raw? was threaded through to SDK layer
          (is (true? (:raw? (:opts @captured-opts)))
              ":raw? should be threaded through strategy to SDK dispatch")
          (is (= ling-id (:id @captured-opts)))
          (is (= "Test raw dispatch" (:task @captured-opts)))))
      (unregister! ling-id))))

(deftest strategy-dispatch-without-raw-omits-option-test
  (testing "AgentSDKStrategy.strategy-dispatch! without :raw? passes nil"
    (let [ling-id (gen-ling-id)
          register! @#'sdk/register-session!
          unregister! @#'sdk/unregister-session!
          captured-opts (atom nil)]
      (register! ling-id {:ling-id ling-id
                          :phase :idle
                          :cwd "/tmp"
                          :client-ref (str "_hive_client_test")
                          :py-loop-var (str "_hive_loop_test")
                          :turn-count 0})
      (with-redefs [sdk/dispatch-headless-sdk!
                    (fn [id task & [opts]]
                      (reset! captured-opts {:id id :task task :opts opts})
                      (let [ch (chan 1)]
                        (close! ch)
                        ch))]
        (let [strategy-inst (sdk-strat/->agent-sdk-strategy)]
          ;; Dispatch WITHOUT :raw?
          (strategy/strategy-dispatch!
           strategy-inst
           {:id ling-id}
           {:task "Normal SAA dispatch"})
          ;; :raw? should not be present (nil in the select-keys result)
          (is (nil? (:raw? (:opts @captured-opts)))
              ":raw? should be nil when not provided")))
      (unregister! ling-id))))

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

(deftest ^:integration multi-turn-dispatch-test
  (testing "Multiple dispatches to same ling increment turn count"
    (when (sdk/available?)
      (let [ling-id (gen-ling-id)
            _ (sdk/spawn-headless-sdk! ling-id {:cwd "/tmp"})]
        ;; First dispatch (raw)
        (let [ch1 (sdk/dispatch-headless-sdk! ling-id "What is 2+2?" {:raw? true})]
          (loop [] (when (<!! ch1) (recur))))
        ;; Check turn count after first dispatch
        (is (pos? (:turn-count (sdk/get-session ling-id)))
            "Turn count should increment after dispatch")
        ;; Second dispatch (raw)
        (let [ch2 (sdk/dispatch-headless-sdk! ling-id "What is 3+3?" {:raw? true})]
          (loop [] (when (<!! ch2) (recur))))
        ;; Turn count should be higher
        (is (>= (:turn-count (sdk/get-session ling-id)) 2)
            "Turn count should be >= 2 after two dispatches")
        (sdk/kill-headless-sdk! ling-id)))))

(comment
  ;; Run tests
  ;; (clojure.test/run-tests 'hive-mcp.agent.headless-sdk-test)
  )
