(ns hive-mcp.agent.sdk-spawn-test
  "Integration tests for Agent SDK spawn lifecycle.

   Validates:
   1. SDK availability check — sdk-status returns meaningful status
   2. Session lifecycle — spawn creates session with expected keys
   3. Readiness check — session phase transitions: :spawning → :idle → :working
   4. Error cases — spawn with SDK unavailable returns error (not silent fallback)
   5. Dispatch timing — dispatch after readiness confirmed succeeds
   6. Forge-strike headless path — simulate forge spawn-one-headless! lifecycle

   All tests mock Python interop at the requiring-resolve boundary
   to avoid needing libpython-clj on the test classpath."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.core.async :as async :refer [<!! >!! chan close!]]
            [hive-mcp.agent.sdk.availability :as avail]
            [hive-mcp.agent.sdk.session :as session]
            [hive-mcp.agent.sdk.lifecycle :as lifecycle]
            [hive-mcp.agent.sdk.event-loop :as event-loop]
            [hive-mcp.agent.sdk.execution :as exec]
            [hive-mcp.agent.sdk.options :as opts]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Infrastructure
;; =============================================================================

(def ^:private avail-atom
  "Access the private sdk-available? atom for test manipulation."
  @#'avail/sdk-available?)

(def ^:private session-registry
  "Access the private session-registry atom for test inspection."
  @#'session/session-registry)

(defn- fresh-state!
  "Reset all SDK state to clean baseline."
  []
  (reset! avail-atom nil)
  (reset! session-registry {}))

(defn sdk-fixture
  "Reset SDK state before and after each test."
  [f]
  (fresh-state!)
  (try (f)
       (finally (fresh-state!))))

(use-fixtures :each sdk-fixture)

;; =============================================================================
;; Section 1: SDK Availability Check
;;
;; Tests that sdk-status returns meaningful status keywords,
;; and that the caching mechanism works correctly.
;; =============================================================================

(deftest sdk-status-returns-keyword
  (testing "sdk-status returns a keyword status (not nil, not exception)"
    (let [status (avail/sdk-status)]
      (is (keyword? status)
          (str "sdk-status should return a keyword, got: " (type status)))
      (is (contains? #{:available :no-libpython :no-sdk :not-initialized} status)
          (str "sdk-status should return a known status keyword, got: " status)))))

(deftest sdk-status-caching
  (testing "sdk-status caches result after first call"
    ;; Force a known status
    (reset! avail-atom :no-libpython)
    (is (= :no-libpython (avail/sdk-status))
        "Should return cached status without re-checking")

    ;; Change cache, verify it sticks
    (reset! avail-atom :available)
    (is (= :available (avail/sdk-status))
        "Should return updated cached status")))

(deftest sdk-status-reset
  (testing "reset-availability! clears the cache"
    (reset! avail-atom :available)
    (is (= :available (avail/sdk-status)))
    (avail/reset-availability!)
    (is (nil? @avail-atom)
        "reset-availability! should set cache to nil")))

(deftest available?-predicate
  (testing "available? returns true only when status is :available"
    (reset! avail-atom :available)
    (is (true? (avail/available?)))

    (reset! avail-atom :no-libpython)
    (is (false? (avail/available?)))

    (reset! avail-atom :no-sdk)
    (is (false? (avail/available?)))

    (reset! avail-atom :not-initialized)
    (is (false? (avail/available?)))))

;; =============================================================================
;; Section 2: Session Registry Lifecycle
;;
;; Tests that session register/unregister/get/update work correctly
;; without needing Python at all — pure atom-based registry.
;; =============================================================================

(deftest session-register-and-get
  (testing "register-session! stores session, get-session retrieves it"
    (let [test-data {:ling-id "test-ling-1"
                     :phase :idle
                     :phase-history []
                     :observations []
                     :started-at (System/currentTimeMillis)
                     :cwd "/tmp/test"}]
      (session/register-session! "test-ling-1" test-data)
      (let [retrieved (session/get-session "test-ling-1")]
        (is (some? retrieved) "Should retrieve registered session")
        (is (= :idle (:phase retrieved)) "Phase should be :idle")
        (is (= "/tmp/test" (:cwd retrieved)) "CWD should match")
        (is (= "test-ling-1" (:ling-id retrieved)) "Ling ID should match")))))

(deftest session-update
  (testing "update-session! merges new data into existing session"
    (session/register-session! "test-ling-2"
                                {:ling-id "test-ling-2"
                                 :phase :idle
                                 :turn-count 0})
    (session/update-session! "test-ling-2" {:phase :silence :turn-count 1})
    (let [updated (session/get-session "test-ling-2")]
      (is (= :silence (:phase updated)) "Phase should be updated to :silence")
      (is (= 1 (:turn-count updated)) "Turn count should be updated to 1")
      (is (= "test-ling-2" (:ling-id updated)) "Ling ID should be preserved"))))

(deftest session-unregister
  (testing "unregister-session! removes session from registry"
    (session/register-session! "test-ling-3"
                                {:ling-id "test-ling-3" :phase :idle})
    (is (some? (session/get-session "test-ling-3"))
        "Session should exist before unregister")
    (session/unregister-session! "test-ling-3")
    (is (nil? (session/get-session "test-ling-3"))
        "Session should be nil after unregister")))

(deftest session-unregister-closes-channels
  (testing "unregister-session! closes message-ch and result-ch"
    (let [msg-ch (chan 10)
          res-ch (chan 1)]
      (session/register-session! "test-ling-4"
                                  {:ling-id "test-ling-4"
                                   :phase :idle
                                   :message-ch msg-ch
                                   :result-ch res-ch})
      (session/unregister-session! "test-ling-4")
      ;; Verify channels are closed by attempting to put
      (is (false? (async/put! msg-ch :test))
          "message-ch should be closed after unregister")
      (is (false? (async/put! res-ch :test))
          "result-ch should be closed after unregister"))))

(deftest session-get-nonexistent
  (testing "get-session returns nil for unregistered ling-id"
    (is (nil? (session/get-session "nonexistent-ling"))
        "Should return nil for unknown ling-id")))

(deftest session-registry-isolation
  (testing "Multiple sessions coexist independently"
    (session/register-session! "ling-A" {:ling-id "ling-A" :phase :idle})
    (session/register-session! "ling-B" {:ling-id "ling-B" :phase :working})
    (is (= :idle (:phase (session/get-session "ling-A"))))
    (is (= :working (:phase (session/get-session "ling-B"))))

    ;; Update A, B should be unaffected
    (session/update-session! "ling-A" {:phase :silence})
    (is (= :silence (:phase (session/get-session "ling-A"))))
    (is (= :working (:phase (session/get-session "ling-B")))
        "Session B should be unaffected by update to A")))

;; =============================================================================
;; Section 3: Session Phase Transitions
;;
;; Tests the phase transition mechanics that underlie readiness checks:
;; :spawning → :idle → :working (via :silence/:abstract/:act)
;; =============================================================================

(deftest phase-transition-lifecycle
  (testing "Session phases transition through expected lifecycle"
    ;; Simulate spawn → register at :idle
    (session/register-session! "phase-ling"
                                {:ling-id "phase-ling"
                                 :phase :idle
                                 :phase-history []
                                 :turn-count 0})

    ;; Verify initial state
    (is (= :idle (:phase (session/get-session "phase-ling"))))

    ;; Transition to :silence (SAA first phase)
    (session/update-session! "phase-ling" {:phase :silence :turn-count 1})
    (is (= :silence (:phase (session/get-session "phase-ling"))))

    ;; Transition to :abstract (SAA second phase)
    (session/update-session! "phase-ling" {:phase :abstract :turn-count 2})
    (is (= :abstract (:phase (session/get-session "phase-ling"))))

    ;; Transition to :act (SAA third phase)
    (session/update-session! "phase-ling" {:phase :act :turn-count 3})
    (is (= :act (:phase (session/get-session "phase-ling"))))
    (is (= 3 (:turn-count (session/get-session "phase-ling"))))))

(deftest phase-with-observations
  (testing "Observations accumulate during silence phase"
    (session/register-session! "obs-ling"
                                {:ling-id "obs-ling"
                                 :phase :silence
                                 :observations []})

    (session/update-session! "obs-ling"
                              {:observations ["Found auth.clj"
                                              "Found handler.clj"]})
    (let [sess (session/get-session "obs-ling")]
      (is (= 2 (count (:observations sess))))
      (is (= "Found auth.clj" (first (:observations sess)))))))

;; =============================================================================
;; Section 4: Spawn Error Cases
;;
;; Tests that spawn-headless-sdk! properly rejects when SDK is unavailable,
;; throwing ex-info with useful diagnostic data (not silent fallback).
;; =============================================================================

(deftest spawn-rejects-when-no-libpython
  (testing "spawn-headless-sdk! throws when SDK status is :no-libpython"
    (reset! avail-atom :no-libpython)
    (let [ex (try
               (lifecycle/spawn-headless-sdk! "err-ling-1"
                                               {:cwd "/tmp/test"})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw ExceptionInfo")
      (is (= "Claude Agent SDK not available" (ex-message ex)))
      (let [data (ex-data ex)]
        (is (= :no-libpython (:sdk-status data))
            "Exception data should contain SDK status")
        (is (= "err-ling-1" (:ling-id data))
            "Exception data should contain ling-id")
        (is (string? (:hint data))
            "Exception data should contain a hint string")))))

(deftest spawn-rejects-when-no-sdk
  (testing "spawn-headless-sdk! throws when SDK status is :no-sdk"
    (reset! avail-atom :no-sdk)
    (let [ex (try
               (lifecycle/spawn-headless-sdk! "err-ling-2"
                                               {:cwd "/tmp/test"})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw ExceptionInfo")
      (let [data (ex-data ex)]
        (is (= :no-sdk (:sdk-status data)))
        (is (string? (:hint data)))))))

(deftest spawn-rejects-when-not-initialized
  (testing "spawn-headless-sdk! throws when SDK status is :not-initialized"
    (reset! avail-atom :not-initialized)
    (let [ex (try
               (lifecycle/spawn-headless-sdk! "err-ling-3"
                                               {:cwd "/tmp/test"})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw ExceptionInfo")
      (is (= :not-initialized (:sdk-status (ex-data ex)))))))

(deftest spawn-rejects-duplicate-session
  (testing "spawn-headless-sdk! throws when session already exists"
    (reset! avail-atom :available)
    ;; Pre-register a session
    (session/register-session! "dup-ling"
                                {:ling-id "dup-ling" :phase :idle})
    (let [ex (try
               (lifecycle/spawn-headless-sdk! "dup-ling"
                                               {:cwd "/tmp/test"})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw ExceptionInfo for duplicate session")
      (is (= "SDK session already exists with this ID" (ex-message ex)))
      (is (= "dup-ling" (:ling-id (ex-data ex)))))))

(deftest spawn-precondition-requires-cwd
  (testing "spawn-headless-sdk! precondition requires cwd to be a string"
    (reset! avail-atom :available)
    (is (thrown? AssertionError
                 (lifecycle/spawn-headless-sdk! "no-cwd-ling" {}))
        "Missing cwd should trigger assertion error")
    (is (thrown? AssertionError
                 (lifecycle/spawn-headless-sdk! "nil-cwd-ling" {:cwd nil}))
        "Nil cwd should trigger assertion error")))

;; =============================================================================
;; Section 5: Spawn Success (Mocked Python)
;;
;; Tests that spawn-headless-sdk! creates a proper session with expected
;; keys when SDK is available, mocking the Python interop layer.
;; =============================================================================

(deftest spawn-success-creates-session
  (testing "spawn-headless-sdk! with mocked Python creates proper session"
    (reset! avail-atom :available)
    (with-redefs [event-loop/start-session-loop!
                  (fn [safe-id]
                    {:loop-var (str "_hive_loop_" safe-id)
                     :thread-var (str "_hive_loop_thread_" safe-id)})

                  event-loop/connect-session-client!
                  (fn [safe-id _opts loop-var]
                    (str "_hive_client_" safe-id))

                  opts/build-base-options-obj
                  (fn [_opts] :mock-options)]

      (let [result (lifecycle/spawn-headless-sdk!
                    "spawn-test-1"
                    {:cwd "/tmp/test"
                     :system-prompt "Test prompt"
                     :presets ["ling" "saa"]})]

        ;; Verify return value
        (is (map? result) "Result should be a map")
        (is (= "spawn-test-1" (:ling-id result)))
        (is (= :spawned (:status result)))
        (is (= :agent-sdk (:backend result)))
        (is (= :idle (:phase result)))

        ;; Verify session was registered
        (let [sess (session/get-session "spawn-test-1")]
          (is (some? sess) "Session should be registered")
          (is (= :idle (:phase sess)) "Phase should start at :idle")
          (is (= [] (:phase-history sess)) "Phase history should be empty")
          (is (= [] (:observations sess)) "Observations should be empty")
          (is (= "/tmp/test" (:cwd sess)) "CWD should match")
          (is (= "Test prompt" (:system-prompt sess)) "System prompt should match")
          (is (= ["ling" "saa"] (:presets sess)) "Presets should match")
          (is (some? (:message-ch sess)) "Should have message channel")
          (is (some? (:result-ch sess)) "Should have result channel")
          (is (some? (:started-at sess)) "Should have started-at timestamp")
          (is (string? (:client-ref sess)) "Should have client-ref string")
          (is (string? (:py-loop-var sess)) "Should have py-loop-var string")
          (is (string? (:py-safe-id sess)) "Should have py-safe-id string")
          (is (= 0 (:turn-count sess)) "Turn count should start at 0")
          (is (nil? (:plan sess)) "Plan should be nil initially"))))))

;; =============================================================================
;; Section 6: Dispatch Lifecycle
;;
;; Tests dispatch-headless-sdk! behavior: requires session, requires
;; persistent client, and returns a channel when successful.
;; =============================================================================

(deftest dispatch-requires-session
  (testing "dispatch-headless-sdk! throws when session not found"
    (let [ex (try
               (lifecycle/dispatch-headless-sdk! "nonexistent-ling" "do work")
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw for missing session")
      (is (= "SDK session not found" (ex-message ex))))))

(deftest dispatch-requires-client
  (testing "dispatch-headless-sdk! throws when no persistent client"
    ;; Register session without client-ref
    (session/register-session! "no-client-ling"
                                {:ling-id "no-client-ling"
                                 :phase :idle
                                 :client-ref nil})
    (let [ex (try
               (lifecycle/dispatch-headless-sdk! "no-client-ling" "do work")
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw for missing client")
      (is (= "No persistent client (was spawn successful?)" (ex-message ex))))))

(deftest dispatch-returns-channel
  (testing "dispatch-headless-sdk! returns async channel on success"
    ;; Register a session with client-ref (simulates successful spawn)
    (session/register-session! "dispatch-ling"
                                {:ling-id "dispatch-ling"
                                 :phase :idle
                                 :client-ref "_hive_client_dispatch_ling"
                                 :py-loop-var "_hive_loop_dispatch_ling"
                                 :py-safe-id "dispatch_ling"
                                 :turn-count 0
                                 :observations []
                                 :cwd "/tmp/test"})

    ;; Mock execute-phase! to return a channel with test data
    (with-redefs [exec/execute-phase!
                  (fn [ling-id prompt phase]
                    (let [ch (chan 1)]
                      (async/go
                        (>!! ch {:type :message :phase phase :data "mock result"})
                        (close! ch))
                      ch))]
      (let [out-ch (lifecycle/dispatch-headless-sdk!
                    "dispatch-ling" "Test task" {:raw? true})]
        (is (some? out-ch) "Should return a channel")
        ;; Read from channel (with timeout)
        (let [msg (async/alt!!
                    out-ch ([v] v)
                    (async/timeout 5000) :timeout)]
          (is (not= :timeout msg) "Should receive message before timeout")
          (is (map? msg) "Message should be a map"))))))

;; =============================================================================
;; Section 7: Kill Lifecycle
;;
;; Tests kill-headless-sdk! teardown behavior.
;; =============================================================================

(deftest kill-requires-session
  (testing "kill-headless-sdk! throws when session not found"
    (let [ex (try
               (lifecycle/kill-headless-sdk! "nonexistent-kill-ling")
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "Should throw for missing session")
      (is (= "SDK session not found" (ex-message ex))))))

(deftest kill-removes-session
  (testing "kill-headless-sdk! removes session and returns result"
    ;; Register a session with mock Python refs
    (session/register-session! "kill-ling"
                                {:ling-id "kill-ling"
                                 :phase :idle
                                 :client-ref "_hive_client_kill_ling"
                                 :py-loop-var "_hive_loop_kill_ling"
                                 :py-safe-id "kill_ling"
                                 :message-ch (chan 10)
                                 :result-ch (chan 1)})

    (with-redefs [event-loop/disconnect-session-client!
                  (fn [_safe-id _loop-var _client-var] nil)
                  event-loop/stop-session-loop!
                  (fn [_safe-id _loop-var] nil)]
      (let [result (lifecycle/kill-headless-sdk! "kill-ling")]
        (is (true? (:killed? result)) "Should report killed")
        (is (= "kill-ling" (:ling-id result)))
        (is (nil? (session/get-session "kill-ling"))
            "Session should be removed after kill")))))

;; =============================================================================
;; Section 8: Status Reporting
;;
;; Tests sdk-status-for and list-sdk-sessions.
;; =============================================================================

(deftest sdk-status-for-registered-session
  (testing "sdk-status-for returns rich status for registered session"
    (session/register-session! "status-ling"
                                {:ling-id "status-ling"
                                 :phase :working
                                 :phase-history [:idle :silence]
                                 :observations ["obs-1" "obs-2"]
                                 :started-at (- (System/currentTimeMillis) 5000)
                                 :cwd "/tmp/status"
                                 :session-id "sess-abc"
                                 :turn-count 3
                                 :client-ref "_hive_client_status_ling"
                                 :py-loop-var "_hive_loop_status_ling"})
    (let [status (lifecycle/sdk-status-for "status-ling")]
      (is (some? status) "Should return status for registered session")
      (is (= "status-ling" (:ling-id status)))
      (is (= :working (:phase status)))
      (is (= [:idle :silence] (:phase-history status)))
      (is (= 2 (:observations-count status)))
      (is (= "/tmp/status" (:cwd status)))
      (is (= :agent-sdk (:backend status)))
      (is (= "sess-abc" (:session-id status)))
      (is (= 3 (:turn-count status)))
      (is (true? (:has-persistent-client? status)))
      (is (true? (:interruptable? status)))
      (is (pos? (:uptime-ms status))
          "Uptime should be positive"))))

(deftest sdk-status-for-nonexistent
  (testing "sdk-status-for returns nil for unregistered ling"
    (is (nil? (lifecycle/sdk-status-for "nonexistent-status-ling")))))

(deftest list-sdk-sessions-empty
  (testing "list-sdk-sessions returns empty vec when no sessions"
    (is (= [] (lifecycle/list-sdk-sessions)))))

(deftest list-sdk-sessions-multiple
  (testing "list-sdk-sessions returns status for all registered sessions"
    (session/register-session! "list-ling-1"
                                {:ling-id "list-ling-1" :phase :idle
                                 :phase-history [] :observations []
                                 :started-at (System/currentTimeMillis)
                                 :cwd "/tmp/a" :turn-count 0})
    (session/register-session! "list-ling-2"
                                {:ling-id "list-ling-2" :phase :working
                                 :phase-history [] :observations []
                                 :started-at (System/currentTimeMillis)
                                 :cwd "/tmp/b" :turn-count 2})
    (let [sessions (lifecycle/list-sdk-sessions)]
      (is (= 2 (count sessions)) "Should list 2 sessions")
      (let [ids (set (map :ling-id sessions))]
        (is (contains? ids "list-ling-1"))
        (is (contains? ids "list-ling-2"))))))

(deftest sdk-session?-predicate
  (testing "sdk-session? returns true for registered, false for unknown"
    (session/register-session! "check-ling"
                                {:ling-id "check-ling" :phase :idle})
    (is (true? (lifecycle/sdk-session? "check-ling")))
    (is (false? (lifecycle/sdk-session? "unknown-ling")))))

;; =============================================================================
;; Section 9: Interrupt Lifecycle
;;
;; Tests interrupt-headless-sdk! behavior for various session states.
;; =============================================================================

(deftest interrupt-no-session
  (testing "interrupt returns failure when session not found"
    (let [result (lifecycle/interrupt-headless-sdk! "no-such-ling")]
      (is (false? (:success? result)))
      (is (= "no-such-ling" (:ling-id result)))
      (is (seq (:errors result)) "Should have error messages"))))

(deftest interrupt-no-client
  (testing "interrupt returns failure when no client ref"
    (session/register-session! "int-no-client"
                                {:ling-id "int-no-client"
                                 :phase :idle
                                 :client-ref nil
                                 :py-loop-var nil})
    (let [result (lifecycle/interrupt-headless-sdk! "int-no-client")]
      (is (false? (:success? result)))
      (is (seq (:errors result))))))

(deftest interrupt-with-client-success
  (testing "interrupt succeeds when client and loop are present"
    (session/register-session! "int-ling"
                                {:ling-id "int-ling"
                                 :phase :silence
                                 :client-ref "_hive_client_int_ling"
                                 :py-loop-var "_hive_loop_int_ling"})
    (with-redefs [event-loop/interrupt-session-client!
                  (fn [_client-var _loop-var]
                    {:success? true})]
      (let [result (lifecycle/interrupt-headless-sdk! "int-ling")]
        (is (true? (:success? result)))
        (is (= "int-ling" (:ling-id result)))
        (is (= :silence (:phase result)))))))

;; =============================================================================
;; Section 10: Kill-All Lifecycle
;;
;; Tests kill-all-sdk! batch teardown.
;; =============================================================================

(deftest kill-all-empty
  (testing "kill-all-sdk! with no sessions returns 0 killed"
    (let [result (lifecycle/kill-all-sdk!)]
      (is (= 0 (:killed result)))
      (is (= 0 (:errors result))))))

(deftest kill-all-multiple
  (testing "kill-all-sdk! kills all registered sessions"
    (session/register-session! "kill-all-1"
                                {:ling-id "kill-all-1" :phase :idle
                                 :message-ch (chan 1) :result-ch (chan 1)})
    (session/register-session! "kill-all-2"
                                {:ling-id "kill-all-2" :phase :working
                                 :message-ch (chan 1) :result-ch (chan 1)})
    ;; No Python refs, so disconnect/stop won't be called
    (let [result (lifecycle/kill-all-sdk!)]
      (is (= 2 (:killed result)) "Should kill both sessions")
      (is (= 0 (:errors result)))
      (is (empty? @session-registry)
          "Registry should be empty after kill-all"))))

;; =============================================================================
;; Section 11: Safe ID Conversion
;;
;; Tests ling-id to Python-safe identifier conversion.
;; =============================================================================

(deftest safe-id-conversion
  (testing "ling-id->safe-id replaces non-alphanumeric chars with underscore"
    (is (= "swarm_worker_123" (session/ling-id->safe-id "swarm-worker-123")))
    (is (= "simple" (session/ling-id->safe-id "simple")))
    (is (= "with_spaces_and_dots" (session/ling-id->safe-id "with spaces.and.dots")))
    (is (= "special__chars__" (session/ling-id->safe-id "special!@chars#$")))))

;; =============================================================================
;; Section 12: Forge-Strike Headless Simulation
;;
;; Simulates the lifecycle of forge spawn-one-headless! without
;; requiring actual subprocess or Python — tests the integration
;; between SDK lifecycle and forge workflow.
;; =============================================================================

(deftest forge-spawn-lifecycle-simulation
  (testing "Simulated forge spawn-one-headless! lifecycle"
    (reset! avail-atom :available)

    ;; Step 1: Spawn — creates session at :idle
    (with-redefs [event-loop/start-session-loop!
                  (fn [safe-id]
                    {:loop-var (str "_hive_loop_" safe-id)
                     :thread-var (str "_hive_loop_thread_" safe-id)})
                  event-loop/connect-session-client!
                  (fn [safe-id _opts _loop-var]
                    (str "_hive_client_" safe-id))
                  opts/build-base-options-obj
                  (fn [_] :mock-opts)]

      (let [spawn-result (lifecycle/spawn-headless-sdk!
                          "forge-ling-1"
                          {:cwd "/tmp/forge"
                           :system-prompt "Forge test"
                           :presets ["ling" "saa"]})]
        (is (= :spawned (:status spawn-result)))
        (is (= :idle (:phase spawn-result)))

        ;; Step 2: Verify readiness — session phase is :idle
        (let [sess (session/get-session "forge-ling-1")]
          (is (= :idle (:phase sess))
              "After spawn, session should be at :idle (ready for dispatch)"))

        ;; Step 3: Status check — should show as spawned
        (let [status (lifecycle/sdk-status-for "forge-ling-1")]
          (is (= :idle (:phase status)))
          (is (= :agent-sdk (:backend status)))
          (is (true? (:has-persistent-client? status))))

        ;; Step 4: Dispatch — triggers SAA phases
        (with-redefs [exec/execute-phase!
                      (fn [ling-id prompt phase]
                        (let [ch (chan 1)]
                          ;; Simulate phase execution completing
                          (async/go
                            (>!! ch {:type :message :phase phase
                                     :data (str "Phase " (name phase) " result")})
                            (close! ch))
                          ch))]
          (let [out-ch (lifecycle/dispatch-headless-sdk!
                        "forge-ling-1" "Implement feature X"
                        {:raw? true})]
            (is (some? out-ch) "Dispatch should return channel")

            ;; Read messages (with timeout)
            (loop [msgs []]
              (let [msg (async/alt!!
                          out-ch ([v] v)
                          (async/timeout 5000) :timeout)]
                (if (or (nil? msg) (= :timeout msg))
                  ;; Verify we got messages
                  (is (pos? (count msgs))
                      "Should have received at least one message from dispatch")
                  (recur (conj msgs msg)))))))

        ;; Step 5: Kill — teardown
        (with-redefs [event-loop/disconnect-session-client!
                      (fn [_ _ _] nil)
                      event-loop/stop-session-loop!
                      (fn [_ _] nil)]
          (let [kill-result (lifecycle/kill-headless-sdk! "forge-ling-1")]
            (is (true? (:killed? kill-result)))
            (is (nil? (session/get-session "forge-ling-1"))
                "Session should be cleaned up after kill")))))))

(deftest forge-spawn-multiple-lings-isolation
  (testing "Multiple forge lings maintain independent sessions"
    (reset! avail-atom :available)

    (with-redefs [event-loop/start-session-loop!
                  (fn [safe-id]
                    {:loop-var (str "_hive_loop_" safe-id)
                     :thread-var (str "_hive_loop_thread_" safe-id)})
                  event-loop/connect-session-client!
                  (fn [safe-id _opts _loop-var]
                    (str "_hive_client_" safe-id))
                  opts/build-base-options-obj
                  (fn [_] :mock-opts)]

      ;; Spawn 3 forge lings
      (doseq [i (range 1 4)]
        (lifecycle/spawn-headless-sdk!
         (str "forge-multi-" i)
         {:cwd (str "/tmp/forge-" i)
          :system-prompt (str "Forge test " i)}))

      ;; Verify all 3 exist independently
      (is (= 3 (count (lifecycle/list-sdk-sessions)))
          "Should have 3 active sessions")

      ;; Verify each has correct CWD
      (doseq [i (range 1 4)]
        (let [sess (session/get-session (str "forge-multi-" i))]
          (is (= (str "/tmp/forge-" i) (:cwd sess))
              (str "Forge ling " i " should have correct CWD"))))

      ;; Kill one, verify others survive
      (with-redefs [event-loop/disconnect-session-client! (fn [_ _ _] nil)
                    event-loop/stop-session-loop! (fn [_ _] nil)]
        (lifecycle/kill-headless-sdk! "forge-multi-2"))

      (is (= 2 (count (lifecycle/list-sdk-sessions)))
          "Should have 2 sessions after killing one")
      (is (nil? (session/get-session "forge-multi-2"))
          "Killed session should be gone")
      (is (some? (session/get-session "forge-multi-1"))
          "Other sessions should survive")
      (is (some? (session/get-session "forge-multi-3"))
          "Other sessions should survive")

      ;; Cleanup
      (with-redefs [event-loop/disconnect-session-client! (fn [_ _ _] nil)
                    event-loop/stop-session-loop! (fn [_ _] nil)]
        (lifecycle/kill-all-sdk!)))))

(comment
  ;; Run all SDK spawn tests
  (clojure.test/run-tests 'hive-mcp.agent.sdk-spawn-test))
