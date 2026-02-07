(ns hive-mcp.tools.consolidated.workflow-test
  "Tests for forge spark's ling-ready poll mechanism.

   Validates that wait-for-ling-ready correctly polls both DataScript
   registration AND CLI readiness before dispatching.

   Two-phase readiness:
     Phase 1: DataScript slave entry exists (usually instant after spawn!)
     Phase 2: CLI ready — mode-specific (vterm prompt marker, headless stdout)

   CLARITY: T - Telemetry (test) validates behavioral correctness."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datascript.core :as d]
            [hive-mcp.tools.consolidated.workflow :as workflow]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.schema :as schema]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Access private functions via var
(def wait-for-ling-ready @#'workflow/wait-for-ling-ready)

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:dynamic *test-conn* nil)

(defn isolated-ds-fixture
  "Each test gets a fresh DataScript connection.
   Swaps the value inside the private conn atom, preserving production conn."
  [f]
  (let [conn-atom  @#'conn/conn          ;; The atom itself (defonce ^:private conn (atom nil))
        test-conn  (d/create-conn schema/schema)
        saved-conn @conn-atom]            ;; Save current DS connection
    (reset! conn-atom test-conn)
    (binding [*test-conn* test-conn]
      (try
        (f)
        (finally
          (reset! conn-atom saved-conn))))))

(use-fixtures :each isolated-ds-fixture)

;; =============================================================================
;; Phase 1: DataScript Registration Tests
;; =============================================================================

(deftest test-ds-timeout-when-slave-missing
  (testing "Returns :ds-timeout when slave never appears in DataScript"
    (with-redefs [workflow/ling-ready-timeout-ms 150
                  workflow/ling-ready-poll-ms 10]
      (let [result (wait-for-ling-ready "nonexistent-ling" :vterm)]
        (is (not (:ready? result)) "Should not be ready")
        (is (>= (:attempts result) 2) "Should have polled multiple times")
        (is (= :ds-timeout (:phase result)) "Should be DS timeout phase")
        (is (nil? (:slave result)) "Should have no slave data")
        (is (>= (:elapsed-ms result) 100) "Should have waited near timeout")))))

(deftest test-ds-delayed-registration
  (testing "Finds slave after delayed DataScript registration + CLI ready"
    (with-redefs [workflow/ling-ready-timeout-ms 5000
                  workflow/ling-ready-poll-ms 10
                  workflow/ling-cli-ready? (constantly true)]
      ;; Schedule slave registration after ~30ms
      (future
        (Thread/sleep 30)
        (ds-lings/add-slave! "delayed-ling" {:status :idle :depth 1 :cwd "/tmp"}))

      (let [result (wait-for-ling-ready "delayed-ling" :vterm)]
        (is (:ready? result) "Should eventually find the slave")
        (is (> (:attempts result) 1) "Should take more than one attempt")
        (is (some? (:slave result)) "Should include slave data")
        (is (= :cli-ready (:phase result)) "Should reach cli-ready phase")))))

;; =============================================================================
;; Phase 2: CLI Readiness Tests
;; =============================================================================

(deftest test-ready-when-ds-and-cli-both-pass
  (testing "Returns ready when DS has slave AND CLI readiness passes"
    (ds-lings/add-slave! "test-ling-001" {:status :idle :depth 1 :cwd "/tmp"})

    (with-redefs [workflow/ling-cli-ready? (constantly true)]
      (let [result (wait-for-ling-ready "test-ling-001" :vterm)]
        (is (:ready? result) "Should be ready")
        (is (= 1 (:attempts result)) "Should find on first attempt")
        (is (< (:elapsed-ms result) 200) "Should be near-instant")
        (is (= :cli-ready (:phase result)) "Should report cli-ready phase")))))

(deftest test-cli-timeout-when-ds-exists-but-cli-not-ready
  (testing "Returns :cli-timeout when DS has slave but CLI never becomes ready"
    (ds-lings/add-slave! "stuck-ling" {:status :idle :depth 1 :cwd "/tmp"})

    (with-redefs [workflow/ling-ready-timeout-ms 150
                  workflow/ling-ready-poll-ms 10
                  workflow/ling-cli-ready? (constantly false)]
      (let [result (wait-for-ling-ready "stuck-ling" :vterm)]
        (is (not (:ready? result)) "Should not be ready")
        (is (>= (:attempts result) 2) "Should have polled multiple times")
        (is (= :cli-timeout (:phase result)) "Should be CLI timeout")
        (is (some? (:slave result)) "Should have slave data (DS found it)")))))

(deftest test-cli-delayed-readiness
  (testing "Polls until CLI becomes ready (DS present from start)"
    (ds-lings/add-slave! "slow-cli-ling" {:status :idle :depth 1 :cwd "/tmp"})

    (let [call-count (atom 0)]
      (with-redefs [workflow/ling-ready-timeout-ms 5000
                    workflow/ling-ready-poll-ms 10
                    workflow/ling-cli-ready? (fn [_agent-id _mode]
                                              ;; Ready after 3rd CLI check
                                               (>= (swap! call-count inc) 3))]
        (let [result (wait-for-ling-ready "slow-cli-ling" :vterm)]
          (is (:ready? result) "Should eventually be ready")
          (is (= 3 (:attempts result)) "Should take 3 attempts")
          (is (= :cli-ready (:phase result))))))))

;; =============================================================================
;; Mode-Specific Dispatch Tests
;; =============================================================================

(deftest test-openrouter-always-cli-ready
  (testing "OpenRouter mode is always CLI-ready (API-based, no CLI startup)"
    (ds-lings/add-slave! "or-ling" {:status :idle :depth 1 :cwd "/tmp"})

    ;; No mocking needed — openrouter returns true by default
    (let [result (wait-for-ling-ready "or-ling" :openrouter)]
      (is (:ready? result) "OpenRouter should be immediately ready")
      (is (= 1 (:attempts result)) "Should pass on first attempt"))))

(deftest test-agent-sdk-always-cli-ready
  (testing "Agent SDK mode is always CLI-ready (in-process)"
    (ds-lings/add-slave! "sdk-ling" {:status :idle :depth 1 :cwd "/tmp"})

    (let [result (wait-for-ling-ready "sdk-ling" :agent-sdk)]
      (is (:ready? result) "Agent SDK should be immediately ready")
      (is (= 1 (:attempts result)) "Should pass on first attempt"))))

(deftest test-vterm-mode-calls-vterm-ready
  (testing "Vterm mode delegates to vterm-ready? via ling-cli-ready?"
    (ds-lings/add-slave! "vterm-ling" {:status :idle :depth 1 :cwd "/tmp"})

    (let [checked-modes (atom [])]
      (with-redefs [workflow/ling-cli-ready? (fn [_id mode]
                                               (swap! checked-modes conj mode)
                                               true)]
        (wait-for-ling-ready "vterm-ling" :vterm)
        (is (= [:vterm] @checked-modes) "Should call ling-cli-ready? with :vterm")))))

(deftest test-headless-mode-calls-headless-ready
  (testing "Headless mode delegates to headless-ready? via ling-cli-ready?"
    (ds-lings/add-slave! "headless-ling" {:status :idle :depth 1 :cwd "/tmp"})

    (let [checked-modes (atom [])]
      (with-redefs [workflow/ling-cli-ready? (fn [_id mode]
                                               (swap! checked-modes conj mode)
                                               true)]
        (wait-for-ling-ready "headless-ling" :headless)
        (is (= [:headless] @checked-modes) "Should call ling-cli-ready? with :headless")))))

;; =============================================================================
;; Poll Timing Tests
;; =============================================================================

(deftest test-poll-timing
  (testing "Poll intervals are fixed at ling-ready-poll-ms (no backoff)"
    (with-redefs [workflow/ling-ready-timeout-ms 200
                  workflow/ling-ready-poll-ms 50
                  workflow/ling-cli-ready? (constantly false)]
      ;; Register slave so we hit CLI check (not DS timeout)
      (ds-lings/add-slave! "timing-ling" {:status :idle :depth 1 :cwd "/tmp"})

      (let [start (System/currentTimeMillis)
            result (wait-for-ling-ready "timing-ling" :vterm)
            elapsed (- (System/currentTimeMillis) start)]
        ;; With 50ms fixed interval and 200ms timeout, should take ~200ms
        (is (>= elapsed 150) "Should have spent time polling")
        (is (<= elapsed 500) "Should not overshoot timeout by too much")
        (is (not (:ready? result)) "Should have timed out")))))
