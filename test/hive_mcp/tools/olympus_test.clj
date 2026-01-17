(ns hive-mcp.tools.olympus-test
  "Tests for Olympus MCP tool handlers.

   TDD: Tests written FIRST, implementation follows.

   Tools:
   - olympus_status: Get current grid layout and ling positions
   - olympus_focus: Focus/maximize specific ling
   - olympus_arrange: Trigger grid arrangement"
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.tools.olympus :as olympus-tools]
            [hive-mcp.swarm.datascript.core :as ds]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn test-fixture [f]
  ;; Guard: only reset if coordinator not running (TAO-adaptive)
  (when-not (try
              (some? @ds/conn)
              (catch Exception _ false))
    (ds/init-conn!))
  (f))

(use-fixtures :each test-fixture)

;;; =============================================================================
;;; olympus_status Tests
;;; =============================================================================

(deftest handle-olympus-status-empty
  (testing "Status with no lings returns empty grid"
    (let [result (olympus-tools/handle-olympus-status {})]
      (is (:success result))
      (is (= 0 (:ling-count result)))
      (is (map? (:layout result)))
      (is (map? (:positions result))))))

(deftest handle-olympus-status-with-lings
  (testing "Status reflects current ling count"
    ;; Setup: Would need to register lings in DataScript
    ;; For stub, just verify structure
    (let [result (olympus-tools/handle-olympus-status {})]
      (is (contains? result :success))
      (is (contains? result :ling-count))
      (is (contains? result :layout))
      (is (contains? result :positions))
      (is (contains? result :active-tab)))))

(deftest handle-olympus-status-layout-mode
  (testing "Status includes layout mode"
    (let [result (olympus-tools/handle-olympus-status {})]
      (is (contains? #{:auto :manual :stacked} (:layout-mode result))))))

;;; =============================================================================
;;; olympus_focus Tests
;;; =============================================================================

(deftest handle-olympus-focus-valid-ling
  (testing "Focus on valid ling ID succeeds"
    (let [result (olympus-tools/handle-olympus-focus {:ling-id "ling-1"})]
      ;; Stub returns success structure
      (is (contains? result :success))
      (is (contains? result :focused-ling)))))

(deftest handle-olympus-focus-by-number
  (testing "Focus by position number (1-4)"
    (let [result (olympus-tools/handle-olympus-focus {:position 1})]
      (is (contains? result :success)))))

(deftest handle-olympus-focus-invalid
  (testing "Focus on non-existent ling returns error"
    (let [result (olympus-tools/handle-olympus-focus {:ling-id "nonexistent"})]
      (is (false? (:success result)))
      (is (contains? result :error)))))

(deftest handle-olympus-focus-restore
  (testing "Focus with restore flag returns to grid"
    (let [result (olympus-tools/handle-olympus-focus {:restore true})]
      (is (:success result))
      (is (nil? (:focused-ling result))))))

;;; =============================================================================
;;; olympus_arrange Tests
;;; =============================================================================

(deftest handle-olympus-arrange-auto
  (testing "Arrange with auto mode calculates optimal layout"
    (let [result (olympus-tools/handle-olympus-arrange {:mode :auto})]
      (is (:success result))
      (is (= :auto (:layout-mode result))))))

(deftest handle-olympus-arrange-manual
  (testing "Arrange with manual mode preserves positions"
    (let [result (olympus-tools/handle-olympus-arrange {:mode :manual})]
      (is (:success result))
      (is (= :manual (:layout-mode result))))))

(deftest handle-olympus-arrange-stacked
  (testing "Arrange with stacked mode overlays lings"
    (let [result (olympus-tools/handle-olympus-arrange {:mode :stacked})]
      (is (:success result))
      (is (= :stacked (:layout-mode result))))))

(deftest handle-olympus-arrange-refresh
  (testing "Arrange triggers Emacs window refresh"
    (let [result (olympus-tools/handle-olympus-arrange {})]
      (is (:success result))
      (is (contains? result :refreshed)))))

;;; =============================================================================
;;; olympus_tab Tests
;;; =============================================================================

(deftest handle-olympus-tab-next
  (testing "Tab navigation to next tab"
    (let [result (olympus-tools/handle-olympus-tab {:direction :next})]
      (is (:success result))
      (is (contains? result :active-tab)))))

(deftest handle-olympus-tab-prev
  (testing "Tab navigation to previous tab"
    (let [result (olympus-tools/handle-olympus-tab {:direction :prev})]
      (is (:success result)))))

(deftest handle-olympus-tab-specific
  (testing "Jump to specific tab number"
    (let [result (olympus-tools/handle-olympus-tab {:tab 2})]
      (is (:success result))
      (is (= 2 (:active-tab result))))))

;;; =============================================================================
;;; DataScript Integration Tests
;;; =============================================================================

(deftest olympus-state-persisted
  (testing "Olympus state stored in DataScript"
    ;; After arrange, state should be queryable
    (olympus-tools/handle-olympus-arrange {:mode :auto})
    (let [status (olympus-tools/handle-olympus-status {})]
      (is (= :auto (:layout-mode status))))))

(deftest olympus-positions-update
  (testing "Positions update when lings change"
    ;; This would require DataScript ling creation
    ;; Stub verifies the contract
    (let [before (olympus-tools/handle-olympus-status {})
          ;; Simulate adding a ling (would be done via swarm_spawn)
          after (olympus-tools/handle-olympus-status {})]
      (is (map? (:positions before)))
      (is (map? (:positions after))))))
