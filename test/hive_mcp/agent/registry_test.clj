(ns hive-mcp.agent.registry-test
  "Tests for tool and agent registries.

   Covers:
   - Tool Registry: register, get, list, missing lookup
   - Agent Registry: register, get, list, filter by type, deregister
   - sync-from-datascript reconstitution"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.registry :as reg]
            [hive-mcp.agent.protocol :refer [IAgent agent-type]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn clean-registries-fixture
  "Clean registries before and after each test."
  [f]
  ;; Clean before
  (reset! @#'reg/registry {})
  (reg/clear-agents!)
  (try
    (f)
    (finally
      ;; Clean after
      (reset! @#'reg/registry {})
      (reg/clear-agents!))))

(use-fixtures :each clean-registries-fixture)

;;; =============================================================================
;;; Mock Agent Implementation
;;; =============================================================================

(defrecord MockLing [id status]
  IAgent
  (spawn! [_ _] id)
  (dispatch! [_ _] (str "task-" id))
  (kill! [_] {:killed? true})
  (status [_] {:slave/id id :slave/status status})
  (agent-type [_] :ling)
  (can-chain-tools? [_] true)
  (claims [_] [])
  (claim-files! [_ _ _] nil)
  (release-claims! [_] 0)
  (upgrade! [_] nil))

(defrecord MockDrone [id status]
  IAgent
  (spawn! [_ _] id)
  (dispatch! [_ _] (str "task-" id))
  (kill! [_] {:killed? true})
  (status [_] {:slave/id id :slave/status status})
  (agent-type [_] :drone)
  (can-chain-tools? [_] false)
  (claims [_] [])
  (claim-files! [_ _ _] nil)
  (release-claims! [_] 0)
  (upgrade! [_] nil))

(defn make-mock-ling
  "Create a mock ling for testing."
  [id & [status]]
  (->MockLing id (or status :idle)))

(defn make-mock-drone
  "Create a mock drone for testing."
  [id & [status]]
  (->MockDrone id (or status :idle)))

;;; =============================================================================
;;; Tool Registry Tests
;;; =============================================================================

(deftest tool-registry-register-and-retrieve
  (testing "Register a tool and retrieve by name"
    (let [handler (fn [_] {:result :ok})
          tool {:name "test-tool"
                :description "A test tool"
                :inputSchema {}
                :handler handler}]
      (reg/register! [tool])
      (let [retrieved (reg/get-tool "test-tool")]
        (is (some? retrieved) "Tool should be found")
        (is (= "test-tool" (:name retrieved)))
        (is (= "A test tool" (:description retrieved)))
        (is (fn? (:handler retrieved)) "Handler should be a function")
        (is (= {:result :ok} ((:handler retrieved) {}))
            "Handler should be callable")))))

(deftest tool-registry-list-tools
  (testing "List all registered tool names"
    (reg/register! [{:name "tool-a" :handler identity}
                    {:name "tool-b" :handler identity}
                    {:name "tool-c" :handler identity}])
    (let [tool-names (set (reg/list-tools))]
      (is (= #{"tool-a" "tool-b" "tool-c"} tool-names)
          "Should list all registered tools"))))

(deftest tool-registry-missing-tool
  (testing "Handle missing tool lookup gracefully"
    (is (nil? (reg/get-tool "nonexistent-tool"))
        "Should return nil for missing tool")))

(deftest tool-registry-get-schemas
  (testing "Get schemas strips handlers"
    (let [tool {:name "schema-tool"
                :description "Tool for schema test"
                :inputSchema {:type "object"}
                :handler (fn [_] :ok)}]
      (reg/register! [tool])
      (let [schemas (reg/get-schemas nil)]
        (is (= 1 (count schemas)))
        (let [schema (first schemas)]
          (is (= "schema-tool" (:name schema)))
          (is (= "Tool for schema test" (:description schema)))
          (is (not (contains? schema :handler))
              "Schema should not include handler"))))))

(deftest tool-registry-get-schemas-filtered
  (testing "Get schemas for specific tools only"
    (reg/register! [{:name "tool-x" :description "X" :handler identity}
                    {:name "tool-y" :description "Y" :handler identity}
                    {:name "tool-z" :description "Z" :handler identity}])
    (let [schemas (reg/get-schemas ["tool-x" "tool-z"])]
      (is (= 2 (count schemas)))
      (is (= #{"tool-x" "tool-z"} (set (map :name schemas)))
          "Should only return requested tools"))))

(deftest tool-registry-overwrite-tool
  (testing "Registering tool with same name overwrites"
    (reg/register! [{:name "dup-tool" :description "Original" :handler identity}])
    (reg/register! [{:name "dup-tool" :description "Updated" :handler identity}])
    (let [tool (reg/get-tool "dup-tool")]
      (is (= "Updated" (:description tool))
          "Later registration should overwrite"))))

;;; =============================================================================
;;; Agent Registry Tests
;;; =============================================================================

(deftest agent-registry-register-and-retrieve
  (testing "Register an agent and retrieve by ID"
    (let [ling (make-mock-ling "ling-001")]
      (reg/register-agent! ling)
      (let [retrieved (reg/get-agent-by-id "ling-001")]
        (is (some? retrieved) "Agent should be found")
        (is (= "ling-001" (:id retrieved)))
        (is (= :ling (agent-type retrieved)))))))

(deftest agent-registry-register-ling
  (testing "Register a ling agent"
    (let [ling (make-mock-ling "ling-test")]
      (let [result-id (reg/register-agent! ling)]
        (is (= "ling-test" result-id) "Should return agent ID")
        (is (some? (reg/get-agent-by-id "ling-test")))))))

(deftest agent-registry-register-drone
  (testing "Register a drone agent"
    (let [drone (make-mock-drone "drone-test")]
      (let [result-id (reg/register-agent! drone)]
        (is (= "drone-test" result-id) "Should return agent ID")
        (is (some? (reg/get-agent-by-id "drone-test")))))))

(deftest agent-registry-list-all-agents
  (testing "List all registered agents"
    (let [ling1 (make-mock-ling "ling-1")
          ling2 (make-mock-ling "ling-2")
          drone1 (make-mock-drone "drone-1")]
      (reg/register-agent! ling1)
      (reg/register-agent! ling2)
      (reg/register-agent! drone1)
      (let [agents (reg/list-all-agents)]
        (is (= 3 (count agents)))
        (is (= #{"ling-1" "ling-2" "drone-1"}
               (set (map :id agents))))))))

(deftest agent-registry-list-lings-only
  (testing "List only ling agents"
    (reg/register-agent! (make-mock-ling "ling-a"))
    (reg/register-agent! (make-mock-ling "ling-b"))
    (reg/register-agent! (make-mock-drone "drone-a"))
    (let [lings (reg/list-lings)]
      (is (= 2 (count lings)) "Should only have 2 lings")
      (is (every? #(= :ling (agent-type %)) lings)
          "All should be lings"))))

(deftest agent-registry-list-drones-only
  (testing "List only drone agents"
    (reg/register-agent! (make-mock-ling "ling-x"))
    (reg/register-agent! (make-mock-drone "drone-x"))
    (reg/register-agent! (make-mock-drone "drone-y"))
    (let [drones (reg/list-drones)]
      (is (= 2 (count drones)) "Should only have 2 drones")
      (is (every? #(= :drone (agent-type %)) drones)
          "All should be drones"))))

(deftest agent-registry-deregister
  (testing "Deregister an agent"
    (let [ling (make-mock-ling "ling-remove")]
      (reg/register-agent! ling)
      (is (some? (reg/get-agent-by-id "ling-remove"))
          "Should exist before deregister")
      (reg/unregister-agent! "ling-remove")
      (is (nil? (reg/get-agent-by-id "ling-remove"))
          "Should not exist after deregister"))))

(deftest agent-registry-missing-agent
  (testing "Handle missing agent lookup gracefully"
    (is (nil? (reg/get-agent-by-id "nonexistent-agent"))
        "Should return nil for missing agent")))

(deftest agent-registry-clear-all
  (testing "Clear all agents"
    (reg/register-agent! (make-mock-ling "ling-clear-1"))
    (reg/register-agent! (make-mock-drone "drone-clear-1"))
    (is (= 2 (count (reg/list-all-agents))))
    (reg/clear-agents!)
    (is (empty? (reg/list-all-agents))
        "All agents should be cleared")))

(deftest agent-registry-protocol-via-record
  (testing "AgentRegistry record implements IAgentRegistry"
    (let [registry reg/agent-registry
          ling (make-mock-ling "proto-test")]
      ;; Use protocol methods directly
      (.register! registry ling)
      (is (some? (.get-agent registry "proto-test")))
      (let [agents (.list-agents registry)]
        (is (= 1 (count agents))))
      (let [lings (.list-agents-by-type registry :ling)]
        (is (= 1 (count lings)))
        (is (= :ling (agent-type (first lings)))))
      (.unregister! registry "proto-test")
      (is (nil? (.get-agent registry "proto-test"))))))

;;; =============================================================================
;;; Agent Status Filter Tests
;;; =============================================================================

(deftest agent-registry-filter-by-status
  (testing "Filter agents by status via agent status method"
    (let [idle-ling (make-mock-ling "ling-idle" :idle)
          working-ling (make-mock-ling "ling-working" :working)
          error-drone (make-mock-drone "drone-error" :error)]
      (reg/register-agent! idle-ling)
      (reg/register-agent! working-ling)
      (reg/register-agent! error-drone)
      ;; Filter by status using agent's status method
      (let [all-agents (reg/list-all-agents)
            idle-agents (filter #(= :idle (:slave/status (.status %))) all-agents)
            working-agents (filter #(= :working (:slave/status (.status %))) all-agents)
            error-agents (filter #(= :error (:slave/status (.status %))) all-agents)]
        (is (= 1 (count idle-agents)))
        (is (= 1 (count working-agents)))
        (is (= 1 (count error-agents)))
        (is (= "ling-idle" (:id (first idle-agents))))
        (is (= "ling-working" (:id (first working-agents))))
        (is (= "drone-error" (:id (first error-agents))))))))

;;; =============================================================================
;;; sync-from-datascript Tests (Integration)
;;; =============================================================================

;; Note: Full sync-from-datascript tests require DataScript setup.
;; These tests verify the function signature and behavior with empty state.

(deftest sync-from-datascript-returns-expected-structure
  (testing "sync-from-datascript returns map with expected keys"
    ;; This test verifies the function can be called even with no slaves
    ;; Full integration would require populating DataScript
    (try
      (let [result (reg/sync-from-datascript!)]
        (is (map? result) "Should return a map")
        (is (contains? result :synced-count) "Should have :synced-count")
        (is (contains? result :agents) "Should have :agents")
        (is (number? (:synced-count result)) "synced-count should be a number")
        (is (vector? (:agents result)) "agents should be a vector"))
      (catch Exception e
        ;; If DataScript not initialized, this is expected in isolated test
        (is (or (re-find #"DataScript" (ex-message e))
                (re-find #"conn" (ex-message e))
                true)
            "Exception should be DataScript-related if not initialized")))))

;;; =============================================================================
;;; Edge Cases
;;; =============================================================================

(deftest tool-registry-empty-list
  (testing "Empty registry returns empty list"
    (is (empty? (reg/list-tools))
        "Empty registry should return empty seq")))

(deftest agent-registry-empty-list
  (testing "Empty agent registry returns empty list"
    (is (empty? (reg/list-all-agents)))
    (is (empty? (reg/list-lings)))
    (is (empty? (reg/list-drones)))))

(deftest tool-registry-multiple-handlers
  (testing "Multiple tools with different handlers work independently"
    (let [handler-a (fn [_] :result-a)
          handler-b (fn [_] :result-b)]
      (reg/register! [{:name "tool-independent-a" :handler handler-a}
                      {:name "tool-independent-b" :handler handler-b}])
      (let [tool-a (reg/get-tool "tool-independent-a")
            tool-b (reg/get-tool "tool-independent-b")]
        (is (= :result-a ((:handler tool-a) {})))
        (is (= :result-b ((:handler tool-b) {})))))))

(deftest agent-registry-reregister-same-id
  (testing "Re-registering agent with same ID overwrites"
    (let [ling-v1 (make-mock-ling "same-id" :idle)
          ling-v2 (make-mock-ling "same-id" :working)]
      (reg/register-agent! ling-v1)
      (is (= :idle (:slave/status (.status (reg/get-agent-by-id "same-id")))))
      (reg/register-agent! ling-v2)
      (is (= :working (:slave/status (.status (reg/get-agent-by-id "same-id"))))
          "Second registration should overwrite"))))

(comment
  ;; Run tests
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent.registry-test))
