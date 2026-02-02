(ns hive-mcp.schema.tools-test
  "Tests for MCP tool parameter schemas.

   TDD via nREPL - evaluate in CIDER to run tests."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [hive-mcp.schema.tools :as tools]))

;; =============================================================================
;; Agent Tool Schema Tests
;; =============================================================================

(deftest agent-spawn-params-test
  (testing "valid spawn params"
    (is (m/validate tools/AgentSpawnParams
                    {:name "worker-1"}))
    (is (m/validate tools/AgentSpawnParams
                    {:name "worker-1"
                     :cwd "/home/user/project"
                     :presets ["research" "clojure"]
                     :terminal "vterm"}))
    (is (m/validate tools/AgentSpawnParams
                    {:name "drone-1"
                     :kanban_task_id "task-123"})))

  (testing "missing name rejected"
    (is (not (m/validate tools/AgentSpawnParams {})))
    (is (not (m/validate tools/AgentSpawnParams
                         {:cwd "/tmp"}))))

  (testing "empty name rejected"
    (is (not (m/validate tools/AgentSpawnParams
                         {:name ""})))))

(deftest agent-dispatch-params-test
  (testing "valid dispatch params"
    (is (m/validate tools/AgentDispatchParams
                    {:task "Implement feature X"}))
    (is (m/validate tools/AgentDispatchParams
                    {:task "Review PR"
                     :ling_id "ling-123"
                     :priority "high"
                     :context "PR #456 needs review"})))

  (testing "missing task rejected"
    (is (not (m/validate tools/AgentDispatchParams {}))))

  (testing "invalid priority rejected"
    (is (not (m/validate tools/AgentDispatchParams
                         {:task "test"
                          :priority "urgent"})))))

(deftest agent-kill-params-test
  (testing "valid kill params"
    (is (m/validate tools/AgentKillParams
                    {:slave_id "ling-123"}))
    (is (m/validate tools/AgentKillParams
                    {:slave_id "all"
                     :force true
                     :directory "/home/user/project"})))

  (testing "missing slave_id rejected"
    (is (not (m/validate tools/AgentKillParams {})))))

(deftest agent-status-params-test
  (testing "valid status params"
    (is (m/validate tools/AgentStatusParams {}))
    (is (m/validate tools/AgentStatusParams
                    {:directory "/tmp"
                     :include_details true}))))

;; =============================================================================
;; Memory Tool Schema Tests
;; =============================================================================

(deftest memory-add-params-test
  (testing "valid add params"
    (is (m/validate tools/MemoryAddParams
                    {:type "decision"
                     :content "Use Malli for validation"}))
    (is (m/validate tools/MemoryAddParams
                    {:type "snippet"
                     :content "(defn hello [] \"world\")"
                     :tags ["clojure" "example"]
                     :duration "long"
                     :directory "/home/user/project"
                     :agent_id "ling-1"
                     :abstraction_level 2}))
    (is (m/validate tools/MemoryAddParams
                    {:type "decision"
                     :content "Test KG relationships"
                     :kg_implements ["entry-1"]
                     :kg_supersedes ["entry-2"]
                     :kg_depends_on ["entry-3"]
                     :kg_refines ["entry-4"]})))

  (testing "missing required fields rejected"
    (is (not (m/validate tools/MemoryAddParams
                         {:type "decision"})))  ; missing content
    (is (not (m/validate tools/MemoryAddParams
                         {:content "missing type"})))))

(deftest memory-query-params-test
  (testing "valid query params"
    (is (m/validate tools/MemoryQueryParams {}))
    (is (m/validate tools/MemoryQueryParams
                    {:type "decision"}))
    (is (m/validate tools/MemoryQueryParams
                    {:type "snippet"
                     :tags ["clojure"]
                     :limit 10
                     :duration "long"
                     :scope "all"
                     :directory "/tmp"}))))

(deftest memory-get-full-params-test
  (testing "valid get-full params"
    (is (m/validate tools/MemoryGetFullParams
                    {:id "entry-123"})))

  (testing "missing id rejected"
    (is (not (m/validate tools/MemoryGetFullParams {}))))

  (testing "empty id rejected"
    (is (not (m/validate tools/MemoryGetFullParams
                         {:id ""})))))

;; =============================================================================
;; KG Tool Schema Tests
;; =============================================================================

(deftest kg-add-edge-params-test
  (testing "valid add-edge params"
    (is (m/validate tools/KGAddEdgeParams
                    {:from "entry-1"
                     :to "entry-2"
                     :relation "implements"}))
    (is (m/validate tools/KGAddEdgeParams
                    {:from "entry-1"
                     :to "entry-2"
                     :relation "depends-on"
                     :scope "hive-mcp"
                     :confidence 0.95
                     :created_by "agent:ling-1"
                     :source_type "manual"})))

  (testing "missing required fields rejected"
    (is (not (m/validate tools/KGAddEdgeParams
                         {:from "entry-1"
                          :to "entry-2"})))  ; missing relation
    (is (not (m/validate tools/KGAddEdgeParams
                         {:from "entry-1"
                          :relation "implements"})))))  ; missing to

(deftest kg-relation-type-test
  (testing "valid relation types"
    (is (m/validate tools/KGRelationType "implements"))
    (is (m/validate tools/KGRelationType "supersedes"))
    (is (m/validate tools/KGRelationType "refines"))
    (is (m/validate tools/KGRelationType "contradicts"))
    (is (m/validate tools/KGRelationType "depends-on"))
    (is (m/validate tools/KGRelationType "derived-from"))
    (is (m/validate tools/KGRelationType "applies-to"))
    (is (m/validate tools/KGRelationType "co-accessed"))
    (is (m/validate tools/KGRelationType "projects-to")))

  (testing "invalid relation types rejected"
    (is (not (m/validate tools/KGRelationType "relates-to")))
    (is (not (m/validate tools/KGRelationType "")))))

(deftest kg-traverse-params-test
  (testing "valid traverse params"
    (is (m/validate tools/KGTraverseParams
                    {:start_node "entry-1"}))
    (is (m/validate tools/KGTraverseParams
                    {:start_node "entry-1"
                     :direction "outgoing"
                     :relations ["implements" "depends-on"]
                     :max_depth 5
                     :scope "hive-mcp"})))

  (testing "missing start_node rejected"
    (is (not (m/validate tools/KGTraverseParams {})))))

(deftest kg-impact-analysis-params-test
  (testing "valid impact-analysis params"
    (is (m/validate tools/KGImpactAnalysisParams
                    {:node_id "entry-1"}))
    (is (m/validate tools/KGImpactAnalysisParams
                    {:node_id "entry-1"
                     :max_depth 10
                     :scope "hive-mcp"})))

  (testing "missing node_id rejected"
    (is (not (m/validate tools/KGImpactAnalysisParams {})))))

(deftest kg-promote-params-test
  (testing "valid promote params"
    (is (m/validate tools/KGPromoteParams
                    {:edge_id "edge-123"
                     :to_scope "global"})))

  (testing "missing required fields rejected"
    (is (not (m/validate tools/KGPromoteParams
                         {:edge_id "edge-123"})))  ; missing to_scope
    (is (not (m/validate tools/KGPromoteParams
                         {:to_scope "global"})))))  ; missing edge_id

;; =============================================================================
;; Validator Function Tests
;; =============================================================================

(deftest validate-params-test
  (testing "valid params return {:valid true}"
    (let [result (tools/validate-params
                  tools/AgentSpawnParams
                  {:name "worker-1"})]
      (is (:valid result))
      (is (nil? (:errors result)))))

  (testing "invalid params return {:valid false :errors ...}"
    (let [result (tools/validate-params
                  tools/AgentSpawnParams
                  {})]
      (is (not (:valid result)))
      (is (some? (:errors result))))))

;; =============================================================================
;; Response Schema Tests
;; =============================================================================

(deftest mcp-response-test
  (testing "success response"
    (is (m/validate tools/MCPSuccessResponse
                    {:type "text"
                     :text "{\"success\": true}"}))
    (is (m/validate tools/MCPSuccessResponse
                    {:type "text"
                     :text "result"
                     :isError false})))

  (testing "error response"
    (is (m/validate tools/MCPErrorResponse
                    {:type "text"
                     :text "Something went wrong"
                     :isError true})))

  (testing "MCPResponse accepts both"
    (is (m/validate tools/MCPResponse
                    {:type "text" :text "ok"}))
    (is (m/validate tools/MCPResponse
                    {:type "text" :text "error" :isError true}))))

(comment
  ;; Run all tests in REPL
  (clojure.test/run-tests 'hive-mcp.schema.tools-test)

  ;; Run specific test
  (agent-spawn-params-test)
  (memory-add-params-test)
  (kg-add-edge-params-test))
