(ns hive-mcp.tools.catchup-kg-test
  "Tests for Knowledge Graph integration in catchup workflow."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.queries :as kg-queries]
            [hive-mcp.tools.catchup :as catchup]))

;; Reset KG before each test
(use-fixtures :each
  (fn [f]
    (kg-conn/reset-conn!)
    (f)))

;; ============================================================================
;; Unit tests for gather-kg-insights
;; ============================================================================

(deftest gather-kg-insights-empty-kg-test
  (testing "returns edge-count=0 when KG is empty"
    (let [gather-fn #'hive-mcp.tools.catchup/gather-kg-insights
          result (gather-fn [] [] [] "test-project")]
      (is (map? result))
      (is (= 0 (:edge-count result)))
      ;; Should not have by-relation when empty
      (is (empty? (:by-relation result))))))

(deftest gather-kg-insights-with-edges-test
  (testing "returns KG stats when edges exist"
    ;; Create some edges
    (kg-edges/add-edge! {:from "decision-1" :to "convention-1"
                         :relation :implements})
    (kg-edges/add-edge! {:from "session-1" :to "decision-1"
                         :relation :derived-from})

    (let [gather-fn #'hive-mcp.tools.catchup/gather-kg-insights
          ;; Mock entry metadata
          decisions-meta [{:id "decision-1" :preview "Test decision"}]
          sessions-meta [{:id "session-1" :preview "Test session"}]
          result (gather-fn decisions-meta [] sessions-meta nil)]
      (is (= 2 (:edge-count result)))
      (is (= {:implements 1 :derived-from 1} (:by-relation result))))))

(deftest gather-kg-insights-related-decisions-test
  (testing "finds related decisions via traversal"
    ;; Create decision chain: convention-1 -> implements -> decision-1
    (kg-edges/add-edge! {:from "convention-1" :to "decision-1"
                         :relation :implements})

    (let [gather-fn #'hive-mcp.tools.catchup/gather-kg-insights
          decisions-meta [{:id "decision-1" :preview "Test decision"}]
          result (gather-fn decisions-meta [] [] nil)]
      ;; Should find convention-1 as related to decision-1
      (is (= 1 (:edge-count result)))
      ;; related-decisions should include the convention that implements
      (is (some #{"convention-1"} (:related-decisions result))))))

(deftest gather-kg-insights-session-derived-test
  (testing "finds entries derived from session summaries"
    ;; Create: decision-1 -> derived-from -> session-1
    ;; (session summary was derived from decision made during session)
    ;; This is an INCOMING edge to session-1
    (kg-edges/add-edge! {:from "decision-1" :to "session-1"
                         :relation :derived-from})

    (let [gather-fn #'hive-mcp.tools.catchup/gather-kg-insights
          sessions-meta [{:id "session-1" :preview "Test session"}]
          result (gather-fn [] [] sessions-meta nil)]
      ;; Should find decision-1 via incoming traversal
      (is (= 1 (:edge-count result)))
      ;; session-derived should include decision-1
      (is (some #{"decision-1"} (:session-derived result))))))

;; ============================================================================
;; Unit tests for find-related-via-session-summaries
;; ============================================================================

(deftest find-related-via-session-summaries-test
  (testing "finds entries that sessions derived from"
    ;; Create: some entry -> derived-from -> session (incoming to session)
    (kg-edges/add-edge! {:from "note-1" :to "session-1"
                         :relation :derived-from})

    (let [find-fn #'hive-mcp.tools.catchup/find-related-via-session-summaries
          result (find-fn ["session-1"] nil)]
      ;; note-1 has outgoing derived-from to session-1
      ;; so when traversing incoming from session-1, we find note-1
      (is (some #{"note-1"} result))))

  (testing "returns empty for no session IDs"
    (let [find-fn #'hive-mcp.tools.catchup/find-related-via-session-summaries
          result (find-fn [] nil)]
      (is (nil? result)))))

;; ============================================================================
;; Unit tests for find-related-decisions-via-kg
;; ============================================================================

(deftest find-related-decisions-via-kg-test
  (testing "finds related entries via implements/refines/depends-on"
    ;; Create: snippet-1 -> implements -> decision-1
    (kg-edges/add-edge! {:from "snippet-1" :to "decision-1"
                         :relation :implements})
    ;; Create: decision-1 -> depends-on -> axiom-1
    (kg-edges/add-edge! {:from "decision-1" :to "axiom-1"
                         :relation :depends-on})

    (let [find-fn #'hive-mcp.tools.catchup/find-related-decisions-via-kg
          result (find-fn ["decision-1"] nil)]
      ;; Should find snippet-1 (incoming implements) and axiom-1 (outgoing depends-on)
      (is (some #{"snippet-1"} result))
      (is (some #{"axiom-1"} result))
      ;; Should not include the original decision-1
      (is (not (some #{"decision-1"} result)))))

  (testing "returns empty for no decision IDs"
    (let [find-fn #'hive-mcp.tools.catchup/find-related-decisions-via-kg
          result (find-fn [] nil)]
      (is (nil? result)))))

;; ============================================================================
;; Unit tests for extract-kg-relations
;; ============================================================================

(deftest extract-kg-relations-test
  (testing "extracts relations from node context"
    ;; Create edges
    (kg-edges/add-edge! {:from "decision-1" :to "old-decision"
                         :relation :supersedes})
    (kg-edges/add-edge! {:from "decision-1" :to "axiom-1"
                         :relation :depends-on})

    ;; Get node context and extract
    (let [context (hive-mcp.knowledge-graph.queries/get-node-context "decision-1")
          extract-fn #'hive-mcp.tools.catchup/extract-kg-relations
          result (extract-fn context)]
      (is (= ["old-decision"] (:supersedes result)))
      (is (= ["axiom-1"] (:depends-on result)))))

  (testing "handles empty context"
    (let [extract-fn #'hive-mcp.tools.catchup/extract-kg-relations
          result (extract-fn {:incoming {:count 0 :edges []}
                              :outgoing {:count 0 :edges []}})]
      (is (empty? result)))))

;; ============================================================================
;; Integration test: KG insights in catchup response
;; ============================================================================

(deftest kg-insights-structure-test
  (testing "kg-insights has expected structure"
    ;; Create some edges to populate stats
    (kg-edges/add-edge! {:from "d1" :to "c1" :relation :implements})
    (kg-edges/add-edge! {:from "d2" :to "d1" :relation :supersedes})
    (kg-edges/add-edge! {:from "s1" :to "d1" :relation :derived-from})

    (let [gather-fn #'hive-mcp.tools.catchup/gather-kg-insights
          decisions-meta [{:id "d1" :preview "Decision 1"}
                          {:id "d2" :preview "Decision 2"}]
          sessions-meta [{:id "s1" :preview "Session 1"}]
          result (gather-fn decisions-meta [] sessions-meta "test-project")]
      ;; Always has edge-count
      (is (contains? result :edge-count))
      (is (= 3 (:edge-count result)))
      ;; Has by-relation when edges exist
      (is (contains? result :by-relation))
      (is (= {:implements 1 :supersedes 1 :derived-from 1}
             (:by-relation result))))))
