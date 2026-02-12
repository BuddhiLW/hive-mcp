(ns hive-mcp.knowledge-graph.edges-migration-test
  "Unit tests for KG edge scope migration (migrate-edge-scopes!).

   Tests cover:
   - Basic scope migration (edges move from old to new scope)
   - Batch correctness (multiple edges updated in single transact!)
   - Idempotency (calling again returns {:migrated 0})
   - Validation (nil/same scope throws)
   - Mixed scopes (only target scope edges are migrated)
   - Edges without scope are unaffected

   Each test uses a fresh DataScript connection via fixture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(use-fixtures :each fixtures/datascript-fixture)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn gen-node-id
  "Generate a unique node ID for testing."
  []
  (str "test-node-" (subs (str (java.util.UUID/randomUUID)) 0 8)))

(defn create-scoped-edge!
  "Create an edge with a specific scope. Returns edge-id."
  [scope & [{:keys [relation] :or {relation :implements}}]]
  (edges/add-edge! {:from (gen-node-id)
                    :to (gen-node-id)
                    :relation relation
                    :scope scope}))

;; =============================================================================
;; Basic Migration Tests
;; =============================================================================

(deftest migrate-edge-scopes-basic-test
  (testing "migrate-edge-scopes! moves edges from old scope to new scope"
    (let [edge-id (create-scoped-edge! "old-project")
          result (edges/migrate-edge-scopes! "old-project" "new-project")
          edge (edges/get-edge edge-id)]
      (is (= 1 (:migrated result)))
      (is (= "old-project" (:old-scope result)))
      (is (= "new-project" (:new-scope result)))
      (is (= "new-project" (:kg-edge/scope edge))))))

(deftest migrate-edge-scopes-batch-test
  (testing "migrate-edge-scopes! batch-updates multiple edges"
    (let [edge-ids (mapv (fn [_] (create-scoped-edge! "batch-old")) (range 5))
          result (edges/migrate-edge-scopes! "batch-old" "batch-new")]
      (is (= 5 (:migrated result)))
      ;; Verify all edges have new scope
      (doseq [eid edge-ids]
        (let [edge (edges/get-edge eid)]
          (is (= "batch-new" (:kg-edge/scope edge))
              (str "Edge " eid " should have new scope")))))))

(deftest migrate-edge-scopes-no-old-scope-edges-test
  (testing "migrate-edge-scopes! returns {:migrated 0} when no edges match old scope"
    (let [result (edges/migrate-edge-scopes! "nonexistent-scope" "new-scope")]
      (is (= 0 (:migrated result))))))

;; =============================================================================
;; Idempotency Tests
;; =============================================================================

(deftest migrate-edge-scopes-idempotent-test
  (testing "Calling migrate-edge-scopes! twice is idempotent"
    (create-scoped-edge! "old-scope")
    (create-scoped-edge! "old-scope")
    (let [result1 (edges/migrate-edge-scopes! "old-scope" "new-scope")
          result2 (edges/migrate-edge-scopes! "old-scope" "new-scope")]
      (is (= 2 (:migrated result1)) "First call should migrate 2 edges")
      (is (= 0 (:migrated result2)) "Second call should find no edges to migrate"))))

;; =============================================================================
;; Validation Tests
;; =============================================================================

(deftest migrate-edge-scopes-nil-old-scope-throws-test
  (testing "migrate-edge-scopes! throws when old-scope is nil"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"requires old-scope and new-scope"
                          (edges/migrate-edge-scopes! nil "new-scope")))))

(deftest migrate-edge-scopes-nil-new-scope-throws-test
  (testing "migrate-edge-scopes! throws when new-scope is nil"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"requires old-scope and new-scope"
                          (edges/migrate-edge-scopes! "old-scope" nil)))))

(deftest migrate-edge-scopes-same-scope-throws-test
  (testing "migrate-edge-scopes! throws when old-scope equals new-scope"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"must be different"
                          (edges/migrate-edge-scopes! "same" "same")))))

;; =============================================================================
;; Mixed Scope / Isolation Tests
;; =============================================================================

(deftest migrate-edge-scopes-preserves-other-scopes-test
  (testing "migrate-edge-scopes! only affects edges with matching scope"
    (let [target-id (create-scoped-edge! "migrate-me")
          keep-id (create-scoped-edge! "keep-me")
          _result (edges/migrate-edge-scopes! "migrate-me" "migrated")]
      ;; Target edge should have new scope
      (is (= "migrated" (:kg-edge/scope (edges/get-edge target-id))))
      ;; Other scope edge should be unchanged
      (is (= "keep-me" (:kg-edge/scope (edges/get-edge keep-id)))))))

(deftest migrate-edge-scopes-preserves-scopeless-edges-test
  (testing "migrate-edge-scopes! doesn't affect edges without scope"
    (let [;; Create edge without scope
          scopeless-id (edges/add-edge! {:from (gen-node-id)
                                         :to (gen-node-id)
                                         :relation :implements})
          ;; Create edge with target scope
          scoped-id (create-scoped-edge! "old-scope")
          _result (edges/migrate-edge-scopes! "old-scope" "new-scope")
          scopeless-edge (edges/get-edge scopeless-id)]
      ;; Scoped edge should be migrated
      (is (= "new-scope" (:kg-edge/scope (edges/get-edge scoped-id))))
      ;; Scopeless edge should have no scope attribute
      (is (nil? (:kg-edge/scope scopeless-edge))))))

;; =============================================================================
;; Edge Attribute Preservation Tests
;; =============================================================================

(deftest migrate-edge-scopes-preserves-attributes-test
  (testing "migrate-edge-scopes! preserves all other edge attributes"
    (let [from (gen-node-id)
          to (gen-node-id)
          edge-id (edges/add-edge! {:from from
                                    :to to
                                    :relation :refines
                                    :scope "old-project"
                                    :confidence 0.8
                                    :created-by "test-agent"
                                    :source-type :manual})
          _ (edges/migrate-edge-scopes! "old-project" "new-project")
          edge (edges/get-edge edge-id)]
      ;; Scope should be updated
      (is (= "new-project" (:kg-edge/scope edge)))
      ;; All other attributes should be preserved
      (is (= from (:kg-edge/from edge)))
      (is (= to (:kg-edge/to edge)))
      (is (= :refines (:kg-edge/relation edge)))
      (is (= 0.8 (:kg-edge/confidence edge)))
      (is (= "test-agent" (:kg-edge/created-by edge)))
      (is (= :manual (:kg-edge/source-type edge))))))

;; =============================================================================
;; Stats Verification
;; =============================================================================

(deftest migrate-edge-scopes-stats-reflect-migration-test
  (testing "edge-stats reflect scope changes after migration"
    (create-scoped-edge! "project-a")
    (create-scoped-edge! "project-a")
    (create-scoped-edge! "project-b")
    (let [stats-before (edges/edge-stats)
          _ (edges/migrate-edge-scopes! "project-a" "project-c")
          stats-after (edges/edge-stats)]
      ;; Before: 2 in project-a, 1 in project-b
      (is (= 2 (get-in stats-before [:by-scope "project-a"])))
      (is (= 1 (get-in stats-before [:by-scope "project-b"])))
      ;; After: 0 in project-a, 2 in project-c, 1 in project-b
      (is (nil? (get-in stats-after [:by-scope "project-a"])))
      (is (= 2 (get-in stats-after [:by-scope "project-c"])))
      (is (= 1 (get-in stats-after [:by-scope "project-b"]))))))
