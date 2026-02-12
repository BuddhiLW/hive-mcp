(ns hive-mcp.tools.memory.crud-hcr-test
  "HCR Wave 4 integration tests for memory query with include_descendants.

   Tests cover:
   - apply-auto-scope-filter with include-descendants? flag
   - Scope tag behavior for full hierarchy
   - Backward compatibility (default false)
   - resolve-project-scope for agent spawn integration"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.tools.memory.crud]
            [hive-mcp.tools.consolidated.agent]
            [hive-mcp.tools.agent.spawn]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-state-fixture
  "Clear caches and registries before each test."
  [f]
  (kg-scope/clear-config-cache!)
  (f))

(use-fixtures :each reset-state-fixture)

;; =============================================================================
;; Test Data
;; =============================================================================

(def ^:private parent-entry
  {:id "entry-parent-1"
   :type "decision"
   :content "Parent project decision"
   :tags ["scope:project:hive-mcp"]
   :project-id "hive-mcp"})

(def ^:private child-entry
  {:id "entry-child-1"
   :type "decision"
   :content "Child project decision"
   :tags ["scope:project:hive-mcp:agora"]
   :project-id "hive-mcp:agora"})

(def ^:private grandchild-entry
  {:id "entry-grandchild-1"
   :type "decision"
   :content "Grandchild project decision"
   :tags ["scope:project:hive-mcp:agora:feature"]
   :project-id "hive-mcp:agora:feature"})

(def ^:private sibling-entry
  {:id "entry-sibling-1"
   :type "decision"
   :content "Sibling project decision"
   :tags ["scope:project:hive-mcp:tools"]
   :project-id "hive-mcp:tools"})

(def ^:private global-entry
  {:id "entry-global-1"
   :type "axiom"
   :content "Global axiom"
   :tags ["scope:global"]
   :project-id "global"})

(def ^:private unrelated-entry
  {:id "entry-unrelated-1"
   :type "decision"
   :content "Unrelated project decision"
   :tags ["scope:project:other-project"]
   :project-id "other-project"})

(def ^:private all-entries
  [parent-entry child-entry grandchild-entry
   sibling-entry global-entry unrelated-entry])

;; =============================================================================
;; Private fn accessor
;; =============================================================================

(def ^:private apply-auto-scope-filter
  @(resolve 'hive-mcp.tools.memory.crud/apply-auto-scope-filter))

(def ^:private resolve-project-scope
  @(resolve 'hive-mcp.tools.agent.spawn/resolve-project-scope))

;; =============================================================================
;; apply-auto-scope-filter Tests
;; =============================================================================

(deftest test-auto-scope-default-upward-only
  (testing "Default (no descendants): child query sees parent, NOT siblings/global/unrelated"
    (let [result (apply-auto-scope-filter all-entries "hive-mcp:agora")]
      ;; Should include self (child)
      (is (some #(= "entry-child-1" (:id %)) result)
          "Child should see its own entry")
      ;; Should include parent
      (is (some #(= "entry-parent-1" (:id %)) result)
          "Child should see parent entry")
      ;; Scope leak fix: global entries NOT included when in project context
      (is (not (some #(= "entry-global-1" (:id %)) result))
          "Child should NOT see global entry (scope leak fix)")
      ;; Should NOT include siblings
      (is (not (some #(= "entry-sibling-1" (:id %)) result))
          "Child should NOT see sibling entry")
      ;; Should NOT include unrelated
      (is (not (some #(= "entry-unrelated-1" (:id %)) result))
          "Child should NOT see unrelated entry"))))

(deftest test-auto-scope-with-descendants-parent-sees-more
  (testing "With descendants=true: parent query includes child project memories"
    (let [result-no-desc (apply-auto-scope-filter all-entries "hive-mcp" false)
          result-with-desc (apply-auto-scope-filter all-entries "hive-mcp" true)]
      ;; Both should see parent (self)
      (is (some #(= "entry-parent-1" (:id %)) result-no-desc))
      (is (some #(= "entry-parent-1" (:id %)) result-with-desc))
      ;; Scope leak fix: global NOT included in project context
      (is (not (some #(= "entry-global-1" (:id %)) result-no-desc))
          "Parent in project context should NOT see global (scope leak fix)")
      (is (not (some #(= "entry-global-1" (:id %)) result-with-desc))
          "Parent with descendants in project context should NOT see global (scope leak fix)")
      ;; Without descendants, parent should NOT see child entries
      (is (not (some #(= "entry-child-1" (:id %)) result-no-desc))
          "Parent without descendants should NOT see child entry")
      ;; Unrelated should never be visible
      (is (not (some #(= "entry-unrelated-1" (:id %)) result-no-desc)))
      (is (not (some #(= "entry-unrelated-1" (:id %)) result-with-desc))))))

(deftest test-auto-scope-global-excluded-in-project-context
  (testing "Scope leak fix: global entries are excluded when querying from project context"
    (let [result-false (apply-auto-scope-filter all-entries "hive-mcp:agora" false)
          result-true (apply-auto-scope-filter all-entries "hive-mcp:agora" true)]
      (is (not (some #(= "entry-global-1" (:id %)) result-false))
          "Global NOT visible from project context (no descendants)")
      (is (not (some #(= "entry-global-1" (:id %)) result-true))
          "Global NOT visible from project context (with descendants)"))))

(deftest test-auto-scope-global-context-sees-global
  (testing "Global context (project-id='global') DOES see global entries"
    (let [result (apply-auto-scope-filter all-entries "global")]
      (is (some #(= "entry-global-1" (:id %)) result)
          "Global context should see global entries"))))

(deftest test-auto-scope-backward-compat-2-arity
  (testing "2-arity call (backward compat) works same as 3-arity with false"
    (let [result-2arity (apply-auto-scope-filter all-entries "hive-mcp")
          result-false (apply-auto-scope-filter all-entries "hive-mcp" false)]
      (is (= (set (map :id result-2arity))
             (set (map :id result-false)))))))

(deftest test-auto-scope-child-sees-self
  (testing "Child query includes its own entries"
    (let [result (apply-auto-scope-filter all-entries "hive-mcp:agora")]
      (is (some #(= "entry-child-1" (:id %)) result)
          "Child should see its own entry"))))

(deftest test-auto-scope-grandchild-traversal
  (testing "Grandchild sees parent + grandparent, NOT global/siblings/unrelated"
    (let [result (apply-auto-scope-filter all-entries "hive-mcp:agora:feature")]
      ;; Self
      (is (some #(= "entry-grandchild-1" (:id %)) result)
          "Grandchild should see its own entry")
      ;; Parent (agora)
      (is (some #(= "entry-child-1" (:id %)) result)
          "Grandchild should see parent (agora) entry")
      ;; Grandparent (hive-mcp)
      (is (some #(= "entry-parent-1" (:id %)) result)
          "Grandchild should see grandparent (hive-mcp) entry")
      ;; Scope leak fix: global NOT visible from project context
      (is (not (some #(= "entry-global-1" (:id %)) result))
          "Grandchild should NOT see global entry (scope leak fix)")
      ;; NOT siblings or unrelated
      (is (not (some #(= "entry-sibling-1" (:id %)) result)))
      (is (not (some #(= "entry-unrelated-1" (:id %)) result))))))

;; =============================================================================
;; full-hierarchy-scope-tags Integration Tests
;; =============================================================================

(deftest test-full-hierarchy-scope-tags-covers-ancestors
  (testing "full-hierarchy-scope-tags includes ancestor scope tags"
    (let [tags (kg-scope/full-hierarchy-scope-tags "hive-mcp:agora")]
      (is (contains? tags "scope:project:hive-mcp:agora"))
      (is (contains? tags "scope:project:hive-mcp"))
      (is (contains? tags "scope:global")))))

(deftest test-full-hierarchy-scope-tags-nil-and-global
  (testing "nil and global return just scope:global"
    (is (= #{"scope:global"} (kg-scope/full-hierarchy-scope-tags nil)))
    (is (= #{"scope:global"} (kg-scope/full-hierarchy-scope-tags "global")))))

(deftest test-full-hierarchy-scope-tags-single-level
  (testing "Single-level project includes self + global"
    (let [tags (kg-scope/full-hierarchy-scope-tags "hive-mcp")]
      (is (contains? tags "scope:project:hive-mcp"))
      (is (contains? tags "scope:global")))))

;; =============================================================================
;; resolve-project-scope Tests (agent spawn HCR Wave 4)
;; =============================================================================

(deftest test-resolve-project-scope-explicit-takes-precedence
  (testing "Explicit project_id takes highest precedence"
    (is (= "explicit-id" (resolve-project-scope "explicit-id" "/some/path" nil)))))

(deftest test-resolve-project-scope-nil-all-falls-back
  (testing "When all nil, returns nil"
    (is (nil? (resolve-project-scope nil nil nil)))))

(deftest test-resolve-project-scope-cwd-fallback
  (testing "When no explicit project_id, falls back via cwd"
    ;; Without a .hive-project.edn at the path, infer-scope-from-path
    ;; returns "global", so it falls through to last-path-segment
    (is (some? (resolve-project-scope nil "/home/user/my-project" nil)))))

(deftest test-resolve-project-scope-explicit-over-cwd
  (testing "Explicit project_id wins over cwd-derived"
    (is (= "explicit" (resolve-project-scope "explicit" "/home/user/other" nil)))))
