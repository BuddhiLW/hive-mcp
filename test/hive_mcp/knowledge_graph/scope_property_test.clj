(ns hive-mcp.knowledge-graph.scope-property-test
  "Property-based tests for KG scope hierarchy.

   Properties proven:
   - normalize-scope: idempotent, deterministic, nil-safe
   - resolve-project-id: idempotent
   - visible-scopes: always ends with global, includes self, terminates
   - scope-contains?: reflexive, transitive via hierarchy
   - global contains all scopes
   - scope->tag: injective (distinct scopes → distinct tags)
   - Hierarchy is well-founded (finite depth)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [clojure.set]
            [hive-mcp.knowledge-graph.scope :as scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Fixtures
;; ============================================================

(defn clean-fixture [f]
  (scope/clear-config-cache!)
  (f)
  (scope/clear-config-cache!))

(use-fixtures :each clean-fixture)

;; ============================================================
;; Private fn access (for testing internal calculations)
;; ============================================================

(def ^:private normalize-scope
  @#'hive-mcp.knowledge-graph.scope/normalize-scope)

(def ^:private global-or-nil?
  @#'hive-mcp.knowledge-graph.scope/global-or-nil?)

;; ============================================================
;; Generators
;; ============================================================

(def gen-segment
  "Generate a valid scope segment (lowercase alphanumeric + hyphen)."
  (gen/fmap (fn [n] (str "proj-" n))
            (gen/choose 0 999)))

(def gen-simple-scope
  "Generate a simple single-segment scope."
  gen-segment)

(def gen-hierarchical-scope
  "Generate a colon-delimited hierarchical scope (1-4 segments)."
  (gen/fmap (fn [segments] (str/join ":" segments))
            (gen/vector gen-segment 1 4)))

(def gen-scope-format
  "Generate a scope in various input formats."
  (gen/one-of
   [gen-hierarchical-scope
    (gen/fmap #(str "scope:project:" %) gen-hierarchical-scope)
    (gen/return "global")
    (gen/return "scope:global")
    (gen/return nil)
    (gen/return "")
    (gen/return "   ")]))

;; ============================================================
;; Properties: normalize-scope
;; ============================================================

(defspec prop-normalize-scope-idempotent 200
  (prop/for-all [s gen-scope-format]
                (let [once  (normalize-scope s)
                      twice (normalize-scope once)]
                  (= once twice))))

(defspec prop-normalize-scope-deterministic 200
  (prop/for-all [s gen-scope-format]
                (= (normalize-scope s) (normalize-scope s))))

(defspec prop-normalize-scope-strips-prefix 200
  (prop/for-all [s gen-hierarchical-scope]
                (let [prefixed  (str "scope:project:" s)
                      result    (normalize-scope prefixed)]
                  (= result s))))

(defspec prop-normalize-scope-global-canonical 200
  (prop/for-all [s (gen/elements ["global" "scope:global"])]
                (= "global" (normalize-scope s))))

(defspec prop-normalize-scope-nil-for-blank 200
  (prop/for-all [s (gen/elements [nil "" "   " "\t"])]
                (nil? (normalize-scope s))))

;; ============================================================
;; Properties: global-or-nil?
;; ============================================================

(defspec prop-global-or-nil-true-cases 200
  (prop/for-all [s (gen/elements [nil "global"])]
                (true? (global-or-nil? s))))

(defspec prop-global-or-nil-false-for-projects 200
  (prop/for-all [s gen-simple-scope]
                (not (global-or-nil? s))))

;; ============================================================
;; Properties: resolve-project-id
;; ============================================================

(defspec prop-resolve-project-id-idempotent 200
  (prop/for-all [s gen-simple-scope]
                (= (scope/resolve-project-id s)
                   (scope/resolve-project-id (scope/resolve-project-id s)))))

(defspec prop-resolve-project-id-nil-safe 200
  (prop/for-all [_ (gen/return nil)]
                (nil? (scope/resolve-project-id nil))))

(defspec prop-resolve-project-id-passthrough-unregistered 200
  (prop/for-all [s gen-simple-scope]
                (= s (scope/resolve-project-id s))))

;; ============================================================
;; Properties: visible-scopes
;; ============================================================

(defspec prop-visible-scopes-ends-with-global 200
  (prop/for-all [s gen-hierarchical-scope]
                (= "global" (last (scope/visible-scopes s)))))

(defspec prop-visible-scopes-includes-self 200
  (prop/for-all [s gen-hierarchical-scope]
                (let [resolved (scope/resolve-project-id (normalize-scope s))
                      vis      (scope/visible-scopes s)]
                  (= resolved (first vis)))))

(defspec prop-visible-scopes-terminates 200
  (prop/for-all [s gen-scope-format]
                (let [vis (scope/visible-scopes s)]
                  (and (vector? vis)
                       (pos? (count vis))
                       (<= (count vis) 100)))))

(defspec prop-visible-scopes-nil-gives-global-only 200
  (prop/for-all [_ (gen/return nil)]
                (= ["global"] (scope/visible-scopes nil))))

(defspec prop-visible-scopes-monotone-length 200
  (prop/for-all [s gen-hierarchical-scope]
                (let [vis (scope/visible-scopes s)]
      ;; Each ancestor has equal or fewer visible scopes
                  (every? true?
                          (map (fn [a b]
                                 (>= (count (scope/visible-scopes a))
                                     (count (scope/visible-scopes b))))
                               vis (rest vis))))))

(defspec prop-visible-scopes-no-duplicates 200
  (prop/for-all [s gen-hierarchical-scope]
                (let [vis (scope/visible-scopes s)]
                  (= (count vis) (count (distinct vis))))))

;; ============================================================
;; Properties: scope-contains?
;; ============================================================

(defspec prop-scope-contains-reflexive 200
  (prop/for-all [s gen-hierarchical-scope]
                (true? (scope/scope-contains? s s))))

(defspec prop-global-contains-all 200
  (prop/for-all [s gen-hierarchical-scope]
                (true? (scope/scope-contains? "global" s))))

(defspec prop-scope-contains-nil-reflexive 200
  (prop/for-all [_ (gen/return nil)]
                (true? (scope/scope-contains? nil nil))))

(defspec prop-scope-contains-parent-child 200
  (prop/for-all [parent gen-simple-scope
                 child  gen-simple-scope]
                (let [hierarchical (str parent ":" child)]
      ;; Parent always contains child in colon-delimited hierarchy
                  (true? (scope/scope-contains? parent hierarchical)))))

(defspec prop-scope-contains-transitive-visible 200
  (prop/for-all [s gen-hierarchical-scope]
                (let [vis (scope/visible-scopes s)]
      ;; Every ancestor visible from s should contain s
                  (every? #(scope/scope-contains? % s) vis))))

;; ============================================================
;; Properties: scope->tag
;; ============================================================

(defspec prop-scope-tag-round-trip 200
  (prop/for-all [s gen-simple-scope]
                (let [tag (scope/scope->tag s)]
                  (= (str "scope:project:" (normalize-scope s)) tag))))

(defspec prop-scope-tag-injective 200
  (prop/for-all [s1 gen-simple-scope
                 s2 gen-simple-scope]
                (if (= (normalize-scope s1) (normalize-scope s2))
                  (= (scope/scope->tag s1) (scope/scope->tag s2))
                  (not= (scope/scope->tag s1) (scope/scope->tag s2)))))

(defspec prop-scope-tag-global-for-nil 200
  (prop/for-all [_ (gen/return nil)]
                (= "scope:global" (scope/scope->tag nil))))

;; ============================================================
;; Properties: visible-scope-tags
;; ============================================================

(defspec prop-visible-scope-tags-always-includes-global 200
  (prop/for-all [s gen-scope-format]
                (contains? (scope/visible-scope-tags s) "scope:global")))

(defspec prop-visible-scope-tags-includes-self-tag 200
  (prop/for-all [s gen-hierarchical-scope]
                (contains? (scope/visible-scope-tags s) (scope/scope->tag s))))

;; ============================================================
;; Properties: derive-hierarchy-scope-filter
;; ============================================================

(defspec prop-derive-filter-nil-for-all 200
  (prop/for-all [_ (gen/return "all")]
                (nil? (scope/derive-hierarchy-scope-filter "all"))))

(defspec prop-derive-filter-nil-for-nil 200
  (prop/for-all [_ (gen/return nil)]
                (nil? (scope/derive-hierarchy-scope-filter nil))))

(defspec prop-derive-filter-returns-set 200
  (prop/for-all [s gen-hierarchical-scope]
                (set? (scope/derive-hierarchy-scope-filter s))))

;; ============================================================
;; Properties: full-hierarchy-scope-tags
;; ============================================================

(defspec prop-full-hierarchy-includes-visible 200
  (prop/for-all [s gen-hierarchical-scope]
                (let [full    (scope/full-hierarchy-scope-tags s)
                      visible (scope/visible-scope-tags s)]
      ;; full-hierarchy is a superset of visible (ancestors)
                  (clojure.set/subset? visible full))))

(defspec prop-full-hierarchy-global-for-nil 200
  (prop/for-all [_ (gen/return nil)]
                (= #{"scope:global"} (scope/full-hierarchy-scope-tags nil))))

;; ============================================================
;; Unit Tests for Extracted Helpers
;; ============================================================

(deftest test-normalize-scope-nil-returns-nil
  (is (nil? (normalize-scope nil))))

(deftest test-normalize-scope-blank-returns-nil
  (is (nil? (normalize-scope "")))
  (is (nil? (normalize-scope "   "))))

(deftest test-normalize-scope-strips-prefix
  (is (= "foo" (normalize-scope "scope:project:foo")))
  (is (= "global" (normalize-scope "scope:global"))))

(deftest test-global-or-nil-predicate
  (is (true? (global-or-nil? nil)))
  (is (true? (global-or-nil? "global")))
  (is (not (global-or-nil? "hive-mcp")))
  (is (not (global-or-nil? ""))))

(deftest test-visible-scopes-deep-hierarchy
  (testing "5-level hierarchy walks correctly"
    (let [vis (scope/visible-scopes "a:b:c:d:e")]
      (is (= ["a:b:c:d:e" "a:b:c:d" "a:b:c" "a:b" "a" "global"] vis)))))

(deftest test-scope-contains-with-alias-registration
  (testing "Alias resolution in scope-contains?"
    (scope/register-project-config! "hive-mcp"
                                    {:project-id "hive-mcp"
                                     :aliases ["emacs-mcp"]})
    (is (true? (scope/scope-contains? "hive-mcp" "emacs-mcp")))
    (is (true? (scope/scope-contains? "emacs-mcp" "hive-mcp")))
    (is (true? (scope/scope-contains? "global" "emacs-mcp")))))

(deftest test-iterate-approach-matches-original
  (testing "iterate+take-while gives correct results for various depths"
    ;; 1-level
    (is (= ["proj" "global"]
           (scope/visible-scopes "proj")))
    ;; 2-level
    (is (= ["a:b" "a" "global"]
           (scope/visible-scopes "a:b")))
    ;; 3-level
    (is (= ["a:b:c" "a:b" "a" "global"]
           (scope/visible-scopes "a:b:c")))))
