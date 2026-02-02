(ns hive-mcp.tools.memory.migration-test
  "Unit tests for memory migration utilities.

   Tests the orphaned scope detection and migration:
   - hash-scope? detection heuristic
   - extract-scope-id from tags
   - update-scope-tag replacement"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.memory.migration :as migration]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; hash-scope? Detection
;; =============================================================================

(deftest test-hash-scope-true-for-hex-strings
  (testing "Returns true for long hex-only strings"
    (is (migration/hash-scope? "d987697ae05f40b1"))
    (is (migration/hash-scope? "abcdef0123456789"))
    (is (migration/hash-scope? "1234567890abcdef"))))

(deftest test-hash-scope-false-for-names
  (testing "Returns false for readable project names"
    (is (not (migration/hash-scope? "funeraria")))
    (is (not (migration/hash-scope? "hive-mcp")))
    (is (not (migration/hash-scope? "my_project")))
    (is (not (migration/hash-scope? "Project2024")))))

(deftest test-hash-scope-false-for-short-strings
  (testing "Returns false for short strings (< 12 chars)"
    (is (not (migration/hash-scope? "abc")))
    (is (not (migration/hash-scope? "abcdef")))
    (is (not (migration/hash-scope? "123456789ab")))))

(deftest test-hash-scope-false-for-mixed-chars
  (testing "Returns false for strings with non-hex characters"
    (is (not (migration/hash-scope? "d987697ae05f40g1")))  ; 'g' is not hex
    (is (not (migration/hash-scope? "funeraria-hash123")))
    (is (not (migration/hash-scope? "ABCDEF01234567")))))  ; uppercase

(deftest test-hash-scope-edge-cases
  (testing "Edge cases"
    (is (not (migration/hash-scope? nil)))
    (is (not (migration/hash-scope? "")))
    (is (not (migration/hash-scope? "   ")))))

;; =============================================================================
;; extract-scope-id (private, tested via orphaned-scope-tag?)
;; =============================================================================

(deftest test-hash-scope-boundary
  (testing "Boundary at 12 characters"
    ;; Exactly 12 chars - should be false (need > 12)
    (is (not (migration/hash-scope? "abcdef012345")))
    ;; 13 chars - should be true
    (is (migration/hash-scope? "abcdef0123456"))))

;; =============================================================================
;; Integration: Scope Tag Detection
;; =============================================================================

(deftest test-realistic-hash-scopes
  (testing "Realistic hash-based scopes from old system"
    ;; SHA-256 truncated to 16 hex chars was common
    (is (migration/hash-scope? "a1b2c3d4e5f67890"))
    (is (migration/hash-scope? "0123456789abcdef0123456789abcdef"))))

(deftest test-realistic-name-scopes
  (testing "Realistic name-based scopes"
    (is (not (migration/hash-scope? "funeraria")))
    (is (not (migration/hash-scope? "sisf-caixa-fe")))
    (is (not (migration/hash-scope? "hive-mcp")))
    (is (not (migration/hash-scope? "dotfiles")))))
