(ns hive-mcp.tools.memory.scope-test
  "Unit tests for memory scope utilities.

   Tests the Go Context Pattern fix:
   - Directory nil → 'global' (no Emacs fallback)
   - Directory provided → project-id from path

   This prevents cross-project scope leaks when Emacs buffer focus
   doesn't match MCP request origin."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.memory.scope :as scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Go Context Pattern: Explicit directory → project-id
;; =============================================================================

(deftest test-get-current-project-id-nil-returns-global
  (testing "nil directory returns 'global' (Go Context Pattern)"
    (is (= "global" (scope/get-current-project-id nil)))
    (is (= "global" (scope/get-current-project-id)))))

(deftest test-get-current-project-id-from-directory
  (testing "Extracts project-id from directory path"
    (is (= "hive-mcp" (scope/get-current-project-id "/home/user/projects/hive-mcp")))
    (is (= "my-project" (scope/get-current-project-id "/var/projects/my-project")))))

(deftest test-get-current-project-id-trailing-slash
  (testing "Handles trailing slash correctly"
    ;; Trailing slash is stripped by str/split, so we get the project name
    (is (= "projects" (scope/get-current-project-id "/home/user/projects/")))))

(deftest test-get-current-project-id-blank-directory
  (testing "Blank directory string returns 'global'"
    (is (= "global" (scope/get-current-project-id "")))
    (is (= "global" (scope/get-current-project-id "   ")))))

(deftest test-get-current-project-id-root-path
  (testing "Root path returns 'global'"
    (is (= "global" (scope/get-current-project-id "/")))))

;; =============================================================================
;; Scope Tag Injection
;; =============================================================================

(deftest test-inject-project-scope-adds-tag
  (testing "Injects scope tag when none present"
    (is (= ["tag1" "scope:project:myproj"]
           (scope/inject-project-scope ["tag1"] "myproj")))))

(deftest test-inject-project-scope-preserves-existing
  (testing "Preserves existing scope tag"
    (is (= ["scope:global" "tag1"]
           (scope/inject-project-scope ["scope:global" "tag1"] "myproj")))))

(deftest test-inject-project-scope-global-project
  (testing "Injects scope:global for global project-id"
    (is (= ["tag1" "scope:global"]
           (scope/inject-project-scope ["tag1"] "global")))))

;; =============================================================================
;; Scope Matching
;; =============================================================================

(deftest test-matches-scope-nil-filter
  (testing "nil filter matches everything"
    (is (scope/matches-scope? {:tags ["scope:project:x"]} nil))
    (is (scope/matches-scope? {:tags ["scope:global"]} nil))
    (is (scope/matches-scope? {:tags []} nil))))

(deftest test-matches-scope-all-filter
  (testing "'all' filter matches everything"
    (is (scope/matches-scope? {:tags ["scope:project:x"]} "all"))
    (is (scope/matches-scope? {:tags ["scope:global"]} "all"))))

(deftest test-matches-scope-global-filter
  (testing "'global' filter matches only scope:global entries"
    (is (scope/matches-scope? {:tags ["scope:global"]} "global"))
    (is (not (scope/matches-scope? {:tags ["scope:project:x"]} "global")))))

(deftest test-matches-scope-specific-filter
  (testing "Specific scope matches that scope OR global"
    (is (scope/matches-scope? {:tags ["scope:project:x"]} "scope:project:x"))
    (is (scope/matches-scope? {:tags ["scope:global"]} "scope:project:x"))
    (is (not (scope/matches-scope? {:tags ["scope:project:y"]} "scope:project:x")))))

;; =============================================================================
;; Cross-Project Leak Prevention (regression test)
;; =============================================================================

(deftest test-no-emacs-fallback-on-nil-directory
  (testing "No Emacs fallback: nil directory → global, not Emacs buffer context"
    ;; This test documents the fix for the cross-project scope leak.
    ;; When directory is nil, we return 'global' rather than querying Emacs.
    ;; This prevents Project B memories from leaking into Project A queries
    ;; when Emacs happens to have a Project B buffer focused.
    (let [result (scope/get-current-project-id nil)]
      (is (= "global" result)
          "Should return 'global' without calling Emacs"))))
