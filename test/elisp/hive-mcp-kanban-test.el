;;; hive-mcp-kanban-test.el --- ERT tests for hive-mcp-kanban -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Tests for project-scoped kanban functionality.
;; Focus: scope tag injection, scope isolation in queries.

;;; Code:

(require 'ert)

;; Load the module under test
(let ((elisp-dir (expand-file-name "../../elisp" (file-name-directory load-file-name))))
  (add-to-list 'load-path elisp-dir)
  (add-to-list 'load-path (expand-file-name "memory" elisp-dir)))
(require 'hive-mcp-kanban)
(require 'hive-mcp-memory)

;;;; Test: Scoped Tag Builder

(ert-deftest hive-mcp-kanban-test-build-tags-with-scope-explicit-project ()
  "Test that build-tags-with-scope includes explicit project scope."
  (let ((tags (hive-mcp-kanban--build-tags-with-scope "todo" "high" "my-project")))
    ;; Should include kanban, status, priority, and scope tag
    (should (member "kanban" tags))
    (should (member "todo" tags))
    (should (member "priority-high" tags))
    (should (member "scope:project:my-project" tags))))

(ert-deftest hive-mcp-kanban-test-build-tags-with-scope-different-status ()
  "Test that build-tags-with-scope works with different statuses."
  (let ((tags-doing (hive-mcp-kanban--build-tags-with-scope "doing" "medium" "test-proj"))
        (tags-review (hive-mcp-kanban--build-tags-with-scope "review" "low" "test-proj")))
    ;; Check status tags
    (should (member "doing" tags-doing))
    (should (member "review" tags-review))
    ;; Check priority tags
    (should (member "priority-medium" tags-doing))
    (should (member "priority-low" tags-review))
    ;; Both should have same project scope
    (should (member "scope:project:test-proj" tags-doing))
    (should (member "scope:project:test-proj" tags-review))))

(ert-deftest hive-mcp-kanban-test-build-tags-with-scope-nil-project ()
  "Test that build-tags-with-scope falls back to current project when nil."
  ;; Mock the project-id function to return a known value
  (cl-letf (((symbol-function 'hive-mcp-memory--project-id)
             (lambda (&optional _dir) "fallback-project")))
    (let ((tags (hive-mcp-kanban--build-tags-with-scope "todo" "high" nil)))
      (should (member "scope:project:fallback-project" tags)))))

(ert-deftest hive-mcp-kanban-test-build-tags-with-scope-preserves-base-tags ()
  "Test that build-tags-with-scope includes all base kanban tags."
  (let ((tags (hive-mcp-kanban--build-tags-with-scope "todo" "high" "proj")))
    ;; Must have exactly: kanban, status, priority-X, scope:project:X
    (should (= 4 (length tags)))
    (should (member "kanban" tags))
    (should (member "todo" tags))
    (should (member "priority-high" tags))
    (should (member "scope:project:proj" tags))))

;;;; Test: Original build-tags still works (backward compat)

(ert-deftest hive-mcp-kanban-test-build-tags-original ()
  "Test that original build-tags function still works."
  (let ((tags (hive-mcp-kanban--build-tags "todo" "high")))
    (should (member "kanban" tags))
    (should (member "todo" tags))
    (should (member "priority-high" tags))
    ;; Original should NOT have scope tag
    (should-not (seq-find (lambda (t) (string-prefix-p "scope:" t)) tags))))

;;;; Test: Scope Extraction Helper

(ert-deftest hive-mcp-kanban-test-extract-scope-from-entry ()
  "Test extracting scope tag from kanban entry tags."
  (let ((entry-with-scope '(:tags ("kanban" "todo" "scope:project:my-proj")))
        (entry-no-scope '(:tags ("kanban" "todo"))))
    ;; Entry with scope should return the scope
    (should (equal "scope:project:my-proj"
                   (hive-mcp-kanban--extract-scope entry-with-scope)))
    ;; Entry without scope should return nil
    (should-not (hive-mcp-kanban--extract-scope entry-no-scope))))

(provide 'hive-mcp-kanban-test)
;;; hive-mcp-kanban-test.el ends here
