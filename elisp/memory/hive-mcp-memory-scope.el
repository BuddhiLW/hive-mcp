;;; hive-mcp-memory-scope.el --- Scope system for memory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Scope system for hive-mcp-memory.
;; Provides layered memory isolation via scope tags.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles scope tag operations
;; - Open/Closed: New scope levels can be added without modification
;;
;; Scope levels:
;;   scope:global         - Universal (SOLID, DDD, etc.)
;;   scope:domain:<name>  - Cross-project within domain
;;   scope:project:<name> - Single project only

;;; Code:

(require 'seq)
(require 'hive-mcp-memory-core)
(require 'hive-mcp-memory-project)

;;;; Scope Tag Creation

(defun hive-mcp-memory-scope-make-tag (level &optional name)
  "Create a scope tag for LEVEL with optional NAME.
LEVEL is one of: global, domain, project.
NAME is required for domain and project levels."
  (pcase level
    ('global "scope:global")
    ('domain (format "scope:domain:%s" name))
    ('project (format "scope:project:%s" name))
    (_ (error "Invalid scope level: %s" level))))

;;;; Scope Tag Parsing

(defun hive-mcp-memory-scope-parse-tag (tag)
  "Parse a scope TAG into (level . name) cons.
Returns nil if TAG is not a scope tag."
  (cond
   ((string= tag "scope:global") '(global . nil))
   ((string-prefix-p "scope:domain:" tag)
    (cons 'domain (substring tag (length "scope:domain:"))))
   ((string-prefix-p "scope:project:" tag)
    (cons 'project (substring tag (length "scope:project:"))))
   (t nil)))

;;;; Scope Detection

(defun hive-mcp-memory-scope-has-tag-p (tags)
  "Return non-nil if TAGS contains any scope tag."
  (seq-some (lambda (tag) (string-prefix-p "scope:" tag)) tags))

;;;; Scope Injection

(defun hive-mcp-memory-scope-inject-project (tags)
  "Inject current project scope into TAGS if no scope present.
Returns modified tags list.
Defensively converts vectors to lists to prevent malformed cons cells."
  (let ((tags-list (hive-mcp-memory-core-ensure-list tags)))
    (if (hive-mcp-memory-scope-has-tag-p tags-list)
        tags-list  ; Already has scope
      (if-let* ((project-name (hive-mcp-memory-project-get-name)))
          (cons (hive-mcp-memory-scope-make-tag 'project project-name) tags-list)
        tags-list))))  ; Not in project, leave as-is (global)

;;;; Applicable Scopes

(defun hive-mcp-memory-scope-applicable-tags (&optional project-name domain-name)
  "Return list of scope tags applicable to current context.
PROJECT-NAME defaults to current project.
DOMAIN-NAME is optional domain filter.
Always includes scope:global."
  (let ((tags (list "scope:global")))
    (when domain-name
      (push (hive-mcp-memory-scope-make-tag 'domain domain-name) tags))
    (when-let* ((proj (or project-name (hive-mcp-memory-project-get-name))))
      (push (hive-mcp-memory-scope-make-tag 'project proj) tags))
    tags))

;;;; Scope Matching

(defun hive-mcp-memory-scope-entry-matches-p (entry scope-tags)
  "Return non-nil if ENTRY matches any of SCOPE-TAGS.
Entries without scope tags are treated as global."
  (let ((entry-tags (plist-get entry :tags)))
    (if (hive-mcp-memory-scope-has-tag-p entry-tags)
        ;; Entry has scope - check if it matches any applicable scope
        (seq-some (lambda (scope-tag)
                    (seq-contains-p entry-tags scope-tag #'string=))
                  scope-tags)
      ;; No scope tag = global, always matches
      t)))

(provide 'hive-mcp-memory-scope)
;;; hive-mcp-memory-scope.el ends here
