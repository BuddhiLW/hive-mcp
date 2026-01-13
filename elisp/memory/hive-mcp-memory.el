;;; hive-mcp-memory.el --- Persistent memory for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Persistent memory system for hive-mcp. Stores notes, snippets,
;; conventions, decisions, and conversation history per-project.
;;
;; This is the facade module that provides the public API.
;; Implementation is delegated to submodules:
;;   - hive-mcp-memory-core     : Core utilities
;;   - hive-mcp-memory-project  : Project identification
;;   - hive-mcp-memory-scope    : Scope system
;;   - hive-mcp-memory-duration : Duration/lifespan
;;   - hive-mcp-memory-storage  : File I/O and caching
;;   - hive-mcp-memory-crud     : CRUD operations
;;   - hive-mcp-memory-query    : Query operations
;;
;; Design principles (SOLID/CLARITY):
;; - Facade Pattern: Unified API hiding module complexity
;; - Backward Compatible: All original functions preserved as aliases

;;; Code:

;; Add memory submodule directory to load path
(let ((memory-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path memory-dir))

;;;; Require Submodules

(require 'hive-mcp-memory-core)
(require 'hive-mcp-memory-project)
(require 'hive-mcp-memory-scope)
(require 'hive-mcp-memory-duration)
(require 'hive-mcp-memory-storage)
(require 'hive-mcp-memory-crud)
(require 'hive-mcp-memory-query)

;;;; Customization Group

(defgroup hive-mcp-memory nil
  "Memory and persistence settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-memory-")

;;;; Backward-Compatible Constants

(defconst hive-mcp-memory-durations hive-mcp-memory-duration-list
  "Valid duration categories, ordered shortest to longest.")

;;;; Backward-Compatible Variables

(defvaralias 'hive-mcp-memory-directory 'hive-mcp-memory-storage-directory)
(defvaralias 'hive-mcp-memory-auto-save 'hive-mcp-memory-storage-auto-save)
(defvaralias 'hive-mcp-memory-duration-days 'hive-mcp-memory-duration-days)
(defvaralias 'hive-mcp-memory-default-duration 'hive-mcp-memory-duration-default)
(defvaralias 'hive-mcp-memory-add-hook 'hive-mcp-memory-crud-add-hook)
(defvaralias 'hive-mcp-conversation-max-entries 'hive-mcp-memory-crud-conversation-max)
(defvaralias 'hive-mcp-project-config-file 'hive-mcp-memory-project-config-file)

;;;; Backward-Compatible Aliases - Core

(defalias 'hive-mcp-memory--generate-id 'hive-mcp-memory-core-generate-id)
(defalias 'hive-mcp-memory--timestamp 'hive-mcp-memory-core-timestamp)
(defalias 'hive-mcp-memory--ensure-list 'hive-mcp-memory-core-ensure-list)

;;;; Backward-Compatible Aliases - Project

(defalias 'hive-mcp-memory--parse-edn-string 'hive-mcp-memory-project--parse-edn-string)
(defalias 'hive-mcp-memory--parse-edn-value 'hive-mcp-memory-project--parse-edn-value)
(defalias 'hive-mcp-memory--read-project-config 'hive-mcp-memory-project--read-config)
(defalias 'hive-mcp-memory--get-stable-project-id 'hive-mcp-memory-project--get-stable-id)
(defalias 'hive-mcp-memory-clear-config-cache 'hive-mcp-memory-project-clear-config-cache)
(defalias 'hive-mcp-memory--project-id 'hive-mcp-memory-project-id)
(defalias 'hive-mcp-memory--project-id-hash 'hive-mcp-memory-project-id-hash)
(defalias 'hive-mcp-memory--get-project-root 'hive-mcp-memory-project-get-root)
(defalias 'hive-mcp-memory--get-project-name 'hive-mcp-memory-project-get-name)

;;;; Backward-Compatible Aliases - Scope

(defalias 'hive-mcp-memory--make-scope-tag 'hive-mcp-memory-scope-make-tag)
(defalias 'hive-mcp-memory--parse-scope-tag 'hive-mcp-memory-scope-parse-tag)
(defalias 'hive-mcp-memory--has-scope-tag-p 'hive-mcp-memory-scope-has-tag-p)
(defalias 'hive-mcp-memory--inject-project-scope 'hive-mcp-memory-scope-inject-project)
(defalias 'hive-mcp-memory--applicable-scope-tags 'hive-mcp-memory-scope-applicable-tags)
(defalias 'hive-mcp-memory--entry-matches-scope-p 'hive-mcp-memory-scope-entry-matches-p)

;;;; Backward-Compatible Aliases - Duration

(defalias 'hive-mcp-memory--calculate-expires 'hive-mcp-memory-duration-calculate-expires)
(defalias 'hive-mcp-memory--get-entry-duration 'hive-mcp-memory-duration-get)
(defalias 'hive-mcp-memory--entry-expired-p 'hive-mcp-memory-duration-expired-p)

;;;; Backward-Compatible Aliases - Storage

(defalias 'hive-mcp-memory--project-dir 'hive-mcp-memory-storage-project-dir)
(defalias 'hive-mcp-memory--file-path 'hive-mcp-memory-storage-file-path)
(defalias 'hive-mcp-memory--read-json-file 'hive-mcp-memory-storage-read-json)
(defalias 'hive-mcp-memory--write-json-file 'hive-mcp-memory-storage-write-json)
(defalias 'hive-mcp-memory--plist-to-alist 'hive-mcp-memory-storage--plist-to-alist)
(defalias 'hive-mcp-memory--convert-for-json 'hive-mcp-memory-storage--convert-for-json)
(defalias 'hive-mcp-memory--cache-key 'hive-mcp-memory-storage--cache-key)
(defalias 'hive-mcp-memory--get-data 'hive-mcp-memory-storage-get-data)
(defalias 'hive-mcp-memory--set-data 'hive-mcp-memory-storage-set-data)

;;;; Backward-Compatible Aliases - CRUD

(defalias 'hive-mcp-memory--normalize-content 'hive-mcp-memory-crud--normalize-content)
(defalias 'hive-mcp-memory-content-hash 'hive-mcp-memory-crud-content-hash)
(defalias 'hive-mcp-memory-find-duplicate 'hive-mcp-memory-crud-find-duplicate)
(defalias 'hive-mcp-memory--merge-tags 'hive-mcp-memory-crud--merge-tags)
(defalias 'hive-mcp-memory-add 'hive-mcp-memory-crud-add)
(defalias 'hive-mcp-memory-get 'hive-mcp-memory-crud-get)
(defalias 'hive-mcp-memory-update 'hive-mcp-memory-crud-update)
(defalias 'hive-mcp-memory-delete 'hive-mcp-memory-crud-delete)
(defalias 'hive-mcp-memory-set-duration 'hive-mcp-memory-crud-set-duration)
(defalias 'hive-mcp-memory-promote 'hive-mcp-memory-crud-promote)
(defalias 'hive-mcp-memory-demote 'hive-mcp-memory-crud-demote)

;;;; Backward-Compatible Aliases - Query

(defalias 'hive-mcp-memory-query 'hive-mcp-memory-query)
(defalias 'hive-mcp-memory-query-expiring 'hive-mcp-memory-query-expiring)
(defalias 'hive-mcp-memory-cleanup-expired 'hive-mcp-memory-cleanup-expired)
(defalias 'hive-mcp-memory-log-access 'hive-mcp-memory-query-log-access)
(defalias 'hive-mcp-memory-mark-helpful 'hive-mcp-memory-query-mark-helpful)
(defalias 'hive-mcp-memory-mark-unhelpful 'hive-mcp-memory-query-mark-unhelpful)
(defalias 'hive-mcp-memory-helpfulness-ratio 'hive-mcp-memory-query-helpfulness-ratio)

;;;; Convenience Functions

(defun hive-mcp-memory-add-note (content &optional tags)
  "Add a note with CONTENT to current project memory.
TAGS is an optional list of tag strings."
  (hive-mcp-memory-crud-add 'note content tags))

(defun hive-mcp-memory-add-snippet (name code &optional language tags)
  "Add a code snippet with NAME and CODE.
LANGUAGE specifies the programming language.
TAGS is an optional list of tag strings."
  (hive-mcp-memory-crud-add 'snippet
                             (list :name name
                                   :code code
                                   :language (or language "unknown"))
                             tags))

(defun hive-mcp-memory-add-convention (description &optional example)
  "Add a project convention with DESCRIPTION.
EXAMPLE provides an optional code example."
  (hive-mcp-memory-crud-add 'convention
                             (list :description description
                                   :example example)))

(defun hive-mcp-memory-add-decision (title rationale &optional alternatives)
  "Add an architecture decision record with TITLE and RATIONALE.
ALTERNATIVES lists other options that were considered."
  (hive-mcp-memory-crud-add 'decision
                             (list :title title
                                   :rationale rationale
                                   :alternatives alternatives)))

(defun hive-mcp-memory-log-conversation (role content)
  "Log a conversation entry with ROLE and CONTENT.
ROLE should be `user' or `assistant'."
  (hive-mcp-memory-crud-add 'conversation
                             (list :role (if (symbolp role) (symbol-name role) role)
                                   :content content)))

;;;; Project Context

(defun hive-mcp-memory-get-project-context ()
  "Return full project context as plist for Claude.
Includes notes, conventions, recent decisions, relevant snippets."
  (let ((pid (hive-mcp-memory-project-id)))
    (list
     :project-id pid
     :project-root (hive-mcp-memory-project-get-root)
     :notes (hive-mcp-memory-query 'note nil pid 10)
     :conventions (hive-mcp-memory-query 'convention nil pid)
     :recent-decisions (hive-mcp-memory-query 'decision nil pid 5)
     :snippets (hive-mcp-memory-query 'snippet nil pid 20))))

;;;; Initialization

(defun hive-mcp-memory-init ()
  "Initialize memory system, create directories if needed."
  (hive-mcp-memory-storage-init))

;;;; Save/Load

(defun hive-mcp-memory-save (&optional project-id)
  "Save all cached memory for PROJECT-ID to disk."
  (let ((pid (or project-id (hive-mcp-memory-project-id))))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let ((key (hive-mcp-memory-storage--cache-key pid type)))
        (when-let* ((data (gethash key hive-mcp-memory-storage--cache)))
          (hive-mcp-memory-storage-write-json
           (hive-mcp-memory-storage-file-path pid type)
           data))))))

(defun hive-mcp-memory-save-all ()
  "Save all cached memory to disk."
  (maphash
   (lambda (key data)
     (let* ((parts (split-string key ":"))
            (pid (car parts))
            (type (cadr parts)))
       (hive-mcp-memory-storage-write-json
        (hive-mcp-memory-storage-file-path pid type)
        data)))
   hive-mcp-memory-storage--cache))

;;;; Migration Support

(defun hive-mcp-memory-migrate-project (old-project-id new-project-id &optional update-scopes)
  "Migrate memory from OLD-PROJECT-ID to NEW-PROJECT-ID.
If UPDATE-SCOPES is non-nil, also update scope tags in entries.
Returns a plist with migration statistics.

Use this when:
- Renaming a project directory (old ID was path-based hash)
- Consolidating memory from multiple project IDs
- Moving to stable .hive-project.edn based IDs"
  (let ((old-dir (hive-mcp-memory-storage-project-dir old-project-id))
        (new-dir (hive-mcp-memory-storage-project-dir new-project-id))
        (types '("note" "snippet" "convention" "decision" "conversation"))
        (migrated 0)
        (updated-scopes 0)
        (errors nil))
    ;; Create new directory if needed
    (make-directory new-dir t)

    (dolist (type types)
      (let ((old-file (expand-file-name (format "%s.json" type) old-dir))
            (new-file (expand-file-name (format "%s.json" type) new-dir)))
        (when (file-exists-p old-file)
          (condition-case err
              (let* ((old-data (hive-mcp-memory-storage-read-json old-file))
                     (existing-data (hive-mcp-memory-storage-read-json new-file))
                     ;; Update scope tags if requested
                     (processed-data
                      (if update-scopes
                          (mapcar
                           (lambda (entry)
                             (let* ((tags (plist-get entry :tags))
                                    (old-scope (format "scope:project:%s" old-project-id))
                                    (new-scope (format "scope:project:%s" new-project-id))
                                    (new-tags (mapcar
                                               (lambda (tag)
                                                 (if (string= tag old-scope)
                                                     (progn
                                                       (cl-incf updated-scopes)
                                                       new-scope)
                                                   tag))
                                               tags)))
                               (plist-put entry :tags new-tags)))
                           old-data)
                        old-data))
                     ;; Merge with existing (avoid duplicates by ID)
                     (existing-ids (mapcar (lambda (e) (plist-get e :id)) existing-data))
                     (new-entries (seq-remove
                                   (lambda (e) (member (plist-get e :id) existing-ids))
                                   processed-data))
                     (merged-data (append existing-data new-entries)))
                ;; Write merged data
                (hive-mcp-memory-storage-write-json new-file merged-data)
                (cl-incf migrated (length new-entries)))
            (error
             (push (cons type (error-message-string err)) errors))))))

    ;; Clear cache for both projects
    (hive-mcp-memory-storage-clear-cache old-project-id)
    (hive-mcp-memory-storage-clear-cache new-project-id)

    (list :migrated migrated
          :updated-scopes updated-scopes
          :errors errors
          :old-project-id old-project-id
          :new-project-id new-project-id)))

(provide 'hive-mcp-memory)
;;; hive-mcp-memory.el ends here
