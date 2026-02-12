;;; hive-mcp-memory-crud.el --- CRUD operations for memory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; CRUD operations for hive-mcp-memory.
;; Handles add, get, update, delete operations with deduplication.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles CRUD + deduplication
;; - Inputs are guarded: Validates at entry points
;; - Yield safe failure: Graceful handling of missing entries

;;; Code:

(require 'seq)
(require 'hive-mcp-memory-core)
(require 'hive-mcp-memory-project)
(require 'hive-mcp-memory-scope)
(require 'hive-mcp-memory-duration)
(require 'hive-mcp-memory-storage)

;;;; Customization

(defcustom hive-mcp-memory-crud-conversation-max 100
  "Maximum conversation log entries per project."
  :type 'integer
  :group 'hive-mcp-memory)

;;;; Hooks

(defvar hive-mcp-memory-crud-add-hook nil
  "Hook run after adding memory entry. Args: TYPE ENTRY PROJECT-ID.")

;;;; Content Deduplication

(defun hive-mcp-memory-crud--normalize-content (content)
  "Normalize CONTENT for consistent hashing.
Trims whitespace, collapses multiple spaces/newlines.
For plists, converts to canonical string representation."
  (let ((text (cond
               ((stringp content) content)
               ((plistp content) (format "%S" content))
               (t (format "%S" content)))))
    ;; Trim leading/trailing whitespace
    (setq text (string-trim text))
    ;; Collapse multiple spaces to single space
    (setq text (replace-regexp-in-string "[ \t]+" " " text))
    ;; Collapse multiple newlines to single newline
    (setq text (replace-regexp-in-string "\n+" "\n" text))
    text))

(defun hive-mcp-memory-crud-content-hash (content)
  "Compute SHA-256 hash of normalized CONTENT.
Returns a 64-character hex string."
  (secure-hash 'sha256 (hive-mcp-memory-crud--normalize-content content)))

(defun hive-mcp-memory-crud-find-duplicate (type content &optional project-id)
  "Find existing entry with same content hash in TYPE.
TYPE is a symbol or string: note, snippet, convention, decision.
CONTENT is the content to check for duplicates.
PROJECT-ID defaults to current project.
Returns the existing entry if found, nil otherwise."
  (let* ((pid (or project-id (hive-mcp-memory-project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         (content-hash (hive-mcp-memory-crud-content-hash content))
         (data (hive-mcp-memory-storage-get-data pid type-str)))
    (seq-find (lambda (entry)
                (let ((entry-hash (plist-get entry :content-hash)))
                  (and entry-hash (string= entry-hash content-hash))))
              data)))

(defun hive-mcp-memory-crud--merge-tags (existing-tags new-tags)
  "Merge NEW-TAGS into EXISTING-TAGS, removing duplicates.
Returns a list of unique tags."
  (hive-mcp-memory-core-merge-lists existing-tags new-tags #'string=))

;;;; Create

(defun hive-mcp-memory-crud-add (type content &optional tags project-id duration)
  "Add a memory entry of TYPE with CONTENT.
TYPE is one of: note, snippet, convention, decision, conversation.
CONTENT is a string or plist.
TAGS is optional list of strings. If no scope tag is present,
automatically injects the current project scope.
PROJECT-ID defaults to current project or global.
DURATION is optional lifespan (symbol from duration list).
Defaults to `hive-mcp-memory-duration-default'.

If an entry with the same content already exists (based on content hash),
the existing entry is returned instead of creating a duplicate.
If TAGS are provided, they are merged with the existing entry's tags."
  (let* ((pid (or project-id (hive-mcp-memory-project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         ;; Auto-inject project scope if no scope tag present
         (tags-with-scope (hive-mcp-memory-scope-inject-project (or tags '())))
         (content-hash (hive-mcp-memory-crud-content-hash content))
         ;; Check for duplicate (skip for conversations - they're logs)
         (existing (unless (string= type-str "conversation")
                     (hive-mcp-memory-crud-find-duplicate type content pid))))
    (if existing
        ;; Duplicate found - merge tags if new ones provided
        (if (and tags-with-scope (not (seq-empty-p tags-with-scope)))
            (let ((merged-tags (hive-mcp-memory-crud--merge-tags
                                (plist-get existing :tags) tags-with-scope)))
              (hive-mcp-memory-crud-update (plist-get existing :id)
                                            (list :tags merged-tags) pid)
              ;; Return updated entry
              (hive-mcp-memory-crud-get (plist-get existing :id) pid))
          ;; No new tags, just return existing
          existing)
      ;; No duplicate - create new entry
      (let* ((dur (or duration hive-mcp-memory-duration-default))
             (entry (list :id (hive-mcp-memory-core-generate-id)
                          :type type-str
                          :content content
                          :content-hash content-hash
                          :tags tags-with-scope
                          :duration (symbol-name dur)
                          :expires (hive-mcp-memory-duration-calculate-expires dur)
                          :created (hive-mcp-memory-core-timestamp)
                          :updated (hive-mcp-memory-core-timestamp)))
             (data (hive-mcp-memory-storage-get-data pid type-str)))
        ;; For conversations, enforce ring buffer limit
        (when (string= type-str "conversation")
          (when (>= (length data) hive-mcp-memory-crud-conversation-max)
            (setq data (seq-take data (1- hive-mcp-memory-crud-conversation-max)))))
        ;; Prepend new entry
        (setq data (cons entry data))
        (hive-mcp-memory-storage-set-data pid type-str data)
        ;; Run hooks
        (run-hook-with-args 'hive-mcp-memory-crud-add-hook type entry pid)
        entry))))

;;;; Read

(defun hive-mcp-memory-crud-get (id &optional project-id)
  "Retrieve memory entry by ID from PROJECT-ID."
  (let ((pid (or project-id (hive-mcp-memory-project-id))))
    (catch 'found
      (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
        (dolist (entry (hive-mcp-memory-storage-get-data pid type))
          (when (string= (plist-get entry :id) id)
            (throw 'found entry))))
      nil)))

;;;; Update

(defun hive-mcp-memory-crud-update (id updates &optional project-id)
  "Update memory entry ID with UPDATES plist.
PROJECT-ID specifies the project (defaults to current)."
  (let* ((pid (or project-id (hive-mcp-memory-project-id)))
         (found nil))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let ((data (hive-mcp-memory-storage-get-data pid type)))
        (setq data
              (mapcar
               (lambda (entry)
                 (if (string= (plist-get entry :id) id)
                     (progn
                       (setq found t)
                       (let ((updated entry)
                             (upd updates))
                         ;; Apply updates
                         (while upd
                           (setq updated (plist-put updated (car upd) (cadr upd)))
                           (setq upd (cddr upd)))
                         (plist-put updated :updated (hive-mcp-memory-core-timestamp))))
                   entry))
               data))
        (when found
          (hive-mcp-memory-storage-set-data pid type data))))
    found))

;;;; Delete

(defun hive-mcp-memory-crud-delete (id &optional project-id)
  "Delete memory entry by ID.
PROJECT-ID specifies the project (defaults to current)."
  (let* ((pid (or project-id (hive-mcp-memory-project-id)))
         (deleted nil))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let* ((data (hive-mcp-memory-storage-get-data pid type))
             (new-data (seq-remove
                        (lambda (entry)
                          (when (string= (plist-get entry :id) id)
                            (setq deleted t)))
                        data)))
        (when deleted
          (hive-mcp-memory-storage-set-data pid type new-data))))
    deleted))

;;;; Duration Operations

(defun hive-mcp-memory-crud-set-duration (id duration &optional project-id)
  "Update entry ID to DURATION and recalculate expires.
DURATION must be one of the valid duration symbols.
PROJECT-ID specifies the project (defaults to current).
Returns t if successful, nil otherwise."
  (hive-mcp-memory-duration-validate duration)
  (hive-mcp-memory-crud-update id
                                (list :duration (symbol-name duration)
                                      :expires (hive-mcp-memory-duration-calculate-expires duration))
                                project-id))

(defun hive-mcp-memory-crud-promote (id &optional project-id)
  "Promote entry ID to next longer duration in the hierarchy.
Returns new duration symbol, or nil if already permanent or not found."
  (when-let* ((entry (hive-mcp-memory-crud-get id project-id)))
    (let* ((current (hive-mcp-memory-duration-get entry))
           (new-duration (hive-mcp-memory-duration-next current)))
      (when new-duration
        (hive-mcp-memory-crud-set-duration id new-duration project-id)
        new-duration))))

(defun hive-mcp-memory-crud-demote (id &optional project-id)
  "Demote entry ID to next shorter duration in the hierarchy.
Returns new duration symbol, or nil if already session or not found."
  (when-let* ((entry (hive-mcp-memory-crud-get id project-id)))
    (let* ((current (hive-mcp-memory-duration-get entry))
           (new-duration (hive-mcp-memory-duration-prev current)))
      (when new-duration
        (hive-mcp-memory-crud-set-duration id new-duration project-id)
        new-duration))))

(provide 'hive-mcp-memory-crud)
;;; hive-mcp-memory-crud.el ends here
