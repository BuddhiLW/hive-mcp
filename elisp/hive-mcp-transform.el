;;; hive-mcp-transform.el --- Pure data transformation layer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:

;; CLARITY-L compliant data transformation layer.
;; 
;; This module contains pure functions for data format conversion:
;; - plist <-> alist conversions for JSON serialization
;; - Content preview/truncation
;; - Scope argument normalization
;; - Entry metadata extraction
;;
;; Design principles:
;; - Pure functions (no side effects)
;; - No domain knowledge (agnostic to memory/kanban/etc)
;; - Composable transformations
;; - JSON-serialization aware (vectors, :false, etc)

;;; Code:

;;;; Core: plist-to-alist

(defun hive-mcp-transform-plist-to-alist (plist)
  "Convert PLIST to alist for JSON serialization.
Keyword keys become symbols (colon stripped).
Lists become vectors (for JSON arrays).
Nested plists are recursively converted.
Nil values become empty vectors."
  (let (alist)
    (while plist
      (let* ((key (car plist))
             (val (cadr plist))
             (key-sym (if (keywordp key)
                          (intern (substring (symbol-name key) 1))
                        key))
             (val-converted (hive-mcp-transform--convert-value val)))
        (push (cons key-sym val-converted) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun hive-mcp-transform--convert-value (val)
  "Convert VAL for JSON serialization.
Plists become alists, lists become vectors, nil becomes []."
  (cond
   ;; Nested plist (starts with keyword)
   ((and (listp val) (keywordp (car-safe val)))
    (hive-mcp-transform-plist-to-alist val))
   ;; List of plists -> vector of alists
   ((and (listp val) val (listp (car-safe val)) (keywordp (car-safe (car-safe val))))
    (apply #'vector
           (mapcar #'hive-mcp-transform-plist-to-alist val)))
   ;; Plain list -> vector
   ((and (listp val) val)
    (apply #'vector
           (mapcar (lambda (v)
                     (if (and (listp v) (keywordp (car-safe v)))
                         (hive-mcp-transform-plist-to-alist v)
                       v))
                   val)))
   ;; nil -> empty vector
   ((null val) [])
   ;; Atoms pass through
   (t val)))

;;;; Batch conversion

(defun hive-mcp-transform-entries-to-vector (entries)
  "Convert list of plist ENTRIES to vector of alists for JSON.
Empty list returns empty vector."
  (apply #'vector (mapcar #'hive-mcp-transform-plist-to-alist entries)))

;;;; Scope argument normalization

(defun hive-mcp-transform-scope-arg (scope-filter)
  "Normalize SCOPE-FILTER string to domain format.
- nil -> nil (auto-filter by project)
- \"all\" -> t (no filter)
- \"global\" -> 'global symbol
- other -> pass through as-is"
  (cond
   ((null scope-filter) nil)
   ((string= scope-filter "all") t)
   ((string= scope-filter "global") 'global)
   (t scope-filter)))

;;;; Content preview

(defun hive-mcp-transform-content-preview (content &optional max-len)
  "Return a preview of CONTENT truncated to MAX-LEN characters.
MAX-LEN defaults to 100.

For plist content, extracts meaningful text in order:
:description > :title > :name > :code > formatted plist

For strings, truncates with ellipsis if needed.
Other types are formatted as strings."
  (let ((max-len (or max-len 100))
        (text (hive-mcp-transform--extract-text content)))
    (if (> (length text) max-len)
        (concat (substring text 0 (- max-len 3)) "...")
      text)))

(defun hive-mcp-transform--extract-text (content)
  "Extract displayable text from CONTENT."
  (cond
   ((stringp content) content)
   ((and (listp content) (keywordp (car-safe content)))
    ;; Plist - try meaningful keys in order
    (or (plist-get content :description)
        (plist-get content :title)
        (plist-get content :name)
        (plist-get content :code)
        (format "%S" content)))
   (t (format "%S" content))))

;;;; Entry metadata extraction

(defun hive-mcp-transform-entry-to-metadata (entry)
  "Convert ENTRY plist to metadata-only alist.
Returns: id, type, preview (truncated content), tags (as vector), created.
Useful for listing entries without full content."
  (let ((content (plist-get entry :content))
        (tags (plist-get entry :tags)))
    `((id . ,(plist-get entry :id))
      (type . ,(plist-get entry :type))
      (preview . ,(hive-mcp-transform-content-preview content))
      (tags . ,(if tags (apply #'vector tags) []))
      (created . ,(plist-get entry :created)))))

;;;; JSON boolean helper

(defun hive-mcp-transform-bool-for-json (value)
  "Convert VALUE to JSON-compatible boolean.
t stays t, nil becomes :false (for json-encode)."
  (if value t :false))

;;;; Error result construction

(defun hive-mcp-transform-error-result (error-key &rest extra-pairs)
  "Construct a JSON-serializable error alist.
ERROR-KEY is the error identifier string.
EXTRA-PAIRS are additional key-value pairs (alternating keys and values).

Example: (hive-mcp-transform-error-result \"not-found\" 'id id-val)"
  (let ((result `((error . ,error-key))))
    (while extra-pairs
      (let ((key (car extra-pairs))
            (val (cadr extra-pairs)))
        (push (cons key val) result))
      (setq extra-pairs (cddr extra-pairs)))
    (nreverse result)))

;;;; Stats transformation

(defun hive-mcp-transform-stats-to-alist (stats-plist keys)
  "Convert STATS-PLIST to alist extracting KEYS.
KEYS is a list of keyword symbols to extract.

Example: (hive-mcp-transform-stats-to-alist stats '(:todo :doing :review))"
  (mapcar (lambda (key)
            (cons (intern (substring (symbol-name key) 1))
                  (plist-get stats-plist key)))
          keys))

;;;; Position transformation

(defun hive-mcp-transform-position-to-alist (&optional point line column)
  "Construct position alist from POINT, LINE, COLUMN.
If not provided, uses current buffer position.
Returns alist with available fields (point, line, column)."
  (let ((result nil))
    (when column
      (push (cons 'column column) result))
    (when line
      (push (cons 'line line) result))
    (when point
      (push (cons 'point point) result))
    result))

;;;; Duplicate check result

(defun hive-mcp-transform-duplicate-result (exists entry content-hash)
  "Construct duplicate check result alist.
EXISTS is boolean (found duplicate or not).
ENTRY is the existing entry plist (or nil).
CONTENT-HASH is the computed hash string."
  `((exists . ,(hive-mcp-transform-bool-for-json exists))
    (entry . ,(when entry (hive-mcp-transform-plist-to-alist entry)))
    (content_hash . ,content-hash)))

(provide 'hive-mcp-transform)
;;; hive-mcp-transform.el ends here
