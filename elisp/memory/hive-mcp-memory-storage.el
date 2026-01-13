;;; hive-mcp-memory-storage.el --- Storage and caching for memory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Storage layer for hive-mcp-memory.
;; Handles JSON file I/O, plist/alist conversion, and caching.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles storage/persistence
;; - Dependency Inversion: CRUD layer depends on this abstraction

;;; Code:

(require 'json)

;;;; Customization

(defcustom hive-mcp-memory-storage-directory
  (expand-file-name "hive-mcp" user-emacs-directory)
  "Directory for hive-mcp persistent storage."
  :type 'directory
  :group 'hive-mcp-memory)

(defcustom hive-mcp-memory-storage-auto-save t
  "Automatically save memory on changes."
  :type 'boolean
  :group 'hive-mcp-memory)

;;;; Internal Variables

(defvar hive-mcp-memory-storage--cache (make-hash-table :test 'equal)
  "In-memory cache of loaded project data.")

;;;; Path Functions

(defun hive-mcp-memory-storage-project-dir (project-id)
  "Return directory path for PROJECT-ID."
  (if (string= project-id "global")
      (expand-file-name "global" hive-mcp-memory-storage-directory)
    (expand-file-name (concat "projects/" project-id)
                      hive-mcp-memory-storage-directory)))

(defun hive-mcp-memory-storage-file-path (project-id type)
  "Return file path for PROJECT-ID and memory TYPE."
  (expand-file-name (format "%s.json" type)
                    (hive-mcp-memory-storage-project-dir project-id)))

;;;; JSON Format Conversion

(defun hive-mcp-memory-storage--plist-to-alist (plist)
  "Convert PLIST with keyword keys to alist with symbol keys.
Recursively converts nested plists.
Uses symbol keys (not strings) as required by `json-serialize'.
Lists are converted to vectors for JSON array serialization."
  (let (alist)
    (while plist
      (let* ((key (car plist))
             (val (cadr plist))
             ;; Convert :keyword to symbol (e.g., :id -> id)
             (key-sym (if (keywordp key)
                          (intern (substring (symbol-name key) 1))
                        key))
             (val-converted (cond
                             ;; Nested plist (starts with keyword)
                             ((and (listp val) (keywordp (car-safe val)))
                              (hive-mcp-memory-storage--plist-to-alist val))
                             ;; List of items -> convert to vector
                             ((and (listp val) val)
                              (apply #'vector
                                     (mapcar (lambda (v)
                                               (if (and (listp v) (keywordp (car-safe v)))
                                                   (hive-mcp-memory-storage--plist-to-alist v)
                                                 v))
                                             val)))
                             ;; Empty list or nil
                             ((null val) [])
                             ;; Scalar value
                             (t val))))
        (push (cons key-sym val-converted) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun hive-mcp-memory-storage--convert-for-json (data)
  "Convert DATA (list of plists) to format suitable for `json-serialize'.
Returns a vector of alists (`json-serialize' requires vectors for arrays)."
  (apply #'vector (mapcar #'hive-mcp-memory-storage--plist-to-alist data)))

;;;; File I/O

(defun hive-mcp-memory-storage-read-json (path)
  "Read JSON file at PATH, return parsed data or empty list."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (if (= (point-min) (point-max))
            '()
          (json-parse-buffer :object-type 'plist :array-type 'list)))
    '()))

(defun hive-mcp-memory-storage-write-json (path data)
  "Write DATA as JSON to PATH.
DATA is expected to be a list of plists.
Uses UTF-8 encoding to avoid interactive coding system prompts."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert (json-serialize (hive-mcp-memory-storage--convert-for-json data)
                              :null-object :null
                              :false-object :false)))))

;;;; Cache Management

(defun hive-mcp-memory-storage--cache-key (project-id type)
  "Return cache key for PROJECT-ID and TYPE."
  (format "%s:%s" project-id type))

(defun hive-mcp-memory-storage-get-data (project-id type)
  "Get memory data for PROJECT-ID and TYPE, loading from disk if needed."
  (let ((key (hive-mcp-memory-storage--cache-key project-id type)))
    (or (gethash key hive-mcp-memory-storage--cache)
        (let ((data (hive-mcp-memory-storage-read-json
                     (hive-mcp-memory-storage-file-path project-id type))))
          (puthash key data hive-mcp-memory-storage--cache)
          data))))

(defun hive-mcp-memory-storage-set-data (project-id type data)
  "Set memory DATA for PROJECT-ID and TYPE."
  (let ((key (hive-mcp-memory-storage--cache-key project-id type)))
    (puthash key data hive-mcp-memory-storage--cache)
    (when hive-mcp-memory-storage-auto-save
      (hive-mcp-memory-storage-write-json
       (hive-mcp-memory-storage-file-path project-id type)
       data))))

(defun hive-mcp-memory-storage-clear-cache (&optional project-id)
  "Clear cache for PROJECT-ID or all if nil."
  (if project-id
      (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
        (remhash (hive-mcp-memory-storage--cache-key project-id type)
                 hive-mcp-memory-storage--cache))
    (clrhash hive-mcp-memory-storage--cache)))

;;;; Initialization

(defun hive-mcp-memory-storage-init ()
  "Initialize storage system, create directories if needed."
  (make-directory (expand-file-name "global" hive-mcp-memory-storage-directory) t)
  (make-directory (expand-file-name "projects" hive-mcp-memory-storage-directory) t)
  (clrhash hive-mcp-memory-storage--cache))

(provide 'hive-mcp-memory-storage)
;;; hive-mcp-memory-storage.el ends here
