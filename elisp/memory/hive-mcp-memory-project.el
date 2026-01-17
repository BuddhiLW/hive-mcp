;;; hive-mcp-memory-project.el --- Project identification for memory system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Project identification and configuration for hive-mcp-memory.
;; Contains: EDN parsing, project ID resolution, config file handling.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles project identification
;; - Open/Closed: Config sources can be extended without modification
;;
;; Resolution order for project ID:
;;   1. Stable :project-id from .hive-project.edn (survives renames)
;;   2. Fallback: SHA1 hash of absolute path (filesystem-safe)

;;; Code:

(require 'project)

;;;; Customization

(defcustom hive-mcp-memory-project-config-file ".hive-project.edn"
  "Filename for project-specific hive-mcp configuration.
This file should contain an EDN map with at least :project-id key.
Example: {:project-id \"my-project\" :aliases [\"old-name\"]}"
  :type 'string
  :group 'hive-mcp-memory)

;;;; Internal Variables

(defvar hive-mcp-memory-project--config-cache (make-hash-table :test 'equal)
  "Cache for project config files. Maps project-root to parsed config.")

;;;; EDN Parsing

(defun hive-mcp-memory-project--parse-edn-string (str)
  "Parse a simple EDN string STR into elisp.
Supports: strings, keywords, numbers, vectors, maps.
This is a minimal parser for .hive-project.edn files."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (hive-mcp-memory-project--parse-edn-value)))

(defun hive-mcp-memory-project--parse-edn-value ()
  "Parse an EDN value at point."
  ;; Skip whitespace and comments
  (while (progn
           (skip-chars-forward " \t\n\r")
           (when (looking-at ";")
             (forward-line 1)
             t)))
  (cond
   ;; String
   ((looking-at "\"")
    (let ((start (point)))
      (forward-char 1)
      (while (not (looking-at "\""))
        (if (looking-at "\\\\")
            (forward-char 2)
          (forward-char 1)))
      (forward-char 1)
      (car (read-from-string (buffer-substring start (point))))))
   ;; Keyword
   ((looking-at ":\\([a-zA-Z0-9_-]+\\)")
    (let ((kw (match-string 1)))
      (goto-char (match-end 0))
      (intern (concat ":" kw))))
   ;; Number
   ((looking-at "-?[0-9]+\\(\\.[0-9]+\\)?")
    (let ((num (match-string 0)))
      (goto-char (match-end 0))
      (string-to-number num)))
   ;; Vector
   ((looking-at "\\[")
    (forward-char 1)
    (let (items)
      (while (not (looking-at "\\]"))
        (push (hive-mcp-memory-project--parse-edn-value) items)
        (skip-chars-forward " \t\n\r,"))
      (forward-char 1)
      (nreverse items)))
   ;; Map
   ((looking-at "{")
    (forward-char 1)
    (let (pairs)
      (while (not (looking-at "}"))
        (let ((key (hive-mcp-memory-project--parse-edn-value))
              (val (hive-mcp-memory-project--parse-edn-value)))
          (push (cons key val) pairs))
        (skip-chars-forward " \t\n\r,"))
      (forward-char 1)
      (nreverse pairs)))
   ;; nil/true/false
   ((looking-at "nil") (goto-char (match-end 0)) nil)
   ((looking-at "true") (goto-char (match-end 0)) t)
   ((looking-at "false") (goto-char (match-end 0)) nil)
   ;; Symbol (fallback)
   ((looking-at "[a-zA-Z][a-zA-Z0-9_-]*")
    (let ((sym (match-string 0)))
      (goto-char (match-end 0))
      (intern sym)))
   (t nil)))

;;;; Project Root Functions

(defun hive-mcp-memory-project-get-root (&optional directory)
  "Get project root for DIRECTORY, or current project if nil.
When DIRECTORY is provided, finds the project containing that path.
Returns nil if not in a project."
  (if directory
      ;; Use provided directory to find project
      (let ((default-directory (file-name-as-directory (expand-file-name directory))))
        (when-let* ((proj (project-current nil default-directory)))
          (project-root proj)))
    ;; Fallback to current buffer's project
    (when-let* ((proj (project-current)))
      (project-root proj))))

(defun hive-mcp-memory-project-get-name (&optional directory)
  "Get project name (directory basename) for DIRECTORY, or current project.
Returns nil if not in a project."
  (when-let* ((root (hive-mcp-memory-project-get-root directory)))
    (file-name-nondirectory (directory-file-name root))))

;;;; Config File Functions

(defun hive-mcp-memory-project--read-config (&optional project-root)
  "Read and parse .hive-project.edn from PROJECT-ROOT.
Returns alist of config values, or nil if file doesn't exist.
Caches result per project root."
  (let* ((root (or project-root (hive-mcp-memory-project-get-root)))
         (cached (gethash root hive-mcp-memory-project--config-cache 'not-found)))
    (if (not (eq cached 'not-found))
        cached
      (let* ((config-path (when root
                            (expand-file-name hive-mcp-memory-project-config-file root)))
             (config (when (and config-path (file-exists-p config-path))
                       (condition-case nil
                           (hive-mcp-memory-project--parse-edn-string
                            (with-temp-buffer
                              (insert-file-contents config-path)
                              (buffer-string)))
                         (error nil)))))
        (puthash root config hive-mcp-memory-project--config-cache)
        config))))

(defun hive-mcp-memory-project--get-stable-id (&optional project-root)
  "Get stable project ID from config file at PROJECT-ROOT.
Returns the :project-id value from .hive-project.edn, or nil if not found."
  (when-let* ((config (hive-mcp-memory-project--read-config project-root))
              (project-id (cdr (assoc :project-id config))))
    (if (stringp project-id)
        project-id
      (symbol-name project-id))))

;;;; Project ID Resolution

(defun hive-mcp-memory-project-id (&optional directory-or-root)
  "Return unique ID for project containing DIRECTORY-OR-ROOT.
DIRECTORY-OR-ROOT can be either a project root path or any path within a project.
If nil, uses the current buffer's project.
Resolution order:
  1. Stable :project-id from .hive-project.edn (survives renames)
  2. Fallback: SHA1 hash of absolute path (filesystem-safe)
Returns \"global\" if not in a project."
  (let ((root (or (when directory-or-root
                    ;; First try as-is (if it's already a project root)
                    ;; then find containing project
                    (or (when (file-directory-p directory-or-root)
                          (let ((default-directory (file-name-as-directory
                                                    (expand-file-name directory-or-root))))
                            (when-let* ((proj (project-current nil default-directory)))
                              (project-root proj))))
                        directory-or-root))
                  (hive-mcp-memory-project-get-root))))
    (if root
        (or (hive-mcp-memory-project--get-stable-id root)
            (substring (sha1 (expand-file-name root)) 0 16))
      "global")))

(defun hive-mcp-memory-project-id-hash (&optional project-root)
  "Return SHA1 hash ID for PROJECT-ROOT (ignores config).
Use this for migration when you need the path-based hash."
  (let ((root (or project-root (hive-mcp-memory-project-get-root))))
    (if root
        (substring (sha1 (expand-file-name root)) 0 16)
      "global")))

;;;; Cache Management

(defun hive-mcp-memory-project-clear-config-cache ()
  "Clear the project config cache. Call after editing .hive-project.edn."
  (interactive)
  (clrhash hive-mcp-memory-project--config-cache)
  (message "Cleared hive-mcp project config cache"))

(provide 'hive-mcp-memory-project)
;;; hive-mcp-memory-project.el ends here
