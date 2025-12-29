;;; emacs-mcp-memory.el --- Persistent memory for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of emacs-mcp.

;;; Commentary:
;;
;; Persistent memory system for emacs-mcp.  Stores notes, snippets,
;; conventions, decisions, and conversation history per-project.
;;
;; Storage is JSON-based in ~/.emacs.d/emacs-mcp/ with project isolation
;; via SHA1 hash of project root path.
;;

;;; Code:

(require 'json)
(require 'project)
(require 'seq)

;;; Customization

(defgroup emacs-mcp-memory nil
  "Memory and persistence settings for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-memory-")

(defcustom emacs-mcp-memory-directory
  (expand-file-name "emacs-mcp" user-emacs-directory)
  "Directory for emacs-mcp persistent storage."
  :type 'directory
  :group 'emacs-mcp-memory)

(defcustom emacs-mcp-conversation-max-entries 100
  "Maximum conversation log entries per project."
  :type 'integer
  :group 'emacs-mcp-memory)

(defcustom emacs-mcp-memory-auto-save t
  "Automatically save memory on changes."
  :type 'boolean
  :group 'emacs-mcp-memory)

;;; Internal Variables

(defvar emacs-mcp-memory--cache (make-hash-table :test 'equal)
  "In-memory cache of loaded project data.")

(defvar emacs-mcp-memory-add-hook nil
  "Hook run after adding memory entry.  Args: TYPE ENTRY PROJECT-ID.")

;;; Utility Functions

(defun emacs-mcp-memory--generate-id ()
  "Generate a unique ID for memory entries."
  (format "%s-%s"
          (format-time-string "%Y%m%d%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 8)))

(defun emacs-mcp-memory--timestamp ()
  "Return current ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun emacs-mcp-memory--project-id (&optional project-root)
  "Return unique ID for PROJECT-ROOT (defaults to current project).
Uses SHA1 hash of absolute path for filesystem-safe naming."
  (let ((root (or project-root (emacs-mcp-memory--get-project-root))))
    (if root
        (substring (sha1 (expand-file-name root)) 0 16)
      "global")))

(defun emacs-mcp-memory--get-project-root ()
  "Get current project root, or nil if not in a project."
  (when-let* ((proj (project-current)))
    (project-root proj)))

;;; File Operations

(defun emacs-mcp-memory--project-dir (project-id)
  "Return directory path for PROJECT-ID."
  (if (string= project-id "global")
      (expand-file-name "global" emacs-mcp-memory-directory)
    (expand-file-name (concat "projects/" project-id)
                      emacs-mcp-memory-directory)))

(defun emacs-mcp-memory--file-path (project-id type)
  "Return file path for PROJECT-ID and memory TYPE."
  (expand-file-name (format "%s.json" type)
                    (emacs-mcp-memory--project-dir project-id)))

(defun emacs-mcp-memory--read-json-file (path)
  "Read JSON file at PATH, return parsed data or empty list."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (if (= (point-min) (point-max))
            '()
          (json-parse-buffer :object-type 'plist :array-type 'list)))
    '()))

(defun emacs-mcp-memory--plist-to-alist (plist)
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
                              (emacs-mcp-memory--plist-to-alist val))
                             ;; List of items -> convert to vector
                             ((and (listp val) val)
                              (apply #'vector
                                     (mapcar (lambda (v)
                                               (if (and (listp v) (keywordp (car-safe v)))
                                                   (emacs-mcp-memory--plist-to-alist v)
                                                 v))
                                             val)))
                             ;; Empty list or nil
                             ((null val) [])
                             ;; Scalar value
                             (t val))))
        (push (cons key-sym val-converted) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun emacs-mcp-memory--convert-for-json (data)
  "Convert DATA (list of plists) to format suitable for `json-serialize'.
Returns a vector of alists (`json-serialize' requires vectors for arrays)."
  (apply #'vector (mapcar #'emacs-mcp-memory--plist-to-alist data)))

(defun emacs-mcp-memory--write-json-file (path data)
  "Write DATA as JSON to PATH.
DATA is expected to be a list of plists."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert (json-serialize (emacs-mcp-memory--convert-for-json data)
                            :null-object :null
                            :false-object :false))))

;;; Cache Management

(defun emacs-mcp-memory--cache-key (project-id type)
  "Return cache key for PROJECT-ID and TYPE."
  (format "%s:%s" project-id type))

(defun emacs-mcp-memory--get-data (project-id type)
  "Get memory data for PROJECT-ID and TYPE, loading from disk if needed."
  (let ((key (emacs-mcp-memory--cache-key project-id type)))
    (or (gethash key emacs-mcp-memory--cache)
        (let ((data (emacs-mcp-memory--read-json-file
                     (emacs-mcp-memory--file-path project-id type))))
          (puthash key data emacs-mcp-memory--cache)
          data))))

(defun emacs-mcp-memory--set-data (project-id type data)
  "Set memory DATA for PROJECT-ID and TYPE."
  (let ((key (emacs-mcp-memory--cache-key project-id type)))
    (puthash key data emacs-mcp-memory--cache)
    (when emacs-mcp-memory-auto-save
      (emacs-mcp-memory--write-json-file
       (emacs-mcp-memory--file-path project-id type)
       data))))

;;; CRUD Operations

(defun emacs-mcp-memory-add (type content &optional tags project-id)
  "Add a memory entry of TYPE with CONTENT.
TYPE is one of: note, snippet, convention, decision, conversation.
CONTENT is a string or plist.
TAGS is optional list of strings.
PROJECT-ID defaults to current project or global."
  (let* ((pid (or project-id (emacs-mcp-memory--project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         (entry (list :id (emacs-mcp-memory--generate-id)
                      :type type-str
                      :content content
                      :tags (or tags '())
                      :created (emacs-mcp-memory--timestamp)
                      :updated (emacs-mcp-memory--timestamp)))
         (data (emacs-mcp-memory--get-data pid type-str)))
    ;; For conversations, enforce ring buffer limit
    (when (string= type-str "conversation")
      (when (>= (length data) emacs-mcp-conversation-max-entries)
        (setq data (seq-take data (1- emacs-mcp-conversation-max-entries)))))
    ;; Prepend new entry
    (setq data (cons entry data))
    (emacs-mcp-memory--set-data pid type-str data)
    ;; Run hooks
    (run-hook-with-args 'emacs-mcp-memory-add-hook type entry pid)
    entry))

(defun emacs-mcp-memory-get (id &optional project-id)
  "Retrieve memory entry by ID from PROJECT-ID."
  (let ((pid (or project-id (emacs-mcp-memory--project-id))))
    (catch 'found
      (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
        (dolist (entry (emacs-mcp-memory--get-data pid type))
          (when (string= (plist-get entry :id) id)
            (throw 'found entry))))
      nil)))

(defun emacs-mcp-memory-query (type &optional tags project-id limit)
  "Query memories by TYPE and optional TAGS.
PROJECT-ID specifies the project (defaults to current).
Returns list of matching entries, most recent first.
LIMIT caps the number of results."
  (let* ((pid (or project-id (emacs-mcp-memory--project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         (data (emacs-mcp-memory--get-data pid type-str))
         (results data))
    ;; Filter by tags if provided
    (when tags
      (setq results
            (seq-filter
             (lambda (entry)
               (let ((entry-tags (plist-get entry :tags)))
                 (seq-every-p (lambda (tag) (member tag entry-tags)) tags)))
             results)))
    ;; Apply limit
    (if (and limit (> (length results) limit))
        (seq-take results limit)
      results)))

(defun emacs-mcp-memory-update (id updates &optional project-id)
  "Update memory entry ID with UPDATES plist.
PROJECT-ID specifies the project (defaults to current)."
  (let* ((pid (or project-id (emacs-mcp-memory--project-id)))
         (found nil))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let ((data (emacs-mcp-memory--get-data pid type)))
        (setq data
              (mapcar
               (lambda (entry)
                 (if (string= (plist-get entry :id) id)
                     (progn
                       (setq found t)
                       (let ((updated entry))
                         ;; Apply updates
                         (while updates
                           (setq updated (plist-put updated (car updates) (cadr updates)))
                           (setq updates (cddr updates)))
                         (plist-put updated :updated (emacs-mcp-memory--timestamp))))
                   entry))
               data))
        (when found
          (emacs-mcp-memory--set-data pid type data))))
    found))

(defun emacs-mcp-memory-delete (id &optional project-id)
  "Delete memory entry by ID.
PROJECT-ID specifies the project (defaults to current)."
  (let* ((pid (or project-id (emacs-mcp-memory--project-id)))
         (deleted nil))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let* ((data (emacs-mcp-memory--get-data pid type))
             (new-data (seq-remove
                        (lambda (entry)
                          (when (string= (plist-get entry :id) id)
                            (setq deleted t)))
                        data)))
        (when deleted
          (emacs-mcp-memory--set-data pid type new-data))))
    deleted))

;;; Convenience Functions

(defun emacs-mcp-memory-add-note (content &optional tags)
  "Add a note with CONTENT to current project memory.
TAGS is an optional list of tag strings."
  (emacs-mcp-memory-add 'note content tags))

(defun emacs-mcp-memory-add-snippet (name code &optional language tags)
  "Add a code snippet with NAME and CODE.
LANGUAGE specifies the programming language.
TAGS is an optional list of tag strings."
  (emacs-mcp-memory-add 'snippet
                        (list :name name
                              :code code
                              :language (or language "unknown"))
                        tags))

(defun emacs-mcp-memory-add-convention (description &optional example)
  "Add a project convention with DESCRIPTION.
EXAMPLE provides an optional code example."
  (emacs-mcp-memory-add 'convention
                        (list :description description
                              :example example)))

(defun emacs-mcp-memory-add-decision (title rationale &optional alternatives)
  "Add an architecture decision record with TITLE and RATIONALE.
ALTERNATIVES lists other options that were considered."
  (emacs-mcp-memory-add 'decision
                        (list :title title
                              :rationale rationale
                              :alternatives alternatives)))

(defun emacs-mcp-memory-log-conversation (role content)
  "Log a conversation entry with ROLE and CONTENT.
ROLE should be `user' or `assistant'."
  (emacs-mcp-memory-add 'conversation
                        (list :role (if (symbolp role) (symbol-name role) role)
                              :content content)))

;;; Project Context

(defun emacs-mcp-memory-get-project-context ()
  "Return full project context as plist for Claude.
Includes notes, conventions, recent decisions, relevant snippets."
  (let ((pid (emacs-mcp-memory--project-id)))
    (list
     :project-id pid
     :project-root (emacs-mcp-memory--get-project-root)
     :notes (emacs-mcp-memory-query 'note nil pid 10)
     :conventions (emacs-mcp-memory-query 'convention nil pid)
     :recent-decisions (emacs-mcp-memory-query 'decision nil pid 5)
     :snippets (emacs-mcp-memory-query 'snippet nil pid 20))))

;;; Initialization

(defun emacs-mcp-memory-init ()
  "Initialize memory system, create directories if needed."
  (make-directory (expand-file-name "global" emacs-mcp-memory-directory) t)
  (make-directory (expand-file-name "projects" emacs-mcp-memory-directory) t)
  (clrhash emacs-mcp-memory--cache))

;;; Save/Load

(defun emacs-mcp-memory-save (&optional project-id)
  "Save all cached memory for PROJECT-ID to disk."
  (let ((pid (or project-id (emacs-mcp-memory--project-id))))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let ((key (emacs-mcp-memory--cache-key pid type)))
        (when-let* ((data (gethash key emacs-mcp-memory--cache)))
          (emacs-mcp-memory--write-json-file
           (emacs-mcp-memory--file-path pid type)
           data))))))

(defun emacs-mcp-memory-save-all ()
  "Save all cached memory to disk."
  (maphash
   (lambda (key data)
     (let* ((parts (split-string key ":"))
            (pid (car parts))
            (type (cadr parts)))
       (emacs-mcp-memory--write-json-file
        (emacs-mcp-memory--file-path pid type)
        data)))
   emacs-mcp-memory--cache))

(provide 'emacs-mcp-memory)
;;; emacs-mcp-memory.el ends here
