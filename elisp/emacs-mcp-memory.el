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

;;; Duration/Lifespan Support

(defconst emacs-mcp-memory-durations '(session short-term long-term permanent)
  "Valid duration categories, ordered shortest to longest.")

(defcustom emacs-mcp-memory-duration-days
  '((session . 0) (short-term . 7) (long-term . 90) (permanent . nil))
  "Days before expiration per duration. nil = never expires."
  :type '(alist :key-type symbol :value-type (choice integer (const nil)))
  :group 'emacs-mcp-memory)

(defcustom emacs-mcp-memory-default-duration 'long-term
  "Default duration for new entries."
  :type 'symbol
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
  (format-time-string "%FT%T%z"))

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

;;; Duration Functions

(defun emacs-mcp-memory--calculate-expires (duration)
  "Calculate expiration timestamp for DURATION.
Returns ISO 8601 timestamp or nil for permanent entries."
  (let ((days (alist-get duration emacs-mcp-memory-duration-days)))
    (cond
     ((null days) nil)  ; permanent - never expires
     ((= days 0) (emacs-mcp-memory--timestamp))  ; session - expires immediately
     (t (format-time-string "%FT%T%z"
                            (time-add (current-time)
                                      (days-to-time days)))))))

(defun emacs-mcp-memory--get-entry-duration (entry)
  "Get the duration of ENTRY, defaulting to `long-term' for legacy entries."
  (let ((dur (plist-get entry :duration)))
    (if dur
        (if (stringp dur) (intern dur) dur)
      'long-term)))

(defun emacs-mcp-memory--entry-expired-p (entry)
  "Return non-nil if ENTRY has expired."
  (when-let* ((expires (plist-get entry :expires)))
    (let ((expires-time (date-to-time expires)))
      (time-less-p expires-time (current-time)))))

(defun emacs-mcp-memory-set-duration (id duration &optional project-id)
  "Update entry ID to DURATION and recalculate expires.
DURATION must be one of `emacs-mcp-memory-durations'.
PROJECT-ID specifies the project (defaults to current).
Returns t if successful, nil otherwise."
  (unless (memq duration emacs-mcp-memory-durations)
    (error "Invalid duration: %s. Must be one of %s"
           duration emacs-mcp-memory-durations))
  (emacs-mcp-memory-update id
                           (list :duration (symbol-name duration)
                                 :expires (emacs-mcp-memory--calculate-expires duration))
                           project-id))

(defun emacs-mcp-memory-promote (id &optional project-id)
  "Promote entry ID to next longer duration in the hierarchy.
Returns new duration symbol, or nil if already permanent or not found."
  (when-let* ((entry (emacs-mcp-memory-get id project-id)))
    (let* ((current (emacs-mcp-memory--get-entry-duration entry))
           (pos (seq-position emacs-mcp-memory-durations current))
           (next-pos (when pos (1+ pos))))
      (when (and next-pos (< next-pos (length emacs-mcp-memory-durations)))
        (let ((new-duration (nth next-pos emacs-mcp-memory-durations)))
          (emacs-mcp-memory-set-duration id new-duration project-id)
          new-duration)))))

(defun emacs-mcp-memory-demote (id &optional project-id)
  "Demote entry ID to next shorter duration in the hierarchy.
Returns new duration symbol, or nil if already session or not found."
  (when-let* ((entry (emacs-mcp-memory-get id project-id)))
    (let* ((current (emacs-mcp-memory--get-entry-duration entry))
           (pos (seq-position emacs-mcp-memory-durations current))
           (prev-pos (when (and pos (> pos 0)) (1- pos))))
      (when prev-pos
        (let ((new-duration (nth prev-pos emacs-mcp-memory-durations)))
          (emacs-mcp-memory-set-duration id new-duration project-id)
          new-duration)))))

(defun emacs-mcp-memory-cleanup-expired (&optional project-id)
  "Delete all expired entries from PROJECT-ID.
Returns the count of deleted entries."
  (let* ((pid (or project-id (emacs-mcp-memory--project-id)))
         (count 0))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let* ((data (emacs-mcp-memory--get-data pid type))
             (new-data (seq-remove
                        (lambda (entry)
                          (when (emacs-mcp-memory--entry-expired-p entry)
                            (setq count (1+ count))
                            t))
                        data)))
        (when (< (length new-data) (length data))
          (emacs-mcp-memory--set-data pid type new-data))))
    count))

(defun emacs-mcp-memory-query-expiring (days &optional project-id)
  "Return entries expiring within DAYS from PROJECT-ID.
Does not include already-expired or permanent entries."
  (let* ((pid (or project-id (emacs-mcp-memory--project-id)))
         (threshold (time-add (current-time) (days-to-time days)))
         (results '()))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (dolist (entry (emacs-mcp-memory--get-data pid type))
        (when-let* ((expires (plist-get entry :expires)))
          (let ((expires-time (date-to-time expires)))
            ;; Include if: not yet expired AND expires within threshold
            (when (and (time-less-p (current-time) expires-time)
                       (time-less-p expires-time threshold))
              (push entry results))))))
    ;; Sort by expiration date (soonest first)
    (sort results
          (lambda (a b)
            (time-less-p (date-to-time (plist-get a :expires))
                         (date-to-time (plist-get b :expires)))))))

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
DATA is expected to be a list of plists.
Uses UTF-8 encoding to avoid interactive coding system prompts."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert (json-serialize (emacs-mcp-memory--convert-for-json data)
                              :null-object :null
                              :false-object :false)))))

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

(defun emacs-mcp-memory-add (type content &optional tags project-id duration)
  "Add a memory entry of TYPE with CONTENT.
TYPE is one of: note, snippet, convention, decision, conversation.
CONTENT is a string or plist.
TAGS is optional list of strings.
PROJECT-ID defaults to current project or global.
DURATION is optional lifespan (symbol from `emacs-mcp-memory-durations').
Defaults to `emacs-mcp-memory-default-duration'."
  (let* ((pid (or project-id (emacs-mcp-memory--project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         (dur (or duration emacs-mcp-memory-default-duration))
         (entry (list :id (emacs-mcp-memory--generate-id)
                      :type type-str
                      :content content
                      :tags (or tags '())
                      :duration (symbol-name dur)
                      :expires (emacs-mcp-memory--calculate-expires dur)
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

(defun emacs-mcp-memory-query (type &optional tags project-id limit duration)
  "Query memories by TYPE and optional TAGS.
PROJECT-ID specifies the project (defaults to current).
Returns list of matching entries, most recent first.
LIMIT caps the number of results.
DURATION filters by lifespan category (symbol from `emacs-mcp-memory-durations').
Entries without :duration are treated as `long-term' for backwards compatibility."
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
    ;; Filter by duration if provided
    (when duration
      (let ((dur-str (symbol-name duration)))
        (setq results
              (seq-filter
               (lambda (entry)
                 (let ((entry-dur (or (plist-get entry :duration) "long-term")))
                   (string= entry-dur dur-str)))
               results))))
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
