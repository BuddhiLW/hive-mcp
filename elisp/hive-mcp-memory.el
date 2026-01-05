;;; hive-mcp-memory.el --- Persistent memory for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Persistent memory system for hive-mcp.  Stores notes, snippets,
;; conventions, decisions, and conversation history per-project.
;;
;; Storage is JSON-based in ~/.emacs.d/hive-mcp/ with project isolation
;; via SHA1 hash of project root path.
;;

;;; Code:

(require 'json)
(require 'project)
(require 'seq)

;; Compatibility: plistp was added in Emacs 29
(unless (fboundp 'plistp)
  (defun plistp (object)
    "Return non-nil if OBJECT is a plist."
    (and (listp object)
         (or (null object)
             (and (keywordp (car object))
                  (plistp (cddr object)))))))

;;; Customization

(defgroup hive-mcp-memory nil
  "Memory and persistence settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-memory-")

(defcustom hive-mcp-memory-directory
  (expand-file-name "hive-mcp" user-emacs-directory)
  "Directory for hive-mcp persistent storage."
  :type 'directory
  :group 'hive-mcp-memory)

(defcustom hive-mcp-conversation-max-entries 100
  "Maximum conversation log entries per project."
  :type 'integer
  :group 'hive-mcp-memory)

(defcustom hive-mcp-memory-auto-save t
  "Automatically save memory on changes."
  :type 'boolean
  :group 'hive-mcp-memory)

;;; Duration/Lifespan Support

(defconst hive-mcp-memory-durations '(session short-term long-term permanent)
  "Valid duration categories, ordered shortest to longest.")

(defcustom hive-mcp-memory-duration-days
  '((session . 0) (short-term . 7) (long-term . 90) (permanent . nil))
  "Days before expiration per duration. nil = never expires."
  :type '(alist :key-type symbol :value-type (choice integer (const nil)))
  :group 'hive-mcp-memory)

(defcustom hive-mcp-memory-default-duration 'long-term
  "Default duration for new entries."
  :type 'symbol
  :group 'hive-mcp-memory)

(defcustom hive-mcp-project-config-file ".hive-project.edn"
  "Filename for project-specific hive-mcp configuration.
This file should contain an EDN map with at least :project-id key.
Example: {:project-id \"my-project\" :aliases [\"old-name\"]}"
  :type 'string
  :group 'hive-mcp-memory)

;;; Internal Variables

(defvar hive-mcp-memory--cache (make-hash-table :test 'equal)
  "In-memory cache of loaded project data.")

(defvar hive-mcp-memory--project-config-cache (make-hash-table :test 'equal)
  "Cache for project config files. Maps project-root to parsed config.")

(defvar hive-mcp-memory-add-hook nil
  "Hook run after adding memory entry.  Args: TYPE ENTRY PROJECT-ID.")

;;; Project Config Functions

(defun hive-mcp-memory--parse-edn-string (str)
  "Parse a simple EDN string STR into elisp.
Supports: strings, keywords, numbers, vectors, maps.
This is a minimal parser for .hive-project.edn files."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (hive-mcp-memory--parse-edn-value)))

(defun hive-mcp-memory--parse-edn-value ()
  "Parse an EDN value at point."
  (skip-chars-forward " \t\n\r")
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
        (push (hive-mcp-memory--parse-edn-value) items)
        (skip-chars-forward " \t\n\r,"))
      (forward-char 1)
      (nreverse items)))
   ;; Map
   ((looking-at "{")
    (forward-char 1)
    (let (pairs)
      (while (not (looking-at "}"))
        (let ((key (hive-mcp-memory--parse-edn-value))
              (val (hive-mcp-memory--parse-edn-value)))
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

(defun hive-mcp-memory--read-project-config (&optional project-root)
  "Read and parse .hive-project.edn from PROJECT-ROOT.
Returns alist of config values, or nil if file doesn't exist.
Caches result per project root."
  (let* ((root (or project-root (hive-mcp-memory--get-project-root)))
         (cached (gethash root hive-mcp-memory--project-config-cache 'not-found)))
    (if (not (eq cached 'not-found))
        cached
      (let* ((config-path (when root
                            (expand-file-name hive-mcp-project-config-file root)))
             (config (when (and config-path (file-exists-p config-path))
                       (condition-case nil
                           (hive-mcp-memory--parse-edn-string
                            (with-temp-buffer
                              (insert-file-contents config-path)
                              (buffer-string)))
                         (error nil)))))
        (puthash root config hive-mcp-memory--project-config-cache)
        config))))

(defun hive-mcp-memory--get-stable-project-id (&optional project-root)
  "Get stable project ID from config file at PROJECT-ROOT.
Returns the :project-id value from .hive-project.edn, or nil if not found."
  (when-let* ((config (hive-mcp-memory--read-project-config project-root))
              (project-id (cdr (assoc :project-id config))))
    (if (stringp project-id)
        project-id
      (symbol-name project-id))))

(defun hive-mcp-memory-clear-config-cache ()
  "Clear the project config cache.  Call after editing .hive-project.edn."
  (interactive)
  (clrhash hive-mcp-memory--project-config-cache)
  (message "Cleared hive-mcp project config cache"))

;;; Utility Functions

(defun hive-mcp-memory--generate-id ()
  "Generate a unique ID for memory entries."
  (format "%s-%s"
          (format-time-string "%Y%m%d%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 8)))

(defun hive-mcp-memory--timestamp ()
  "Return current ISO 8601 timestamp."
  (format-time-string "%FT%T%z"))

(defun hive-mcp-memory--project-id (&optional project-root)
  "Return unique ID for PROJECT-ROOT (defaults to current project).
Resolution order:
  1. Stable :project-id from .hive-project.edn (survives renames)
  2. Fallback: SHA1 hash of absolute path (filesystem-safe)
Returns \"global\" if not in a project."
  (let ((root (or project-root (hive-mcp-memory--get-project-root))))
    (if root
        (or (hive-mcp-memory--get-stable-project-id root)
            (substring (sha1 (expand-file-name root)) 0 16))
      "global")))

(defun hive-mcp-memory--project-id-hash (&optional project-root)
  "Return SHA1 hash ID for PROJECT-ROOT (ignores config).
Use this for migration when you need the path-based hash."
  (let ((root (or project-root (hive-mcp-memory--get-project-root))))
    (if root
        (substring (sha1 (expand-file-name root)) 0 16)
      "global")))

(defun hive-mcp-memory--get-project-root ()
  "Get current project root, or nil if not in a project."
  (when-let* ((proj (project-current)))
    (project-root proj)))

(defun hive-mcp-memory--get-project-name ()
  "Get current project name (directory basename), or nil if not in a project."
  (when-let* ((root (hive-mcp-memory--get-project-root)))
    (file-name-nondirectory (directory-file-name root))))

;;; Scope System
;;
;; Scopes allow layered memory isolation:
;;   scope:global         - universal (SOLID, DDD, etc.)
;;   scope:domain:<name>  - cross-project within domain
;;   scope:project:<name> - single project only
;;
;; Query behavior: returns union of applicable scopes

(defun hive-mcp-memory--make-scope-tag (level &optional name)
  "Create a scope tag for LEVEL with optional NAME.
LEVEL is one of: global, domain, project.
NAME is required for domain and project levels."
  (pcase level
    ('global "scope:global")
    ('domain (format "scope:domain:%s" name))
    ('project (format "scope:project:%s" name))
    (_ (error "Invalid scope level: %s" level))))

(defun hive-mcp-memory--parse-scope-tag (tag)
  "Parse a scope TAG into (level . name) cons.
Returns nil if TAG is not a scope tag."
  (cond
   ((string= tag "scope:global") '(global . nil))
   ((string-prefix-p "scope:domain:" tag)
    (cons 'domain (substring tag (length "scope:domain:"))))
   ((string-prefix-p "scope:project:" tag)
    (cons 'project (substring tag (length "scope:project:"))))
   (t nil)))

(defun hive-mcp-memory--has-scope-tag-p (tags)
  "Return non-nil if TAGS contains any scope tag."
  (seq-some (lambda (tag) (string-prefix-p "scope:" tag)) tags))

(defun hive-mcp-memory--inject-project-scope (tags)
  "Inject current project scope into TAGS if no scope present.
Returns modified tags list."
  (if (hive-mcp-memory--has-scope-tag-p tags)
      tags  ; Already has scope
    (if-let* ((project-name (hive-mcp-memory--get-project-name)))
        (cons (hive-mcp-memory--make-scope-tag 'project project-name) tags)
      tags)))  ; Not in project, leave as-is (global)

(defun hive-mcp-memory--applicable-scope-tags (&optional project-name domain-name)
  "Return list of scope tags applicable to current context.
PROJECT-NAME defaults to current project.
DOMAIN-NAME is optional domain filter.
Always includes scope:global."
  (let ((tags (list "scope:global")))
    (when domain-name
      (push (hive-mcp-memory--make-scope-tag 'domain domain-name) tags))
    (when-let* ((proj (or project-name (hive-mcp-memory--get-project-name))))
      (push (hive-mcp-memory--make-scope-tag 'project proj) tags))
    tags))

(defun hive-mcp-memory--entry-matches-scope-p (entry scope-tags)
  "Return non-nil if ENTRY matches any of SCOPE-TAGS.
Entries without scope tags are treated as global."
  (let ((entry-tags (plist-get entry :tags)))
    (if (hive-mcp-memory--has-scope-tag-p entry-tags)
        ;; Entry has scope - check if it matches any applicable scope
        (seq-some (lambda (scope-tag)
                    (seq-contains-p entry-tags scope-tag #'string=))
                  scope-tags)
      ;; No scope tag = global, always matches
      t)))

;;; Duration Functions

(defun hive-mcp-memory--calculate-expires (duration)
  "Calculate expiration timestamp for DURATION.
Returns ISO 8601 timestamp or nil for permanent entries."
  (let ((days (alist-get duration hive-mcp-memory-duration-days)))
    (cond
     ((null days) nil)  ; permanent - never expires
     ((= days 0) (hive-mcp-memory--timestamp))  ; session - expires immediately
     (t (format-time-string "%FT%T%z"
                            (time-add (current-time)
                                      (days-to-time days)))))))

(defun hive-mcp-memory--get-entry-duration (entry)
  "Get the duration of ENTRY, defaulting to `long-term' for legacy entries."
  (let ((dur (plist-get entry :duration)))
    (if dur
        (if (stringp dur) (intern dur) dur)
      'long-term)))

(defun hive-mcp-memory--entry-expired-p (entry)
  "Return non-nil if ENTRY has expired."
  (when-let* ((expires (plist-get entry :expires)))
    (let ((expires-time (date-to-time expires)))
      (time-less-p expires-time (current-time)))))

(defun hive-mcp-memory-set-duration (id duration &optional project-id)
  "Update entry ID to DURATION and recalculate expires.
DURATION must be one of `hive-mcp-memory-durations'.
PROJECT-ID specifies the project (defaults to current).
Returns t if successful, nil otherwise."
  (unless (memq duration hive-mcp-memory-durations)
    (error "Invalid duration: %s. Must be one of %s"
           duration hive-mcp-memory-durations))
  (hive-mcp-memory-update id
                           (list :duration (symbol-name duration)
                                 :expires (hive-mcp-memory--calculate-expires duration))
                           project-id))

(defun hive-mcp-memory-promote (id &optional project-id)
  "Promote entry ID to next longer duration in the hierarchy.
Returns new duration symbol, or nil if already permanent or not found."
  (when-let* ((entry (hive-mcp-memory-get id project-id)))
    (let* ((current (hive-mcp-memory--get-entry-duration entry))
           (pos (seq-position hive-mcp-memory-durations current))
           (next-pos (when pos (1+ pos))))
      (when (and next-pos (< next-pos (length hive-mcp-memory-durations)))
        (let ((new-duration (nth next-pos hive-mcp-memory-durations)))
          (hive-mcp-memory-set-duration id new-duration project-id)
          new-duration)))))

(defun hive-mcp-memory-demote (id &optional project-id)
  "Demote entry ID to next shorter duration in the hierarchy.
Returns new duration symbol, or nil if already session or not found."
  (when-let* ((entry (hive-mcp-memory-get id project-id)))
    (let* ((current (hive-mcp-memory--get-entry-duration entry))
           (pos (seq-position hive-mcp-memory-durations current))
           (prev-pos (when (and pos (> pos 0)) (1- pos))))
      (when prev-pos
        (let ((new-duration (nth prev-pos hive-mcp-memory-durations)))
          (hive-mcp-memory-set-duration id new-duration project-id)
          new-duration)))))

(defun hive-mcp-memory-cleanup-expired (&optional project-id)
  "Delete all expired entries from PROJECT-ID.
Returns the count of deleted entries."
  (let* ((pid (or project-id (hive-mcp-memory--project-id)))
         (count 0))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let* ((data (hive-mcp-memory--get-data pid type))
             (new-data (seq-remove
                        (lambda (entry)
                          (when (hive-mcp-memory--entry-expired-p entry)
                            (setq count (1+ count))
                            t))
                        data)))
        (when (< (length new-data) (length data))
          (hive-mcp-memory--set-data pid type new-data))))
    count))

(defun hive-mcp-memory-query-expiring (days &optional project-id)
  "Return entries expiring within DAYS from PROJECT-ID.
Does not include already-expired or permanent entries."
  (let* ((pid (or project-id (hive-mcp-memory--project-id)))
         (threshold (time-add (current-time) (days-to-time days)))
         (results '()))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (dolist (entry (hive-mcp-memory--get-data pid type))
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

(defun hive-mcp-memory--project-dir (project-id)
  "Return directory path for PROJECT-ID."
  (if (string= project-id "global")
      (expand-file-name "global" hive-mcp-memory-directory)
    (expand-file-name (concat "projects/" project-id)
                      hive-mcp-memory-directory)))

(defun hive-mcp-memory--file-path (project-id type)
  "Return file path for PROJECT-ID and memory TYPE."
  (expand-file-name (format "%s.json" type)
                    (hive-mcp-memory--project-dir project-id)))

(defun hive-mcp-memory--read-json-file (path)
  "Read JSON file at PATH, return parsed data or empty list."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (if (= (point-min) (point-max))
            '()
          (json-parse-buffer :object-type 'plist :array-type 'list)))
    '()))

(defun hive-mcp-memory--plist-to-alist (plist)
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
                              (hive-mcp-memory--plist-to-alist val))
                             ;; List of items -> convert to vector
                             ((and (listp val) val)
                              (apply #'vector
                                     (mapcar (lambda (v)
                                               (if (and (listp v) (keywordp (car-safe v)))
                                                   (hive-mcp-memory--plist-to-alist v)
                                                 v))
                                             val)))
                             ;; Empty list or nil
                             ((null val) [])
                             ;; Scalar value
                             (t val))))
        (push (cons key-sym val-converted) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun hive-mcp-memory--convert-for-json (data)
  "Convert DATA (list of plists) to format suitable for `json-serialize'.
Returns a vector of alists (`json-serialize' requires vectors for arrays)."
  (apply #'vector (mapcar #'hive-mcp-memory--plist-to-alist data)))

(defun hive-mcp-memory--write-json-file (path data)
  "Write DATA as JSON to PATH.
DATA is expected to be a list of plists.
Uses UTF-8 encoding to avoid interactive coding system prompts."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert (json-serialize (hive-mcp-memory--convert-for-json data)
                              :null-object :null
                              :false-object :false)))))

;;; Content Deduplication

(defun hive-mcp-memory--normalize-content (content)
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

(defun hive-mcp-memory-content-hash (content)
  "Compute SHA-256 hash of normalized CONTENT.
Returns a 64-character hex string."
  (secure-hash 'sha256 (hive-mcp-memory--normalize-content content)))

(defun hive-mcp-memory-find-duplicate (type content &optional project-id)
  "Find existing entry with same content hash in TYPE.
TYPE is a symbol or string: note, snippet, convention, decision.
CONTENT is the content to check for duplicates.
PROJECT-ID defaults to current project.
Returns the existing entry if found, nil otherwise."
  (let* ((pid (or project-id (hive-mcp-memory--project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         (content-hash (hive-mcp-memory-content-hash content))
         (data (hive-mcp-memory--get-data pid type-str)))
    (seq-find (lambda (entry)
                (let ((entry-hash (plist-get entry :content-hash)))
                  (and entry-hash (string= entry-hash content-hash))))
              data)))

(defun hive-mcp-memory--merge-tags (existing-tags new-tags)
  "Merge NEW-TAGS into EXISTING-TAGS, removing duplicates.
Returns a list of unique tags."
  (seq-uniq (append existing-tags new-tags) #'string=))

;;; Cache Management

(defun hive-mcp-memory--cache-key (project-id type)
  "Return cache key for PROJECT-ID and TYPE."
  (format "%s:%s" project-id type))

(defun hive-mcp-memory--get-data (project-id type)
  "Get memory data for PROJECT-ID and TYPE, loading from disk if needed."
  (let ((key (hive-mcp-memory--cache-key project-id type)))
    (or (gethash key hive-mcp-memory--cache)
        (let ((data (hive-mcp-memory--read-json-file
                     (hive-mcp-memory--file-path project-id type))))
          (puthash key data hive-mcp-memory--cache)
          data))))

(defun hive-mcp-memory--set-data (project-id type data)
  "Set memory DATA for PROJECT-ID and TYPE."
  (let ((key (hive-mcp-memory--cache-key project-id type)))
    (puthash key data hive-mcp-memory--cache)
    (when hive-mcp-memory-auto-save
      (hive-mcp-memory--write-json-file
       (hive-mcp-memory--file-path project-id type)
       data))))

;;; CRUD Operations

(defun hive-mcp-memory-add (type content &optional tags project-id duration)
  "Add a memory entry of TYPE with CONTENT.
TYPE is one of: note, snippet, convention, decision, conversation.
CONTENT is a string or plist.
TAGS is optional list of strings.  If no scope tag (scope:global,
scope:domain:*, scope:project:*) is present, automatically injects
the current project scope.
PROJECT-ID defaults to current project or global.
DURATION is optional lifespan (symbol from `hive-mcp-memory-durations').
Defaults to `hive-mcp-memory-default-duration'.

If an entry with the same content already exists (based on content hash),
the existing entry is returned instead of creating a duplicate.
If TAGS are provided, they are merged with the existing entry's tags."
  (let* ((pid (or project-id (hive-mcp-memory--project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         ;; Auto-inject project scope if no scope tag present
         (tags-with-scope (hive-mcp-memory--inject-project-scope (or tags '())))
         (content-hash (hive-mcp-memory-content-hash content))
         ;; Check for duplicate (skip for conversations - they're logs)
         (existing (unless (string= type-str "conversation")
                     (hive-mcp-memory-find-duplicate type content pid))))
    (if existing
        ;; Duplicate found - merge tags if new ones provided
        (if (and tags-with-scope (not (seq-empty-p tags-with-scope)))
            (let ((merged-tags (hive-mcp-memory--merge-tags
                                (plist-get existing :tags) tags-with-scope)))
              (hive-mcp-memory-update (plist-get existing :id)
                                       (list :tags merged-tags) pid)
              ;; Return updated entry
              (hive-mcp-memory-get (plist-get existing :id) pid))
          ;; No new tags, just return existing
          existing)
      ;; No duplicate - create new entry
      (let* ((dur (or duration hive-mcp-memory-default-duration))
             (entry (list :id (hive-mcp-memory--generate-id)
                          :type type-str
                          :content content
                          :content-hash content-hash
                          :tags tags-with-scope
                          :duration (symbol-name dur)
                          :expires (hive-mcp-memory--calculate-expires dur)
                          :created (hive-mcp-memory--timestamp)
                          :updated (hive-mcp-memory--timestamp)))
             (data (hive-mcp-memory--get-data pid type-str)))
        ;; For conversations, enforce ring buffer limit
        (when (string= type-str "conversation")
          (when (>= (length data) hive-mcp-conversation-max-entries)
            (setq data (seq-take data (1- hive-mcp-conversation-max-entries)))))
        ;; Prepend new entry
        (setq data (cons entry data))
        (hive-mcp-memory--set-data pid type-str data)
        ;; Run hooks
        (run-hook-with-args 'hive-mcp-memory-add-hook type entry pid)
        entry))))

(defun hive-mcp-memory-get (id &optional project-id)
  "Retrieve memory entry by ID from PROJECT-ID."
  (let ((pid (or project-id (hive-mcp-memory--project-id))))
    (catch 'found
      (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
        (dolist (entry (hive-mcp-memory--get-data pid type))
          (when (string= (plist-get entry :id) id)
            (throw 'found entry))))
      nil)))

(defun hive-mcp-memory-query (type &optional tags project-id limit duration scope-filter)
  "Query memories by TYPE and optional TAGS.
PROJECT-ID specifies the project (defaults to current).
Returns list of matching entries, most recent first.
LIMIT caps the number of results.
DURATION filters by lifespan category (symbol from
\=`hive-mcp-memory-durations').
Entries without :duration are treated as long-term for
backwards compatibility.
SCOPE-FILTER controls scope filtering:
  - nil: apply automatic scope filtering (global + current project)
  - t or \\='all: return all entries regardless of scope
  - \\='global: return only scope:global entries
  - string: filter by specific scope tag"
  (let* ((pid (or project-id (hive-mcp-memory--project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         (data (hive-mcp-memory--get-data pid type-str))
         (results data))
    ;; Apply scope filtering
    (unless (or (eq scope-filter t) (eq scope-filter 'all))
      (let ((applicable-scopes
             (cond
              ((eq scope-filter 'global) (list "scope:global"))
              ((stringp scope-filter) (list scope-filter "scope:global"))
              (t (hive-mcp-memory--applicable-scope-tags)))))
        (setq results
              (seq-filter
               (lambda (entry)
                 (hive-mcp-memory--entry-matches-scope-p entry applicable-scopes))
               results))))
    ;; Filter by tags if provided (non-scope tags)
    (when tags
      (let ((non-scope-tags (seq-remove (lambda (tag) (string-prefix-p "scope:" tag)) tags)))
        (when non-scope-tags
          (setq results
                (seq-filter
                 (lambda (entry)
                   (let ((entry-tags (plist-get entry :tags)))
                     ;; Handle both list and vector tags (vectors from JSON)
                     (seq-every-p (lambda (tag) (seq-contains-p entry-tags tag #'equal))
                                  non-scope-tags)))
                 results)))))
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

(defun hive-mcp-memory-update (id updates &optional project-id)
  "Update memory entry ID with UPDATES plist.
PROJECT-ID specifies the project (defaults to current)."
  (let* ((pid (or project-id (hive-mcp-memory--project-id)))
         (found nil))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let ((data (hive-mcp-memory--get-data pid type)))
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
                         (plist-put updated :updated (hive-mcp-memory--timestamp))))
                   entry))
               data))
        (when found
          (hive-mcp-memory--set-data pid type data))))
    found))

(defun hive-mcp-memory-delete (id &optional project-id)
  "Delete memory entry by ID.
PROJECT-ID specifies the project (defaults to current)."
  (let* ((pid (or project-id (hive-mcp-memory--project-id)))
         (deleted nil))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let* ((data (hive-mcp-memory--get-data pid type))
             (new-data (seq-remove
                        (lambda (entry)
                          (when (string= (plist-get entry :id) id)
                            (setq deleted t)))
                        data)))
        (when deleted
          (hive-mcp-memory--set-data pid type new-data))))
    deleted))

;;; Audit Log Functions

(defun hive-mcp-memory-log-access (id &optional project-id)
  "Log access to memory entry ID.
Increments :access-count and updates :last-accessed timestamp.
PROJECT-ID specifies the project (defaults to current).
Returns the updated entry, or nil if not found."
  (when-let* ((entry (hive-mcp-memory-get id project-id)))
    (let* ((access-count (or (plist-get entry :access-count) 0))
           (new-count (1+ access-count)))
      (hive-mcp-memory-update id
                               (list :access-count new-count
                                     :last-accessed (hive-mcp-memory--timestamp))
                               project-id)
      ;; Return updated entry
      (hive-mcp-memory-get id project-id))))

(defun hive-mcp-memory-mark-helpful (id &optional project-id)
  "Mark memory entry ID as helpful.
Increments :helpful-count.
PROJECT-ID specifies the project (defaults to current).
Returns the updated entry, or nil if not found."
  (when-let* ((entry (hive-mcp-memory-get id project-id)))
    (let* ((helpful-count (or (plist-get entry :helpful-count) 0))
           (new-count (1+ helpful-count)))
      (hive-mcp-memory-update id
                               (list :helpful-count new-count)
                               project-id)
      ;; Return updated entry
      (hive-mcp-memory-get id project-id))))

(defun hive-mcp-memory-mark-unhelpful (id &optional project-id)
  "Mark memory entry ID as unhelpful.
Increments :unhelpful-count.
PROJECT-ID specifies the project (defaults to current).
Returns the updated entry, or nil if not found."
  (when-let* ((entry (hive-mcp-memory-get id project-id)))
    (let* ((unhelpful-count (or (plist-get entry :unhelpful-count) 0))
           (new-count (1+ unhelpful-count)))
      (hive-mcp-memory-update id
                               (list :unhelpful-count new-count)
                               project-id)
      ;; Return updated entry
      (hive-mcp-memory-get id project-id))))

(defun hive-mcp-memory-helpfulness-ratio (id &optional project-id)
  "Calculate helpfulness ratio for memory entry ID.
Returns helpful-count / (helpful-count + unhelpful-count).
Returns nil if no feedback has been given (to avoid division by zero).
PROJECT-ID specifies the project (defaults to current)."
  (when-let* ((entry (hive-mcp-memory-get id project-id)))
    (let* ((helpful (or (plist-get entry :helpful-count) 0))
           (unhelpful (or (plist-get entry :unhelpful-count) 0))
           (total (+ helpful unhelpful)))
      (if (zerop total)
          nil  ; No feedback yet
        (/ (float helpful) total)))))

;;; Convenience Functions

(defun hive-mcp-memory-add-note (content &optional tags)
  "Add a note with CONTENT to current project memory.
TAGS is an optional list of tag strings."
  (hive-mcp-memory-add 'note content tags))

(defun hive-mcp-memory-add-snippet (name code &optional language tags)
  "Add a code snippet with NAME and CODE.
LANGUAGE specifies the programming language.
TAGS is an optional list of tag strings."
  (hive-mcp-memory-add 'snippet
                        (list :name name
                              :code code
                              :language (or language "unknown"))
                        tags))

(defun hive-mcp-memory-add-convention (description &optional example)
  "Add a project convention with DESCRIPTION.
EXAMPLE provides an optional code example."
  (hive-mcp-memory-add 'convention
                        (list :description description
                              :example example)))

(defun hive-mcp-memory-add-decision (title rationale &optional alternatives)
  "Add an architecture decision record with TITLE and RATIONALE.
ALTERNATIVES lists other options that were considered."
  (hive-mcp-memory-add 'decision
                        (list :title title
                              :rationale rationale
                              :alternatives alternatives)))

(defun hive-mcp-memory-log-conversation (role content)
  "Log a conversation entry with ROLE and CONTENT.
ROLE should be `user' or `assistant'."
  (hive-mcp-memory-add 'conversation
                        (list :role (if (symbolp role) (symbol-name role) role)
                              :content content)))

;;; Project Context

(defun hive-mcp-memory-get-project-context ()
  "Return full project context as plist for Claude.
Includes notes, conventions, recent decisions, relevant snippets."
  (let ((pid (hive-mcp-memory--project-id)))
    (list
     :project-id pid
     :project-root (hive-mcp-memory--get-project-root)
     :notes (hive-mcp-memory-query 'note nil pid 10)
     :conventions (hive-mcp-memory-query 'convention nil pid)
     :recent-decisions (hive-mcp-memory-query 'decision nil pid 5)
     :snippets (hive-mcp-memory-query 'snippet nil pid 20))))

;;; Initialization

(defun hive-mcp-memory-init ()
  "Initialize memory system, create directories if needed."
  (make-directory (expand-file-name "global" hive-mcp-memory-directory) t)
  (make-directory (expand-file-name "projects" hive-mcp-memory-directory) t)
  (clrhash hive-mcp-memory--cache))

;;; Save/Load

(defun hive-mcp-memory-save (&optional project-id)
  "Save all cached memory for PROJECT-ID to disk."
  (let ((pid (or project-id (hive-mcp-memory--project-id))))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let ((key (hive-mcp-memory--cache-key pid type)))
        (when-let* ((data (gethash key hive-mcp-memory--cache)))
          (hive-mcp-memory--write-json-file
           (hive-mcp-memory--file-path pid type)
           data))))))

(defun hive-mcp-memory-save-all ()
  "Save all cached memory to disk."
  (maphash
   (lambda (key data)
     (let* ((parts (split-string key ":"))
            (pid (car parts))
            (type (cadr parts)))
       (hive-mcp-memory--write-json-file
        (hive-mcp-memory--file-path pid type)
        data)))
   hive-mcp-memory--cache))

;;; Migration Support

(defun hive-mcp-memory-migrate-project (old-project-id new-project-id &optional update-scopes)
  "Migrate memory from OLD-PROJECT-ID to NEW-PROJECT-ID.
If UPDATE-SCOPES is non-nil, also update scope tags in entries.
Returns a plist with migration statistics.

Use this when:
- Renaming a project directory (old ID was path-based hash)
- Consolidating memory from multiple project IDs
- Moving to stable .hive-project.edn based IDs"
  (let ((old-dir (hive-mcp-memory--project-dir old-project-id))
        (new-dir (hive-mcp-memory--project-dir new-project-id))
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
              (let* ((old-data (hive-mcp-memory--read-json-file old-file))
                     (existing-data (hive-mcp-memory--read-json-file new-file))
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
                (hive-mcp-memory--write-json-file new-file merged-data)
                (cl-incf migrated (length new-entries)))
            (error
             (push (cons type (error-message-string err)) errors))))))

    ;; Clear cache for both projects
    (dolist (type types)
      (remhash (hive-mcp-memory--cache-key old-project-id type)
               hive-mcp-memory--cache)
      (remhash (hive-mcp-memory--cache-key new-project-id type)
               hive-mcp-memory--cache))

    (list :migrated migrated
          :updated-scopes updated-scopes
          :errors errors
          :old-project-id old-project-id
          :new-project-id new-project-id)))

(provide 'hive-mcp-memory)
;;; hive-mcp-memory.el ends here
