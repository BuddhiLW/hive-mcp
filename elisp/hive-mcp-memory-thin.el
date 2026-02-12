;;; hive-mcp-memory-thin.el --- Thin API layer for Chroma-backed memory  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Refactored memory module - thin API layer.
;;
;; Previous: 919 LOC handling JSON file I/O, caching, CRUD
;; Current: ~250 LOC providing helpers + MCP tool delegation
;;
;; Storage is now delegated to Clojure MCP server → Chroma.
;; This module provides:
;;   - Project ID resolution (from .hive-project.edn or path hash)
;;   - Scope tag helpers (global, domain, project)
;;   - Duration helpers (TTL categories, expiration calculation)
;;   - Convenience functions that delegate to MCP tools
;;
;; DEPENDENCY: Requires hive-mcp Clojure server with Chroma storage.

;;; Code:

(require 'project)
(require 'seq)
(require 'parseedn)

;; Compatibility: plistp was added in Emacs 29
(unless (fboundp 'plistp)
  (defun plistp (object)
    "Return non-nil if OBJECT is a plist."
    (and (listp object)
         (or (null object)
             (and (keywordp (car object))
                  (plistp (cddr object)))))))

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup hive-mcp-memory nil
  "Memory and persistence settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-memory-")

(defcustom hive-mcp-conversation-max-entries 100
  "Maximum conversation log entries per project.
Sent to server as configuration hint."
  :type 'integer
  :group 'hive-mcp-memory)

(defcustom hive-mcp-project-config-file ".hive-project.edn"
  "Filename for project-specific hive-mcp configuration.
This file should contain an EDN map with at least :project-id key.
Example: {:project-id \"my-project\" :aliases [\"old-name\"]}"
  :type 'string
  :group 'hive-mcp-memory)

;;; ============================================================
;;; Duration/Lifespan Constants
;;; ============================================================

(defconst hive-mcp-memory-durations '(ephemeral short medium long permanent)
  "Valid duration categories, ordered shortest to longest.
These correspond to Chroma metadata TTL values.")

(defcustom hive-mcp-memory-duration-days
  '((ephemeral . 1) (short . 7) (medium . 30) (long . 90) (permanent . nil))
  "Days before expiration per duration. nil = never expires.
Used for client-side duration display and hints."
  :type '(alist :key-type symbol :value-type (choice integer (const nil)))
  :group 'hive-mcp-memory)

(defcustom hive-mcp-memory-default-duration 'medium
  "Default duration for new entries."
  :type 'symbol
  :group 'hive-mcp-memory)

;;; ============================================================
;;; Internal Variables
;;; ============================================================

(defvar hive-mcp-memory--project-config-cache (make-hash-table :test 'equal)
  "Cache for project config files. Maps project-root to parsed config.")

(defvar hive-mcp-memory-add-hook nil
  "Hook run after adding memory entry. Args: TYPE ENTRY PROJECT-ID.")

;;; ============================================================
;;; EDN Parser (using parseedn library)
;;; ============================================================

(defun hive-mcp-memory--parse-edn-string (str)
  "Parse an EDN string STR into elisp using parseedn.
Returns a hash-table for maps, vectors for vectors, etc."
  (parseedn-read-str str))

(defun hive-mcp-memory--edn-get (edn key)
  "Get KEY from EDN hash-table or alist.
KEY should be a keyword symbol like :project-id."
  (cond
   ((hash-table-p edn) (gethash key edn))
   ((listp edn) (cdr (assoc key edn)))
   (t nil)))

;;; ============================================================
;;; Project Configuration
;;; ============================================================

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
              (project-id (hive-mcp-memory--edn-get config :project-id)))
    (if (stringp project-id)
        project-id
      (symbol-name project-id))))

(defun hive-mcp-memory-clear-config-cache ()
  "Clear the project config cache. Call after editing .hive-project.edn."
  (interactive)
  (clrhash hive-mcp-memory--project-config-cache)
  (message "Cleared hive-mcp project config cache"))

;;; ============================================================
;;; Project Helpers
;;; ============================================================

(defun hive-mcp-memory--get-project-root ()
  "Get current project root, or nil if not in a project."
  (when-let* ((proj (project-current)))
    (project-root proj)))

(defun hive-mcp-memory--get-project-name ()
  "Get current project name (directory basename), or nil if not in a project."
  (when-let* ((root (hive-mcp-memory--get-project-root)))
    (file-name-nondirectory (directory-file-name root))))

(defun hive-mcp-memory--project-id (&optional project-root)
  "Return unique ID for PROJECT-ROOT (defaults to current project).
Resolution order:
  1. Stable :project-id from .hive-project.edn (survives renames)
  2. Fallback: SHA1 hash of absolute path (filesystem-safe)
Returns \"global\" if not in a project.
PROJECT-ROOT must be a string path or nil."
  ;; CLARITY-I: Validate input type (fix for stringp bug from MCP)
  (let ((root (cond
               ((stringp project-root) project-root)
               ((null project-root) (hive-mcp-memory--get-project-root))
               (t (progn
                    (message "[hive-mcp] Warning: project-root not a string: %S" project-root)
                    (hive-mcp-memory--get-project-root))))))
    (if root
        (or (hive-mcp-memory--get-stable-project-id root)
            (substring (sha1 (expand-file-name root)) 0 16))
      "global")))

(defun hive-mcp-memory--project-id-hash (&optional project-root)
  "Return SHA1 hash ID for PROJECT-ROOT (ignores config).
Use this for migration when you need the path-based hash.
PROJECT-ROOT must be a string path or nil."
  ;; CLARITY-I: Validate input type (fix for stringp bug from MCP)
  (let ((root (cond
               ((stringp project-root) project-root)
               ((null project-root) (hive-mcp-memory--get-project-root))
               (t (progn
                    (message "[hive-mcp] Warning: project-root not a string: %S" project-root)
                    (hive-mcp-memory--get-project-root))))))
    (if root
        (substring (sha1 (expand-file-name root)) 0 16)
      "global")))

;;; ============================================================
;;; Scope System
;;; ============================================================
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

;;; ============================================================
;;; Duration Helpers
;;; ============================================================

(defun hive-mcp-memory--calculate-expires (duration)
  "Calculate expiration timestamp for DURATION.
Returns ISO 8601 timestamp or nil for permanent entries.
Used for client-side display; actual TTL managed by Chroma."
  (let ((days (alist-get duration hive-mcp-memory-duration-days)))
    (cond
     ((null days) nil)  ; permanent - never expires
     ((= days 0) (format-time-string "%FT%T%z"))  ; ephemeral
     (t (format-time-string "%FT%T%z"
                            (time-add (current-time)
                                      (days-to-time days)))))))

(defun hive-mcp-memory--duration-next (duration)
  "Return next longer DURATION in hierarchy, or nil if permanent."
  (let ((pos (seq-position hive-mcp-memory-durations duration)))
    (when (and pos (< (1+ pos) (length hive-mcp-memory-durations)))
      (nth (1+ pos) hive-mcp-memory-durations))))

(defun hive-mcp-memory--duration-prev (duration)
  "Return next shorter DURATION in hierarchy, or nil if ephemeral."
  (let ((pos (seq-position hive-mcp-memory-durations duration)))
    (when (and pos (> pos 0))
      (nth (1- pos) hive-mcp-memory-durations))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun hive-mcp-memory--generate-id ()
  "Generate a unique ID for memory entries.
Note: In Chroma-delegate mode, IDs are generated server-side.
This is provided for backwards compatibility."
  (format "%s-%s"
          (format-time-string "%Y%m%d%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 8)))

(defun hive-mcp-memory--timestamp ()
  "Return current ISO 8601 timestamp."
  (format-time-string "%FT%T%z"))

(defun hive-mcp-memory--normalize-content (content)
  "Normalize CONTENT for consistent hashing.
Trims whitespace, collapses multiple spaces/newlines."
  (let ((text (cond
               ((stringp content) content)
               ((plistp content) (format "%S" content))
               (t (format "%S" content)))))
    (setq text (string-trim text))
    (setq text (replace-regexp-in-string "[ \t]+" " " text))
    (setq text (replace-regexp-in-string "\n+" "\n" text))
    text))

(defun hive-mcp-memory-content-hash (content)
  "Compute SHA-256 hash of normalized CONTENT.
Returns a 64-character hex string."
  (secure-hash 'sha256 (hive-mcp-memory--normalize-content content)))

;;; ============================================================
;;; MCP Tool Delegation Stubs
;;; ============================================================
;;
;; These functions provide the same API as the original module
;; but signal that storage is delegated to MCP tools.
;;
;; IMPORTANT: After chroma-architect updates Clojure side,
;; these can call MCP tools directly or be removed in favor
;; of hive-mcp-api.el functions.

(defvar hive-mcp-memory--storage-delegate nil
  "Function to call for storage operations.
Should accept (op type &rest args) and delegate to MCP.
If nil, storage operations will error with instructions.")

;; ============================================================
;; CIDER-based Storage Delegate (calls Clojure MCP handlers)
;; ============================================================

(defun hive-mcp-memory--cider-delegate (op &rest args)
  "Delegate memory operation OP to Clojure MCP handlers via CIDER.
OP is one of: add, get, query, update, delete.
ARGS vary by operation:
  - add: type content tags project-id duration
  - get: id project-id
  - query: type tags project-id limit duration scope-filter
  - update: id updates project-id
  - delete: id project-id
Returns the result from Clojure or nil on failure."
  (unless (and (featurep 'cider) (cider-connected-p))
    (error "CIDER not connected. Memory operations require nREPL connection"))
  (let* ((clj-code (hive-mcp-memory--build-clj-call op args))
         (response (cider-nrepl-send-sync-request
                    (list "op" "eval"
                          "code" clj-code
                          "ns" "hive-mcp.tools.memory")))
         (value (nrepl-dict-get response "value"))
         (err (nrepl-dict-get response "err")))
    (when err
      (message "[hive-mcp-memory] Error: %s" err))
    (when value
      (car (read-from-string value)))))

(defun hive-mcp-memory--build-clj-call (op args)
  "Build Clojure code to call MCP handler for OP with ARGS."
  (pcase op
    ('add
     (let ((type (nth 0 args))
           (content (nth 1 args))
           (tags (nth 2 args))
           (project-id (nth 3 args))
           (duration (nth 4 args)))
       (format "(handle-mcp-memory-add {:type %S :content %S :tags %s :directory %s :duration %s})"
               (symbol-name type)
               (if (stringp content) content (prin1-to-string content))
               (if tags (format "'%S" tags) "nil")
               (if project-id (format "\"%s\"" project-id) "nil")
               (if duration (format "\"%s\"" (symbol-name duration)) "nil"))))
    ('get
     (let ((id (nth 0 args))
           (project-id (nth 1 args)))
       (format "(handle-mcp-memory-get-full {:id %S :directory %s})"
               id
               (if project-id (format "\"%s\"" project-id) "nil"))))
    ('query
     (let ((type (nth 0 args))
           (tags (nth 1 args))
           (project-id (nth 2 args))
           (limit (nth 3 args))
           (duration (nth 4 args))
           (scope-filter (nth 5 args)))
       (format "(handle-mcp-memory-query {:type %S :tags %s :directory %s :limit %s :duration %s :scope %s})"
               (when type (symbol-name type))
               (if tags (format "'%S" tags) "nil")
               (if project-id (format "\"%s\"" project-id) "nil")
               (or limit "20")
               (if duration (format "\"%s\"" duration) "nil")
               (if scope-filter (format "\"%s\"" scope-filter) "nil"))))
    ('update
     (let ((id (nth 0 args))
           (updates (nth 1 args))
           (project-id (nth 2 args)))
       (format "(handle-mcp-memory-update-tags {:id %S :tags %s})"
               id
               (if updates (format "'%S" updates) "nil"))))
    ('delete
     (let ((id (nth 0 args)))
       ;; No direct delete handler - use expiration
       (format "(message \"Delete not directly supported - use duration expiration for %s\")" id)))
    (_
     (error "Unknown memory operation: %s" op))))

(defun hive-mcp-memory-init-cider-delegate ()
  "Initialize the CIDER-based storage delegate.
Call this after CIDER connects to enable memory operations."
  (interactive)
  (setq hive-mcp-memory--storage-delegate #'hive-mcp-memory--cider-delegate)
  (message "hive-mcp-memory: CIDER delegate initialized"))

(defun hive-mcp-memory--delegate (operation &rest args)
  "Delegate OPERATION with ARGS to storage backend.
If no delegate is set, signal an error with instructions."
  (if hive-mcp-memory--storage-delegate
      (apply hive-mcp-memory--storage-delegate operation args)
    (error "hive-mcp-memory: Storage delegate not configured. \
Use MCP tools (mcp_memory_add, mcp_memory_query) directly, \
or set `hive-mcp-memory--storage-delegate'")))

;; Stub implementations - these maintain API compatibility
;; but delegate actual storage to MCP/Chroma

(defun hive-mcp-memory-add (type content &optional tags project-id duration)
  "Add a memory entry of TYPE with CONTENT.
TYPE is one of: note, snippet, convention, decision, conversation.
CONTENT is a string or plist.
TAGS is optional list of strings.
PROJECT-ID defaults to current project or global.
DURATION is optional lifespan symbol.

STORAGE: Delegated to MCP server → Chroma."
  (let ((tags-with-scope (hive-mcp-memory--inject-project-scope (or tags '())))
        (pid (or project-id (hive-mcp-memory--project-id)))
        (dur (or duration hive-mcp-memory-default-duration)))
    (hive-mcp-memory--delegate 'add type content tags-with-scope pid dur)))

(defun hive-mcp-memory-get (id &optional project-id)
  "Retrieve memory entry by ID from PROJECT-ID.
STORAGE: Delegated to MCP server → Chroma."
  (hive-mcp-memory--delegate 'get id project-id))

(defun hive-mcp-memory-query (type &optional tags project-id limit duration scope-filter)
  "Query memories by TYPE and optional TAGS.
PROJECT-ID specifies the project.
LIMIT caps the number of results.
DURATION filters by lifespan category.
SCOPE-FILTER controls scope filtering.

STORAGE: Delegated to MCP server → Chroma."
  (hive-mcp-memory--delegate 'query type tags project-id limit duration scope-filter))

(defun hive-mcp-memory-update (id updates &optional project-id)
  "Update memory entry ID with UPDATES plist.
STORAGE: Delegated to MCP server → Chroma."
  (hive-mcp-memory--delegate 'update id updates project-id))

(defun hive-mcp-memory-delete (id &optional project-id)
  "Delete memory entry by ID.
STORAGE: Delegated to MCP server → Chroma."
  (hive-mcp-memory--delegate 'delete id project-id))

;;; ============================================================
;;; Convenience Functions (unchanged API)
;;; ============================================================

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

;;; ============================================================
;;; Project Context (queries via delegate)
;;; ============================================================

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

;;; ============================================================
;;; Initialization
;;; ============================================================

(defun hive-mcp-memory-init ()
  "Initialize memory system.
In Chroma-delegate mode, this just clears caches."
  (clrhash hive-mcp-memory--project-config-cache)
  (message "hive-mcp-memory initialized (Chroma-delegate mode)"))

(provide 'hive-mcp-memory)
;;; hive-mcp-memory-thin.el ends here
