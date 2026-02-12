;;; hive-mcp-api.el --- Stable API for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Stable API for Claude to call via the hive-mcp Clojure server's
;; eval_elisp tool.  All functions here return JSON-serializable data.
;;
;; Usage from Claude (via Clojure server):
;;   (ec/eval-elisp "(hive-mcp-api-get-context)")
;;   (ec/eval-elisp "(hive-mcp-api-memory-add \"note\" \"Remember this\")")
;;

;;; Code:

(require 'hive-mcp-memory)
(require 'hive-mcp-kanban)
(require 'hive-mcp-context)
(require 'hive-mcp-graceful)
(require 'hive-mcp-transform)

;; Workflows loaded conditionally
(declare-function hive-mcp-workflow-list "hive-mcp-workflows")
(declare-function hive-mcp-workflow-run "hive-mcp-workflows")
(declare-function hive-mcp-workflow-register "hive-mcp-workflows")
(declare-function hive-mcp-register-trigger "hive-mcp-triggers")
(declare-function hive-mcp-list-triggers "hive-mcp-triggers")

;;;; Input Validation Helpers (CLARITY-I):
;;
;; Validate inputs at API boundaries. Fail fast with clear errors.

(defconst hive-mcp-api--valid-memory-types
  '("note" "snippet" "convention" "decision" "conversation")
  "Valid memory entry types.")

(defconst hive-mcp-api--valid-memory-durations
  '("ephemeral" "short" "medium" "long" "permanent"
    "session" "short-term" "long-term")
  "Valid memory duration values.")

(defconst hive-mcp-api--valid-kanban-statuses
  '("todo" "doing" "review" "done")
  "Valid kanban task statuses.")

(defconst hive-mcp-api--valid-kanban-priorities
  '("high" "medium" "low")
  "Valid kanban task priorities.")

(defconst hive-mcp-api--valid-notification-types
  '("info" "warning" "error")
  "Valid notification types.")

(defconst hive-mcp-api--valid-conversation-roles
  '("user" "assistant")
  "Valid conversation roles.")

(defconst hive-mcp-api--max-content-length 100000
  "Maximum content length for memory entries (100KB).")

(defconst hive-mcp-api--max-timeout-ms 600000
  "Maximum timeout in milliseconds (10 minutes).")

(defun hive-mcp-api--validate-string (value name &optional allow-empty max-length)
  "Validate VALUE is a non-empty string.
NAME is used in error messages.
If ALLOW-EMPTY is non-nil, empty strings are accepted.
MAX-LENGTH limits string length if provided.
Returns VALUE if valid, signals error otherwise."
  (unless (stringp value)
    (error "CLARITY-I: %s must be a string, got %s" name (type-of value)))
  (when (and (not allow-empty) (string-empty-p value))
    (error "CLARITY-I: %s cannot be empty" name))
  (when (and max-length (> (length value) max-length))
    (error "CLARITY-I: %s exceeds maximum length of %d" name max-length))
  value)

(defun hive-mcp-api--validate-string-or-nil (value name &optional max-length)
  "Validate VALUE is a string or nil.
NAME is used in error messages.
MAX-LENGTH limits string length if provided.
Returns VALUE if valid, signals error otherwise."
  (when value
    (hive-mcp-api--validate-string value name t max-length))
  value)

(defun hive-mcp-api--validate-positive-integer (value name &optional max-value)
  "Validate VALUE is a positive integer.
NAME is used in error messages.
MAX-VALUE is optional upper bound.
Returns VALUE if valid, signals error otherwise."
  (unless (integerp value)
    (error "CLARITY-I: %s must be an integer, got %s" name (type-of value)))
  (when (< value 1)
    (error "CLARITY-I: %s must be positive, got %d" name value))
  (when (and max-value (> value max-value))
    (error "CLARITY-I: %s exceeds maximum of %d, got %d" name max-value value))
  value)

(defun hive-mcp-api--validate-non-negative-integer (value name &optional max-value)
  "Validate VALUE is a non-negative integer (0 or positive).
NAME is used in error messages.
MAX-VALUE is optional upper bound.
Returns VALUE if valid, signals error otherwise."
  (unless (integerp value)
    (error "CLARITY-I: %s must be an integer, got %s" name (type-of value)))
  (when (< value 0)
    (error "CLARITY-I: %s must be non-negative, got %d" name value))
  (when (and max-value (> value max-value))
    (error "CLARITY-I: %s exceeds maximum of %d, got %d" name max-value value))
  value)

(defun hive-mcp-api--validate-enum (value valid-values name)
  "Validate VALUE is one of VALID-VALUES.
NAME is used in error messages.
Returns VALUE if valid, signals error otherwise."
  (unless (stringp value)
    (error "CLARITY-I: %s must be a string, got %s" name (type-of value)))
  (unless (member value valid-values)
    (error "CLARITY-I: %s must be one of %s, got \"%s\""
           name (mapconcat #'identity valid-values ", ") value))
  value)

(defun hive-mcp-api--validate-memory-type (type)
  "Validate TYPE is a valid memory type.
Returns TYPE if valid, signals error otherwise."
  (hive-mcp-api--validate-enum type hive-mcp-api--valid-memory-types "memory type"))

(defun hive-mcp-api--validate-memory-duration (duration)
  "Validate DURATION is a valid memory duration.
Returns DURATION if valid, signals error otherwise."
  (hive-mcp-api--validate-enum duration hive-mcp-api--valid-memory-durations "memory duration"))

(defun hive-mcp-api--validate-kanban-status (status)
  "Validate STATUS is a valid kanban status.
Returns STATUS if valid, signals error otherwise."
  (hive-mcp-api--validate-enum status hive-mcp-api--valid-kanban-statuses "kanban status"))

(defun hive-mcp-api--validate-kanban-priority (priority)
  "Validate PRIORITY is a valid kanban priority.
Returns PRIORITY if valid, signals error otherwise."
  (hive-mcp-api--validate-enum priority hive-mcp-api--valid-kanban-priorities "kanban priority"))

(defun hive-mcp-api--validate-notification-type (type)
  "Validate TYPE is a valid notification type.
Returns TYPE if valid, signals error otherwise."
  (hive-mcp-api--validate-enum type hive-mcp-api--valid-notification-types "notification type"))

(defun hive-mcp-api--validate-conversation-role (role)
  "Validate ROLE is a valid conversation role.
Returns ROLE if valid, signals error otherwise."
  (hive-mcp-api--validate-enum role hive-mcp-api--valid-conversation-roles "conversation role"))

(defun hive-mcp-api--validate-id (id)
  "Validate ID is a valid identifier string.
Must be non-empty string, alphanumeric with hyphens/underscores.
Returns ID if valid, signals error otherwise."
  (hive-mcp-api--validate-string id "id")
  (unless (string-match-p "^[a-zA-Z0-9_-]+$" id)
    (error "CLARITY-I: id must be alphanumeric with hyphens/underscores, got \"%s\"" id))
  id)

(defun hive-mcp-api--validate-content (content)
  "Validate CONTENT is valid memory content.
Must be a string or plist, within size limits.
Returns CONTENT if valid, signals error otherwise."
  (cond
   ((stringp content)
    (when (> (length content) hive-mcp-api--max-content-length)
      (error "CLARITY-I: content exceeds maximum length of %d"
             hive-mcp-api--max-content-length))
    content)
   ((listp content)
    ;; Plist/alist content - serialize to check size
    (let ((serialized (prin1-to-string content)))
      (when (> (length serialized) hive-mcp-api--max-content-length)
        (error "CLARITY-I: content exceeds maximum length of %d"
               hive-mcp-api--max-content-length)))
    content)
   (t
    (error "CLARITY-I: content must be a string or list, got %s"
           (type-of content)))))

(defun hive-mcp-api--validate-timeout (timeout-ms)
  "Validate TIMEOUT-MS is a valid timeout value.
Must be positive integer not exceeding max timeout.
Returns TIMEOUT-MS if valid, signals error otherwise."
  (hive-mcp-api--validate-positive-integer
   timeout-ms "timeout" hive-mcp-api--max-timeout-ms))

(defun hive-mcp-api--validate-limit (limit)
  "Validate LIMIT is a valid result limit.
Must be positive integer, max 1000.
Returns LIMIT if valid, signals error otherwise."
  (hive-mcp-api--validate-positive-integer limit "limit" 1000))

(defun hive-mcp-api--validate-list-of-strings (value name)
  "Validate VALUE is a list of strings.
NAME is used in error messages.
Returns VALUE if valid, signals error otherwise."
  (unless (listp value)
    (error "CLARITY-I: %s must be a list, got %s" name (type-of value)))
  (dolist (item value)
    (unless (stringp item)
      (error "CLARITY-I: %s must contain only strings, found %s"
             name (type-of item))))
  value)

(defun hive-mcp-api--validate-tags (tags)
  "Validate TAGS is a valid list of tag strings.
Returns TAGS if valid, signals error otherwise."
  (when tags
    (hive-mcp-api--validate-list-of-strings tags "tags"))
  tags)

(defun hive-mcp-api--sanitize-prompt (prompt)
  "Sanitize PROMPT string for safe use.
Trims whitespace, removes control characters (except newlines).
Returns sanitized string."
  (hive-mcp-api--validate-string prompt "prompt")
  (let ((sanitized (string-trim prompt)))
    ;; Remove control characters except newlines and tabs
    (replace-regexp-in-string "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" "" sanitized)))

;;;; Context API:

(defun hive-mcp-api-get-context ()
  "Return full context as JSON-compatible plist.
Includes buffer, region, defun, project, git, and memory.
Returns partial context on errors - never fails completely."
  (hive-mcp-with-fallback
      (let ((ctx (hive-mcp-context-full)))
        ;; Add memory context with fallback
        (plist-put ctx :memory
                   (hive-mcp-with-fallback
                       (hive-mcp-memory-get-project-context)
                     nil))
        ctx)
    ;; Complete fallback: minimal context
    (list :error "context-unavailable"
          :buffer (buffer-name)
          :point (point))))

(defun hive-mcp-api-get-buffer-context ()
  "Return current buffer context."
  (hive-mcp-context-buffer))

(defun hive-mcp-api-get-region ()
  "Return selected region text and metadata, or nil."
  (hive-mcp-context-region))

(defun hive-mcp-api-get-defun ()
  "Return current function context."
  (hive-mcp-context-defun))

(defun hive-mcp-api-get-project ()
  "Return project context."
  (hive-mcp-context-project))

(defun hive-mcp-api-get-git ()
  "Return git context."
  (hive-mcp-context-git))

(defun hive-mcp-api-get-surrounding-lines (&optional before after)
  "Return lines surrounding point.
BEFORE and AFTER default to 5 lines each."
  (hive-mcp-context-surrounding-lines before after))

;;;; Memory API:

(defun hive-mcp-api-memory-query (type &optional tags limit scope-filter)
  "Query project memory by TYPE with optional TAGS filter.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\",
or \"conversation\".
TAGS is a list of strings.
LIMIT is max results (default 20).
SCOPE-FILTER controls scope filtering:
  - nil or omitted: auto-filter by current project scope + global
  - \"all\": return all entries regardless of scope
  - \"global\": return only scope:global entries
  - specific scope tag string: filter by that scope
Returns a vector of alists suitable for JSON encoding.
Returns empty vector on error."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-memory-type type)
  (hive-mcp-api--validate-tags tags)
  (when limit (hive-mcp-api--validate-limit limit))
  (hive-mcp-with-fallback
      (let* ((scope-arg (hive-mcp-transform-scope-arg scope-filter))
             (results (hive-mcp-memory-query (intern type) tags nil
                                               (or limit 20) nil scope-arg)))
        (hive-mcp-transform-entries-to-vector results))
    []))

(defun hive-mcp-api-memory-add (type content &optional tags duration)
  "Add entry to project memory.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\".
CONTENT is the entry content (string or plist).
TAGS is optional list of strings.
DURATION is optional string: \"session\", \"short-term\", \"long-term\", \"permanent\".
Returns the created entry as alist suitable for JSON encoding.
Returns error alist on failure."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-memory-type type)
  (hive-mcp-api--validate-content content)
  (hive-mcp-api--validate-tags tags)
  (when duration (hive-mcp-api--validate-memory-duration duration))
  (hive-mcp-with-fallback
      (let ((entry (hive-mcp-memory-add (intern type) content tags nil
                                         (when duration (intern duration)))))
        (hive-mcp-transform-plist-to-alist entry))
    (hive-mcp-transform-error-result "memory-add-failed" 'type nil 'id nil)))

(defun hive-mcp-api-memory-get (id)
  "Get memory entry by ID."
  ;; CLARITY-I: Validate id at boundary
  (hive-mcp-api--validate-id id)
  (hive-mcp-memory-get id))

(defun hive-mcp-api-memory-update (id updates)
  "Update memory entry ID with UPDATES plist."
  ;; CLARITY-I: Validate id at boundary
  (hive-mcp-api--validate-id id)
  (hive-mcp-memory-update id updates))

(defun hive-mcp-api-memory-delete (id)
  "Delete memory entry by ID."
  ;; CLARITY-I: Validate id at boundary
  (hive-mcp-api--validate-id id)
  (hive-mcp-memory-delete id))

(defun hive-mcp-api-memory-query-metadata (type &optional tags limit scope-filter)
  "Query project memory by TYPE, returning only metadata.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\",
or \"conversation\".
TAGS is a list of strings.
LIMIT is max results (default 20).
SCOPE-FILTER: see `hive-mcp-api-memory-query' for options.
Returns a vector of alists with only: id, type, preview, tags, created.
Use `hive-mcp-api-memory-get-full' to fetch full content by ID."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-memory-type type)
  (hive-mcp-api--validate-tags tags)
  (when limit (hive-mcp-api--validate-limit limit))
  (let* ((scope-arg (hive-mcp-transform-scope-arg scope-filter))
         (results (hive-mcp-memory-query (intern type) tags nil
                                           (or limit 20) nil scope-arg)))
    (apply #'vector (mapcar #'hive-mcp-transform-entry-to-metadata results))))

(defun hive-mcp-api-get-project-name ()
  "Return the current project name, or nil if not in a project."
  (hive-mcp-memory--get-project-name))

(defun hive-mcp-api-get-applicable-scopes (&optional domain)
  "Return list of scope tags applicable to current context.
DOMAIN is optional domain name to include.
Always includes scope:global and current project scope if in a project."
  (hive-mcp-memory--applicable-scope-tags nil domain))

(defun hive-mcp-api-memory-get-full (id)
  "Get full memory entry by ID.
Returns the complete entry as alist suitable for JSON encoding.
Use this after `hive-mcp-api-memory-query-metadata' to fetch full content."
  ;; CLARITY-I: Validate id at boundary
  (hive-mcp-api--validate-id id)
  (when-let* ((entry (hive-mcp-memory-get id)))
    (hive-mcp-transform-plist-to-alist entry)))

(defun hive-mcp-api-memory-get-project-context ()
  "Return full project context including all memory."
  (hive-mcp-memory-get-project-context))

;;;; Memory Deduplication API:

(defun hive-mcp-api-memory-check-duplicate (type content)
  "Check if CONTENT already exists in memory TYPE.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\".
CONTENT is the content to check.
Returns an alist with:
  - exists: t if duplicate found, nil otherwise
  - entry: the existing entry (as alist) if found, nil otherwise
  - content-hash: the computed hash for the content"
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-memory-type type)
  (hive-mcp-api--validate-content content)
  (let* ((type-sym (if (stringp type) (intern type) type))
         (content-hash (hive-mcp-memory-content-hash content))
         (existing (hive-mcp-memory-find-duplicate type-sym content)))
    (hive-mcp-transform-duplicate-result existing existing content-hash)))

(defun hive-mcp-api-memory-content-hash (content)
  "Compute content hash for CONTENT.
Returns the SHA-256 hash as a string."
  (hive-mcp-memory-content-hash content))

;;;; Memory Duration API:

(defun hive-mcp-api-memory-set-duration (id duration)
  "Set DURATION (string) for entry ID. Returns updated entry as alist."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-id id)
  (hive-mcp-api--validate-memory-duration duration)
  (hive-mcp-memory-set-duration id (intern duration))
  (hive-mcp-transform-plist-to-alist (hive-mcp-memory-get id)))

(defun hive-mcp-api-memory-promote (id)
  "Promote entry ID to longer duration. Returns updated entry."
  ;; CLARITY-I: Validate id at boundary
  (hive-mcp-api--validate-id id)
  (hive-mcp-memory-promote id)
  (hive-mcp-transform-plist-to-alist (hive-mcp-memory-get id)))

(defun hive-mcp-api-memory-demote (id)
  "Demote entry ID to shorter duration. Returns updated entry."
  ;; CLARITY-I: Validate id at boundary
  (hive-mcp-api--validate-id id)
  (hive-mcp-memory-demote id)
  (hive-mcp-transform-plist-to-alist (hive-mcp-memory-get id)))

(defun hive-mcp-api-memory-cleanup-expired ()
  "Remove expired entries. Returns count deleted."
  (hive-mcp-memory-cleanup-expired))

(defun hive-mcp-api-memory-expiring-soon (days)
  "Return entries expiring within DAYS as vector of alists."
  ;; CLARITY-I: Validate days at boundary
  (hive-mcp-api--validate-positive-integer days "days")
  (let ((results (hive-mcp-memory-query-expiring days)))
    (hive-mcp-transform-entries-to-vector results)))

;;;; Conversation API:

(defun hive-mcp-api-conversation-log (role content)
  "Log conversation entry.
ROLE is \"user\" or \"assistant\".
CONTENT is the message content."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-conversation-role role)
  (hive-mcp-api--validate-content content)
  (hive-mcp-memory-log-conversation (intern role) content))

(defun hive-mcp-api-conversation-history (&optional limit)
  "Get recent conversation history.
LIMIT defaults to 20 entries."
  ;; CLARITY-I: Validate limit at boundary
  (when limit (hive-mcp-api--validate-limit limit))
  (hive-mcp-memory-query 'conversation nil nil (or limit 20)))

(defun hive-mcp-api-conversation-clear ()
  "Clear conversation history for current project."
  (let ((pid (hive-mcp-memory--project-id)))
    (hive-mcp-memory--set-data pid "conversation" '())
    t))

;;;; Workflow API:

(defun hive-mcp-api-list-workflows ()
  "Return list of registered workflows."
  (if (fboundp 'hive-mcp-workflow-list)
      (hive-mcp-workflow-list)
    '()))

(defun hive-mcp-api-run-workflow (name &optional args)
  "Run workflow by NAME with optional ARGS plist.
Returns error alist on failure instead of signaling."
  (hive-mcp-with-fallback
      (if (fboundp 'hive-mcp-workflow-run)
          (hive-mcp-workflow-run name args)
        '((error . "workflows-not-available")))
    (hive-mcp-transform-error-result "workflow-execution-failed" 'workflow name)))

(defun hive-mcp-api-register-workflow (name spec)
  "Register a new workflow.
NAME is the workflow name.
SPEC is plist with :description, :steps, :params."
  (if (fboundp 'hive-mcp-workflow-register)
      (hive-mcp-workflow-register name spec)
    (error "Workflows not available")))

(declare-function hive-mcp-workflow-wrap "hive-mcp-workflows")
(declare-function hive-mcp-workflow-catchup "hive-mcp-workflows")

(defun hive-mcp-api-workflow-wrap (&optional accomplishments decisions conventions
                                              in-progress next-actions completed-tasks)
  "Run the wrap workflow with session data.
ACCOMPLISHMENTS - list of completed tasks (stored as note, short-term)
DECISIONS - list of decisions made (stored as decision, long-term)
CONVENTIONS - list of conventions (stored as convention, permanent)
IN-PROGRESS - list of in-progress items (for summary)
NEXT-ACTIONS - list of priorities for next session (for summary)
COMPLETED-TASKS - list of kanban task IDs to mark done

All stored entries auto-inject project scope.
Returns structured result suitable for JSON encoding."
  (hive-mcp-with-fallback
      (if (fboundp 'hive-mcp-workflow-wrap)
          (let ((result (hive-mcp-workflow-wrap
                         (list :accomplishments accomplishments
                               :decisions decisions
                               :conventions conventions
                               :in-progress in-progress
                               :next-actions next-actions
                               :completed-tasks completed-tasks))))
            (hive-mcp-transform-plist-to-alist result))
        '((error . "wrap-workflow-not-available")))
    '((error . "wrap-workflow-failed"))))

(defun hive-mcp-api-workflow-catchup ()
  "Run the catchup workflow to restore session context.
Queries session notes, decisions, and conventions with project scope.
Returns structured result suitable for JSON encoding."
  (hive-mcp-with-fallback
      (if (fboundp 'hive-mcp-workflow-catchup)
          (let ((result (hive-mcp-workflow-catchup)))
            (hive-mcp-transform-plist-to-alist result))
        '((error . "catchup-workflow-not-available")))
    '((error . "catchup-workflow-failed"))))

(declare-function hive-mcp-workflow-wrap--gather-session-data "hive-mcp-workflows" (&optional directory))

(defun hive-mcp-api-wrap-gather (&optional directory)
  "Gather session data for wrap workflow.
DIRECTORY overrides `default-directory' for git operations.
Returns plist with gathered data from memory, git, kanban, and channel.
Use before wrap to preview/confirm data before storing."
  (hive-mcp-with-fallback
      (if (fboundp 'hive-mcp-workflow-wrap--gather-session-data)
          (let ((result (hive-mcp-workflow-wrap--gather-session-data directory)))
            (hive-mcp-transform-plist-to-alist result))
        '((error . "wrap-gather-not-available")))
    '((error . "wrap-gather-failed"))))

;;;; Trigger API:

(defun hive-mcp-api-register-trigger (name spec)
  "Register a trigger for automation.
NAME is the trigger name.
SPEC is plist with :event, :condition, :action."
  (if (fboundp 'hive-mcp-register-trigger)
      (hive-mcp-register-trigger name spec)
    (error "Triggers not available")))

(defun hive-mcp-api-list-triggers ()
  "Return list of registered triggers."
  (if (fboundp 'hive-mcp-list-triggers)
      (hive-mcp-list-triggers)
    '()))

;;;; Interaction API:

(defun hive-mcp-api-notify (message &optional type)
  "Show notification MESSAGE to user.
TYPE is \"info\", \"warning\", or \"error\"."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-string message "message")
  (when type (hive-mcp-api--validate-notification-type type))
  (pcase type
    ("error" (user-error "%s" message))
    ("warning" (display-warning 'hive-mcp message :warning))
    (_ (message "[MCP] %s" message)))
  t)

(defun hive-mcp-api-prompt (prompt &optional default)
  "Show PROMPT and ask user for input.
DEFAULT provides an optional initial value.
Returns the user's response string."
  ;; CLARITY-I: Validate and sanitize prompt at boundary
  (let ((safe-prompt (hive-mcp-api--sanitize-prompt prompt)))
    (hive-mcp-api--validate-string-or-nil default "default")
    (read-string (concat safe-prompt ": ") default)))

(defun hive-mcp-api-confirm (prompt)
  "Show PROMPT and ask user for yes/no confirmation.
Returns t or nil."
  ;; CLARITY-I: Validate and sanitize prompt at boundary
  (let ((safe-prompt (hive-mcp-api--sanitize-prompt prompt)))
    (yes-or-no-p safe-prompt)))

(defun hive-mcp-api-select (prompt options)
  "Ask user to select from OPTIONS.
PROMPT is the prompt string.
OPTIONS is a list of strings.
Returns the selected option."
  ;; CLARITY-I: Validate inputs at boundary
  (let ((safe-prompt (hive-mcp-api--sanitize-prompt prompt)))
    (hive-mcp-api--validate-list-of-strings options "options")
    (completing-read (concat safe-prompt ": ") options nil t)))

;;;; Buffer and File Operations API:

(defun hive-mcp-api-open-file (path &optional line)
  "Open file at PATH and optionally go to LINE.
Returns t on success, error alist on failure."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-string path "path")
  (when line (hive-mcp-api--validate-positive-integer line "line"))
  (hive-mcp-with-fallback
      (progn
        (find-file path)
        (when line
          (goto-char (point-min))
          (forward-line (1- line)))
        t)
    (hive-mcp-transform-error-result "file-open-failed" 'path path)))

(defun hive-mcp-api-save-buffer ()
  "Save current buffer."
  (save-buffer)
  t)

(defun hive-mcp-api-switch-buffer (name)
  "Switch to buffer NAME."
  ;; CLARITY-I: Validate buffer name at boundary
  (hive-mcp-api--validate-string name "buffer name")
  (switch-to-buffer name)
  t)

(defun hive-mcp-api-get-buffer-list ()
  "Return list of buffer names."
  (mapcar #'buffer-name (buffer-list)))

(defun hive-mcp-api-kill-buffer (&optional name)
  "Kill buffer NAME or current buffer."
  ;; CLARITY-I: Validate buffer name at boundary
  (hive-mcp-api--validate-string-or-nil name "buffer name")
  (kill-buffer name)
  t)

;;;; Navigation API:

(defun hive-mcp-api-goto-line (line)
  "Go to LINE number.
Returns alist with line and column position."
  ;; CLARITY-I: Validate line at boundary
  (hive-mcp-api--validate-positive-integer line "line")
  (goto-char (point-min))
  (forward-line (1- line))
  (hive-mcp-transform-position-to-alist nil (line-number-at-pos) (current-column)))

(defun hive-mcp-api-goto-point (point)
  "Go to POINT position.
Returns alist with point, line, and column position."
  ;; CLARITY-I: Validate point at boundary
  (hive-mcp-api--validate-positive-integer point "point")
  (goto-char point)
  (hive-mcp-transform-position-to-alist (point) (line-number-at-pos) (current-column)))

(defun hive-mcp-api-search-forward (string &optional bound)
  "Search forward for STRING.
BOUND limits the search to that buffer position.
Returns position if found, nil otherwise."
  ;; CLARITY-I: Validate string at boundary
  (hive-mcp-api--validate-string string "search string")
  (when bound (hive-mcp-api--validate-positive-integer bound "bound"))
  (search-forward string bound t))

(defun hive-mcp-api-search-regexp (regexp &optional bound)
  "Search forward for REGEXP.
BOUND limits the search to that buffer position.
Returns position if found, nil otherwise."
  ;; CLARITY-I: Validate regexp at boundary
  (hive-mcp-api--validate-string regexp "regexp")
  (when bound (hive-mcp-api--validate-positive-integer bound "bound"))
  (re-search-forward regexp bound t))

;;;; Visual Feedback API:

(defun hive-mcp-api-highlight-line (&optional line)
  "Highlight LINE (or current line) briefly."
  ;; CLARITY-I: Validate line at boundary
  (when line (hive-mcp-api--validate-positive-integer line "line"))
  (save-excursion
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (when (fboundp 'pulse-momentary-highlight-one-line)
      (pulse-momentary-highlight-one-line (point))))
  t)

(defun hive-mcp-api-highlight-region (start end)
  "Highlight region from START to END briefly."
  ;; CLARITY-I: Validate positions at boundary
  (hive-mcp-api--validate-positive-integer start "start")
  (hive-mcp-api--validate-positive-integer end "end")
  (when (fboundp 'pulse-momentary-highlight-region)
    (pulse-momentary-highlight-region start end))
  t)

(defun hive-mcp-api-show-in-buffer (name content &optional mode)
  "Display CONTENT in buffer NAME with optional MODE."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-string name "buffer name")
  (hive-mcp-api--validate-string content "content" t)  ; allow empty
  (hive-mcp-api--validate-string-or-nil mode "mode")
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (when (and mode (fboundp (intern mode)))
        (funcall (intern mode))))
    (display-buffer buf)
    name))

;;;; Version Info:

(defconst hive-mcp-api-version "0.1.0"
  "Version of the hive-mcp API.")

(defun hive-mcp-api-version ()
  "Return API version."
  hive-mcp-api-version)

(defun hive-mcp-api-capabilities ()
  "Return list of available API capabilities."
  (list
   :version hive-mcp-api-version
   :capabilities '("context" "memory" "conversation" "workflows" "triggers"
                   "interaction" "navigation" "visual-feedback" "kanban")
   :memory-types '("note" "snippet" "convention" "decision" "conversation")
   :memory-durations '("session" "short-term" "long-term" "permanent")
   :kanban-statuses '("todo" "doing" "review" "done")
   :kanban-priorities '("high" "medium" "low")
   :workflow-step-types '("elisp" "shell" "prompt" "confirm" "condition"
                          "memory-add" "notify")))

;;;; In-Memory Kanban API:

(defun hive-mcp-api-kanban-create (title &optional priority context directory)
  "API: Create kanban task with TITLE.
PRIORITY is optional (default: medium). Valid: high, medium, low.
CONTEXT is optional notes.
DIRECTORY specifies the project directory for scoping.
Returns created entry as alist."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-string title "title")
  (when priority (hive-mcp-api--validate-kanban-priority priority))
  (hive-mcp-api--validate-string-or-nil context "context")
  (hive-mcp-with-fallback
      (let* ((project-id (when directory (hive-mcp-memory-project-id directory)))
             (entry (hive-mcp-kanban-task-create title priority context project-id)))
        (hive-mcp-transform-plist-to-alist entry))
    (hive-mcp-transform-error-result "kanban-create-failed" 'title title)))

(defun hive-mcp-api-kanban-list (&optional status directory)
  "API: List kanban tasks.
STATUS filters by todo/doing/review. If nil, returns all tasks.
DIRECTORY specifies the project directory for scoping.
Returns vector of alists."
  ;; CLARITY-I: Validate status at boundary (done excluded from filter)
  (when status (hive-mcp-api--validate-kanban-status status))
  (hive-mcp-with-fallback
      (let* ((project-id (when directory (hive-mcp-memory-project-id directory)))
             (entries (if status
                          (hive-mcp-kanban-list-by-status status project-id)
                        (hive-mcp-kanban-list-all project-id))))
        (hive-mcp-transform-entries-to-vector entries))
    []))

(defun hive-mcp-api-kanban-move (task-id new-status &optional directory)
  "API: Move task TASK-ID to NEW-STATUS.
Valid statuses: todo, doing, review, done.
If moved to done, task is DELETED.
DIRECTORY specifies the project directory for scoping.
Returns updated entry as alist, or ((deleted . t)) if done."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-id task-id)
  (hive-mcp-api--validate-kanban-status new-status)
  (hive-mcp-with-fallback
      (let* ((project-id (when directory (hive-mcp-memory-project-id directory)))
             (result (hive-mcp-kanban-task-move task-id new-status project-id)))
        (if (eq result t)
            '((deleted . t) (status . "done"))
          (hive-mcp-transform-plist-to-alist result)))
    (hive-mcp-transform-error-result "kanban-move-failed" 'task_id task-id)))

(defun hive-mcp-api-kanban-delete (task-id &optional directory)
  "API: Delete task TASK-ID.
DIRECTORY specifies the project directory for scoping.
Returns ((deleted . t)) on success, ((deleted . :false)) if not found."
  ;; CLARITY-I: Validate id at boundary
  (hive-mcp-api--validate-id task-id)
  (hive-mcp-with-fallback
      (let ((project-id (when directory (hive-mcp-memory-project-id directory))))
        (if (hive-mcp-kanban-task-delete task-id project-id)
            '((deleted . t))
          '((deleted . :false))))
    (hive-mcp-transform-error-result "kanban-delete-failed" 'task_id task-id)))

(defun hive-mcp-api-kanban-stats (&optional directory)
  "API: Get task counts by status.
DIRECTORY specifies the project directory for scoping.
Returns alist with todo, doing, review counts."
  (hive-mcp-with-fallback
      (let* ((project-id (when directory (hive-mcp-memory-project-id directory)))
             (stats (hive-mcp-kanban-stats project-id)))
        (hive-mcp-transform-stats-to-alist stats '(:todo :doing :review)))
    (hive-mcp-transform-error-result "kanban-stats-failed")))

(defun hive-mcp-api-kanban-update (task-id &optional title priority context directory)
  "API: Update task TASK-ID with new TITLE, PRIORITY, or CONTEXT.
Only provided fields are updated.
DIRECTORY specifies the project directory for scoping.
Returns updated entry as alist."
  ;; CLARITY-I: Validate inputs at boundary
  (hive-mcp-api--validate-id task-id)
  (hive-mcp-api--validate-string-or-nil title "title")
  (when priority (hive-mcp-api--validate-kanban-priority priority))
  (hive-mcp-api--validate-string-or-nil context "context")
  (hive-mcp-with-fallback
      (let* ((project-id (when directory (hive-mcp-memory-project-id directory)))
             (entry (hive-mcp-kanban-task-update task-id title priority context project-id)))
        (hive-mcp-transform-plist-to-alist entry))
    (hive-mcp-transform-error-result "kanban-update-failed" 'task_id task-id)))

(provide 'hive-mcp-api)
;;; hive-mcp-api.el ends here
