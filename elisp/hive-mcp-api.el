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

;; Workflows loaded conditionally
(declare-function hive-mcp-workflow-list "hive-mcp-workflows")
(declare-function hive-mcp-workflow-run "hive-mcp-workflows")
(declare-function hive-mcp-workflow-register "hive-mcp-workflows")
(declare-function hive-mcp-register-trigger "hive-mcp-triggers")
(declare-function hive-mcp-list-triggers "hive-mcp-triggers")

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

(defun hive-mcp-api--plist-to-alist (plist)
  "Convert PLIST to alist for JSON serialization.
Keyword keys become symbols, lists become vectors."
  (let (alist)
    (while plist
      (let* ((key (car plist))
             (val (cadr plist))
             (key-sym (if (keywordp key)
                          (intern (substring (symbol-name key) 1))
                        key))
             (val-converted (cond
                             ((and (listp val) (keywordp (car-safe val)))
                              (hive-mcp-api--plist-to-alist val))
                             ((and (listp val) val)
                              (apply #'vector
                                     (mapcar (lambda (v)
                                               (if (and (listp v) (keywordp (car-safe v)))
                                                   (hive-mcp-api--plist-to-alist v)
                                                 v))
                                             val)))
                             ((null val) [])
                             (t val))))
        (push (cons key-sym val-converted) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun hive-mcp-api--convert-entries (entries)
  "Convert list of plist ENTRIES to vector of alists for JSON."
  (apply #'vector (mapcar #'hive-mcp-api--plist-to-alist entries)))

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
  (hive-mcp-with-fallback
      (let* ((scope-arg (cond
                         ((null scope-filter) nil)  ; auto-filter
                         ((string= scope-filter "all") t)
                         ((string= scope-filter "global") 'global)
                         (t scope-filter)))  ; specific scope tag
             (results (hive-mcp-memory-query (intern type) tags nil
                                               (or limit 20) nil scope-arg)))
        (hive-mcp-api--convert-entries results))
    []))

(defun hive-mcp-api-memory-add (type content &optional tags duration)
  "Add entry to project memory.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\".
CONTENT is the entry content (string or plist).
TAGS is optional list of strings.
DURATION is optional string: \"session\", \"short-term\", \"long-term\", \"permanent\".
Returns the created entry as alist suitable for JSON encoding.
Returns error alist on failure."
  (hive-mcp-with-fallback
      (let ((entry (hive-mcp-memory-add (intern type) content tags nil
                                         (when duration (intern duration)))))
        (hive-mcp-api--plist-to-alist entry))
    '((error . "memory-add-failed") (type . nil) (id . nil))))

(defun hive-mcp-api-memory-get (id)
  "Get memory entry by ID."
  (hive-mcp-memory-get id))

(defun hive-mcp-api-memory-update (id updates)
  "Update memory entry ID with UPDATES plist."
  (hive-mcp-memory-update id updates))

(defun hive-mcp-api-memory-delete (id)
  "Delete memory entry by ID."
  (hive-mcp-memory-delete id))

(defun hive-mcp-api-memory--content-preview (content &optional max-len)
  "Return a preview of CONTENT truncated to MAX-LEN characters.
MAX-LEN defaults to 100.  Handles both string and plist content."
  (let ((max-len (or max-len 100))
        (text (cond
               ((stringp content) content)
               ((plistp content)
                ;; For plists, try to get a meaningful preview
                (or (plist-get content :description)
                    (plist-get content :title)
                    (plist-get content :name)
                    (plist-get content :code)
                    (format "%S" content)))
               (t (format "%S" content)))))
    (if (> (length text) max-len)
        (concat (substring text 0 (- max-len 3)) "...")
      text)))

(defun hive-mcp-api-memory--entry-to-metadata (entry)
  "Convert ENTRY plist to metadata-only alist.
Returns id, type, preview (first 100 chars), tags, created."
  (let ((content (plist-get entry :content)))
    `((id . ,(plist-get entry :id))
      (type . ,(plist-get entry :type))
      (preview . ,(hive-mcp-api-memory--content-preview content))
      (tags . ,(or (plist-get entry :tags) []))
      (created . ,(plist-get entry :created)))))

(defun hive-mcp-api-memory-query-metadata (type &optional tags limit scope-filter)
  "Query project memory by TYPE, returning only metadata.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\",
or \"conversation\".
TAGS is a list of strings.
LIMIT is max results (default 20).
SCOPE-FILTER: see `hive-mcp-api-memory-query' for options.
Returns a vector of alists with only: id, type, preview, tags, created.
Use `hive-mcp-api-memory-get-full' to fetch full content by ID."
  (let* ((scope-arg (cond
                     ((null scope-filter) nil)
                     ((string= scope-filter "all") t)
                     ((string= scope-filter "global") 'global)
                     (t scope-filter)))
         (results (hive-mcp-memory-query (intern type) tags nil
                                           (or limit 20) nil scope-arg)))
    (apply #'vector (mapcar #'hive-mcp-api-memory--entry-to-metadata results))))

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
  (when-let* ((entry (hive-mcp-memory-get id)))
    (hive-mcp-api--plist-to-alist entry)))

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
  (let* ((type-sym (if (stringp type) (intern type) type))
         (content-hash (hive-mcp-memory-content-hash content))
         (existing (hive-mcp-memory-find-duplicate type-sym content)))
    `((exists . ,(if existing t :false))
      (entry . ,(when existing
                  (hive-mcp-api--plist-to-alist existing)))
      (content_hash . ,content-hash))))

(defun hive-mcp-api-memory-content-hash (content)
  "Compute content hash for CONTENT.
Returns the SHA-256 hash as a string."
  (hive-mcp-memory-content-hash content))

;;;; Memory Duration API:

(defun hive-mcp-api-memory-set-duration (id duration)
  "Set DURATION (string) for entry ID. Returns updated entry as alist."
  (hive-mcp-memory-set-duration id (intern duration))
  (hive-mcp-api--plist-to-alist (hive-mcp-memory-get id)))

(defun hive-mcp-api-memory-promote (id)
  "Promote entry ID to longer duration. Returns updated entry."
  (hive-mcp-memory-promote id)
  (hive-mcp-api--plist-to-alist (hive-mcp-memory-get id)))

(defun hive-mcp-api-memory-demote (id)
  "Demote entry ID to shorter duration. Returns updated entry."
  (hive-mcp-memory-demote id)
  (hive-mcp-api--plist-to-alist (hive-mcp-memory-get id)))

(defun hive-mcp-api-memory-cleanup-expired ()
  "Remove expired entries. Returns count deleted."
  (hive-mcp-memory-cleanup-expired))

(defun hive-mcp-api-memory-expiring-soon (days)
  "Return entries expiring within DAYS as vector of alists."
  (let ((results (hive-mcp-memory-query-expiring days)))
    (hive-mcp-api--convert-entries results)))

;;;; Conversation API:

(defun hive-mcp-api-conversation-log (role content)
  "Log conversation entry.
ROLE is \"user\" or \"assistant\".
CONTENT is the message content."
  (hive-mcp-memory-log-conversation (intern role) content))

(defun hive-mcp-api-conversation-history (&optional limit)
  "Get recent conversation history.
LIMIT defaults to 20 entries."
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
    '((error . "workflow-execution-failed") (workflow . name))))

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
            (hive-mcp-api--plist-to-alist result))
        '((error . "wrap-workflow-not-available")))
    '((error . "wrap-workflow-failed"))))

(defun hive-mcp-api-workflow-catchup ()
  "Run the catchup workflow to restore session context.
Queries session notes, decisions, and conventions with project scope.
Returns structured result suitable for JSON encoding."
  (hive-mcp-with-fallback
      (if (fboundp 'hive-mcp-workflow-catchup)
          (let ((result (hive-mcp-workflow-catchup)))
            (hive-mcp-api--plist-to-alist result))
        '((error . "catchup-workflow-not-available")))
    '((error . "catchup-workflow-failed"))))

(declare-function hive-mcp-workflow-wrap--gather-session-data "hive-mcp-workflows")

(defun hive-mcp-api-wrap-gather ()
  "Gather session data for wrap workflow.
Returns plist with gathered data from memory, git, kanban, and channel.
Use before wrap to preview/confirm data before storing."
  (hive-mcp-with-fallback
      (if (fboundp 'hive-mcp-workflow-wrap--gather-session-data)
          (let ((result (hive-mcp-workflow-wrap--gather-session-data)))
            (hive-mcp-api--plist-to-alist result))
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
  (pcase type
    ("error" (user-error "%s" message))
    ("warning" (display-warning 'hive-mcp message :warning))
    (_ (message "[MCP] %s" message)))
  t)

(defun hive-mcp-api-prompt (prompt &optional default)
  "Show PROMPT and ask user for input.
DEFAULT provides an optional initial value.
Returns the user's response string."
  (read-string (concat prompt ": ") default))

(defun hive-mcp-api-confirm (prompt)
  "Show PROMPT and ask user for yes/no confirmation.
Returns t or nil."
  (yes-or-no-p prompt))

(defun hive-mcp-api-select (prompt options)
  "Ask user to select from OPTIONS.
PROMPT is the prompt string.
OPTIONS is a list of strings.
Returns the selected option."
  (completing-read (concat prompt ": ") options nil t))

;;;; Buffer and File Operations API:

(defun hive-mcp-api-open-file (path &optional line)
  "Open file at PATH and optionally go to LINE.
Returns t on success, error alist on failure."
  (hive-mcp-with-fallback
      (progn
        (find-file path)
        (when line
          (goto-char (point-min))
          (forward-line (1- line)))
        t)
    `((error . "file-open-failed") (path . ,path))))

(defun hive-mcp-api-save-buffer ()
  "Save current buffer."
  (save-buffer)
  t)

(defun hive-mcp-api-switch-buffer (name)
  "Switch to buffer NAME."
  (switch-to-buffer name)
  t)

(defun hive-mcp-api-get-buffer-list ()
  "Return list of buffer names."
  (mapcar #'buffer-name (buffer-list)))

(defun hive-mcp-api-kill-buffer (&optional name)
  "Kill buffer NAME or current buffer."
  (kill-buffer name)
  t)

;;;; Navigation API:

(defun hive-mcp-api-goto-line (line)
  "Go to LINE number."
  (goto-char (point-min))
  (forward-line (1- line))
  (list :line (line-number-at-pos) :column (current-column)))

(defun hive-mcp-api-goto-point (point)
  "Go to POINT position."
  (goto-char point)
  (list :point (point) :line (line-number-at-pos) :column (current-column)))

(defun hive-mcp-api-search-forward (string &optional bound)
  "Search forward for STRING.
BOUND limits the search to that buffer position.
Returns position if found, nil otherwise."
  (search-forward string bound t))

(defun hive-mcp-api-search-regexp (regexp &optional bound)
  "Search forward for REGEXP.
BOUND limits the search to that buffer position.
Returns position if found, nil otherwise."
  (re-search-forward regexp bound t))

;;;; Visual Feedback API:

(defun hive-mcp-api-highlight-line (&optional line)
  "Highlight LINE (or current line) briefly."
  (save-excursion
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (when (fboundp 'pulse-momentary-highlight-one-line)
      (pulse-momentary-highlight-one-line (point))))
  t)

(defun hive-mcp-api-highlight-region (start end)
  "Highlight region from START to END briefly."
  (when (fboundp 'pulse-momentary-highlight-region)
    (pulse-momentary-highlight-region start end))
  t)

(defun hive-mcp-api-show-in-buffer (name content &optional mode)
  "Display CONTENT in buffer NAME with optional MODE."
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

(defun hive-mcp-api-kanban-create (title &optional priority context)
  "API: Create kanban task with TITLE.
PRIORITY is optional (default: medium). Valid: high, medium, low.
CONTEXT is optional notes.
Returns created entry as alist."
  (hive-mcp-with-fallback
      (let ((entry (hive-mcp-kanban-task-create title priority context)))
        (hive-mcp-api--plist-to-alist entry))
    `((error . "kanban-create-failed") (title . ,title))))

(defun hive-mcp-api-kanban-list (&optional status)
  "API: List kanban tasks.
STATUS filters by todo/doing/review. If nil, returns all tasks.
Returns vector of alists."
  (hive-mcp-with-fallback
      (let ((entries (if status
                         (hive-mcp-kanban-list-by-status status)
                       (hive-mcp-kanban-list-all))))
        (hive-mcp-api--convert-entries entries))
    []))

(defun hive-mcp-api-kanban-move (task-id new-status)
  "API: Move task TASK-ID to NEW-STATUS.
Valid statuses: todo, doing, review, done.
If moved to done, task is DELETED.
Returns updated entry as alist, or ((deleted . t)) if done."
  (hive-mcp-with-fallback
      (let ((result (hive-mcp-kanban-task-move task-id new-status)))
        (if (eq result t)
            '((deleted . t) (status . "done"))
          (hive-mcp-api--plist-to-alist result)))
    `((error . "kanban-move-failed") (task_id . ,task-id))))

(defun hive-mcp-api-kanban-delete (task-id)
  "API: Delete task TASK-ID.
Returns ((deleted . t)) on success, ((deleted . :false)) if not found."
  (hive-mcp-with-fallback
      (if (hive-mcp-kanban-task-delete task-id)
          '((deleted . t))
        '((deleted . :false)))
    `((error . "kanban-delete-failed") (task_id . ,task-id))))

(defun hive-mcp-api-kanban-stats ()
  "API: Get task counts by status.
Returns alist with todo, doing, review counts."
  (hive-mcp-with-fallback
      (let ((stats (hive-mcp-kanban-stats)))
        `((todo . ,(plist-get stats :todo))
          (doing . ,(plist-get stats :doing))
          (review . ,(plist-get stats :review))))
    '((error . "kanban-stats-failed"))))

(defun hive-mcp-api-kanban-update (task-id &optional title priority context)
  "API: Update task TASK-ID with new TITLE, PRIORITY, or CONTEXT.
Only provided fields are updated.
Returns updated entry as alist."
  (hive-mcp-with-fallback
      (let ((entry (hive-mcp-kanban-task-update task-id title priority context)))
        (hive-mcp-api--plist-to-alist entry))
    `((error . "kanban-update-failed") (task_id . ,task-id))))

(provide 'hive-mcp-api)
;;; hive-mcp-api.el ends here
