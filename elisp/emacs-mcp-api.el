;;; emacs-mcp-api.el --- Stable API for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of emacs-mcp.

;;; Commentary:
;;
;; Stable API for Claude to call via the emacs-mcp Clojure server's
;; eval_elisp tool.  All functions here return JSON-serializable data.
;;
;; Usage from Claude (via Clojure server):
;;   (ec/eval-elisp "(emacs-mcp-api-get-context)")
;;   (ec/eval-elisp "(emacs-mcp-api-memory-add \"note\" \"Remember this\")")
;;

;;; Code:

(require 'emacs-mcp-memory)
(require 'emacs-mcp-context)

;; Workflows loaded conditionally
(declare-function emacs-mcp-workflow-list "emacs-mcp-workflows")
(declare-function emacs-mcp-workflow-run "emacs-mcp-workflows")
(declare-function emacs-mcp-workflow-register "emacs-mcp-workflows")
(declare-function emacs-mcp-register-trigger "emacs-mcp-triggers")
(declare-function emacs-mcp-list-triggers "emacs-mcp-triggers")

;;;; Context API:

(defun emacs-mcp-api-get-context ()
  "Return full context as JSON-compatible plist.
Includes buffer, region, defun, project, git, and memory."
  (let ((ctx (emacs-mcp-context-full)))
    ;; Add memory context
    (plist-put ctx :memory (emacs-mcp-memory-get-project-context))
    ctx))

(defun emacs-mcp-api-get-buffer-context ()
  "Return current buffer context."
  (emacs-mcp-context-buffer))

(defun emacs-mcp-api-get-region ()
  "Return selected region text and metadata, or nil."
  (emacs-mcp-context-region))

(defun emacs-mcp-api-get-defun ()
  "Return current function context."
  (emacs-mcp-context-defun))

(defun emacs-mcp-api-get-project ()
  "Return project context."
  (emacs-mcp-context-project))

(defun emacs-mcp-api-get-git ()
  "Return git context."
  (emacs-mcp-context-git))

(defun emacs-mcp-api-get-surrounding-lines (&optional before after)
  "Return lines surrounding point.
BEFORE and AFTER default to 5 lines each."
  (emacs-mcp-context-surrounding-lines before after))

;;;; Memory API:

(defun emacs-mcp-api--plist-to-alist (plist)
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
                              (emacs-mcp-api--plist-to-alist val))
                             ((and (listp val) val)
                              (apply #'vector
                                     (mapcar (lambda (v)
                                               (if (and (listp v) (keywordp (car-safe v)))
                                                   (emacs-mcp-api--plist-to-alist v)
                                                 v))
                                             val)))
                             ((null val) [])
                             (t val))))
        (push (cons key-sym val-converted) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun emacs-mcp-api--convert-entries (entries)
  "Convert list of plist ENTRIES to vector of alists for JSON."
  (apply #'vector (mapcar #'emacs-mcp-api--plist-to-alist entries)))

(defun emacs-mcp-api-memory-query (type &optional tags limit)
  "Query project memory by TYPE with optional TAGS filter.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\",
or \"conversation\".
TAGS is a list of strings.
LIMIT is max results (default 20).
Returns a vector of alists suitable for JSON encoding."
  (let ((results (emacs-mcp-memory-query (intern type) tags nil (or limit 20))))
    (emacs-mcp-api--convert-entries results)))

(defun emacs-mcp-api-memory-add (type content &optional tags duration)
  "Add entry to project memory.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\".
CONTENT is the entry content (string or plist).
TAGS is optional list of strings.
DURATION is optional string: \"session\", \"short-term\", \"long-term\", \"permanent\".
Returns the created entry as alist suitable for JSON encoding."
  (let ((entry (emacs-mcp-memory-add (intern type) content tags nil
                                     (when duration (intern duration)))))
    (emacs-mcp-api--plist-to-alist entry)))

(defun emacs-mcp-api-memory-get (id)
  "Get memory entry by ID."
  (emacs-mcp-memory-get id))

(defun emacs-mcp-api-memory-update (id updates)
  "Update memory entry ID with UPDATES plist."
  (emacs-mcp-memory-update id updates))

(defun emacs-mcp-api-memory-delete (id)
  "Delete memory entry by ID."
  (emacs-mcp-memory-delete id))

(defun emacs-mcp-api-memory--content-preview (content &optional max-len)
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

(defun emacs-mcp-api-memory--entry-to-metadata (entry)
  "Convert ENTRY plist to metadata-only alist.
Returns id, type, preview (first 100 chars), tags, created."
  (let ((content (plist-get entry :content)))
    `((id . ,(plist-get entry :id))
      (type . ,(plist-get entry :type))
      (preview . ,(emacs-mcp-api-memory--content-preview content))
      (tags . ,(or (plist-get entry :tags) []))
      (created . ,(plist-get entry :created)))))

(defun emacs-mcp-api-memory-query-metadata (type &optional tags limit)
  "Query project memory by TYPE, returning only metadata.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\",
or \"conversation\".
TAGS is a list of strings.
LIMIT is max results (default 20).
Returns a vector of alists with only: id, type, preview, tags, created.
Use `emacs-mcp-api-memory-get-full' to fetch full content by ID."
  (let ((results (emacs-mcp-memory-query (intern type) tags nil (or limit 20))))
    (apply #'vector (mapcar #'emacs-mcp-api-memory--entry-to-metadata results))))

(defun emacs-mcp-api-memory-get-full (id)
  "Get full memory entry by ID.
Returns the complete entry as alist suitable for JSON encoding.
Use this after `emacs-mcp-api-memory-query-metadata' to fetch full content."
  (when-let* ((entry (emacs-mcp-memory-get id)))
    (emacs-mcp-api--plist-to-alist entry)))

(defun emacs-mcp-api-memory-get-project-context ()
  "Return full project context including all memory."
  (emacs-mcp-memory-get-project-context))

;;;; Memory Deduplication API:

(defun emacs-mcp-api-memory-check-duplicate (type content)
  "Check if CONTENT already exists in memory TYPE.
TYPE is a string: \"note\", \"snippet\", \"convention\", \"decision\".
CONTENT is the content to check.
Returns an alist with:
  - exists: t if duplicate found, nil otherwise
  - entry: the existing entry (as alist) if found, nil otherwise
  - content-hash: the computed hash for the content"
  (let* ((type-sym (if (stringp type) (intern type) type))
         (content-hash (emacs-mcp-memory-content-hash content))
         (existing (emacs-mcp-memory-find-duplicate type-sym content)))
    `((exists . ,(if existing t :false))
      (entry . ,(when existing
                  (emacs-mcp-api--plist-to-alist existing)))
      (content_hash . ,content-hash))))

(defun emacs-mcp-api-memory-content-hash (content)
  "Compute content hash for CONTENT.
Returns the SHA-256 hash as a string."
  (emacs-mcp-memory-content-hash content))

;;;; Memory Duration API:

(defun emacs-mcp-api-memory-set-duration (id duration)
  "Set DURATION (string) for entry ID. Returns updated entry as alist."
  (emacs-mcp-memory-set-duration id (intern duration))
  (emacs-mcp-api--plist-to-alist (emacs-mcp-memory-get id)))

(defun emacs-mcp-api-memory-promote (id)
  "Promote entry ID to longer duration. Returns updated entry."
  (emacs-mcp-memory-promote id)
  (emacs-mcp-api--plist-to-alist (emacs-mcp-memory-get id)))

(defun emacs-mcp-api-memory-demote (id)
  "Demote entry ID to shorter duration. Returns updated entry."
  (emacs-mcp-memory-demote id)
  (emacs-mcp-api--plist-to-alist (emacs-mcp-memory-get id)))

(defun emacs-mcp-api-memory-cleanup-expired ()
  "Remove expired entries. Returns count deleted."
  (emacs-mcp-memory-cleanup-expired))

(defun emacs-mcp-api-memory-expiring-soon (days)
  "Return entries expiring within DAYS as vector of alists."
  (let ((results (emacs-mcp-memory-query-expiring days)))
    (emacs-mcp-api--convert-entries results)))

;;;; Conversation API:

(defun emacs-mcp-api-conversation-log (role content)
  "Log conversation entry.
ROLE is \"user\" or \"assistant\".
CONTENT is the message content."
  (emacs-mcp-memory-log-conversation (intern role) content))

(defun emacs-mcp-api-conversation-history (&optional limit)
  "Get recent conversation history.
LIMIT defaults to 20 entries."
  (emacs-mcp-memory-query 'conversation nil nil (or limit 20)))

(defun emacs-mcp-api-conversation-clear ()
  "Clear conversation history for current project."
  (let ((pid (emacs-mcp-memory--project-id)))
    (emacs-mcp-memory--set-data pid "conversation" '())
    t))

;;;; Workflow API:

(defun emacs-mcp-api-list-workflows ()
  "Return list of registered workflows."
  (if (fboundp 'emacs-mcp-workflow-list)
      (emacs-mcp-workflow-list)
    '()))

(defun emacs-mcp-api-run-workflow (name &optional args)
  "Run workflow by NAME with optional ARGS plist."
  (if (fboundp 'emacs-mcp-workflow-run)
      (emacs-mcp-workflow-run name args)
    (error "Workflows not available")))

(defun emacs-mcp-api-register-workflow (name spec)
  "Register a new workflow.
NAME is the workflow name.
SPEC is plist with :description, :steps, :params."
  (if (fboundp 'emacs-mcp-workflow-register)
      (emacs-mcp-workflow-register name spec)
    (error "Workflows not available")))

;;;; Trigger API:

(defun emacs-mcp-api-register-trigger (name spec)
  "Register a trigger for automation.
NAME is the trigger name.
SPEC is plist with :event, :condition, :action."
  (if (fboundp 'emacs-mcp-register-trigger)
      (emacs-mcp-register-trigger name spec)
    (error "Triggers not available")))

(defun emacs-mcp-api-list-triggers ()
  "Return list of registered triggers."
  (if (fboundp 'emacs-mcp-list-triggers)
      (emacs-mcp-list-triggers)
    '()))

;;;; Interaction API:

(defun emacs-mcp-api-notify (message &optional type)
  "Show notification MESSAGE to user.
TYPE is \"info\", \"warning\", or \"error\"."
  (pcase type
    ("error" (user-error "%s" message))
    ("warning" (display-warning 'emacs-mcp message :warning))
    (_ (message "[MCP] %s" message)))
  t)

(defun emacs-mcp-api-prompt (prompt &optional default)
  "Show PROMPT and ask user for input.
DEFAULT provides an optional initial value.
Returns the user's response string."
  (read-string (concat prompt ": ") default))

(defun emacs-mcp-api-confirm (prompt)
  "Show PROMPT and ask user for yes/no confirmation.
Returns t or nil."
  (yes-or-no-p prompt))

(defun emacs-mcp-api-select (prompt options)
  "Ask user to select from OPTIONS.
PROMPT is the prompt string.
OPTIONS is a list of strings.
Returns the selected option."
  (completing-read (concat prompt ": ") options nil t))

;;;; Buffer and File Operations API:

(defun emacs-mcp-api-open-file (path &optional line)
  "Open file at PATH and optionally go to LINE."
  (find-file path)
  (when line
    (goto-char (point-min))
    (forward-line (1- line)))
  t)

(defun emacs-mcp-api-save-buffer ()
  "Save current buffer."
  (save-buffer)
  t)

(defun emacs-mcp-api-switch-buffer (name)
  "Switch to buffer NAME."
  (switch-to-buffer name)
  t)

(defun emacs-mcp-api-get-buffer-list ()
  "Return list of buffer names."
  (mapcar #'buffer-name (buffer-list)))

(defun emacs-mcp-api-kill-buffer (&optional name)
  "Kill buffer NAME or current buffer."
  (kill-buffer name)
  t)

;;;; Navigation API:

(defun emacs-mcp-api-goto-line (line)
  "Go to LINE number."
  (goto-char (point-min))
  (forward-line (1- line))
  (list :line (line-number-at-pos) :column (current-column)))

(defun emacs-mcp-api-goto-point (point)
  "Go to POINT position."
  (goto-char point)
  (list :point (point) :line (line-number-at-pos) :column (current-column)))

(defun emacs-mcp-api-search-forward (string &optional bound)
  "Search forward for STRING.
BOUND limits the search to that buffer position.
Returns position if found, nil otherwise."
  (search-forward string bound t))

(defun emacs-mcp-api-search-regexp (regexp &optional bound)
  "Search forward for REGEXP.
BOUND limits the search to that buffer position.
Returns position if found, nil otherwise."
  (re-search-forward regexp bound t))

;;;; Visual Feedback API:

(defun emacs-mcp-api-highlight-line (&optional line)
  "Highlight LINE (or current line) briefly."
  (save-excursion
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (when (fboundp 'pulse-momentary-highlight-one-line)
      (pulse-momentary-highlight-one-line (point))))
  t)

(defun emacs-mcp-api-highlight-region (start end)
  "Highlight region from START to END briefly."
  (when (fboundp 'pulse-momentary-highlight-region)
    (pulse-momentary-highlight-region start end))
  t)

(defun emacs-mcp-api-show-in-buffer (name content &optional mode)
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

(defconst emacs-mcp-api-version "0.1.0"
  "Version of the emacs-mcp API.")

(defun emacs-mcp-api-version ()
  "Return API version."
  emacs-mcp-api-version)

(defun emacs-mcp-api-capabilities ()
  "Return list of available API capabilities."
  (list
   :version emacs-mcp-api-version
   :capabilities '("context" "memory" "conversation" "workflows" "triggers"
                   "interaction" "navigation" "visual-feedback")
   :memory-types '("note" "snippet" "convention" "decision" "conversation")
   :memory-durations '("session" "short-term" "long-term" "permanent")
   :workflow-step-types '("elisp" "shell" "prompt" "confirm" "condition"
                          "memory-add" "notify")))

(provide 'emacs-mcp-api)
;;; emacs-mcp-api.el ends here
