;;; emacs-mcp-ai-bridge.el --- Universal AI integration bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, integration
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Universal AI integration bridge for emacs-mcp.
;;
;; Provides shared utilities for integrating with various Emacs AI packages
;; (gptel, aidermacs, ellama, org-ai).  All addons share these core functions:
;;
;; - Context injection: Add memory to any AI prompt
;; - Response storage: Store AI output to memory
;; - Interaction logging: Audit trail for AI interactions
;; - Swarm dispatch: Delegate tasks to swarm agents
;;
;; Architecture:
;;
;;                 ┌─────────────────────────────────┐
;;                 │      emacs-mcp-ai-bridge        │
;;                 │                                 │
;;                 │  ┌──────────┐  ┌─────────────┐  │
;;                 │  │ Memory   │  │   Swarm     │  │
;;                 │  │ Bridge   │  │   Bridge    │  │
;;                 │  └────┬─────┘  └──────┬──────┘  │
;;                 │       │               │         │
;;                 └───────┼───────────────┼─────────┘
;;                         │               │
;;         ┌───────────────┼───────────────┼───────────────┐
;;         │               │               │               │
;;         ▼               ▼               ▼               ▼
;;   ┌───────────┐   ┌───────────┐   ┌───────────┐   ┌───────────┐
;;   │  gptel    │   │ aidermacs │   │  ellama   │   │  org-ai   │
;;   └───────────┘   └───────────┘   └───────────┘   └───────────┘

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Soft dependencies on emacs-mcp modules
(declare-function emacs-mcp-memory-query "emacs-mcp-memory")
(declare-function emacs-mcp-memory-add "emacs-mcp-memory")
(declare-function emacs-mcp-memory-search-semantic "emacs-mcp-memory")
(declare-function emacs-mcp-swarm-dispatch "emacs-mcp-swarm")
(declare-function emacs-mcp-swarm-status "emacs-mcp-swarm")

;;;; Customization

(defgroup emacs-mcp-ai-bridge nil
  "Universal AI integration bridge."
  :group 'emacs-mcp
  :prefix "emacs-mcp-ai-")

(defcustom emacs-mcp-ai-context-conventions-limit 5
  "Maximum number of conventions to inject into AI context."
  :type 'integer
  :group 'emacs-mcp-ai-bridge)

(defcustom emacs-mcp-ai-context-decisions-limit 3
  "Maximum number of decisions to inject into AI context."
  :type 'integer
  :group 'emacs-mcp-ai-bridge)

(defcustom emacs-mcp-ai-context-semantic-limit 3
  "Maximum number of semantic search results to inject."
  :type 'integer
  :group 'emacs-mcp-ai-bridge)

(defcustom emacs-mcp-ai-store-notable-responses t
  "If non-nil, automatically store notable AI responses to memory."
  :type 'boolean
  :group 'emacs-mcp-ai-bridge)

(defcustom emacs-mcp-ai-notable-min-length 200
  "Minimum length for a response to be considered notable."
  :type 'integer
  :group 'emacs-mcp-ai-bridge)

(defcustom emacs-mcp-ai-log-interactions t
  "If non-nil, log AI interactions for audit trail."
  :type 'boolean
  :group 'emacs-mcp-ai-bridge)

;;;; Memory Bridge

(defun emacs-mcp-ai--memory-available-p ()
  "Check if memory system is available."
  (and (require 'emacs-mcp-memory nil t)
       (fboundp 'emacs-mcp-memory-query)))

(defun emacs-mcp-ai-get-conventions (&optional limit)
  "Get conventions from memory, formatted for AI context.
LIMIT overrides `emacs-mcp-ai-context-conventions-limit'."
  (when (emacs-mcp-ai--memory-available-p)
    (let* ((n (or limit emacs-mcp-ai-context-conventions-limit))
           (entries (emacs-mcp-memory-query 'convention nil nil n)))
      (when entries
        (mapconcat
         (lambda (e)
           (format "- %s" (plist-get e :content)))
         entries
         "\n")))))

(defun emacs-mcp-ai-get-decisions (&optional limit)
  "Get recent decisions from memory, formatted for AI context.
LIMIT overrides `emacs-mcp-ai-context-decisions-limit'."
  (when (emacs-mcp-ai--memory-available-p)
    (let* ((n (or limit emacs-mcp-ai-context-decisions-limit))
           (entries (emacs-mcp-memory-query 'decision nil nil n)))
      (when entries
        (mapconcat
         (lambda (e)
           (let ((created (plist-get e :created)))
             (format "- [%s] %s"
                     (if created (substring created 0 10) "?")
                     (plist-get e :content))))
         entries
         "\n")))))

(defun emacs-mcp-ai-semantic-search (query &optional limit)
  "Search memory semantically for QUERY.
LIMIT overrides `emacs-mcp-ai-context-semantic-limit'.
Returns formatted context string or nil."
  (when (and (emacs-mcp-ai--memory-available-p)
             (fboundp 'emacs-mcp-memory-search-semantic))
    (let* ((n (or limit emacs-mcp-ai-context-semantic-limit))
           (results (emacs-mcp-memory-search-semantic query n)))
      (when results
        (mapconcat
         (lambda (r)
           (format "- [%s] %s"
                   (plist-get r :type)
                   (plist-get r :content)))
         results
         "\n")))))

;;;; Context Building

(defun emacs-mcp-ai-build-context (&optional prompt)
  "Build AI context from memory.
If PROMPT is provided, includes semantic search results.
Returns formatted context string suitable for injection."
  (let ((sections '()))
    ;; Conventions
    (when-let* ((convs (emacs-mcp-ai-get-conventions)))
      (push (format "## Team Conventions\n%s" convs) sections))

    ;; Recent decisions
    (when-let* ((decs (emacs-mcp-ai-get-decisions)))
      (push (format "## Recent Decisions\n%s" decs) sections))

    ;; Semantic search (if prompt provided)
    (when prompt
      (when-let* ((semantic (emacs-mcp-ai-semantic-search prompt)))
        (push (format "## Related Context\n%s" semantic) sections)))

    (when sections
      (concat "# Project Knowledge\n\n"
              (mapconcat #'identity (nreverse sections) "\n\n")))))

(defun emacs-mcp-ai-inject-context (prompt &optional include-semantic)
  "Inject memory context into PROMPT.
If INCLUDE-SEMANTIC is non-nil, includes semantic search results.
Returns the augmented prompt string."
  (let ((context (emacs-mcp-ai-build-context
                  (when include-semantic prompt))))
    (if context
        (concat context "\n\n---\n\n" prompt)
      prompt)))

;;;; Response Storage

(defun emacs-mcp-ai-response-notable-p (response)
  "Check if RESPONSE is notable enough to store.
Returns non-nil if response should be stored."
  (and response
       (stringp response)
       (>= (length response) emacs-mcp-ai-notable-min-length)
       ;; Contains code or structured content
       (or (string-match-p "```" response)
           (string-match-p "^[-*] " response)
           (string-match-p "^[0-9]+\\. " response))))

(defun emacs-mcp-ai-store-response (response source &optional tags)
  "Store RESPONSE to memory if notable.
SOURCE is the AI package name (e.g., \"gptel\").
TAGS is optional list of additional tags."
  (when (and emacs-mcp-ai-store-notable-responses
             (emacs-mcp-ai--memory-available-p)
             (emacs-mcp-ai-response-notable-p response))
    (let* ((truncated (if (> (length response) 2000)
                          (concat (substring response 0 1997) "...")
                        response))
           (all-tags (append (list source "ai-response") tags)))
      (emacs-mcp-memory-add 'snippet truncated all-tags)
      (message "[ai-bridge] Stored notable response from %s" source))))

;;;; Interaction Logging

(defvar emacs-mcp-ai--interaction-log nil
  "Log of recent AI interactions for audit.")

(defun emacs-mcp-ai-log-interaction (source action &optional details)
  "Log an AI interaction.
SOURCE is the AI package, ACTION is what happened.
DETAILS is optional plist of additional info."
  (when emacs-mcp-ai-log-interactions
    (push (list :timestamp (format-time-string "%FT%T%z")
                :source source
                :action action
                :details details)
          emacs-mcp-ai--interaction-log)
    ;; Keep last 100 entries
    (when (> (length emacs-mcp-ai--interaction-log) 100)
      (setq emacs-mcp-ai--interaction-log
            (seq-take emacs-mcp-ai--interaction-log 100)))))

(defun emacs-mcp-ai-get-interaction-log (&optional limit)
  "Get recent interaction log entries.
LIMIT defaults to 20."
  (seq-take emacs-mcp-ai--interaction-log (or limit 20)))

;;;; Swarm Bridge

(defun emacs-mcp-ai--swarm-available-p ()
  "Check if swarm system is available."
  (and (require 'emacs-mcp-swarm nil t)
       (fboundp 'emacs-mcp-swarm-dispatch)))

(cl-defun emacs-mcp-ai-dispatch-to-swarm (task &key preset slave-id callback source)
  "Dispatch TASK to swarm agent.
PRESET is the agent preset (e.g., \"code-review\").
SLAVE-ID optionally targets a specific slave.
CALLBACK is called with result when complete.
SOURCE identifies the calling AI package.

Returns task-id on success, nil on failure."
  (when (emacs-mcp-ai--swarm-available-p)
    (emacs-mcp-ai-log-interaction (or source "ai-bridge")
                                   "swarm-dispatch"
                                   (list :task task :preset preset))
    (let ((target (or slave-id
                       ;; Find or spawn appropriate slave
                       (emacs-mcp-ai--get-or-spawn-slave preset))))
      (when target
        (emacs-mcp-swarm-dispatch target task)))))

(defun emacs-mcp-ai--get-or-spawn-slave (preset)
  "Get an idle slave with PRESET or spawn one.
Returns slave-id or nil."
  (when (emacs-mcp-ai--swarm-available-p)
    (let* ((status (emacs-mcp-swarm-status))
           (slaves (plist-get status :slaves-detail))
           ;; Find idle slave with matching preset
           (matching (cl-find-if
                      (lambda (s)
                        (and (eq (plist-get s :status) 'idle)
                             (member preset (plist-get s :presets))))
                      slaves)))
      (if matching
          (plist-get matching :slave-id)
        ;; Spawn new slave with preset
        (when (fboundp 'emacs-mcp-swarm-spawn)
          (emacs-mcp-swarm-spawn
           (or preset "ai-worker")
           :presets (list preset)))))))

;;;; Format Adapters

(defun emacs-mcp-ai-format-context-for (package context)
  "Format CONTEXT for specific AI PACKAGE.
Returns appropriately formatted string."
  (pcase package
    ('gptel
     ;; gptel uses markdown
     context)
    ('aider
     ;; Aider expects specific format
     (replace-regexp-in-string "^# " "## " context))
    ('ellama
     ;; ellama uses llm.el format
     context)
    ('org-ai
     ;; org-ai uses org-mode format
     (replace-regexp-in-string "^## " "** "
                               (replace-regexp-in-string "^# " "* " context)))
    (_
     context)))

;;;; Hooks Infrastructure

(defvar emacs-mcp-ai-pre-request-hook nil
  "Hook run before any AI request.
Functions receive (source prompt) arguments.")

(defvar emacs-mcp-ai-post-response-hook nil
  "Hook run after AI response.
Functions receive (source response) arguments.")

(defun emacs-mcp-ai-run-pre-request-hooks (source prompt)
  "Run pre-request hooks for SOURCE with PROMPT.
Returns possibly modified prompt."
  (let ((result prompt))
    (dolist (fn emacs-mcp-ai-pre-request-hook)
      (when-let* ((modified (funcall fn source result)))
        (setq result modified)))
    result))

(defun emacs-mcp-ai-run-post-response-hooks (source response)
  "Run post-response hooks for SOURCE with RESPONSE."
  (dolist (fn emacs-mcp-ai-post-response-hook)
    (funcall fn source response)))

;;;; Channel Events (for push-based updates)

(declare-function emacs-mcp-channel-connected-p "emacs-mcp-channel")
(declare-function emacs-mcp-channel-send "emacs-mcp-channel")

(defun emacs-mcp-ai--channel-available-p ()
  "Check if channel is available for push events."
  (and (require 'emacs-mcp-channel nil t)
       (fboundp 'emacs-mcp-channel-connected-p)
       (emacs-mcp-channel-connected-p)))

(defun emacs-mcp-ai-emit-event (event-type data)
  "Emit AI bridge EVENT-TYPE with DATA through channel."
  (when (emacs-mcp-ai--channel-available-p)
    (condition-case err
        (emacs-mcp-channel-send
         `(("type" . ,(concat "ai-bridge:" event-type))
           ("timestamp" . ,(float-time))
           ,@data))
      (error
       (message "[ai-bridge] Channel emit error: %s" (error-message-string err))))))

;;;; API

(defun emacs-mcp-ai-bridge-status ()
  "Get AI bridge status."
  (list :memory-available (emacs-mcp-ai--memory-available-p)
        :swarm-available (emacs-mcp-ai--swarm-available-p)
        :channel-available (emacs-mcp-ai--channel-available-p)
        :log-entries (length emacs-mcp-ai--interaction-log)
        :settings (list :conventions-limit emacs-mcp-ai-context-conventions-limit
                        :decisions-limit emacs-mcp-ai-context-decisions-limit
                        :store-notable emacs-mcp-ai-store-notable-responses)))

(provide 'emacs-mcp-ai-bridge)
;;; emacs-mcp-ai-bridge.el ends here
