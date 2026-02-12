;;; hive-mcp-ai-bridge.el --- Universal AI integration bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, integration
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Universal AI integration bridge for hive-mcp.
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
;;                 │      hive-mcp-ai-bridge        │
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

;; Soft dependencies on hive-mcp modules
(declare-function hive-mcp-memory-query "hive-mcp-memory")
(declare-function hive-mcp-memory-add "hive-mcp-memory")
(declare-function hive-mcp-memory-search-semantic "hive-mcp-memory")
(declare-function hive-mcp-swarm-dispatch "hive-mcp-swarm")
(declare-function hive-mcp-swarm-status "hive-mcp-swarm")

;;;; Customization

(defgroup hive-mcp-ai-bridge nil
  "Universal AI integration bridge."
  :group 'hive-mcp
  :prefix "hive-mcp-ai-")

(defcustom hive-mcp-ai-context-conventions-limit 5
  "Maximum number of conventions to inject into AI context."
  :type 'integer
  :group 'hive-mcp-ai-bridge)

(defcustom hive-mcp-ai-context-decisions-limit 3
  "Maximum number of decisions to inject into AI context."
  :type 'integer
  :group 'hive-mcp-ai-bridge)

(defcustom hive-mcp-ai-context-semantic-limit 3
  "Maximum number of semantic search results to inject."
  :type 'integer
  :group 'hive-mcp-ai-bridge)

(defcustom hive-mcp-ai-store-notable-responses t
  "If non-nil, automatically store notable AI responses to memory."
  :type 'boolean
  :group 'hive-mcp-ai-bridge)

(defcustom hive-mcp-ai-notable-min-length 200
  "Minimum length for a response to be considered notable."
  :type 'integer
  :group 'hive-mcp-ai-bridge)

(defcustom hive-mcp-ai-log-interactions t
  "If non-nil, log AI interactions for audit trail."
  :type 'boolean
  :group 'hive-mcp-ai-bridge)

;;;; Memory Bridge

(defun hive-mcp-ai--memory-available-p ()
  "Check if memory system is available."
  (and (require 'hive-mcp-memory nil t)
       (fboundp 'hive-mcp-memory-query)))

(defun hive-mcp-ai-get-conventions (&optional limit)
  "Get conventions from memory, formatted for AI context.
LIMIT overrides `hive-mcp-ai-context-conventions-limit'."
  (when (hive-mcp-ai--memory-available-p)
    (let* ((n (or limit hive-mcp-ai-context-conventions-limit))
           (entries (hive-mcp-memory-query 'convention nil nil n)))
      (when entries
        (mapconcat
         (lambda (e)
           (format "- %s" (plist-get e :content)))
         entries
         "\n")))))

(defun hive-mcp-ai-get-decisions (&optional limit)
  "Get recent decisions from memory, formatted for AI context.
LIMIT overrides `hive-mcp-ai-context-decisions-limit'."
  (when (hive-mcp-ai--memory-available-p)
    (let* ((n (or limit hive-mcp-ai-context-decisions-limit))
           (entries (hive-mcp-memory-query 'decision nil nil n)))
      (when entries
        (mapconcat
         (lambda (e)
           (let ((created (plist-get e :created)))
             (format "- [%s] %s"
                     (if created (substring created 0 10) "?")
                     (plist-get e :content))))
         entries
         "\n")))))

(defun hive-mcp-ai-semantic-search (query &optional limit)
  "Search memory semantically for QUERY.
LIMIT overrides `hive-mcp-ai-context-semantic-limit'.
Returns formatted context string or nil."
  (when (and (hive-mcp-ai--memory-available-p)
             (fboundp 'hive-mcp-memory-search-semantic))
    (let* ((n (or limit hive-mcp-ai-context-semantic-limit))
           (results (hive-mcp-memory-search-semantic query n)))
      (when results
        (mapconcat
         (lambda (r)
           (format "- [%s] %s"
                   (plist-get r :type)
                   (plist-get r :content)))
         results
         "\n")))))

;;;; Context Building

(defun hive-mcp-ai-build-context (&optional prompt)
  "Build AI context from memory.
If PROMPT is provided, includes semantic search results.
Returns formatted context string suitable for injection."
  (let ((sections '()))
    ;; Conventions
    (when-let* ((convs (hive-mcp-ai-get-conventions)))
      (push (format "## Team Conventions\n%s" convs) sections))

    ;; Recent decisions
    (when-let* ((decs (hive-mcp-ai-get-decisions)))
      (push (format "## Recent Decisions\n%s" decs) sections))

    ;; Semantic search (if prompt provided)
    (when prompt
      (when-let* ((semantic (hive-mcp-ai-semantic-search prompt)))
        (push (format "## Related Context\n%s" semantic) sections)))

    (when sections
      (concat "# Project Knowledge\n\n"
              (mapconcat #'identity (nreverse sections) "\n\n")))))

(defun hive-mcp-ai-inject-context (prompt &optional include-semantic)
  "Inject memory context into PROMPT.
If INCLUDE-SEMANTIC is non-nil, includes semantic search results.
Returns the augmented prompt string."
  (let ((context (hive-mcp-ai-build-context
                  (when include-semantic prompt))))
    (if context
        (concat context "\n\n---\n\n" prompt)
      prompt)))

;;;; Response Storage

(defun hive-mcp-ai-response-notable-p (response)
  "Check if RESPONSE is notable enough to store.
Returns non-nil if response should be stored."
  (and response
       (stringp response)
       (>= (length response) hive-mcp-ai-notable-min-length)
       ;; Contains code or structured content
       (or (string-match-p "```" response)
           (string-match-p "^[-*] " response)
           (string-match-p "^[0-9]+\\. " response))))

(defun hive-mcp-ai-store-response (response source &optional tags)
  "Store RESPONSE to memory if notable.
SOURCE is the AI package name (e.g., \"gptel\").
TAGS is optional list of additional tags."
  (when (and hive-mcp-ai-store-notable-responses
             (hive-mcp-ai--memory-available-p)
             (hive-mcp-ai-response-notable-p response))
    (let* ((truncated (if (> (length response) 2000)
                          (concat (substring response 0 1997) "...")
                        response))
           (all-tags (append (list source "ai-response") tags)))
      (hive-mcp-memory-add 'snippet truncated all-tags)
      (message "[ai-bridge] Stored notable response from %s" source))))

;;;; Interaction Logging

(defvar hive-mcp-ai--interaction-log nil
  "Log of recent AI interactions for audit.")

(defun hive-mcp-ai-log-interaction (source action &optional details)
  "Log an AI interaction.
SOURCE is the AI package, ACTION is what happened.
DETAILS is optional plist of additional info."
  (when hive-mcp-ai-log-interactions
    (push (list :timestamp (format-time-string "%FT%T%z")
                :source source
                :action action
                :details details)
          hive-mcp-ai--interaction-log)
    ;; Keep last 100 entries
    (when (> (length hive-mcp-ai--interaction-log) 100)
      (setq hive-mcp-ai--interaction-log
            (seq-take hive-mcp-ai--interaction-log 100)))))

(defun hive-mcp-ai-get-interaction-log (&optional limit)
  "Get recent interaction log entries.
LIMIT defaults to 20."
  (seq-take hive-mcp-ai--interaction-log (or limit 20)))

;;;; Swarm Bridge

(defun hive-mcp-ai--swarm-available-p ()
  "Check if swarm system is available."
  (and (require 'hive-mcp-swarm nil t)
       (fboundp 'hive-mcp-swarm-dispatch)))

(cl-defun hive-mcp-ai-dispatch-to-swarm (task &key preset slave-id _callback source)
  "Dispatch TASK to swarm agent.
PRESET is the agent preset (e.g., \"code-review\").
SLAVE-ID optionally targets a specific slave.
_CALLBACK is reserved for future async result handling.
SOURCE identifies the calling AI package.

Returns task-id on success, nil on failure."
  (when (hive-mcp-ai--swarm-available-p)
    (hive-mcp-ai-log-interaction (or source "ai-bridge")
                                   "swarm-dispatch"
                                   (list :task task :preset preset))
    (let ((target (or slave-id
                       ;; Find or spawn appropriate slave
                       (hive-mcp-ai--get-or-spawn-slave preset))))
      (when target
        (hive-mcp-swarm-dispatch target task)))))

(defun hive-mcp-ai--get-or-spawn-slave (preset)
  "Get an idle slave with PRESET or spawn one.
Returns slave-id or nil."
  (when (hive-mcp-ai--swarm-available-p)
    (let* ((status (hive-mcp-swarm-status))
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
        (when (fboundp 'hive-mcp-swarm-spawn)
          (hive-mcp-swarm-spawn
           (or preset "ai-worker")
           :presets (list preset)))))))

;;;; Format Adapters

(defun hive-mcp-ai-format-context-for (package context)
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

(defvar hive-mcp-ai-pre-request-hook nil
  "Hook run before any AI request.
Functions receive (source prompt) arguments.")

(defvar hive-mcp-ai-post-response-hook nil
  "Hook run after AI response.
Functions receive (source response) arguments.")

(defun hive-mcp-ai-run-pre-request-hooks (source prompt)
  "Run pre-request hooks for SOURCE with PROMPT.
Returns possibly modified prompt."
  (let ((result prompt))
    (dolist (fn hive-mcp-ai-pre-request-hook)
      (when-let* ((modified (funcall fn source result)))
        (setq result modified)))
    result))

(defun hive-mcp-ai-run-post-response-hooks (source response)
  "Run post-response hooks for SOURCE with RESPONSE."
  (dolist (fn hive-mcp-ai-post-response-hook)
    (funcall fn source response)))

;;;; Channel Events (for push-based updates)

(declare-function hive-mcp-channel-connected-p "hive-mcp-channel")
(declare-function hive-mcp-channel-send "hive-mcp-channel")

(defun hive-mcp-ai--channel-available-p ()
  "Check if channel is available for push events."
  (and (require 'hive-mcp-channel nil t)
       (fboundp 'hive-mcp-channel-connected-p)
       (hive-mcp-channel-connected-p)))

(defun hive-mcp-ai-emit-event (event-type data)
  "Emit AI bridge EVENT-TYPE with DATA through channel."
  (when (hive-mcp-ai--channel-available-p)
    (condition-case err
        (hive-mcp-channel-send
         `(("type" . ,(concat "ai-bridge:" event-type))
           ("timestamp" . ,(float-time))
           ,@data))
      (error
       (message "[ai-bridge] Channel emit error: %s" (error-message-string err))))))

;;;; API

(defun hive-mcp-ai-bridge-status ()
  "Get AI bridge status."
  (list :memory-available (hive-mcp-ai--memory-available-p)
        :swarm-available (hive-mcp-ai--swarm-available-p)
        :channel-available (hive-mcp-ai--channel-available-p)
        :log-entries (length hive-mcp-ai--interaction-log)
        :settings (list :conventions-limit hive-mcp-ai-context-conventions-limit
                        :decisions-limit hive-mcp-ai-context-decisions-limit
                        :store-notable hive-mcp-ai-store-notable-responses)))

(provide 'hive-mcp-ai-bridge)
;;; hive-mcp-ai-bridge.el ends here
