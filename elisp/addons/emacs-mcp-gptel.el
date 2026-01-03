;;; emacs-mcp-gptel.el --- gptel integration for emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, mcp, gptel
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Integration between emacs-mcp and gptel.
;;
;; OPTIONAL DEPENDENCIES:
;; - gptel (https://github.com/karthink/gptel)
;;
;; Features:
;; - Inject memory context into gptel prompts before sending
;; - Store notable gptel responses to memory
;; - Dispatch complex tasks to swarm agents
;; - Memory-aware system directives
;;
;; Usage:
;;   (emacs-mcp-gptel-mode 1)  ; Enable integration globally
;;
;; Or add to your config:
;;   (with-eval-after-load 'gptel
;;     (emacs-mcp-gptel-mode 1))
;;
;; Key Integration Points:
;; - gptel-prompt-filter-hook: Context injection before query
;; - gptel-post-response-functions: Response storage and logging
;; - gptel-directives: Memory-aware system prompts

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Soft dependencies
(declare-function gptel-send "gptel")
(declare-function gptel-request "gptel")
(declare-function emacs-mcp-ai-build-context "emacs-mcp-ai-bridge")
(declare-function emacs-mcp-ai-inject-context "emacs-mcp-ai-bridge")
(declare-function emacs-mcp-ai-store-response "emacs-mcp-ai-bridge")
(declare-function emacs-mcp-ai-log-interaction "emacs-mcp-ai-bridge")
(declare-function emacs-mcp-ai-dispatch-to-swarm "emacs-mcp-ai-bridge")
(declare-function emacs-mcp-ai-run-pre-request-hooks "emacs-mcp-ai-bridge")
(declare-function emacs-mcp-ai-run-post-response-hooks "emacs-mcp-ai-bridge")
(declare-function emacs-mcp-ai-format-context-for "emacs-mcp-ai-bridge")

(defvar gptel-directives)
(defvar gptel-prompt-filter-hook)
(defvar gptel-post-response-functions)
(defvar gptel-pre-response-hook)
(defvar gptel--system-message)

;;;; Customization

(defgroup emacs-mcp-gptel nil
  "Integration between emacs-mcp and gptel."
  :group 'emacs-mcp
  :prefix "emacs-mcp-gptel-")

(defcustom emacs-mcp-gptel-inject-context t
  "When non-nil, inject memory context into gptel prompts."
  :type 'boolean
  :group 'emacs-mcp-gptel)

(defcustom emacs-mcp-gptel-include-semantic t
  "When non-nil, include semantic search in context injection."
  :type 'boolean
  :group 'emacs-mcp-gptel)

(defcustom emacs-mcp-gptel-store-responses t
  "When non-nil, store notable gptel responses to memory."
  :type 'boolean
  :group 'emacs-mcp-gptel)

(defcustom emacs-mcp-gptel-log-interactions t
  "When non-nil, log gptel interactions for audit trail."
  :type 'boolean
  :group 'emacs-mcp-gptel)

(defcustom emacs-mcp-gptel-context-prefix "\n\n---\n## Project Context\n\n"
  "Prefix added before injected context."
  :type 'string
  :group 'emacs-mcp-gptel)

(defcustom emacs-mcp-gptel-swarm-tasks-pattern
  "\\(?:^\\|[[:space:]]\\)@swarm:\\s-*\\(.+\\)"
  "Pattern to detect swarm dispatch requests in prompts.
When matched, the task is dispatched to a swarm agent."
  :type 'regexp
  :group 'emacs-mcp-gptel)

;;;; Internal State

(defvar emacs-mcp-gptel--initialized nil
  "Whether the addon has been initialized.")

(defvar emacs-mcp-gptel--last-prompt nil
  "The last prompt sent, for response context.")

(defvar emacs-mcp-gptel--response-start nil
  "Marker for response start position.")

;;;; AI Bridge Integration

(defun emacs-mcp-gptel--ensure-bridge ()
  "Ensure emacs-mcp-ai-bridge is available."
  (or (featurep 'emacs-mcp-ai-bridge)
      (require 'emacs-mcp-ai-bridge nil t)))

(defun emacs-mcp-gptel--ensure-gptel ()
  "Ensure gptel is available."
  (or (featurep 'gptel)
      (require 'gptel nil t)))

;;;; Context Injection

(defun emacs-mcp-gptel--inject-context ()
  "Inject memory context into the current prompt buffer.
This runs in `gptel-prompt-filter-hook' before the query is sent."
  (when (and emacs-mcp-gptel-inject-context
             (emacs-mcp-gptel--ensure-bridge))
    (let* ((prompt-text (buffer-string))
           (context (emacs-mcp-ai-build-context
                     (when emacs-mcp-gptel-include-semantic
                       prompt-text))))
      (when context
        (goto-char (point-min))
        (insert (emacs-mcp-ai-format-context-for 'gptel context))
        (insert emacs-mcp-gptel-context-prefix))
      ;; Store prompt for response context
      (setq emacs-mcp-gptel--last-prompt prompt-text))))

;;;; Response Handling

(defun emacs-mcp-gptel--mark-response-start ()
  "Mark the start of gptel response.
This runs in `gptel-pre-response-hook'."
  (setq emacs-mcp-gptel--response-start (point-marker)))

(defun emacs-mcp-gptel--handle-response (beg end)
  "Handle gptel response between BEG and END.
This runs in `gptel-post-response-functions'."
  (when (emacs-mcp-gptel--ensure-bridge)
    (let ((response (buffer-substring-no-properties beg end)))
      ;; Log interaction
      (when emacs-mcp-gptel-log-interactions
        (emacs-mcp-ai-log-interaction
         "gptel" "response"
         (list :prompt-length (length (or emacs-mcp-gptel--last-prompt ""))
               :response-length (length response)
               :buffer (buffer-name))))

      ;; Store notable responses
      (when emacs-mcp-gptel-store-responses
        (emacs-mcp-ai-store-response response "gptel"))

      ;; Run post-response hooks
      (emacs-mcp-ai-run-post-response-hooks "gptel" response))))

;;;; Swarm Integration

(defun emacs-mcp-gptel--check-swarm-dispatch ()
  "Check for @swarm: directives in the current prompt.
Dispatches matching tasks to swarm agents."
  (when (and (emacs-mcp-gptel--ensure-bridge)
             (emacs-mcp-gptel--ensure-gptel))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward emacs-mcp-gptel-swarm-tasks-pattern nil t)
        (let ((task (match-string 1)))
          (emacs-mcp-ai-dispatch-to-swarm
           task
           :source "gptel"
           :preset "code-review"))))))

;;;; Memory-Aware Directives

(defun emacs-mcp-gptel-directive-with-context ()
  "Create a gptel directive that includes memory context.
Returns a cons cell suitable for `gptel-directives'."
  (when (emacs-mcp-gptel--ensure-bridge)
    (let ((context (emacs-mcp-ai-build-context)))
      (cons "Memory-Aware"
            (concat "You are a helpful assistant with access to project knowledge.\n\n"
                    (or context "")
                    "\n\nUse this context to provide informed, project-specific responses.")))))

;;;###autoload
(defun emacs-mcp-gptel-add-memory-directive ()
  "Add a memory-aware directive to `gptel-directives'."
  (interactive)
  (when (emacs-mcp-gptel--ensure-gptel)
    (when-let* ((directive (emacs-mcp-gptel-directive-with-context)))
      (add-to-list 'gptel-directives directive)
      (message "Added Memory-Aware directive to gptel-directives"))))

;;;; Interactive Commands

;;;###autoload
(defun emacs-mcp-gptel-send-with-context ()
  "Send prompt to gptel with memory context injection.
Like `gptel-send' but ensures context is injected."
  (interactive)
  (let ((emacs-mcp-gptel-inject-context t))
    (call-interactively #'gptel-send)))

;;;###autoload
(defun emacs-mcp-gptel-query-memory (query)
  "Query memory with QUERY and insert results at point."
  (interactive "sQuery memory: ")
  (when (emacs-mcp-gptel--ensure-bridge)
    (let ((results (emacs-mcp-ai-build-context query)))
      (if results
          (insert results)
        (message "No relevant memory found")))))

;;;###autoload
(defun emacs-mcp-gptel-store-region (beg end)
  "Store region between BEG and END as memory snippet."
  (interactive "r")
  (when (emacs-mcp-gptel--ensure-bridge)
    (let ((content (buffer-substring-no-properties beg end)))
      (emacs-mcp-ai-store-response content "gptel-manual"
                                    '("user-selected"))
      (message "Stored selection to memory"))))

;;;###autoload
(defun emacs-mcp-gptel-dispatch-to-swarm (task)
  "Dispatch TASK to swarm agent."
  (interactive "sTask for swarm: ")
  (when (emacs-mcp-gptel--ensure-bridge)
    (let ((result (emacs-mcp-ai-dispatch-to-swarm
                   task
                   :source "gptel-interactive"
                   :preset "general")))
      (if result
          (message "Dispatched to swarm: %s" result)
        (message "Swarm dispatch failed - is swarm available?")))))

;;;; Minor Mode

(defun emacs-mcp-gptel--setup-hooks ()
  "Set up gptel hooks for integration."
  (when (emacs-mcp-gptel--ensure-gptel)
    ;; Context injection before query
    (add-hook 'gptel-prompt-filter-hook #'emacs-mcp-gptel--inject-context)
    (add-hook 'gptel-prompt-filter-hook #'emacs-mcp-gptel--check-swarm-dispatch)

    ;; Response handling
    (add-hook 'gptel-pre-response-hook #'emacs-mcp-gptel--mark-response-start)
    (add-hook 'gptel-post-response-functions #'emacs-mcp-gptel--handle-response)))

(defun emacs-mcp-gptel--teardown-hooks ()
  "Remove gptel hooks."
  (remove-hook 'gptel-prompt-filter-hook #'emacs-mcp-gptel--inject-context)
  (remove-hook 'gptel-prompt-filter-hook #'emacs-mcp-gptel--check-swarm-dispatch)
  (remove-hook 'gptel-pre-response-hook #'emacs-mcp-gptel--mark-response-start)
  (remove-hook 'gptel-post-response-functions #'emacs-mcp-gptel--handle-response))

;;;###autoload
(define-minor-mode emacs-mcp-gptel-mode
  "Minor mode for emacs-mcp integration with gptel.

When enabled:
- Memory context is injected into prompts before sending
- Notable responses are stored to memory
- Interactions are logged for audit
- @swarm: directives dispatch tasks to agents"
  :init-value nil
  :lighter " MCP-GPT"
  :global t
  :group 'emacs-mcp-gptel
  (if emacs-mcp-gptel-mode
      (progn
        (emacs-mcp-gptel--setup-hooks)
        (setq emacs-mcp-gptel--initialized t)
        (message "emacs-mcp-gptel: enabled"))
    (emacs-mcp-gptel--teardown-hooks)
    (message "emacs-mcp-gptel: disabled")))

;;;; Transient Menu (optional)

(with-eval-after-load 'transient
  (transient-define-prefix emacs-mcp-gptel-menu ()
    "MCP integration menu for gptel."
    ["emacs-mcp + gptel"
     ["Context"
      ("c" "Send with context" emacs-mcp-gptel-send-with-context)
      ("q" "Query memory" emacs-mcp-gptel-query-memory)
      ("d" "Add memory directive" emacs-mcp-gptel-add-memory-directive)]
     ["Store"
      ("s" "Store region" emacs-mcp-gptel-store-region)]
     ["Swarm"
      ("w" "Dispatch to swarm" emacs-mcp-gptel-dispatch-to-swarm)]
     ["Toggle"
      ("m" "Toggle mode" emacs-mcp-gptel-mode)]]))

;;;; Addon Lifecycle

(defun emacs-mcp-gptel--addon-init ()
  "Initialize gptel addon."
  (require 'emacs-mcp-ai-bridge nil t)
  (message "emacs-mcp-gptel: initialized"))

(defun emacs-mcp-gptel--addon-shutdown ()
  "Shutdown gptel addon."
  (when emacs-mcp-gptel-mode
    (emacs-mcp-gptel-mode -1))
  (message "emacs-mcp-gptel: shutdown complete"))

;;;; Registration

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'gptel
   :version "0.1.0"
   :description "gptel integration with memory and swarm"
   :requires '(emacs-mcp-ai-bridge)
   :provides '(emacs-mcp-gptel-mode)
   :init #'emacs-mcp-gptel--addon-init
   :shutdown #'emacs-mcp-gptel--addon-shutdown))

(provide 'emacs-mcp-gptel)
;;; emacs-mcp-gptel.el ends here
