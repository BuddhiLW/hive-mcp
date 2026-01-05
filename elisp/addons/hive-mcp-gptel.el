;;; hive-mcp-gptel.el --- gptel integration for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, mcp, gptel
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Integration between hive-mcp and gptel.
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
;;   (hive-mcp-gptel-mode 1)  ; Enable integration globally
;;
;; Or add to your config:
;;   (with-eval-after-load 'gptel
;;     (hive-mcp-gptel-mode 1))
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
(declare-function hive-mcp-ai-build-context "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-inject-context "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-store-response "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-log-interaction "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-dispatch-to-swarm "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-run-pre-request-hooks "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-run-post-response-hooks "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-format-context-for "hive-mcp-ai-bridge")

(defvar gptel-directives)
(defvar gptel-prompt-filter-hook)
(defvar gptel-post-response-functions)
(defvar gptel-pre-response-hook)
(defvar gptel--system-message)

;;;; Customization

(defgroup hive-mcp-gptel nil
  "Integration between hive-mcp and gptel."
  :group 'hive-mcp
  :prefix "hive-mcp-gptel-")

(defcustom hive-mcp-gptel-inject-context t
  "When non-nil, inject memory context into gptel prompts."
  :type 'boolean
  :group 'hive-mcp-gptel)

(defcustom hive-mcp-gptel-include-semantic t
  "When non-nil, include semantic search in context injection."
  :type 'boolean
  :group 'hive-mcp-gptel)

(defcustom hive-mcp-gptel-store-responses t
  "When non-nil, store notable gptel responses to memory."
  :type 'boolean
  :group 'hive-mcp-gptel)

(defcustom hive-mcp-gptel-log-interactions t
  "When non-nil, log gptel interactions for audit trail."
  :type 'boolean
  :group 'hive-mcp-gptel)

(defcustom hive-mcp-gptel-context-prefix "\n\n---\n## Project Context\n\n"
  "Prefix added before injected context."
  :type 'string
  :group 'hive-mcp-gptel)

(defcustom hive-mcp-gptel-swarm-tasks-pattern
  "\\(?:^\\|[[:space:]]\\)@swarm:\\s-*\\(.+\\)"
  "Pattern to detect swarm dispatch requests in prompts.
When matched, the task is dispatched to a swarm agent."
  :type 'regexp
  :group 'hive-mcp-gptel)

;;;; Internal State

(defvar hive-mcp-gptel--initialized nil
  "Whether the addon has been initialized.")

(defvar hive-mcp-gptel--last-prompt nil
  "The last prompt sent, for response context.")

(defvar hive-mcp-gptel--response-start nil
  "Marker for response start position.")

;;;; AI Bridge Integration

(defun hive-mcp-gptel--ensure-bridge ()
  "Ensure hive-mcp-ai-bridge is available."
  (or (featurep 'hive-mcp-ai-bridge)
      (require 'hive-mcp-ai-bridge nil t)))

(defun hive-mcp-gptel--ensure-gptel ()
  "Ensure gptel is available."
  (or (featurep 'gptel)
      (require 'gptel nil t)))

;;;; Context Injection

(defun hive-mcp-gptel--inject-context ()
  "Inject memory context into the current prompt buffer.
This runs in `gptel-prompt-filter-hook' before the query is sent."
  (when (and hive-mcp-gptel-inject-context
             (hive-mcp-gptel--ensure-bridge))
    (let* ((prompt-text (buffer-string))
           (context (hive-mcp-ai-build-context
                     (when hive-mcp-gptel-include-semantic
                       prompt-text))))
      (when context
        (goto-char (point-min))
        (insert (hive-mcp-ai-format-context-for 'gptel context))
        (insert hive-mcp-gptel-context-prefix))
      ;; Store prompt for response context
      (setq hive-mcp-gptel--last-prompt prompt-text))))

;;;; Response Handling

(defun hive-mcp-gptel--mark-response-start ()
  "Mark the start of gptel response.
This runs in `gptel-pre-response-hook'."
  (setq hive-mcp-gptel--response-start (point-marker)))

(defun hive-mcp-gptel--handle-response (beg end)
  "Handle gptel response between BEG and END.
This runs in `gptel-post-response-functions'."
  (when (hive-mcp-gptel--ensure-bridge)
    (let ((response (buffer-substring-no-properties beg end)))
      ;; Log interaction
      (when hive-mcp-gptel-log-interactions
        (hive-mcp-ai-log-interaction
         "gptel" "response"
         (list :prompt-length (length (or hive-mcp-gptel--last-prompt ""))
               :response-length (length response)
               :buffer (buffer-name))))

      ;; Store notable responses
      (when hive-mcp-gptel-store-responses
        (hive-mcp-ai-store-response response "gptel"))

      ;; Run post-response hooks
      (hive-mcp-ai-run-post-response-hooks "gptel" response))))

;;;; Swarm Integration

(defun hive-mcp-gptel--check-swarm-dispatch ()
  "Check for @swarm: directives in the current prompt.
Dispatches matching tasks to swarm agents."
  (when (and (hive-mcp-gptel--ensure-bridge)
             (hive-mcp-gptel--ensure-gptel))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward hive-mcp-gptel-swarm-tasks-pattern nil t)
        (let ((task (match-string 1)))
          (hive-mcp-ai-dispatch-to-swarm
           task
           :source "gptel"
           :preset "code-review"))))))

;;;; Memory-Aware Directives

(defun hive-mcp-gptel-directive-with-context ()
  "Create a gptel directive that includes memory context.
Returns a cons cell suitable for `gptel-directives'."
  (when (hive-mcp-gptel--ensure-bridge)
    (let ((context (hive-mcp-ai-build-context)))
      (cons "Memory-Aware"
            (concat "You are a helpful assistant with access to project knowledge.\n\n"
                    (or context "")
                    "\n\nUse this context to provide informed, project-specific responses.")))))

;;;###autoload
(defun hive-mcp-gptel-add-memory-directive ()
  "Add a memory-aware directive to `gptel-directives'."
  (interactive)
  (when (hive-mcp-gptel--ensure-gptel)
    (when-let* ((directive (hive-mcp-gptel-directive-with-context)))
      (add-to-list 'gptel-directives directive)
      (message "Added Memory-Aware directive to gptel-directives"))))

;;;; Interactive Commands

;;;###autoload
(defun hive-mcp-gptel-send-with-context ()
  "Send prompt to gptel with memory context injection.
Like `gptel-send' but ensures context is injected."
  (interactive)
  (let ((hive-mcp-gptel-inject-context t))
    (call-interactively #'gptel-send)))

;;;###autoload
(defun hive-mcp-gptel-query-memory (query)
  "Query memory with QUERY and insert results at point."
  (interactive "sQuery memory: ")
  (when (hive-mcp-gptel--ensure-bridge)
    (let ((results (hive-mcp-ai-build-context query)))
      (if results
          (insert results)
        (message "No relevant memory found")))))

;;;###autoload
(defun hive-mcp-gptel-store-region (beg end)
  "Store region between BEG and END as memory snippet."
  (interactive "r")
  (when (hive-mcp-gptel--ensure-bridge)
    (let ((content (buffer-substring-no-properties beg end)))
      (hive-mcp-ai-store-response content "gptel-manual"
                                    '("user-selected"))
      (message "Stored selection to memory"))))

;;;###autoload
(defun hive-mcp-gptel-dispatch-to-swarm (task)
  "Dispatch TASK to swarm agent."
  (interactive "sTask for swarm: ")
  (when (hive-mcp-gptel--ensure-bridge)
    (let ((result (hive-mcp-ai-dispatch-to-swarm
                   task
                   :source "gptel-interactive"
                   :preset "general")))
      (if result
          (message "Dispatched to swarm: %s" result)
        (message "Swarm dispatch failed - is swarm available?")))))

;;;; Minor Mode

(defun hive-mcp-gptel--setup-hooks ()
  "Set up gptel hooks for integration."
  (when (hive-mcp-gptel--ensure-gptel)
    ;; Context injection before query
    (add-hook 'gptel-prompt-filter-hook #'hive-mcp-gptel--inject-context)
    (add-hook 'gptel-prompt-filter-hook #'hive-mcp-gptel--check-swarm-dispatch)

    ;; Response handling
    (add-hook 'gptel-pre-response-hook #'hive-mcp-gptel--mark-response-start)
    (add-hook 'gptel-post-response-functions #'hive-mcp-gptel--handle-response)))

(defun hive-mcp-gptel--teardown-hooks ()
  "Remove gptel hooks."
  (remove-hook 'gptel-prompt-filter-hook #'hive-mcp-gptel--inject-context)
  (remove-hook 'gptel-prompt-filter-hook #'hive-mcp-gptel--check-swarm-dispatch)
  (remove-hook 'gptel-pre-response-hook #'hive-mcp-gptel--mark-response-start)
  (remove-hook 'gptel-post-response-functions #'hive-mcp-gptel--handle-response))

;;;###autoload
(define-minor-mode hive-mcp-gptel-mode
  "Minor mode for hive-mcp integration with gptel.

When enabled:
- Memory context is injected into prompts before sending
- Notable responses are stored to memory
- Interactions are logged for audit
- @swarm: directives dispatch tasks to agents"
  :init-value nil
  :lighter " MCP-GPT"
  :global t
  :group 'hive-mcp-gptel
  (if hive-mcp-gptel-mode
      (progn
        (hive-mcp-gptel--setup-hooks)
        (setq hive-mcp-gptel--initialized t)
        (message "hive-mcp-gptel: enabled"))
    (hive-mcp-gptel--teardown-hooks)
    (message "hive-mcp-gptel: disabled")))

;;;; Transient Menu (optional)

(with-eval-after-load 'transient
  (transient-define-prefix hive-mcp-gptel-menu ()
    "MCP integration menu for gptel."
    ["hive-mcp + gptel"
     ["Context"
      ("c" "Send with context" hive-mcp-gptel-send-with-context)
      ("q" "Query memory" hive-mcp-gptel-query-memory)
      ("d" "Add memory directive" hive-mcp-gptel-add-memory-directive)]
     ["Store"
      ("s" "Store region" hive-mcp-gptel-store-region)]
     ["Swarm"
      ("w" "Dispatch to swarm" hive-mcp-gptel-dispatch-to-swarm)]
     ["Toggle"
      ("m" "Toggle mode" hive-mcp-gptel-mode)]]))

;;;; Addon Lifecycle

(defun hive-mcp-gptel--addon-init ()
  "Initialize gptel addon."
  (require 'hive-mcp-ai-bridge nil t)
  (message "hive-mcp-gptel: initialized"))

(defun hive-mcp-gptel--addon-shutdown ()
  "Shutdown gptel addon."
  (when hive-mcp-gptel-mode
    (hive-mcp-gptel-mode -1))
  (message "hive-mcp-gptel: shutdown complete"))

;;;; Registration

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'gptel
   :version "0.1.0"
   :description "gptel integration with memory and swarm"
   :requires '(hive-mcp-ai-bridge)
   :provides '(hive-mcp-gptel-mode)
   :init #'hive-mcp-gptel--addon-init
   :shutdown #'hive-mcp-gptel--addon-shutdown))

(provide 'hive-mcp-gptel)
;;; hive-mcp-gptel.el ends here
