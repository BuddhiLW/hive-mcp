;;; emacs-mcp-claude-code.el --- Integrate claude-code.el with emacs-mcp  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (claude-code "0.4.0"))
;; Keywords: tools, ai, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This package integrates claude-code.el (the Emacs interface to Claude Code CLI)
;; with emacs-mcp (the MCP server for Claude).  It enables:
;;
;; - Automatic context injection when sending commands to Claude
;; - Memory persistence for important conversation snippets
;; - Unified transient menu for MCP operations
;; - Auto-logging of conversations to project memory
;;
;; Usage:
;;   (require 'emacs-mcp-claude-code)
;;   (emacs-mcp-claude-code-mode 1)
;;
;; This adds emacs-mcp integration to claude-code.el without modifying
;; the original package.

;;; Code:

(require 'claude-code)

;; Soft dependencies - load if available
(declare-function emacs-mcp-api-get-context "emacs-mcp-api")
(declare-function emacs-mcp-api-memory-add "emacs-mcp-api")
(declare-function emacs-mcp-api-memory-query "emacs-mcp-api")
(declare-function emacs-mcp-api-list-workflows "emacs-mcp-api")
(declare-function emacs-mcp-api-run-workflow "emacs-mcp-api")
(declare-function emacs-mcp-api-notify "emacs-mcp-api")
(declare-function emacs-mcp-api-conversation-log "emacs-mcp-api")
(declare-function emacs-mcp-api-capabilities "emacs-mcp-api")

;;;; Customization:

(defgroup emacs-mcp-claude-code nil
  "Integration between claude-code.el and emacs-mcp."
  :group 'claude-code
  :prefix "emacs-mcp-claude-code-")

(defcustom emacs-mcp-claude-code-auto-context nil
  "When non-nil, automatically include MCP context in commands.
Context includes buffer info, project, git status, and relevant memory."
  :type 'boolean
  :group 'emacs-mcp-claude-code)

(defcustom emacs-mcp-claude-code-log-conversations nil
  "When non-nil, log conversations to emacs-mcp memory.
This enables persistent conversation history per project."
  :type 'boolean
  :group 'emacs-mcp-claude-code)

(defcustom emacs-mcp-claude-code-context-format 'compact
  "Format for context injection.
- `compact': Single line summary
- `full': Complete context JSON
- `smart': Include only relevant sections"
  :type '(choice (const :tag "Compact summary" compact)
                 (const :tag "Full JSON context" full)
                 (const :tag "Smart selection" smart))
  :group 'emacs-mcp-claude-code)

(defcustom emacs-mcp-claude-code-notify-on-complete t
  "When non-nil, use emacs-mcp notifications when Claude completes."
  :type 'boolean
  :group 'emacs-mcp-claude-code)

;;;; Internal State:

(defvar emacs-mcp-claude-code--available nil
  "Cached check for whether emacs-mcp-api is available.")

;;;; Utility Functions:

(defun emacs-mcp-claude-code--available-p ()
  "Check if emacs-mcp-api is available."
  (or emacs-mcp-claude-code--available
      (setq emacs-mcp-claude-code--available
            (featurep 'emacs-mcp-api))))

(defun emacs-mcp-claude-code--ensure-available ()
  "Ensure emacs-mcp is available, error if not."
  (unless (emacs-mcp-claude-code--available-p)
    (if (require 'emacs-mcp-api nil t)
        (setq emacs-mcp-claude-code--available t)
      (error "Emacs-mcp-api not available.  Load emacs-mcp first"))))

(defun emacs-mcp-claude-code--format-context-compact (ctx)
  "Format CTX as compact one-line summary."
  (let ((buffer (plist-get ctx :buffer))
        (project (plist-get ctx :project))
        (git (plist-get ctx :git)))
    (format "[%s @ %s%s]"
            (or (plist-get buffer :name) "unknown")
            (or (plist-get project :name) "no-project")
            (if (plist-get git :dirty) " (modified)" ""))))

(defun emacs-mcp-claude-code--format-context-smart (ctx)
  "Format CTX with only relevant sections."
  (let ((parts '())
        (buffer (plist-get ctx :buffer))
        (region (plist-get ctx :region))
        (defun-ctx (plist-get ctx :defun))
        (project (plist-get ctx :project))
        (git (plist-get ctx :git))
        (memory (plist-get ctx :memory)))
    ;; Current location
    (when buffer
      (push (format "File: %s:%d"
                    (or (plist-get buffer :file) (plist-get buffer :name))
                    (or (plist-get buffer :line) 1))
            parts))
    ;; Region if active
    (when region
      (push (format "Selected: %s" (plist-get region :text)) parts))
    ;; Current function
    (when defun-ctx
      (push (format "In: %s" (plist-get defun-ctx :name)) parts))
    ;; Git status
    (when (and git (plist-get git :dirty))
      (let ((modified (plist-get git :modified)))
        (when modified
          (push (format "Modified files: %s"
                        (mapconcat #'identity (seq-take modified 3) ", "))
                parts))))
    ;; Recent notes
    (when memory
      (let ((notes (plist-get memory :notes)))
        (when (and notes (> (length notes) 0))
          (push (format "Recent notes: %d" (length notes)) parts))))
    (string-join (nreverse parts) "\n")))

(defun emacs-mcp-claude-code--get-context-string ()
  "Get context string based on `emacs-mcp-claude-code-context-format'."
  (when (emacs-mcp-claude-code--available-p)
    (let ((ctx (emacs-mcp-api-get-context)))
      (pcase emacs-mcp-claude-code-context-format
        ('compact (emacs-mcp-claude-code--format-context-compact ctx))
        ('full (json-encode ctx))
        ('smart (emacs-mcp-claude-code--format-context-smart ctx))))))

;;;; Integration Commands:

;;;###autoload
(defun emacs-mcp-claude-code-send-with-context ()
  "Read command and send to Claude with emacs-mcp context."
  (interactive)
  (emacs-mcp-claude-code--ensure-available)
  (let* ((ctx-string (emacs-mcp-claude-code--get-context-string))
         (cmd (read-string "Claude command: " nil 'claude-code-command-history))
         (full-cmd (if ctx-string
                       (format "%s\n\nContext:\n%s" cmd ctx-string)
                     cmd)))
    (claude-code--do-send-command full-cmd)))

;;;###autoload
(defun emacs-mcp-claude-code-save-to-memory ()
  "Save selected text or prompt to project memory."
  (interactive)
  (emacs-mcp-claude-code--ensure-available)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (read-string "Note content: ")))
         (type (completing-read "Type: " '("note" "snippet" "convention" "decision") nil t))
         (tags (split-string (read-string "Tags (comma-separated): ") "," t " ")))
    (emacs-mcp-api-memory-add type text tags)
    (message "Saved to project memory as %s" type)))

;;;###autoload
(defun emacs-mcp-claude-code-query-memory ()
  "Query project memory and display results."
  (interactive)
  (emacs-mcp-claude-code--ensure-available)
  (let* ((type (completing-read "Query type: "
                                '("note" "snippet" "convention" "decision" "conversation")
                                nil t "note"))
         (results (emacs-mcp-api-memory-query type nil 10))
         (buf (get-buffer-create "*MCP Memory*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "=== Project Memory: %s ===\n\n" type))
      (if (= (length results) 0)
          (insert "No entries found.\n")
        (dotimes (i (length results))
          (let* ((entry (aref results i))
                 (content (alist-get 'content entry))
                 (created (alist-get 'created entry))
                 (tags (alist-get 'tags entry)))
            (insert (format "--- Entry %d ---\n" (1+ i)))
            (insert (format "Created: %s\n" created))
            (when (and tags (> (length tags) 0))
              (insert (format "Tags: %s\n" (mapconcat #'identity tags ", "))))
            (insert (format "\n%s\n\n" content)))))
      (goto-char (point-min)))
    (display-buffer buf)))

;;;###autoload
(defun emacs-mcp-claude-code-run-workflow ()
  "Select and run an emacs-mcp workflow."
  (interactive)
  (emacs-mcp-claude-code--ensure-available)
  (let* ((workflows (emacs-mcp-api-list-workflows))
         (names (mapcar (lambda (w) (alist-get 'name w)) workflows))
         (selected (completing-read "Run workflow: " names nil t)))
    (emacs-mcp-api-run-workflow selected)
    (message "Workflow '%s' executed" selected)))

;;;###autoload
(defun emacs-mcp-claude-code-show-capabilities ()
  "Show emacs-mcp capabilities and status."
  (interactive)
  (if (emacs-mcp-claude-code--available-p)
      (let ((caps (emacs-mcp-api-capabilities)))
        (message "Emacs-mcp v%s: %s"
                 (plist-get caps :version)
                 (mapconcat #'symbol-name (plist-get caps :capabilities) ", ")))
    (message "Emacs-mcp is not loaded")))

;;;###autoload
(defun emacs-mcp-claude-code-get-context ()
  "Get and display current emacs-mcp context."
  (interactive)
  (emacs-mcp-claude-code--ensure-available)
  (let* ((ctx (emacs-mcp-api-get-context))
         (formatted (emacs-mcp-claude-code--format-context-smart ctx))
         (buf (get-buffer-create "*MCP Context*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Current Context ===\n\n")
      (insert formatted)
      (insert "\n\n=== Full JSON ===\n\n")
      (insert (json-encode ctx))
      (goto-char (point-min))
      (when (fboundp 'json-mode) (json-mode)))
    (display-buffer buf)))

;;;; Transient Menu:

;;;###autoload (autoload 'emacs-mcp-claude-code-transient "emacs-mcp-claude-code" nil t)
(transient-define-prefix emacs-mcp-claude-code-transient ()
  "MCP integration menu for Claude Code."
  ["emacs-mcp Integration"
   ["Context & Memory"
    ("c" "Show context" emacs-mcp-claude-code-get-context)
    ("s" "Send with context" emacs-mcp-claude-code-send-with-context)
    ("m" "Save to memory" emacs-mcp-claude-code-save-to-memory)
    ("q" "Query memory" emacs-mcp-claude-code-query-memory)]
   ["Workflows & Status"
    ("w" "Run workflow" emacs-mcp-claude-code-run-workflow)
    ("?" "Show capabilities" emacs-mcp-claude-code-show-capabilities)]
   ["Settings"
    ("C" "Toggle auto-context" emacs-mcp-claude-code-toggle-auto-context)
    ("L" "Toggle conversation logging" emacs-mcp-claude-code-toggle-logging)]])

;;;; Toggle Commands:

(defun emacs-mcp-claude-code-toggle-auto-context ()
  "Toggle automatic context injection."
  (interactive)
  (setq emacs-mcp-claude-code-auto-context (not emacs-mcp-claude-code-auto-context))
  (message "Auto-context %s" (if emacs-mcp-claude-code-auto-context "enabled" "disabled")))

(defun emacs-mcp-claude-code-toggle-logging ()
  "Toggle conversation logging."
  (interactive)
  (setq emacs-mcp-claude-code-log-conversations (not emacs-mcp-claude-code-log-conversations))
  (message "Conversation logging %s" (if emacs-mcp-claude-code-log-conversations "enabled" "disabled")))

;;;; Hooks and Advice:

(defun emacs-mcp-claude-code--maybe-add-context (cmd)
  "Maybe add context to CMD if auto-context is enabled."
  (if (and emacs-mcp-claude-code-auto-context
           (emacs-mcp-claude-code--available-p))
      (let ((ctx (emacs-mcp-claude-code--get-context-string)))
        (if ctx
            (format "%s\n\n[Context: %s]" cmd ctx)
          cmd))
    cmd))

(defun emacs-mcp-claude-code--log-command (cmd)
  "Log CMD to conversation memory if logging is enabled."
  (when (and emacs-mcp-claude-code-log-conversations
             (emacs-mcp-claude-code--available-p))
    (ignore-errors
      (emacs-mcp-api-conversation-log "user" cmd))))

(defun emacs-mcp-claude-code--notification-function (title message)
  "Use emacs-mcp for notifications if available.
TITLE and MESSAGE are passed to the notification."
  (if (and emacs-mcp-claude-code-notify-on-complete
           (emacs-mcp-claude-code--available-p))
      (emacs-mcp-api-notify (format "%s: %s" title message) "info")
    ;; Fall back to default
    (claude-code-default-notification title message)))

;;;; Advice Functions:

(defun emacs-mcp-claude-code--advise-send-command (orig-fun cmd)
  "Advice for `claude-code--do-send-command' to add context and logging.
ORIG-FUN is the original function, CMD is the command."
  (let ((enhanced-cmd (emacs-mcp-claude-code--maybe-add-context cmd)))
    (emacs-mcp-claude-code--log-command cmd)
    (funcall orig-fun enhanced-cmd)))

;;;; Keymap Extensions:

(defvar emacs-mcp-claude-code-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'emacs-mcp-claude-code-get-context)
    (define-key map (kbd "s") #'emacs-mcp-claude-code-send-with-context)
    (define-key map (kbd "m") #'emacs-mcp-claude-code-save-to-memory)
    (define-key map (kbd "q") #'emacs-mcp-claude-code-query-memory)
    (define-key map (kbd "w") #'emacs-mcp-claude-code-run-workflow)
    (define-key map (kbd "M") #'emacs-mcp-claude-code-transient)
    map)
  "Keymap for emacs-mcp-claude-code commands.")

;;;; Minor Mode:

;;;###autoload
(define-minor-mode emacs-mcp-claude-code-mode
  "Minor mode for emacs-mcp integration with claude-code.el.

When enabled, provides:
- Context injection for Claude commands
- Memory persistence integration
- Workflow access
- Conversation logging

Key bindings under `C-c c m' prefix (customizable)."
  :init-value nil
  :lighter " MCP"
  :global t
  :group 'emacs-mcp-claude-code
  (if emacs-mcp-claude-code-mode
      (progn
        ;; Add advice for context injection
        (advice-add 'claude-code--do-send-command :around
                    #'emacs-mcp-claude-code--advise-send-command)
        ;; Try to load emacs-mcp-api
        (require 'emacs-mcp-api nil t)
        ;; Extend claude-code command map
        (define-key claude-code-command-map (kbd "M") #'emacs-mcp-claude-code-transient)
        (message "Emacs-mcp-claude-code enabled"))
    ;; Remove advice
    (advice-remove 'claude-code--do-send-command
                   #'emacs-mcp-claude-code--advise-send-command)
    (message "Emacs-mcp-claude-code disabled")))

;;;; Addon Registration:

(eval-after-load 'emacs-mcp-addons
  '(emacs-mcp-addon-register
    'claude-code
    :version "0.1.0"
    :description "Integration with claude-code.el (Claude Code CLI)"
    :requires '(claude-code emacs-mcp-api)
    :provides '(emacs-mcp-claude-code-mode emacs-mcp-claude-code-transient)))

(provide 'emacs-mcp-claude-code)
;;; emacs-mcp-claude-code.el ends here
