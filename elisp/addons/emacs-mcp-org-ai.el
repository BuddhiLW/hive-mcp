;;; emacs-mcp-org-ai.el --- Integrate org-ai with emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-ai "0.1.0"))
;; Keywords: tools, ai, org-mode, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates org-ai (AI chat in org-mode) with emacs-mcp.
;;
;; Features:
;; - Inject MCP context into org-ai prompts
;; - Save org-ai conversations to project memory
;; - Query memory for previous AI interactions
;; - Auto-include project context in AI chats
;; - Access emacs-mcp workflows from org-ai blocks
;;
;; Usage:
;;   (emacs-mcp-addon-load 'org-ai)
;;   (emacs-mcp-org-ai-mode 1)
;;
;; Or enable auto-loading when org-ai loads:
;;   (add-to-list 'emacs-mcp-addon-auto-load-list '(org-ai . org-ai))
;;   (emacs-mcp-addons-auto-load)
;;
;; In org-ai blocks, use:
;;   C-c C-c m - Insert MCP context at point
;;   C-c C-c s - Save conversation to memory
;;   C-c C-c q - Query similar conversations

;;; Code:

(require 'emacs-mcp-api)

;; Soft dependencies - don't error if org-ai isn't installed
(declare-function org-ai-prompt "org-ai")
(declare-function org-ai-on-region "org-ai")
(declare-function org-ai-switch-chat-model "org-ai")
(declare-function org-ai-stream-text "org-ai")

;;;; Customization

(defgroup emacs-mcp-org-ai nil
  "Integration between org-ai and emacs-mcp."
  :group 'emacs-mcp
  :group 'org-ai
  :prefix "emacs-mcp-org-ai-")

(defcustom emacs-mcp-org-ai-auto-context nil
  "When non-nil, automatically include MCP context in org-ai prompts.
Context includes buffer info, project details, git status, and relevant memory."
  :type 'boolean
  :group 'emacs-mcp-org-ai)

(defcustom emacs-mcp-org-ai-log-conversations t
  "When non-nil, save org-ai conversations to emacs-mcp memory.
This enables persistent conversation history per project."
  :type 'boolean
  :group 'emacs-mcp-org-ai)

(defcustom emacs-mcp-org-ai-context-format 'smart
  "Format for context injection.
- `compact': Single line summary
- `full': Complete context JSON
- `smart': Include only relevant sections"
  :type '(choice (const :tag "Compact summary" compact)
                 (const :tag "Full JSON context" full)
                 (const :tag "Smart selection" smart))
  :group 'emacs-mcp-org-ai)

(defcustom emacs-mcp-org-ai-include-memory t
  "When non-nil, include relevant memory entries in context."
  :type 'boolean
  :group 'emacs-mcp-org-ai)

;;;; Internal State

(defvar emacs-mcp-org-ai--available nil
  "Cached check for whether emacs-mcp-api is available.")

(defvar emacs-mcp-org-ai--last-conversation nil
  "Last org-ai conversation for potential saving.")

;;;; Utility Functions

(defun emacs-mcp-org-ai--available-p ()
  "Check if emacs-mcp-api is available."
  (or emacs-mcp-org-ai--available
      (setq emacs-mcp-org-ai--available
            (featurep 'emacs-mcp-api))))

(defun emacs-mcp-org-ai--ensure-available ()
  "Ensure emacs-mcp is available, error if not."
  (unless (emacs-mcp-org-ai--available-p)
    (if (require 'emacs-mcp-api nil t)
        (setq emacs-mcp-org-ai--available t)
      (error "emacs-mcp-api not available. Load emacs-mcp first"))))

(defun emacs-mcp-org-ai--format-context-compact (ctx)
  "Format CTX as compact one-line summary."
  (let ((buffer (plist-get ctx :buffer))
        (project (plist-get ctx :project))
        (git (plist-get ctx :git)))
    (format "[%s @ %s%s]"
            (or (plist-get buffer :name) "unknown")
            (or (plist-get project :name) "no-project")
            (if (plist-get git :dirty) " (modified)" ""))))

(defun emacs-mcp-org-ai--format-context-smart (ctx)
  "Format CTX with only relevant sections for org-ai."
  (let ((parts '())
        (buffer (plist-get ctx :buffer))
        (region (plist-get ctx :region))
        (defun-ctx (plist-get ctx :defun))
        (project (plist-get ctx :project))
        (git (plist-get ctx :git))
        (memory (plist-get ctx :memory)))
    ;; Current file and location
    (when buffer
      (push (format "File: %s (line %d)"
                    (or (plist-get buffer :file) (plist-get buffer :name))
                    (or (plist-get buffer :line) 1))
            parts))
    ;; Current function/section
    (when defun-ctx
      (push (format "Context: %s" (plist-get defun-ctx :name)) parts))
    ;; Selected region
    (when region
      (let ((text (plist-get region :text)))
        (when (and text (< (length text) 200))
          (push (format "Selection: %s" text) parts))))
    ;; Project info
    (when project
      (let ((name (plist-get project :name))
            (root (plist-get project :root)))
        (when name
          (push (format "Project: %s" name) parts))
        (when root
          (push (format "Root: %s" root) parts))))
    ;; Git status
    (when (and git (plist-get git :dirty))
      (let ((modified (plist-get git :modified))
            (branch (plist-get git :branch)))
        (when branch
          (push (format "Branch: %s" branch) parts))
        (when modified
          (push (format "Modified: %s"
                        (mapconcat #'identity (seq-take modified 3) ", "))
                parts))))
    ;; Recent memory (if enabled)
    (when (and emacs-mcp-org-ai-include-memory memory)
      (let ((notes (plist-get memory :notes))
            (snippets (plist-get memory :snippets)))
        (when (and notes (> (length notes) 0))
          (push (format "Recent notes: %d available" (length notes)) parts))
        (when (and snippets (> (length snippets) 0))
          (push (format "Code snippets: %d available" (length snippets)) parts))))
    (if parts
        (concat "# Project Context\n"
                (mapconcat (lambda (p) (concat "- " p)) (nreverse parts) "\n"))
      "")))

(defun emacs-mcp-org-ai--get-context-string ()
  "Get context string based on `emacs-mcp-org-ai-context-format'."
  (when (emacs-mcp-org-ai--available-p)
    (let ((ctx (emacs-mcp-api-get-context)))
      (pcase emacs-mcp-org-ai-context-format
        ('compact (claude-code-mcp--format-context-compact ctx))
        ('full (format "```json\n%s\n```" (json-encode ctx)))
        ('smart (emacs-mcp-org-ai--format-context-smart ctx))))))

;;;; Integration Commands

;;;###autoload
(defun emacs-mcp-org-ai-insert-context ()
  "Insert MCP context at point in org-ai block."
  (interactive)
  (emacs-mcp-org-ai--ensure-available)
  (let ((ctx (emacs-mcp-org-ai--get-context-string)))
    (when ctx
      (insert "\n" ctx "\n\n")
      (message "Inserted MCP context"))))

;;;###autoload
(defun emacs-mcp-org-ai-save-conversation ()
  "Save the current org-ai conversation to project memory."
  (interactive)
  (emacs-mcp-org-ai--ensure-available)
  (let* ((conversation (if (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                         (buffer-substring-no-properties
                          (point-min) (point-max))))
         (tags (split-string
                (read-string "Tags (comma-separated): " "org-ai,conversation")
                "," t " "))
         (summary (read-string "Summary (optional): " nil)))
    (let ((content (if (and summary (not (string-empty-p summary)))
                       (format "Summary: %s\n\n%s" summary conversation)
                     conversation)))
      (emacs-mcp-api-memory-add "conversation" content tags)
      (message "Saved conversation to memory"))))

;;;###autoload
(defun emacs-mcp-org-ai-query-conversations ()
  "Query previous org-ai conversations from memory."
  (interactive)
  (emacs-mcp-org-ai--ensure-available)
  (let* ((query (read-string "Search for: " nil))
         (results (emacs-mcp-api-memory-query "conversation" '("org-ai") 10))
         (buf (get-buffer-create "*MCP Org-AI Conversations*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Org-AI Conversation History\n\n")
      (if (= (length results) 0)
          (insert "No conversations found.\n")
        (dotimes (i (length results))
          (let* ((entry (aref results i))
                 (content (alist-get 'content entry))
                 (created (alist-get 'created entry))
                 (tags (alist-get 'tags entry)))
            (insert (format "* Conversation %d\n" (1+ i)))
            (insert (format ":PROPERTIES:\n"))
            (insert (format ":CREATED: %s\n" created))
            (when (and tags (> (length tags) 0))
              (insert (format ":TAGS: %s\n" (mapconcat #'identity tags ", "))))
            (insert ":END:\n\n")
            (insert content)
            (insert "\n\n"))))
      (goto-char (point-min)))
    (display-buffer buf)))

;;;###autoload
(defun emacs-mcp-org-ai-save-to-memory ()
  "Save selected text or region to emacs-mcp memory."
  (interactive)
  (emacs-mcp-org-ai--ensure-available)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (read-string "Content: ")))
         (type (completing-read "Type: " '("note" "snippet" "decision" "convention") nil t))
         (tags (split-string (read-string "Tags (comma-separated): " "org-ai") "," t " ")))
    (emacs-mcp-api-memory-add type text tags)
    (message "Saved to memory as %s" type)))

;;;###autoload
(defun emacs-mcp-org-ai-query-memory ()
  "Query emacs-mcp memory and insert results."
  (interactive)
  (emacs-mcp-org-ai--ensure-available)
  (let* ((type (completing-read "Query type: "
                                '("note" "snippet" "decision" "convention")
                                nil t "note"))
         (results (emacs-mcp-api-memory-query type nil 5)))
    (if (= (length results) 0)
        (message "No %s entries found" type)
      (let ((buf (get-buffer-create "*MCP Memory Results*")))
        (with-current-buffer buf
          (erase-buffer)
          (org-mode)
          (insert (format "#+TITLE: Memory: %s\n\n" type))
          (dotimes (i (length results))
            (let* ((entry (aref results i))
                   (content (alist-get 'content entry))
                   (tags (alist-get 'tags entry)))
              (insert (format "* Entry %d\n" (1+ i)))
              (when (and tags (> (length tags) 0))
                (insert (format "Tags: %s\n\n" (mapconcat #'identity tags ", "))))
              (insert content)
              (insert "\n\n")))
          (goto-char (point-min)))
        (display-buffer buf)))))

;;;###autoload
(defun emacs-mcp-org-ai-run-workflow ()
  "Select and run an emacs-mcp workflow."
  (interactive)
  (emacs-mcp-org-ai--ensure-available)
  (let* ((workflows (emacs-mcp-api-list-workflows))
         (names (mapcar (lambda (w) (alist-get 'name w)) workflows))
         (selected (completing-read "Run workflow: " names nil t)))
    (emacs-mcp-api-run-workflow selected)
    (message "Workflow '%s' executed" selected)))

;;;###autoload
(defun emacs-mcp-org-ai-show-context ()
  "Show current emacs-mcp context in a buffer."
  (interactive)
  (emacs-mcp-org-ai--ensure-available)
  (let* ((ctx (emacs-mcp-api-get-context))
         (formatted (emacs-mcp-org-ai--format-context-smart ctx))
         (buf (get-buffer-create "*MCP Context*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Current MCP Context\n\n")
      (insert formatted)
      (insert "\n\n* Full JSON\n\n")
      (insert "#+BEGIN_SRC json\n")
      (insert (json-encode ctx))
      (insert "\n#+END_SRC\n")
      (goto-char (point-min)))
    (display-buffer buf)))

;;;; Transient Menu

;;;###autoload (autoload 'emacs-mcp-org-ai-transient "emacs-mcp-org-ai" nil t)
(transient-define-prefix emacs-mcp-org-ai-transient ()
  "MCP integration menu for org-ai."
  ["emacs-mcp + org-ai"
   ["Context"
    ("c" "Show context" emacs-mcp-org-ai-show-context)
    ("i" "Insert context" emacs-mcp-org-ai-insert-context)]
   ["Memory"
    ("s" "Save conversation" emacs-mcp-org-ai-save-conversation)
    ("m" "Save to memory" emacs-mcp-org-ai-save-to-memory)
    ("q" "Query memory" emacs-mcp-org-ai-query-memory)
    ("h" "Query conversations" emacs-mcp-org-ai-query-conversations)]
   ["Workflows"
    ("w" "Run workflow" emacs-mcp-org-ai-run-workflow)]
   ["Settings"
    ("C" "Toggle auto-context" emacs-mcp-org-ai-toggle-auto-context)
    ("L" "Toggle logging" emacs-mcp-org-ai-toggle-logging)
    ("M" "Toggle memory inclusion" emacs-mcp-org-ai-toggle-memory)]])

;;;; Toggle Commands

(defun emacs-mcp-org-ai-toggle-auto-context ()
  "Toggle automatic context injection."
  (interactive)
  (setq emacs-mcp-org-ai-auto-context (not emacs-mcp-org-ai-auto-context))
  (message "Auto-context %s" (if emacs-mcp-org-ai-auto-context "enabled" "disabled")))

(defun emacs-mcp-org-ai-toggle-logging ()
  "Toggle conversation logging."
  (interactive)
  (setq emacs-mcp-org-ai-log-conversations (not emacs-mcp-org-ai-log-conversations))
  (message "Conversation logging %s"
           (if emacs-mcp-org-ai-log-conversations "enabled" "disabled")))

(defun emacs-mcp-org-ai-toggle-memory ()
  "Toggle memory inclusion in context."
  (interactive)
  (setq emacs-mcp-org-ai-include-memory (not emacs-mcp-org-ai-include-memory))
  (message "Memory inclusion %s"
           (if emacs-mcp-org-ai-include-memory "enabled" "disabled")))

;;;; Hooks and Advice

(defun emacs-mcp-org-ai--maybe-add-context (text)
  "Maybe add context to TEXT if auto-context is enabled."
  (if (and emacs-mcp-org-ai-auto-context
           (emacs-mcp-org-ai--available-p))
      (let ((ctx (emacs-mcp-org-ai--get-context-string)))
        (if (and ctx (not (string-empty-p ctx)))
            (format "%s\n\n%s" ctx text)
          text))
    text))

(defun emacs-mcp-org-ai--log-conversation (prompt response)
  "Log PROMPT and RESPONSE to conversation memory if logging is enabled."
  (when (and emacs-mcp-org-ai-log-conversations
             (emacs-mcp-org-ai--available-p))
    (ignore-errors
      (let ((content (format "** User\n%s\n\n** Assistant\n%s" prompt response)))
        (setq emacs-mcp-org-ai--last-conversation content)
        (emacs-mcp-api-conversation-log "org-ai" content)))))

;;;; Keymap for org-ai blocks

(defvar emacs-mcp-org-ai-block-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c m") 'emacs-mcp-org-ai-insert-context)
    (define-key map (kbd "C-c C-c s") 'emacs-mcp-org-ai-save-conversation)
    (define-key map (kbd "C-c C-c q") 'emacs-mcp-org-ai-query-memory)
    (define-key map (kbd "C-c C-c h") 'emacs-mcp-org-ai-query-conversations)
    (define-key map (kbd "C-c C-c w") 'emacs-mcp-org-ai-run-workflow)
    (define-key map (kbd "C-c C-c M") 'emacs-mcp-org-ai-transient)
    map)
  "Keymap for org-ai blocks with MCP integration.")

;;;; Minor Mode

;;;###autoload
(define-minor-mode emacs-mcp-org-ai-mode
  "Minor mode for emacs-mcp integration with org-ai.

When enabled, provides:
- Context injection for AI prompts
- Memory persistence integration
- Conversation history
- Workflow access

Key bindings in org-ai blocks:
  C-c C-c m - Insert context
  C-c C-c s - Save conversation
  C-c C-c q - Query memory
  C-c C-c M - Open transient menu"
  :init-value nil
  :lighter " MCP-AI"
  :global t
  :group 'emacs-mcp-org-ai
  (if emacs-mcp-org-ai-mode
      (progn
        ;; Try to load emacs-mcp-api
        (require 'emacs-mcp-api nil t)
        ;; Add keybindings to org-ai-mode if available
        (when (boundp 'org-ai-mode-map)
          (set-keymap-parent org-ai-mode-map emacs-mcp-org-ai-block-map))
        (message "emacs-mcp-org-ai enabled"))
    (message "emacs-mcp-org-ai disabled")))

;;;; Addon Registration

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'org-ai
   :version "0.1.0"
   :description "Integration with org-ai (AI chat in org-mode)"
   :requires '(org-ai emacs-mcp-api)
   :provides '(emacs-mcp-org-ai-mode emacs-mcp-org-ai-transient)))

(provide 'emacs-mcp-org-ai)
;;; emacs-mcp-org-ai.el ends here
