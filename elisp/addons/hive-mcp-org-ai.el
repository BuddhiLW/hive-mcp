;;; hive-mcp-org-ai.el --- Integrate org-ai with hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, org-mode, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates org-ai (AI chat in org-mode) with hive-mcp.
;;
;; OPTIONAL DEPENDENCIES:
;; - org-ai (for AI chat in org-mode integration)
;;
;; This addon activates automatically when org-ai is loaded.
;; Without org-ai, the addon is silently disabled.
;;
;; Features:
;; - Inject MCP context into org-ai prompts
;; - Save org-ai conversations to project memory
;; - Query memory for previous AI interactions
;; - Auto-include project context in AI chats
;; - Access hive-mcp workflows from org-ai blocks
;;
;; Usage:
;;   (hive-mcp-addon-load 'org-ai)
;;   (hive-mcp-org-ai-mode 1)
;;
;; Or enable auto-loading when org-ai loads:
;;   (add-to-list 'hive-mcp-addon-auto-load-list '(org-ai . org-ai))
;;   (hive-mcp-addons-auto-load)
;;
;; In org-ai blocks, use:
;;   C-c C-c m - Insert MCP context at point
;;   C-c C-c s - Save conversation to memory
;;   C-c C-c q - Query similar conversations

;;; Code:

;; Only require hive-mcp-api if available
(require 'hive-mcp-api nil t)

;; Soft dependencies - don't error if org-ai isn't installed
(declare-function org-ai-prompt "org-ai")
(declare-function org-ai-on-region "org-ai")
(declare-function org-ai-switch-chat-model "org-ai")
(declare-function org-ai-stream-text "org-ai")

;;;; Customization:

(defgroup hive-mcp-org-ai nil
  "Integration between org-ai and hive-mcp."
  :group 'hive-mcp
  :group 'org-ai
  :prefix "hive-mcp-org-ai-")

(defcustom hive-mcp-org-ai-auto-context nil
  "When non-nil, automatically include MCP context in org-ai prompts.
Context includes buffer info, project details, git status, and relevant memory."
  :type 'boolean
  :group 'hive-mcp-org-ai)

(defcustom hive-mcp-org-ai-log-conversations t
  "When non-nil, save org-ai conversations to hive-mcp memory.
This enables persistent conversation history per project."
  :type 'boolean
  :group 'hive-mcp-org-ai)

(defcustom hive-mcp-org-ai-context-format 'smart
  "Format for context injection.
- `compact': Single line summary
- `full': Complete context JSON
- `smart': Include only relevant sections"
  :type '(choice (const :tag "Compact summary" compact)
                 (const :tag "Full JSON context" full)
                 (const :tag "Smart selection" smart))
  :group 'hive-mcp-org-ai)

(defcustom hive-mcp-org-ai-include-memory t
  "When non-nil, include relevant memory entries in context."
  :type 'boolean
  :group 'hive-mcp-org-ai)

;;;; Internal State:

(defvar hive-mcp-org-ai--available nil
  "Cached check for whether hive-mcp-api is available.")

(defvar hive-mcp-org-ai--last-conversation nil
  "Last org-ai conversation for potential saving.")

;;;; Utility Functions:

(defun hive-mcp-org-ai--available-p ()
  "Check if hive-mcp-api is available."
  (or hive-mcp-org-ai--available
      (setq hive-mcp-org-ai--available
            (featurep 'hive-mcp-api))))

(defun hive-mcp-org-ai--ensure-available ()
  "Ensure hive-mcp is available, error if not."
  (unless (hive-mcp-org-ai--available-p)
    (if (require 'hive-mcp-api nil t)
        (setq hive-mcp-org-ai--available t)
      (error "Emacs-mcp-api not available.  Load hive-mcp first"))))

(defun hive-mcp-org-ai--format-context-compact (ctx)
  "Format CTX as compact one-line summary."
  (let ((buffer (plist-get ctx :buffer))
        (project (plist-get ctx :project))
        (git (plist-get ctx :git)))
    (format "[%s @ %s%s]"
            (or (plist-get buffer :name) "unknown")
            (or (plist-get project :name) "no-project")
            (if (plist-get git :dirty) " (modified)" ""))))

(defun hive-mcp-org-ai--format-context-smart (ctx)
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
    (when (and hive-mcp-org-ai-include-memory memory)
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

(defun hive-mcp-org-ai--get-context-string ()
  "Get context string based on `hive-mcp-org-ai-context-format'."
  (when (hive-mcp-org-ai--available-p)
    (let ((ctx (hive-mcp-api-get-context)))
      (pcase hive-mcp-org-ai-context-format
        ('compact (hive-mcp-claude-code--format-context-compact ctx))
        ('full (format "```json\n%s\n```" (json-encode ctx)))
        ('smart (hive-mcp-org-ai--format-context-smart ctx))))))

;;;; Integration Commands:

;;;###autoload
(defun hive-mcp-org-ai-insert-context ()
  "Insert MCP context at point in org-ai block."
  (interactive)
  (hive-mcp-org-ai--ensure-available)
  (let ((ctx (hive-mcp-org-ai--get-context-string)))
    (when ctx
      (insert "\n" ctx "\n\n")
      (message "Inserted MCP context"))))

;;;###autoload
(defun hive-mcp-org-ai-save-conversation ()
  "Save the current org-ai conversation to project memory."
  (interactive)
  (hive-mcp-org-ai--ensure-available)
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
      (hive-mcp-api-memory-add "conversation" content tags)
      (message "Saved conversation to memory"))))

;;;###autoload
(defun hive-mcp-org-ai-query-conversations ()
  "Query previous org-ai conversations from memory."
  (interactive)
  (hive-mcp-org-ai--ensure-available)
  (let* ((query (read-string "Search for: " nil))
         (results (hive-mcp-api-memory-query "conversation" '("org-ai") 10))
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
(defun hive-mcp-org-ai-save-to-memory ()
  "Save selected text or region to hive-mcp memory."
  (interactive)
  (hive-mcp-org-ai--ensure-available)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (read-string "Content: ")))
         (type (completing-read "Type: " '("note" "snippet" "decision" "convention") nil t))
         (tags (split-string (read-string "Tags (comma-separated): " "org-ai") "," t " ")))
    (hive-mcp-api-memory-add type text tags)
    (message "Saved to memory as %s" type)))

;;;###autoload
(defun hive-mcp-org-ai-query-memory ()
  "Query hive-mcp memory and insert results."
  (interactive)
  (hive-mcp-org-ai--ensure-available)
  (let* ((type (completing-read "Query type: "
                                '("note" "snippet" "decision" "convention")
                                nil t "note"))
         (results (hive-mcp-api-memory-query type nil 5)))
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
(defun hive-mcp-org-ai-run-workflow ()
  "Select and run an hive-mcp workflow."
  (interactive)
  (hive-mcp-org-ai--ensure-available)
  (let* ((workflows (hive-mcp-api-list-workflows))
         (names (mapcar (lambda (w) (alist-get 'name w)) workflows))
         (selected (completing-read "Run workflow: " names nil t)))
    (hive-mcp-api-run-workflow selected)
    (message "Workflow '%s' executed" selected)))

;;;###autoload
(defun hive-mcp-org-ai-show-context ()
  "Show current hive-mcp context in a buffer."
  (interactive)
  (hive-mcp-org-ai--ensure-available)
  (let* ((ctx (hive-mcp-api-get-context))
         (formatted (hive-mcp-org-ai--format-context-smart ctx))
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

;;;; Transient Menu:

;;;###autoload (autoload 'hive-mcp-org-ai-transient "hive-mcp-org-ai" nil t)
(transient-define-prefix hive-mcp-org-ai-transient ()
  "MCP integration menu for org-ai."
  ["hive-mcp + org-ai"
   ["Context"
    ("c" "Show context" hive-mcp-org-ai-show-context)
    ("i" "Insert context" hive-mcp-org-ai-insert-context)]
   ["Memory"
    ("s" "Save conversation" hive-mcp-org-ai-save-conversation)
    ("m" "Save to memory" hive-mcp-org-ai-save-to-memory)
    ("q" "Query memory" hive-mcp-org-ai-query-memory)
    ("h" "Query conversations" hive-mcp-org-ai-query-conversations)]
   ["Workflows"
    ("w" "Run workflow" hive-mcp-org-ai-run-workflow)]
   ["Settings"
    ("C" "Toggle auto-context" hive-mcp-org-ai-toggle-auto-context)
    ("L" "Toggle logging" hive-mcp-org-ai-toggle-logging)
    ("M" "Toggle memory inclusion" hive-mcp-org-ai-toggle-memory)]])

;;;; Toggle Commands:

(defun hive-mcp-org-ai-toggle-auto-context ()
  "Toggle automatic context injection."
  (interactive)
  (setq hive-mcp-org-ai-auto-context (not hive-mcp-org-ai-auto-context))
  (message "Auto-context %s" (if hive-mcp-org-ai-auto-context "enabled" "disabled")))

(defun hive-mcp-org-ai-toggle-logging ()
  "Toggle conversation logging."
  (interactive)
  (setq hive-mcp-org-ai-log-conversations (not hive-mcp-org-ai-log-conversations))
  (message "Conversation logging %s"
           (if hive-mcp-org-ai-log-conversations "enabled" "disabled")))

(defun hive-mcp-org-ai-toggle-memory ()
  "Toggle memory inclusion in context."
  (interactive)
  (setq hive-mcp-org-ai-include-memory (not hive-mcp-org-ai-include-memory))
  (message "Memory inclusion %s"
           (if hive-mcp-org-ai-include-memory "enabled" "disabled")))

;;;; Hooks and Advice:

(defun hive-mcp-org-ai--maybe-add-context (text)
  "Maybe add context to TEXT if auto-context is enabled."
  (if (and hive-mcp-org-ai-auto-context
           (hive-mcp-org-ai--available-p))
      (let ((ctx (hive-mcp-org-ai--get-context-string)))
        (if (and ctx (not (string-empty-p ctx)))
            (format "%s\n\n%s" ctx text)
          text))
    text))

(defun hive-mcp-org-ai--log-conversation (prompt response)
  "Log PROMPT and RESPONSE to conversation memory if logging is enabled."
  (when (and hive-mcp-org-ai-log-conversations
             (hive-mcp-org-ai--available-p))
    (ignore-errors
      (let ((content (format "** User\n%s\n\n** Assistant\n%s" prompt response)))
        (setq hive-mcp-org-ai--last-conversation content)
        (hive-mcp-api-conversation-log "org-ai" content)))))

;;;; Keymap for org-ai blocks:

(defvar hive-mcp-org-ai-block-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c m") 'hive-mcp-org-ai-insert-context)
    (define-key map (kbd "C-c C-c s") 'hive-mcp-org-ai-save-conversation)
    (define-key map (kbd "C-c C-c q") 'hive-mcp-org-ai-query-memory)
    (define-key map (kbd "C-c C-c h") 'hive-mcp-org-ai-query-conversations)
    (define-key map (kbd "C-c C-c w") 'hive-mcp-org-ai-run-workflow)
    (define-key map (kbd "C-c C-c M") 'hive-mcp-org-ai-transient)
    map)
  "Keymap for org-ai blocks with MCP integration.")

;;;; Minor Mode:

;;;###autoload
(define-minor-mode hive-mcp-org-ai-mode
  "Minor mode for hive-mcp integration with org-ai.

When enabled, provides:
- Context injection for AI prompts
- Memory persistence integration
- Conversation history
- Workflow access

Key bindings in org-ai blocks (see `hive-mcp-org-ai-block-map'):
  \\[hive-mcp-org-ai-insert-context] - Insert context
  \\[hive-mcp-org-ai-save-conversation] - Save conversation
  \\[hive-mcp-org-ai-query-memory] - Query memory
  \\[hive-mcp-org-ai-transient] - Open transient menu

Requires `org-ai' package to be installed."
  :init-value nil
  :lighter " MCP-AI"
  :global t
  :group 'hive-mcp-org-ai
  (if hive-mcp-org-ai-mode
      (if (not (featurep 'org-ai))
          (progn
            (setq hive-mcp-org-ai-mode nil)
            (message "hive-mcp-org-ai: org-ai not available, addon disabled"))
        ;; Try to load hive-mcp-api
        (require 'hive-mcp-api nil t)
        ;; Add keybindings to org-ai-mode if available
        (when (boundp 'org-ai-mode-map)
          (set-keymap-parent org-ai-mode-map hive-mcp-org-ai-block-map))
        (message "Emacs-mcp-org-ai enabled"))
    (message "Emacs-mcp-org-ai disabled")))

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'org-ai
   :version "0.1.0"
   :description "Integration with org-ai (AI chat in org-mode)"
   :requires '(org-ai hive-mcp-api)
   :provides '(hive-mcp-org-ai-mode hive-mcp-org-ai-transient)))

(provide 'hive-mcp-org-ai)
;;; hive-mcp-org-ai.el ends here
