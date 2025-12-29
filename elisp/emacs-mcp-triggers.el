;;; emacs-mcp-triggers.el --- Keybindings and hooks for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of emacs-mcp.

;;; Commentary:
;;
;; Trigger system for emacs-mcp including keybindings, hooks, and
;; automation infrastructure.
;;

;;; Code:

(require 'emacs-mcp-memory)
(require 'emacs-mcp-context)

;;; Customization

(defgroup emacs-mcp-triggers nil
  "Trigger and automation settings for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-trigger-")

(defcustom emacs-mcp-keymap-prefix (kbd "C-c m")
  "Prefix key for emacs-mcp commands."
  :type 'key-sequence
  :group 'emacs-mcp-triggers)

(defcustom emacs-mcp-after-save-hook-enabled nil
  "When non-nil, run emacs-mcp hooks after saving files."
  :type 'boolean
  :group 'emacs-mcp-triggers)

;;; Trigger Registry

(defvar emacs-mcp-trigger-registry (make-hash-table :test 'equal)
  "Registry of user-defined triggers.
Each entry is NAME -> plist with :event :condition :action.")

(defun emacs-mcp-register-trigger (name trigger-spec)
  "Register a trigger with NAME and TRIGGER-SPEC.
TRIGGER-SPEC is a plist with :event, :condition, :action keys.
Events: after-save, compilation-finish, diagnostics-error, custom."
  (puthash name trigger-spec emacs-mcp-trigger-registry))

(defun emacs-mcp-unregister-trigger (name)
  "Remove trigger with NAME."
  (remhash name emacs-mcp-trigger-registry))

(defun emacs-mcp-list-triggers ()
  "Return list of registered triggers."
  (let (triggers)
    (maphash (lambda (k v)
               (push (list :name k
                           :event (plist-get v :event)
                           :description (plist-get v :description))
                     triggers))
             emacs-mcp-trigger-registry)
    (nreverse triggers)))

(defun emacs-mcp-trigger-run (event &optional data)
  "Run all triggers matching EVENT with optional DATA."
  (maphash
   (lambda (_name spec)
     (when (eq (plist-get spec :event) event)
       (let ((condition (plist-get spec :condition))
             (action (plist-get spec :action)))
         (when (or (null condition) (funcall condition data))
           (condition-case err
               (funcall action data)
             (error
              (message "MCP trigger error: %s" (error-message-string err))))))))
   emacs-mcp-trigger-registry))

;;; Keymap

(defvar emacs-mcp-command-map
  (let ((map (make-sparse-keymap)))
    ;; Menu
    (define-key map (kbd "m") #'emacs-mcp-transient-menu)

    ;; Memory operations
    (define-key map (kbd "n") #'emacs-mcp-add-note-interactive)
    (define-key map (kbd "s") #'emacs-mcp-add-snippet-interactive)
    (define-key map (kbd "c") #'emacs-mcp-add-convention-interactive)
    (define-key map (kbd "d") #'emacs-mcp-add-decision-interactive)
    (define-key map (kbd "l") #'emacs-mcp-show-memory)

    ;; Context
    (define-key map (kbd "x") #'emacs-mcp-show-context)

    ;; Workflows
    (define-key map (kbd "w") #'emacs-mcp-run-workflow-interactive)
    (define-key map (kbd "W") #'emacs-mcp-define-workflow-interactive)

    ;; Conversation
    (define-key map (kbd "h") #'emacs-mcp-show-conversation-history)
    (define-key map (kbd "H") #'emacs-mcp-clear-conversation)

    map)
  "Keymap for emacs-mcp commands.")

;;;###autoload
(defun emacs-mcp-setup-keybindings ()
  "Set up emacs-mcp keybindings."
  (global-set-key emacs-mcp-keymap-prefix emacs-mcp-command-map))

;;; Interactive Commands

(defun emacs-mcp-add-note-interactive ()
  "Interactively add a note to project memory."
  (interactive)
  (let* ((content (read-string "Note: "))
         (tags-str (read-string "Tags (comma-separated, optional): "))
         (tags (when (not (string-empty-p tags-str))
                 (split-string tags-str "," t "\\s-*"))))
    (emacs-mcp-memory-add-note content tags)
    (message "Note added to project memory")))

(defun emacs-mcp-add-snippet-interactive ()
  "Save region or prompt for code as a snippet."
  (interactive)
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (read-string "Code: ")))
         (name (read-string "Snippet name: "))
         (language (emacs-mcp-context-language))
         (tags-str (read-string "Tags (comma-separated, optional): "))
         (tags (when (not (string-empty-p tags-str))
                 (split-string tags-str "," t "\\s-*"))))
    (emacs-mcp-memory-add-snippet name code language tags)
    (message "Snippet '%s' saved" name)))

(defun emacs-mcp-add-convention-interactive ()
  "Interactively add a project convention."
  (interactive)
  (let* ((description (read-string "Convention: "))
         (example (read-string "Example (optional): ")))
    (emacs-mcp-memory-add-convention
     description
     (unless (string-empty-p example) example))
    (message "Convention added")))

(defun emacs-mcp-add-decision-interactive ()
  "Interactively record an architecture decision."
  (interactive)
  (let* ((title (read-string "Decision title: "))
         (rationale (read-string "Rationale: "))
         (alternatives (read-string "Alternatives considered (optional): ")))
    (emacs-mcp-memory-add-decision
     title
     rationale
     (unless (string-empty-p alternatives) alternatives))
    (message "Decision recorded")))

(defun emacs-mcp-show-memory ()
  "Display project memory in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-memory*"))
        (context (emacs-mcp-memory-get-project-context)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Project Memory =\n")
      (insert (format "Project: %s\n\n" (or (plist-get context :project-root) "global")))

      ;; Notes
      (insert "== Notes ==\n")
      (dolist (note (plist-get context :notes))
        (insert (format "- %s [%s]\n"
                        (plist-get note :content)
                        (plist-get note :created))))
      (insert "\n")

      ;; Conventions
      (insert "== Conventions ==\n")
      (dolist (conv (plist-get context :conventions))
        (let ((c (plist-get conv :content)))
          (insert (format "- %s\n" (plist-get c :description)))
          (when-let* ((ex (plist-get c :example)))
            (insert (format "  Example: %s\n" ex)))))
      (insert "\n")

      ;; Decisions
      (insert "== Recent Decisions ==\n")
      (dolist (dec (plist-get context :recent-decisions))
        (let ((d (plist-get dec :content)))
          (insert (format "- %s\n  %s\n"
                          (plist-get d :title)
                          (plist-get d :rationale)))))
      (insert "\n")

      ;; Snippets
      (insert "== Snippets ==\n")
      (dolist (snip (plist-get context :snippets))
        (let ((s (plist-get snip :content)))
          (insert (format "- %s [%s]\n"
                          (plist-get s :name)
                          (plist-get s :language)))))

      (goto-char (point-min)))
    (display-buffer buf)))

(defun emacs-mcp-show-context ()
  "Display current context in a buffer (for debugging)."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-context*"))
        (context (emacs-mcp-context-full)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Current Context =\n\n")
      (insert (pp-to-string context))
      (goto-char (point-min))
      (emacs-lisp-mode))
    (display-buffer buf)))

(defun emacs-mcp-show-conversation-history ()
  "Display conversation history."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-conversations*"))
        (history (emacs-mcp-memory-query 'conversation nil nil 50)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Conversation History =\n\n")
      (dolist (entry (reverse history))
        (let ((c (plist-get entry :content)))
          (insert (format "[%s] %s:\n%s\n\n"
                          (plist-get entry :created)
                          (plist-get c :role)
                          (plist-get c :content)))))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun emacs-mcp-clear-conversation ()
  "Clear conversation history for current project."
  (interactive)
  (when (yes-or-no-p "Clear conversation history? ")
    (let ((pid (emacs-mcp-memory--project-id)))
      (emacs-mcp-memory--set-data pid "conversation" '())
      (message "Conversation history cleared"))))

;;; Built-in Hook Handlers

(defun emacs-mcp--after-save-handler ()
  "Handler for `after-save-hook'."
  (when emacs-mcp-after-save-hook-enabled
    (emacs-mcp-trigger-run 'after-save
                           (list :file (buffer-file-name)
                                 :buffer (current-buffer)
                                 :mode major-mode))))

(defun emacs-mcp--compilation-finish-handler (buffer status)
  "Handler for `compilation-finish-functions'.
BUFFER is the compilation buffer, STATUS is the result string."
  (emacs-mcp-trigger-run 'compilation-finish
                         (list :buffer buffer
                               :status status
                               :success (string-match-p "finished" status))))

;;;###autoload
(defun emacs-mcp-setup-hooks ()
  "Set up emacs-mcp hooks."
  (add-hook 'after-save-hook #'emacs-mcp--after-save-handler)
  (add-hook 'compilation-finish-functions #'emacs-mcp--compilation-finish-handler))

;;; Workflow Stubs (to be defined in emacs-mcp-workflows.el)

(defun emacs-mcp-run-workflow-interactive ()
  "Run a workflow interactively."
  (interactive)
  (if (fboundp 'emacs-mcp-workflow-run-interactive)
      (call-interactively 'emacs-mcp-workflow-run-interactive)
    (message "Workflows not loaded yet")))

(defun emacs-mcp-define-workflow-interactive ()
  "Define a new workflow interactively."
  (interactive)
  (message "Use `emacs-mcp-workflow-register' to define workflows programmatically"))

;;; Transient Menu Stub

(defun emacs-mcp-transient-menu ()
  "Open the main emacs-mcp transient menu."
  (interactive)
  (if (fboundp 'emacs-mcp-transient-main)
      (emacs-mcp-transient-main)
    (message "Use C-c m <key> for quick access. Menu requires transient package.")))

(provide 'emacs-mcp-triggers)
;;; emacs-mcp-triggers.el ends here
