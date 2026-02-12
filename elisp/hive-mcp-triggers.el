;;; hive-mcp-triggers.el --- Keybindings and hooks for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Trigger system for hive-mcp including keybindings, hooks, and
;; automation infrastructure.
;;

;;; Code:

(require 'hive-mcp-memory)
(require 'hive-mcp-context)

;; Forward declaration for byte-compiler
(defvar hive-mcp-mode-map)

;;; Customization

(defgroup hive-mcp-triggers nil
  "Trigger and automation settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-trigger-")

(defcustom hive-mcp-keymap-prefix nil
  "Prefix key for hive-mcp commands.
Set this in your init file before enabling `hive-mcp-mode'.
Example: (setq hive-mcp-keymap-prefix (kbd \"C-c m\"))
Note: Control-c <letter> is reserved for user bindings per Emacs conventions."
  :type '(choice (const :tag "None" nil) key-sequence)
  :group 'hive-mcp-triggers)

(defcustom hive-mcp-after-save-hook-enabled nil
  "When non-nil, run hive-mcp hooks after saving files."
  :type 'boolean
  :group 'hive-mcp-triggers)

;;; Trigger Registry

(defvar hive-mcp-trigger-registry (make-hash-table :test 'equal)
  "Registry of user-defined triggers.
Each entry is NAME -> plist with :event :condition :action.")

(defun hive-mcp-register-trigger (name trigger-spec)
  "Register a trigger with NAME and TRIGGER-SPEC.
TRIGGER-SPEC is a plist with :event, :condition, :action keys.
Events: after-save, compilation-finish, diagnostics-error, custom."
  (puthash name trigger-spec hive-mcp-trigger-registry))

(defun hive-mcp-unregister-trigger (name)
  "Remove trigger with NAME."
  (remhash name hive-mcp-trigger-registry))

(defun hive-mcp-list-triggers ()
  "Return list of registered triggers."
  (let (triggers)
    (maphash (lambda (k v)
               (push (list :name k
                           :event (plist-get v :event)
                           :description (plist-get v :description))
                     triggers))
             hive-mcp-trigger-registry)
    (nreverse triggers)))

(defun hive-mcp-trigger-run (event &optional data)
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
   hive-mcp-trigger-registry))

;;; Keymap

(defvar hive-mcp-command-map
  (let ((map (make-sparse-keymap)))
    ;; Menu
    (define-key map (kbd "m") #'hive-mcp-transient-menu)

    ;; Memory operations
    (define-key map (kbd "n") #'hive-mcp-add-note-interactive)
    (define-key map (kbd "s") #'hive-mcp-add-snippet-interactive)
    (define-key map (kbd "c") #'hive-mcp-add-convention-interactive)
    (define-key map (kbd "d") #'hive-mcp-add-decision-interactive)
    (define-key map (kbd "l") #'hive-mcp-show-memory)

    ;; Context
    (define-key map (kbd "x") #'hive-mcp-show-context)

    ;; Workflows
    (define-key map (kbd "w") #'hive-mcp-run-workflow-interactive)
    (define-key map (kbd "W") #'hive-mcp-define-workflow-interactive)

    ;; Conversation
    (define-key map (kbd "h") #'hive-mcp-show-conversation-history)
    (define-key map (kbd "H") #'hive-mcp-clear-conversation)

    map)
  "Keymap for hive-mcp commands.")

;;;###autoload
(defun hive-mcp-setup-keybindings ()
  "Set up hive-mcp keybindings.
Only sets up keybindings if `hive-mcp-keymap-prefix' is configured.
Users should set this in their init file before enabling `hive-mcp-mode'.
Example configuration:
  (setq hive-mcp-keymap-prefix (kbd \"C-c m\"))
  (hive-mcp-mode 1)"
  (when hive-mcp-keymap-prefix
    (define-key hive-mcp-mode-map hive-mcp-keymap-prefix hive-mcp-command-map)))

;;; Interactive Commands

(defun hive-mcp-add-note-interactive ()
  "Interactively add a note to project memory."
  (interactive)
  (let* ((content (read-string "Note: "))
         (tags-str (read-string "Tags (comma-separated, optional): "))
         (tags (when (not (string-empty-p tags-str))
                 (split-string tags-str "," t "\\s-*"))))
    (hive-mcp-memory-add-note content tags)
    (message "Note added to project memory")))

(defun hive-mcp-add-snippet-interactive ()
  "Save region or prompt for code as a snippet."
  (interactive)
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (read-string "Code: ")))
         (name (read-string "Snippet name: "))
         (language (hive-mcp-context-language))
         (tags-str (read-string "Tags (comma-separated, optional): "))
         (tags (when (not (string-empty-p tags-str))
                 (split-string tags-str "," t "\\s-*"))))
    (hive-mcp-memory-add-snippet name code language tags)
    (message "Snippet '%s' saved" name)))

(defun hive-mcp-add-convention-interactive ()
  "Interactively add a project convention."
  (interactive)
  (let* ((description (read-string "Convention: "))
         (example (read-string "Example (optional): ")))
    (hive-mcp-memory-add-convention
     description
     (unless (string-empty-p example) example))
    (message "Convention added")))

(defun hive-mcp-add-decision-interactive ()
  "Interactively record an architecture decision."
  (interactive)
  (let* ((title (read-string "Decision title: "))
         (rationale (read-string "Rationale: "))
         (alternatives (read-string "Alternatives considered (optional): ")))
    (hive-mcp-memory-add-decision
     title
     rationale
     (unless (string-empty-p alternatives) alternatives))
    (message "Decision recorded")))

(defun hive-mcp-show-memory ()
  "Display project memory in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-memory*"))
        (context (hive-mcp-memory-get-project-context)))
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

(defun hive-mcp-show-context ()
  "Display current context in a buffer (for debugging)."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-context*"))
        (context (hive-mcp-context-full)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Current Context =\n\n")
      (insert (pp-to-string context))
      (goto-char (point-min))
      (emacs-lisp-mode))
    (display-buffer buf)))

(defun hive-mcp-show-conversation-history ()
  "Display conversation history."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-conversations*"))
        (history (hive-mcp-memory-query 'conversation nil nil 50)))
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

(defun hive-mcp-clear-conversation ()
  "Clear conversation history for current project."
  (interactive)
  (when (yes-or-no-p "Clear conversation history? ")
    (let ((pid (hive-mcp-memory--project-id)))
      (hive-mcp-memory--set-data pid "conversation" '())
      (message "Conversation history cleared"))))

;;; Built-in Hook Handlers

(defun hive-mcp--after-save-handler ()
  "Handler for `after-save-hook'."
  (when hive-mcp-after-save-hook-enabled
    (hive-mcp-trigger-run 'after-save
                           (list :file (buffer-file-name)
                                 :buffer (current-buffer)
                                 :mode major-mode))))

(defun hive-mcp--compilation-finish-handler (buffer status)
  "Handler for `compilation-finish-functions'.
BUFFER is the compilation buffer, STATUS is the result string."
  (hive-mcp-trigger-run 'compilation-finish
                         (list :buffer buffer
                               :status status
                               :success (string-match-p "finished" status))))

;;;###autoload
(defun hive-mcp-setup-hooks ()
  "Set up hive-mcp hooks."
  (add-hook 'after-save-hook #'hive-mcp--after-save-handler)
  (add-hook 'compilation-finish-functions #'hive-mcp--compilation-finish-handler))

;;; Workflow Stubs (to be defined in hive-mcp-workflows.el)

(defun hive-mcp-run-workflow-interactive ()
  "Run a workflow interactively."
  (interactive)
  (if (fboundp 'hive-mcp-workflow-run-interactive)
      (call-interactively #'hive-mcp-workflow-run-interactive)
    (message "Workflows not loaded yet")))

(defun hive-mcp-define-workflow-interactive ()
  "Define a new workflow interactively."
  (interactive)
  (message "Use `hive-mcp-workflow-register' to define workflows programmatically"))

;;; Transient Menu Stub

(defun hive-mcp-transient-menu ()
  "Open the main hive-mcp transient menu."
  (interactive)
  (if (fboundp 'hive-mcp-transient-main)
      (hive-mcp-transient-main)
    (message "Use C-c m <key> for quick access. Menu requires transient package.")))

(provide 'hive-mcp-triggers)
;;; hive-mcp-triggers.el ends here
