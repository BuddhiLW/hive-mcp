;;; emacs-mcp-transient.el --- Transient menus for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of emacs-mcp.

;;; Commentary:
;;
;; Transient (magit-style) menus for emacs-mcp.
;; Provides quick access to memory, context, and workflow operations.
;;

;;; Code:

(require 'transient)

;; Forward declarations for byte-compiler
(declare-function emacs-mcp-list-triggers "emacs-mcp-triggers")

;;; Extension Point

(defvar emacs-mcp-transient-extra-groups nil
  "Extra groups to add to main transient menu.
Each element should be a valid transient group vector.")

;;; Main Menu

;;;###autoload (autoload 'emacs-mcp-transient-main "emacs-mcp-transient" nil t)
(transient-define-prefix emacs-mcp-transient-main ()
  "Main emacs-mcp menu."
  ["Emacs MCP"
   ["Memory"
    ("n" "Add note" emacs-mcp-add-note-interactive)
    ("s" "Save snippet" emacs-mcp-add-snippet-interactive)
    ("c" "Add convention" emacs-mcp-add-convention-interactive)
    ("d" "Record decision" emacs-mcp-add-decision-interactive)
    ("l" "Browse memory" emacs-mcp-show-memory)]

   ["Context"
    ("x" "Show context" emacs-mcp-show-context)
    ("b" "Buffer info" emacs-mcp-show-buffer-info)
    ("p" "Project info" emacs-mcp-show-project-info)
    ("g" "Git status" emacs-mcp-show-git-info)]

   ["Workflows"
    ("w" "Run workflow" emacs-mcp-run-workflow-interactive)
    ("W" "List workflows" emacs-mcp-list-workflows-interactive)
    ("t" "List triggers" emacs-mcp-list-triggers-interactive)]

   ["History"
    ("h" "Conversation history" emacs-mcp-show-conversation-history)
    ("H" "Clear history" emacs-mcp-clear-conversation)]])

;;; Memory Submenu

;;;###autoload (autoload 'emacs-mcp-transient-memory "emacs-mcp-transient" nil t)
(transient-define-prefix emacs-mcp-transient-memory ()
  "Memory management menu."
  ["Project Memory"
   ["Add"
    ("n" "Note" emacs-mcp-add-note-interactive)
    ("s" "Snippet" emacs-mcp-add-snippet-interactive)
    ("c" "Convention" emacs-mcp-add-convention-interactive)
    ("d" "Decision" emacs-mcp-add-decision-interactive)]

   ["Browse"
    ("l" "All memory" emacs-mcp-show-memory)
    ("N" "Notes only" emacs-mcp-browse-notes)
    ("S" "Snippets only" emacs-mcp-browse-snippets)]

   ["Manage"
    ("e" "Export memory" emacs-mcp-export-memory)
    ("D" "Delete entry" emacs-mcp-delete-memory-interactive)]])

;;; Workflow Submenu

;;;###autoload (autoload 'emacs-mcp-transient-workflows "emacs-mcp-transient" nil t)
(transient-define-prefix emacs-mcp-transient-workflows ()
  "Workflow management menu."
  ["Workflows"
   ["Execute"
    ("w" "Run workflow" emacs-mcp-run-workflow-interactive)
    ("r" "Run with args" emacs-mcp-run-workflow-with-args)]

   ["Manage"
    ("l" "List workflows" emacs-mcp-list-workflows-interactive)
    ("t" "List triggers" emacs-mcp-list-triggers-interactive)
    ("e" "Edit workflows file" emacs-mcp-edit-workflows-file)]])

;;; Context Display Commands

(defun emacs-mcp-show-buffer-info ()
  "Show current buffer context."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-buffer-info*"))
        (info (emacs-mcp-context-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Buffer Context =\n\n")
      (insert (format "Name:     %s\n" (plist-get info :name)))
      (insert (format "File:     %s\n" (or (plist-get info :file) "(no file)")))
      (insert (format "Mode:     %s\n" (plist-get info :mode)))
      (insert (format "Modified: %s\n" (if (eq (plist-get info :modified) t) "yes" "no")))
      (insert (format "Point:    %d (line %d, col %d)\n"
                      (plist-get info :point)
                      (plist-get info :line)
                      (plist-get info :column)))
      (insert (format "Size:     %d bytes\n" (plist-get info :size)))
      (insert (format "Encoding: %s\n" (plist-get info :encoding)))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun emacs-mcp-show-project-info ()
  "Show current project context."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-project-info*"))
        (info (emacs-mcp-context-project)))
    (with-current-buffer buf
      (erase-buffer)
      (if info
          (progn
            (insert "= Project Context =\n\n")
            (insert (format "Name: %s\n" (plist-get info :name)))
            (insert (format "Root: %s\n" (plist-get info :root)))
            (insert (format "Type: %s\n" (plist-get info :type))))
        (insert "Not in a project"))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun emacs-mcp-show-git-info ()
  "Show current git context."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-git-info*"))
        (info (emacs-mcp-context-git)))
    (with-current-buffer buf
      (erase-buffer)
      (if info
          (progn
            (insert "= Git Context =\n\n")
            (insert (format "Branch:  %s\n" (plist-get info :branch)))
            (insert (format "Dirty:   %s\n" (if (eq (plist-get info :dirty) t) "yes" "no")))
            (insert "\nStaged files:\n")
            (dolist (f (plist-get info :staged))
              (insert (format "  + %s\n" f)))
            (insert "\nModified files:\n")
            (dolist (f (plist-get info :modified))
              (insert (format "  M %s\n" f)))
            (insert "\nRecent commits:\n")
            (dolist (c (plist-get info :recent-commits))
              (insert (format "  %s\n" c))))
        (insert "Not in a git repository"))
      (goto-char (point-min)))
    (display-buffer buf)))

;;; Memory Browse Commands

(defun emacs-mcp-browse-notes ()
  "Browse notes in project memory."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-notes*"))
        (notes (emacs-mcp-memory-query 'note)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Project Notes =\n\n")
      (if notes
          (dolist (note notes)
            (insert (format "---\n[%s]\n%s\n"
                            (plist-get note :created)
                            (plist-get note :content)))
            (when-let* ((tags (plist-get note :tags)))
              (insert (format "Tags: %s\n" (string-join tags ", ")))))
        (insert "(no notes)"))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun emacs-mcp-browse-snippets ()
  "Browse snippets in project memory."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-snippets*"))
        (snippets (emacs-mcp-memory-query 'snippet)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Project Snippets =\n\n")
      (if snippets
          (dolist (snip snippets)
            (let ((c (plist-get snip :content)))
              (insert (format "--- %s [%s] ---\n%s\n\n"
                              (plist-get c :name)
                              (plist-get c :language)
                              (plist-get c :code)))))
        (insert "(no snippets)"))
      (goto-char (point-min)))
    (display-buffer buf)))

;;; Workflow Commands

(defun emacs-mcp-list-workflows-interactive ()
  "List available workflows."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-workflows*"))
        (workflows (if (fboundp 'emacs-mcp-workflow-list)
                       (emacs-mcp-workflow-list)
                     '())))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Available Workflows =\n\n")
      (if workflows
          (dolist (wf workflows)
            (insert (format "- %s\n  %s\n\n"
                            (plist-get wf :name)
                            (or (plist-get wf :description) "(no description)"))))
        (insert "(no workflows defined)"))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun emacs-mcp-list-triggers-interactive ()
  "List registered triggers."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp-triggers*"))
        (triggers (emacs-mcp-list-triggers)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "= Registered Triggers =\n\n")
      (if triggers
          (dolist (tr triggers)
            (insert (format "- %s [%s]\n  %s\n\n"
                            (plist-get tr :name)
                            (plist-get tr :event)
                            (or (plist-get tr :description) "(no description)"))))
        (insert "(no triggers registered)"))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun emacs-mcp-run-workflow-with-args ()
  "Run a workflow with arguments."
  (interactive)
  (message "TODO: Implement workflow argument prompting"))

(defun emacs-mcp-edit-workflows-file ()
  "Edit the workflows definition file."
  (interactive)
  (if (boundp 'emacs-mcp-workflows-file)
      (find-file emacs-mcp-workflows-file)
    (message "Workflows file not configured")))

;;; Memory Management

(defun emacs-mcp-export-memory ()
  "Export project memory to a file."
  (interactive)
  (let* ((context (emacs-mcp-memory-get-project-context))
         (filename (read-file-name "Export to: " nil nil nil "memory-export.json")))
    (with-temp-file filename
      (insert (json-serialize context)))
    (message "Memory exported to %s" filename)))

(defun emacs-mcp-delete-memory-interactive ()
  "Delete a memory entry by ID."
  (interactive)
  (let ((id (read-string "Entry ID to delete: ")))
    (if (emacs-mcp-memory-delete id)
        (message "Entry deleted")
      (message "Entry not found"))))

(provide 'emacs-mcp-transient)
;;; emacs-mcp-transient.el ends here
