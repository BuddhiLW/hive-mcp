;;; hive-mcp-transient.el --- Transient menus for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Transient (magit-style) menus for hive-mcp.
;; Provides quick access to memory, context, and workflow operations.
;;

;;; Code:

(require 'transient)

;; Forward declarations for byte-compiler
(declare-function hive-mcp-list-triggers "hive-mcp-triggers")

;;; Extension Point

(defvar hive-mcp-transient-extra-groups nil
  "Extra groups to add to main transient menu.
Each element should be a valid transient group vector.")

;;; Main Menu

;;;###autoload (autoload 'hive-mcp-transient-main "hive-mcp-transient" nil t)
(transient-define-prefix hive-mcp-transient-main ()
  "Main hive-mcp menu."
  ["Emacs MCP"
   ["Memory"
    ("n" "Add note" hive-mcp-add-note-interactive)
    ("s" "Save snippet" hive-mcp-add-snippet-interactive)
    ("c" "Add convention" hive-mcp-add-convention-interactive)
    ("d" "Record decision" hive-mcp-add-decision-interactive)
    ("l" "Browse memory" hive-mcp-show-memory)]

   ["Context"
    ("x" "Show context" hive-mcp-show-context)
    ("b" "Buffer info" hive-mcp-show-buffer-info)
    ("p" "Project info" hive-mcp-show-project-info)
    ("g" "Git status" hive-mcp-show-git-info)]

   ["Workflows"
    ("w" "Run workflow" hive-mcp-run-workflow-interactive)
    ("W" "List workflows" hive-mcp-list-workflows-interactive)
    ("t" "List triggers" hive-mcp-list-triggers-interactive)]

   ["History"
    ("h" "Conversation history" hive-mcp-show-conversation-history)
    ("H" "Clear history" hive-mcp-clear-conversation)]])

;;; Memory Submenu

;;;###autoload (autoload 'hive-mcp-transient-memory "hive-mcp-transient" nil t)
(transient-define-prefix hive-mcp-transient-memory ()
  "Memory management menu."
  ["Project Memory"
   ["Add"
    ("n" "Note" hive-mcp-add-note-interactive)
    ("s" "Snippet" hive-mcp-add-snippet-interactive)
    ("c" "Convention" hive-mcp-add-convention-interactive)
    ("d" "Decision" hive-mcp-add-decision-interactive)]

   ["Browse"
    ("l" "All memory" hive-mcp-show-memory)
    ("N" "Notes only" hive-mcp-browse-notes)
    ("S" "Snippets only" hive-mcp-browse-snippets)]

   ["Manage"
    ("e" "Export memory" hive-mcp-export-memory)
    ("D" "Delete entry" hive-mcp-delete-memory-interactive)]])

;;; Workflow Submenu

;;;###autoload (autoload 'hive-mcp-transient-workflows "hive-mcp-transient" nil t)
(transient-define-prefix hive-mcp-transient-workflows ()
  "Workflow management menu."
  ["Workflows"
   ["Execute"
    ("w" "Run workflow" hive-mcp-run-workflow-interactive)
    ("r" "Run with args" hive-mcp-run-workflow-with-args)]

   ["Manage"
    ("l" "List workflows" hive-mcp-list-workflows-interactive)
    ("t" "List triggers" hive-mcp-list-triggers-interactive)
    ("e" "Edit workflows file" hive-mcp-edit-workflows-file)]])

;;; Context Display Commands

(defun hive-mcp-show-buffer-info ()
  "Show current buffer context."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-buffer-info*"))
        (info (hive-mcp-context-buffer)))
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

(defun hive-mcp-show-project-info ()
  "Show current project context."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-project-info*"))
        (info (hive-mcp-context-project)))
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

(defun hive-mcp-show-git-info ()
  "Show current git context."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-git-info*"))
        (info (hive-mcp-context-git)))
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

(defun hive-mcp-browse-notes ()
  "Browse notes in project memory."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-notes*"))
        (notes (hive-mcp-memory-query 'note)))
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

(defun hive-mcp-browse-snippets ()
  "Browse snippets in project memory."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-snippets*"))
        (snippets (hive-mcp-memory-query 'snippet)))
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

(defun hive-mcp-list-workflows-interactive ()
  "List available workflows."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-workflows*"))
        (workflows (if (fboundp 'hive-mcp-workflow-list)
                       (hive-mcp-workflow-list)
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

(defun hive-mcp-list-triggers-interactive ()
  "List registered triggers."
  (interactive)
  (let ((buf (get-buffer-create "*hive-mcp-triggers*"))
        (triggers (hive-mcp-list-triggers)))
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

(defun hive-mcp-run-workflow-with-args ()
  "Run a workflow with arguments."
  (interactive)
  (message "TODO: Implement workflow argument prompting"))

(defun hive-mcp-edit-workflows-file ()
  "Edit the workflows definition file."
  (interactive)
  (if (boundp 'hive-mcp-workflows-file)
      (find-file hive-mcp-workflows-file)
    (message "Workflows file not configured")))

;;; Memory Management

(defun hive-mcp-export-memory ()
  "Export project memory to a file."
  (interactive)
  (let* ((context (hive-mcp-memory-get-project-context))
         (filename (read-file-name "Export to: " nil nil nil "memory-export.json")))
    (with-temp-file filename
      (insert (json-serialize context)))
    (message "Memory exported to %s" filename)))

(defun hive-mcp-delete-memory-interactive ()
  "Delete a memory entry by ID."
  (interactive)
  (let ((id (read-string "Entry ID to delete: ")))
    (if (hive-mcp-memory-delete id)
        (message "Entry deleted")
      (message "Entry not found"))))

(provide 'hive-mcp-transient)
;;; hive-mcp-transient.el ends here
