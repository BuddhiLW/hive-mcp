;;; hive-mcp-kanban.el --- In-memory kanban via memory system -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.5.1
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Lightweight in-memory kanban system built on top of hive-mcp-memory.
;; Tasks are stored as memory entries with short-term duration.
;; Moving a task to "done" DELETES it (ephemeral task completion).
;;
;; Task Schema:
;;   {:type "note"
;;    :content {:task-type "kanban"
;;              :title "Task description"
;;              :status "todo"        ;; "todo" | "doing" | "review"
;;              :priority "high"      ;; "high" | "medium" | "low"
;;              :created "timestamp"
;;              :started nil          ;; set when moved to doing
;;              :context "notes"}
;;    :tags ["kanban" "todo" "priority-high" "scope:project:<project-id>"]
;;    :duration "short-term"}
;;
;; Project Scoping:
;;   Tasks are automatically tagged with "scope:project:<id>" for isolation.
;;   Project ID is derived from .hive-project.edn or falls back to path hash.
;;   Query operations filter by scope to ensure project isolation.
;;

;;; Code:

(require 'hive-mcp-memory)
(require 'seq)

;;; Constants

(defconst hive-mcp-kanban-statuses '("todo" "doing" "review" "done")
  "Valid kanban task statuses.")

(defconst hive-mcp-kanban-priorities '("high" "medium" "low")
  "Valid kanban task priorities.")

;;; Utility Functions

(defun hive-mcp-kanban--timestamp ()
  "Return current ISO 8601 timestamp."
  (format-time-string "%FT%T%z"))

(defun hive-mcp-kanban--build-tags (status priority)
  "Build tags list from STATUS and PRIORITY."
  (list "kanban" status (format "priority-%s" priority)))

(defun hive-mcp-kanban--build-tags-with-scope (status priority project-id)
  "Build tags list from STATUS, PRIORITY, and PROJECT-ID.
Includes scope tag for project isolation.
If PROJECT-ID is nil, falls back to current project."
  (let* ((base-tags (hive-mcp-kanban--build-tags status priority))
         (effective-project (or project-id (hive-mcp-memory--project-id)))
         (scope-tag (hive-mcp-memory--make-scope-tag 'project effective-project)))
    (append base-tags (list scope-tag))))

(defun hive-mcp-kanban--extract-scope (entry)
  "Extract scope tag from ENTRY tags.
Returns the scope tag string, or nil if not found."
  (let ((tags (plist-get entry :tags)))
    (seq-find (lambda (tag) (string-prefix-p "scope:" tag)) tags)))

(defun hive-mcp-kanban--is-kanban-entry-p (entry)
  "Return non-nil if ENTRY is a kanban task."
  (let ((content (plist-get entry :content)))
    (and (plistp content)
         (string= (plist-get content :task-type) "kanban"))))

;;; Core Functions

(defun hive-mcp-kanban-task-create (title &optional priority context project-id)
  "Create a kanban task in memory with short-term duration.
TITLE is required. PRIORITY defaults to medium. CONTEXT is optional notes.
PROJECT-ID specifies the project (defaults to current).
Returns the created entry with scope tag for project isolation."
  (let* ((prio (or priority "medium"))
         (status "todo")
         (content (list :task-type "kanban"
                        :title title
                        :status status
                        :priority prio
                        :created (hive-mcp-kanban--timestamp)
                        :started nil
                        :context context))
         ;; Use scoped tags for project isolation
         (tags (hive-mcp-kanban--build-tags-with-scope status prio project-id)))
    ;; Validate priority
    (unless (member prio hive-mcp-kanban-priorities)
      (error "Invalid priority: %s. Must be one of %s"
             prio hive-mcp-kanban-priorities))
    ;; Add to memory with short-term duration
    (hive-mcp-memory-add 'note content tags project-id 'short-term)))

(defun hive-mcp-kanban-task-move (task-id new-status &optional project-id)
  "Move task TASK-ID to NEW-STATUS. If done, delete task.
Valid statuses: todo, doing, review, done.
PROJECT-ID specifies the project (defaults to current).
Returns the updated entry, or t if deleted (done)."
  ;; Validate status
  (unless (member new-status hive-mcp-kanban-statuses)
    (error "Invalid status: %s. Must be one of %s"
           new-status hive-mcp-kanban-statuses))
  ;; Get existing entry
  (let ((entry (hive-mcp-memory-get task-id project-id)))
    (unless entry
      (error "Task not found: %s" task-id))
    (unless (hive-mcp-kanban--is-kanban-entry-p entry)
      (error "Entry is not a kanban task: %s" task-id))
    ;; If done, delete the task
    (if (string= new-status "done")
        (progn
          (hive-mcp-memory-delete task-id project-id)
          t)
      ;; Otherwise, update status and tags
      (let* ((content (plist-get entry :content))
             (priority (plist-get content :priority))
             (new-content (plist-put (copy-sequence content) :status new-status))
             ;; Preserve existing scope or use provided project-id
             (existing-scope (hive-mcp-kanban--extract-scope entry))
             (new-tags (if existing-scope
                           ;; Preserve existing scope tag
                           (append (hive-mcp-kanban--build-tags new-status priority)
                                   (list existing-scope))
                         ;; Use scoped tags with project-id
                         (hive-mcp-kanban--build-tags-with-scope new-status priority project-id))))
        ;; Set :started timestamp when moving to "doing"
        (when (string= new-status "doing")
          (setq new-content (plist-put new-content :started (hive-mcp-kanban--timestamp))))
        ;; Update the entry
        (hive-mcp-memory-update task-id
                                 (list :content new-content
                                       :tags new-tags)
                                 project-id)
        ;; Return updated entry
        (hive-mcp-memory-get task-id project-id)))))

(defun hive-mcp-kanban-task-delete (task-id &optional project-id)
  "Delete task TASK-ID from memory.
PROJECT-ID specifies the project (defaults to current).
Returns t if deleted, nil if not found."
  (hive-mcp-memory-delete task-id project-id))

(defun hive-mcp-kanban-list-all (&optional project-id)
  "List all kanban tasks for the specified project.
PROJECT-ID specifies the project (defaults to current).
Returns list of entries sorted by priority (high first).
Only returns tasks scoped to the specified project."
  (let* ((effective-project (or project-id (hive-mcp-memory--project-id)))
         (scope-filter (hive-mcp-memory--make-scope-tag 'project effective-project))
         ;; Pass scope-filter to memory-query (6th parameter)
         (entries (hive-mcp-memory-query 'note '("kanban") project-id nil nil scope-filter))
         (kanban-entries (seq-filter #'hive-mcp-kanban--is-kanban-entry-p entries)))
    ;; Sort by priority: high > medium > low
    (sort kanban-entries
          (lambda (a b)
            (let ((prio-a (plist-get (plist-get a :content) :priority))
                  (prio-b (plist-get (plist-get b :content) :priority)))
              (< (seq-position hive-mcp-kanban-priorities prio-a)
                 (seq-position hive-mcp-kanban-priorities prio-b)))))))

(defun hive-mcp-kanban-list-by-status (status &optional project-id)
  "List tasks by STATUS (todo, doing, review) for the specified project.
PROJECT-ID specifies the project (defaults to current).
Returns list of entries for that status, scoped to the project."
  (unless (member status hive-mcp-kanban-statuses)
    (error "Invalid status: %s" status))
  (when (string= status "done")
    (error "Cannot list 'done' tasks - they are deleted on completion"))
  (let* ((effective-project (or project-id (hive-mcp-memory--project-id)))
         (scope-filter (hive-mcp-memory--make-scope-tag 'project effective-project))
         ;; Pass scope-filter to memory-query (6th parameter)
         (entries (hive-mcp-memory-query 'note (list "kanban" status) project-id nil nil scope-filter)))
    (seq-filter #'hive-mcp-kanban--is-kanban-entry-p entries)))

(defun hive-mcp-kanban-stats (&optional project-id)
  "Return task counts by status.
PROJECT-ID specifies the project (defaults to current).
Returns plist with :todo, :doing, :review counts."
  (let ((all-tasks (hive-mcp-kanban-list-all project-id))
        (counts (list :todo 0 :doing 0 :review 0)))
    (dolist (task all-tasks)
      (let* ((content (plist-get task :content))
             (status (plist-get content :status))
             (key (intern (concat ":" status))))
        (plist-put counts key (1+ (plist-get counts key)))))
    counts))

(defun hive-mcp-kanban-task-update (task-id &optional title priority context project-id)
  "Update task TASK-ID with new TITLE, PRIORITY, or CONTEXT.
Only provided fields are updated.
PROJECT-ID specifies the project (defaults to current).
Returns the updated entry."
  (let ((entry (hive-mcp-memory-get task-id project-id)))
    (unless entry
      (error "Task not found: %s" task-id))
    (unless (hive-mcp-kanban--is-kanban-entry-p entry)
      (error "Entry is not a kanban task: %s" task-id))
    (let* ((content (copy-sequence (plist-get entry :content)))
           (current-status (plist-get content :status))
           (new-priority (or priority (plist-get content :priority))))
      ;; Validate priority if provided
      (when priority
        (unless (member priority hive-mcp-kanban-priorities)
          (error "Invalid priority: %s" priority)))
      ;; Update fields if provided
      (when title
        (setq content (plist-put content :title title)))
      (when priority
        (setq content (plist-put content :priority priority)))
      (when context
        (setq content (plist-put content :context context)))
      ;; Rebuild tags with potentially new priority, preserving scope
      (let* ((existing-scope (hive-mcp-kanban--extract-scope entry))
             (new-tags (if existing-scope
                           ;; Preserve existing scope tag
                           (append (hive-mcp-kanban--build-tags current-status new-priority)
                                   (list existing-scope))
                         ;; Use scoped tags with project-id
                         (hive-mcp-kanban--build-tags-with-scope current-status new-priority project-id))))
        (hive-mcp-memory-update task-id
                                 (list :content content
                                       :tags new-tags)
                                 project-id))
      ;; Return updated entry
      (hive-mcp-memory-get task-id project-id))))

(provide 'hive-mcp-kanban)
;;; hive-mcp-kanban.el ends here
