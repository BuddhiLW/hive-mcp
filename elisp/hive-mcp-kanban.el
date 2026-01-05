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
;;    :tags ["kanban" "todo" "priority-high"]
;;    :duration "short-term"}
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

(defun hive-mcp-kanban--is-kanban-entry-p (entry)
  "Return non-nil if ENTRY is a kanban task."
  (let ((content (plist-get entry :content)))
    (and (plistp content)
         (string= (plist-get content :task-type) "kanban"))))

;;; Core Functions

(defun hive-mcp-kanban-task-create (title &optional priority context)
  "Create a kanban task in memory with short-term duration.
TITLE is required. PRIORITY defaults to medium. CONTEXT is optional notes.
Returns the created entry."
  (let* ((prio (or priority "medium"))
         (status "todo")
         (content (list :task-type "kanban"
                        :title title
                        :status status
                        :priority prio
                        :created (hive-mcp-kanban--timestamp)
                        :started nil
                        :context context))
         (tags (hive-mcp-kanban--build-tags status prio)))
    ;; Validate priority
    (unless (member prio hive-mcp-kanban-priorities)
      (error "Invalid priority: %s. Must be one of %s"
             prio hive-mcp-kanban-priorities))
    ;; Add to memory with short-term duration
    (hive-mcp-memory-add 'note content tags nil 'short-term)))

(defun hive-mcp-kanban-task-move (task-id new-status)
  "Move task TASK-ID to NEW-STATUS. If done, delete task.
Valid statuses: todo, doing, review, done.
Returns the updated entry, or t if deleted (done)."
  ;; Validate status
  (unless (member new-status hive-mcp-kanban-statuses)
    (error "Invalid status: %s. Must be one of %s"
           new-status hive-mcp-kanban-statuses))
  ;; Get existing entry
  (let ((entry (hive-mcp-memory-get task-id)))
    (unless entry
      (error "Task not found: %s" task-id))
    (unless (hive-mcp-kanban--is-kanban-entry-p entry)
      (error "Entry is not a kanban task: %s" task-id))
    ;; If done, delete the task
    (if (string= new-status "done")
        (progn
          (hive-mcp-memory-delete task-id)
          t)
      ;; Otherwise, update status and tags
      (let* ((content (plist-get entry :content))
             (priority (plist-get content :priority))
             (new-content (plist-put (copy-sequence content) :status new-status))
             (new-tags (hive-mcp-kanban--build-tags new-status priority)))
        ;; Set :started timestamp when moving to "doing"
        (when (string= new-status "doing")
          (setq new-content (plist-put new-content :started (hive-mcp-kanban--timestamp))))
        ;; Update the entry
        (hive-mcp-memory-update task-id
                                 (list :content new-content
                                       :tags new-tags))
        ;; Return updated entry
        (hive-mcp-memory-get task-id)))))

(defun hive-mcp-kanban-task-delete (task-id)
  "Delete task TASK-ID from memory.
Returns t if deleted, nil if not found."
  (hive-mcp-memory-delete task-id))

(defun hive-mcp-kanban-list-all ()
  "List all kanban tasks.
Returns list of entries sorted by priority (high first)."
  (let* ((entries (hive-mcp-memory-query 'note '("kanban")))
         (kanban-entries (seq-filter #'hive-mcp-kanban--is-kanban-entry-p entries)))
    ;; Sort by priority: high > medium > low
    (sort kanban-entries
          (lambda (a b)
            (let ((prio-a (plist-get (plist-get a :content) :priority))
                  (prio-b (plist-get (plist-get b :content) :priority)))
              (< (seq-position hive-mcp-kanban-priorities prio-a)
                 (seq-position hive-mcp-kanban-priorities prio-b)))))))

(defun hive-mcp-kanban-list-by-status (status)
  "List tasks by STATUS (todo, doing, review).
Returns list of entries for that status."
  (unless (member status hive-mcp-kanban-statuses)
    (error "Invalid status: %s" status))
  (when (string= status "done")
    (error "Cannot list 'done' tasks - they are deleted on completion"))
  (let ((entries (hive-mcp-memory-query 'note (list "kanban" status))))
    (seq-filter #'hive-mcp-kanban--is-kanban-entry-p entries)))

(defun hive-mcp-kanban-stats ()
  "Return task counts by status.
Returns plist with :todo, :doing, :review counts."
  (let ((all-tasks (hive-mcp-kanban-list-all))
        (counts (list :todo 0 :doing 0 :review 0)))
    (dolist (task all-tasks)
      (let* ((content (plist-get task :content))
             (status (plist-get content :status))
             (key (intern (concat ":" status))))
        (plist-put counts key (1+ (plist-get counts key)))))
    counts))

(defun hive-mcp-kanban-task-update (task-id &optional title priority context)
  "Update task TASK-ID with new TITLE, PRIORITY, or CONTEXT.
Only provided fields are updated. Returns the updated entry."
  (let ((entry (hive-mcp-memory-get task-id)))
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
      ;; Rebuild tags with potentially new priority
      (let ((new-tags (hive-mcp-kanban--build-tags current-status new-priority)))
        (hive-mcp-memory-update task-id
                                 (list :content content
                                       :tags new-tags)))
      ;; Return updated entry
      (hive-mcp-memory-get task-id))))

(provide 'hive-mcp-kanban)
;;; hive-mcp-kanban.el ends here
