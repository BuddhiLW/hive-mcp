;;; emacs-mcp-kanban.el --- In-memory kanban via memory system -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.5.1
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT
;; This file is part of emacs-mcp.

;;; Commentary:
;;
;; Lightweight in-memory kanban system built on top of emacs-mcp-memory.
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

(require 'emacs-mcp-memory)
(require 'seq)

;;; Constants

(defconst emacs-mcp-kanban-statuses '("todo" "doing" "review" "done")
  "Valid kanban task statuses.")

(defconst emacs-mcp-kanban-priorities '("high" "medium" "low")
  "Valid kanban task priorities.")

;;; Utility Functions

(defun emacs-mcp-kanban--timestamp ()
  "Return current ISO 8601 timestamp."
  (format-time-string "%FT%T%z"))

(defun emacs-mcp-kanban--build-tags (status priority)
  "Build tags list from STATUS and PRIORITY."
  (list "kanban" status (format "priority-%s" priority)))

(defun emacs-mcp-kanban--is-kanban-entry-p (entry)
  "Return non-nil if ENTRY is a kanban task."
  (let ((content (plist-get entry :content)))
    (and (plistp content)
         (string= (plist-get content :task-type) "kanban"))))

;;; Core Functions

(defun emacs-mcp-kanban-task-create (title &optional priority context)
  "Create a kanban task in memory with short-term duration.
TITLE is required. PRIORITY defaults to medium. CONTEXT is optional notes.
Returns the created entry."
  (let* ((prio (or priority "medium"))
         (status "todo")
         (content (list :task-type "kanban"
                        :title title
                        :status status
                        :priority prio
                        :created (emacs-mcp-kanban--timestamp)
                        :started nil
                        :context context))
         (tags (emacs-mcp-kanban--build-tags status prio)))
    ;; Validate priority
    (unless (member prio emacs-mcp-kanban-priorities)
      (error "Invalid priority: %s. Must be one of %s"
             prio emacs-mcp-kanban-priorities))
    ;; Add to memory with short-term duration
    (emacs-mcp-memory-add 'note content tags nil 'short-term)))

(defun emacs-mcp-kanban-task-move (task-id new-status)
  "Move task TASK-ID to NEW-STATUS. If done, delete task.
Valid statuses: todo, doing, review, done.
Returns the updated entry, or t if deleted (done)."
  ;; Validate status
  (unless (member new-status emacs-mcp-kanban-statuses)
    (error "Invalid status: %s. Must be one of %s"
           new-status emacs-mcp-kanban-statuses))
  ;; Get existing entry
  (let ((entry (emacs-mcp-memory-get task-id)))
    (unless entry
      (error "Task not found: %s" task-id))
    (unless (emacs-mcp-kanban--is-kanban-entry-p entry)
      (error "Entry is not a kanban task: %s" task-id))
    ;; If done, delete the task
    (if (string= new-status "done")
        (progn
          (emacs-mcp-memory-delete task-id)
          t)
      ;; Otherwise, update status and tags
      (let* ((content (plist-get entry :content))
             (priority (plist-get content :priority))
             (new-content (plist-put (copy-sequence content) :status new-status))
             (new-tags (emacs-mcp-kanban--build-tags new-status priority)))
        ;; Set :started timestamp when moving to "doing"
        (when (string= new-status "doing")
          (setq new-content (plist-put new-content :started (emacs-mcp-kanban--timestamp))))
        ;; Update the entry
        (emacs-mcp-memory-update task-id
                                 (list :content new-content
                                       :tags new-tags))
        ;; Return updated entry
        (emacs-mcp-memory-get task-id)))))

(defun emacs-mcp-kanban-task-delete (task-id)
  "Delete task TASK-ID from memory.
Returns t if deleted, nil if not found."
  (emacs-mcp-memory-delete task-id))

(defun emacs-mcp-kanban-list-all ()
  "List all kanban tasks.
Returns list of entries sorted by priority (high first)."
  (let* ((entries (emacs-mcp-memory-query 'note '("kanban")))
         (kanban-entries (seq-filter #'emacs-mcp-kanban--is-kanban-entry-p entries)))
    ;; Sort by priority: high > medium > low
    (sort kanban-entries
          (lambda (a b)
            (let ((prio-a (plist-get (plist-get a :content) :priority))
                  (prio-b (plist-get (plist-get b :content) :priority)))
              (< (seq-position emacs-mcp-kanban-priorities prio-a)
                 (seq-position emacs-mcp-kanban-priorities prio-b)))))))

(defun emacs-mcp-kanban-list-by-status (status)
  "List tasks by STATUS (todo, doing, review).
Returns list of entries for that status."
  (unless (member status emacs-mcp-kanban-statuses)
    (error "Invalid status: %s" status))
  (when (string= status "done")
    (error "Cannot list 'done' tasks - they are deleted on completion"))
  (let ((entries (emacs-mcp-memory-query 'note (list "kanban" status))))
    (seq-filter #'emacs-mcp-kanban--is-kanban-entry-p entries)))

(defun emacs-mcp-kanban-stats ()
  "Return task counts by status.
Returns plist with :todo, :doing, :review counts."
  (let ((all-tasks (emacs-mcp-kanban-list-all))
        (counts (list :todo 0 :doing 0 :review 0)))
    (dolist (task all-tasks)
      (let* ((content (plist-get task :content))
             (status (plist-get content :status))
             (key (intern (concat ":" status))))
        (plist-put counts key (1+ (plist-get counts key)))))
    counts))

(defun emacs-mcp-kanban-task-update (task-id &optional title priority context)
  "Update task TASK-ID with new TITLE, PRIORITY, or CONTEXT.
Only provided fields are updated. Returns the updated entry."
  (let ((entry (emacs-mcp-memory-get task-id)))
    (unless entry
      (error "Task not found: %s" task-id))
    (unless (emacs-mcp-kanban--is-kanban-entry-p entry)
      (error "Entry is not a kanban task: %s" task-id))
    (let* ((content (copy-sequence (plist-get entry :content)))
           (current-status (plist-get content :status))
           (new-priority (or priority (plist-get content :priority))))
      ;; Validate priority if provided
      (when priority
        (unless (member priority emacs-mcp-kanban-priorities)
          (error "Invalid priority: %s" priority)))
      ;; Update fields if provided
      (when title
        (setq content (plist-put content :title title)))
      (when priority
        (setq content (plist-put content :priority priority)))
      (when context
        (setq content (plist-put content :context context)))
      ;; Rebuild tags with potentially new priority
      (let ((new-tags (emacs-mcp-kanban--build-tags current-status new-priority)))
        (emacs-mcp-memory-update task-id
                                 (list :content content
                                       :tags new-tags)))
      ;; Return updated entry
      (emacs-mcp-memory-get task-id))))

(provide 'emacs-mcp-kanban)
;;; emacs-mcp-kanban.el ends here
