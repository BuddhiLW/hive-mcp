;;; hive-mcp-kanban-standalone.el --- Standalone org-file backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Standalone org-file backend for hive-mcp-kanban.
;; Stores kanban tasks in a local org file for personal/offline use.
;;
;; Design principles:
;; - Uses org-mode as the storage format
;; - Level 1 headings = Projects
;; - Level 2 headings = Tasks
;; - Properties store metadata (ID, VIBE_ID, AGENT, SESSION, etc.)
;;
;; This backend is ideal for:
;; - Personal task tracking
;; - Offline work
;; - Integration with existing org-mode workflows

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'hive-mcp-kanban-protocol)

;; Forward declarations
(defvar org-id-locations)

;;;; Customization:

(defcustom hive-mcp-kanban-org-file
  (expand-file-name ".hive-mcp-kanban.org" user-emacs-directory)
  "Path to standalone kanban org file."
  :type 'file
  :group 'hive-mcp-kanban)

;;;; Internal State:

(defvar hive-mcp-kanban--standalone-buffer nil
  "Buffer visiting the standalone kanban org file.")

;;;; Org File Helpers:

(defun hive-mcp-kanban--init-org-file (file)
  "Initialize a new kanban org FILE."
  (with-temp-file file
    (insert "#+TITLE: hive-mcp Kanban\n")
    (insert "#+STARTUP: overview\n")
    (insert "#+TODO: TODO IN-PROGRESS IN-REVIEW | DONE CANCELLED\n\n")
    (insert "* Default Project\n")
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: %s\n" (org-id-uuid)))
    (insert (format ":CREATED: %s\n" (format-time-string "%F")))
    (insert ":END:\n")))

(defmacro hive-mcp-kanban--with-org-file (&rest body)
  "Execute BODY with the kanban org file as current buffer."
  (declare (indent 0) (debug t))
  `(let ((file hive-mcp-kanban-org-file))
     ;; Ensure org-id is properly initialized
     (require 'org-id)
     (unless org-id-locations
       (setq org-id-locations (make-hash-table :test 'equal)))
     (unless (file-exists-p file)
       (hive-mcp-kanban--init-org-file file))
     (with-current-buffer (find-file-noselect file)
       ,@body)))

;;;; Backend Implementation:

(cl-defmethod hive-mcp-kanban--list-projects ((_backend (eql standalone)))
  "List projects from standalone org file.
Each top-level heading is a project."
  (hive-mcp-kanban--with-org-file
    (org-map-entries
     (lambda ()
       (let ((id (org-id-get-create))
             (title (org-get-heading t t t t)))
         `((id . ,id) (name . ,title))))
     "LEVEL=1")))

(cl-defmethod hive-mcp-kanban--list-tasks ((_backend (eql standalone)) project-id &optional status)
  "List tasks from standalone org file for PROJECT-ID, filtered by STATUS.
If PROJECT-ID is nil, lists all tasks from all projects."
  (hive-mcp-kanban--with-org-file
    (let ((tasks '()))
      (org-map-entries
       (lambda ()
         (let* ((props (org-entry-properties))
                (parent-id (save-excursion
                             (when (org-up-heading-safe)
                               (org-id-get))))
                (task-status (cdr (assoc "TODO" props)))
                (mapped-status (hive-mcp-kanban--org-to-vibe-status task-status)))
           ;; If no project-id specified, list all tasks; otherwise filter by project
           (when (and (or (null project-id) (equal parent-id project-id))
                      (or (null status)
                          (equal mapped-status status)))
             (push `((id . ,(org-id-get-create))
                     (title . ,(org-get-heading t t t t))
                     (status . ,mapped-status)
                     (description . ,(org-entry-get nil "DESCRIPTION"))
                     (agent . ,(org-entry-get nil "AGENT"))
                     (session . ,(org-entry-get nil "SESSION")))
                   tasks))))
       "LEVEL=2")
      (nreverse tasks))))

(cl-defmethod hive-mcp-kanban--get-task ((_backend (eql standalone)) task-id)
  "Get TASK-ID from standalone org file."
  (hive-mcp-kanban--with-org-file
    (let ((marker (org-id-find task-id 'marker)))
      (when marker
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (let ((props (org-entry-properties)))
            `((id . ,task-id)
              (title . ,(org-get-heading t t t t))
              (status . ,(hive-mcp-kanban--org-to-vibe-status
                          (cdr (assoc "TODO" props))))
              (description . ,(org-entry-get nil "DESCRIPTION"))
              (agent . ,(org-entry-get nil "AGENT"))
              (session . ,(org-entry-get nil "SESSION"))
              (created . ,(org-entry-get nil "CREATED"))
              (updated . ,(org-entry-get nil "UPDATED")))))))))

(cl-defmethod hive-mcp-kanban--create-task ((_backend (eql standalone)) project-id title &optional description)
  "Create task with TITLE in PROJECT-ID, optionally with DESCRIPTION.
If PROJECT-ID is nil, uses the first project (Default Project)."
  (hive-mcp-kanban--with-org-file
    ;; Find project - if nil, use the first heading as default
    (let ((marker (if project-id
                      (org-id-find project-id 'marker)
                    ;; Find first level-1 heading (default project)
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward "^\\* " nil t)
                        (point-marker))))))
      (when marker
        (with-current-buffer (or (marker-buffer marker) (current-buffer))
          (goto-char (if (markerp marker) marker (point-min)))
          (when (and (not (markerp marker)) (re-search-forward "^\\* " nil t))
            (beginning-of-line))
          (org-end-of-subtree t)
          (insert "\n** TODO " title "\n")
          (org-entry-put nil "ID" (org-id-uuid))
          (org-entry-put nil "CREATED" (format-time-string "%F %R"))
          (when description
            (org-entry-put nil "DESCRIPTION" description))
          (when hive-mcp-kanban-track-agents
            (org-entry-put nil "AGENT" (hive-mcp-kanban--current-agent))
            (org-entry-put nil "SESSION" (hive-mcp-kanban--current-session)))
          (save-buffer)
          `((id . ,(org-entry-get nil "ID"))))))))

(cl-defmethod hive-mcp-kanban--update-task ((_backend (eql standalone)) task-id &rest props)
  "Update TASK-ID in standalone org file with PROPS."
  (hive-mcp-kanban--with-org-file
    (let ((marker (org-id-find task-id 'marker)))
      (when marker
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (when-let* ((title (plist-get props :title)))
            (org-edit-headline title))
          (when-let* ((desc (plist-get props :description)))
            (org-entry-put nil "DESCRIPTION" desc))
          (when-let* ((status (plist-get props :status)))
            (let ((org-status (hive-mcp-kanban--vibe-to-org-status status)))
              (org-todo org-status)))
          (org-entry-put nil "UPDATED" (format-time-string "%F %R"))
          (when hive-mcp-kanban-track-agents
            (org-entry-put nil "LAST_AGENT" (hive-mcp-kanban--current-agent)))
          (save-buffer)
          t)))))

(cl-defmethod hive-mcp-kanban--delete-task ((_backend (eql standalone)) task-id)
  "Delete TASK-ID from standalone org file."
  (hive-mcp-kanban--with-org-file
    (let ((marker (org-id-find task-id 'marker)))
      (when marker
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (org-cut-subtree)
          (save-buffer)
          t)))))

;;;; Vibe-ID Sync Helpers:
;;
;; These functions support delta sync between standalone and vibe backends
;; by tracking VIBE_ID properties on org entries.

(defun hive-mcp-kanban-create-task-with-vibe-id (title vibe-id &optional description status)
  "Create task with VIBE-ID for delta sync tracking.
TITLE is the task title, VIBE-ID is the vibe-kanban UUID.
Optional DESCRIPTION and STATUS (defaults to todo)."
  (hive-mcp-kanban--with-org-file
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* " nil t)
        (beginning-of-line)
        (org-end-of-subtree t)
        (let ((org-status (upcase (or (hive-mcp-kanban--vibe-to-org-status status) "TODO"))))
          (insert "\n** " org-status " " title "\n")
          (org-entry-put nil "ID" (org-id-uuid))
          (org-entry-put nil "VIBE_ID" vibe-id)
          (org-entry-put nil "CREATED" (format-time-string "%F %R"))
          (when description
            (org-entry-put nil "DESCRIPTION" description))
          (save-buffer)
          `((id . ,(org-entry-get nil "ID"))
            (vibe_id . ,vibe-id)))))))

(defun hive-mcp-kanban-find-by-vibe-id (vibe-id)
  "Find task by VIBE-ID. Returns task alist or nil."
  (hive-mcp-kanban--with-org-file
    (let ((found nil))
      (org-map-entries
       (lambda ()
         (when (equal (org-entry-get nil "VIBE_ID") vibe-id)
           (setq found `((id . ,(org-entry-get nil "ID"))
                         (vibe_id . ,vibe-id)
                         (title . ,(org-get-heading t t t t))
                         (status . ,(hive-mcp-kanban--org-to-vibe-status
                                     (org-entry-get nil "TODO")))))))
       "LEVEL=2")
      found)))

(defun hive-mcp-kanban-get-synced-vibe-ids ()
  "Get list of all VIBE_IDs already synced to org-kanban."
  (hive-mcp-kanban--with-org-file
    (let ((ids '()))
      (org-map-entries
       (lambda ()
         (when-let* ((vibe-id (org-entry-get nil "VIBE_ID")))
           (push vibe-id ids)))
       "LEVEL=2")
      ids)))

(defun hive-mcp-kanban-backfill-vibe-id (title vibe-id)
  "Add VIBE-ID to existing task matched by TITLE.
Returns t if task was found and updated, nil otherwise."
  (hive-mcp-kanban--with-org-file
    (let ((found nil))
      (org-map-entries
       (lambda ()
         (let ((task-title (org-get-heading t t t t)))
           (when (string= task-title title)
             (org-entry-put nil "VIBE_ID" vibe-id)
             (setq found t))))
       "LEVEL=2")
      (when found (save-buffer))
      found)))

;;;; Direct Org File Loading (for board UI):

(defun hive-mcp-kanban-standalone-load-tasks-by-status (file)
  "Load tasks from org FILE grouped by status.
Returns alist of (STATUS . tasks-list)."
  (let ((tasks-by-status '(("TODO" . nil)
                           ("IN-PROGRESS" . nil)
                           ("IN-REVIEW" . nil)
                           ("DONE" . nil))))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-map-entries
         (lambda ()
           (let* ((todo-state (org-get-todo-state))
                  (title (org-get-heading t t t t))
                  (id (org-id-get))
                  (task `((id . ,id)
                          (title . ,title)
                          (status . ,todo-state))))
             (when todo-state
               (let ((cell (assoc todo-state tasks-by-status)))
                 (when cell
                   (setcdr cell (append (cdr cell) (list task))))))))
         t)))
    tasks-by-status))

(provide 'hive-mcp-kanban-standalone)
;;; hive-mcp-kanban-standalone.el ends here
