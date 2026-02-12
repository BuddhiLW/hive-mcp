;;; hive-mcp-kanban-vibe.el --- Vibe-kanban MCP backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Vibe-kanban MCP backend for hive-mcp-kanban.
;; Communicates with the vibe-kanban MCP server for cloud/team collaboration.
;;
;; This backend is ideal for:
;; - Team collaboration
;; - Cloud-based task tracking
;; - Integration with other MCP tools
;;
;; Requires vibe-kanban MCP server to be running and configured.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'hive-mcp-kanban-protocol)

;;;; Customization:

(defcustom hive-mcp-kanban-default-project nil
  "Default vibe-kanban project ID.
Set this to your project UUID for automatic project selection."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Project UUID"))
  :group 'hive-mcp-kanban)

;;;; Internal State:

(defvar hive-mcp-kanban--vibe-cache nil
  "Cache for vibe-kanban data.")

;;;; MCP Communication:

(defun hive-mcp-kanban--mcp-call (tool params)
  "Call vibe-kanban MCP TOOL with PARAMS.
This uses emacsclient to communicate with the MCP server."
  ;; Note: In practice, this would call the MCP tool via the server
  ;; For now, we return a placeholder that can be filled in
  (let ((cmd (format "(mcp__vibe_kanban__%s %s)"
                     tool
                     (if params (json-encode params) ""))))
    ;; This is a placeholder - actual implementation depends on MCP bridge
    (message "MCP call: %s" cmd)
    nil))

;;;; Backend Implementation:

(cl-defmethod hive-mcp-kanban--list-projects ((_backend (eql vibe)))
  "List projects from vibe-kanban MCP."
  (hive-mcp-kanban--mcp-call "list_projects" nil))

(cl-defmethod hive-mcp-kanban--list-tasks ((_backend (eql vibe)) project-id &optional status)
  "List tasks from vibe-kanban for PROJECT-ID, optionally filtered by STATUS."
  (let ((params `((project_id . ,project-id))))
    (when status
      (push `(status . ,status) params))
    (hive-mcp-kanban--mcp-call "list_tasks" params)))

(cl-defmethod hive-mcp-kanban--get-task ((_backend (eql vibe)) task-id)
  "Get TASK-ID from vibe-kanban."
  (hive-mcp-kanban--mcp-call "get_task" `((task_id . ,task-id))))

(cl-defmethod hive-mcp-kanban--create-task ((_backend (eql vibe)) project-id title &optional description)
  "Create task with TITLE in PROJECT-ID, optionally with DESCRIPTION."
  (let ((params `((project_id . ,project-id)
                  (title . ,title))))
    (when description
      (push `(description . ,description) params))
    (hive-mcp-kanban--mcp-call "create_task" params)))

(cl-defmethod hive-mcp-kanban--update-task ((_backend (eql vibe)) task-id &rest props)
  "Update TASK-ID in vibe-kanban with PROPS."
  (let ((params `((task_id . ,task-id))))
    (when-let* ((title (plist-get props :title)))
      (push `(title . ,title) params))
    (when-let* ((desc (plist-get props :description)))
      (push `(description . ,desc) params))
    (when-let* ((status (plist-get props :status)))
      (push `(status . ,status) params))
    (hive-mcp-kanban--mcp-call "update_task" params)))

(cl-defmethod hive-mcp-kanban--delete-task ((_backend (eql vibe)) task-id)
  "Delete TASK-ID from vibe-kanban."
  (hive-mcp-kanban--mcp-call "delete_task" `((task_id . ,task-id))))

;;;; Cache Management:

(defun hive-mcp-kanban-vibe-clear-cache ()
  "Clear the vibe-kanban cache."
  (interactive)
  (setq hive-mcp-kanban--vibe-cache nil)
  (message "Vibe-kanban cache cleared"))

(defun hive-mcp-kanban-vibe-refresh-cache (project-id)
  "Refresh cache for PROJECT-ID from vibe-kanban."
  (let ((tasks (hive-mcp-kanban--mcp-call "list_tasks"
                                          `((project_id . ,project-id)))))
    (setq hive-mcp-kanban--vibe-cache
          (plist-put hive-mcp-kanban--vibe-cache
                     (intern project-id)
                     `((tasks . ,tasks)
                       (timestamp . ,(current-time)))))
    tasks))

(provide 'hive-mcp-kanban-vibe)
;;; hive-mcp-kanban-vibe.el ends here
