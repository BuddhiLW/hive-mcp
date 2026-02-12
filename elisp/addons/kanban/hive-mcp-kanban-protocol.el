;;; hive-mcp-kanban-protocol.el --- Backend protocol for kanban -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Backend protocol abstraction layer for hive-mcp-kanban.
;; Defines the interface that all kanban backends must implement.
;;
;; Design principles (SOLID/CLARITY):
;; - Interface Segregation: Minimal protocol for backend operations
;; - Open/Closed: New backends via cl-defmethod, not modification
;; - Dependency Inversion: Callers depend on protocol, not concrete backends
;;
;; Available backends:
;; - `vibe': vibe-kanban MCP server (cloud/team collaboration)
;; - `standalone': Local org file (personal/offline use)
;;
;; To implement a new backend, define cl-defmethod for each generic:
;;
;;   (cl-defmethod hive-mcp-kanban--list-projects ((_backend (eql my-backend)))
;;     "List projects from my-backend."
;;     ...)

;;; Code:

(require 'cl-lib)

;;;; Backend Protocol Generic Functions:
;;
;; These define the interface contract that all backends must fulfill.
;; Each generic function dispatches on the first argument (backend symbol).

(cl-defgeneric hive-mcp-kanban--list-projects (backend)
  "List all projects from BACKEND.

Returns a list of alists, each with at least:
  - `id': Unique project identifier
  - `name': Human-readable project name

Example return value:
  (((id . \"uuid-1\") (name . \"Project Alpha\"))
   ((id . \"uuid-2\") (name . \"Project Beta\")))")

(cl-defgeneric hive-mcp-kanban--list-tasks (backend project-id &optional status)
  "List tasks from BACKEND for PROJECT-ID.

PROJECT-ID identifies the project (may be nil for some backends).
STATUS optionally filters by kanban status (e.g., \"todo\", \"inprogress\").

Returns a list of task alists, each with:
  - `id': Unique task identifier
  - `title': Task title
  - `status': Kanban status (\"todo\", \"inprogress\", \"inreview\", \"done\")
  - `description': Optional task description
  - `agent': Optional agent identifier that created/modified task
  - `session': Optional session identifier")

(cl-defgeneric hive-mcp-kanban--get-task (backend task-id)
  "Get task details from BACKEND by TASK-ID.

Returns a single task alist with full details:
  - `id': Task identifier
  - `title': Task title
  - `status': Kanban status
  - `description': Full description
  - `agent': Creating agent
  - `session': Creating session
  - `created': Creation timestamp
  - `updated': Last update timestamp

Returns nil if task not found.")

(cl-defgeneric hive-mcp-kanban--create-task (backend project-id title &optional description)
  "Create task in BACKEND for PROJECT-ID with TITLE.

DESCRIPTION is optional task description.
PROJECT-ID may be nil for backends that support a default project.

Returns alist with at least:
  - `id': Newly created task identifier")

(cl-defgeneric hive-mcp-kanban--update-task (backend task-id &rest props)
  "Update TASK-ID in BACKEND with PROPS.

PROPS is a plist with optional keys:
  - `:title': New title
  - `:description': New description
  - `:status': New kanban status

Returns non-nil on success.")

(cl-defgeneric hive-mcp-kanban--delete-task (backend task-id)
  "Delete TASK-ID from BACKEND.

Returns non-nil on success.")

;;;; Status Mapping:

(defcustom hive-mcp-kanban-status-map
  '(("TODO" . "todo")
    ("IN-PROGRESS" . "inprogress")
    ("IN-REVIEW" . "inreview")
    ("DONE" . "done")
    ("CANCELLED" . "cancelled"))
  "Map org TODO states to vibe-kanban statuses.
Used for backend interoperability."
  :type '(alist :key-type string :value-type string)
  :group 'hive-mcp-kanban)

(defun hive-mcp-kanban--org-to-vibe-status (org-status)
  "Convert ORG-STATUS to vibe-kanban status."
  (or (cdr (assoc org-status hive-mcp-kanban-status-map))
      "todo"))

(defun hive-mcp-kanban--vibe-to-org-status (vibe-status)
  "Convert VIBE-STATUS to org TODO state."
  (or (car (rassoc vibe-status hive-mcp-kanban-status-map))
      "TODO"))

;;;; Agent/Session Tracking:

(defcustom hive-mcp-kanban-track-agents t
  "When non-nil, track which agent (session) worked on tasks."
  :type 'boolean
  :group 'hive-mcp-kanban)

(defun hive-mcp-kanban--current-agent ()
  "Get current agent identifier.
Uses CLAUDE_SESSION_ID env var if available, else emacs PID."
  (or (getenv "CLAUDE_SESSION_ID")
      (format "emacs-%s" (emacs-pid))))

(defun hive-mcp-kanban--current-session ()
  "Get current session identifier (timestamp-based)."
  (format-time-string "%Y%m%d-%H%M%S"))

(provide 'hive-mcp-kanban-protocol)
;;; hive-mcp-kanban-protocol.el ends here
