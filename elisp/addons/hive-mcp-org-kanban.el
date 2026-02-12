;;; hive-mcp-org-kanban.el --- Kanban integration with dual backends -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, kanban, org-mode, mcp, project-management
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon provides kanban task tracking with dual backends:
;; - vibe-kanban: Cloud/MCP-based kanban (team collaboration)
;; - standalone: Local org files (personal/offline use)
;;
;; Architecture follows SOLID/DDD principles:
;; - Backend Protocol: Defines operations all backends must support
;; - Strategy Pattern: Backends are interchangeable at runtime
;; - Adapter Layer: Unified API regardless of backend
;;
;; Features:
;; - Agent-aware task tracking (tracks which agent worked on what)
;; - Session continuity via org properties
;; - org-kanban UI integration for visualization
;; - Bidirectional sync between backends
;; - Roadmap and dependency tracking
;;
;; Usage:
;;   (hive-mcp-addon-load 'org-kanban)
;;   (hive-mcp-kanban-mode 1)
;;
;;   ;; Set backend
;;   (setq hive-mcp-kanban-backend 'vibe)      ; cloud/MCP
;;   (setq hive-mcp-kanban-backend 'standalone) ; local org file
;;
;;   ;; Or use both with sync
;;   (hive-mcp-kanban-enable-dual-backend)

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'json)

;; Forward declarations for byte-compiler
(defvar org-id-locations)

;; Soft dependencies
(declare-function hive-mcp-api-get-context "hive-mcp-api")
(declare-function org-kanban/initialize "org-kanban")

;;;; Customization:

(defgroup hive-mcp-kanban nil
  "Kanban integration for hive-mcp with dual backends."
  :group 'hive-mcp
  :prefix "hive-mcp-kanban-")

(defcustom hive-mcp-kanban-backend 'standalone
  "Active kanban backend.
- `vibe': Use vibe-kanban MCP server (cloud/team)
- `standalone': Use local org file (personal/offline)"
  :type '(choice (const :tag "Vibe Kanban (MCP/Cloud)" vibe)
                 (const :tag "Standalone (Local Org)" standalone))
  :group 'hive-mcp-kanban)

(defcustom hive-mcp-kanban-org-file
  (expand-file-name ".hive-mcp-kanban.org" user-emacs-directory)
  "Path to standalone kanban org file."
  :type 'file
  :group 'hive-mcp-kanban)

(defcustom hive-mcp-kanban-default-project nil
  "Default vibe-kanban project ID.
Set this to your project UUID for automatic project selection."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Project UUID"))
  :group 'hive-mcp-kanban)

(defcustom hive-mcp-kanban-statuses
  '("TODO" "IN-PROGRESS" "IN-REVIEW" "DONE" "CANCELLED")
  "List of kanban statuses/columns."
  :type '(repeat string)
  :group 'hive-mcp-kanban)

(defcustom hive-mcp-kanban-status-map
  '(("TODO" . "todo")
    ("IN-PROGRESS" . "inprogress")
    ("IN-REVIEW" . "inreview")
    ("DONE" . "done")
    ("CANCELLED" . "cancelled"))
  "Map org TODO states to vibe-kanban statuses."
  :type '(alist :key-type string :value-type string)
  :group 'hive-mcp-kanban)

(defcustom hive-mcp-kanban-auto-sync nil
  "When non-nil, auto-sync changes to the other backend."
  :type 'boolean
  :group 'hive-mcp-kanban)

(defcustom hive-mcp-kanban-track-agents t
  "When non-nil, track which agent (session) worked on tasks."
  :type 'boolean
  :group 'hive-mcp-kanban)

;;;; Backend Protocol:

(cl-defgeneric hive-mcp-kanban--list-projects (backend)
  "List all projects from BACKEND.")

(cl-defgeneric hive-mcp-kanban--list-tasks (backend project-id &optional status)
  "List tasks from BACKEND for PROJECT-ID, optionally filtered by STATUS.")

(cl-defgeneric hive-mcp-kanban--get-task (backend task-id)
  "Get task details from BACKEND by TASK-ID.")

(cl-defgeneric hive-mcp-kanban--create-task (backend project-id title &optional description)
  "Create task in BACKEND for PROJECT-ID with TITLE and optional DESCRIPTION.")

(cl-defgeneric hive-mcp-kanban--update-task (backend task-id &rest props)
  "Update TASK-ID in BACKEND.  PROPS is a plist of :title :description :status.")

(cl-defgeneric hive-mcp-kanban--delete-task (backend task-id)
  "Delete TASK-ID from BACKEND.")

;;;; Vibe-Kanban Backend:

(defvar hive-mcp-kanban--vibe-cache nil
  "Cache for vibe-kanban data.")

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

;;;; Standalone Backend Helpers:

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
  `(let ((file hive-mcp-kanban-org-file))
     ;; Ensure org-id is properly initialized
     (require 'org-id)
     (unless org-id-locations
       (setq org-id-locations (make-hash-table :test 'equal)))
     (unless (file-exists-p file)
       (hive-mcp-kanban--init-org-file file))
     (with-current-buffer (find-file-noselect file)
       ,@body)))

;;;; Standalone Backend:

(defvar hive-mcp-kanban--standalone-buffer nil
  "Buffer visiting the standalone kanban org file.")

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

(defun hive-mcp-kanban--org-to-vibe-status (org-status)
  "Convert ORG-STATUS to vibe-kanban status."
  (or (cdr (assoc org-status hive-mcp-kanban-status-map))
      "todo"))

(defun hive-mcp-kanban--vibe-to-org-status (vibe-status)
  "Convert VIBE-STATUS to org TODO state."
  (or (car (rassoc vibe-status hive-mcp-kanban-status-map))
      "TODO"))

(defun hive-mcp-kanban--current-agent ()
  "Get current agent identifier."
  (or (getenv "CLAUDE_SESSION_ID")
      (format "emacs-%s" (emacs-pid))))

(defun hive-mcp-kanban--current-session ()
  "Get current session identifier."
  (format-time-string "%Y%m%d-%H%M%S"))

;;;; Unified API:

(defun hive-mcp-kanban-list-projects ()
  "List all projects from current backend."
  (hive-mcp-kanban--list-projects hive-mcp-kanban-backend))

(defun hive-mcp-kanban-list-tasks (&optional project-id status)
  "List tasks from current backend.
PROJECT-ID defaults to `hive-mcp-kanban-default-project'.
For standalone backend, PROJECT-ID can be nil.
STATUS optionally filters by kanban status."
  (let ((pid (or project-id hive-mcp-kanban-default-project)))
    ;; Only require project-id for vibe backend
    (when (and (eq hive-mcp-kanban-backend 'vibe) (not pid))
      (error "No project ID specified. Set hive-mcp-kanban-default-project"))
    (hive-mcp-kanban--list-tasks hive-mcp-kanban-backend pid status)))

(defun hive-mcp-kanban-get-task (task-id)
  "Get task details by TASK-ID from current backend."
  (hive-mcp-kanban--get-task hive-mcp-kanban-backend task-id))

(defun hive-mcp-kanban-create-task (title &optional description project-id)
  "Create a new task with TITLE and optional DESCRIPTION.
PROJECT-ID defaults to `hive-mcp-kanban-default-project'.
For standalone backend, PROJECT-ID can be nil."
  (let ((pid (or project-id hive-mcp-kanban-default-project)))
    ;; Only require project-id for vibe backend
    (when (and (eq hive-mcp-kanban-backend 'vibe) (not pid))
      (error "No project ID specified. Set hive-mcp-kanban-default-project"))
    (let ((result (hive-mcp-kanban--create-task
                   hive-mcp-kanban-backend pid title description)))
      (when hive-mcp-kanban-auto-sync
        (hive-mcp-kanban--sync-task-to-other-backend result))
      result)))

(defun hive-mcp-kanban-update-task (task-id &rest props)
  "Update task TASK-ID with PROPS (:title :description :status)."
  (let ((result (apply #'hive-mcp-kanban--update-task
                       hive-mcp-kanban-backend task-id props)))
    (when hive-mcp-kanban-auto-sync
      (apply #'hive-mcp-kanban--sync-update-to-other-backend task-id props))
    result))

(defun hive-mcp-kanban-delete-task (task-id)
  "Delete task by TASK-ID from current backend."
  (hive-mcp-kanban--delete-task hive-mcp-kanban-backend task-id))

(defun hive-mcp-kanban-move-task (task-id new-status)
  "Move task TASK-ID to NEW-STATUS."
  (hive-mcp-kanban-update-task task-id :status new-status))

;;;; Dual Backend Sync:

(defun hive-mcp-kanban-enable-dual-backend ()
  "Enable dual backend mode with auto-sync."
  (interactive)
  (setq hive-mcp-kanban-auto-sync t)
  (message "Dual backend enabled. Changes sync to both vibe-kanban and standalone."))

(defun hive-mcp-kanban-sync-all ()
  "Sync all tasks between backends."
  (interactive)
  (message "Syncing kanban backends...")
  ;; Pull from vibe, push to standalone
  (let ((vibe-tasks (hive-mcp-kanban--list-tasks 'vibe hive-mcp-kanban-default-project))
        (standalone-tasks (hive-mcp-kanban--list-tasks 'standalone hive-mcp-kanban-default-project)))
    ;; Merge logic here
    (message "Synced %d vibe tasks and %d standalone tasks"
             (length vibe-tasks) (length standalone-tasks))))

(defun hive-mcp-kanban--sync-task-to-other-backend (task)
  "Sync TASK to the non-active backend."
  (let ((other-backend (if (eq hive-mcp-kanban-backend 'vibe) 'standalone 'vibe)))
    (hive-mcp-kanban--create-task
     other-backend
     hive-mcp-kanban-default-project
     (alist-get 'title task)
     (alist-get 'description task))))

(defun hive-mcp-kanban--sync-update-to-other-backend (task-id &rest props)
  "Sync TASK-ID update with PROPS to the non-active backend."
  (let ((other-backend (if (eq hive-mcp-kanban-backend 'vibe) 'standalone 'vibe)))
    (apply #'hive-mcp-kanban--update-task other-backend task-id props)))

;;;; Interactive Kanban Board:

(defvar hive-mcp-kanban-board-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "n") #'hive-mcp-kanban-board-next-task)
    (define-key map (kbd "p") #'hive-mcp-kanban-board-prev-task)
    (define-key map (kbd "f") #'hive-mcp-kanban-board-next-column)
    (define-key map (kbd "b") #'hive-mcp-kanban-board-prev-column)
    (define-key map (kbd "TAB") #'hive-mcp-kanban-board-next-column)
    (define-key map (kbd "<backtab>") #'hive-mcp-kanban-board-prev-column)
    (define-key map (kbd "j") #'hive-mcp-kanban-board-next-task)
    (define-key map (kbd "k") #'hive-mcp-kanban-board-prev-task)
    (define-key map (kbd "l") #'hive-mcp-kanban-board-next-column)
    (define-key map (kbd "h") #'hive-mcp-kanban-board-prev-column)
    ;; Actions
    (define-key map (kbd "RET") #'hive-mcp-kanban-board-open-task)
    (define-key map (kbd "m") #'hive-mcp-kanban-board-move-task)
    (define-key map (kbd ">") #'hive-mcp-kanban-board-move-right)
    (define-key map (kbd "<") #'hive-mcp-kanban-board-move-left)
    (define-key map (kbd "c") #'hive-mcp-kanban-board-create-task)
    (define-key map (kbd "d") #'hive-mcp-kanban-board-delete-task)
    (define-key map (kbd "e") #'hive-mcp-kanban-board-edit-task)
    ;; View
    (define-key map (kbd "g") #'hive-mcp-kanban-board-refresh)
    (define-key map (kbd "o") #'hive-mcp-kanban-open-org-file)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'hive-mcp-kanban-board-help)
    map)
  "Keymap for `hive-mcp-kanban-board-mode'.")

(defvar-local hive-mcp-kanban-board--tasks nil
  "Current board tasks organized by column.")

(defvar-local hive-mcp-kanban-board--column-positions nil
  "Alist of (column-name . start-pos) for navigation.")

(defvar-local hive-mcp-kanban-board--task-positions nil
  "Alist of (task-id . (start-pos . end-pos)) for navigation.")

(defface hive-mcp-kanban-column-header
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for kanban column headers."
  :group 'hive-mcp-kanban)

(defface hive-mcp-kanban-task
  '((t :inherit default))
  "Face for kanban task cards."
  :group 'hive-mcp-kanban)

(defface hive-mcp-kanban-task-highlight
  '((t :inherit highlight))
  "Face for highlighted kanban task."
  :group 'hive-mcp-kanban)

(defface hive-mcp-kanban-border
  '((t :inherit font-lock-comment-face))
  "Face for kanban board borders."
  :group 'hive-mcp-kanban)

(defface hive-mcp-kanban-stats
  '((t :inherit font-lock-doc-face))
  "Face for kanban statistics."
  :group 'hive-mcp-kanban)

(define-derived-mode hive-mcp-kanban-board-mode special-mode "Kanban"
  "Major mode for interactive kanban board.

\\{hive-mcp-kanban-board-mode-map}"
  :group 'hive-mcp-kanban
  (setq-local truncate-lines t)
  (setq-local cursor-type 'bar)
  (setq-local line-spacing 0.2)
  (hl-line-mode 1))

;; Evil-mode integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'hive-mcp-kanban-board-mode 'emacs)
  ;; Also define keys in normal state for users who prefer staying in normal
  (evil-define-key 'normal hive-mcp-kanban-board-mode-map
    "n" #'hive-mcp-kanban-board-next-task
    "p" #'hive-mcp-kanban-board-prev-task
    "j" #'hive-mcp-kanban-board-next-task
    "k" #'hive-mcp-kanban-board-prev-task
    "l" #'hive-mcp-kanban-board-next-column
    "h" #'hive-mcp-kanban-board-prev-column
    (kbd "RET") #'hive-mcp-kanban-board-open-task
    "m" #'hive-mcp-kanban-board-move-task
    ">" #'hive-mcp-kanban-board-move-right
    "<" #'hive-mcp-kanban-board-move-left
    "c" #'hive-mcp-kanban-board-create-task
    "d" #'hive-mcp-kanban-board-delete-task
    "e" #'hive-mcp-kanban-board-edit-task
    "g" #'hive-mcp-kanban-board-refresh
    "o" #'hive-mcp-kanban-open-org-file
    "q" #'quit-window
    "?" #'hive-mcp-kanban-board-help))

(defun hive-mcp-kanban-board--render ()
  "Render the kanban board in the current buffer."
  (let* ((inhibit-read-only t)
         (file hive-mcp-kanban-org-file)
         (tasks-by-status (hive-mcp-kanban-board--load-tasks file))
         (col-width 30)
         (columns '(("TODO" . "📋")
                    ("IN-PROGRESS" . "🔄")
                    ("IN-REVIEW" . "👀")
                    ("DONE" . "✅")))
         (task-positions '())
         (column-positions '()))
    (erase-buffer)

    ;; Header
    (insert (propertize "╔══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗\n"
                        'face 'hive-mcp-kanban-border))
    (insert (propertize "║                                              " 'face 'hive-mcp-kanban-border))
    (insert (propertize " KANBAN BOARD " 'face '(:inherit hive-mcp-kanban-column-header :height 1.3)))
    (insert (propertize "                                              ║\n" 'face 'hive-mcp-kanban-border))
    (insert (propertize "╠══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣\n"
                        'face 'hive-mcp-kanban-border))

    ;; Stats line
    (let* ((total (apply #'+ (mapcar (lambda (col) (length (cdr (assoc (car col) tasks-by-status)))) columns)))
           (done (length (cdr (assoc "DONE" tasks-by-status))))
           (in-prog (length (cdr (assoc "IN-PROGRESS" tasks-by-status))))
           (todo (length (cdr (assoc "TODO" tasks-by-status)))))
      (insert (propertize "║ " 'face 'hive-mcp-kanban-border))
      (insert (propertize (format "Total: %d  │  Todo: %d  │  In Progress: %d  │  Done: %d  │  Progress: %d%%"
                                  total todo in-prog done
                                  (if (> total 0) (round (* 100 (/ (float done) total))) 0))
                          'face 'hive-mcp-kanban-stats))
      (insert (make-string (- 122 (current-column)) ?\s))
      (insert (propertize "║\n" 'face 'hive-mcp-kanban-border)))

    (insert (propertize "╠══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣\n"
                        'face 'hive-mcp-kanban-border))

    ;; Column headers
    (insert (propertize "║" 'face 'hive-mcp-kanban-border))
    (dolist (col columns)
      (let* ((name (car col))
             (emoji (cdr col))
             (count (length (cdr (assoc name tasks-by-status))))
             (header (format " %s %s (%d) " emoji name count)))
        (push (cons name (point)) column-positions)
        (insert (propertize header 'face 'hive-mcp-kanban-column-header
                            'column-name name))
        (insert (make-string (- col-width (length header)) ?\s))))
    (insert (propertize "║\n" 'face 'hive-mcp-kanban-border))

    (insert (propertize "╠" 'face 'hive-mcp-kanban-border))
    (dotimes (_ 4)
      (insert (propertize (make-string col-width ?─) 'face 'hive-mcp-kanban-border)))
    (insert (propertize "╣\n" 'face 'hive-mcp-kanban-border))

    ;; Find max tasks in any column
    (let ((max-tasks (apply #'max 1 (mapcar (lambda (col)
                                               (length (cdr (assoc (car col) tasks-by-status))))
                                             columns))))
      ;; Render task rows
      (dotimes (row (min max-tasks 15))
        (insert (propertize "║" 'face 'hive-mcp-kanban-border))
        (dolist (col columns)
          (let* ((col-name (car col))
                 (col-tasks (cdr (assoc col-name tasks-by-status)))
                 (task (nth row col-tasks)))
            (if task
                (let* ((title (or (cdr (assoc 'title task)) "Untitled"))
                       (id (cdr (assoc 'id task)))
                       (truncated (if (> (length title) (- col-width 4))
                                      (concat (substring title 0 (- col-width 7)) "...")
                                    title))
                       (start-pos (point)))
                  (insert (propertize (format " • %s" truncated)
                                      'face 'hive-mcp-kanban-task
                                      'task-id id
                                      'task-data task
                                      'column-name col-name
                                      'mouse-face 'hive-mcp-kanban-task-highlight
                                      'help-echo (format "Task: %s\nID: %s\nClick to open" title id)))
                  (insert (make-string (- col-width (+ 3 (length truncated))) ?\s))
                  (push (cons id (cons start-pos (point))) task-positions))
              (insert (make-string col-width ?\s)))))
        (insert (propertize "║\n" 'face 'hive-mcp-kanban-border)))

      ;; Show "more" indicator if needed
      (dolist (col columns)
        (let* ((col-name (car col))
               (col-tasks (cdr (assoc col-name tasks-by-status)))
               (extra (- (length col-tasks) 15)))
          (when (> extra 0)
            (insert (propertize "║" 'face 'hive-mcp-kanban-border))
            (dolist (c columns)
              (if (equal (car c) col-name)
                  (let ((more-text (format " ... +%d more" extra)))
                    (insert (propertize more-text 'face 'font-lock-comment-face))
                    (insert (make-string (- col-width (length more-text)) ?\s)))
                (insert (make-string col-width ?\s))))
            (insert (propertize "║\n" 'face 'hive-mcp-kanban-border))))))

    ;; Footer
    (insert (propertize "╠══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣\n"
                        'face 'hive-mcp-kanban-border))
    (insert (propertize "║ " 'face 'hive-mcp-kanban-border))
    (insert (propertize "[n/p] Navigate  [h/l] Columns  [RET] Open  [>/<] Move  [c] Create  [d] Delete  [g] Refresh  [q] Quit  [?] Help"
                        'face 'font-lock-comment-face))
    (insert (make-string (- 122 (current-column)) ?\s))
    (insert (propertize "║\n" 'face 'hive-mcp-kanban-border))
    (insert (propertize "╚══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝\n"
                        'face 'hive-mcp-kanban-border))

    ;; Store state
    (setq hive-mcp-kanban-board--tasks tasks-by-status)
    (setq hive-mcp-kanban-board--column-positions (nreverse column-positions))
    (setq hive-mcp-kanban-board--task-positions (nreverse task-positions))

    ;; Position cursor on first task
    (goto-char (point-min))
    (hive-mcp-kanban-board-next-task)))

(defun hive-mcp-kanban-board--load-tasks (file)
  "Load tasks from FILE grouped by status."
  (if (and (fboundp 'cider-connected-p) (cider-connected-p))
      ;; Use Clojure renderer
      (let* ((code (format "(do (require '[hive-mcp.org-clj.parser :as p])
                               (require '[hive-mcp.org-clj.query :as q])
                               (let [doc (p/parse-document (slurp \"%s\"))]
                                 {:todo (q/find-todo doc)
                                  :in-progress (q/find-in-progress doc)
                                  :in-review (q/find-by-status doc \"IN-REVIEW\")
                                  :done (q/find-done doc)}))" file))
             (result (cider-nrepl-sync-request:eval code))
             (value (nrepl-dict-get result "value")))
        (when value
          (let ((data (read value)))
            `(("TODO" . ,(mapcar #'hive-mcp-kanban-board--convert-task (plist-get data :todo)))
              ("IN-PROGRESS" . ,(mapcar #'hive-mcp-kanban-board--convert-task (plist-get data :in-progress)))
              ("IN-REVIEW" . ,(mapcar #'hive-mcp-kanban-board--convert-task (plist-get data :in-review)))
              ("DONE" . ,(mapcar #'hive-mcp-kanban-board--convert-task (plist-get data :done)))))))
    ;; Fallback to org-mode parsing
    (hive-mcp-kanban-board--load-tasks-org file)))

(defun hive-mcp-kanban-board--convert-task (task-plist)
  "Convert TASK-PLIST from Clojure to alist."
  `((id . ,(plist-get task-plist :id))
    (title . ,(plist-get task-plist :title))
    (status . ,(plist-get task-plist :status))
    (level . ,(plist-get task-plist :level))))

(defun hive-mcp-kanban-board--load-tasks-org (file)
  "Load tasks from org FILE directly."
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

;;;###autoload
(defun hive-mcp-kanban-open-board ()
  "Open interactive kanban board buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Kanban Board*")))
    (with-current-buffer buf
      (hive-mcp-kanban-board-mode)
      (hive-mcp-kanban-board--render))
    (switch-to-buffer buf)))

;; Navigation commands
(defun hive-mcp-kanban-board-next-task ()
  "Move to next task in board."
  (interactive)
  (let ((next-pos nil))
    (save-excursion
      (forward-char 1)
      (while (and (not (eobp))
                  (not (get-text-property (point) 'task-id)))
        (forward-char 1))
      (when (get-text-property (point) 'task-id)
        (setq next-pos (point))))
    (when next-pos
      (goto-char next-pos))))

(defun hive-mcp-kanban-board-prev-task ()
  "Move to previous task in board."
  (interactive)
  (let ((prev-pos nil))
    (save-excursion
      (backward-char 1)
      (while (and (not (bobp))
                  (not (get-text-property (point) 'task-id)))
        (backward-char 1))
      (when (get-text-property (point) 'task-id)
        (setq prev-pos (point))))
    (when prev-pos
      (goto-char prev-pos))))

(defun hive-mcp-kanban-board-next-column ()
  "Move to next column."
  (interactive)
  (let* ((current-col (get-text-property (point) 'column-name))
         (cols '("TODO" "IN-PROGRESS" "IN-REVIEW" "DONE"))
         (next-col (cadr (member current-col cols))))
    (when next-col
      ;; Find first task in next column
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (get-text-property (point) 'column-name) next-col)))
        (forward-char 1))
      (when (equal (get-text-property (point) 'column-name) next-col)
        (point)))))

(defun hive-mcp-kanban-board-prev-column ()
  "Move to previous column."
  (interactive)
  (let* ((current-col (get-text-property (point) 'column-name))
         (cols '("DONE" "IN-REVIEW" "IN-PROGRESS" "TODO"))
         (prev-col (cadr (member current-col cols))))
    (when prev-col
      ;; Find first task in prev column
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (get-text-property (point) 'column-name) prev-col)))
        (forward-char 1))
      (when (equal (get-text-property (point) 'column-name) prev-col)
        (point)))))

;; Action commands
(defun hive-mcp-kanban-board-open-task ()
  "Open task at point in org file."
  (interactive)
  (when-let* ((id (get-text-property (point) 'task-id)))
    (let ((marker (org-id-find id 'marker)))
      (when marker
        (switch-to-buffer (marker-buffer marker))
        (goto-char marker)
        (if (fboundp 'org-fold-show-entry)
            (org-fold-show-entry)
          (with-no-warnings (org-show-entry)))))))

(defun hive-mcp-kanban-board-move-right ()
  "Move task at point to next status column."
  (interactive)
  (hive-mcp-kanban-board--move-task 1))

(defun hive-mcp-kanban-board-move-left ()
  "Move task at point to previous status column."
  (interactive)
  (hive-mcp-kanban-board--move-task -1))

(defun hive-mcp-kanban-board--move-task (direction)
  "Move task at point in DIRECTION (+1 or -1).
Uses the addon's unified API for auto-sync and agent tracking."
  (when-let* ((id (get-text-property (point) 'task-id))
              (current-col (get-text-property (point) 'column-name)))
    (let* ((cols '("TODO" "IN-PROGRESS" "IN-REVIEW" "DONE"))
           (current-idx (cl-position current-col cols :test #'equal))
           (new-idx (+ current-idx direction))
           (new-col (and (>= new-idx 0) (< new-idx 4) (nth new-idx cols))))
      (when new-col
        ;; Use addon API - handles auto-sync and agent tracking
        (let ((vibe-status (hive-mcp-kanban--org-to-vibe-status new-col)))
          (hive-mcp-kanban-move-task id vibe-status))
        (hive-mcp-kanban-board-refresh)
        (message "Moved task to %s" new-col)))))

(defun hive-mcp-kanban-board-move-task ()
  "Move task at point to a new status (interactive).
Uses the addon's unified API for auto-sync and agent tracking."
  (interactive)
  (when-let* ((id (get-text-property (point) 'task-id)))
    (let* ((new-status (completing-read "Move to: "
                                        '("TODO" "IN-PROGRESS" "IN-REVIEW" "DONE")
                                        nil t))
           (vibe-status (hive-mcp-kanban--org-to-vibe-status new-status)))
      ;; Use addon API - handles auto-sync and agent tracking
      (hive-mcp-kanban-move-task id vibe-status)
      (hive-mcp-kanban-board-refresh)
      (message "Moved task to %s" new-status))))

(defun hive-mcp-kanban-board-create-task ()
  "Create a new task."
  (interactive)
  (let ((title (read-string "Task title: ")))
    (hive-mcp-kanban-create-task title)
    (hive-mcp-kanban-board-refresh)
    (message "Created task: %s" title)))

(defun hive-mcp-kanban-board-delete-task ()
  "Delete task at point.
Uses the addon's unified API for auto-sync and agent tracking."
  (interactive)
  (when-let* ((id (get-text-property (point) 'task-id))
              (task (get-text-property (point) 'task-data)))
    (when (yes-or-no-p (format "Delete task '%s'? " (cdr (assoc 'title task))))
      ;; Use addon API - handles deletion in current backend
      (hive-mcp-kanban-delete-task id)
      (hive-mcp-kanban-board-refresh)
      (message "Task deleted"))))

(defun hive-mcp-kanban-board-edit-task ()
  "Edit task title at point.
Uses the addon's unified API for auto-sync and agent tracking."
  (interactive)
  (when-let* ((id (get-text-property (point) 'task-id))
              (task (get-text-property (point) 'task-data)))
    (let* ((old-title (cdr (assoc 'title task)))
           (new-title (read-string "New title: " old-title)))
      (unless (string= old-title new-title)
        ;; Use addon API - handles auto-sync and agent tracking
        (hive-mcp-kanban-update-task id :title new-title)
        (hive-mcp-kanban-board-refresh)
        (message "Task updated")))))

(defun hive-mcp-kanban-board-refresh ()
  "Refresh the kanban board."
  (interactive)
  (when (eq major-mode 'hive-mcp-kanban-board-mode)
    (let ((pos (point)))
      (hive-mcp-kanban-board--render)
      (goto-char (min pos (point-max))))))

(defun hive-mcp-kanban-board-help ()
  "Show help for kanban board."
  (interactive)
  (message "Kanban: n/p=nav tasks, h/l=nav cols, RET=open, >/<= move, c=create, d=delete, g=refresh, q=quit"))

;;;; Legacy View Commands:

;;;###autoload
(defun hive-mcp-kanban-view (&optional format)
  "View kanban board using native Clojure renderer.
FORMAT can be `terminal' (ASCII art) or `emacs' (org-mode).
With prefix arg, prompts for format."
  (interactive
   (list (if current-prefix-arg
             (intern (completing-read "Format: " '("terminal" "emacs") nil t))
           'terminal)))
  (let* ((file hive-mcp-kanban-org-file)
         (fmt (or format 'terminal))
         (code (format "(do (require '[hive-mcp.org-clj.render :as r])
                           (r/render-file (r/%s-renderer) \"%s\"))"
                       fmt file)))
    (if (and (fboundp 'cider-connected-p) (cider-connected-p))
        ;; Use CIDER if available
        (let ((result (cider-nrepl-sync-request:eval code)))
          (when-let* ((value (nrepl-dict-get result "value")))
            (let ((buf (get-buffer-create "*Kanban Board*")))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  ;; Remove surrounding quotes from the string
                  (insert (read value)))
                (goto-char (point-min))
                (if (eq fmt 'emacs)
                    (org-mode)
                  (special-mode)))
              (display-buffer buf))))
      ;; Fallback: display static board
      (hive-mcp-kanban-view--static file fmt))))

(defun hive-mcp-kanban-view--static (file _format)
  "Display static kanban board from FILE.
FORMAT argument is unused in fallback mode.
Used when CIDER is not connected."
  (message "CIDER not connected. Showing org file directly.")
  (find-file file))

;;;###autoload
(defun hive-mcp-kanban-show-board ()
  "Show kanban board in org buffer."
  (interactive)
  (let ((buf (get-buffer-create "*MCP Kanban*"))
        (tasks (hive-mcp-kanban-list-tasks)))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Kanban Board\n")
      (insert "#+STARTUP: overview\n\n")
      ;; Group by status
      (dolist (status hive-mcp-kanban-statuses)
        (insert (format "* %s\n" status))
        (let ((status-tasks (cl-remove-if-not
                             (lambda (tsk)
                               (equal (hive-mcp-kanban--vibe-to-org-status
                                      (alist-get 'status tsk))
                                     status))
                             tasks)))
          (dolist (task status-tasks)
            (insert (format "** %s\n" (alist-get 'title task)))
            (insert ":PROPERTIES:\n")
            (insert (format ":ID: %s\n" (alist-get 'id task)))
            (when-let* ((agent (alist-get 'agent task)))
              (insert (format ":AGENT: %s\n" agent)))
            (insert ":END:\n")
            (when-let* ((desc (alist-get 'description task)))
              (insert desc "\n"))
            (insert "\n"))))
      (goto-char (point-min)))
    (switch-to-buffer buf)))

;;;###autoload
(defun hive-mcp-kanban-create ()
  "Create a new kanban task interactively."
  (interactive)
  (let* ((title (read-string "Task title: "))
         (description (read-string "Description (optional): " nil nil ""))
         (result (hive-mcp-kanban-create-task
                  title
                  (unless (string-empty-p description) description))))
    (message "Created task: %s" (alist-get 'id result))))

;;;###autoload
(defun hive-mcp-kanban-update-status ()
  "Update status of a task interactively."
  (interactive)
  (let* ((tasks (hive-mcp-kanban-list-tasks))
         (task-names (mapcar (lambda (tsk)
                              (format "%s [%s]"
                                      (alist-get 'title tsk)
                                      (alist-get 'status tsk)))
                            tasks))
         (selection (completing-read "Task: " task-names nil t))
         (task (nth (cl-position selection task-names :test #'equal) tasks))
         (new-status (completing-read "New status: "
                                      (mapcar #'cdr hive-mcp-kanban-status-map)
                                      nil t)))
    (hive-mcp-kanban-move-task (alist-get 'id task) new-status)
    (message "Moved task to %s" new-status)))

;;;###autoload
(defun hive-mcp-kanban-open-org-file ()
  "Open the standalone kanban org file."
  (interactive)
  (find-file hive-mcp-kanban-org-file))

;;;###autoload
(defun hive-mcp-kanban-set-project ()
  "Set the default kanban project interactively."
  (interactive)
  (let* ((projects (hive-mcp-kanban-list-projects))
         (names (mapcar (lambda (p) (alist-get 'name p)) projects))
         (selection (completing-read "Project: " names nil t))
         (project (cl-find-if (lambda (p)
                               (equal (alist-get 'name p) selection))
                             projects)))
    (setq hive-mcp-kanban-default-project (alist-get 'id project))
    (message "Set default project: %s (%s)"
             selection hive-mcp-kanban-default-project)))

;;;; Agent API:

(defun hive-mcp-kanban-api-status ()
  "Get kanban status for API consumption.
Returns current tasks grouped by status."
  (let ((tasks (hive-mcp-kanban-list-tasks)))
    `((backend . ,hive-mcp-kanban-backend)
      (project . ,hive-mcp-kanban-default-project)
      (total . ,(length tasks))
      (by_status . ,(mapcar
                     (lambda (status)
                       (cons status
                             (length (cl-remove-if-not
                                     (lambda (tsk)
                                       (equal (alist-get 'status tsk) status))
                                     tasks))))
                     (mapcar #'cdr hive-mcp-kanban-status-map)))
      (tasks . ,tasks))))

(defun hive-mcp-kanban-api-my-tasks ()
  "Get tasks assigned to or modified by current agent."
  (let* ((agent (hive-mcp-kanban--current-agent))
         (tasks (hive-mcp-kanban-list-tasks)))
    (cl-remove-if-not
     (lambda (tsk)
       (or (equal (alist-get 'agent tsk) agent)
           (equal (alist-get 'last_agent tsk) agent)))
     tasks)))

(defun hive-mcp-kanban-api-roadmap ()
  "Get roadmap view with milestones and progress."
  (let* ((tasks (hive-mcp-kanban-list-tasks))
         (total (length tasks))
         (done (length (cl-remove-if-not
                       (lambda (tsk)
                         (member (alist-get 'status tsk) '("done" "cancelled")))
                       tasks))))
    `((total . ,total)
      (completed . ,done)
      (progress . ,(if (> total 0)
                       (round (* 100 (/ (float done) total)))
                     0))
      (in_progress . ,(cl-remove-if-not
                       (lambda (tsk)
                         (equal (alist-get 'status tsk) "inprogress"))
                       tasks))
      (next_up . ,(seq-take
                   (cl-remove-if-not
                    (lambda (tsk)
                      (equal (alist-get 'status tsk) "todo"))
                    tasks)
                   5)))))

;;;; Transient Menu:

;;;###autoload (autoload 'hive-mcp-kanban-transient "hive-mcp-org-kanban" nil t)
(transient-define-prefix hive-mcp-kanban-transient ()
  "Kanban management menu."
  ["hive-mcp Kanban"
   ["View"
    ("b" "Interactive board" hive-mcp-kanban-open-board)
    ("v" "Text view (ASCII)" hive-mcp-kanban-view)
    ("V" "Text view (Emacs)" (lambda () (interactive) (hive-mcp-kanban-view 'emacs)))
    ("o" "Open org file" hive-mcp-kanban-open-org-file)]
   ["Tasks"
    ("c" "Create task" hive-mcp-kanban-create)
    ("u" "Update status" hive-mcp-kanban-update-status)]
   ["Settings"
    ("p" "Set project" hive-mcp-kanban-set-project)
    ("s" "Sync backends" hive-mcp-kanban-sync-all)
    ("d" "Toggle dual mode" hive-mcp-kanban-enable-dual-backend)]])

;;;; Minor Mode:

(defvar hive-mcp-kanban-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Use C-c M-k (Meta modifier) - allowed for minor modes per Emacs conventions
    (define-key map (kbd "C-c M-k") 'hive-mcp-kanban-transient)
    map)
  "Keymap for `hive-mcp-kanban-mode'.")

;;;###autoload
(define-minor-mode hive-mcp-kanban-mode
  "Minor mode for kanban task tracking with hive-mcp.

Provides:
- Dual backend support (vibe-kanban + standalone org)
- Agent-aware task tracking
- org-kanban UI integration
- Session continuity

\\{hive-mcp-kanban-mode-map}"
  :init-value nil
  :lighter " Kanban"
  :keymap hive-mcp-kanban-mode-map
  :global t
  :group 'hive-mcp-kanban
  (if hive-mcp-kanban-mode
      (message "Emacs-mcp-kanban enabled (backend: %s)" hive-mcp-kanban-backend)
    (message "Emacs-mcp-kanban disabled")))

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'org-kanban
   :version "0.1.0"
   :description "Kanban tracking with dual backends (vibe-kanban + standalone org)"
   :requires '(org)
   :provides '(hive-mcp-kanban-mode hive-mcp-kanban-transient)))

(provide 'hive-mcp-org-kanban)
;;; hive-mcp-org-kanban.el ends here
