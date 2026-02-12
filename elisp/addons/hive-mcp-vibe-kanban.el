;;; hive-mcp-vibe-kanban.el --- Vibe Kanban integration for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, project, kanban
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates vibe-kanban (task management MCP server) with hive-mcp.
;;
;; Vibe Kanban provides a visual kanban board for task management that integrates
;; with Claude Code and other MCP clients.
;;
;; Features:
;; - Auto-start vibe-kanban server on addon load (async, non-blocking)
;; - Auto-shutdown when addon is unloaded
;; - Customizable port and command
;;
;; Usage:
;;   ;; Add to always-load for auto-start on Emacs startup:
;;   (add-to-list 'hive-mcp-addon-always-load 'vibe-kanban)
;;
;;   ;; Or set auto-start and load manually:
;;   (setq hive-mcp-vibe-kanban-auto-start t)
;;   (hive-mcp-addon-load 'vibe-kanban)
;;
;; Prerequisites:
;;   npm install -g vibe-kanban
;;   OR
;;   npx vibe-kanban (will auto-install)

;;; Code:

(require 'hive-mcp-api)

;;;; Customization:

(defgroup hive-mcp-vibe-kanban nil
  "Vibe Kanban integration for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-vibe-kanban-")

(defcustom hive-mcp-vibe-kanban-auto-start t
  "When non-nil, automatically start vibe-kanban server on addon load.
The server starts asynchronously and does not block Emacs startup."
  :type 'boolean
  :group 'hive-mcp-vibe-kanban)

(defcustom hive-mcp-vibe-kanban-command "npx"
  "Command to run vibe-kanban.
Default is `npx' which auto-installs if needed."
  :type 'string
  :group 'hive-mcp-vibe-kanban)

(defcustom hive-mcp-vibe-kanban-args '("vibe-kanban")
  "Arguments to pass to the vibe-kanban command."
  :type '(repeat string)
  :group 'hive-mcp-vibe-kanban)

(defcustom hive-mcp-vibe-kanban-port 3333
  "Port for vibe-kanban server."
  :type 'integer
  :group 'hive-mcp-vibe-kanban)

(defcustom hive-mcp-vibe-kanban-project-dir nil
  "Project directory for vibe-kanban.
If nil, uses current project root or `default-directory'."
  :type '(choice (const nil) directory)
  :group 'hive-mcp-vibe-kanban)

;;;; Internal Variables:

(defvar hive-mcp-vibe-kanban--process nil
  "Process object for the vibe-kanban server.")

(defvar hive-mcp-vibe-kanban--buffer-name "*vibe-kanban*"
  "Buffer name for vibe-kanban output.")

;;;; Helper Functions:

(defun hive-mcp-vibe-kanban--project-dir ()
  "Get the project directory for vibe-kanban."
  (or hive-mcp-vibe-kanban-project-dir
      (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun hive-mcp-vibe-kanban--running-p ()
  "Return non-nil if vibe-kanban server is running."
  (and hive-mcp-vibe-kanban--process
       (process-live-p hive-mcp-vibe-kanban--process)))

;;;; Server Control:

(defun hive-mcp-vibe-kanban--start-async ()
  "Start vibe-kanban server asynchronously.
Returns the process object."
  (unless (hive-mcp-vibe-kanban--running-p)
    (let* ((default-directory (hive-mcp-vibe-kanban--project-dir))
           (buf (get-buffer-create hive-mcp-vibe-kanban--buffer-name)))
      (message "hive-mcp-vibe-kanban: Starting server...")
      (setq hive-mcp-vibe-kanban--process
            (apply #'start-process
                   "vibe-kanban"
                   buf
                   hive-mcp-vibe-kanban-command
                   hive-mcp-vibe-kanban-args))
      ;; Set up process sentinel for status messages
      (set-process-sentinel
       hive-mcp-vibe-kanban--process
       (lambda (proc event)
         (message "hive-mcp-vibe-kanban: %s" (string-trim event))))
      hive-mcp-vibe-kanban--process)))

(defun hive-mcp-vibe-kanban--stop ()
  "Stop the vibe-kanban server."
  (when (hive-mcp-vibe-kanban--running-p)
    (kill-process hive-mcp-vibe-kanban--process)
    (setq hive-mcp-vibe-kanban--process nil)
    (message "hive-mcp-vibe-kanban: Server stopped")))

;;;; Interactive Commands:

;;;###autoload
(defun hive-mcp-vibe-kanban-start ()
  "Start the vibe-kanban server."
  (interactive)
  (if (hive-mcp-vibe-kanban--running-p)
      (message "hive-mcp-vibe-kanban: Server already running")
    (hive-mcp-vibe-kanban--start-async)))

;;;###autoload
(defun hive-mcp-vibe-kanban-stop ()
  "Stop the vibe-kanban server."
  (interactive)
  (hive-mcp-vibe-kanban--stop))

;;;###autoload
(defun hive-mcp-vibe-kanban-restart ()
  "Restart the vibe-kanban server."
  (interactive)
  (hive-mcp-vibe-kanban--stop)
  (sit-for 0.5)
  (hive-mcp-vibe-kanban--start-async))

;;;###autoload
(defun hive-mcp-vibe-kanban-status ()
  "Show vibe-kanban server status."
  (interactive)
  (message "hive-mcp-vibe-kanban: %s"
           (if (hive-mcp-vibe-kanban--running-p)
               "Running"
             "Stopped")))

;;;###autoload
(defun hive-mcp-vibe-kanban-show-buffer ()
  "Show the vibe-kanban output buffer."
  (interactive)
  (if-let* ((buf (get-buffer hive-mcp-vibe-kanban--buffer-name)))
      (display-buffer buf)
    (message "hive-mcp-vibe-kanban: No output buffer")))

;;;; Addon Lifecycle Functions:

(defun hive-mcp-vibe-kanban--addon-init ()
  "Synchronous init for vibe-kanban addon."
  (require 'hive-mcp-api nil t)
  (message "hive-mcp-vibe-kanban: initialized"))

(defun hive-mcp-vibe-kanban--addon-async-init ()
  "Asynchronous init for vibe-kanban addon.
Starts the server in background if auto-start is enabled.
Returns the process object for lifecycle tracking."
  (when hive-mcp-vibe-kanban-auto-start
    (hive-mcp-vibe-kanban--start-async)))

(defun hive-mcp-vibe-kanban--addon-shutdown ()
  "Shutdown function for vibe-kanban addon."
  (hive-mcp-vibe-kanban--stop)
  (message "hive-mcp-vibe-kanban: shutdown complete"))

;;;; Transient Menu:

;;;###autoload (autoload 'hive-mcp-vibe-kanban-transient "hive-mcp-vibe-kanban" nil t)
(transient-define-prefix hive-mcp-vibe-kanban-transient ()
  "Vibe Kanban control menu."
  ["Vibe Kanban"
   ["Server"
    ("s" "Start" hive-mcp-vibe-kanban-start)
    ("S" "Stop" hive-mcp-vibe-kanban-stop)
    ("r" "Restart" hive-mcp-vibe-kanban-restart)
    ("?" "Status" hive-mcp-vibe-kanban-status)]
   ["View"
    ("b" "Show buffer" hive-mcp-vibe-kanban-show-buffer)]])

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'vibe-kanban
   :version "0.1.0"
   :description "Vibe Kanban task management server"
   :requires '(hive-mcp-api)
   :provides '(hive-mcp-vibe-kanban-transient)
   :init #'hive-mcp-vibe-kanban--addon-init
   :async-init #'hive-mcp-vibe-kanban--addon-async-init
   :shutdown #'hive-mcp-vibe-kanban--addon-shutdown))

(provide 'hive-mcp-vibe-kanban)
;;; hive-mcp-vibe-kanban.el ends here
