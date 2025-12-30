;;; emacs-mcp-vibe-kanban.el --- Vibe Kanban integration for emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, project, kanban
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates vibe-kanban (task management MCP server) with emacs-mcp.
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
;;   (add-to-list 'emacs-mcp-addon-always-load 'vibe-kanban)
;;
;;   ;; Or set auto-start and load manually:
;;   (setq emacs-mcp-vibe-kanban-auto-start t)
;;   (emacs-mcp-addon-load 'vibe-kanban)
;;
;; Prerequisites:
;;   npm install -g vibe-kanban
;;   OR
;;   npx vibe-kanban (will auto-install)

;;; Code:

(require 'emacs-mcp-api)

;;;; Customization:

(defgroup emacs-mcp-vibe-kanban nil
  "Vibe Kanban integration for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-vibe-kanban-")

(defcustom emacs-mcp-vibe-kanban-auto-start t
  "When non-nil, automatically start vibe-kanban server on addon load.
The server starts asynchronously and does not block Emacs startup."
  :type 'boolean
  :group 'emacs-mcp-vibe-kanban)

(defcustom emacs-mcp-vibe-kanban-command "npx"
  "Command to run vibe-kanban.
Default is `npx' which auto-installs if needed."
  :type 'string
  :group 'emacs-mcp-vibe-kanban)

(defcustom emacs-mcp-vibe-kanban-args '("vibe-kanban")
  "Arguments to pass to the vibe-kanban command."
  :type '(repeat string)
  :group 'emacs-mcp-vibe-kanban)

(defcustom emacs-mcp-vibe-kanban-port 3333
  "Port for vibe-kanban server."
  :type 'integer
  :group 'emacs-mcp-vibe-kanban)

(defcustom emacs-mcp-vibe-kanban-project-dir nil
  "Project directory for vibe-kanban.
If nil, uses current project root or `default-directory'."
  :type '(choice (const nil) directory)
  :group 'emacs-mcp-vibe-kanban)

;;;; Internal Variables:

(defvar emacs-mcp-vibe-kanban--process nil
  "Process object for the vibe-kanban server.")

(defvar emacs-mcp-vibe-kanban--buffer-name "*vibe-kanban*"
  "Buffer name for vibe-kanban output.")

;;;; Helper Functions:

(defun emacs-mcp-vibe-kanban--project-dir ()
  "Get the project directory for vibe-kanban."
  (or emacs-mcp-vibe-kanban-project-dir
      (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun emacs-mcp-vibe-kanban--running-p ()
  "Return non-nil if vibe-kanban server is running."
  (and emacs-mcp-vibe-kanban--process
       (process-live-p emacs-mcp-vibe-kanban--process)))

;;;; Server Control:

(defun emacs-mcp-vibe-kanban--start-async ()
  "Start vibe-kanban server asynchronously.
Returns the process object."
  (unless (emacs-mcp-vibe-kanban--running-p)
    (let* ((default-directory (emacs-mcp-vibe-kanban--project-dir))
           (buf (get-buffer-create emacs-mcp-vibe-kanban--buffer-name)))
      (message "emacs-mcp-vibe-kanban: Starting server...")
      (setq emacs-mcp-vibe-kanban--process
            (apply #'start-process
                   "vibe-kanban"
                   buf
                   emacs-mcp-vibe-kanban-command
                   emacs-mcp-vibe-kanban-args))
      ;; Set up process sentinel for status messages
      (set-process-sentinel
       emacs-mcp-vibe-kanban--process
       (lambda (proc event)
         (message "emacs-mcp-vibe-kanban: %s" (string-trim event))))
      emacs-mcp-vibe-kanban--process)))

(defun emacs-mcp-vibe-kanban--stop ()
  "Stop the vibe-kanban server."
  (when (emacs-mcp-vibe-kanban--running-p)
    (kill-process emacs-mcp-vibe-kanban--process)
    (setq emacs-mcp-vibe-kanban--process nil)
    (message "emacs-mcp-vibe-kanban: Server stopped")))

;;;; Interactive Commands:

;;;###autoload
(defun emacs-mcp-vibe-kanban-start ()
  "Start the vibe-kanban server."
  (interactive)
  (if (emacs-mcp-vibe-kanban--running-p)
      (message "emacs-mcp-vibe-kanban: Server already running")
    (emacs-mcp-vibe-kanban--start-async)))

;;;###autoload
(defun emacs-mcp-vibe-kanban-stop ()
  "Stop the vibe-kanban server."
  (interactive)
  (emacs-mcp-vibe-kanban--stop))

;;;###autoload
(defun emacs-mcp-vibe-kanban-restart ()
  "Restart the vibe-kanban server."
  (interactive)
  (emacs-mcp-vibe-kanban--stop)
  (sit-for 0.5)
  (emacs-mcp-vibe-kanban--start-async))

;;;###autoload
(defun emacs-mcp-vibe-kanban-status ()
  "Show vibe-kanban server status."
  (interactive)
  (message "emacs-mcp-vibe-kanban: %s"
           (if (emacs-mcp-vibe-kanban--running-p)
               "Running"
             "Stopped")))

;;;###autoload
(defun emacs-mcp-vibe-kanban-show-buffer ()
  "Show the vibe-kanban output buffer."
  (interactive)
  (if-let* ((buf (get-buffer emacs-mcp-vibe-kanban--buffer-name)))
      (display-buffer buf)
    (message "emacs-mcp-vibe-kanban: No output buffer")))

;;;; Addon Lifecycle Functions:

(defun emacs-mcp-vibe-kanban--addon-init ()
  "Synchronous init for vibe-kanban addon."
  (require 'emacs-mcp-api nil t)
  (message "emacs-mcp-vibe-kanban: initialized"))

(defun emacs-mcp-vibe-kanban--addon-async-init ()
  "Asynchronous init for vibe-kanban addon.
Starts the server in background if auto-start is enabled.
Returns the process object for lifecycle tracking."
  (when emacs-mcp-vibe-kanban-auto-start
    (emacs-mcp-vibe-kanban--start-async)))

(defun emacs-mcp-vibe-kanban--addon-shutdown ()
  "Shutdown function for vibe-kanban addon."
  (emacs-mcp-vibe-kanban--stop)
  (message "emacs-mcp-vibe-kanban: shutdown complete"))

;;;; Transient Menu:

;;;###autoload (autoload 'emacs-mcp-vibe-kanban-transient "emacs-mcp-vibe-kanban" nil t)
(transient-define-prefix emacs-mcp-vibe-kanban-transient ()
  "Vibe Kanban control menu."
  ["Vibe Kanban"
   ["Server"
    ("s" "Start" emacs-mcp-vibe-kanban-start)
    ("S" "Stop" emacs-mcp-vibe-kanban-stop)
    ("r" "Restart" emacs-mcp-vibe-kanban-restart)
    ("?" "Status" emacs-mcp-vibe-kanban-status)]
   ["View"
    ("b" "Show buffer" emacs-mcp-vibe-kanban-show-buffer)]])

;;;; Addon Registration:

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'vibe-kanban
   :version "0.1.0"
   :description "Vibe Kanban task management server"
   :requires '(emacs-mcp-api)
   :provides '(emacs-mcp-vibe-kanban-transient)
   :init #'emacs-mcp-vibe-kanban--addon-init
   :async-init #'emacs-mcp-vibe-kanban--addon-async-init
   :shutdown #'emacs-mcp-vibe-kanban--addon-shutdown))

(provide 'emacs-mcp-vibe-kanban)
;;; emacs-mcp-vibe-kanban.el ends here
