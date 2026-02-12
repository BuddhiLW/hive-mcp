;;; hive-mcp-hooks.el --- Hook event processor for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "1.0"))
;; Keywords: tools, ai, mcp, hooks
;; SPDX-License-Identifier: MIT

;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Elisp hook event processor for hive-mcp.
;;
;; This module handles hook events pushed through the bidirectional channel,
;; allowing Clojure-side events to trigger Emacs-side actions.
;;
;; Supported hook types:
;;   - hook/commit: Git commit hooks (pre-commit, post-commit)
;;   - hook/wrap:   Session wrap workflow triggers
;;   - hook/notify: User notifications via message/notification system
;;
;; Design principles (SOLID/CLARITY):
;;   - Single Responsibility: Only processes hook events
;;   - Open/Closed: New hook types via new handler functions
;;   - Dependency Inversion: Depends on channel API, not internals
;;   - Yield safe failure: Errors logged, never crash
;;
;; Integration:
;;   - Registers with `hive-mcp-channel-on' for event dispatch
;;   - Can piggyback on swarm-events for task-related hooks
;;
;; Usage:
;;   (require 'hive-mcp-hooks)
;;   (hive-mcp-hooks-init)  ; Auto-called when addon loads

;;; Code:

(require 'cl-lib)

;; Soft dependencies
(declare-function hive-mcp-channel-on "hive-mcp-channel")
(declare-function hive-mcp-channel-off "hive-mcp-channel")
(declare-function hive-mcp-channel-connected-p "hive-mcp-channel")
(declare-function hive-mcp-with-fallback "hive-mcp-graceful")
(declare-function magit-status "magit" (&optional directory cache))

;;;; Customization

(defgroup hive-mcp-hooks nil
  "Hook event processing for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-hooks-")

(defcustom hive-mcp-hooks-notify-function #'message
  "Function to call for notifications.
Should accept a format string and arguments like `message'.
Can be set to `notifications-notify' for desktop notifications."
  :type 'function
  :group 'hive-mcp-hooks)

(defcustom hive-mcp-hooks-commit-auto-stage t
  "Whether to auto-stage all changes on commit hook.
When non-nil, hook/commit events with action \\='stage\\=' stage all files."
  :type 'boolean
  :group 'hive-mcp-hooks)

(defcustom hive-mcp-hooks-wrap-auto-save t
  "Whether to auto-save buffers on wrap hook.
When non-nil, hook/wrap events save all modified buffers."
  :type 'boolean
  :group 'hive-mcp-hooks)

(defcustom hive-mcp-hooks-log-events nil
  "Whether to log all hook events to *Messages*.
Useful for debugging hook integrations."
  :type 'boolean
  :group 'hive-mcp-hooks)

;;;; Internal State

(defvar hive-mcp-hooks--initialized nil
  "Non-nil when hooks module has been initialized.")

(defvar hive-mcp-hooks--handler-alist nil
  "Alist of (HOOK-TYPE . HANDLER-FUNCTION) for custom handlers.
Users can add custom handlers via `hive-mcp-hooks-register'.")

;;;; Logging

(defun hive-mcp-hooks--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS if logging is enabled."
  (when hive-mcp-hooks-log-events
    (apply #'message (concat "[hive-mcp-hooks] " format-string) args)))

;;;; Core Event Processing

(defun hive-mcp-hooks-process (event)
  "Process a hook EVENT from the channel.
EVENT is an alist with at least a \"type\" key.
Dispatches to appropriate handler based on event type."
  (let* ((type-str (or (cdr (assoc "type" event))
                       (cdr (assoc 'type event))))
         (data (or (cdr (assoc "data" event))
                   (cdr (assoc 'data event))
                   event)))
    (hive-mcp-hooks--log "Processing event: %s" type-str)
    (condition-case err
        (pcase type-str
          ("hook/commit" (hive-mcp-hooks--handle-commit data))
          ("hook/wrap" (hive-mcp-hooks--handle-wrap data))
          ("hook/notify" (hive-mcp-hooks--handle-notify data))
          ;; Check for custom handlers
          (_
           (when-let* ((handler (cdr (assoc type-str hive-mcp-hooks--handler-alist))))
             (funcall handler data))))
      (error
       (message "[hive-mcp-hooks] Error processing %s: %s"
                type-str (error-message-string err))))))

;;;; Hook Handlers

(defun hive-mcp-hooks--handle-commit (data)
  "Handle a hook/commit event with DATA.
DATA may contain:
  - action: \\='stage\\=', \\='commit\\=', \\='amend\\='
  - message: commit message (for commit/amend)
  - files: list of files to stage (optional)
  - directory: working directory"
  (let* ((action (or (cdr (assoc "action" data))
                     (cdr (assoc 'action data))
                     "status"))
         (commit-msg (or (cdr (assoc "message" data))
                         (cdr (assoc 'message data))))
         (files (or (cdr (assoc "files" data))
                    (cdr (assoc 'files data))))
         (directory (or (cdr (assoc "directory" data))
                        (cdr (assoc 'directory data))
                        default-directory)))
    (hive-mcp-hooks--log "Commit hook: action=%s dir=%s" action directory)
    (let ((default-directory directory))
      (pcase action
        ("stage"
         (if files
             ;; Stage specific files
             (dolist (file files)
               (hive-mcp-hooks--git-stage file))
           ;; Stage all if enabled
           (when hive-mcp-hooks-commit-auto-stage
             (hive-mcp-hooks--git-stage-all))))
        ("commit"
         (when commit-msg
           (hive-mcp-hooks--git-commit commit-msg)))
        ("amend"
         (hive-mcp-hooks--git-amend commit-msg))
        ("status"
         (hive-mcp-hooks--show-git-status directory))
        (_
         (hive-mcp-hooks--log "Unknown commit action: %s" action))))))

(defun hive-mcp-hooks--handle-wrap (data)
  "Handle a hook/wrap event with DATA.
DATA may contain:
  - action: \\='gather\\=', \\='crystallize\\=', \\='complete\\='
  - session-id: current session identifier
  - save-buffers: whether to save modified buffers"
  (let* ((action (or (cdr (assoc "action" data))
                     (cdr (assoc 'action data))
                     "gather"))
         (session-id (or (cdr (assoc "session-id" data))
                         (cdr (assoc 'session-id data))))
         (save-buffers (or (cdr (assoc "save-buffers" data))
                           (cdr (assoc 'save-buffers data))
                           hive-mcp-hooks-wrap-auto-save)))
    (hive-mcp-hooks--log "Wrap hook: action=%s session=%s" action session-id)
    (pcase action
      ("gather"
       ;; Prepare for wrap - save buffers, collect state
       (when save-buffers
         (save-some-buffers t))
       (hive-mcp-hooks--notify "Wrap: Gathering session data..."))
      ("crystallize"
       ;; Crystallization phase
       (hive-mcp-hooks--notify "Wrap: Crystallizing session..."))
      ("complete"
       ;; Wrap completed
       (when save-buffers
         (save-some-buffers t))
       (hive-mcp-hooks--notify "Wrap: Session complete for %s"
                               (or session-id "current session")))
      (_
       (hive-mcp-hooks--log "Unknown wrap action: %s" action)))))

(defun hive-mcp-hooks--handle-notify (data)
  "Handle a hook/notify event with DATA.
DATA may contain:
  - message: notification message (required)
  - level: \\='info\\=', \\='warning\\=', \\='error\\=' (default: info)
  - title: notification title (optional)"
  (let* ((msg (or (cdr (assoc "message" data))
                  (cdr (assoc 'message data))
                  "Notification"))
         (level (or (cdr (assoc "level" data))
                    (cdr (assoc 'level data))
                    "info"))
         (title (or (cdr (assoc "title" data))
                    (cdr (assoc 'title data)))))
    (hive-mcp-hooks--log "Notify: [%s] %s" level msg)
    (pcase level
      ("error"
       (hive-mcp-hooks--notify-error msg title))
      ("warning"
       (hive-mcp-hooks--notify-warning msg title))
      (_
       (hive-mcp-hooks--notify msg)))))

;;;; Git Operations

(defun hive-mcp-hooks--git-stage (file)
  "Stage FILE for commit."
  (let ((output (shell-command-to-string (format "git add %s 2>&1" (shell-quote-argument file)))))
    (unless (string-empty-p output)
      (hive-mcp-hooks--log "git add %s: %s" file output))))

(defun hive-mcp-hooks--git-stage-all ()
  "Stage all modified files."
  (let ((output (shell-command-to-string "git add -A 2>&1")))
    (hive-mcp-hooks--log "git add -A: %s" (if (string-empty-p output) "done" output))))

(defun hive-mcp-hooks--git-commit (message)
  "Create a commit with MESSAGE."
  (let ((output (shell-command-to-string
                 (format "git commit -m %s 2>&1" (shell-quote-argument message)))))
    (hive-mcp-hooks--log "git commit: %s" output)
    (hive-mcp-hooks--notify "Committed: %s" (truncate-string-to-width message 50))))

(defun hive-mcp-hooks--git-amend (&optional message)
  "Amend the last commit, optionally with new MESSAGE."
  (let* ((cmd (if message
                  (format "git commit --amend -m %s 2>&1" (shell-quote-argument message))
                "git commit --amend --no-edit 2>&1"))
         (output (shell-command-to-string cmd)))
    (hive-mcp-hooks--log "git commit --amend: %s" output)))

(defun hive-mcp-hooks--show-git-status (directory)
  "Show git status for DIRECTORY.
Uses Magit if available, otherwise falls back to shell."
  (if (fboundp 'magit-status)
      (magit-status directory)
    (let ((default-directory directory))
      (shell-command "git status"))))

;;;; Notification Helpers

(defun hive-mcp-hooks--notify (format-string &rest args)
  "Send a notification with FORMAT-STRING and ARGS."
  (apply hive-mcp-hooks-notify-function format-string args))

(defun hive-mcp-hooks--notify-warning (message &optional title)
  "Send a warning notification with MESSAGE and optional TITLE."
  (display-warning 'hive-mcp-hooks
                   (if title (format "%s: %s" title message) message)
                   :warning))

(defun hive-mcp-hooks--notify-error (message &optional title)
  "Send an error notification with MESSAGE and optional TITLE."
  (display-warning 'hive-mcp-hooks
                   (if title (format "%s: %s" title message) message)
                   :error))

;;;; Channel Integration

(defun hive-mcp-hooks--channel-handler (event)
  "Channel message handler that dispatches hook events.
EVENT is the decoded channel message."
  (let ((type-str (or (cdr (assoc "type" event))
                      (cdr (assoc 'type event)))))
    (when (and type-str (string-prefix-p "hook/" type-str))
      (hive-mcp-hooks-process event))))

(defun hive-mcp-hooks--register-with-channel ()
  "Register hook handlers with the bidirectional channel."
  (when (require 'hive-mcp-channel nil t)
    ;; Register for each hook type
    (dolist (type '(:hook/commit :hook/wrap :hook/notify))
      (hive-mcp-channel-on type #'hive-mcp-hooks-process))
    (hive-mcp-hooks--log "Registered with channel")))

(defun hive-mcp-hooks--unregister-from-channel ()
  "Unregister hook handlers from the channel."
  (when (require 'hive-mcp-channel nil t)
    (dolist (type '(:hook/commit :hook/wrap :hook/notify))
      (hive-mcp-channel-off type #'hive-mcp-hooks-process))
    (hive-mcp-hooks--log "Unregistered from channel")))

;;;; Public API

;;;###autoload
(defun hive-mcp-hooks-register (hook-type handler)
  "Register HANDLER for custom HOOK-TYPE.
HOOK-TYPE should be a string like \"hook/my-custom\".
HANDLER receives the event data as an alist."
  (setf (alist-get hook-type hive-mcp-hooks--handler-alist nil nil #'equal)
        handler))

;;;###autoload
(defun hive-mcp-hooks-unregister (hook-type)
  "Unregister handler for HOOK-TYPE."
  (setf (alist-get hook-type hive-mcp-hooks--handler-alist nil 'remove #'equal)
        nil))

;;;###autoload
(defun hive-mcp-hooks-init ()
  "Initialize the hooks module.
Registers handlers with the channel if connected."
  (interactive)
  (unless hive-mcp-hooks--initialized
    (hive-mcp-hooks--register-with-channel)
    (setq hive-mcp-hooks--initialized t)
    (hive-mcp-hooks--log "Initialized")))

;;;###autoload
(defun hive-mcp-hooks-shutdown ()
  "Shutdown the hooks module."
  (interactive)
  (when hive-mcp-hooks--initialized
    (hive-mcp-hooks--unregister-from-channel)
    (setq hive-mcp-hooks--initialized nil)
    (hive-mcp-hooks--log "Shutdown")))

;;;; Swarm Events Integration

(defun hive-mcp-hooks-emit-hook-event (hook-type data)
  "Emit a hook event of HOOK-TYPE with DATA through swarm-events.
This allows Emacs-side code to trigger hooks that can be processed
by both Emacs and Clojure sides."
  (when (require 'hive-mcp-swarm-events nil t)
    (declare-function hive-mcp-swarm-events-emit "hive-mcp-swarm-events")
    (hive-mcp-swarm-events-emit hook-type data)))

;;;###autoload
(defun hive-mcp-hooks-trigger-wrap (action &optional session-id)
  "Trigger a wrap hook with ACTION and optional SESSION-ID.
ACTION should be \\='gather\\=', \\='crystallize\\=', or \\='complete\\='."
  (hive-mcp-hooks-emit-hook-event
   "hook/wrap"
   `(("action" . ,action)
     ("session-id" . ,session-id)
     ("timestamp" . ,(truncate (float-time))))))

;;;###autoload
(defun hive-mcp-hooks-trigger-notify (message &optional level title)
  "Trigger a notification hook with MESSAGE, LEVEL, and TITLE."
  (hive-mcp-hooks-emit-hook-event
   "hook/notify"
   `(("message" . ,message)
     ("level" . ,(or level "info"))
     ("title" . ,title))))

;;;; Addon Registration

(defun hive-mcp-hooks--addon-init ()
  "Addon init function - called when addon loads."
  (hive-mcp-hooks-init))

(defun hive-mcp-hooks--addon-shutdown ()
  "Addon shutdown function - called when addon unloads."
  (hive-mcp-hooks-shutdown))

;; Register with addon system
(with-eval-after-load 'hive-mcp-addons
  (declare-function hive-mcp-addon-register "hive-mcp-addons")
  (hive-mcp-addon-register
   'hooks
   :version "0.1.0"
   :description "Hook event processor for channel-based triggers"
   :requires '(hive-mcp-channel)
   :provides '(hive-mcp-hooks-init hive-mcp-hooks-process)
   :init #'hive-mcp-hooks--addon-init
   :shutdown #'hive-mcp-hooks--addon-shutdown))

(provide 'hive-mcp-hooks)
;;; hive-mcp-hooks.el ends here
