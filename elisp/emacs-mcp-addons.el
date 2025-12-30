;;; emacs-mcp-addons.el --- Addon system for emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Lazy-loaded addon system for emacs-mcp integrations.
;;
;; Built-in addons:
;;   - claude-code-mcp: Integration with claude-code.el
;;   - cider-mcp: Integration with CIDER/nREPL
;;   - org-ai-mcp: Integration with org-ai
;;
;; Usage:
;;   ;; Load specific addon on-demand
;;   (emacs-mcp-addon-load 'claude-code-mcp)
;;
;;   ;; Or enable auto-loading for detected packages
;;   (emacs-mcp-addons-auto-load)
;;
;; Writing custom addons:
;;   See `addons/emacs-mcp-addon-template.el` for a skeleton.
;;   Place your addon in the addons/ directory or add its location
;;   to `emacs-mcp-addon-directories'.

;;; Code:

(require 'cl-lib)

;;;; Customization:

(defgroup emacs-mcp-addons nil
  "Addon system for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-addon-")

(defcustom emacs-mcp-addon-directories
  (list (expand-file-name "addons" (file-name-directory
                                     (or load-file-name buffer-file-name))))
  "Directories to search for emacs-mcp addons."
  :type '(repeat directory)
  :group 'emacs-mcp-addons)

(defcustom emacs-mcp-addon-auto-load-list
  '((cider . cider)
    (claude-code . claude-code)
    (org-ai . org-ai)
    (org-kanban . org-kanban)
    (vibe-kanban . vibe-kanban)
    (swarm . vterm))
  "Alist of (ADDON . TRIGGER-FEATURE).
When TRIGGER-FEATURE is loaded, ADDON will be auto-loaded.
Addon names correspond to files: emacs-mcp-ADDON.el"
  :type '(alist :key-type symbol :value-type symbol)
  :group 'emacs-mcp-addons)

(defcustom emacs-mcp-addon-always-load nil
  "List of addons to always load when `emacs-mcp-mode' is enabled.
These load regardless of whether the trigger feature is present.
Example: \\='(cider org-kanban package-lint)"
  :type '(repeat symbol)
  :group 'emacs-mcp-addons)

;;;; Registry:

(defvar emacs-mcp-addon--registry (make-hash-table :test 'eq)
  "Hash table of registered addons.
Keys are addon symbols, values are plists with :loaded, :version, :description,
:init, :async-init, :shutdown, and runtime state.")

(defvar emacs-mcp-addon--hooks nil
  "Alist of (FEATURE . ADDON) for deferred loading.")

(defvar emacs-mcp-addon--processes (make-hash-table :test 'eq)
  "Hash table tracking async processes started by addons.
Keys are addon symbols, values are process objects.")

(defvar emacs-mcp-addon--timers (make-hash-table :test 'eq)
  "Hash table tracking timers started by addons.
Keys are addon symbols, values are lists of timer objects.")

;;;; Core Functions:

(defun emacs-mcp-addon--find-file (addon)
  "Find the file for ADDON in addon directories."
  (let ((filename (format "emacs-mcp-%s.el" addon)))
    (cl-some (lambda (dir)
               (let ((path (expand-file-name filename dir)))
                 (when (file-exists-p path) path)))
             emacs-mcp-addon-directories)))

(defun emacs-mcp-addon-available-p (addon)
  "Return non-nil if ADDON is available (file exists)."
  (emacs-mcp-addon--find-file addon))

(defun emacs-mcp-addon-loaded-p (addon)
  "Return non-nil if ADDON is already loaded."
  (plist-get (gethash addon emacs-mcp-addon--registry) :loaded))

(defun emacs-mcp-addon-running-p (addon)
  "Return non-nil if ADDON has active async processes or timers."
  (or (when-let* ((proc (gethash addon emacs-mcp-addon--processes)))
        (process-live-p proc))
      (when-let* ((timers (gethash addon emacs-mcp-addon--timers)))
        (cl-some #'timerp timers))))

;;;; Lifecycle Management:

(defun emacs-mcp-addon--run-init (addon)
  "Run the :init function for ADDON if defined."
  (when-let* ((props (gethash addon emacs-mcp-addon--registry))
              (init-fn (plist-get props :init)))
    (condition-case err
        (progn
          (funcall init-fn)
          (message "Emacs-mcp addon %s: init completed" addon))
      (error
       (message "Emacs-mcp addon %s: init failed: %s" addon (error-message-string err))))))

(defun emacs-mcp-addon--run-async-init (addon)
  "Run the :async-init function for ADDON if defined.
The async-init function should return a process object or nil."
  (when-let* ((props (gethash addon emacs-mcp-addon--registry))
              (async-init-fn (plist-get props :async-init)))
    (condition-case err
        (let ((result (funcall async-init-fn)))
          ;; Track returned process if any
          (when (processp result)
            (puthash addon result emacs-mcp-addon--processes))
          (message "Emacs-mcp addon %s: async-init started" addon))
      (error
       (message "Emacs-mcp addon %s: async-init failed: %s" addon (error-message-string err))))))

(defun emacs-mcp-addon--run-shutdown (addon)
  "Run the :shutdown function for ADDON if defined."
  (when-let* ((props (gethash addon emacs-mcp-addon--registry))
              (shutdown-fn (plist-get props :shutdown)))
    (condition-case err
        (progn
          (funcall shutdown-fn)
          (message "Emacs-mcp addon %s: shutdown completed" addon))
      (error
       (message "Emacs-mcp addon %s: shutdown failed: %s" addon (error-message-string err))))))

(defun emacs-mcp-addon--cleanup (addon)
  "Clean up processes and timers for ADDON."
  ;; Kill process if running
  (when-let* ((proc (gethash addon emacs-mcp-addon--processes)))
    (when (process-live-p proc)
      (kill-process proc))
    (remhash addon emacs-mcp-addon--processes))
  ;; Cancel timers
  (when-let* ((timers (gethash addon emacs-mcp-addon--timers)))
    (dolist (timer timers)
      (when (timerp timer)
        (cancel-timer timer)))
    (remhash addon emacs-mcp-addon--timers)))

(defun emacs-mcp-addon-register-process (addon process)
  "Register PROCESS as belonging to ADDON for lifecycle management."
  (puthash addon process emacs-mcp-addon--processes))

(defun emacs-mcp-addon-register-timer (addon timer)
  "Register TIMER as belonging to ADDON for lifecycle management."
  (let ((existing (gethash addon emacs-mcp-addon--timers)))
    (puthash addon (cons timer existing) emacs-mcp-addon--timers)))

(defun emacs-mcp-addon-unregister-timer (addon timer)
  "Unregister TIMER from ADDON."
  (when-let* ((timers (gethash addon emacs-mcp-addon--timers)))
    (puthash addon (delq timer timers) emacs-mcp-addon--timers)))

;;;; Load/Unload:

;;;###autoload
(defun emacs-mcp-addon-load (addon)
  "Load ADDON if available and not already loaded.
After loading, runs :init (sync) then :async-init (non-blocking).
Returns t if loaded successfully, nil otherwise."
  (interactive
   (list (intern (completing-read "Load addon: "
                                  (emacs-mcp-addon-list-available)
                                  nil t))))
  (cond
   ((emacs-mcp-addon-loaded-p addon)
    (message "Addon %s already loaded" addon)
    t)
   ((emacs-mcp-addon--find-file addon)
    (let ((file (emacs-mcp-addon--find-file addon)))
      (load file nil t)
      (puthash addon (plist-put (gethash addon emacs-mcp-addon--registry)
                                :loaded t)
               emacs-mcp-addon--registry)
      ;; Run lifecycle hooks
      (emacs-mcp-addon--run-init addon)
      (emacs-mcp-addon--run-async-init addon)
      (message "Loaded emacs-mcp addon: %s" addon)
      t))
   (t
    (message "Addon %s not found" addon)
    nil)))

;;;###autoload
(defun emacs-mcp-addon-unload (addon)
  "Unload ADDON, running shutdown hooks and cleaning up resources."
  (interactive
   (list (intern (completing-read "Unload addon: "
                                  (emacs-mcp-addon-list-loaded)
                                  nil t))))
  (when (emacs-mcp-addon-loaded-p addon)
    ;; Run shutdown hook
    (emacs-mcp-addon--run-shutdown addon)
    ;; Clean up processes and timers
    (emacs-mcp-addon--cleanup addon)
    ;; Mark as unloaded
    (puthash addon (plist-put (gethash addon emacs-mcp-addon--registry)
                              :loaded nil)
             emacs-mcp-addon--registry)
    (message "Unloaded emacs-mcp addon: %s" addon)
    t))

;;;###autoload
(defun emacs-mcp-addon-restart (addon)
  "Restart ADDON by unloading and reloading it."
  (interactive
   (list (intern (completing-read "Restart addon: "
                                  (emacs-mcp-addon-list-loaded)
                                  nil t))))
  (emacs-mcp-addon-unload addon)
  (emacs-mcp-addon-load addon))

(defun emacs-mcp-addon-list-available ()
  "Return list of available addon symbols."
  (let (addons)
    (dolist (dir emacs-mcp-addon-directories)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir nil "^emacs-mcp-.*\\.el$"))
          (when (string-match "^emacs-mcp-\\(.+\\)\\.el$" file)
            (push (intern (match-string 1 file)) addons)))))
    (delete-dups addons)))

(defun emacs-mcp-addon-list-loaded ()
  "Return list of loaded addon symbols."
  (let (loaded)
    (maphash (lambda (addon props)
               (when (plist-get props :loaded)
                 (push addon loaded)))
             emacs-mcp-addon--registry)
    loaded))

;;;; Auto-loading:

(defun emacs-mcp-addon--after-load-hook (feature)
  "Hook function to auto-load addons when FEATURE is loaded."
  (dolist (entry emacs-mcp-addon-auto-load-list)
    (when (eq (cdr entry) feature)
      (emacs-mcp-addon-load (car entry)))))

;;;###autoload
(defun emacs-mcp-addons-auto-load ()
  "Enable auto-loading of addons when their trigger packages load.
Uses `emacs-mcp-addon-auto-load-list' to determine mappings.
This function is called during initialization; for manual setup,
users can also configure auto-loading in their init file."
  (interactive)
  (dolist (entry emacs-mcp-addon-auto-load-list)
    (let ((addon (car entry))
          (trigger (cdr entry)))
      ;; If trigger already loaded, load addon now
      (if (featurep trigger)
          (emacs-mcp-addon-load addon)
        ;; Set up deferred loading via eval-after-load
        ;; Note: eval-after-load is appropriate here as this runs during init
        (eval-after-load trigger
          `(emacs-mcp-addon-load ',addon))))))

;;;###autoload
(defun emacs-mcp-addons-load-always ()
  "Load all addons in `emacs-mcp-addon-always-load'.
Called during `emacs-mcp-initialize' to load user's preferred addons."
  (interactive)
  (dolist (addon emacs-mcp-addon-always-load)
    (emacs-mcp-addon-load addon)))

;;;###autoload
(defun emacs-mcp-addons-setup ()
  "Set up addon system: load always-load addons and enable auto-loading.
This is the main entry point, called from `emacs-mcp-initialize'."
  (interactive)
  ;; First load always-load addons
  (emacs-mcp-addons-load-always)
  ;; Then set up deferred auto-loading for feature-triggered addons
  (emacs-mcp-addons-auto-load))

;;;; Registration (for addon authors):

(defun emacs-mcp-addon-register (addon &rest props)
  "Register ADDON with PROPS.
PROPS is a plist that may contain:

Metadata:
  :version     - Version string
  :description - One-line description
  :requires    - List of required features
  :provides    - List of provided features/commands

Lifecycle hooks:
  :init        - Function to run synchronously after loading.
                 Use for lightweight setup that must complete before use.

  :async-init  - Function to run asynchronously after loading.
                 Should return a process object if starting a subprocess.
                 Use for long-running startup (e.g., starting nREPL, npx).
                 Non-blocking - does not delay Emacs startup.

  :shutdown    - Function to run when unloading the addon.
                 Use for cleanup (stopping servers, saving state).

Example:
  (emacs-mcp-addon-register
   \\='my-addon
   :version \"1.0.0\"
   :description \"My cool addon\"
   :init #\\='my-addon-setup-keybindings
   :async-init #\\='my-addon-start-server
   :shutdown #\\='my-addon-stop-server)"
  (puthash addon props emacs-mcp-addon--registry))

;;;; Info command:

;;;###autoload
(defun emacs-mcp-addon-info ()
  "Display information about available and loaded addons."
  (interactive)
  (let ((buf (get-buffer-create "*emacs-mcp addons*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== emacs-mcp Addons ===\n\n")
      (insert "Available addons:\n")
      (dolist (addon (emacs-mcp-addon-list-available))
        (let* ((loaded (emacs-mcp-addon-loaded-p addon))
               (running (emacs-mcp-addon-running-p addon))
               (props (gethash addon emacs-mcp-addon--registry))
               (has-init (plist-get props :init))
               (has-async (plist-get props :async-init))
               (has-shutdown (plist-get props :shutdown)))
          (insert (format "  %s%s %s\n"
                          (if loaded "[loaded]" "[      ]")
                          (if running "*" " ")
                          addon))
          (when-let* ((desc (plist-get props :description)))
            (insert (format "           %s\n" desc)))
          (when (or has-init has-async has-shutdown)
            (insert (format "           lifecycle: %s%s%s\n"
                            (if has-init "init " "")
                            (if has-async "async-init " "")
                            (if has-shutdown "shutdown" ""))))))
      (insert "\n")
      (insert "Legend: [loaded] = loaded, * = has running process/timer\n\n")
      (insert "Addon directories:\n")
      (dolist (dir emacs-mcp-addon-directories)
        (insert (format "  %s\n" dir)))
      (goto-char (point-min)))
    (display-buffer buf)))

(provide 'emacs-mcp-addons)
;;; emacs-mcp-addons.el ends here
