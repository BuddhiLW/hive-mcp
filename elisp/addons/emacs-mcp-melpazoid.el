;;; emacs-mcp-melpazoid.el --- Melpazoid integration for emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, lisp, maint
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates melpazoid (https://github.com/riscy/melpazoid)
;; with emacs-mcp for comprehensive MELPA submission testing.
;;
;; Melpazoid runs in a Docker container and performs:
;; - package-lint checks
;; - byte-compilation with multiple Emacs versions
;; - checkdoc validation
;; - melpazoid-specific style checks
;; - license verification
;; - load testing
;;
;; Features:
;; - Run melpazoid from within Emacs
;; - Parse and display structured results
;; - Save results to MCP memory
;; - Track submission readiness
;;
;; Requirements:
;; - Docker must be installed and running
;; - melpazoid repository cloned locally
;;
;; Usage:
;;   (setq emacs-mcp-melpazoid-path "/path/to/melpazoid")
;;   (emacs-mcp-addon-load 'melpazoid)
;;   M-x emacs-mcp-melpazoid-run
;;
;; Or use the transient menu:
;;   M-x emacs-mcp-melpazoid-transient

;;; Code:

(require 'emacs-mcp-api)

;; Forward declarations
(declare-function emacs-mcp-addon-register "emacs-mcp-addons")
(declare-function transient-define-prefix "transient")

;;;; Customization:

(defgroup emacs-mcp-melpazoid nil
  "Melpazoid integration for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-melpazoid-")

(defcustom emacs-mcp-melpazoid-path nil
  "Path to the melpazoid repository.
If nil, will attempt to find it in common locations."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Path"))
  :group 'emacs-mcp-melpazoid)

(defcustom emacs-mcp-melpazoid-save-results t
  "When non-nil, save melpazoid results to MCP memory."
  :type 'boolean
  :group 'emacs-mcp-melpazoid)

(defcustom emacs-mcp-melpazoid-auto-detect-recipe t
  "When non-nil, auto-detect recipe from recipes/ directory."
  :type 'boolean
  :group 'emacs-mcp-melpazoid)

(defcustom emacs-mcp-melpazoid-timeout 300
  "Timeout in seconds for melpazoid runs."
  :type 'integer
  :group 'emacs-mcp-melpazoid)

;;;; Internal Variables:

;; Forward declarations for byte-compiler (defined at runtime by transient)
(defvar emacs-mcp-melpazoid--transient-menu)
(declare-function emacs-mcp-melpazoid--transient-menu "emacs-mcp-melpazoid")

(defvar emacs-mcp-melpazoid--process nil
  "Current melpazoid process.")

(defvar emacs-mcp-melpazoid--last-results nil
  "Last melpazoid run results.")

(defvar emacs-mcp-melpazoid--buffer-name "*MCP Melpazoid*"
  "Buffer name for melpazoid output.")

(defvar emacs-mcp-melpazoid--common-paths
  '("~/dotfiles/gitthings/melpazoid"
    "~/melpazoid"
    "~/projects/melpazoid"
    "~/.local/share/melpazoid")
  "Common paths to search for melpazoid.")

;;;; Utility Functions:

(defun emacs-mcp-melpazoid--find-path ()
  "Find melpazoid installation path."
  (or emacs-mcp-melpazoid-path
      (seq-find (lambda (p)
                  (let ((expanded (expand-file-name p)))
                    (and (file-directory-p expanded)
                         (file-exists-p (expand-file-name "Makefile" expanded)))))
                emacs-mcp-melpazoid--common-paths)
      (error "Melpazoid not found. Set `emacs-mcp-melpazoid-path' or clone from https://github.com/riscy/melpazoid")))

(defun emacs-mcp-melpazoid--docker-available-p ()
  "Check if Docker is available and running."
  (zerop (call-process "docker" nil nil nil "info")))

(defun emacs-mcp-melpazoid--find-recipe (project-dir)
  "Find MELPA recipe file in PROJECT-DIR."
  (let ((recipes-dir (expand-file-name "recipes" project-dir)))
    (when (file-directory-p recipes-dir)
      (car (directory-files recipes-dir t "^[^.]")))))

(defun emacs-mcp-melpazoid--read-recipe (recipe-file)
  "Read recipe contents from RECIPE-FILE."
  (when (and recipe-file (file-exists-p recipe-file))
    (with-temp-buffer
      (insert-file-contents recipe-file)
      (string-trim (buffer-string)))))

(defun emacs-mcp-melpazoid--parse-output (output)
  "Parse melpazoid OUTPUT into structured results."
  (let ((results (list :errors '()
                       :warnings '()
                       :info '()
                       :sections '())))
    ;; Parse errors
    (let ((pos 0))
      (while (string-match "\\[31m.*?error.*?\\[0m\\|Error:" output pos)
        (push (match-string 0 output) (plist-get results :errors))
        (setq pos (match-end 0))))
    ;; Parse warnings
    (let ((pos 0))
      (while (string-match "\\[33m.*?warning.*?\\[0m\\|Warning:" output pos)
        (push (match-string 0 output) (plist-get results :warnings))
        (setq pos (match-end 0))))
    ;; Parse sections
    (let ((pos 0))
      (while (string-match "^⸺ `\\([^`]+\\)`" output pos)
        (push (match-string 1 output) (plist-get results :sections))
        (setq pos (match-end 0))))
    ;; Count issues
    (plist-put results :error-count (length (plist-get results :errors)))
    (plist-put results :warning-count (length (plist-get results :warnings)))
    (plist-put results :success (zerop (plist-get results :error-count)))
    results))

(defun emacs-mcp-melpazoid--format-summary (results)
  "Format RESULTS into a human-readable summary."
  (format "Melpazoid Results:
  Errors:   %d
  Warnings: %d
  Status:   %s
  Sections: %s"
          (plist-get results :error-count)
          (plist-get results :warning-count)
          (if (plist-get results :success) "PASS" "FAIL")
          (mapconcat #'identity (plist-get results :sections) ", ")))

;;;; Core Functions:

;;;###autoload
(defun emacs-mcp-melpazoid-run (project-dir &optional recipe)
  "Run melpazoid on PROJECT-DIR with optional RECIPE.
If RECIPE is nil, attempts to auto-detect from recipes/ directory."
  (interactive
   (list (read-directory-name "Project directory: "
                              (or (locate-dominating-file default-directory ".git")
                                  default-directory))
         nil))
  (unless (emacs-mcp-melpazoid--docker-available-p)
    (error "Docker is not available. Please start Docker first"))
  (let* ((melpazoid-path (emacs-mcp-melpazoid--find-path))
         (project-dir (expand-file-name project-dir))
         (recipe-file (when emacs-mcp-melpazoid-auto-detect-recipe
                        (emacs-mcp-melpazoid--find-recipe project-dir)))
         (recipe-str (or recipe
                         (emacs-mcp-melpazoid--read-recipe recipe-file)
                         (read-string "MELPA recipe (or empty to skip): ")))
         (buf (get-buffer-create emacs-mcp-melpazoid--buffer-name))
         (cmd (if (string-empty-p recipe-str)
                  (format "cd %s && LOCAL_REPO=%s make"
                          (shell-quote-argument melpazoid-path)
                          (shell-quote-argument project-dir))
                (format "cd %s && RECIPE='%s' LOCAL_REPO=%s make"
                        (shell-quote-argument melpazoid-path)
                        recipe-str
                        (shell-quote-argument project-dir)))))
    ;; Prepare buffer
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=" (make-string 60 ?=) "\n")
        (insert " Melpazoid Check\n")
        (insert (format " Project: %s\n" project-dir))
        (when recipe-str
          (insert (format " Recipe: %s\n" recipe-str)))
        (insert (format " Started: %s\n" (format-time-string "%F %T")))
        (insert "=" (make-string 60 ?=) "\n\n")
        (insert "Running melpazoid (this may take a few minutes)...\n\n")))
    (display-buffer buf)
    ;; Kill any existing process
    (when (and emacs-mcp-melpazoid--process
               (process-live-p emacs-mcp-melpazoid--process))
      (kill-process emacs-mcp-melpazoid--process))
    ;; Start new process
    (setq emacs-mcp-melpazoid--process
          (start-process-shell-command
           "melpazoid"
           buf
           cmd))
    (set-process-sentinel
     emacs-mcp-melpazoid--process
     (lambda (proc event)
       (when (string-match-p "\\(finished\\|exited\\)" event)
         (with-current-buffer (process-buffer proc)
           (let* ((output (buffer-string))
                  (results (emacs-mcp-melpazoid--parse-output output)))
             (setq emacs-mcp-melpazoid--last-results results)
             (goto-char (point-max))
             (insert "\n" (make-string 60 ?=) "\n")
             (insert (emacs-mcp-melpazoid--format-summary results))
             (insert "\n" (make-string 60 ?=) "\n")
             ;; Save to memory if enabled
             (when emacs-mcp-melpazoid-save-results
               (emacs-mcp-api-memory-add
                "note"
                (format "Melpazoid run on %s\n%s\n\nFull output:\n%s"
                        project-dir
                        (emacs-mcp-melpazoid--format-summary results)
                        output)
                '("melpazoid" "melpa" "lint")))
             (message "Melpazoid %s: %d errors, %d warnings"
                      (if (plist-get results :success) "PASSED" "FAILED")
                      (plist-get results :error-count)
                      (plist-get results :warning-count)))))))
    (message "Melpazoid started... check %s buffer" emacs-mcp-melpazoid--buffer-name)))

;;;###autoload
(defun emacs-mcp-melpazoid-run-current-project ()
  "Run melpazoid on the current project."
  (interactive)
  (let ((root (or (locate-dominating-file default-directory ".git")
                  (locate-dominating-file default-directory "elisp")
                  default-directory)))
    (emacs-mcp-melpazoid-run root)))

;;;###autoload
(defun emacs-mcp-melpazoid-stop ()
  "Stop the current melpazoid process."
  (interactive)
  (when (and emacs-mcp-melpazoid--process
             (process-live-p emacs-mcp-melpazoid--process))
    (kill-process emacs-mcp-melpazoid--process)
    (message "Melpazoid process stopped")))

;;;###autoload
(defun emacs-mcp-melpazoid-show-results ()
  "Display the melpazoid results buffer."
  (interactive)
  (if-let* ((buf (get-buffer emacs-mcp-melpazoid--buffer-name)))
      (display-buffer buf)
    (message "No melpazoid results available. Run M-x emacs-mcp-melpazoid-run first")))

;;;###autoload
(defun emacs-mcp-melpazoid-save-results ()
  "Save last melpazoid results to MCP memory."
  (interactive)
  (if emacs-mcp-melpazoid--last-results
      (let ((content (with-current-buffer
                         (get-buffer emacs-mcp-melpazoid--buffer-name)
                       (buffer-string))))
        (emacs-mcp-api-memory-add "note" content '("melpazoid" "melpa"))
        (message "Saved melpazoid results to memory"))
    (message "No melpazoid results to save")))

;;;###autoload
(defun emacs-mcp-melpazoid-status ()
  "Return the current melpazoid status."
  (interactive)
  (let ((status (list :running (and emacs-mcp-melpazoid--process
                                    (process-live-p emacs-mcp-melpazoid--process))
                      :has-results (and emacs-mcp-melpazoid--last-results t)
                      :last-success (when emacs-mcp-melpazoid--last-results
                                      (plist-get emacs-mcp-melpazoid--last-results :success))
                      :last-errors (when emacs-mcp-melpazoid--last-results
                                     (plist-get emacs-mcp-melpazoid--last-results :error-count))
                      :last-warnings (when emacs-mcp-melpazoid--last-results
                                       (plist-get emacs-mcp-melpazoid--last-results :warning-count)))))
    (when (called-interactively-p 'any)
      (message "Melpazoid: %s, Last run: %s (%d errors, %d warnings)"
               (if (plist-get status :running) "running" "idle")
               (if (plist-get status :has-results)
                   (if (plist-get status :last-success) "PASS" "FAIL")
                 "no results")
               (or (plist-get status :last-errors) 0)
               (or (plist-get status :last-warnings) 0)))
    status))

;;;; Transient Menu:

;;;###autoload
(defun emacs-mcp-melpazoid-transient ()
  "Melpazoid MELPA testing menu."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (unless (fboundp 'emacs-mcp-melpazoid--transient-menu)
          (transient-define-prefix emacs-mcp-melpazoid--transient-menu ()
            "Melpazoid MELPA testing menu."
            ["Melpazoid - MELPA Submission Testing"
             ["Run"
              ("r" "Run on project" emacs-mcp-melpazoid-run-current-project)
              ("R" "Run on directory" emacs-mcp-melpazoid-run)
              ("s" "Stop" emacs-mcp-melpazoid-stop)]
             ["Results"
              ("v" "View results" emacs-mcp-melpazoid-show-results)
              ("S" "Save to memory" emacs-mcp-melpazoid-save-results)
              ("?" "Status" emacs-mcp-melpazoid-status)]]))
        (emacs-mcp-melpazoid--transient-menu))
    (message "Transient package not available. Use M-x emacs-mcp-melpazoid-run-current-project")))

;;;; Minor Mode:

;;;###autoload
(define-minor-mode emacs-mcp-melpazoid-mode
  "Minor mode for melpazoid integration.

Provides Docker-based MELPA submission testing with:
- Full melpazoid test suite
- Multiple Emacs version testing
- Comprehensive lint checks
- Results tracking in MCP memory"
  :init-value nil
  :lighter " MCP-Mz"
  :global t
  :group 'emacs-mcp-melpazoid
  (if emacs-mcp-melpazoid-mode
      (progn
        (require 'emacs-mcp-api nil t)
        (message "Emacs-mcp-melpazoid enabled"))
    (emacs-mcp-melpazoid-stop)
    (message "Emacs-mcp-melpazoid disabled")))

;;;; Addon Registration:

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'melpazoid
   :version "0.1.0"
   :description "Melpazoid Docker-based MELPA testing"
   :requires '(emacs-mcp-api)
   :provides '(emacs-mcp-melpazoid-mode emacs-mcp-melpazoid-transient)))

(provide 'emacs-mcp-melpazoid)
;;; emacs-mcp-melpazoid.el ends here
