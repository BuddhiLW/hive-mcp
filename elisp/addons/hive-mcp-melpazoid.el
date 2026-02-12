;;; hive-mcp-melpazoid.el --- Melpazoid integration for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, lisp, maint
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates melpazoid (https://github.com/riscy/melpazoid)
;; with hive-mcp for comprehensive MELPA submission testing.
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
;;   (setq hive-mcp-melpazoid-path "/path/to/melpazoid")
;;   (hive-mcp-addon-load 'melpazoid)
;;   M-x hive-mcp-melpazoid-run
;;
;; Or use the transient menu:
;;   M-x hive-mcp-melpazoid-transient

;;; Code:

(require 'hive-mcp-api)

;; Forward declarations
(declare-function hive-mcp-addon-register "hive-mcp-addons")
(declare-function transient-define-prefix "transient")

;;;; Customization:

(defgroup hive-mcp-melpazoid nil
  "Melpazoid integration for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-melpazoid-")

(defcustom hive-mcp-melpazoid-path nil
  "Path to the melpazoid repository.
If nil, will attempt to find it in common locations."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Path"))
  :group 'hive-mcp-melpazoid)

(defcustom hive-mcp-melpazoid-save-results t
  "When non-nil, save melpazoid results to MCP memory."
  :type 'boolean
  :group 'hive-mcp-melpazoid)

(defcustom hive-mcp-melpazoid-auto-detect-recipe t
  "When non-nil, auto-detect recipe from recipes/ directory."
  :type 'boolean
  :group 'hive-mcp-melpazoid)

(defcustom hive-mcp-melpazoid-timeout 300
  "Timeout in seconds for melpazoid runs."
  :type 'integer
  :group 'hive-mcp-melpazoid)

(defcustom hive-mcp-melpazoid-fast-mode nil
  "When non-nil, run melpazoid without full Docker rebuild.
Fast mode options:
- `local': Run melpazoid.py directly with local Python/Emacs (fastest)
- `cached': Use Docker but skip image rebuild if it exists
- nil: Always rebuild Docker image (slowest, most thorough)"
  :type '(choice (const :tag "Full Docker rebuild" nil)
                 (const :tag "Local Python (no Docker)" local)
                 (const :tag "Cached Docker image" cached))
  :group 'hive-mcp-melpazoid)

;;;; Internal Variables:

;; Forward declarations for byte-compiler (defined at runtime by transient)
(defvar hive-mcp-melpazoid--transient-menu)
(declare-function hive-mcp-melpazoid--transient-menu "hive-mcp-melpazoid")

(defvar hive-mcp-melpazoid--process nil
  "Current melpazoid process.")

(defvar hive-mcp-melpazoid--last-results nil
  "Last melpazoid run results.")

(defvar hive-mcp-melpazoid--buffer-name "*MCP Melpazoid*"
  "Buffer name for melpazoid output.")

(defvar hive-mcp-melpazoid--common-paths
  '("~/dotfiles/gitthings/melpazoid"
    "~/melpazoid"
    "~/projects/melpazoid"
    "~/.local/share/melpazoid")
  "Common paths to search for melpazoid.")

;;;; Utility Functions:

(defun hive-mcp-melpazoid--find-path ()
  "Find melpazoid installation path."
  (or hive-mcp-melpazoid-path
      (seq-find (lambda (p)
                  (let ((expanded (expand-file-name p)))
                    (and (file-directory-p expanded)
                         (file-exists-p (expand-file-name "Makefile" expanded)))))
                hive-mcp-melpazoid--common-paths)
      (error "Melpazoid not found. Set `hive-mcp-melpazoid-path' or clone from https://github.com/riscy/melpazoid")))

(defun hive-mcp-melpazoid--docker-available-p ()
  "Check if Docker is available and running."
  (zerop (call-process "docker" nil nil nil "info")))

(defun hive-mcp-melpazoid--docker-image-exists-p ()
  "Check if melpazoid Docker image already exists."
  (let ((output (shell-command-to-string "docker images -q melpazoid:latest 2>/dev/null")))
    (not (string-empty-p (string-trim output)))))

(defun hive-mcp-melpazoid--build-command (melpazoid-path project-dir recipe-str)
  "Build the melpazoid command based on `hive-mcp-melpazoid-fast-mode'.
MELPAZOID-PATH is the path to melpazoid repo.
PROJECT-DIR is the project to check.
RECIPE-STR is the MELPA recipe string."
  (let ((base-env (if (string-empty-p recipe-str)
                      (format "LOCAL_REPO=%s" (shell-quote-argument project-dir))
                    (format "RECIPE='%s' LOCAL_REPO=%s"
                            recipe-str (shell-quote-argument project-dir)))))
    (pcase hive-mcp-melpazoid-fast-mode
      ('local
       ;; Run melpazoid.py directly - fastest, no Docker
       (format "cd %s && %s python3 melpazoid/melpazoid.py"
               (shell-quote-argument melpazoid-path) base-env))
      ('cached
       ;; Use Docker but skip rebuild if image exists
       (if (hive-mcp-melpazoid--docker-image-exists-p)
           (format "cd %s && %s docker run --rm -v %s:/pkg melpazoid:latest python3 melpazoid/melpazoid.py"
                   (shell-quote-argument melpazoid-path)
                   base-env
                   (shell-quote-argument project-dir))
         ;; Fall back to full build if no image
         (format "cd %s && %s make" (shell-quote-argument melpazoid-path) base-env)))
      (_
       ;; Default: full Docker rebuild
       (format "cd %s && %s make"
               (shell-quote-argument melpazoid-path) base-env)))))

(defun hive-mcp-melpazoid--find-recipe (project-dir)
  "Find MELPA recipe file in PROJECT-DIR."
  (let ((recipes-dir (expand-file-name "recipes" project-dir)))
    (when (file-directory-p recipes-dir)
      (car (directory-files recipes-dir t "^[^.]")))))

(defun hive-mcp-melpazoid--read-recipe (recipe-file)
  "Read recipe contents from RECIPE-FILE."
  (when (and recipe-file (file-exists-p recipe-file))
    (with-temp-buffer
      (insert-file-contents recipe-file)
      (string-trim (buffer-string)))))

(defun hive-mcp-melpazoid--parse-output (output)
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

(defun hive-mcp-melpazoid--format-summary (results)
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
(defun hive-mcp-melpazoid-run (project-dir &optional recipe)
  "Run melpazoid on PROJECT-DIR with optional RECIPE.
If RECIPE is nil, attempts to auto-detect from recipes/ directory."
  (interactive
   (list (read-directory-name "Project directory: "
                              (or (locate-dominating-file default-directory ".git")
                                  default-directory))
         nil))
  ;; Docker only required for non-local modes
  (unless (or (eq hive-mcp-melpazoid-fast-mode 'local)
              (hive-mcp-melpazoid--docker-available-p))
    (error "Docker is not available. Please start Docker first"))
  (let* ((melpazoid-path (hive-mcp-melpazoid--find-path))
         (project-dir (expand-file-name project-dir))
         (recipe-file (when hive-mcp-melpazoid-auto-detect-recipe
                        (hive-mcp-melpazoid--find-recipe project-dir)))
         (recipe-str (or recipe
                         (hive-mcp-melpazoid--read-recipe recipe-file)
                         (read-string "MELPA recipe (or empty to skip): ")))
         (buf (get-buffer-create hive-mcp-melpazoid--buffer-name))
         (cmd (hive-mcp-melpazoid--build-command melpazoid-path project-dir recipe-str))
         (mode-label (pcase hive-mcp-melpazoid-fast-mode
                       ('local "LOCAL (no Docker)")
                       ('cached "CACHED Docker")
                       (_ "FULL Docker rebuild"))))
    ;; Prepare buffer
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=" (make-string 60 ?=) "\n")
        (insert " Melpazoid Check\n")
        (insert (format " Mode: %s\n" mode-label))
        (insert (format " Project: %s\n" project-dir))
        (when (and recipe-str (not (string-empty-p recipe-str)))
          (insert (format " Recipe: %s\n" recipe-str)))
        (insert (format " Started: %s\n" (format-time-string "%F %T")))
        (insert "=" (make-string 60 ?=) "\n\n")
        (insert (if (eq hive-mcp-melpazoid-fast-mode 'local)
                    "Running melpazoid locally...\n\n"
                  "Running melpazoid (this may take a few minutes)...\n\n"))))
    (display-buffer buf)
    ;; Kill any existing process
    (when (and hive-mcp-melpazoid--process
               (process-live-p hive-mcp-melpazoid--process))
      (kill-process hive-mcp-melpazoid--process))
    ;; Start new process
    (setq hive-mcp-melpazoid--process
          (start-process-shell-command
           "melpazoid"
           buf
           cmd))
    (set-process-sentinel
     hive-mcp-melpazoid--process
     (lambda (proc event)
       (when (string-match-p "\\(finished\\|exited\\)" event)
         (with-current-buffer (process-buffer proc)
           (let* ((output (buffer-string))
                  (results (hive-mcp-melpazoid--parse-output output)))
             (setq hive-mcp-melpazoid--last-results results)
             (goto-char (point-max))
             (insert "\n" (make-string 60 ?=) "\n")
             (insert (hive-mcp-melpazoid--format-summary results))
             (insert "\n" (make-string 60 ?=) "\n")
             ;; Save to memory if enabled
             (when hive-mcp-melpazoid-save-results
               (hive-mcp-api-memory-add
                "note"
                (format "Melpazoid run on %s\n%s\n\nFull output:\n%s"
                        project-dir
                        (hive-mcp-melpazoid--format-summary results)
                        output)
                '("melpazoid" "melpa" "lint")))
             (message "Melpazoid %s: %d errors, %d warnings"
                      (if (plist-get results :success) "PASSED" "FAILED")
                      (plist-get results :error-count)
                      (plist-get results :warning-count)))))))
    (message "Melpazoid started... check %s buffer" hive-mcp-melpazoid--buffer-name)))

;;;###autoload
(defun hive-mcp-melpazoid-run-current-project ()
  "Run melpazoid on the current project."
  (interactive)
  (let ((root (or (locate-dominating-file default-directory ".git")
                  (locate-dominating-file default-directory "elisp")
                  default-directory)))
    (hive-mcp-melpazoid-run root)))

;;;###autoload
(defun hive-mcp-melpazoid-stop ()
  "Stop the current melpazoid process."
  (interactive)
  (when (and hive-mcp-melpazoid--process
             (process-live-p hive-mcp-melpazoid--process))
    (kill-process hive-mcp-melpazoid--process)
    (message "Melpazoid process stopped")))

;;;###autoload
(defun hive-mcp-melpazoid-show-results ()
  "Display the melpazoid results buffer."
  (interactive)
  (if-let* ((buf (get-buffer hive-mcp-melpazoid--buffer-name)))
      (display-buffer buf)
    (message "No melpazoid results available. Run M-x hive-mcp-melpazoid-run first")))

;;;###autoload
(defun hive-mcp-melpazoid-save-results ()
  "Save last melpazoid results to MCP memory."
  (interactive)
  (if hive-mcp-melpazoid--last-results
      (let ((content (with-current-buffer
                         (get-buffer hive-mcp-melpazoid--buffer-name)
                       (buffer-string))))
        (hive-mcp-api-memory-add "note" content '("melpazoid" "melpa"))
        (message "Saved melpazoid results to memory"))
    (message "No melpazoid results to save")))

;;;###autoload
(defun hive-mcp-melpazoid-status ()
  "Return the current melpazoid status."
  (interactive)
  (let ((status (list :running (and hive-mcp-melpazoid--process
                                    (process-live-p hive-mcp-melpazoid--process))
                      :has-results (and hive-mcp-melpazoid--last-results t)
                      :last-success (when hive-mcp-melpazoid--last-results
                                      (plist-get hive-mcp-melpazoid--last-results :success))
                      :last-errors (when hive-mcp-melpazoid--last-results
                                     (plist-get hive-mcp-melpazoid--last-results :error-count))
                      :last-warnings (when hive-mcp-melpazoid--last-results
                                       (plist-get hive-mcp-melpazoid--last-results :warning-count)))))
    (when (called-interactively-p 'any)
      (message "Melpazoid: %s, Last run: %s (%d errors, %d warnings)"
               (if (plist-get status :running) "running" "idle")
               (if (plist-get status :has-results)
                   (if (plist-get status :last-success) "PASS" "FAIL")
                 "no results")
               (or (plist-get status :last-errors) 0)
               (or (plist-get status :last-warnings) 0)))
    status))

;;;###autoload
(defun hive-mcp-melpazoid-cycle-fast-mode ()
  "Cycle through fast mode options: nil -> local -> cached -> nil."
  (interactive)
  (setq hive-mcp-melpazoid-fast-mode
        (pcase hive-mcp-melpazoid-fast-mode
          ('nil 'local)
          ('local 'cached)
          ('cached nil)))
  (message "Melpazoid fast-mode: %s"
           (pcase hive-mcp-melpazoid-fast-mode
             ('local "LOCAL (no Docker - fastest)")
             ('cached "CACHED (skip Docker rebuild)")
             (_ "FULL (Docker rebuild - thorough)"))))

;;;###autoload
(defun hive-mcp-melpazoid-run-fast ()
  "Run melpazoid in local mode (fastest, no Docker)."
  (interactive)
  (let ((hive-mcp-melpazoid-fast-mode 'local))
    (hive-mcp-melpazoid-run-current-project)))

;;;; Transient Menu:

;;;###autoload
(defun hive-mcp-melpazoid-transient ()
  "Melpazoid MELPA testing menu."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (unless (fboundp 'hive-mcp-melpazoid--transient-menu)
          (transient-define-prefix hive-mcp-melpazoid--transient-menu ()
            "Melpazoid MELPA testing menu."
            ["Melpazoid - MELPA Submission Testing"
             ["Run"
              ("r" "Run on project" hive-mcp-melpazoid-run-current-project)
              ("f" "Run FAST (local)" hive-mcp-melpazoid-run-fast)
              ("R" "Run on directory" hive-mcp-melpazoid-run)
              ("s" "Stop" hive-mcp-melpazoid-stop)]
             ["Results"
              ("v" "View results" hive-mcp-melpazoid-show-results)
              ("S" "Save to memory" hive-mcp-melpazoid-save-results)
              ("?" "Status" hive-mcp-melpazoid-status)]
             ["Settings"
              ("m" "Cycle fast-mode" hive-mcp-melpazoid-cycle-fast-mode)]]))
        (hive-mcp-melpazoid--transient-menu))
    (message "Transient package not available. Use M-x hive-mcp-melpazoid-run-current-project")))

;;;; Minor Mode:

;;;###autoload
(define-minor-mode hive-mcp-melpazoid-mode
  "Minor mode for melpazoid integration.

Provides Docker-based MELPA submission testing with:
- Full melpazoid test suite
- Multiple Emacs version testing
- Comprehensive lint checks
- Results tracking in MCP memory"
  :init-value nil
  :lighter " MCP-Mz"
  :global t
  :group 'hive-mcp-melpazoid
  (if hive-mcp-melpazoid-mode
      (progn
        (require 'hive-mcp-api nil t)
        (message "Emacs-mcp-melpazoid enabled"))
    (hive-mcp-melpazoid-stop)
    (message "Emacs-mcp-melpazoid disabled")))

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'melpazoid
   :version "0.1.0"
   :description "Melpazoid Docker-based MELPA testing"
   :requires '(hive-mcp-api)
   :provides '(hive-mcp-melpazoid-mode hive-mcp-melpazoid-transient)))

(provide 'hive-mcp-melpazoid)
;;; hive-mcp-melpazoid.el ends here
