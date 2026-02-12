;;; hive-mcp-package-lint.el --- MELPA submission tools for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, lisp, maint
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon provides MELPA submission workflow automation for hive-mcp.
;;
;; Features:
;; - Run package-lint on elisp files
;; - Byte-compile with warnings
;; - Checkdoc validation
;; - Full MELPA compliance check workflow
;; - Save lint results to MCP memory
;;
;; Usage:
;;   (hive-mcp-addon-load 'package-lint)
;;   M-x hive-mcp-package-lint-check-file
;;   M-x hive-mcp-package-lint-check-all
;;
;; Or use the transient menu:
;;   M-x hive-mcp-package-lint-transient

;;; Code:

(require 'hive-mcp-api)
(require 'transient)

;; Soft dependencies
(declare-function package-lint-buffer "package-lint")
(declare-function package-lint-current-buffer "package-lint")
(declare-function checkdoc-current-buffer "checkdoc")
(declare-function transient-define-prefix "transient")
(declare-function hive-mcp-addon-register "hive-mcp-addons")

;;;; Customization:

(defgroup hive-mcp-package-lint nil
  "MELPA submission tools for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-package-lint-")

(defcustom hive-mcp-package-lint-save-results t
  "When non-nil, save lint results to MCP memory."
  :type 'boolean
  :group 'hive-mcp-package-lint)

(defcustom hive-mcp-package-lint-elisp-patterns '("*.el")
  "Glob patterns for finding elisp files."
  :type '(repeat string)
  :group 'hive-mcp-package-lint)

(defcustom hive-mcp-package-lint-exclude-patterns '("*-autoloads.el" "*-pkg.el" ".dir-locals.el")
  "Patterns to exclude from linting."
  :type '(repeat string)
  :group 'hive-mcp-package-lint)

;;;; Internal Variables:

(defvar hive-mcp-package-lint--last-results nil
  "Last lint results for reference.")

(defvar hive-mcp-package-lint--buffer-name "*MCP Package Lint*"
  "Buffer name for lint output.")

;;;; Utility Functions:

(defun hive-mcp-package-lint--ensure-package-lint ()
  "Ensure package-lint is available."
  (unless (require 'package-lint nil t)
    (error "Package-lint not installed.  Install with: M-x package-install RET package-lint RET")))

(defun hive-mcp-package-lint--find-elisp-files (directory)
  "Find all elisp files in DIRECTORY, excluding unwanted patterns."
  (let ((files '()))
    (dolist (pattern hive-mcp-package-lint-elisp-patterns)
      (setq files (append files (file-expand-wildcards
                                 (expand-file-name pattern directory) t))))
    ;; Recursively search subdirectories
    (dolist (subdir (directory-files directory t "^[^.]"))
      (when (and (file-directory-p subdir)
                 (not (member (file-name-nondirectory subdir) '("." ".."))))
        (setq files (append files (hive-mcp-package-lint--find-elisp-files subdir)))))
    ;; Filter out excluded patterns
    (seq-remove
     (lambda (f)
       (seq-some (lambda (pat)
                   (string-match-p (wildcard-to-regexp pat) (file-name-nondirectory f)))
                 hive-mcp-package-lint-exclude-patterns))
     files)))

(defun hive-mcp-package-lint--format-results (results file)
  "Format RESULTS from linting FILE into readable text."
  (if (null results)
      (format "  %s: OK (no issues)\n" (file-name-nondirectory file))
    (concat
     (format "  %s: %d issue(s)\n" (file-name-nondirectory file) (length results))
     (mapconcat
      (lambda (r)
        (format "    Line %d: [%s] %s"
                (nth 0 r)
                (symbol-name (nth 2 r))
                (nth 3 r)))
      results
      "\n")
     "\n")))

;;;; Package-Lint Functions:

;;;###autoload
(defun hive-mcp-package-lint-check-file (file)
  "Run package-lint on FILE and display results."
  (interactive "fEllisp file to lint: ")
  (hive-mcp-package-lint--ensure-package-lint)
  (let* ((results (with-temp-buffer
                    (insert-file-contents file)
                    (emacs-lisp-mode)
                    (package-lint-buffer)))
         (output (hive-mcp-package-lint--format-results results file)))
    (setq hive-mcp-package-lint--last-results
          (list :file file :results results :type 'package-lint))
    (message "%s" output)
    results))

;;;###autoload
(defun hive-mcp-package-lint-check-buffer ()
  "Run package-lint on current buffer."
  (interactive)
  (hive-mcp-package-lint--ensure-package-lint)
  (let ((results (package-lint-current-buffer)))
    (setq hive-mcp-package-lint--last-results
          (list :file (buffer-file-name)
                :results results
                :type 'package-lint))
    (if results
        (message "Package-lint: %d issue(s) found" (length results))
      (message "Package-lint: OK"))
    results))

;;;; Byte-Compile Functions:

;;;###autoload
(defun hive-mcp-package-lint-byte-compile-file (file)
  "Byte-compile FILE and capture warnings."
  (interactive "fEllisp file to compile: ")
  (let* ((byte-compile-warnings t)
         (byte-compile-error-on-warn nil)
         (warning-buffer (get-buffer-create "*Compile-Log*"))
         (start-pos nil)
         (warnings '()))
    ;; Clear and prepare warning buffer
    (with-current-buffer warning-buffer
      (goto-char (point-max))
      (setq start-pos (point)))
    ;; Compile
    (byte-compile-file file)
    ;; Collect warnings
    (with-current-buffer warning-buffer
      (when (> (point-max) start-pos)
        (setq warnings (buffer-substring-no-properties start-pos (point-max)))))
    (setq hive-mcp-package-lint--last-results
          (list :file file :results warnings :type 'byte-compile))
    (if (string-empty-p (string-trim warnings))
        (message "Byte-compile: OK")
      (message "Byte-compile warnings:\n%s" warnings))
    warnings))

;;;###autoload
(defun hive-mcp-package-lint-byte-compile-buffer ()
  "Byte-compile current buffer."
  (interactive)
  (if buffer-file-name
      (hive-mcp-package-lint-byte-compile-file buffer-file-name)
    (error "Buffer is not visiting a file")))

;;;; Checkdoc Functions:

;;;###autoload
(defun hive-mcp-package-lint-checkdoc-file (file)
  "Run checkdoc on FILE and return issues."
  (interactive "fEllisp file to check: ")
  (let ((issues '()))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (setq buffer-file-name file)
      (condition-case err
          (checkdoc-current-buffer t)
        (error
         (push (format "checkdoc error: %s" (error-message-string err)) issues))))
    (let ((checkdoc-buf (get-buffer "*Checkdoc Status*")))
      (when checkdoc-buf
        (with-current-buffer checkdoc-buf
          (setq issues (buffer-substring-no-properties (point-min) (point-max))))))
    (setq hive-mcp-package-lint--last-results
          (list :file file :results issues :type 'checkdoc))
    (if (or (null issues) (string-empty-p (string-trim issues)))
        (message "Checkdoc: OK")
      (message "Checkdoc issues:\n%s" issues))
    issues))

;;;###autoload
(defun hive-mcp-package-lint-checkdoc-buffer ()
  "Run checkdoc on current buffer."
  (interactive)
  (if buffer-file-name
      (hive-mcp-package-lint-checkdoc-file buffer-file-name)
    (checkdoc-current-buffer t)))

;;;; Full MELPA Compliance Check:

;;;###autoload
(defun hive-mcp-package-lint-check-all (directory)
  "Run full MELPA compliance check on all elisp files in DIRECTORY.
Runs package-lint, byte-compile, and checkdoc on each file."
  (interactive "DProject directory: ")
  (hive-mcp-package-lint--ensure-package-lint)
  (let* ((files (hive-mcp-package-lint--find-elisp-files directory))
         (buf (get-buffer-create hive-mcp-package-lint--buffer-name))
         (total-issues 0)
         (all-results '()))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=" (make-string 60 ?=) "\n")
      (insert " MELPA Compliance Check\n")
      (insert " Directory: " directory "\n")
      (insert " Files: " (number-to-string (length files)) "\n")
      (insert "=" (make-string 60 ?=) "\n\n")

      ;; Package-lint
      (insert "## Package-Lint Results\n")
      (insert "-" (make-string 40 ?-) "\n")
      (dolist (file files)
        (let ((results (with-temp-buffer
                         (insert-file-contents file)
                         (emacs-lisp-mode)
                         (condition-case nil
                             (package-lint-buffer)
                           (error nil)))))
          (push (list :file file :type 'package-lint :results results) all-results)
          (setq total-issues (+ total-issues (length results)))
          (insert (hive-mcp-package-lint--format-results results file))))
      (insert "\n")

      ;; Byte-compile
      (insert "## Byte-Compile Results\n")
      (insert "-" (make-string 40 ?-) "\n")
      (dolist (file files)
        (let* ((byte-compile-warnings t)
               (byte-compile-error-on-warn nil)
               (result (condition-case err
                           (progn (byte-compile-file file) "OK")
                         (error (format "Error: %s" (error-message-string err))))))
          (push (list :file file :type 'byte-compile :results result) all-results)
          (insert (format "  %s: %s\n" (file-name-nondirectory file) result))))
      (insert "\n")

      ;; Checkdoc
      (insert "## Checkdoc Results\n")
      (insert "-" (make-string 40 ?-) "\n")
      (dolist (file files)
        (let ((issues nil))
          (with-temp-buffer
            (insert-file-contents file)
            (emacs-lisp-mode)
            (setq buffer-file-name file)
            (condition-case nil
                (checkdoc-current-buffer t)
              (error nil)))
          (let ((checkdoc-buf (get-buffer "*Checkdoc Status*")))
            (when checkdoc-buf
              (with-current-buffer checkdoc-buf
                (unless (string-empty-p (string-trim (buffer-string)))
                  (setq issues (buffer-string)))
                (erase-buffer))))
          (push (list :file file :type 'checkdoc :results issues) all-results)
          (insert (format "  %s: %s\n"
                          (file-name-nondirectory file)
                          (if issues "issues found" "OK")))))
      (insert "\n")

      ;; Summary
      (insert "=" (make-string 60 ?=) "\n")
      (insert (format " Summary: %d package-lint issues across %d files\n"
                      total-issues (length files)))
      (insert "=" (make-string 60 ?=) "\n")
      (goto-char (point-min)))

    ;; Store results
    (setq hive-mcp-package-lint--last-results all-results)

    ;; Save to memory if enabled
    (when hive-mcp-package-lint-save-results
      (hive-mcp-api-memory-add
       "note"
       (with-current-buffer buf (buffer-string))
       '("melpa" "lint" "compliance")))

    (display-buffer buf)
    all-results))

;;;###autoload
(defun hive-mcp-package-lint-check-current-project ()
  "Run full MELPA compliance check on current project."
  (interactive)
  (let ((root (or (locate-dominating-file default-directory ".git")
                  (locate-dominating-file default-directory "elisp")
                  default-directory)))
    (hive-mcp-package-lint-check-all root)))

;;;; Save Results to Memory:

;;;###autoload
(defun hive-mcp-package-lint-save-results ()
  "Save last lint results to MCP memory."
  (interactive)
  (if hive-mcp-package-lint--last-results
      (let* ((type (plist-get hive-mcp-package-lint--last-results :type))
             (file (plist-get hive-mcp-package-lint--last-results :file))
             (results (plist-get hive-mcp-package-lint--last-results :results))
             (content (format "Type: %s\nFile: %s\nResults:\n%s"
                              type file
                              (if (listp results)
                                  (mapconcat
                                   (lambda (r)
                                     (format "  Line %d: %s" (nth 0 r) (nth 3 r)))
                                   results "\n")
                                results))))
        (hive-mcp-api-memory-add "note" content '("lint" "melpa"))
        (message "Saved lint results to memory"))
    (message "No lint results to save")))

;;;; MCP Tool API Functions:

;;;###autoload
(defun hive-mcp-package-lint-run (file-or-dir)
  "Run package-lint on FILE-OR-DIR. Returns structured results."
  (hive-mcp-package-lint--ensure-package-lint)
  (if (file-directory-p file-or-dir)
      (hive-mcp-package-lint-check-all file-or-dir)
    (hive-mcp-package-lint-check-file file-or-dir)))

;;;###autoload
(defun hive-mcp-package-lint-status ()
  "Return last lint status as JSON-compatible plist."
  (list :has-results (and hive-mcp-package-lint--last-results t)
        :last-type (when hive-mcp-package-lint--last-results
                     (plist-get hive-mcp-package-lint--last-results :type))
        :last-file (when hive-mcp-package-lint--last-results
                     (plist-get hive-mcp-package-lint--last-results :file))))

;;;; Transient Menu:

;;;###autoload (autoload 'hive-mcp-package-lint-transient "hive-mcp-package-lint" nil t)
(transient-define-prefix hive-mcp-package-lint-transient ()
  "MELPA submission tools menu."
  ["MELPA Compliance Tools"
   ["Single File"
    ("l" "Package-lint buffer" hive-mcp-package-lint-check-buffer)
    ("b" "Byte-compile buffer" hive-mcp-package-lint-byte-compile-buffer)
    ("d" "Checkdoc buffer" hive-mcp-package-lint-checkdoc-buffer)]
   ["Project"
    ("a" "Check all (project)" hive-mcp-package-lint-check-current-project)
    ("A" "Check all (directory)" hive-mcp-package-lint-check-all)]
   ["Memory"
    ("s" "Save results to memory" hive-mcp-package-lint-save-results)]])

;;;; Minor Mode:

;;;###autoload
(define-minor-mode hive-mcp-package-lint-mode
  "Minor mode for MELPA submission workflow tools.

Provides:
- Package-lint integration
- Byte-compile checking
- Checkdoc validation
- Full compliance workflow"
  :init-value nil
  :lighter " MCP-Lint"
  :global t
  :group 'hive-mcp-package-lint
  (if hive-mcp-package-lint-mode
      (progn
        (require 'hive-mcp-api nil t)
        (message "Emacs-mcp-package-lint enabled"))
    (message "Emacs-mcp-package-lint disabled")))

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'package-lint
   :version "0.1.0"
   :description "MELPA submission tools (package-lint, byte-compile, checkdoc)"
   :requires '(package-lint hive-mcp-api)
   :provides '(hive-mcp-package-lint-mode hive-mcp-package-lint-transient)))

(provide 'hive-mcp-package-lint)
;;; hive-mcp-package-lint.el ends here
