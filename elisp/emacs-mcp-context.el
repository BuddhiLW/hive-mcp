;;; emacs-mcp-context.el --- Context gathering for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of emacs-mcp.

;;; Commentary:
;;
;; Context gathering functions for emacs-mcp.  Provides rich information
;; about current buffer, region, defun, project, and git status.
;;
;; All functions return JSON-serializable plists.
;;

;;; Code:

(require 'project)
(require 'subr-x)

;;; Customization

(defgroup emacs-mcp-context nil
  "Context gathering settings for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-context-")

;;; Extension Points

(defvar emacs-mcp-context-providers nil
  "Alist of (NAME . FUNCTION) for additional context providers.
Each function should return a plist or nil.")

(defvar emacs-mcp-context-gather-hook nil
  "Hook run when gathering context.  Can modify context plist.
Called with single argument: the context plist being built.")

(defun emacs-mcp-context-register-provider (name fn)
  "Register context provider NAME calling FN."
  (setf (alist-get name emacs-mcp-context-providers) fn))

;;; Buffer Context

(defun emacs-mcp-context-buffer ()
  "Return current buffer context as plist."
  (list
   :name (buffer-name)
   :file (buffer-file-name)
   :mode (symbol-name major-mode)
   :modified (if (buffer-modified-p) t :false)
   :readonly (if buffer-read-only t :false)
   :point (point)
   :line (line-number-at-pos)
   :column (current-column)
   :size (buffer-size)
   :encoding (symbol-name buffer-file-coding-system)))

;;; Region Context

(defun emacs-mcp-context-region ()
  "Return region context if active, nil otherwise."
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (list
       :active t
       :start start
       :end end
       :start-line (line-number-at-pos start)
       :end-line (line-number-at-pos end)
       :length (- end start)
       :text (buffer-substring-no-properties start end)))))

;;; Defun Context

(defun emacs-mcp-context-defun ()
  "Return current defun context if in a function."
  (save-excursion
    (condition-case nil
        (when (beginning-of-defun)
          (let ((start (point))
                (name (emacs-mcp-context--defun-name)))
            (end-of-defun)
            (list
             :name name
             :start-line (line-number-at-pos start)
             :end-line (line-number-at-pos)
             :start start
             :end (point)
             :text (buffer-substring-no-properties start (point)))))
      (error nil))))

(defun emacs-mcp-context--defun-name ()
  "Extract name of defun at point."
  (save-excursion
    (beginning-of-defun)
    (cond
     ;; Clojure/Lisp style
     ((looking-at "(def\\(?:un\\|n\\|method\\|macro\\|test\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)")
      (match-string-no-properties 1))
     ;; Python style
     ((looking-at "\\(?:async\\s-+\\)?def\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)")
      (match-string-no-properties 1))
     ;; JavaScript/TypeScript
     ((looking-at "\\(?:async\\s-+\\)?function\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)")
      (match-string-no-properties 1))
     ;; Rust
     ((looking-at "\\(?:pub\\s-+\\)?fn\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)")
      (match-string-no-properties 1))
     ;; Go
     ((looking-at "func\\s-+\\(?:([^)]+)\\s-+\\)?\\([a-zA-Z_][a-zA-Z0-9_]*\\)")
      (match-string-no-properties 1))
     (t nil))))

;;; Project Context

(defun emacs-mcp-context-project-root ()
  "Return project root directory or nil."
  (when-let* ((proj (project-current)))
    (project-root proj)))

(defun emacs-mcp-context-project ()
  "Return project context as plist."
  (when-let* ((root (emacs-mcp-context-project-root)))
    (list
     :root root
     :name (file-name-nondirectory (directory-file-name root))
     :type (emacs-mcp-context--detect-project-type root))))

(defun emacs-mcp-context--detect-project-type (root)
  "Detect project type based on marker files in ROOT."
  (cond
   ((file-exists-p (expand-file-name "deps.edn" root)) "clojure-deps")
   ((file-exists-p (expand-file-name "project.clj" root)) "clojure-lein")
   ((file-exists-p (expand-file-name "bb.edn" root)) "babashka")
   ((file-exists-p (expand-file-name "shadow-cljs.edn" root)) "clojurescript")
   ((file-exists-p (expand-file-name "package.json" root)) "node")
   ((file-exists-p (expand-file-name "Cargo.toml" root)) "rust")
   ((file-exists-p (expand-file-name "pyproject.toml" root)) "python")
   ((file-exists-p (expand-file-name "setup.py" root)) "python")
   ((file-exists-p (expand-file-name "go.mod" root)) "go")
   ((file-exists-p (expand-file-name "Makefile" root)) "make")
   ((file-exists-p (expand-file-name ".git" root)) "git")
   (t "unknown")))

;;; Git Context

(defun emacs-mcp-context-git ()
  "Return git context as plist, nil if not in git repo."
  (when-let* ((root (emacs-mcp-context-project-root)))
    (let ((default-directory root))
      (when (file-directory-p (expand-file-name ".git" root))
        (list
         :branch (emacs-mcp-context--git-current-branch)
         :dirty (if (emacs-mcp-context--git-dirty-p) t :false)
         :staged (emacs-mcp-context--git-staged-files)
         :modified (emacs-mcp-context--git-modified-files)
         :recent-commits (emacs-mcp-context--git-recent-commits 5))))))

(defun emacs-mcp-context--git-current-branch ()
  "Return current git branch name."
  (let ((result (string-trim
                 (shell-command-to-string
                  "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))
    (unless (string-empty-p result) result)))

(defun emacs-mcp-context--git-dirty-p ()
  "Return t if working tree has uncommitted changes."
  (not (string-empty-p
        (shell-command-to-string "git status --porcelain 2>/dev/null"))))

(defun emacs-mcp-context--git-staged-files ()
  "Return list of staged file paths."
  (let ((output (shell-command-to-string
                 "git diff --cached --name-only 2>/dev/null")))
    (unless (string-empty-p output)
      (split-string (string-trim output) "\n" t))))

(defun emacs-mcp-context--git-modified-files ()
  "Return list of modified (unstaged) file paths."
  (let ((output (shell-command-to-string
                 "git diff --name-only 2>/dev/null")))
    (unless (string-empty-p output)
      (split-string (string-trim output) "\n" t))))

(defun emacs-mcp-context--git-recent-commits (n)
  "Return last N commit summaries."
  (let ((output (shell-command-to-string
                 (format "git log --oneline -n %d 2>/dev/null" n))))
    (unless (string-empty-p output)
      (split-string (string-trim output) "\n" t))))

;;; Language Detection

(defun emacs-mcp-context-language ()
  "Detect programming language from major mode."
  (pcase major-mode
    ('clojure-mode "clojure")
    ('clojurescript-mode "clojurescript")
    ('clojurec-mode "clojure")
    ('emacs-lisp-mode "elisp")
    ('lisp-mode "common-lisp")
    ('scheme-mode "scheme")
    ('python-mode "python")
    ('python-ts-mode "python")
    ('js-mode "javascript")
    ('js2-mode "javascript")
    ('typescript-mode "typescript")
    ('typescript-ts-mode "typescript")
    ('rust-mode "rust")
    ('rust-ts-mode "rust")
    ('go-mode "go")
    ('go-ts-mode "go")
    ('c-mode "c")
    ('c++-mode "c++")
    ('java-mode "java")
    ('ruby-mode "ruby")
    ('sh-mode "shell")
    ('sql-mode "sql")
    ('html-mode "html")
    ('css-mode "css")
    ('json-mode "json")
    ('yaml-mode "yaml")
    ('markdown-mode "markdown")
    ('org-mode "org")
    (_ (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))))

;;; Full Context

(defun emacs-mcp-context-full ()
  "Return complete current context for Claude.
Includes buffer, region, defun, project, git, and custom providers."
  (let ((context
         (list
          :timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")
          :buffer (emacs-mcp-context-buffer)
          :region (emacs-mcp-context-region)
          :defun (emacs-mcp-context-defun)
          :project (emacs-mcp-context-project)
          :git (emacs-mcp-context-git)
          :language (emacs-mcp-context-language))))
    ;; Add custom providers
    (dolist (provider emacs-mcp-context-providers)
      (when-let* ((data (funcall (cdr provider))))
        (setq context (plist-put context (car provider) data))))
    ;; Run hooks
    (run-hook-with-args 'emacs-mcp-context-gather-hook context)
    context))

;;; Convenience Functions

(defun emacs-mcp-context-current-line ()
  "Return current line content."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun emacs-mcp-context-surrounding-lines (&optional before after)
  "Return lines surrounding point.
BEFORE and AFTER default to 5 lines each."
  (let ((before-n (or before 5))
        (after-n (or after 5)))
    (save-excursion
      (let ((current-line (line-number-at-pos))
            (start (progn
                     (forward-line (- before-n))
                     (line-beginning-position)))
            (end (progn
                   (goto-char (point))
                   (forward-line (+ before-n after-n 1))
                   (line-end-position))))
        (list
         :current-line current-line
         :start-line (line-number-at-pos start)
         :end-line (line-number-at-pos end)
         :text (buffer-substring-no-properties start end))))))

(provide 'emacs-mcp-context)
;;; emacs-mcp-context.el ends here
