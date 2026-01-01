;;; emacs-mcp-magit.el --- Magit integration for emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, git, vc, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates Magit with emacs-mcp for comprehensive Git
;; operations accessible via MCP tools.
;;
;; Features:
;; - Enhanced repository status (beyond basic git context)
;; - Branch operations (list, create, checkout)
;; - Staging and commits (non-interactive for MCP use)
;; - Diff and log viewing
;; - Remote operations (fetch, pull, push)
;;
;; When Magit is not installed, falls back to shell git commands.
;;
;; Usage:
;;   (emacs-mcp-addon-load 'magit)
;;   M-x emacs-mcp-magit-transient

;;; Code:

(require 'emacs-mcp-api)

;; Forward declarations for Magit
(declare-function magit-toplevel "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-get-upstream-branch "magit-git")
(declare-function magit-list-local-branch-names "magit-git")
(declare-function magit-list-remote-branch-names "magit-git")
(declare-function magit-stage-modified "magit-apply")
(declare-function magit-unstage-all "magit-apply")
(declare-function magit-refresh "magit-mode")
(declare-function magit-status "magit-status")

;; Forward declarations
(declare-function emacs-mcp-addon-register "emacs-mcp-addons")
(declare-function transient-define-prefix "transient")

;;;; Customization:

(defgroup emacs-mcp-magit nil
  "Magit integration for emacs-mcp."
  :group 'emacs-mcp
  :group 'magit
  :prefix "emacs-mcp-magit-")

(defcustom emacs-mcp-magit-log-count 10
  "Default number of commits to show in log operations."
  :type 'integer
  :group 'emacs-mcp-magit)

(defcustom emacs-mcp-magit-diff-context-lines 3
  "Number of context lines to show in diffs."
  :type 'integer
  :group 'emacs-mcp-magit)

(defcustom emacs-mcp-magit-prefer-magit t
  "When non-nil, prefer Magit functions over shell commands."
  :type 'boolean
  :group 'emacs-mcp-magit)

;;;; Internal:

(defvar emacs-mcp-magit--last-operation nil
  "Last git operation performed.")

(defun emacs-mcp-magit--magit-available-p ()
  "Return non-nil if Magit is available and preferred."
  (and emacs-mcp-magit-prefer-magit
       (featurep 'magit)
       ;; Verify key functions are actually bound
       (fboundp 'magit-toplevel)))

(defun emacs-mcp-magit--repo-root ()
  "Return the git repository root directory."
  (if (emacs-mcp-magit--magit-available-p)
      (magit-toplevel)
    (let ((root (locate-dominating-file default-directory ".git")))
      (when root (expand-file-name root)))))

(defun emacs-mcp-magit--ensure-repo ()
  "Ensure we are in a git repository."
  (unless (emacs-mcp-magit--repo-root)
    (error "Not in a git repository")))

(defun emacs-mcp-magit--shell-command (cmd)
  "Execute git CMD and return trimmed output."
  (let ((default-directory (or (emacs-mcp-magit--repo-root)
                               default-directory)))
    (string-trim (shell-command-to-string cmd))))

(defun emacs-mcp-magit--shell-lines (cmd)
  "Execute git CMD and return list of output lines."
  (let ((output (emacs-mcp-magit--shell-command cmd)))
    (unless (string-empty-p output)
      (split-string output "\n" t))))

;;;; Status Functions:

(defun emacs-mcp-magit--get-staged-files ()
  "Return list of staged files."
  (emacs-mcp-magit--shell-lines "git diff --cached --name-only 2>/dev/null"))

(defun emacs-mcp-magit--get-unstaged-files ()
  "Return list of unstaged modified files."
  (emacs-mcp-magit--shell-lines "git diff --name-only 2>/dev/null"))

(defun emacs-mcp-magit--get-untracked-files ()
  "Return list of untracked files."
  (emacs-mcp-magit--shell-lines "git ls-files --others --exclude-standard 2>/dev/null"))

(defun emacs-mcp-magit--get-current-branch ()
  "Return current branch name."
  (if (emacs-mcp-magit--magit-available-p)
      (magit-get-current-branch)
    (emacs-mcp-magit--shell-command "git rev-parse --abbrev-ref HEAD 2>/dev/null")))

(defun emacs-mcp-magit--get-upstream-branch ()
  "Return upstream tracking branch."
  (if (emacs-mcp-magit--magit-available-p)
      (magit-get-upstream-branch)
    (let ((result (emacs-mcp-magit--shell-command
                   "git rev-parse --abbrev-ref @{upstream} 2>/dev/null")))
      (unless (string-empty-p result) result))))

(defun emacs-mcp-magit--get-stash-list ()
  "Return list of stashes."
  (emacs-mcp-magit--shell-lines "git stash list --oneline 2>/dev/null"))

(defun emacs-mcp-magit--get-recent-commits (&optional count)
  "Return last COUNT commits as list of plists."
  (let* ((n (or count emacs-mcp-magit-log-count))
         (format-str "%H%x00%h%x00%an%x00%ae%x00%s%x00%ai")
         (lines (emacs-mcp-magit--shell-lines
                 (format "git log -n%d --format='%s' 2>/dev/null" n format-str))))
    (mapcar (lambda (line)
              (let ((parts (split-string line "\x00")))
                (list :hash (nth 0 parts)
                      :short-hash (nth 1 parts)
                      :author (nth 2 parts)
                      :email (nth 3 parts)
                      :subject (nth 4 parts)
                      :date (nth 5 parts))))
            lines)))

(defun emacs-mcp-magit--get-ahead-behind ()
  "Return commits ahead/behind upstream as plist."
  (let ((result (emacs-mcp-magit--shell-command
                 "git rev-list --left-right --count @{upstream}...HEAD 2>/dev/null")))
    (if (string-empty-p result)
        (list :ahead 0 :behind 0)
      (let ((parts (split-string result)))
        (list :behind (string-to-number (or (nth 0 parts) "0"))
              :ahead (string-to-number (or (nth 1 parts) "0")))))))

;;;; Branch Functions:

(defun emacs-mcp-magit--list-local-branches ()
  "Return list of local branch names."
  (if (emacs-mcp-magit--magit-available-p)
      (magit-list-local-branch-names)
    (emacs-mcp-magit--shell-lines "git branch --format='%(refname:short)' 2>/dev/null")))

(defun emacs-mcp-magit--list-remote-branches ()
  "Return list of remote branch names."
  (if (emacs-mcp-magit--magit-available-p)
      (magit-list-remote-branch-names)
    (emacs-mcp-magit--shell-lines "git branch -r --format='%(refname:short)' 2>/dev/null")))

(defun emacs-mcp-magit--create-branch (name &optional start-point)
  "Create branch NAME at START-POINT."
  (emacs-mcp-magit--ensure-repo)
  (let ((start (or start-point "HEAD")))
    (emacs-mcp-magit--shell-command
     (format "git branch %s %s 2>&1"
             (shell-quote-argument name)
             (shell-quote-argument start)))))

(defun emacs-mcp-magit--checkout-branch (name)
  "Checkout branch NAME."
  (emacs-mcp-magit--ensure-repo)
  (emacs-mcp-magit--shell-command
   (format "git checkout %s 2>&1" (shell-quote-argument name))))

;;;; Staging Functions:

(defun emacs-mcp-magit--stage-file (file)
  "Stage FILE for commit."
  (emacs-mcp-magit--ensure-repo)
  ;; Use git directly - magit-stage-files is interactive
  (let ((result (emacs-mcp-magit--shell-command
                 (format "git add %s 2>&1" (shell-quote-argument file)))))
    (when (emacs-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

(defun emacs-mcp-magit--stage-all ()
  "Stage all modified files."
  (emacs-mcp-magit--ensure-repo)
  (if (emacs-mcp-magit--magit-available-p)
      (magit-stage-modified t)
    (emacs-mcp-magit--shell-command "git add -u 2>&1")))

(defun emacs-mcp-magit--unstage-file (file)
  "Unstage FILE."
  (emacs-mcp-magit--ensure-repo)
  ;; Use git directly - magit-unstage-files is interactive
  (let ((result (emacs-mcp-magit--shell-command
                 (format "git reset HEAD %s 2>&1" (shell-quote-argument file)))))
    (when (emacs-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

(defun emacs-mcp-magit--unstage-all ()
  "Unstage all staged files."
  (emacs-mcp-magit--ensure-repo)
  (if (emacs-mcp-magit--magit-available-p)
      (magit-unstage-all)
    (emacs-mcp-magit--shell-command "git reset HEAD 2>&1")))

;;;; Commit Functions:

(defun emacs-mcp-magit--commit (message)
  "Create commit with MESSAGE."
  (emacs-mcp-magit--ensure-repo)
  (let ((staged (emacs-mcp-magit--get-staged-files)))
    (unless staged
      (error "No staged changes to commit"))
    (let ((result (emacs-mcp-magit--shell-command
                   (format "git commit -m %s 2>&1"
                           (shell-quote-argument message)))))
      (when (emacs-mcp-magit--magit-available-p)
        (magit-refresh))
      result)))

(defun emacs-mcp-magit--commit-all (message)
  "Stage all changes and commit with MESSAGE."
  (emacs-mcp-magit--ensure-repo)
  (let ((result (emacs-mcp-magit--shell-command
                 (format "git commit -am %s 2>&1"
                         (shell-quote-argument message)))))
    (when (emacs-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

;;;; Diff Functions:

(defun emacs-mcp-magit--diff-staged ()
  "Get diff of staged changes."
  (emacs-mcp-magit--ensure-repo)
  (emacs-mcp-magit--shell-command
   (format "git diff --cached -U%d 2>/dev/null"
           emacs-mcp-magit-diff-context-lines)))

(defun emacs-mcp-magit--diff-unstaged ()
  "Get diff of unstaged changes."
  (emacs-mcp-magit--ensure-repo)
  (emacs-mcp-magit--shell-command
   (format "git diff -U%d 2>/dev/null"
           emacs-mcp-magit-diff-context-lines)))

;;;; Remote Functions:

(defun emacs-mcp-magit--fetch (&optional remote)
  "Fetch from REMOTE (default: all remotes)."
  (emacs-mcp-magit--ensure-repo)
  (let ((result (if remote
                    (emacs-mcp-magit--shell-command
                     (format "git fetch %s 2>&1" (shell-quote-argument remote)))
                  (emacs-mcp-magit--shell-command "git fetch --all 2>&1"))))
    (when (emacs-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

(defun emacs-mcp-magit--pull ()
  "Pull from upstream."
  (emacs-mcp-magit--ensure-repo)
  (let ((result (emacs-mcp-magit--shell-command "git pull 2>&1")))
    (when (emacs-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

(defun emacs-mcp-magit--push (&optional set-upstream)
  "Push to remote.  SET-UPSTREAM to set tracking if needed."
  (emacs-mcp-magit--ensure-repo)
  (let* ((branch (emacs-mcp-magit--get-current-branch))
         (upstream (emacs-mcp-magit--get-upstream-branch))
         (cmd (if (or upstream (not set-upstream))
                  "git push 2>&1"
                (format "git push -u origin %s 2>&1"
                        (shell-quote-argument branch))))
         (result (emacs-mcp-magit--shell-command cmd)))
    (when (emacs-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

;;;; MCP API Functions:

;;;###autoload
(defun emacs-mcp-magit-api-status ()
  "Return comprehensive repository status as plist."
  (emacs-mcp-magit--ensure-repo)
  (let ((branch (emacs-mcp-magit--get-current-branch))
        (upstream (emacs-mcp-magit--get-upstream-branch))
        (ahead-behind (emacs-mcp-magit--get-ahead-behind)))
    (list :repository (emacs-mcp-magit--repo-root)
          :branch branch
          :upstream upstream
          :ahead (plist-get ahead-behind :ahead)
          :behind (plist-get ahead-behind :behind)
          :staged (emacs-mcp-magit--get-staged-files)
          :staged-count (length (emacs-mcp-magit--get-staged-files))
          :unstaged (emacs-mcp-magit--get-unstaged-files)
          :unstaged-count (length (emacs-mcp-magit--get-unstaged-files))
          :untracked (emacs-mcp-magit--get-untracked-files)
          :untracked-count (length (emacs-mcp-magit--get-untracked-files))
          :stashes (emacs-mcp-magit--get-stash-list)
          :recent-commits (emacs-mcp-magit--get-recent-commits 5)
          :clean (and (null (emacs-mcp-magit--get-staged-files))
                      (null (emacs-mcp-magit--get-unstaged-files))
                      (null (emacs-mcp-magit--get-untracked-files)))
          :magit-available (emacs-mcp-magit--magit-available-p))))

;;;###autoload
(defun emacs-mcp-magit-api-branches ()
  "Return branch information as plist."
  (emacs-mcp-magit--ensure-repo)
  (list :current (emacs-mcp-magit--get-current-branch)
        :upstream (emacs-mcp-magit--get-upstream-branch)
        :local (emacs-mcp-magit--list-local-branches)
        :remote (emacs-mcp-magit--list-remote-branches)))

;;;###autoload
(defun emacs-mcp-magit-api-log (&optional count)
  "Return recent COUNT commits."
  (emacs-mcp-magit--ensure-repo)
  (emacs-mcp-magit--get-recent-commits count))

;;;###autoload
(defun emacs-mcp-magit-api-diff (&optional target)
  "Return diff for TARGET (staged, unstaged, or all)."
  (emacs-mcp-magit--ensure-repo)
  (pcase target
    ((or 'nil 'staged) (emacs-mcp-magit--diff-staged))
    ('unstaged (emacs-mcp-magit--diff-unstaged))
    ('all (concat (emacs-mcp-magit--diff-staged)
                  "\n---\n"
                  (emacs-mcp-magit--diff-unstaged)))
    (_ (emacs-mcp-magit--diff-staged))))

;;;###autoload
(defun emacs-mcp-magit-api-stage (files)
  "Stage FILES for commit."
  (emacs-mcp-magit--ensure-repo)
  (cond
   ((eq files 'all) (emacs-mcp-magit--stage-all))
   ((stringp files) (emacs-mcp-magit--stage-file files))
   ((listp files) (dolist (f files) (emacs-mcp-magit--stage-file f)))))

;;;###autoload
(defun emacs-mcp-magit-api-commit (message &optional options)
  "Create commit with MESSAGE.
OPTIONS may contain :all to stage all changes first."
  (emacs-mcp-magit--ensure-repo)
  (if (plist-get options :all)
      (emacs-mcp-magit--commit-all message)
    (emacs-mcp-magit--commit message)))

;;;###autoload
(defun emacs-mcp-magit-api-push (&optional options)
  "Push to remote.
OPTIONS may contain :set-upstream to set tracking."
  (emacs-mcp-magit--ensure-repo)
  (emacs-mcp-magit--push (plist-get options :set-upstream)))

;;;###autoload
(defun emacs-mcp-magit-api-pull ()
  "Pull from upstream."
  (emacs-mcp-magit--ensure-repo)
  (emacs-mcp-magit--pull))

;;;###autoload
(defun emacs-mcp-magit-api-fetch (&optional remote)
  "Fetch from REMOTE (default: all remotes)."
  (emacs-mcp-magit--ensure-repo)
  (emacs-mcp-magit--fetch remote))

;;;; Interactive Commands:

;;;###autoload
(defun emacs-mcp-magit-status ()
  "Display repository status."
  (interactive)
  (let* ((status (emacs-mcp-magit-api-status))
         (buf (get-buffer-create "*MCP Git Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== MCP Git Status ===\n\n")
        (insert (format "Repository: %s\n" (plist-get status :repository)))
        (insert (format "Branch: %s" (plist-get status :branch)))
        (when-let* ((upstream (plist-get status :upstream)))
          (insert (format " -> %s" upstream)))
        (insert "\n")
        (when (or (> (plist-get status :ahead) 0)
                  (> (plist-get status :behind) 0))
          (insert (format "[ahead %d, behind %d]\n"
                          (plist-get status :ahead)
                          (plist-get status :behind))))
        (insert "\n")
        (insert (format "Staged: %d files\n" (plist-get status :staged-count)))
        (dolist (f (plist-get status :staged))
          (insert (format "  + %s\n" f)))
        (insert (format "Unstaged: %d files\n" (plist-get status :unstaged-count)))
        (dolist (f (plist-get status :unstaged))
          (insert (format "  M %s\n" f)))
        (insert (format "Untracked: %d files\n" (plist-get status :untracked-count)))
        (dolist (f (plist-get status :untracked))
          (insert (format "  ? %s\n" f)))
        (insert (format "\nClean: %s\n" (if (plist-get status :clean) "Yes" "No")))
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun emacs-mcp-magit-commit-interactive ()
  "Interactive commit with prompted message."
  (interactive)
  (emacs-mcp-magit--ensure-repo)
  (let ((message (read-string "Commit message: ")))
    (when (string-empty-p message)
      (error "Commit message cannot be empty"))
    (message "%s" (emacs-mcp-magit-api-commit message))))

;;;###autoload
(defun emacs-mcp-magit-stage-current-file ()
  "Stage the current buffer's file."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (progn
        (emacs-mcp-magit-api-stage file)
        (message "Staged: %s" (file-name-nondirectory file)))
    (error "Buffer is not visiting a file")))

;;;###autoload
(defun emacs-mcp-magit-open-magit-status ()
  "Open Magit status if available, otherwise show MCP status."
  (interactive)
  (if (emacs-mcp-magit--magit-available-p)
      (magit-status)
    (emacs-mcp-magit-status)))

;;;; Transient Menu:

;;;###autoload
(defun emacs-mcp-magit-transient ()
  "MCP Magit menu."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (transient-define-prefix emacs-mcp-magit--menu ()
          "MCP Magit menu."
          ["emacs-mcp + Magit"
           ["Status"
            ("s" "MCP Status" emacs-mcp-magit-status)
            ("S" "Magit status" emacs-mcp-magit-open-magit-status)]
           ["Stage & Commit"
            ("a" "Stage file" emacs-mcp-magit-stage-current-file)
            ("A" "Stage all" (lambda () (interactive)
                               (message "%s" (emacs-mcp-magit-api-stage 'all))))
            ("c" "Commit" emacs-mcp-magit-commit-interactive)]
           ["Remote"
            ("f" "Fetch" (lambda () (interactive)
                           (message "%s" (emacs-mcp-magit-api-fetch))))
            ("p" "Pull" (lambda () (interactive)
                          (message "%s" (emacs-mcp-magit-api-pull))))
            ("P" "Push" (lambda () (interactive)
                          (message "%s" (emacs-mcp-magit-api-push))))]])
        (emacs-mcp-magit--menu))
    (message "Transient not available")))

;;;; Minor Mode:

(defvar emacs-mcp-magit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g s") #'emacs-mcp-magit-status)
    (define-key map (kbd "C-c g S") #'emacs-mcp-magit-open-magit-status)
    (define-key map (kbd "C-c g a") #'emacs-mcp-magit-stage-current-file)
    (define-key map (kbd "C-c g c") #'emacs-mcp-magit-commit-interactive)
    (define-key map (kbd "C-c g g") #'emacs-mcp-magit-transient)
    map)
  "Keymap for `emacs-mcp-magit-mode'.")

;;;###autoload
(define-minor-mode emacs-mcp-magit-mode
  "Minor mode for Magit integration."
  :init-value nil
  :lighter " MCP-Git"
  :global t
  :keymap emacs-mcp-magit-mode-map
  :group 'emacs-mcp-magit
  (if emacs-mcp-magit-mode
      (message "emacs-mcp-magit enabled")
    (message "emacs-mcp-magit disabled")))

;;;; Addon Lifecycle:

(defun emacs-mcp-magit--addon-init ()
  "Initialize magit addon."
  (require 'emacs-mcp-api nil t)
  (when (require 'magit nil t)
    ;; Require submodules containing functions we use
    (require 'magit-git nil t)    ; magit-toplevel, magit-get-current-branch, etc.
    (require 'magit-apply nil t)  ; magit-stage-modified, magit-unstage-all
    (require 'magit-mode nil t))  ; magit-refresh
  (message "emacs-mcp-magit: initialized%s"
           (if (featurep 'magit) " (with Magit)" " (shell fallback)")))

(defun emacs-mcp-magit--addon-shutdown ()
  "Shutdown magit addon."
  (when emacs-mcp-magit-mode
    (emacs-mcp-magit-mode -1))
  (message "emacs-mcp-magit: shutdown"))

;;;; Addon Registration:

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'magit
   :version "0.1.0"
   :description "Magit Git integration with shell fallback"
   :requires '(emacs-mcp-api)
   :provides '(emacs-mcp-magit-mode emacs-mcp-magit-transient)
   :init #'emacs-mcp-magit--addon-init
   :shutdown #'emacs-mcp-magit--addon-shutdown))

(provide 'emacs-mcp-magit)
;;; emacs-mcp-magit.el ends here
