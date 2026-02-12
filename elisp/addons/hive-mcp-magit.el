;;; hive-mcp-magit.el --- Magit integration for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, git, vc, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates Magit with hive-mcp for comprehensive Git
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
;;   (hive-mcp-addon-load 'magit)
;;   M-x hive-mcp-magit-transient

;;; Code:

(require 'hive-mcp-api)

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
(declare-function hive-mcp-addon-register "hive-mcp-addons")
(declare-function transient-define-prefix "transient")

;;;; Customization:

(defgroup hive-mcp-magit nil
  "Magit integration for hive-mcp."
  :group 'hive-mcp
  :group 'magit
  :prefix "hive-mcp-magit-")

(defcustom hive-mcp-magit-log-count 10
  "Default number of commits to show in log operations."
  :type 'integer
  :group 'hive-mcp-magit)

(defcustom hive-mcp-magit-diff-context-lines 3
  "Number of context lines to show in diffs."
  :type 'integer
  :group 'hive-mcp-magit)

(defcustom hive-mcp-magit-prefer-magit t
  "When non-nil, prefer Magit functions over shell commands."
  :type 'boolean
  :group 'hive-mcp-magit)

;;;; Internal:

(defvar hive-mcp-magit--last-operation nil
  "Last git operation performed.")

(defun hive-mcp-magit--magit-available-p ()
  "Return non-nil if Magit is available and preferred."
  (and hive-mcp-magit-prefer-magit
       (featurep 'magit)
       ;; Verify key functions are actually bound
       (fboundp 'magit-toplevel)))

(defun hive-mcp-magit--repo-root ()
  "Return the git repository root directory."
  (if (hive-mcp-magit--magit-available-p)
      (magit-toplevel)
    (let ((root (locate-dominating-file default-directory ".git")))
      (when root (expand-file-name root)))))

(defun hive-mcp-magit--ensure-repo ()
  "Ensure we are in a git repository."
  (unless (hive-mcp-magit--repo-root)
    (error "Not in a git repository")))

(defun hive-mcp-magit--shell-command (cmd)
  "Execute git CMD and return trimmed output."
  (let ((default-directory (or (hive-mcp-magit--repo-root)
                               default-directory)))
    (string-trim (shell-command-to-string cmd))))

(defun hive-mcp-magit--shell-lines (cmd)
  "Execute git CMD and return list of output lines."
  (let ((output (hive-mcp-magit--shell-command cmd)))
    (unless (string-empty-p output)
      (split-string output "\n" t))))

;;;; Status Functions:

(defun hive-mcp-magit--get-staged-files ()
  "Return list of staged files."
  (hive-mcp-magit--shell-lines "git diff --cached --name-only 2>/dev/null"))

(defun hive-mcp-magit--get-unstaged-files ()
  "Return list of unstaged modified files."
  (hive-mcp-magit--shell-lines "git diff --name-only 2>/dev/null"))

(defun hive-mcp-magit--get-untracked-files ()
  "Return list of untracked files."
  (hive-mcp-magit--shell-lines "git ls-files --others --exclude-standard 2>/dev/null"))

(defun hive-mcp-magit--get-current-branch ()
  "Return current branch name."
  (if (hive-mcp-magit--magit-available-p)
      (magit-get-current-branch)
    (hive-mcp-magit--shell-command "git rev-parse --abbrev-ref HEAD 2>/dev/null")))

(defun hive-mcp-magit--get-upstream-branch ()
  "Return upstream tracking branch."
  (if (hive-mcp-magit--magit-available-p)
      (magit-get-upstream-branch)
    (let ((result (hive-mcp-magit--shell-command
                   "git rev-parse --abbrev-ref @{upstream} 2>/dev/null")))
      (unless (string-empty-p result) result))))

(defun hive-mcp-magit--get-stash-list ()
  "Return list of stashes."
  (hive-mcp-magit--shell-lines "git stash list --oneline 2>/dev/null"))

(defun hive-mcp-magit--get-recent-commits (&optional count)
  "Return last COUNT commits as list of plists."
  (let* ((n (or count hive-mcp-magit-log-count))
         (format-str "%H%x00%h%x00%an%x00%ae%x00%s%x00%ai")
         (lines (hive-mcp-magit--shell-lines
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

(defun hive-mcp-magit--get-ahead-behind ()
  "Return commits ahead/behind upstream as plist."
  (let ((result (hive-mcp-magit--shell-command
                 "git rev-list --left-right --count @{upstream}...HEAD 2>/dev/null")))
    (if (string-empty-p result)
        (list :ahead 0 :behind 0)
      (let ((parts (split-string result)))
        (list :behind (string-to-number (or (nth 0 parts) "0"))
              :ahead (string-to-number (or (nth 1 parts) "0")))))))

;;;; Branch Functions:

(defun hive-mcp-magit--list-local-branches ()
  "Return list of local branch names."
  (if (hive-mcp-magit--magit-available-p)
      (magit-list-local-branch-names)
    (hive-mcp-magit--shell-lines "git branch --format='%(refname:short)' 2>/dev/null")))

(defun hive-mcp-magit--list-remote-branches ()
  "Return list of remote branch names."
  (if (hive-mcp-magit--magit-available-p)
      (magit-list-remote-branch-names)
    (hive-mcp-magit--shell-lines "git branch -r --format='%(refname:short)' 2>/dev/null")))

(defun hive-mcp-magit--create-branch (name &optional start-point)
  "Create branch NAME at START-POINT."
  (hive-mcp-magit--ensure-repo)
  (let ((start (or start-point "HEAD")))
    (hive-mcp-magit--shell-command
     (format "git branch %s %s 2>&1"
             (shell-quote-argument name)
             (shell-quote-argument start)))))

(defun hive-mcp-magit--checkout-branch (name)
  "Checkout branch NAME."
  (hive-mcp-magit--ensure-repo)
  (hive-mcp-magit--shell-command
   (format "git checkout %s 2>&1" (shell-quote-argument name))))

;;;; Staging Functions:

(defun hive-mcp-magit--stage-file (file)
  "Stage FILE for commit."
  (hive-mcp-magit--ensure-repo)
  ;; Use git directly - magit-stage-files is interactive
  (let ((result (hive-mcp-magit--shell-command
                 (format "git add %s 2>&1" (shell-quote-argument file)))))
    (when (hive-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

(defun hive-mcp-magit--stage-all ()
  "Stage all modified files."
  (hive-mcp-magit--ensure-repo)
  (if (hive-mcp-magit--magit-available-p)
      (magit-stage-modified t)
    (hive-mcp-magit--shell-command "git add -u 2>&1")))

(defun hive-mcp-magit--unstage-file (file)
  "Unstage FILE."
  (hive-mcp-magit--ensure-repo)
  ;; Use git directly - magit-unstage-files is interactive
  (let ((result (hive-mcp-magit--shell-command
                 (format "git reset HEAD %s 2>&1" (shell-quote-argument file)))))
    (when (hive-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

(defun hive-mcp-magit--unstage-all ()
  "Unstage all staged files."
  (hive-mcp-magit--ensure-repo)
  (if (hive-mcp-magit--magit-available-p)
      (magit-unstage-all)
    (hive-mcp-magit--shell-command "git reset HEAD 2>&1")))

;;;; Commit Functions:

(defun hive-mcp-magit--commit (message)
  "Create commit with MESSAGE."
  (hive-mcp-magit--ensure-repo)
  (let ((staged (hive-mcp-magit--get-staged-files)))
    (unless staged
      (error "No staged changes to commit"))
    (let ((result (hive-mcp-magit--shell-command
                   (format "git commit -m %s 2>&1"
                           (shell-quote-argument message)))))
      (when (hive-mcp-magit--magit-available-p)
        (magit-refresh))
      result)))

(defun hive-mcp-magit--commit-all (message)
  "Stage all changes and commit with MESSAGE."
  (hive-mcp-magit--ensure-repo)
  (let ((result (hive-mcp-magit--shell-command
                 (format "git commit -am %s 2>&1"
                         (shell-quote-argument message)))))
    (when (hive-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

;;;; Diff Functions:

(defun hive-mcp-magit--diff-staged ()
  "Get diff of staged changes."
  (hive-mcp-magit--ensure-repo)
  (hive-mcp-magit--shell-command
   (format "git diff --cached -U%d 2>/dev/null"
           hive-mcp-magit-diff-context-lines)))

(defun hive-mcp-magit--diff-unstaged ()
  "Get diff of unstaged changes."
  (hive-mcp-magit--ensure-repo)
  (hive-mcp-magit--shell-command
   (format "git diff -U%d 2>/dev/null"
           hive-mcp-magit-diff-context-lines)))

;;;; Remote Functions:

(defun hive-mcp-magit--fetch (&optional remote)
  "Fetch from REMOTE (default: all remotes)."
  (hive-mcp-magit--ensure-repo)
  (let ((result (if remote
                    (hive-mcp-magit--shell-command
                     (format "git fetch %s 2>&1" (shell-quote-argument remote)))
                  (hive-mcp-magit--shell-command "git fetch --all 2>&1"))))
    (when (hive-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

(defun hive-mcp-magit--pull ()
  "Pull from upstream."
  (hive-mcp-magit--ensure-repo)
  (let ((result (hive-mcp-magit--shell-command "git pull 2>&1")))
    (when (hive-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

(defun hive-mcp-magit--push (&optional set-upstream)
  "Push to remote.  SET-UPSTREAM to set tracking if needed."
  (hive-mcp-magit--ensure-repo)
  (let* ((branch (hive-mcp-magit--get-current-branch))
         (upstream (hive-mcp-magit--get-upstream-branch))
         (cmd (if (or upstream (not set-upstream))
                  "git push 2>&1"
                (format "git push -u origin %s 2>&1"
                        (shell-quote-argument branch))))
         (result (hive-mcp-magit--shell-command cmd)))
    (when (hive-mcp-magit--magit-available-p)
      (magit-refresh))
    result))

;;;; MCP API Functions:

;;;###autoload
(defun hive-mcp-magit-api-status (&optional directory)
  "Return comprehensive repository status as plist.
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (let ((branch (hive-mcp-magit--get-current-branch))
          (upstream (hive-mcp-magit--get-upstream-branch))
          (ahead-behind (hive-mcp-magit--get-ahead-behind)))
      (list :repository (hive-mcp-magit--repo-root)
            :branch branch
            :upstream upstream
            :ahead (plist-get ahead-behind :ahead)
            :behind (plist-get ahead-behind :behind)
            :staged (hive-mcp-magit--get-staged-files)
            :staged-count (length (hive-mcp-magit--get-staged-files))
            :unstaged (hive-mcp-magit--get-unstaged-files)
            :unstaged-count (length (hive-mcp-magit--get-unstaged-files))
            :untracked (hive-mcp-magit--get-untracked-files)
            :untracked-count (length (hive-mcp-magit--get-untracked-files))
            :stashes (hive-mcp-magit--get-stash-list)
            :recent-commits (hive-mcp-magit--get-recent-commits 5)
            :clean (and (null (hive-mcp-magit--get-staged-files))
                        (null (hive-mcp-magit--get-unstaged-files))
                        (null (hive-mcp-magit--get-untracked-files)))
            :magit-available (hive-mcp-magit--magit-available-p)))))

;;;###autoload
(defun hive-mcp-magit-api-branches (&optional directory)
  "Return branch information as plist.
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (list :current (hive-mcp-magit--get-current-branch)
          :upstream (hive-mcp-magit--get-upstream-branch)
          :local (hive-mcp-magit--list-local-branches)
          :remote (hive-mcp-magit--list-remote-branches))))

;;;###autoload
(defun hive-mcp-magit-api-log (&optional count directory)
  "Return recent COUNT commits.
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (hive-mcp-magit--get-recent-commits count)))

;;;###autoload
(defun hive-mcp-magit-api-diff (&optional target directory)
  "Return diff for TARGET (staged, unstaged, or all).
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (pcase target
      ((or 'nil 'staged) (hive-mcp-magit--diff-staged))
      ('unstaged (hive-mcp-magit--diff-unstaged))
      ('all (concat (hive-mcp-magit--diff-staged)
                    "\n---\n"
                    (hive-mcp-magit--diff-unstaged)))
      (_ (hive-mcp-magit--diff-staged)))))

;;;###autoload
(defun hive-mcp-magit-api-stage (files &optional directory)
  "Stage FILES for commit.
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (cond
     ((eq files 'all) (hive-mcp-magit--stage-all))
     ((stringp files) (hive-mcp-magit--stage-file files))
     ((listp files) (dolist (f files) (hive-mcp-magit--stage-file f))))))

;;;###autoload
(defun hive-mcp-magit-api-commit (message &optional options directory)
  "Create commit with MESSAGE.
OPTIONS may contain :all to stage all changes first.
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (if (plist-get options :all)
        (hive-mcp-magit--commit-all message)
      (hive-mcp-magit--commit message))))

;;;###autoload
(defun hive-mcp-magit-api-push (&optional options directory)
  "Push to remote.
OPTIONS may contain :set-upstream to set tracking.
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (hive-mcp-magit--push (plist-get options :set-upstream))))

;;;###autoload
(defun hive-mcp-magit-api-pull (&optional directory)
  "Pull from upstream.
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (hive-mcp-magit--pull)))

;;;###autoload
(defun hive-mcp-magit-api-fetch (&optional remote directory)
  "Fetch from REMOTE (default: all remotes).
DIRECTORY overrides `default-directory' if provided."
  (let ((default-directory (or directory default-directory)))
    (hive-mcp-magit--ensure-repo)
    (hive-mcp-magit--fetch remote)))

;;;; Interactive Commands:

;;;###autoload
(defun hive-mcp-magit-status ()
  "Display repository status."
  (interactive)
  (let* ((status (hive-mcp-magit-api-status))
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
(defun hive-mcp-magit-commit-interactive ()
  "Interactive commit with prompted message."
  (interactive)
  (hive-mcp-magit--ensure-repo)
  (let ((message (read-string "Commit message: ")))
    (when (string-empty-p message)
      (error "Commit message cannot be empty"))
    (message "%s" (hive-mcp-magit-api-commit message))))

;;;###autoload
(defun hive-mcp-magit-stage-current-file ()
  "Stage the current buffer's file."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (progn
        (hive-mcp-magit-api-stage file)
        (message "Staged: %s" (file-name-nondirectory file)))
    (error "Buffer is not visiting a file")))

;;;###autoload
(defun hive-mcp-magit-open-magit-status ()
  "Open Magit status if available, otherwise show MCP status."
  (interactive)
  (if (hive-mcp-magit--magit-available-p)
      (magit-status)
    (hive-mcp-magit-status)))

;;;; Transient Menu:

;;;###autoload
(defun hive-mcp-magit-transient ()
  "MCP Magit menu."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (transient-define-prefix hive-mcp-magit--menu ()
          "MCP Magit menu."
          ["hive-mcp + Magit"
           ["Status"
            ("s" "MCP Status" hive-mcp-magit-status)
            ("S" "Magit status" hive-mcp-magit-open-magit-status)]
           ["Stage & Commit"
            ("a" "Stage file" hive-mcp-magit-stage-current-file)
            ("A" "Stage all" (lambda () (interactive)
                               (message "%s" (hive-mcp-magit-api-stage 'all))))
            ("c" "Commit" hive-mcp-magit-commit-interactive)]
           ["Remote"
            ("f" "Fetch" (lambda () (interactive)
                           (message "%s" (hive-mcp-magit-api-fetch))))
            ("p" "Pull" (lambda () (interactive)
                          (message "%s" (hive-mcp-magit-api-pull))))
            ("P" "Push" (lambda () (interactive)
                          (message "%s" (hive-mcp-magit-api-push))))]])
        (hive-mcp-magit--menu))
    (message "Transient not available")))

;;;; Minor Mode:

(defvar hive-mcp-magit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g s") #'hive-mcp-magit-status)
    (define-key map (kbd "C-c g S") #'hive-mcp-magit-open-magit-status)
    (define-key map (kbd "C-c g a") #'hive-mcp-magit-stage-current-file)
    (define-key map (kbd "C-c g c") #'hive-mcp-magit-commit-interactive)
    (define-key map (kbd "C-c g g") #'hive-mcp-magit-transient)
    map)
  "Keymap for `hive-mcp-magit-mode'.")

;;;###autoload
(define-minor-mode hive-mcp-magit-mode
  "Minor mode for Magit integration."
  :init-value nil
  :lighter " MCP-Git"
  :global t
  :keymap hive-mcp-magit-mode-map
  :group 'hive-mcp-magit
  (if hive-mcp-magit-mode
      (message "hive-mcp-magit enabled")
    (message "hive-mcp-magit disabled")))

;;;; Addon Lifecycle:

(defun hive-mcp-magit--addon-init ()
  "Initialize magit addon."
  (require 'hive-mcp-api nil t)
  (when (require 'magit nil t)
    ;; Require submodules containing functions we use
    (require 'magit-git nil t)    ; magit-toplevel, magit-get-current-branch, etc.
    (require 'magit-apply nil t)  ; magit-stage-modified, magit-unstage-all
    (require 'magit-mode nil t))  ; magit-refresh
  (message "hive-mcp-magit: initialized%s"
           (if (featurep 'magit) " (with Magit)" " (shell fallback)")))

(defun hive-mcp-magit--addon-shutdown ()
  "Shutdown magit addon."
  (when hive-mcp-magit-mode
    (hive-mcp-magit-mode -1))
  (message "hive-mcp-magit: shutdown"))

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'magit
   :version "0.1.0"
   :description "Magit Git integration with shell fallback"
   :requires '(hive-mcp-api)
   :provides '(hive-mcp-magit-mode hive-mcp-magit-transient)
   :init #'hive-mcp-magit--addon-init
   :shutdown #'hive-mcp-magit--addon-shutdown))

(provide 'hive-mcp-magit)
;;; hive-mcp-magit.el ends here
