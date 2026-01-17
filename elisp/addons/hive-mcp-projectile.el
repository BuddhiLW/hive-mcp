;;; hive-mcp-projectile.el --- Projectile integration for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, project, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates Projectile (project management) with hive-mcp.
;;
;; OPTIONAL DEPENDENCIES:
;; - projectile (https://github.com/bbatsov/projectile)
;;   Install via: M-x package-install RET projectile RET
;;
;; This addon activates automatically when projectile is loaded.
;; Without projectile, the addon is silently disabled.
;;
;; Features:
;; - Get current project root and info
;; - List all known projectile projects
;; - Detect project type (npm, maven, lein, etc.)
;; - List project files with optional pattern filtering
;; - Find files in project by name
;; - Search/grep in project (uses rg if available)
;;
;; Usage:
;;   (hive-mcp-addon-load 'projectile)
;;   M-x hive-mcp-projectile-transient

;;; Code:

(require 'hive-mcp-api)

;; Soft dependencies
(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function projectile-project-type "projectile")
(declare-function projectile-current-project-files "projectile")
(declare-function projectile-recentf-files "projectile")
(declare-function projectile-switch-project-by-name "projectile")
(declare-function projectile-project-p "projectile")
(declare-function projectile-invalidate-cache "projectile")

(defvar projectile-known-projects)

;; Forward declarations
(declare-function hive-mcp-addon-register "hive-mcp-addons")
(declare-function transient-define-prefix "transient")

;;;; Customization:

(defgroup hive-mcp-projectile nil
  "Projectile integration for hive-mcp."
  :group 'hive-mcp
  :group 'projectile
  :prefix "hive-mcp-projectile-")

(defcustom hive-mcp-projectile-max-files 1000
  "Maximum number of files to return in project file listings."
  :type 'integer
  :group 'hive-mcp-projectile)

(defcustom hive-mcp-projectile-max-search-results 100
  "Maximum number of search results to return."
  :type 'integer
  :group 'hive-mcp-projectile)

(defcustom hive-mcp-projectile-use-ripgrep t
  "When non-nil, prefer ripgrep over grep for searching."
  :type 'boolean
  :group 'hive-mcp-projectile)

;;;; Internal:

(defvar hive-mcp-projectile--initialized nil
  "Whether the addon has been initialized.")

(defun hive-mcp-projectile--ensure-projectile ()
  "Ensure projectile is available."
  (and (featurep 'projectile)
       (fboundp 'projectile-project-root)))

(defun hive-mcp-projectile--in-project-p ()
  "Return non-nil if currently in a projectile project."
  (and (hive-mcp-projectile--ensure-projectile)
       (projectile-project-p)))

(defun hive-mcp-projectile--ripgrep-available-p ()
  "Return non-nil if ripgrep is available."
  (and hive-mcp-projectile-use-ripgrep
       (executable-find "rg")))

;;;; Project Info:

(defun hive-mcp-projectile--get-project-root ()
  "Get current projectile project root, or nil."
  (when (hive-mcp-projectile--ensure-projectile)
    (condition-case nil
        (projectile-project-root)
      (error nil))))

(defun hive-mcp-projectile--get-project-name ()
  "Get current projectile project name, or nil."
  (when (hive-mcp-projectile--ensure-projectile)
    (condition-case nil
        (projectile-project-name)
      (error nil))))

(defun hive-mcp-projectile--get-project-type ()
  "Get current projectile project type as a string."
  (when (hive-mcp-projectile--ensure-projectile)
    (condition-case nil
        (let ((type (projectile-project-type)))
          (if type (symbol-name type) "generic"))
      (error "unknown"))))

(defun hive-mcp-projectile--detect-extended-type (root)
  "Detect extended project type based on files in ROOT."
  (when root
    (let ((markers nil))
      (dolist (pair '(("package.json" . npm)
                      ("Cargo.toml" . cargo)
                      ("go.mod" . go-mod)
                      ("deps.edn" . clojure-deps)
                      ("project.clj" . leiningen)
                      ("shadow-cljs.edn" . shadow-cljs)
                      ("pyproject.toml" . python-pyproject)
                      ("pom.xml" . maven)
                      ("build.gradle" . gradle)
                      ("Makefile" . make)
                      ("Dockerfile" . docker)
                      (".git" . git)))
        (when (file-exists-p (expand-file-name (car pair) root))
          (push (cdr pair) markers)))
      (nreverse markers))))

;;;; File Operations:

(defun hive-mcp-projectile--list-files (&optional pattern)
  "List project files, optionally filtered by PATTERN."
  (when (hive-mcp-projectile--in-project-p)
    (let* ((files (projectile-current-project-files))
           (filtered (if pattern
                         (seq-filter
                          (lambda (f)
                            (or (string-match-p (regexp-quote pattern) f)
                                (string-match-p (wildcard-to-regexp pattern) f)))
                          files)
                       files)))
      (seq-take filtered hive-mcp-projectile-max-files))))

(defun hive-mcp-projectile--find-file-matches (filename)
  "Find files in project matching FILENAME."
  (when (hive-mcp-projectile--in-project-p)
    (let* ((files (projectile-current-project-files))
           (matches (seq-filter
                     (lambda (f)
                       (or (string= (file-name-nondirectory f) filename)
                           (string-match-p (regexp-quote filename)
                                           (file-name-nondirectory f))))
                     files)))
      matches)))

(defun hive-mcp-projectile--recent-files ()
  "Get recently visited files in current project."
  (when (hive-mcp-projectile--in-project-p)
    (condition-case nil
        (projectile-recentf-files)
      (error nil))))

;;;; Search:

(defun hive-mcp-projectile--search (pattern)
  "Search project for PATTERN using rg or grep."
  (when-let* ((root (hive-mcp-projectile--get-project-root)))
    (let* ((default-directory root)
           (cmd (if (hive-mcp-projectile--ripgrep-available-p)
                    (format "rg --line-number --no-heading --color=never -e %s ."
                            (shell-quote-argument pattern))
                  (format "grep -r -n -H -e %s ."
                          (shell-quote-argument pattern))))
           (output (shell-command-to-string cmd))
           (lines (split-string output "\n" t))
           (results nil))
      (dolist (line (seq-take lines hive-mcp-projectile-max-search-results))
        (when (string-match "^\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$" line)
          (push (list :file (match-string 1 line)
                      :line (string-to-number (match-string 2 line))
                      :content (string-trim (match-string 3 line)))
                results)))
      (nreverse results))))

;;;; MCP API Functions:

;;;###autoload
(defun hive-mcp-projectile-api-project-info ()
  "Get current project info as plist."
  (when (hive-mcp-projectile--in-project-p)
    (let ((root (hive-mcp-projectile--get-project-root)))
      (list :name (hive-mcp-projectile--get-project-name)
            :root root
            :type (hive-mcp-projectile--get-project-type)
            :extended-types (hive-mcp-projectile--detect-extended-type root)
            :file-count (length (projectile-current-project-files))
            :in-project t))))

;;;###autoload
(defun hive-mcp-projectile-api-list-projects ()
  "List all known projectile projects."
  (if (hive-mcp-projectile--ensure-projectile)
      (let ((projects (or (and (boundp 'projectile-known-projects)
                               projectile-known-projects)
                          nil)))
        (apply #'vector
               (mapcar (lambda (root)
                         (list :root root
                               :name (file-name-nondirectory
                                      (directory-file-name root))
                               :exists (file-directory-p root)
                               :types (hive-mcp-projectile--detect-extended-type root)))
                       projects)))
    []))

;;;###autoload
(defun hive-mcp-projectile-api-project-files (&optional pattern)
  "List files in current project, optionally filtered by PATTERN."
  (if (hive-mcp-projectile--in-project-p)
      (apply #'vector (hive-mcp-projectile--list-files pattern))
    []))

;;;###autoload
(defun hive-mcp-projectile-api-find-file (filename)
  "Find files matching FILENAME in current project."
  (if (hive-mcp-projectile--in-project-p)
      (let* ((matches (hive-mcp-projectile--find-file-matches filename))
             (root (hive-mcp-projectile--get-project-root)))
        (apply #'vector
               (mapcar (lambda (f)
                         (list :relative f
                               :absolute (expand-file-name f root)))
                       matches)))
    []))

;;;###autoload
(defun hive-mcp-projectile-api-recent-files ()
  "Get recently visited files in current project."
  (if (hive-mcp-projectile--in-project-p)
      (apply #'vector (or (hive-mcp-projectile--recent-files) '()))
    []))

;;;###autoload
(defun hive-mcp-projectile-api-search (pattern)
  "Search current project for PATTERN."
  (if (hive-mcp-projectile--in-project-p)
      (apply #'vector (hive-mcp-projectile--search pattern))
    []))

;;;; Interactive Commands:

;;;###autoload
(defun hive-mcp-projectile-show-info ()
  "Display current project info."
  (interactive)
  (if (hive-mcp-projectile--in-project-p)
      (let* ((info (hive-mcp-projectile-api-project-info))
             (buf (get-buffer-create "*MCP Project Info*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert "=== Projectile Project Info ===\n\n")
          (insert (format "Name: %s\n" (plist-get info :name)))
          (insert (format "Root: %s\n" (plist-get info :root)))
          (insert (format "Type: %s\n" (plist-get info :type)))
          (insert (format "Extended Types: %s\n"
                          (mapconcat #'symbol-name
                                     (plist-get info :extended-types)
                                     ", ")))
          (insert (format "File Count: %d\n" (plist-get info :file-count)))
          (goto-char (point-min)))
        (display-buffer buf))
    (message "Not in a projectile project")))

;;;###autoload
(defun hive-mcp-projectile-list-projects ()
  "Display all known projects."
  (interactive)
  (let* ((projects (hive-mcp-projectile-api-list-projects))
         (buf (get-buffer-create "*MCP Known Projects*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Known Projectile Projects ===\n\n")
      (if (= (length projects) 0)
          (insert "No projects found.\n")
        (dotimes (i (length projects))
          (let ((proj (aref projects i)))
            (insert (format "%d. %s\n   Root: %s\n   Exists: %s\n\n"
                            (1+ i)
                            (plist-get proj :name)
                            (plist-get proj :root)
                            (if (plist-get proj :exists) "yes" "NO"))))))
      (goto-char (point-min)))
    (display-buffer buf)))

;;;###autoload
(defun hive-mcp-projectile-search-interactive (pattern)
  "Search project for PATTERN and display results."
  (interactive "sSearch pattern: ")
  (if (hive-mcp-projectile--in-project-p)
      (let* ((results (hive-mcp-projectile-api-search pattern))
             (buf (get-buffer-create "*MCP Project Search*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "=== Search Results for '%s' ===\n\n" pattern))
          (insert (format "Found: %d matches\n\n" (length results)))
          (if (= (length results) 0)
              (insert "No matches found.\n")
            (dotimes (i (length results))
              (let ((match (aref results i)))
                (insert (format "%s:%d: %s\n"
                                (plist-get match :file)
                                (plist-get match :line)
                                (plist-get match :content))))))
          (goto-char (point-min))
          (grep-mode))
        (display-buffer buf))
    (message "Not in a projectile project")))

;;;; Transient Menu:

;;;###autoload
(defun hive-mcp-projectile-transient ()
  "MCP Projectile menu."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (transient-define-prefix hive-mcp-projectile--menu ()
          "MCP Projectile menu."
          ["hive-mcp + Projectile"
           ["Info"
            ("i" "Project info" hive-mcp-projectile-show-info)
            ("p" "List projects" hive-mcp-projectile-list-projects)]
           ["Search"
            ("s" "Search" hive-mcp-projectile-search-interactive)]])
        (hive-mcp-projectile--menu))
    (message "Transient not available")))

;;;; Addon Lifecycle:

(defun hive-mcp-projectile--addon-init ()
  "Initialize projectile addon.
Does nothing if projectile is not available."
  (if (not (featurep 'projectile))
      (message "hive-mcp-projectile: projectile package not found, addon disabled")
    (require 'hive-mcp-api nil t)
    (setq hive-mcp-projectile--initialized t)
    (message "hive-mcp-projectile: initialized")))

(defun hive-mcp-projectile--addon-shutdown ()
  "Shutdown projectile addon."
  (setq hive-mcp-projectile--initialized nil)
  (message "hive-mcp-projectile: shutdown"))

;;;; Minor Mode:

;;;###autoload
(define-minor-mode hive-mcp-projectile-mode
  "Minor mode for Projectile integration."
  :init-value nil
  :lighter " MCP-Proj"
  :global t
  :group 'hive-mcp-projectile
  (if hive-mcp-projectile-mode
      (hive-mcp-projectile--addon-init)
    (message "hive-mcp-projectile disabled")))

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'projectile
   :version "0.1.0"
   :description "Projectile project management integration"
   :requires '(projectile hive-mcp-api)
   :provides '(hive-mcp-projectile-mode hive-mcp-projectile-transient)
   :init #'hive-mcp-projectile--addon-init
   :shutdown #'hive-mcp-projectile--addon-shutdown))

(provide 'hive-mcp-projectile)
;;; hive-mcp-projectile.el ends here
