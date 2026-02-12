;;; hive-mcp-docs.el --- Emacs documentation tools for MCP -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, help, docs, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon exposes Emacs's built-in documentation facilities to MCP.
;; Instead of building a full RAG system, it leverages Emacs's powerful
;; introspection capabilities and lets the LLM synthesize the results.
;;
;; MCP Tools provided:
;;   - mcp_describe_function: Get function documentation
;;   - mcp_describe_variable: Get variable documentation
;;   - mcp_apropos: Search symbols by pattern
;;   - mcp_package_functions: List functions in a package/feature
;;   - mcp_find_keybindings: Find keybindings for a command
;;   - mcp_info_lookup: Search Info manuals
;;   - mcp_package_commentary: Get package commentary section
;;
;; Usage:
;;   (hive-mcp-addon-load 'docs)
;;
;; Or add to always-load list:
;;   (add-to-list 'hive-mcp-addon-always-load 'docs)

;;; Code:

(require 'help-fns)
(require 'apropos)
(require 'info-look nil t)

;; Forward declarations
(declare-function hive-mcp-addon-register "hive-mcp-addons")

;;; Customization:

(defgroup hive-mcp-docs nil
  "Emacs documentation tools for MCP."
  :group 'hive-mcp
  :prefix "hive-mcp-docs-")

(defcustom hive-mcp-docs-max-results 50
  "Maximum number of results to return from apropos searches."
  :type 'integer
  :group 'hive-mcp-docs)

(defcustom hive-mcp-docs-include-source-location t
  "When non-nil, include source file location in function descriptions."
  :type 'boolean
  :group 'hive-mcp-docs)

;;; Helper Functions:

(defun hive-mcp-docs--get-docstring (symbol)
  "Get the docstring for SYMBOL (function or variable)."
  (cond
   ((fboundp symbol) (documentation symbol t))
   ((boundp symbol) (documentation-property symbol 'variable-documentation t))
   (t nil)))

(defun hive-mcp-docs--function-signature (func)
  "Get the signature/arglist for FUNC."
  (when (fboundp func)
    (let ((arglist (help-function-arglist func t)))
      (if arglist
          (format "%S" arglist)
        "()"))))

(defun hive-mcp-docs--function-source-file (func)
  "Get the source file for FUNC."
  (when (fboundp func)
    (let ((file (find-lisp-object-file-name func 'defun)))
      (when file
        (if (stringp file)
            file
          (format "%s" file))))))

(defun hive-mcp-docs--symbol-type (symbol)
  "Determine the type of SYMBOL."
  (cond
   ((and (fboundp symbol) (commandp symbol)) "interactive command")
   ((and (fboundp symbol) (macrop (symbol-function symbol))) "macro")
   ((and (fboundp symbol) (subrp (symbol-function symbol))) "built-in function")
   ((fboundp symbol) "function")
   ((and (boundp symbol) (custom-variable-p symbol)) "customizable variable")
   ((boundp symbol) "variable")
   ((facep symbol) "face")
   (t "symbol")))

;;; MCP Tool Functions:

;;;###autoload
(defun hive-mcp-docs-describe-function (function-name)
  "Get documentation for FUNCTION-NAME.
Returns a plist with :name, :type, :signature, :docstring, :file."
  (let* ((sym (if (symbolp function-name)
                  function-name
                (intern-soft function-name))))
    (if (and sym (fboundp sym))
        (list :name (symbol-name sym)
              :type (hive-mcp-docs--symbol-type sym)
              :signature (hive-mcp-docs--function-signature sym)
              :docstring (or (hive-mcp-docs--get-docstring sym)
                             "No documentation available.")
              :file (when hive-mcp-docs-include-source-location
                      (hive-mcp-docs--function-source-file sym)))
      (list :error (format "Function not found: %s" function-name)))))

;;;###autoload
(defun hive-mcp-docs-describe-variable (variable-name)
  "Get documentation for VARIABLE-NAME.
Returns a plist with :name, :type, :value, :docstring, :file."
  (let* ((sym (if (symbolp variable-name)
                  variable-name
                (intern-soft variable-name))))
    (if (and sym (boundp sym))
        (let ((val (symbol-value sym)))
          (list :name (symbol-name sym)
                :type (hive-mcp-docs--symbol-type sym)
                :value (if (> (length (format "%S" val)) 500)
                           (format "%s... (truncated)"
                                   (substring (format "%S" val) 0 500))
                         (format "%S" val))
                :docstring (or (documentation-property sym 'variable-documentation t)
                               "No documentation available.")
                :file (when hive-mcp-docs-include-source-location
                        (find-lisp-object-file-name sym 'defvar))))
      (list :error (format "Variable not found: %s" variable-name)))))

;;;###autoload
(defun hive-mcp-docs-apropos (pattern &optional type)
  "Search for symbols matching PATTERN.
TYPE can be: nil (all), \"function\", \"variable\", \"command\", \"face\".
Returns a list of matching symbols with basic info."
  (let* ((regexp (if (string-match-p "\\\\|\\|\\[\\|\\*\\|\\+" pattern)
                     pattern
                   (regexp-quote pattern)))
         (all-symbols (apropos-internal regexp))
         (filtered (cl-remove-if-not
                    (lambda (sym)
                      (pcase type
                        ("function" (fboundp sym))
                        ("variable" (boundp sym))
                        ("command" (commandp sym))
                        ("face" (facep sym))
                        (_ t)))
                    all-symbols))
         (limited (seq-take filtered hive-mcp-docs-max-results)))
    (list :pattern pattern
          :type (or type "all")
          :total-matches (length filtered)
          :returned (length limited)
          :symbols
          (mapcar (lambda (sym)
                    (list :name (symbol-name sym)
                          :type (hive-mcp-docs--symbol-type sym)
                          :docstring-preview
                          (when-let* ((doc (hive-mcp-docs--get-docstring sym)))
                            (if (> (length doc) 100)
                                (concat (substring doc 0 100) "...")
                              doc))))
                  limited))))

;;;###autoload
(defun hive-mcp-docs-package-functions (package-or-prefix)
  "List all functions defined by PACKAGE-OR-PREFIX.
PACKAGE-OR-PREFIX can be a feature name or a symbol prefix string.
Returns list of functions with signatures."
  (let* ((prefix (if (symbolp package-or-prefix)
                     (symbol-name package-or-prefix)
                   package-or-prefix))
         (prefix-regexp (concat "^" (regexp-quote prefix)))
         (functions (cl-remove-if-not
                     (lambda (sym)
                       (and (fboundp sym)
                            (string-match-p prefix-regexp (symbol-name sym))))
                     (apropos-internal prefix-regexp #'fboundp)))
         (limited (seq-take functions hive-mcp-docs-max-results)))
    (list :prefix prefix
          :total-functions (length functions)
          :returned (length limited)
          :functions
          (mapcar (lambda (sym)
                    (list :name (symbol-name sym)
                          :type (hive-mcp-docs--symbol-type sym)
                          :signature (hive-mcp-docs--function-signature sym)
                          :interactive (commandp sym)))
                  limited))))

;;;###autoload
(defun hive-mcp-docs-find-keybindings (command)
  "Find all keybindings for COMMAND.
Returns list of key sequences that invoke the command."
  (let* ((sym (if (symbolp command)
                  command
                (intern-soft command))))
    (if (and sym (commandp sym))
        (let ((keys (where-is-internal sym)))
          (list :command (symbol-name sym)
                :docstring-preview
                (when-let* ((doc (documentation sym t)))
                  (if (> (length doc) 200)
                      (concat (substring doc 0 200) "...")
                    doc))
                :bindings
                (mapcar (lambda (key)
                          (list :key (key-description key)))
                        keys)
                :binding-count (length keys)))
      (list :error (format "Command not found: %s" command)))))

;;;###autoload
(defun hive-mcp-docs-info-lookup (symbol-name)
  "Look up SYMBOL-NAME in Info manuals.
Returns relevant Info node content if found."
  (let ((sym (if (symbolp symbol-name)
                 symbol-name
               (intern-soft symbol-name))))
    (if (and (featurep 'info-look) sym)
        (condition-case err
            (let* ((info-lookup-mode 'emacs-lisp-mode)
                   (info (info-lookup-symbol sym)))
              (list :symbol (symbol-name sym)
                    :found t
                    :note "Info node opened in Emacs"))
          (error
           (list :symbol (format "%s" symbol-name)
                 :found nil
                 :note "No Info documentation found")))
      (list :symbol (format "%s" symbol-name)
            :found nil
            :note "info-look not available or symbol invalid"))))

;;;###autoload
(defun hive-mcp-docs-package-commentary (file-or-feature)
  "Get the Commentary section from FILE-OR-FEATURE.
FILE-OR-FEATURE can be a filename or a feature symbol."
  (let* ((lib-file (cond
                    ((and (stringp file-or-feature)
                          (file-exists-p file-or-feature))
                     file-or-feature)
                    ((symbolp file-or-feature)
                     (locate-library (symbol-name file-or-feature)))
                    ((stringp file-or-feature)
                     (locate-library file-or-feature))
                    (t nil)))
         ;; Prefer .el source over .elc compiled
         (file (when lib-file
                 (if (string-suffix-p ".elc" lib-file)
                     (let ((src (concat (file-name-sans-extension lib-file) ".el")))
                       (if (file-exists-p src) src lib-file))
                   lib-file))))
    (if file
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((commentary-start (re-search-forward "^;;; Commentary:?\\s-*$" nil t))
                (commentary-end nil))
            (when commentary-start
              (setq commentary-end
                    (or (re-search-forward "^;;; \\(Code\\|History\\|Change ?Log\\):?\\s-*$" nil t)
                        (point-max))))
            (if commentary-start
                (let* ((raw (buffer-substring-no-properties
                             commentary-start
                             (or commentary-end (point-max))))
                       ;; Clean up comment prefixes
                       (cleaned (replace-regexp-in-string "^;;+ ?" "" raw)))
                  (list :file (file-name-nondirectory file)
                        :path file
                        :commentary (string-trim cleaned)))
              (list :file (file-name-nondirectory file)
                    :path file
                    :commentary nil
                    :note "No Commentary section found"))))
      (list :error (format "Could not locate: %s" file-or-feature)))))

;;;###autoload
(defun hive-mcp-docs-list-packages ()
  "List all loaded features (packages) with their files."
  (let ((features-info
         (mapcar (lambda (feat)
                   (list :name (symbol-name feat)
                         :file (when-let* ((file (locate-library (symbol-name feat))))
                                 (file-name-nondirectory file))))
                 features)))
    (list :total (length features)
          :features (seq-take features-info 100))))

;;; Interactive Commands:

;;;###autoload
(defun hive-mcp-docs-describe-at-point ()
  "Describe the symbol at point using MCP docs tools."
  (interactive)
  (let* ((sym (symbol-at-point))
         (result (cond
                  ((fboundp sym) (hive-mcp-docs-describe-function sym))
                  ((boundp sym) (hive-mcp-docs-describe-variable sym))
                  (t (list :error "No describable symbol at point")))))
    (message "%S" result)))

;;;###autoload
(defun hive-mcp-docs-apropos-interactive (pattern)
  "Interactive apropos search using MCP docs."
  (interactive "sApropos pattern: ")
  (let ((result (hive-mcp-docs-apropos pattern)))
    (with-current-buffer (get-buffer-create "*MCP Docs Apropos*")
      (erase-buffer)
      (insert (format "Apropos: %s\n" pattern))
      (insert (format "Found %d matches (showing %d)\n\n"
                      (plist-get result :total-matches)
                      (plist-get result :returned)))
      (dolist (sym (plist-get result :symbols))
        (insert (format "%-40s [%s]\n"
                        (plist-get sym :name)
                        (plist-get sym :type)))
        (when-let* ((doc (plist-get sym :docstring-preview)))
          (insert (format "    %s\n" doc))))
      (goto-char (point-min)))
    (display-buffer "*MCP Docs Apropos*")))

;;; Transient Menu:

;;;###autoload (autoload 'hive-mcp-docs-transient "hive-mcp-docs" nil t)
(transient-define-prefix hive-mcp-docs-transient ()
  "MCP documentation tools menu."
  ["hive-mcp Documentation Tools"
   ["Describe"
    ("f" "Describe function" describe-function)
    ("v" "Describe variable" describe-variable)
    ("." "Describe at point" hive-mcp-docs-describe-at-point)]
   ["Search"
    ("a" "Apropos" hive-mcp-docs-apropos-interactive)
    ("p" "Package functions" hive-mcp-docs-package-functions-interactive)
    ("k" "Find keybindings" hive-mcp-docs-find-keybindings-interactive)]
   ["Info"
    ("i" "Info lookup" info-lookup-symbol)
    ("c" "Package commentary" hive-mcp-docs-commentary-interactive)]])

(defun hive-mcp-docs-package-functions-interactive (prefix)
  "Interactive package functions lookup."
  (interactive "sPackage/prefix: ")
  (let ((result (hive-mcp-docs-package-functions prefix)))
    (with-current-buffer (get-buffer-create "*MCP Docs Package*")
      (erase-buffer)
      (insert (format "Functions for: %s\n" prefix))
      (insert (format "Found %d functions (showing %d)\n\n"
                      (plist-get result :total-functions)
                      (plist-get result :returned)))
      (dolist (fn (plist-get result :functions))
        (insert (format "%s%s %s\n"
                        (if (plist-get fn :interactive) "*" " ")
                        (plist-get fn :name)
                        (or (plist-get fn :signature) ""))))
      (insert "\n* = interactive command\n")
      (goto-char (point-min)))
    (display-buffer "*MCP Docs Package*")))

(defun hive-mcp-docs-find-keybindings-interactive (command)
  "Interactive keybinding lookup."
  (interactive "CCommand: ")
  (let ((result (hive-mcp-docs-find-keybindings command)))
    (if (plist-get result :error)
        (message "%s" (plist-get result :error))
      (message "%s is bound to: %s"
               (plist-get result :command)
               (mapconcat (lambda (b) (plist-get b :key))
                          (plist-get result :bindings)
                          ", ")))))

(defun hive-mcp-docs-commentary-interactive (feature)
  "Interactive commentary lookup."
  (interactive "SFeature: ")
  (let ((result (hive-mcp-docs-package-commentary feature)))
    (if (plist-get result :error)
        (message "%s" (plist-get result :error))
      (with-current-buffer (get-buffer-create "*MCP Docs Commentary*")
        (erase-buffer)
        (insert (format "Commentary for: %s\n" (plist-get result :file)))
        (insert (format "Path: %s\n\n" (plist-get result :path)))
        (insert (or (plist-get result :commentary)
                    (plist-get result :note)
                    "No commentary found"))
        (goto-char (point-min)))
      (display-buffer "*MCP Docs Commentary*"))))

;;; Addon Lifecycle:

(defun hive-mcp-docs--addon-init ()
  "Initialize docs addon."
  (message "hive-mcp-docs: initialized - documentation tools ready"))

(defun hive-mcp-docs--addon-shutdown ()
  "Shutdown docs addon."
  (message "hive-mcp-docs: shutdown"))

;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'docs
   :version "0.1.0"
   :description "Emacs documentation lookup tools for MCP"
   :requires '()
   :provides '(hive-mcp-docs-transient
               hive-mcp-docs-describe-function
               hive-mcp-docs-describe-variable
               hive-mcp-docs-apropos
               hive-mcp-docs-package-functions
               hive-mcp-docs-find-keybindings)
   :init #'hive-mcp-docs--addon-init
   :shutdown #'hive-mcp-docs--addon-shutdown))

(provide 'hive-mcp-docs)
;;; hive-mcp-docs.el ends here
