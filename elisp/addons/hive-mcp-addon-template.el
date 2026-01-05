;;; hive-mcp-addon-template.el --- Template for hive-mcp addons -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your@email.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Template for creating hive-mcp addons.
;;
;; OPTIONAL DEPENDENCIES:
;; - Replace 'target-package' with the package you're integrating
;; - Add your package's dependencies here
;;
;; Copy this file and rename to hive-mcp-{yourpackage}.el
;;
;; To create your own addon:
;; 1. Copy this file to addons/hive-mcp-YOUR-ADDON.el
;; 2. Replace "template" with your addon name throughout
;; 3. Implement your integration functions
;; 4. Register with hive-mcp-addon-register
;;
;; Your addon will be auto-discovered if placed in the addons/ directory.
;; Users can load it with: (hive-mcp-addon-load 'YOUR-ADDON)
;;
;; For auto-loading when a trigger package loads, add an entry to
;; `hive-mcp-addon-auto-load-list' in your config:
;;   (add-to-list 'hive-mcp-addon-auto-load-list '(YOUR-ADDON . trigger-package))

;;; Code:

(require 'hive-mcp-api)

;; Soft dependency - don't error if target package isn't installed
(declare-function target-package-function "target-package")

;;;; Customization:

(defgroup hive-mcp-template nil
  "Integration between hive-mcp and target-package."
  :group 'hive-mcp
  :prefix "hive-mcp-template-")

(defcustom hive-mcp-template-auto-context nil
  "When non-nil, automatically include MCP context."
  :type 'boolean
  :group 'hive-mcp-template)

;;;; Internal:

(defvar hive-mcp-template--initialized nil
  "Whether the addon has been initialized.")

(defun hive-mcp-template--ensure-api ()
  "Ensure hive-mcp-api is available."
  (unless (featurep 'hive-mcp-api)
    (require 'hive-mcp-api nil t))
  (featurep 'hive-mcp-api))

;;;; Public API:

;;;###autoload
(defun hive-mcp-template-get-context ()
  "Get MCP context formatted for target-package."
  (when (hive-mcp-template--ensure-api)
    (hive-mcp-api-get-context)))

;;;###autoload
(defun hive-mcp-template-save-to-memory (content type &optional tags)
  "Save CONTENT to hive-mcp memory as TYPE with optional TAGS."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Content: "))
         (completing-read "Type: " '("note" "snippet" "decision") nil t)
         (split-string (read-string "Tags (comma-separated): ") "," t " ")))
  (when (hive-mcp-template--ensure-api)
    (hive-mcp-api-memory-add type content tags)
    (message "Saved to memory as %s" type)))

;;;###autoload
(defun hive-mcp-template-query-memory (type &optional limit)
  "Query hive-mcp memory for entries of TYPE, return up to LIMIT entries."
  (when (hive-mcp-template--ensure-api)
    (hive-mcp-api-memory-query type nil (or limit 10))))

;;;; Integration hooks (customize these for your target package):

;; Example: Advice to add context to a target function
;; (defun hive-mcp-template--add-context-advice (orig-fun &rest args)
;;   "Advice to inject MCP context."
;;   (if hive-mcp-template-auto-context
;;       (let ((ctx (hive-mcp-template-get-context)))
;;         ;; Modify args or behavior based on context
;;         (apply orig-fun args))
;;     (apply orig-fun args)))

;; Example: Hook function
;; (defun hive-mcp-template--on-some-hook ()
;;   "Hook to run when something happens in target package."
;;   (when hive-mcp-template-auto-context
;;     (hive-mcp-template-save-to-memory
;;      (buffer-substring-no-properties (point-min) (point-max))
;;      "note"
;;      '("auto-logged"))))

;;;; Minor mode (optional):

;;;###autoload
(define-minor-mode hive-mcp-template-mode
  "Minor mode for hive-mcp integration with target-package."
  :init-value nil
  :lighter " MCP-T"
  :global t
  :group 'hive-mcp-template
  (if hive-mcp-template-mode
      (progn
        ;; Enable: add advice, hooks, keybindings
        ;; (advice-add 'target-function :around #'hive-mcp-template--add-context-advice)
        (setq hive-mcp-template--initialized t)
        (message "hive-mcp-template enabled"))
    ;; Disable: remove advice, hooks
    ;; (advice-remove 'target-function #'hive-mcp-template--add-context-advice)
    (message "hive-mcp-template disabled")))

;;;; Transient menu (optional, requires transient.el):

;; (transient-define-prefix hive-mcp-template-transient ()
;;   "MCP integration menu for target-package."
;;   ["hive-mcp + target-package"
;;    ["Context"
;;     ("c" "Show context" hive-mcp-template-show-context)
;;     ("s" "Save to memory" hive-mcp-template-save-to-memory)]
;;    ["Query"
;;     ("q" "Query memory" hive-mcp-template-query-memory)]])

;;;; Addon Lifecycle Functions (NEW - recommended approach):

;; These functions are called automatically by the addon system:

(defun hive-mcp-template--addon-init ()
  "Synchronous init - runs immediately after loading.
Use for lightweight setup that must complete before use."
  (require 'hive-mcp-api nil t)
  ;; Example: setup keybindings, hooks
  (message "hive-mcp-template: initialized"))

(defun hive-mcp-template--addon-async-init ()
  "Asynchronous init - runs in background after loading.
Use for long-running startup (servers, subprocesses).
Should return a process object if starting a subprocess."
  ;; Example: start a server
  ;; (when hive-mcp-template-auto-start
  ;;   (start-process "my-server" "*my-server*" "my-command"))
  nil)

(defun hive-mcp-template--addon-shutdown ()
  "Shutdown - runs when addon is unloaded.
Use for cleanup (stopping servers, saving state)."
  ;; Example: stop server, save state
  (message "hive-mcp-template: shutdown complete"))

;;;; Registration:

;; Register this addon with hive-mcp
(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'addon-template
   :version "0.1.0"
   :description "Template addon for hive-mcp"
   :requires '(hive-mcp-api)
   :provides '(hive-mcp-template-mode)
   ;; Lifecycle hooks (all optional):
   :init #'hive-mcp-template--addon-init
   :async-init #'hive-mcp-template--addon-async-init
   :shutdown #'hive-mcp-template--addon-shutdown))

(provide 'hive-mcp-addon-template)
;;; hive-mcp-addon-template.el ends here
