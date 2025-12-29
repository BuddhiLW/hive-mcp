;;; emacs-mcp.el --- Emacs integration for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho

;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; Keywords: tools, ai, convenience
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; emacs-mcp provides an Emacs-side companion to the emacs-mcp Clojure server.
;; It offers:
;; - Persistent memory (notes, snippets, conventions, decisions, conversations)
;; - Context gathering (buffer, region, project, git)
;; - Trigger system (hooks, keybindings, automations)
;; - Workflow orchestration (multi-step user-defined automations)
;;
;; While Claude controls Emacs via the Clojure server's eval_elisp tool,
;; this package provides the functions Claude calls and the UI for users.
;;
;; Quick start:
;;   (require 'emacs-mcp)
;;   (emacs-mcp-mode 1)
;;
;; Keybindings (default prefix: C-c m):
;;   C-c m m  - Open main menu (transient)
;;   C-c m n  - Add note to project memory
;;   C-c m s  - Save region as snippet
;;   C-c m c  - Add project convention
;;   C-c m d  - Record architecture decision
;;   C-c m l  - Browse project memory
;;   C-c m w  - Run workflow
;;   C-c m h  - Show conversation history
;;   C-c m x  - Show current context (debug)
;;
;; API for Claude (via eval_elisp):
;;   (emacs-mcp-api-get-context)
;;   (emacs-mcp-api-memory-add "note" "content" '("tag1"))
;;   (emacs-mcp-api-memory-query "note")
;;   (emacs-mcp-api-run-workflow "workflow-name")
;;

;;; Code:

(require 'json)
(require 'project)

;;; Customization

(defgroup emacs-mcp nil
  "Emacs integration for Claude MCP."
  :group 'tools
  :prefix "emacs-mcp-")

(defcustom emacs-mcp-auto-initialize t
  "Automatically initialize emacs-mcp on mode enable."
  :type 'boolean
  :group 'emacs-mcp)

(defcustom emacs-mcp-load-builtin-workflows t
  "Load built-in example workflows on initialization."
  :type 'boolean
  :group 'emacs-mcp)

(defcustom emacs-mcp-auto-enable nil
  "Automatically enable `emacs-mcp-mode' when Emacs starts.
Set this to t in your config to have emacs-mcp ready on startup."
  :type 'boolean
  :group 'emacs-mcp)

(defcustom emacs-mcp-setup-addons t
  "Set up addon system during initialization.
When non-nil, loads addons from `emacs-mcp-addon-always-load'
and enables auto-loading for feature-triggered addons."
  :type 'boolean
  :group 'emacs-mcp)

;;; Require submodules

(require 'emacs-mcp-memory)
(require 'emacs-mcp-context)
(require 'emacs-mcp-triggers)
(require 'emacs-mcp-workflows)
(require 'emacs-mcp-api)
(require 'emacs-mcp-addons)

;; Forward declaration for byte-compiler
(declare-function emacs-mcp-addons-setup "emacs-mcp-addons")

;; Optional: transient menus
(when (require 'transient nil t)
  (require 'emacs-mcp-transient))

;;; Initialization

(defvar emacs-mcp--initialized nil
  "Non-nil if emacs-mcp has been initialized.")

;;;###autoload
(defun emacs-mcp-initialize ()
  "Initialize emacs-mcp system."
  (interactive)
  (unless emacs-mcp--initialized
    ;; Initialize memory system
    (emacs-mcp-memory-init)

    ;; Load saved workflows
    (emacs-mcp-workflow--load)

    ;; Register built-in workflows
    (when emacs-mcp-load-builtin-workflows
      (emacs-mcp-workflow--register-builtins))

    ;; Set up keybindings
    (emacs-mcp-setup-keybindings)

    ;; Set up hooks
    (emacs-mcp-setup-hooks)

    ;; Set up addons (always-load + auto-load triggers)
    (when emacs-mcp-setup-addons
      (emacs-mcp-addons-setup))

    (setq emacs-mcp--initialized t)
    (message "emacs-mcp initialized (C-c m for menu)")))

(defun emacs-mcp-reset ()
  "Reset emacs-mcp state (for debugging)."
  (interactive)
  (setq emacs-mcp--initialized nil)
  (clrhash emacs-mcp-memory--cache)
  (clrhash emacs-mcp-workflow-registry)
  (clrhash emacs-mcp-trigger-registry)
  (message "emacs-mcp reset"))

;;; Minor Mode

;;;###autoload
(define-minor-mode emacs-mcp-mode
  "Minor mode for emacs-mcp integration with Claude.

Provides persistent memory, context gathering, workflows, and
keybindings for AI-assisted development.

\\{emacs-mcp-command-map}"
  :global t
  :lighter " MCP"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map emacs-mcp-keymap-prefix emacs-mcp-command-map)
            map)
  (if emacs-mcp-mode
      (when emacs-mcp-auto-initialize
        (emacs-mcp-initialize))
    (message "emacs-mcp mode disabled")))

;;; Convenience aliases for common operations

(defalias 'emacs-mcp-note 'emacs-mcp-add-note-interactive
  "Add a note to project memory.")

(defalias 'emacs-mcp-snippet 'emacs-mcp-add-snippet-interactive
  "Save region as a snippet.")

(defalias 'emacs-mcp-context 'emacs-mcp-show-context
  "Show current context.")

(defalias 'emacs-mcp-memory 'emacs-mcp-show-memory
  "Show project memory.")

(defalias 'emacs-mcp-workflow 'emacs-mcp-workflow-run-interactive
  "Run a workflow.")

;;; Auto-load on init (optional)

(defun emacs-mcp--maybe-auto-enable ()
  "Maybe enable `emacs-mcp-mode' automatically.
Controlled by `emacs-mcp-auto-enable'."
  (when emacs-mcp-auto-enable
    (emacs-mcp-mode 1)))

;; Set up auto-enable hook (runs if emacs-mcp-auto-enable is t)
(add-hook 'after-init-hook #'emacs-mcp--maybe-auto-enable)

(provide 'emacs-mcp)
;;; emacs-mcp.el ends here
