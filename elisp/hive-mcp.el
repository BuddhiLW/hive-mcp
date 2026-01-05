;;; hive-mcp.el --- Emacs integration for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho

;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Maintainer: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.5.1
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; Keywords: tools, ai, convenience
;; URL: https://github.com/BuddhiLW/hive-mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; hive-mcp provides an Emacs-side companion to the hive-mcp Clojure server.
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
;;   (require 'hive-mcp)
;;   (hive-mcp-mode 1)
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
;;   (hive-mcp-api-get-context)
;;   (hive-mcp-api-memory-add "note" "content" '("tag1"))
;;   (hive-mcp-api-memory-query "note")
;;   (hive-mcp-api-run-workflow "workflow-name")
;;

;;; Code:

(require 'json)
(require 'project)

;;; Customization

(defgroup hive-mcp nil
  "Emacs integration for Claude MCP."
  :group 'tools
  :prefix "hive-mcp-")

(defcustom hive-mcp-auto-initialize t
  "Automatically initialize hive-mcp on mode enable."
  :type 'boolean
  :group 'hive-mcp)

(defcustom hive-mcp-load-builtin-workflows t
  "Load built-in example workflows on initialization."
  :type 'boolean
  :group 'hive-mcp)

(defcustom hive-mcp-auto-enable nil
  "Automatically enable `hive-mcp-mode' when Emacs starts.
Set this to t in your config to have hive-mcp ready on startup."
  :type 'boolean
  :group 'hive-mcp)

(defcustom hive-mcp-setup-addons t
  "Set up addon system during initialization.
When non-nil, loads addons from `hive-mcp-addon-always-load'
and enables auto-loading for feature-triggered addons."
  :type 'boolean
  :group 'hive-mcp)

;;; Require submodules

(require 'hive-mcp-memory)
(require 'hive-mcp-kanban)
(require 'hive-mcp-context)
(require 'hive-mcp-triggers)
(require 'hive-mcp-workflows)
(require 'hive-mcp-api)
(require 'hive-mcp-addons)
(require 'hive-mcp-channel)
(require 'hive-mcp-hivemind)

;; Forward declaration for byte-compiler
(declare-function hive-mcp-addons-setup "hive-mcp-addons")

;; Optional: transient menus
(when (require 'transient nil t)
  (require 'hive-mcp-transient))

;;; Initialization

(defvar hive-mcp--initialized nil
  "Non-nil if hive-mcp has been initialized.")

;;;###autoload
(defun hive-mcp-initialize ()
  "Initialize hive-mcp system."
  (interactive)
  (unless hive-mcp--initialized
    ;; Initialize memory system
    (hive-mcp-memory-init)

    ;; Load saved workflows
    (hive-mcp-workflow--load)

    ;; Register built-in workflows
    (when hive-mcp-load-builtin-workflows
      (hive-mcp-workflow--register-builtins))

    ;; Set up keybindings
    (hive-mcp-setup-keybindings)

    ;; Set up hooks
    (hive-mcp-setup-hooks)

    ;; Set up addons (always-load + auto-load triggers)
    (when hive-mcp-setup-addons
      (hive-mcp-addons-setup))

    ;; Connect to bidirectional channel (if server is running)
    (when (fboundp 'hive-mcp-channel-setup-auto-connect)
      (hive-mcp-channel-setup-auto-connect))

    (setq hive-mcp--initialized t)
    (message "Emacs-mcp initialized (C-c m for menu)")))

(defun hive-mcp-reset ()
  "Reset hive-mcp state (for debugging)."
  (interactive)
  (setq hive-mcp--initialized nil)
  (clrhash hive-mcp-memory--cache)
  (clrhash hive-mcp-workflow-registry)
  (clrhash hive-mcp-trigger-registry)
  (message "Emacs-mcp reset"))

;;; Minor Mode

;;;###autoload
(define-minor-mode hive-mcp-mode
  "Minor mode for hive-mcp integration with Claude.

Provides persistent memory, context gathering, workflows, and
keybindings for AI-assisted development.

\\{hive-mcp-command-map}"
  :global t
  :lighter " MCP"
  :keymap (let ((map (make-sparse-keymap)))
            (when hive-mcp-keymap-prefix
              (define-key map hive-mcp-keymap-prefix hive-mcp-command-map))
            map)
  (if hive-mcp-mode
      (when hive-mcp-auto-initialize
        (hive-mcp-initialize))
    (message "Emacs-mcp mode disabled")))

;;; Convenience aliases for common operations

(defalias 'hive-mcp-note #'hive-mcp-add-note-interactive
  "Add a note to project memory.")

(defalias 'hive-mcp-snippet #'hive-mcp-add-snippet-interactive
  "Save region as a snippet.")

(defalias 'hive-mcp-context #'hive-mcp-show-context
  "Show current context.")

(defalias 'hive-mcp-memory #'hive-mcp-show-memory
  "Show project memory.")

(defalias 'hive-mcp-workflow #'hive-mcp-workflow-run-interactive
  "Run a workflow.")

;;; Auto-load on init (optional)
;; Users who want auto-enable should add to their init file:
;;   (setq hive-mcp-auto-enable t)
;;   (add-hook 'after-init-hook #'hive-mcp--maybe-auto-enable)

(defun hive-mcp--maybe-auto-enable ()
  "Maybe enable `hive-mcp-mode' automatically.
Controlled by `hive-mcp-auto-enable'.
To use, add to your init file:
  (setq hive-mcp-auto-enable t)
  (add-hook \\='after-init-hook #\\='hive-mcp--maybe-auto-enable)"
  (when hive-mcp-auto-enable
    (hive-mcp-mode 1)))

(provide 'hive-mcp)
;;; hive-mcp.el ends here
