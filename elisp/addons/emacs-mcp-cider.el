;;; emacs-mcp-cider.el --- Integrate CIDER/nREPL with emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cider "1.0"))
;; Keywords: tools, clojure, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates CIDER (Clojure IDE for Emacs) with emacs-mcp.
;;
;; Features:
;; - Add Clojure namespace/project context to MCP
;; - Save REPL results to memory
;; - Query memory for previous solutions
;; - Auto-log REPL sessions
;;
;; Usage:
;;   (emacs-mcp-addon-load 'cider)
;;   (emacs-mcp-cider-mode 1)
;;
;; Or enable auto-loading when CIDER loads:
;;   (emacs-mcp-addons-auto-load)

;;; Code:

(require 'emacs-mcp-api)

;; Soft dependencies
(declare-function cider-current-ns "cider-client")
(declare-function cider-current-connection "cider-client")
(declare-function cider-nrepl-eval-sync "cider-nrepl")
(declare-function cider-last-sexp "cider-eval")
(declare-function cider-interactive-eval "cider-eval")

;;;; Customization

(defgroup emacs-mcp-cider nil
  "Integration between CIDER and emacs-mcp."
  :group 'emacs-mcp
  :group 'cider
  :prefix "emacs-mcp-cider-")

(defcustom emacs-mcp-cider-auto-log-results nil
  "When non-nil, automatically log REPL results to memory."
  :type 'boolean
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-log-threshold 100
  "Minimum result length to auto-log (avoids logging trivial results)."
  :type 'integer
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-auto-start-nrepl nil
  "When non-nil, automatically start nREPL server on addon load.
The server starts asynchronously and does not block Emacs startup."
  :type 'boolean
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-auto-connect t
  "When non-nil, automatically connect CIDER when nREPL is available.
Works with both auto-started and externally started nREPL servers."
  :type 'boolean
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-nrepl-port 7910
  "Default port for nREPL server."
  :type 'integer
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-project-dir nil
  "Project directory for starting nREPL.
If nil, uses the current project root or `default-directory'."
  :type '(choice (const nil) directory)
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-connect-retry-interval 1.0
  "Seconds between connection retry attempts."
  :type 'number
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-connect-max-retries 30
  "Maximum number of connection retry attempts (0 = unlimited)."
  :type 'integer
  :group 'emacs-mcp-cider)

;;;; Internal

(defvar emacs-mcp-cider--last-eval nil
  "Last evaluated expression and result for potential saving.")

(defvar emacs-mcp-cider--nrepl-process nil
  "Process object for auto-started nREPL server.")

(defvar emacs-mcp-cider--connect-timer nil
  "Timer for auto-connect retry attempts.")

(defvar emacs-mcp-cider--connect-attempts 0
  "Number of connection attempts made.")

;;;; Async nREPL Start & Auto-Connect

(defun emacs-mcp-cider--project-dir ()
  "Get the project directory for nREPL."
  (or emacs-mcp-cider-project-dir
      (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun emacs-mcp-cider--port-open-p (port)
  "Check if PORT is accepting connections."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "nrepl-check"
                   :host "localhost"
                   :service port
                   :nowait nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun emacs-mcp-cider--start-nrepl-async ()
  "Start nREPL server asynchronously in background.
Does not block Emacs startup."
  (let* ((default-directory (emacs-mcp-cider--project-dir))
         (port (number-to-string emacs-mcp-cider-nrepl-port))
         (buf-name "*nREPL-server*"))
    (message "emacs-mcp-cider: Starting nREPL on port %s..." port)
    (setq emacs-mcp-cider--nrepl-process
          (start-process "nrepl-server" buf-name
                         "clojure" "-M:nrepl"))))

(defun emacs-mcp-cider--try-connect ()
  "Try to connect CIDER to nREPL. Returns t if successful."
  (when (and (featurep 'cider)
             (fboundp 'cider-connect-clj)
             (not (cider-connected-p))
             (emacs-mcp-cider--port-open-p emacs-mcp-cider-nrepl-port))
    (condition-case err
        (progn
          (cider-connect-clj (list :host "localhost"
                                   :port emacs-mcp-cider-nrepl-port))
          (message "emacs-mcp-cider: Connected to nREPL on port %d"
                   emacs-mcp-cider-nrepl-port)
          t)
      (error
       (message "emacs-mcp-cider: Connection failed: %s" (error-message-string err))
       nil))))

(defun emacs-mcp-cider--auto-connect-tick ()
  "Timer callback for auto-connect attempts."
  (setq emacs-mcp-cider--connect-attempts (1+ emacs-mcp-cider--connect-attempts))
  (cond
   ;; Already connected - stop trying
   ((and (featurep 'cider) (cider-connected-p))
    (emacs-mcp-cider--stop-auto-connect)
    (message "emacs-mcp-cider: Already connected"))
   ;; Successfully connected
   ((emacs-mcp-cider--try-connect)
    (emacs-mcp-cider--stop-auto-connect))
   ;; Max retries reached
   ((and (> emacs-mcp-cider-connect-max-retries 0)
         (>= emacs-mcp-cider--connect-attempts emacs-mcp-cider-connect-max-retries))
    (emacs-mcp-cider--stop-auto-connect)
    (message "emacs-mcp-cider: Auto-connect gave up after %d attempts"
             emacs-mcp-cider--connect-attempts))))

(defun emacs-mcp-cider--start-auto-connect ()
  "Start the auto-connect timer."
  (unless emacs-mcp-cider--connect-timer
    (setq emacs-mcp-cider--connect-attempts 0)
    (setq emacs-mcp-cider--connect-timer
          (run-with-timer emacs-mcp-cider-connect-retry-interval
                          emacs-mcp-cider-connect-retry-interval
                          #'emacs-mcp-cider--auto-connect-tick))
    (message "emacs-mcp-cider: Auto-connect started (checking every %.1fs)"
             emacs-mcp-cider-connect-retry-interval)))

(defun emacs-mcp-cider--stop-auto-connect ()
  "Stop the auto-connect timer."
  (when emacs-mcp-cider--connect-timer
    (cancel-timer emacs-mcp-cider--connect-timer)
    (setq emacs-mcp-cider--connect-timer nil)))

;;;###autoload
(defun emacs-mcp-cider-start-nrepl ()
  "Start nREPL server and auto-connect CIDER.
Non-blocking - runs in background."
  (interactive)
  (emacs-mcp-cider--start-nrepl-async)
  (when emacs-mcp-cider-auto-connect
    (emacs-mcp-cider--start-auto-connect)))

;;;###autoload
(defun emacs-mcp-cider-auto-connect ()
  "Start polling for nREPL and connect when available.
Use this when nREPL is started externally."
  (interactive)
  (emacs-mcp-cider--start-auto-connect))

;;;###autoload
(defun emacs-mcp-cider-stop-nrepl ()
  "Stop the auto-started nREPL server."
  (interactive)
  (emacs-mcp-cider--stop-auto-connect)
  (when (and emacs-mcp-cider--nrepl-process
             (process-live-p emacs-mcp-cider--nrepl-process))
    (kill-process emacs-mcp-cider--nrepl-process)
    (setq emacs-mcp-cider--nrepl-process nil)
    (message "emacs-mcp-cider: nREPL server stopped")))

;;;; Context Functions

(defun emacs-mcp-cider--get-clojure-context ()
  "Get Clojure-specific context for MCP."
  (when (and (featurep 'cider)
             (fboundp 'cider-current-connection)
             (cider-current-connection))
    (list :clojure
          (list :namespace (when (fboundp 'cider-current-ns)
                             (cider-current-ns))
                :connected t
                :repl-type (when (boundp 'cider-repl-type)
                             cider-repl-type)))))

;;;; Memory Integration

;;;###autoload
(defun emacs-mcp-cider-save-last-result ()
  "Save the last REPL result to memory."
  (interactive)
  (if emacs-mcp-cider--last-eval
      (let* ((expr (plist-get emacs-mcp-cider--last-eval :expr))
             (result (plist-get emacs-mcp-cider--last-eval :result))
             (ns (plist-get emacs-mcp-cider--last-eval :ns))
             (tags (split-string
                    (read-string "Tags (comma-separated): " "clojure,repl")
                    "," t " "))
             (content (format "Namespace: %s\nExpression: %s\nResult: %s"
                              ns expr result)))
        (emacs-mcp-api-memory-add "snippet" content tags)
        (message "Saved REPL result to memory"))
    (message "No recent REPL result to save")))

;;;###autoload
(defun emacs-mcp-cider-save-defun ()
  "Save the current function definition to memory."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'defun))
         (defun-text (when bounds
                       (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (ns (when (fboundp 'cider-current-ns) (cider-current-ns)))
         (tags (split-string
                (read-string "Tags (comma-separated): " "clojure,function")
                "," t " ")))
    (if defun-text
        (let ((content (format "Namespace: %s\n\n%s" ns defun-text)))
          (emacs-mcp-api-memory-add "snippet" content tags)
          (message "Saved function to memory"))
      (message "No function at point"))))

;;;###autoload
(defun emacs-mcp-cider-query-solutions (query)
  "Query memory for Clojure solutions matching QUERY."
  (interactive "sSearch for: ")
  (let* ((results (emacs-mcp-api-memory-query "snippet" '("clojure") 20))
         (buf (get-buffer-create "*MCP Clojure Solutions*")))
    (with-current-buffer buf
      (erase-buffer)
      (clojure-mode)
      (insert ";; === Clojure Solutions from Memory ===\n\n")
      (if (= (length results) 0)
          (insert ";; No solutions found\n")
        (dotimes (i (length results))
          (let* ((entry (aref results i))
                 (content (alist-get 'content entry))
                 (tags (alist-get 'tags entry)))
            (insert (format ";; --- Entry %d [%s] ---\n"
                            (1+ i)
                            (mapconcat #'identity tags ", ")))
            (insert content)
            (insert "\n\n"))))
      (goto-char (point-min)))
    (display-buffer buf)))

;;;; Eval with Context

;;;###autoload
(defun emacs-mcp-cider-eval-with-context ()
  "Evaluate expression with MCP context injected as comment."
  (interactive)
  (when (fboundp 'cider-interactive-eval)
    (let* ((ctx (emacs-mcp-api-get-context))
           (ctx-comment (format ";; Context: %s @ %s\n"
                                (plist-get (plist-get ctx :buffer) :name)
                                (plist-get (plist-get ctx :project) :name)))
           (expr (when (fboundp 'cider-last-sexp) (cider-last-sexp))))
      ;; Store for potential saving
      (setq emacs-mcp-cider--last-eval
            (list :expr expr
                  :ns (when (fboundp 'cider-current-ns) (cider-current-ns))
                  :context ctx))
      (cider-interactive-eval expr))))

;;;; Advice for auto-logging

(defun emacs-mcp-cider--after-eval-advice (orig-fun &rest args)
  "Advice to capture eval results for potential saving."
  (let ((result (apply orig-fun args)))
    (when (and emacs-mcp-cider-auto-log-results
               result
               (> (length (format "%s" result)) emacs-mcp-cider-log-threshold))
      (setq emacs-mcp-cider--last-eval
            (list :expr (car args)
                  :result result
                  :ns (when (fboundp 'cider-current-ns) (cider-current-ns)))))
    result))

;;;; Transient Menu

;;;###autoload (autoload 'emacs-mcp-cider-transient "emacs-mcp-cider" nil t)
(transient-define-prefix emacs-mcp-cider-transient ()
  "MCP integration menu for CIDER."
  ["emacs-mcp + CIDER"
   ["nREPL Server"
    ("n" "Start nREPL (async)" emacs-mcp-cider-start-nrepl)
    ("N" "Stop nREPL" emacs-mcp-cider-stop-nrepl)
    ("c" "Auto-connect" emacs-mcp-cider-auto-connect)]
   ["Evaluate"
    ("e" "Eval with context" emacs-mcp-cider-eval-with-context)]
   ["Memory"
    ("s" "Save last result" emacs-mcp-cider-save-last-result)
    ("d" "Save defun" emacs-mcp-cider-save-defun)
    ("q" "Query solutions" emacs-mcp-cider-query-solutions)]
   ["Settings"
    ("L" "Toggle auto-log" emacs-mcp-cider-toggle-auto-log)]])

(defun emacs-mcp-cider-toggle-auto-log ()
  "Toggle automatic logging of REPL results."
  (interactive)
  (setq emacs-mcp-cider-auto-log-results (not emacs-mcp-cider-auto-log-results))
  (message "Auto-log %s" (if emacs-mcp-cider-auto-log-results "enabled" "disabled")))

;;;; MCP Tool API Functions
;; These functions are called by the emacs-mcp Clojure server as MCP tools

;;;###autoload
(defun emacs-mcp-cider-eval-silent (code)
  "Evaluate CODE via CIDER silently, return result.
Fast evaluation without REPL buffer output."
  (if (and (featurep 'cider) (cider-connected-p))
      (let ((result (cider-nrepl-sync-request:eval code)))
        (or (nrepl-dict-get result "value")
            (nrepl-dict-get result "err")
            (nrepl-dict-get result "out")))
    (error "CIDER not connected")))

;;;###autoload
(defun emacs-mcp-cider-eval-explicit (code)
  "Evaluate CODE via CIDER interactively.
Shows output in REPL buffer for collaborative debugging."
  (if (and (featurep 'cider) (cider-connected-p))
      (progn
        (cider-interactive-eval code)
        (format "Sent to REPL: %s" (truncate-string-to-width code 50 nil nil "...")))
    (error "CIDER not connected")))

;;;###autoload
(defun emacs-mcp-cider-status ()
  "Return CIDER connection status as JSON-compatible plist."
  (list :connected (and (featurep 'cider) (cider-connected-p))
        :repl-buffer (when (and (featurep 'cider) (cider-connected-p))
                       (buffer-name (car (cider-repl-buffers))))
        :namespace (when (and (featurep 'cider) (cider-connected-p))
                     (cider-current-ns))
        :repl-type (when (and (featurep 'cider) (boundp 'cider-repl-type))
                     cider-repl-type)))

;;;; Minor Mode

;;;###autoload
(define-minor-mode emacs-mcp-cider-mode
  "Minor mode for emacs-mcp integration with CIDER.

Provides:
- Save REPL results to memory
- Query past solutions
- Clojure-aware context
- Auto-start nREPL server (async, non-blocking)
- Auto-connect CIDER when nREPL becomes available"
  :init-value nil
  :lighter " MCP-Clj"
  :global t
  :group 'emacs-mcp-cider
  (if emacs-mcp-cider-mode
      (progn
        (require 'emacs-mcp-api nil t)
        ;; Auto-start nREPL if configured (async, non-blocking)
        (when emacs-mcp-cider-auto-start-nrepl
          (emacs-mcp-cider-start-nrepl))
        ;; Auto-connect if nREPL is already running externally
        (when (and emacs-mcp-cider-auto-connect
                   (not emacs-mcp-cider-auto-start-nrepl))
          (emacs-mcp-cider--start-auto-connect))
        (message "emacs-mcp-cider enabled"))
    ;; Cleanup on disable
    (emacs-mcp-cider--stop-auto-connect)
    (message "emacs-mcp-cider disabled")))

;;;; Addon Registration

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'cider
   :version "0.1.0"
   :description "Integration with CIDER (Clojure IDE)"
   :requires '(cider emacs-mcp-api)
   :provides '(emacs-mcp-cider-mode emacs-mcp-cider-transient)))

(provide 'emacs-mcp-cider)
;;; emacs-mcp-cider.el ends here
