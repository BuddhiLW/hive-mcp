;;; emacs-mcp-cider.el --- Integrate CIDER/nREPL with emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, clojure, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates CIDER (Clojure IDE for Emacs) with emacs-mcp.
;;
;; OPTIONAL DEPENDENCIES:
;; - cider (https://github.com/clojure-emacs/cider)
;;   Install via: M-x package-install RET cider RET
;;
;; This addon activates automatically when cider is loaded.
;; Without cider, the addon is silently disabled.
;;
;; Features:
;; - Add Clojure namespace/project context to MCP
;; - Save REPL results to memory
;; - Query memory for previous solutions
;; - Auto-log REPL sessions
;; - Auto-start nREPL server (async, non-blocking)
;; - Auto-connect CIDER when nREPL becomes available
;;
;; Usage:
;;   (emacs-mcp-addon-load 'cider)
;;   (emacs-mcp-cider-mode 1)
;;
;; Or enable auto-loading when CIDER loads:
;;   (emacs-mcp-addons-auto-load)
;;
;; For auto-start nREPL on addon load:
;;   (setq emacs-mcp-cider-auto-start-nrepl t)
;;   (add-to-list 'emacs-mcp-addon-always-load 'cider)

;;; Code:

(require 'emacs-mcp-api)

;; Soft dependencies
(declare-function cider-current-ns "cider-client")
(declare-function cider-current-connection "cider-client")
(declare-function cider-nrepl-eval-sync "cider-nrepl")
(declare-function cider-last-sexp "cider-eval")
(declare-function cider-interactive-eval "cider-eval")
(declare-function cider-connect-clj "cider")
(declare-function cider-connected-p "cider-connection")
(declare-function cider-repl-buffers "cider-connection")

;;;; Customization:

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

(defcustom emacs-mcp-cider-eval-timeout 60
  "Timeout in seconds for nREPL evaluation with heartbeat polling.
Default is 60 seconds. The evaluation uses async request with polling
instead of blocking sync request."
  :type 'integer
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-poll-interval 0.1
  "Interval in seconds between heartbeat polls during async eval.
Smaller values = more responsive, larger values = less CPU overhead."
  :type 'number
  :group 'emacs-mcp-cider)

;;;; Internal:

(defvar emacs-mcp-cider--last-eval nil
  "Last evaluated expression and result for potential saving.")

(defvar emacs-mcp-cider--async-result nil
  "Result from async evaluation, set by callback.")

(defvar emacs-mcp-cider--async-done nil
  "Flag indicating async evaluation completed.")

(defvar emacs-mcp-cider--async-error nil
  "Error from async evaluation, if any.")

(defvar emacs-mcp-cider--nrepl-process nil
  "Process object for auto-started nREPL server.")

(defvar emacs-mcp-cider--connect-timer nil
  "Timer for auto-connect retry attempts.")

(defvar emacs-mcp-cider--connect-attempts 0
  "Number of connection attempts made.")

;;;; Multi-Session Registry:

(defvar emacs-mcp-cider--sessions (make-hash-table :test 'equal)
  "Registry of named CIDER sessions.
Keys are session names (strings).
Values are plists with :port, :process, :buffer, :agent-id, :project-dir.")

(defcustom emacs-mcp-cider-session-port-base 7920
  "Base port for spawning new CIDER sessions.
Sessions will use ports starting from this value."
  :type 'integer
  :group 'emacs-mcp-cider)

(defcustom emacs-mcp-cider-session-port-max 7999
  "Maximum port for CIDER sessions."
  :type 'integer
  :group 'emacs-mcp-cider)

(defun emacs-mcp-cider--find-available-port ()
  "Find the next available port for a new session."
  (let ((port emacs-mcp-cider-session-port-base)
        (used-ports (mapcar (lambda (name)
                              (plist-get (gethash name emacs-mcp-cider--sessions) :port))
                            (hash-table-keys emacs-mcp-cider--sessions))))
    (while (and (<= port emacs-mcp-cider-session-port-max)
                (or (member port used-ports)
                    (emacs-mcp-cider--port-open-p port)))
      (setq port (1+ port)))
    (if (<= port emacs-mcp-cider-session-port-max)
        port
      (error "No available ports in range %d-%d"
             emacs-mcp-cider-session-port-base
             emacs-mcp-cider-session-port-max))))

;;;###autoload
(defun emacs-mcp-cider-spawn-session (name &optional project-dir agent-id)
  "Spawn a new named CIDER session.
NAME is the session identifier (e.g., \"agent-1\", \"task-render\").
PROJECT-DIR is the directory to start nREPL in (default: current project).
AGENT-ID optionally links this session to a swarm agent."
  (interactive "sSession name: ")
  (when (gethash name emacs-mcp-cider--sessions)
    (error "Session '%s' already exists" name))
  (let* ((port (emacs-mcp-cider--find-available-port))
         (dir (or project-dir (emacs-mcp-cider--project-dir)))
         (default-directory dir)
         (buf-name (format "*nREPL-%s*" name))
         (process (start-process (format "nrepl-%s" name) buf-name
                                 "clojure" "-M:nrepl"
                                 "-p" (number-to-string port))))
    ;; Register session
    (puthash name
             (list :port port
                   :process process
                   :buffer buf-name
                   :agent-id agent-id
                   :project-dir dir
                   :status 'starting
                   :cider-buffer nil)
             emacs-mcp-cider--sessions)
    (message "emacs-mcp-cider: Spawning session '%s' on port %d..." name port)
    ;; Start auto-connect for this session
    (run-with-timer 2 1
                    (lambda ()
                      (emacs-mcp-cider--try-connect-session name)))
    (list :name name :port port :status "starting")))

(defun emacs-mcp-cider--try-connect-session (name)
  "Try to connect CIDER to session NAME."
  (let* ((session (gethash name emacs-mcp-cider--sessions))
         (port (plist-get session :port))
         (status (plist-get session :status)))
    (when (and session (eq status 'starting))
      (if (emacs-mcp-cider--port-open-p port)
          (condition-case err
              (let ((conn (cider-connect-clj (list :host "localhost" :port port))))
                (puthash name
                         (plist-put (plist-put session :status 'connected)
                                    :cider-buffer (buffer-name conn))
                         emacs-mcp-cider--sessions)
                (message "emacs-mcp-cider: Session '%s' connected on port %d" name port))
            (error
             (message "emacs-mcp-cider: Session '%s' connection failed: %s"
                      name (error-message-string err))))
        ;; Still waiting for nREPL to start
        (let ((attempts (or (plist-get session :attempts) 0)))
          (if (< attempts 30)
              (progn
                (puthash name (plist-put session :attempts (1+ attempts))
                         emacs-mcp-cider--sessions)
                (run-with-timer 1 nil
                                (lambda () (emacs-mcp-cider--try-connect-session name))))
            (puthash name (plist-put session :status 'timeout)
                     emacs-mcp-cider--sessions)
            (message "emacs-mcp-cider: Session '%s' timed out waiting for nREPL" name)))))))

;;;###autoload
(defun emacs-mcp-cider-list-sessions ()
  "List all active CIDER sessions.
Returns a vector (for JSON array encoding) of session plists."
  (interactive)
  (let ((sessions '()))
    (maphash (lambda (name props)
               (let ((status (plist-get props :status)))
                 (push (list :name name
                             :port (plist-get props :port)
                             :status (if (symbolp status) (symbol-name status) status)
                             :agent-id (plist-get props :agent-id)
                             :cider-buffer (plist-get props :cider-buffer))
                       sessions)))
             emacs-mcp-cider--sessions)
    (let ((result (vconcat sessions)))  ; Vector for json-encode → [] not null
      (if (called-interactively-p 'any)
          (message "Sessions: %s" (json-encode result))
        result))))

;;;###autoload
(defun emacs-mcp-cider-get-session (name)
  "Get session info for NAME."
  (gethash name emacs-mcp-cider--sessions))

;;;###autoload
(defun emacs-mcp-cider-kill-session (name)
  "Kill the CIDER session NAME."
  (interactive
   (list (completing-read "Kill session: "
                          (hash-table-keys emacs-mcp-cider--sessions))))
  (let ((session (gethash name emacs-mcp-cider--sessions)))
    (when session
      (let ((process (plist-get session :process))
            (cider-buf (plist-get session :cider-buffer)))
        ;; Kill CIDER connection
        (when (and cider-buf (get-buffer cider-buf))
          (with-current-buffer cider-buf
            (when (fboundp 'cider-quit) (cider-quit))))
        ;; Kill nREPL process
        (when (and process (process-live-p process))
          (kill-process process)))
      (remhash name emacs-mcp-cider--sessions)
      (message "emacs-mcp-cider: Session '%s' killed" name))))

;;;###autoload
(defun emacs-mcp-cider-eval-in-session (name code)
  "Evaluate CODE in the CIDER session NAME.
Uses async evaluation with heartbeat polling."
  (let* ((session (gethash name emacs-mcp-cider--sessions))
         (cider-buf (plist-get session :cider-buffer)))
    (unless session
      (error "Session '%s' not found" name))
    (unless (eq (plist-get session :status) 'connected)
      (error "Session '%s' not connected (status: %s)"
             name (plist-get session :status)))
    (with-current-buffer cider-buf
      (emacs-mcp-cider--eval-with-heartbeat code))))

;;;###autoload
(defun emacs-mcp-cider-kill-all-sessions ()
  "Kill all CIDER sessions."
  (interactive)
  (maphash (lambda (name _props)
             (emacs-mcp-cider-kill-session name))
           emacs-mcp-cider--sessions)
  (clrhash emacs-mcp-cider--sessions)
  (message "emacs-mcp-cider: All sessions killed"))

;;;; Async nREPL Start & Auto-Connect:

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
Does not block Emacs startup.  Uses the port from `emacs-mcp-cider-nrepl-port'."
  (let* ((default-directory (emacs-mcp-cider--project-dir))
         (port (number-to-string emacs-mcp-cider-nrepl-port))
         (buf-name "*nREPL-server*"))
    (message "emacs-mcp-cider: Starting nREPL on port %s in %s..." port default-directory)
    (setq emacs-mcp-cider--nrepl-process
          (start-process "nrepl-server" buf-name
                         "clojure" "-M:dev" "-m" "nrepl.cmdline"
                         "--port" port
                         "--middleware" "[cider.nrepl/cider-middleware,refactor-nrepl.middleware/wrap-refactor]"))))

(defun emacs-mcp-cider--try-connect ()
  "Try to connect CIDER to nREPL.  Returns t if successful."
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

;;;; Context Functions:

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

;;;; Memory Integration:

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

;;;; Eval with Context:

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

;;;; Advice for auto-logging:

(defun emacs-mcp-cider--after-eval-advice (orig-fun &rest args)
  "Advice around ORIG-FUN with ARGS to capture eval results for saving."
  (let ((result (apply orig-fun args)))
    (when (and emacs-mcp-cider-auto-log-results
               result
               (> (length (format "%s" result)) emacs-mcp-cider-log-threshold))
      (setq emacs-mcp-cider--last-eval
            (list :expr (car args)
                  :result result
                  :ns (when (fboundp 'cider-current-ns) (cider-current-ns)))))
    result))

;;;; Transient Menu:

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

;;;; MCP Tool API Functions:
;; These functions are called by the emacs-mcp Clojure server as MCP tools

;;;###autoload
(defun emacs-mcp-cider-eval-silent (code)
  "Evaluate CODE via CIDER silently, return result.
Uses async evaluation with heartbeat polling instead of sync request.
Polls every `emacs-mcp-cider-poll-interval' seconds until result ready
or `emacs-mcp-cider-eval-timeout' seconds elapsed."
  (if (and (featurep 'cider) (cider-connected-p))
      (emacs-mcp-cider--eval-with-heartbeat code)
    (error "CIDER not connected")))

(defun emacs-mcp-cider--eval-with-heartbeat (code)
  "Evaluate CODE asynchronously with heartbeat polling.
Returns result when ready or signals error on timeout."
  ;; Reset state
  (setq emacs-mcp-cider--async-result nil
        emacs-mcp-cider--async-done nil
        emacs-mcp-cider--async-error nil)
  ;; Send async request - callback may be called multiple times
  (cider-nrepl-request:eval
   code
   (lambda (response)
     (let ((value (nrepl-dict-get response "value"))
           (err (nrepl-dict-get response "err"))
           (out (nrepl-dict-get response "out"))
           (status (nrepl-dict-get response "status")))
       ;; Accumulate value/err/out as they arrive
       (when value
         (setq emacs-mcp-cider--async-result value))
       (when err
         (setq emacs-mcp-cider--async-error err))
       (when (and out (not emacs-mcp-cider--async-result))
         (setq emacs-mcp-cider--async-result out))
       ;; Mark done when status contains "done"
       (when (member "done" status)
         (setq emacs-mcp-cider--async-done t)))))
  ;; Poll with heartbeat until done or timeout
  (let ((start-time (float-time))
        (timeout emacs-mcp-cider-eval-timeout)
        (interval emacs-mcp-cider-poll-interval))
    (while (and (not emacs-mcp-cider--async-done)
                (< (- (float-time) start-time) timeout))
      (accept-process-output nil interval))
    (cond
     ((not emacs-mcp-cider--async-done)
      (error "Eval timed out after %d seconds (heartbeat polling)" timeout))
     (emacs-mcp-cider--async-error
      emacs-mcp-cider--async-error)
     (emacs-mcp-cider--async-result
      emacs-mcp-cider--async-result)
     (t "nil"))))

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

;;;###autoload
(defun emacs-mcp-cider-doc (symbol-name)
  "Get documentation for SYMBOL-NAME using CIDER.
Returns a plist with :doc, :arglists, :ns, :name."
  (if (and (featurep 'cider) (cider-connected-p))
      (let* ((info (cider-var-info symbol-name))
             (doc (nrepl-dict-get info "doc"))
             (arglists (nrepl-dict-get info "arglists"))
             (ns (nrepl-dict-get info "ns"))
             (name (nrepl-dict-get info "name"))
             (file (nrepl-dict-get info "file"))
             (line (nrepl-dict-get info "line")))
        (list :doc (or doc "No documentation available")
              :arglists (or arglists "")
              :ns (or ns "")
              :name (or name symbol-name)
              :file (or file "")
              :line (or line 0)))
    (error "CIDER not connected")))

;;;###autoload
(defun emacs-mcp-cider-apropos (pattern &optional docs-p)
  "Search for symbols matching PATTERN using CIDER.
If DOCS-P is non-nil, also search in docstrings.
Returns a vector of matching symbols with their info."
  (if (and (featurep 'cider) (cider-connected-p))
      (let* ((ns (cider-current-ns))
             (response (cider-nrepl-send-sync-request
                        (list "op" "apropos"
                              "query" pattern
                              "ns" ns
                              "docs?" (if docs-p "t" nil)
                              "privates?" "t"
                              "case-sensitive?" nil)))
             (apropos-matches (nrepl-dict-get response "apropos-matches")))
        (if apropos-matches
            (vconcat
             (mapcar (lambda (match)
                       (list :name (nrepl-dict-get match "name")
                             :type (nrepl-dict-get match "type")
                             :doc (or (nrepl-dict-get match "doc") "")))
                     apropos-matches))
          []))
    (error "CIDER not connected")))

;;;###autoload
(defun emacs-mcp-cider-info (symbol-name)
  "Get full semantic info for SYMBOL-NAME via CIDER's info op.
Returns comprehensive information including source location, spec, etc."
  (if (and (featurep 'cider) (cider-connected-p))
      (let* ((info (cider-var-info symbol-name)))
        (if info
            (let ((result '()))
              ;; Extract all useful fields from the nrepl-dict
              (dolist (key '("name" "ns" "doc" "arglists" "file" "line" "column"
                             "resource" "macro" "special-form" "protocol"
                             "spec" "see-also" "added" "deprecated"))
                (let ((val (nrepl-dict-get info key)))
                  (when val
                    (setq result (plist-put result (intern (concat ":" key)) val)))))
              result)
          (list :error (format "No info found for '%s'" symbol-name))))
    (error "CIDER not connected")))

;;;###autoload
(defun emacs-mcp-cider-complete (prefix)
  "Get completions for PREFIX using CIDER.
Returns a vector of completion candidates."
  (if (and (featurep 'cider) (cider-connected-p))
      (let* ((ns (cider-current-ns))
             (context (cider-completion-get-context))
             (response (cider-nrepl-send-sync-request
                        (list "op" "complete"
                              "ns" ns
                              "prefix" prefix
                              "context" context)))
             (completions (nrepl-dict-get response "completions")))
        (if completions
            (vconcat
             (mapcar (lambda (c)
                       (list :candidate (nrepl-dict-get c "candidate")
                             :type (nrepl-dict-get c "type")
                             :ns (nrepl-dict-get c "ns")))
                     completions))
          []))
    (error "CIDER not connected")))

;;;; Addon Lifecycle Functions:

(defun emacs-mcp-cider--addon-init ()
  "Synchronous init for cider addon.
Sets up keybindings and loads required features.
Does nothing if cider is not available."
  (unless (featurep 'cider)
    (message "emacs-mcp-cider: cider package not found, addon disabled")
    (cl-return-from emacs-mcp-cider--addon-init nil))
  (require 'emacs-mcp-api nil t)
  (message "emacs-mcp-cider: initialized"))

(defun emacs-mcp-cider--addon-async-init ()
  "Asynchronous init for cider addon.
Starts nREPL server in background if configured.
Returns the process object for lifecycle tracking."
  (when emacs-mcp-cider-auto-start-nrepl
    (emacs-mcp-cider--start-nrepl-async)
    ;; Register timer with addon system for cleanup
    (when emacs-mcp-cider-auto-connect
      (emacs-mcp-cider--start-auto-connect)
      (when emacs-mcp-cider--connect-timer
        (when (fboundp 'emacs-mcp-addon-register-timer)
          (emacs-mcp-addon-register-timer 'cider emacs-mcp-cider--connect-timer))))
    ;; Return process for lifecycle tracking
    emacs-mcp-cider--nrepl-process))

(defun emacs-mcp-cider--addon-shutdown ()
  "Shutdown function for cider addon.
Stops nREPL server and cleans up timers."
  (emacs-mcp-cider--stop-auto-connect)
  (when (and emacs-mcp-cider--nrepl-process
             (process-live-p emacs-mcp-cider--nrepl-process))
    (kill-process emacs-mcp-cider--nrepl-process)
    (setq emacs-mcp-cider--nrepl-process nil))
  (message "emacs-mcp-cider: shutdown complete"))

;;;; Minor Mode:

;;;###autoload
(define-minor-mode emacs-mcp-cider-mode
  "Minor mode for emacs-mcp integration with CIDER.

Provides:
- Save REPL results to memory
- Query past solutions
- Clojure-aware context

Note: nREPL auto-start is handled by addon lifecycle hooks.
Set `emacs-mcp-cider-auto-start-nrepl' to t and load the addon."
  :init-value nil
  :lighter " MCP-Clj"
  :global t
  :group 'emacs-mcp-cider
  (if emacs-mcp-cider-mode
      (message "emacs-mcp-cider mode enabled")
    (message "emacs-mcp-cider mode disabled")))

;;;; Addon Registration:

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'cider
   :version "0.2.0"
   :description "Integration with CIDER (Clojure IDE) - async nREPL startup"
   :requires '(cider emacs-mcp-api)
   :provides '(emacs-mcp-cider-mode emacs-mcp-cider-transient)
   :init #'emacs-mcp-cider--addon-init
   :async-init #'emacs-mcp-cider--addon-async-init
   :shutdown #'emacs-mcp-cider--addon-shutdown))

(provide 'emacs-mcp-cider)
;;; emacs-mcp-cider.el ends here
