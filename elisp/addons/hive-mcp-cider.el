;;; hive-mcp-cider.el --- Integrate CIDER/nREPL with hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, clojure, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates CIDER (Clojure IDE for Emacs) with hive-mcp.
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
;;   (hive-mcp-addon-load 'cider)
;;   (hive-mcp-cider-mode 1)
;;
;; Or enable auto-loading when CIDER loads:
;;   (hive-mcp-addons-auto-load)
;;
;; For auto-start nREPL on addon load:
;;   (setq hive-mcp-cider-auto-start-nrepl t)
;;   (add-to-list 'hive-mcp-addon-always-load 'cider)

;;; Code:

(require 'hive-mcp-api)

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

(defgroup hive-mcp-cider nil
  "Integration between CIDER and hive-mcp."
  :group 'hive-mcp
  :group 'cider
  :prefix "hive-mcp-cider-")

(defcustom hive-mcp-cider-auto-log-results nil
  "When non-nil, automatically log REPL results to memory."
  :type 'boolean
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-log-threshold 100
  "Minimum result length to auto-log (avoids logging trivial results)."
  :type 'integer
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-auto-start-nrepl nil
  "When non-nil, automatically start nREPL server on addon load.
The server starts asynchronously and does not block Emacs startup."
  :type 'boolean
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-auto-connect t
  "When non-nil, automatically connect CIDER when nREPL is available.
Works with both auto-started and externally started nREPL servers."
  :type 'boolean
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-nrepl-port 7910
  "Default port for nREPL server."
  :type 'integer
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-project-dir nil
  "Project directory for starting nREPL.
If nil, uses the current project root or `default-directory'."
  :type '(choice (const nil) directory)
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-connect-retry-interval 1.0
  "Seconds between connection retry attempts."
  :type 'number
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-connect-max-retries 30
  "Maximum number of connection retry attempts (0 = unlimited)."
  :type 'integer
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-eval-timeout 60
  "Timeout in seconds for nREPL evaluation with heartbeat polling.
Default is 60 seconds. The evaluation uses async request with polling
instead of blocking sync request."
  :type 'integer
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-poll-interval 0.1
  "Interval in seconds between heartbeat polls during async eval.
Smaller values = more responsive, larger values = less CPU overhead."
  :type 'number
  :group 'hive-mcp-cider)

;;;; Internal:

(defvar hive-mcp-cider--last-eval nil
  "Last evaluated expression and result for potential saving.")

(defvar hive-mcp-cider--async-result nil
  "Result from async evaluation, set by callback.")

(defvar hive-mcp-cider--async-done nil
  "Flag indicating async evaluation completed.")

(defvar hive-mcp-cider--async-error nil
  "Error from async evaluation, if any.")

(defvar hive-mcp-cider--nrepl-process nil
  "Process object for auto-started nREPL server.")

(defvar hive-mcp-cider--connect-timer nil
  "Timer for auto-connect retry attempts.")

(defvar hive-mcp-cider--connect-attempts 0
  "Number of connection attempts made.")

;;;; Multi-Session Registry:

(defvar hive-mcp-cider--sessions (make-hash-table :test 'equal)
  "Registry of named CIDER sessions.
Keys are session names (strings).
Values are plists with :port, :process, :buffer, :agent-id, :project-dir.")

(defcustom hive-mcp-cider-session-port-base 7920
  "Base port for spawning new CIDER sessions.
Sessions will use ports starting from this value."
  :type 'integer
  :group 'hive-mcp-cider)

(defcustom hive-mcp-cider-session-port-max 7999
  "Maximum port for CIDER sessions."
  :type 'integer
  :group 'hive-mcp-cider)

(defun hive-mcp-cider--find-available-port ()
  "Find the next available port for a new session."
  (let ((port hive-mcp-cider-session-port-base)
        (used-ports (mapcar (lambda (name)
                              (plist-get (gethash name hive-mcp-cider--sessions) :port))
                            (hash-table-keys hive-mcp-cider--sessions))))
    (while (and (<= port hive-mcp-cider-session-port-max)
                (or (member port used-ports)
                    (hive-mcp-cider--port-open-p port)))
      (setq port (1+ port)))
    (if (<= port hive-mcp-cider-session-port-max)
        port
      (error "No available ports in range %d-%d"
             hive-mcp-cider-session-port-base
             hive-mcp-cider-session-port-max))))

;;;###autoload
(defun hive-mcp-cider-spawn-session (name &optional project-dir agent-id)
  "Spawn a new named CIDER session.
NAME is the session identifier (e.g., \"agent-1\", \"task-render\").
PROJECT-DIR is the directory to start nREPL in (default: current project).
AGENT-ID optionally links this session to a swarm agent."
  (interactive "sSession name: ")
  (when (gethash name hive-mcp-cider--sessions)
    (error "Session '%s' already exists" name))
  (let* ((port (hive-mcp-cider--find-available-port))
         (dir (or project-dir (hive-mcp-cider--project-dir)))
         (default-directory dir)
         (buf-name (format "*nREPL-%s*" name))
         (process (start-process (format "nrepl-%s" name) buf-name
                                 "clojure" "-M:nrepl"
                                 "-p" (number-to-string port)))
         ;; Create timer and store reference for cleanup
         (timer (run-with-timer 2 1
                                (lambda ()
                                  (condition-case err
                                      (hive-mcp-cider--try-connect-session name)
                                    (error
                                     (message "[cider] Timer error connecting session %s: %s"
                                              name (error-message-string err))))))))
    ;; Register session with timer reference
    (puthash name
             (list :port port
                   :process process
                   :buffer buf-name
                   :agent-id agent-id
                   :project-dir dir
                   :status 'starting
                   :cider-buffer nil
                   :timer timer)  ; Store timer for cleanup
             hive-mcp-cider--sessions)
    (message "hive-mcp-cider: Spawning session '%s' on port %d..." name port)
    (list :name name :port port :status "starting")))

(defun hive-mcp-cider--cancel-session-timer (name)
  "Cancel the auto-connect timer for session NAME."
  (when-let* ((session (gethash name hive-mcp-cider--sessions))
              (timer (plist-get session :timer)))
    (when (timerp timer)
      (cancel-timer timer))
    ;; Clear timer from session
    (puthash name (plist-put session :timer nil) hive-mcp-cider--sessions)))

(defun hive-mcp-cider--try-connect-session (name)
  "Try to connect CIDER to session NAME."
  (let* ((session (gethash name hive-mcp-cider--sessions))
         (port (plist-get session :port))
         (status (plist-get session :status)))
    (when (and session (eq status 'starting))
      (if (hive-mcp-cider--port-open-p port)
          (condition-case err
              (let ((conn (cider-connect-clj (list :host "localhost" :port port))))
                ;; Cancel timer - connection succeeded
                (hive-mcp-cider--cancel-session-timer name)
                (puthash name
                         (plist-put (plist-put session :status 'connected)
                                    :cider-buffer (buffer-name conn))
                         hive-mcp-cider--sessions)
                (message "hive-mcp-cider: Session '%s' connected on port %d" name port))
            (error
             ;; Cancel timer - connection failed permanently
             (hive-mcp-cider--cancel-session-timer name)
             (puthash name (plist-put session :status 'error) hive-mcp-cider--sessions)
             (message "hive-mcp-cider: Session '%s' connection failed: %s"
                      name (error-message-string err))))
        ;; Still waiting for nREPL to start
        (let ((attempts (or (plist-get session :attempts) 0)))
          (if (< attempts 30)
              (puthash name (plist-put session :attempts (1+ attempts))
                       hive-mcp-cider--sessions)
            ;; Cancel timer - timeout reached
            (hive-mcp-cider--cancel-session-timer name)
            (puthash name (plist-put session :status 'timeout)
                     hive-mcp-cider--sessions)
            (message "hive-mcp-cider: Session '%s' timed out waiting for nREPL" name)))))))

;;;###autoload
(defun hive-mcp-cider-list-sessions ()
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
             hive-mcp-cider--sessions)
    (let ((result (vconcat sessions)))  ; Vector for json-encode → [] not null
      (if (called-interactively-p 'any)
          (message "Sessions: %s" (json-encode result))
        result))))

;;;###autoload
(defun hive-mcp-cider-get-session (name)
  "Get session info for NAME."
  (gethash name hive-mcp-cider--sessions))

;;;###autoload
(defun hive-mcp-cider-kill-session (name)
  "Kill the CIDER session NAME."
  (interactive
   (list (completing-read "Kill session: "
                          (hash-table-keys hive-mcp-cider--sessions))))
  (let ((session (gethash name hive-mcp-cider--sessions)))
    (when session
      ;; Cancel auto-connect timer first
      (hive-mcp-cider--cancel-session-timer name)
      (let ((process (plist-get session :process))
            (cider-buf (plist-get session :cider-buffer)))
        ;; Kill CIDER connection
        (when (and cider-buf (get-buffer cider-buf))
          (with-current-buffer cider-buf
            (when (fboundp 'cider-quit) (cider-quit))))
        ;; Kill nREPL process
        (when (and process (process-live-p process))
          (kill-process process)))
      (remhash name hive-mcp-cider--sessions)
      (message "hive-mcp-cider: Session '%s' killed" name))))

;;;###autoload
(defun hive-mcp-cider-eval-in-session (name code)
  "Evaluate CODE in the CIDER session NAME.
Uses async evaluation with heartbeat polling."
  (let* ((session (gethash name hive-mcp-cider--sessions))
         (cider-buf (plist-get session :cider-buffer)))
    (unless session
      (error "Session '%s' not found" name))
    (unless (eq (plist-get session :status) 'connected)
      (error "Session '%s' not connected (status: %s)"
             name (plist-get session :status)))
    (with-current-buffer cider-buf
      (hive-mcp-cider--eval-with-heartbeat code))))

;;;###autoload
(defun hive-mcp-cider-kill-all-sessions ()
  "Kill all CIDER sessions."
  (interactive)
  (maphash (lambda (name _props)
             (hive-mcp-cider-kill-session name))
           hive-mcp-cider--sessions)
  (clrhash hive-mcp-cider--sessions)
  (message "hive-mcp-cider: All sessions killed"))

(defun hive-mcp-cider--find-connected-session ()
  "Find first connected session from registry.
Returns session name (string) or nil."
  (let (result)
    (maphash (lambda (name props)
               (when (and (not result)
                          (eq (plist-get props :status) 'connected))
                 (setq result name)))
             hive-mcp-cider--sessions)
    result))

(defun hive-mcp-cider--switch-to-session (name)
  "Switch to the CIDER session NAME, making it the default connection.
Returns t if successful, nil otherwise."
  (let* ((session (gethash name hive-mcp-cider--sessions))
         (cider-buf (plist-get session :cider-buffer)))
    (when (and cider-buf (get-buffer cider-buf))
      (with-current-buffer cider-buf
        (when (fboundp 'cider-make-connection-default)
          (cider-make-connection-default (current-buffer))
          t)))))

(defun hive-mcp-cider--ensure-connected ()
  "Ensure CIDER is connected, reusing existing session or spawning new.
Returns t if connected, signals error otherwise.
Priority: 1) Already connected 2) Reuse registry session 3) Connect to port 4) Spawn new."
  (cond
   ;; Already connected via CIDER - nothing to do
   ((and (featurep 'cider) (cider-connected-p))
    t)
   ;; Check for existing connected session in our registry
   ((when-let* ((session-name (hive-mcp-cider--find-connected-session)))
      (message "hive-mcp-cider: Reusing session '%s'" session-name)
      (hive-mcp-cider--switch-to-session session-name)
      t))
   ;; Try default port if nREPL is running there
   ((hive-mcp-cider--port-open-p hive-mcp-cider-nrepl-port)
    (message "hive-mcp-cider: Auto-connecting to port %d" hive-mcp-cider-nrepl-port)
    (condition-case nil
        (progn
          (cider-connect-clj (list :host "localhost"
                                   :port hive-mcp-cider-nrepl-port))
          t)
      (error nil)))
   ;; No session available - spawn new one (or wait for existing 'auto')
   (t
    (let ((auto-session (gethash "auto" hive-mcp-cider--sessions)))
      ;; Clean up failed/timed out auto session before spawning new one
      (when (and auto-session
                 (memq (plist-get auto-session :status) '(error timeout)))
        (hive-mcp-cider-kill-session "auto")
        (setq auto-session nil))
      ;; Spawn if no auto session exists
      (unless auto-session
        (message "hive-mcp-cider: Spawning new session 'auto'")
        (hive-mcp-cider-spawn-session "auto"))
      ;; Wait for connection with timeout
      (let ((attempts 0)
            (max-attempts 30))
        (while (and (< attempts max-attempts)
                    (not (cider-connected-p)))
          (sleep-for 0.5)
          (setq attempts (1+ attempts)))
        (cider-connected-p))))))

;;;; Async nREPL Start & Auto-Connect:

(defun hive-mcp-cider--project-dir ()
  "Get the project directory for nREPL."
  (or hive-mcp-cider-project-dir
      (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun hive-mcp-cider--port-open-p (port)
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

(defun hive-mcp-cider--start-nrepl-async ()
  "Start nREPL server asynchronously in background.
Does not block Emacs startup.  Uses the port from `hive-mcp-cider-nrepl-port'."
  (let* ((default-directory (hive-mcp-cider--project-dir))
         (port (number-to-string hive-mcp-cider-nrepl-port))
         (buf-name "*nREPL-server*"))
    (message "hive-mcp-cider: Starting nREPL on port %s in %s..." port default-directory)
    (setq hive-mcp-cider--nrepl-process
          (start-process "nrepl-server" buf-name
                         "clojure" "-M:dev" "-m" "nrepl.cmdline"
                         "--port" port
                         "--middleware" "[cider.nrepl/cider-middleware,refactor-nrepl.middleware/wrap-refactor]"))))

(defun hive-mcp-cider--try-connect ()
  "Try to connect CIDER to nREPL.  Returns t if successful."
  (when (and (featurep 'cider)
             (fboundp 'cider-connect-clj)
             (not (cider-connected-p))
             (hive-mcp-cider--port-open-p hive-mcp-cider-nrepl-port))
    (condition-case err
        (progn
          (cider-connect-clj (list :host "localhost"
                                   :port hive-mcp-cider-nrepl-port))
          (message "hive-mcp-cider: Connected to nREPL on port %d"
                   hive-mcp-cider-nrepl-port)
          t)
      (error
       (message "hive-mcp-cider: Connection failed: %s" (error-message-string err))
       nil))))

(defun hive-mcp-cider--auto-connect-tick ()
  "Timer callback for auto-connect attempts."
  (condition-case err
      (progn
        (setq hive-mcp-cider--connect-attempts (1+ hive-mcp-cider--connect-attempts))
        (cond
   ;; Already connected - stop trying
   ((and (featurep 'cider) (cider-connected-p))
    (hive-mcp-cider--stop-auto-connect)
    (message "hive-mcp-cider: Already connected"))
   ;; Successfully connected
   ((hive-mcp-cider--try-connect)
    (hive-mcp-cider--stop-auto-connect))
   ;; Max retries reached
   ((and (> hive-mcp-cider-connect-max-retries 0)
         (>= hive-mcp-cider--connect-attempts hive-mcp-cider-connect-max-retries))
    (hive-mcp-cider--stop-auto-connect)
    (message "hive-mcp-cider: Auto-connect gave up after %d attempts"
             hive-mcp-cider--connect-attempts))))
    (error
     (message "[cider] Auto-connect tick error: %s"
              (error-message-string err)))))

(defun hive-mcp-cider--start-auto-connect ()
  "Start the auto-connect timer."
  (unless hive-mcp-cider--connect-timer
    (setq hive-mcp-cider--connect-attempts 0)
    (setq hive-mcp-cider--connect-timer
          (run-with-timer hive-mcp-cider-connect-retry-interval
                          hive-mcp-cider-connect-retry-interval
                          #'hive-mcp-cider--auto-connect-tick))
    (message "hive-mcp-cider: Auto-connect started (checking every %.1fs)"
             hive-mcp-cider-connect-retry-interval)))

(defun hive-mcp-cider--stop-auto-connect ()
  "Stop the auto-connect timer."
  (when hive-mcp-cider--connect-timer
    (cancel-timer hive-mcp-cider--connect-timer)
    (setq hive-mcp-cider--connect-timer nil)))

;;;###autoload
(defun hive-mcp-cider-start-nrepl ()
  "Start nREPL server and auto-connect CIDER.
Non-blocking - runs in background."
  (interactive)
  (hive-mcp-cider--start-nrepl-async)
  (when hive-mcp-cider-auto-connect
    (hive-mcp-cider--start-auto-connect)))

;;;###autoload
(defun hive-mcp-cider-auto-connect ()
  "Start polling for nREPL and connect when available.
Use this when nREPL is started externally."
  (interactive)
  (hive-mcp-cider--start-auto-connect))

;;;###autoload
(defun hive-mcp-cider-stop-nrepl ()
  "Stop the auto-started nREPL server."
  (interactive)
  (hive-mcp-cider--stop-auto-connect)
  (when (and hive-mcp-cider--nrepl-process
             (process-live-p hive-mcp-cider--nrepl-process))
    (kill-process hive-mcp-cider--nrepl-process)
    (setq hive-mcp-cider--nrepl-process nil)
    (message "hive-mcp-cider: nREPL server stopped")))

;;;; Context Functions:

(defun hive-mcp-cider--get-clojure-context ()
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
(defun hive-mcp-cider-save-last-result ()
  "Save the last REPL result to memory."
  (interactive)
  (if hive-mcp-cider--last-eval
      (let* ((expr (plist-get hive-mcp-cider--last-eval :expr))
             (result (plist-get hive-mcp-cider--last-eval :result))
             (ns (plist-get hive-mcp-cider--last-eval :ns))
             (tags (split-string
                    (read-string "Tags (comma-separated): " "clojure,repl")
                    "," t " "))
             (content (format "Namespace: %s\nExpression: %s\nResult: %s"
                              ns expr result)))
        (hive-mcp-api-memory-add "snippet" content tags)
        (message "Saved REPL result to memory"))
    (message "No recent REPL result to save")))

;;;###autoload
(defun hive-mcp-cider-save-defun ()
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
          (hive-mcp-api-memory-add "snippet" content tags)
          (message "Saved function to memory"))
      (message "No function at point"))))

;;;###autoload
(defun hive-mcp-cider-query-solutions (query)
  "Query memory for Clojure solutions matching QUERY."
  (interactive "sSearch for: ")
  (let* ((results (hive-mcp-api-memory-query "snippet" '("clojure") 20))
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
(defun hive-mcp-cider-eval-with-context ()
  "Evaluate expression with MCP context injected as comment."
  (interactive)
  (when (fboundp 'cider-interactive-eval)
    (let* ((ctx (hive-mcp-api-get-context))
           (ctx-comment (format ";; Context: %s @ %s\n"
                                (plist-get (plist-get ctx :buffer) :name)
                                (plist-get (plist-get ctx :project) :name)))
           (expr (when (fboundp 'cider-last-sexp) (cider-last-sexp))))
      ;; Store for potential saving
      (setq hive-mcp-cider--last-eval
            (list :expr expr
                  :ns (when (fboundp 'cider-current-ns) (cider-current-ns))
                  :context ctx))
      (cider-interactive-eval expr))))

;;;; Advice for auto-logging:

(defun hive-mcp-cider--after-eval-advice (orig-fun &rest args)
  "Advice around ORIG-FUN with ARGS to capture eval results for saving."
  (let ((result (apply orig-fun args)))
    (when (and hive-mcp-cider-auto-log-results
               result
               (> (length (format "%s" result)) hive-mcp-cider-log-threshold))
      (setq hive-mcp-cider--last-eval
            (list :expr (car args)
                  :result result
                  :ns (when (fboundp 'cider-current-ns) (cider-current-ns)))))
    result))

;;;; Transient Menu:

;;;###autoload (autoload 'hive-mcp-cider-transient "hive-mcp-cider" nil t)
(transient-define-prefix hive-mcp-cider-transient ()
  "MCP integration menu for CIDER."
  ["hive-mcp + CIDER"
   ["nREPL Server"
    ("n" "Start nREPL (async)" hive-mcp-cider-start-nrepl)
    ("N" "Stop nREPL" hive-mcp-cider-stop-nrepl)
    ("c" "Auto-connect" hive-mcp-cider-auto-connect)]
   ["Evaluate"
    ("e" "Eval with context" hive-mcp-cider-eval-with-context)]
   ["Memory"
    ("s" "Save last result" hive-mcp-cider-save-last-result)
    ("d" "Save defun" hive-mcp-cider-save-defun)
    ("q" "Query solutions" hive-mcp-cider-query-solutions)]
   ["Settings"
    ("L" "Toggle auto-log" hive-mcp-cider-toggle-auto-log)]])

(defun hive-mcp-cider-toggle-auto-log ()
  "Toggle automatic logging of REPL results."
  (interactive)
  (setq hive-mcp-cider-auto-log-results (not hive-mcp-cider-auto-log-results))
  (message "Auto-log %s" (if hive-mcp-cider-auto-log-results "enabled" "disabled")))

;;;; MCP Tool API Functions:
;; These functions are called by the hive-mcp Clojure server as MCP tools

;;;###autoload
(defun hive-mcp-cider-eval-silent (code)
  "Evaluate CODE via CIDER silently, return result.
Auto-connects if not connected, reusing existing session if available.
Uses async evaluation with heartbeat polling."
  (unless (hive-mcp-cider--ensure-connected)
    (error "CIDER could not connect - no nREPL available"))
  (hive-mcp-cider--eval-with-heartbeat code))

(defun hive-mcp-cider--eval-with-heartbeat (code)
  "Evaluate CODE asynchronously with heartbeat polling.
Returns result when ready or signals error on timeout."
  ;; Reset state
  (setq hive-mcp-cider--async-result nil
        hive-mcp-cider--async-done nil
        hive-mcp-cider--async-error nil)
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
         (setq hive-mcp-cider--async-result value))
       (when err
         (setq hive-mcp-cider--async-error err))
       (when (and out (not hive-mcp-cider--async-result))
         (setq hive-mcp-cider--async-result out))
       ;; Mark done when status contains "done"
       (when (member "done" status)
         (setq hive-mcp-cider--async-done t)))))
  ;; Poll with heartbeat until done or timeout
  (let ((start-time (float-time))
        (timeout hive-mcp-cider-eval-timeout)
        (interval hive-mcp-cider-poll-interval))
    (while (and (not hive-mcp-cider--async-done)
                (< (- (float-time) start-time) timeout))
      (accept-process-output nil interval))
    (cond
     ((not hive-mcp-cider--async-done)
      (error "Eval timed out after %d seconds (heartbeat polling)" timeout))
     (hive-mcp-cider--async-error
      hive-mcp-cider--async-error)
     (hive-mcp-cider--async-result
      hive-mcp-cider--async-result)
     (t "nil"))))

;;;###autoload
(defun hive-mcp-cider-eval-explicit (code)
  "Evaluate CODE via CIDER interactively.
Auto-connects if not connected, reusing existing session if available.
Shows output in REPL buffer for collaborative debugging."
  (unless (hive-mcp-cider--ensure-connected)
    (error "CIDER could not connect - no nREPL available"))
  (cider-interactive-eval code)
  (format "Sent to REPL: %s" (truncate-string-to-width code 50 nil nil "...")))

;;;###autoload
(defun hive-mcp-cider-status ()
  "Return CIDER connection status as JSON-compatible plist."
  (list :connected (and (featurep 'cider) (cider-connected-p))
        :repl-buffer (when (and (featurep 'cider) (cider-connected-p))
                       (buffer-name (car (cider-repl-buffers))))
        :namespace (when (and (featurep 'cider) (cider-connected-p))
                     (cider-current-ns))
        :repl-type (when (and (featurep 'cider) (boundp 'cider-repl-type))
                     cider-repl-type)))

;;;###autoload
(defun hive-mcp-cider-doc (symbol-name)
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
(defun hive-mcp-cider-apropos (pattern &optional docs-p)
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
(defun hive-mcp-cider-info (symbol-name)
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
(defun hive-mcp-cider-complete (prefix)
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

(defun hive-mcp-cider--addon-init ()
  "Synchronous init for cider addon.
Sets up keybindings and loads required features.
Does nothing if cider is not available."
  (if (not (featurep 'cider))
      (message "hive-mcp-cider: cider package not found, addon disabled")
    (require 'hive-mcp-api nil t)
    (message "hive-mcp-cider: initialized")))

(defun hive-mcp-cider--addon-async-init ()
  "Asynchronous init for cider addon.
Starts nREPL server in background if configured.
Returns the process object for lifecycle tracking."
  (when hive-mcp-cider-auto-start-nrepl
    (hive-mcp-cider--start-nrepl-async)
    ;; Register timer with addon system for cleanup
    (when hive-mcp-cider-auto-connect
      (hive-mcp-cider--start-auto-connect)
      (when hive-mcp-cider--connect-timer
        (when (fboundp 'hive-mcp-addon-register-timer)
          (hive-mcp-addon-register-timer 'cider hive-mcp-cider--connect-timer))))
    ;; Return process for lifecycle tracking
    hive-mcp-cider--nrepl-process))

(defun hive-mcp-cider--addon-shutdown ()
  "Shutdown function for cider addon.
Stops nREPL server and cleans up timers."
  (hive-mcp-cider--stop-auto-connect)
  (when (and hive-mcp-cider--nrepl-process
             (process-live-p hive-mcp-cider--nrepl-process))
    (kill-process hive-mcp-cider--nrepl-process)
    (setq hive-mcp-cider--nrepl-process nil))
  (message "hive-mcp-cider: shutdown complete"))

;;;; Minor Mode:

;;;###autoload
(define-minor-mode hive-mcp-cider-mode
  "Minor mode for hive-mcp integration with CIDER.

Provides:
- Save REPL results to memory
- Query past solutions
- Clojure-aware context

Note: nREPL auto-start is handled by addon lifecycle hooks.
Set `hive-mcp-cider-auto-start-nrepl' to t and load the addon."
  :init-value nil
  :lighter " MCP-Clj"
  :global t
  :group 'hive-mcp-cider
  (if hive-mcp-cider-mode
      (message "hive-mcp-cider mode enabled")
    (message "hive-mcp-cider mode disabled")))

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'cider
   :version "0.2.0"
   :description "Integration with CIDER (Clojure IDE) - async nREPL startup"
   :requires '(cider hive-mcp-api)
   :provides '(hive-mcp-cider-mode hive-mcp-cider-transient)
   :init #'hive-mcp-cider--addon-init
   :async-init #'hive-mcp-cider--addon-async-init
   :shutdown #'hive-mcp-cider--addon-shutdown))

(provide 'hive-mcp-cider)
;;; hive-mcp-cider.el ends here
