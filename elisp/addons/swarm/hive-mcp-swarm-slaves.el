;;; hive-mcp-swarm-slaves.el --- Slave lifecycle management for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Slave lifecycle management for hive-mcp-swarm.
;; Handles spawning, killing, and state management of Claude slave instances.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles slave lifecycle
;; - Open/Closed: New terminal backends via dispatch, not modification
;; - Dependency Inversion: Callers depend on spawn/kill API, not internals
;;
;; Functions provided:
;; - Spawn: Create new slave instances with presets
;; - Kill: Terminate slaves cleanly without prompts
;; - Safety: Depth, rate-limit, and slave count checks
;; - ID generation: Unique slave and task IDs

;;; Code:

(require 'cl-lib)

;; Soft dependencies on terminal backends
(declare-function vterm "vterm")
(declare-function vterm-mode "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

(declare-function eat "eat")
(declare-function eat-mode "eat")
(declare-function eat-exec "eat")
(declare-function eat-term-send-string "eat")

(declare-function claude-code-ide "claude-code-ide")
(declare-function claude-code-ide--create-terminal-session "claude-code-ide")
(declare-function claude-code-ide-mcp-server-ensure-server "claude-code-ide-mcp-server")

;; Ollama backend via hive-mcp-ellama
(declare-function hive-mcp-ellama-swarm-spawn "hive-mcp-ellama")
(declare-function hive-mcp-ellama-swarm-dispatch "hive-mcp-ellama")

;; Dependencies on sibling modules
(declare-function hive-mcp-swarm-terminal-send "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-presets-build-system-prompt "hive-mcp-swarm-presets")
(declare-function hive-mcp-swarm-presets-role-to-presets "hive-mcp-swarm-presets")
(declare-function hive-mcp-swarm-presets-role-to-tier "hive-mcp-swarm-presets")
(declare-function hive-mcp-swarm-prompts-clear-slave "hive-mcp-swarm-prompts")
(declare-function hive-mcp-swarm-events-emit-slave-spawned "hive-mcp-swarm-events")
(declare-function hive-mcp-swarm-events-emit-slave-killed "hive-mcp-swarm-events")

;; Dependencies on main module (variables)
(defvar hive-mcp-swarm--slaves)
(defvar hive-mcp-swarm--task-counter)
(defvar hive-mcp-swarm--session-id)
(defvar hive-mcp-swarm--current-depth)
(defvar hive-mcp-swarm--ancestry)
(defvar hive-mcp-swarm-max-slaves)
(defvar hive-mcp-swarm-max-depth)
(defvar hive-mcp-swarm-rate-limit-window)
(defvar hive-mcp-swarm-rate-limit-max-spawns)
(defvar hive-mcp-swarm-terminal)
(defvar hive-mcp-swarm-buffer-prefix)
(defvar hive-mcp-swarm-claude-command)
(defvar hive-mcp-swarm-prompt-mode)

;; Forward declaration for list-presets (interactive use)
(declare-function hive-mcp-swarm-list-presets "hive-mcp-swarm")

;;;; Internal State:

(defvar hive-mcp-swarm-slaves--spawn-timestamps nil
  "List of recent spawn timestamps for rate limiting.")

;;;; ID Generation:

(defun hive-mcp-swarm-slaves-generate-id (name)
  "Generate unique slave ID for NAME."
  (format "swarm-%s-%d" name (floor (float-time))))

(defun hive-mcp-swarm-slaves-generate-task-id (slave-id)
  "Generate unique task ID for SLAVE-ID."
  (cl-incf hive-mcp-swarm--task-counter)
  (format "task-%s-%03d"
          (replace-regexp-in-string "^swarm-" "" slave-id)
          hive-mcp-swarm--task-counter))

;;;; Safety Checks:

(defun hive-mcp-swarm-slaves--depth-label (depth)
  "Return human-readable label for DEPTH level."
  (pcase depth
    (0 "master")
    (1 "child")
    (2 "grandchild")
    (3 "great-grandchild")
    (_ (format "depth-%d" depth))))

(defun hive-mcp-swarm-slaves-check-depth ()
  "Check if we can spawn at current depth.
Returns the current depth if allowed, signals error if blocked."
  (let* ((depth (string-to-number (or (getenv "CLAUDE_SWARM_DEPTH") "0")))
         (master-id (getenv "CLAUDE_SWARM_MASTER"))
         (my-id (getenv "CLAUDE_SWARM_SLAVE_ID")))
    (setq hive-mcp-swarm--current-depth depth)
    ;; Track ancestry for loop detection
    (when (and my-id master-id)
      (push (cons my-id master-id) hive-mcp-swarm--ancestry))
    (when (>= depth hive-mcp-swarm-max-depth)
      (error "Recursion limit reached: %s (depth %d) cannot spawn children.
Maximum depth is %d (master -> child -> grandchild -> great-grandchild).
This limit prevents runaway recursive spawning."
             (hive-mcp-swarm-slaves--depth-label depth)
             depth
             hive-mcp-swarm-max-depth))
    depth))

(defun hive-mcp-swarm-slaves-check-rate-limit ()
  "Check if spawn rate limit allows a new spawn.
Removes old timestamps and checks count within window."
  (let* ((now (float-time))
         (window-start (- now hive-mcp-swarm-rate-limit-window)))
    ;; Prune old timestamps
    (setq hive-mcp-swarm-slaves--spawn-timestamps
          (cl-remove-if (lambda (ts) (< ts window-start))
                        hive-mcp-swarm-slaves--spawn-timestamps))
    ;; Check limit
    (when (>= (length hive-mcp-swarm-slaves--spawn-timestamps)
              hive-mcp-swarm-rate-limit-max-spawns)
      (error "Rate limit exceeded: %d spawns in %d seconds.
Wait before spawning more slaves to prevent spawn storms."
             hive-mcp-swarm-rate-limit-max-spawns
             hive-mcp-swarm-rate-limit-window))
    ;; Record this spawn attempt
    (push now hive-mcp-swarm-slaves--spawn-timestamps)))

(defun hive-mcp-swarm-slaves-check-limit ()
  "Check if we can spawn more slaves."
  (when (>= (hash-table-count hive-mcp-swarm--slaves) hive-mcp-swarm-max-slaves)
    (error "Maximum slave count (%d) reached.
Kill some slaves with `hive-mcp-swarm-kill' before spawning more."
           hive-mcp-swarm-max-slaves)))

;;;; Project Root Helper:

(defun hive-mcp-swarm-slaves--project-root ()
  "Get current project root."
  (or (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      default-directory))

;;;; Spawn Functions:

(cl-defun hive-mcp-swarm-slaves-spawn (name &key presets cwd role terminal backend model)
  "Spawn a new Claude slave with NAME - FULLY ASYNC.

PRESETS is a list of preset names to apply (e.g., \\='(\"tdd\" \"clarity\")).
CWD is the working directory (defaults to current project root).
ROLE is a predefined role that maps to presets AND tier (backend/model).
TERMINAL overrides `hive-mcp-swarm-terminal' for this spawn.
BACKEND explicitly sets the backend (claude-code-ide, vterm, eat, ollama).
MODEL sets the model for ollama backend (devstral, devstral-small, deepseek-r1:7b).

When ROLE is specified without explicit BACKEND/MODEL, uses tier mapping
from `hive-mcp-swarm-tier-mapping' for two-tier orchestration:
- Premium tier (claude-code-ide): coordination, review, architecture
- Free tier (ollama): implementation, testing, documentation

Returns the slave-id IMMEDIATELY.  The actual spawn happens async.
Poll the slave's :status to check progress: spawning -> starting -> idle."
  (interactive
   (list (read-string "Slave name: ")
         :presets (completing-read-multiple
                   "Presets: "
                   (hive-mcp-swarm-list-presets))))
  ;; Safety checks (order matters: depth -> rate -> slave count)
  (hive-mcp-swarm-slaves-check-depth)
  (hive-mcp-swarm-slaves-check-rate-limit)
  (hive-mcp-swarm-slaves-check-limit)

  ;; Resolve role to presets and tier if provided
  (when role
    (unless presets
      (setq presets (hive-mcp-swarm-presets-role-to-presets role)))
    ;; Apply tier mapping for backend/model if not explicitly set
    (when-let* ((tier (hive-mcp-swarm-presets-role-to-tier role)))
      (unless backend
        (setq backend (plist-get tier :backend)))
      (unless model
        (setq model (plist-get tier :model)))))

  (let* ((slave-id (hive-mcp-swarm-slaves-generate-id name))
         (work-dir (or cwd (hive-mcp-swarm-slaves--project-root) default-directory))
         ;; Backend priority: explicit backend > terminal arg > default terminal
         ;; This allows tier mapping to set backend via role
         (term-backend (or backend terminal hive-mcp-swarm-terminal))
         (parent-id (or (getenv "CLAUDE_SWARM_SLAVE_ID") "master"))
         (spawn-depth (1+ hive-mcp-swarm--current-depth)))

    ;; Register slave IMMEDIATELY with "spawning" status - BEFORE any work
    (puthash slave-id
             (list :slave-id slave-id
                   :name name
                   :role role
                   :presets presets
                   :status 'spawning  ; Not starting yet - we return immediately
                   :buffer nil        ; Buffer created async
                   :terminal term-backend
                   :backend term-backend  ; Track actual backend used
                   :model model           ; Track model for ollama backend
                   :cwd work-dir
                   :depth spawn-depth
                   :parent-id parent-id
                   :current-task nil
                   :task-queue '()
                   :tasks-completed 0
                   :tasks-failed 0
                   :spawned-at (format-time-string "%FT%T%z")
                   :last-activity (format-time-string "%FT%T%z"))
             hive-mcp-swarm--slaves)

    ;; Log spawn intent
    (message "[swarm] Spawning %s (%s) at depth %d, parent: %s (async)"
             slave-id
             (hive-mcp-swarm-slaves--depth-label spawn-depth)
             spawn-depth
             parent-id)

    ;; Emit slave-spawned event via channel
    (hive-mcp-swarm-events-emit-slave-spawned slave-id name presets)

    ;; FULLY ASYNC: Defer ALL work to timer so we return IMMEDIATELY
    (run-with-timer
     0 nil
     (lambda ()
       (condition-case err
           (hive-mcp-swarm-slaves--do-spawn-async slave-id name presets work-dir term-backend)
         (error
          ;; Mark slave as errored
          (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves)))
            (plist-put slave :status 'error)
            (plist-put slave :error (error-message-string err)))
          (message "[swarm] Spawn error for %s: %s" slave-id (error-message-string err))))))

    (when (called-interactively-p 'any)
      (message "Spawning slave: %s (async)" slave-id))

    slave-id))

(defun hive-mcp-swarm-slaves--do-spawn-async (slave-id name presets work-dir term-backend)
  "Actually spawn the slave buffer for SLAVE-ID.
Called async from `hive-mcp-swarm-slaves-spawn'.
NAME is the slave name, PRESETS the preset list.
WORK-DIR is the working directory, TERM-BACKEND the terminal type."
  (let* ((slave (gethash slave-id hive-mcp-swarm--slaves))
         (buffer-name (format "%s%s*" hive-mcp-swarm-buffer-prefix name))
         (system-prompt (hive-mcp-swarm-presets-build-system-prompt presets))
         buffer)

    (unless slave
      (error "Slave record not found: %s" slave-id))

    ;; Require terminal emulator (could potentially block on first load)
    (pcase term-backend
      ('claude-code-ide (unless (require 'claude-code-ide nil t)
                          (error "Claude-code-ide is required but not available")))
      ('vterm (unless (require 'vterm nil t)
                (error "Vterm is required but not available")))
      ('eat (unless (require 'eat nil t)
              (error "Eat is required but not available")))
      ('ollama (unless (require 'hive-mcp-ellama nil t)
                 (error "hive-mcp-ellama is required but not available"))))

    ;; Update status to starting
    (plist-put slave :status 'starting)

    ;; Create terminal buffer
    (setq buffer (generate-new-buffer buffer-name))
    (plist-put slave :buffer buffer)

    (let* ((default-directory work-dir)
           (process-environment
            (append
             (list (format "CLAUDE_SWARM_DEPTH=%d" (plist-get slave :depth))
                   (format "CLAUDE_SWARM_MASTER=%s" (or hive-mcp-swarm--session-id "direct"))
                   (format "CLAUDE_SWARM_SLAVE_ID=%s" slave-id))
             process-environment))
           (permission-flag (pcase hive-mcp-swarm-prompt-mode
                              ('bypass "--permission-mode bypassPermissions")
                              (_ "")))
           (claude-cmd (if system-prompt
                           (let ((prompt-file (make-temp-file "swarm-prompt-" nil ".md")))
                             (with-temp-file prompt-file
                               (insert system-prompt))
                             (format "cd %s && %s %s --system-prompt %s"
                                     (shell-quote-argument work-dir)
                                     hive-mcp-swarm-claude-command
                                     permission-flag
                                     (shell-quote-argument prompt-file)))
                         (format "cd %s && %s %s"
                                 (shell-quote-argument work-dir)
                                 hive-mcp-swarm-claude-command
                                 permission-flag))))

      (pcase term-backend
        ('claude-code-ide
         ;; Use full claude-code-ide session with MCP WebSocket integration
         ;; Kill the pre-created buffer since claude-code-ide creates its own
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         ;; Ensure MCP server is running (starts it if needed)
         (let* ((port (when (fboundp 'claude-code-ide-mcp-server-ensure-server)
                        (claude-code-ide-mcp-server-ensure-server)))
                (_ (unless port
                     (error "Failed to start MCP server for claude-code-ide backend")))
                (result (claude-code-ide--create-terminal-session
                         buffer-name
                         work-dir
                         port
                         nil  ;; continue
                         nil  ;; resume
                         slave-id)))
           (setq buffer (car result))
           (plist-put slave :buffer buffer)
           (plist-put slave :process (cdr result))
           ;; Apply system prompt if present via CLI command after session starts
           (when system-prompt
             (let ((prompt-to-send system-prompt))
               (run-at-time 1.0 nil
                            (lambda ()
                              (when (buffer-live-p buffer)
                                (hive-mcp-swarm-terminal-send
                                 buffer
                                 (format "/system-prompt %s"
                                         (shell-quote-argument prompt-to-send))
                                 'claude-code-ide))))))))
        ('vterm
         (with-current-buffer buffer
           (vterm-mode)
           (run-at-time 0.5 nil
                        (lambda ()
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (vterm-send-string claude-cmd)
                              (vterm-send-return)))))))
        ('eat
         (with-current-buffer buffer
           (eat-mode)
           (eat-exec buffer "swarm-shell" "/bin/bash" nil '("-l")))
         (run-at-time 0.5 nil
                      (lambda ()
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (when (and (boundp 'eat-terminal) eat-terminal)
                              (eat-term-send-string eat-terminal claude-cmd)
                              (eat-term-send-string eat-terminal "\r")))))))
        ('ollama
         ;; Ollama backend uses hive-mcp-ellama for local LLM inference
         ;; No terminal buffer needed - uses async elisp callbacks
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         ;; Get model from slave metadata or use default
         (let ((model (or (plist-get slave :model)
                          (bound-and-true-p hive-mcp-ellama-default-model)
                          "devstral")))
           ;; Spawn via ellama addon
           (hive-mcp-ellama-swarm-spawn slave-id name model)
           ;; Store model in slave record
           (plist-put slave :model model)
           (plist-put slave :backend 'ollama)
           ;; Mark as idle immediately since no terminal startup
           (plist-put slave :status 'idle)))))

  ;; Log completion
  (message "[swarm] Spawned %s buffer created" slave-id)

  ;; Schedule status transition to idle
  (run-at-time
   3 nil
   (lambda ()
     (when-let* ((s (gethash slave-id hive-mcp-swarm--slaves)))
       (when (memq (plist-get s :status) '(starting spawning))
         (plist-put s :status 'idle))))))

;;;; Kill Functions:

(defun hive-mcp-swarm-slaves-kill (slave-id)
  "Kill slave SLAVE-ID without prompts.
Force-kills the buffer to prevent blocking on process/unsaved prompts.
Handles vterm/eat process cleanup to ensure no confirmation dialogs.
Emits slave-killed event via channel for push-based updates."
  (interactive
   (list (completing-read "Kill slave: "
                          (hash-table-keys hive-mcp-swarm--slaves))))
  (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves)))
    (let ((buffer (plist-get slave :buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; Step 1: Mark buffer as unmodified to prevent "save buffer?" prompts
          (set-buffer-modified-p nil)
          ;; Step 2: Disable process query-on-exit for ALL processes in this buffer
          ;; This prevents "Buffer has running process; kill it?" prompts
          (when-let* ((proc (get-buffer-process buffer)))
            (set-process-query-on-exit-flag proc nil))
          ;; Also handle vterm's internal process tracking if present
          (when (and (boundp 'vterm--process) vterm--process
                     (process-live-p vterm--process))
            (set-process-query-on-exit-flag vterm--process nil)))
        ;; Step 3: Kill buffer with ALL hooks disabled
        ;; - kill-buffer-query-functions: prevents "process running" prompts
        ;; - kill-buffer-hook: prevents any cleanup hooks from blocking
        ;; - vterm-exit-functions: prevents vterm cleanup hooks
        (let ((kill-buffer-query-functions nil)
              (kill-buffer-hook nil)
              (vterm-exit-functions nil))
          (kill-buffer buffer))))
    ;; Emit slave-killed event via channel before cleanup
    (hive-mcp-swarm-events-emit-slave-killed slave-id)
    (remhash slave-id hive-mcp-swarm--slaves)
    ;; Clear prompt tracking for this slave
    (hive-mcp-swarm-prompts-clear-slave slave-id)
    (message "Killed slave: %s" slave-id)))

(defun hive-mcp-swarm-slaves-kill-all ()
  "Kill all slaves."
  (interactive)
  (let ((count 0))
    (maphash (lambda (id _)
               (hive-mcp-swarm-slaves-kill id)
               (cl-incf count))
             hive-mcp-swarm--slaves)
    (message "Killed %d slaves" count)))

;;;; Accessors:

(defun hive-mcp-swarm-slaves-get-spawn-timestamps ()
  "Get the list of recent spawn timestamps."
  hive-mcp-swarm-slaves--spawn-timestamps)

(defun hive-mcp-swarm-slaves-get-spawn-count ()
  "Get the count of recent spawns within the rate limit window."
  (length hive-mcp-swarm-slaves--spawn-timestamps))

;;;; Lifecycle:

(defun hive-mcp-swarm-slaves-init ()
  "Initialize slaves module."
  (setq hive-mcp-swarm-slaves--spawn-timestamps nil))

(defun hive-mcp-swarm-slaves-shutdown ()
  "Shutdown slaves module."
  (setq hive-mcp-swarm-slaves--spawn-timestamps nil))

(provide 'hive-mcp-swarm-slaves)
;;; hive-mcp-swarm-slaves.el ends here
