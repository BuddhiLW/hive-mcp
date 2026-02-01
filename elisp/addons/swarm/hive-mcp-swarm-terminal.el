;;; hive-mcp-swarm-terminal.el --- Terminal backend abstraction for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Terminal backend abstraction layer for hive-mcp-swarm.
;; Provides unified interface for vterm, eat, and claude-code-ide backends.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles terminal I/O
;; - Open/Closed: New backends via dispatch, not modification
;; - Dependency Inversion: Callers depend on abstraction, not concrete backends
;;
;; All operations are NON-BLOCKING to prevent freezing single-threaded Emacs.

;;; Code:

(require 'cl-lib)

;; Soft dependencies - checked at runtime
(declare-function vterm "vterm")
(declare-function vterm-mode "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

(declare-function eat "eat")
(declare-function eat-mode "eat")
(declare-function eat-exec "eat")
(declare-function eat-term-send-string "eat")

(declare-function claude-code-ide--terminal-send-string "claude-code-ide")
(declare-function claude-code-ide--terminal-send-return "claude-code-ide")
(declare-function claude-code-ide--create-terminal-session "claude-code-ide")
(declare-function claude-code-ide-mcp-server-ensure-server "claude-code-ide-mcp-server")

(declare-function hive-mcp-ellama-dispatch "hive-mcp-ellama")

;; Forward declarations for event emission (from hive-mcp-swarm-events)
(declare-function hive-mcp-swarm-events-emit-auto-started "hive-mcp-swarm-events")
(declare-function hive-mcp-swarm-events-emit-auto-error "hive-mcp-swarm-events")
(declare-function hive-mcp-swarm-events-emit-auto-completed "hive-mcp-swarm-events")
(declare-function hive-mcp-swarm-events-emit-idle-timeout "hive-mcp-swarm-events")
(declare-function hive-mcp-swarm-events-emit-prompt-stall "hive-mcp-swarm-events")

;; Forward declarations for prompt detection (from hive-mcp-swarm-prompts)
(declare-function hive-mcp-swarm-prompts-find-pending "hive-mcp-swarm-prompts")
(declare-function hive-mcp-swarm-prompts--send-desktop-notification "hive-mcp-swarm-prompts")
(declare-function hive-mcp-swarm-events-emit-ling-ready-for-wrap "hive-mcp-swarm-events")

;; External state from swarm module
(defvar hive-mcp-swarm--slaves)

;;;; Buffer-Local State (Auto-Completion Detection):

(defvar-local hive-mcp-swarm-terminal--working-p nil
  "Non-nil when the terminal is actively processing a task.
Set to t when `hive-mcp-swarm-terminal-send' is called with a task prompt.
Set to nil when the terminal becomes ready again.")

(defvar-local hive-mcp-swarm-terminal--task-start-time nil
  "Time when the current task started (float-time).
Used to calculate task duration for auto-shout events.")

(defvar-local hive-mcp-swarm-terminal--pending-prompt nil
  "The prompt text that was sent, for completion detection.")

;;;; Customization:

(defgroup hive-mcp-swarm-terminal nil
  "Terminal backend settings for swarm."
  :group 'hive-mcp-swarm
  :prefix "hive-mcp-swarm-terminal-")

(defcustom hive-mcp-swarm-terminal-backend 'claude-code-ide
  "Terminal backend for slave sessions.
- `claude-code-ide': MCP WebSocket integration (recommended)
- `vterm': Native terminal (requires native compilation)
- `eat': Pure Emacs Lisp (experimental)"
  :type '(choice (const :tag "claude-code-ide (recommended)" claude-code-ide)
                 (const :tag "vterm" vterm)
                 (const :tag "eat (experimental)" eat))
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-send-delay 0.1
  "Delay in seconds between send-string and send-return.
Ensures terminal processes text before return is sent."
  :type 'float
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-ready-timeout 10
  "Seconds to wait for terminal to become ready."
  :type 'integer
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-prompt-marker "❯"
  "Marker indicating Claude CLI is ready for input.
This is the universal Claude Code prompt character."
  :type 'string
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-ready-search-window 1500
  "Number of characters from buffer end to search for ready marker.
Claude Code UI includes separator lines (────) and status displays
after the prompt marker, which can push it 600+ chars from buffer end.
Default 1500 ensures marker is found in typical UI configurations."
  :type 'integer
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-auto-shout t
  "If non-nil, automatically emit hivemind shout when lings complete tasks.
When a ling finishes a task (transitions from working to ready),
an auto-completion event is emitted without requiring explicit shout calls."
  :type 'boolean
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-completion-poll-interval 1.0
  "Interval in seconds between completion checks for working buffers."
  :type 'float
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-error-patterns
  '(;; Claude CLI errors
    ("Error:" . "cli-error")
    ("error:" . "cli-error")
    ("ERROR" . "cli-error")
    ;; Tool/execution errors
    ("failed with exit code" . "tool-error")
    ("Command failed" . "tool-error")
    ("Traceback (most recent" . "tool-error")
    ("Exception:" . "tool-error")
    ("Panic:" . "tool-error")
    ;; API errors
    ("API error" . "api-error")
    ("rate limit" . "api-error")
    ("authentication failed" . "api-error")
    ;; Process errors
    ("Killed" . "process-error")
    ("Segmentation fault" . "process-error")
    ("Out of memory" . "process-error"))
  "Alist of (PATTERN . ERROR-TYPE) for detecting errors in terminal output.
PATTERN is a string to search for in the recent terminal output.
ERROR-TYPE is a category string for the error."
  :type '(alist :key-type string :value-type string)
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-error-search-lines 100
  "Number of lines to search for error patterns in terminal output."
  :type 'integer
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-idle-timeout 30.0
  "Seconds of inactivity before a working slave is considered idle.
Used by Layer 2 convergence pattern to detect lings that go silent
without shouting progress."
  :type 'float
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-idle-poll-interval 5.0
  "Interval in seconds between idle detection checks."
  :type 'float
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-auto-wrap t
  "If non-nil, automatically trigger wrap when lings complete tasks.
When a ling finishes a task and becomes idle, the auto-wrap hook will:
1. Emit ling-ready-for-wrap event to trigger wrap workflow
2. Only wrap once per session (prevents duplicate wraps)
3. Emit wrap_notify event for coordinator permeation"
  :type 'boolean
  :group 'hive-mcp-swarm-terminal)

;;;; Completion Watcher State:

(defvar hive-mcp-swarm-terminal--completion-timer nil
  "Timer for polling task completion across all working slave buffers.")

(defvar hive-mcp-swarm-terminal--completion-callback nil
  "Callback to invoke when task completion is detected.
Called with (buffer slave-id duration-secs).")

;;;; Layer 2: Idle Detection State (Terminal Introspection)

(defvar hive-mcp-swarm-terminal--activity-timestamps (make-hash-table :test 'equal)
  "Hash of slave-id -> last-activity-timestamp (float-time).
Updated when terminal output is detected for a slave.")

(defvar hive-mcp-swarm-terminal--last-shout-timestamps (make-hash-table :test 'equal)
  "Hash of slave-id -> last-shout-timestamp (float-time).
Updated when a hivemind_shout is received from a slave.
Used to distinguish legitimately quiet lings from dead ones.")

(defvar hive-mcp-swarm-terminal--idle-timer nil
  "Timer for polling idle detection across working slaves.")

(defvar hive-mcp-swarm-terminal--idle-emitted (make-hash-table :test 'equal)
  "Hash of slave-id -> t for slaves that have already had idle event emitted.
Prevents duplicate idle events for the same stall.")

(defvar hive-mcp-swarm-terminal--buffer-sizes (make-hash-table :test 'equal)
  "Hash of slave-id -> last-known-buffer-size.
Used to detect terminal output for activity tracking.")

;;;; Auto-Wrap State (Session Crystallization):

(defvar hive-mcp-swarm-terminal--wrapped-sessions (make-hash-table :test 'equal)
  "Hash of slave-id -> t for sessions that have already been auto-wrapped.
Prevents duplicate wraps for the same session.")

;;;; Backend Detection:

(defun hive-mcp-swarm-terminal-backend-available-p (backend)
  "Check if BACKEND is available."
  (pcase backend
    ('claude-code-ide (require 'claude-code-ide nil t))
    ('vterm (and (require 'vterm nil t)
                 (fboundp 'vterm-mode)
                 (fboundp 'vterm-send-string)))
    ('eat (require 'eat nil t))
    (_ nil)))

(defun hive-mcp-swarm-terminal-detect-buffer-backend (buffer)
  "Detect terminal backend used in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ;; claude-code-ide uses vterm internally - check BEFORE generic vterm
       ((and (derived-mode-p 'vterm-mode)
             (bound-and-true-p claude-code-ide-session-id))
        'claude-code-ide)
       ((derived-mode-p 'vterm-mode) 'vterm)
       ((derived-mode-p 'eat-mode) 'eat)
       (t nil)))))

;;;; Helpers:

(defun hive-mcp-swarm-terminal--find-slave-by-buffer (buffer)
  "Find slave-id that owns BUFFER by iterating slaves hash."
  (when (and (buffer-live-p buffer)
             (boundp 'hive-mcp-swarm--slaves)
             (hash-table-p hive-mcp-swarm--slaves))
    (catch 'found
      (maphash (lambda (id slave)
                 (when (eq (plist-get slave :buffer) buffer)
                   (throw 'found id)))
               hive-mcp-swarm--slaves)
      nil)))

;;;; Error Detection:

(defun hive-mcp-swarm-terminal--detect-error (buffer)
  "Detect error patterns in BUFFER's recent output.
Returns (ERROR-TYPE . ERROR-PREVIEW) if error found, nil otherwise.
Searches the last `hive-mcp-swarm-terminal-error-search-lines' lines."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (let* ((search-start (save-excursion
                               (forward-line (- hive-mcp-swarm-terminal-error-search-lines))
                               (point)))
               (recent-text (buffer-substring-no-properties search-start (point-max))))
          (catch 'found-error
            (dolist (pattern-pair hive-mcp-swarm-terminal-error-patterns)
              (let ((pattern (car pattern-pair))
                    (error-type (cdr pattern-pair)))
                (when (string-match-p (regexp-quote pattern) recent-text)
                  ;; Extract error preview: find the line containing the pattern
                  (let ((error-preview
                         (when (string-match (concat "^.*" (regexp-quote pattern) ".*$")
                                             recent-text)
                           (substring recent-text
                                      (match-beginning 0)
                                      (min (match-end 0)
                                           (+ (match-beginning 0) 200))))))
                    (throw 'found-error (cons error-type (or error-preview pattern)))))))
            nil))))))

(defun hive-mcp-swarm-terminal--truncate-for-preview (text max-length)
  "Truncate TEXT to MAX-LENGTH chars, adding ellipsis if needed."
  (if (and text (> (length text) max-length))
      (concat (substring text 0 (- max-length 3)) "...")
    (or text "")))

;;;; Non-Blocking Send Operations:

(defun hive-mcp-swarm-terminal--send-vterm (buffer text)
  "Send TEXT to vterm BUFFER, then return. Non-blocking."
  (with-current-buffer buffer
    (vterm-send-string text)
    ;; Schedule return via timer - never blocks
    (run-at-time hive-mcp-swarm-terminal-send-delay nil
                 (lambda ()
                   (condition-case err
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (vterm-send-return)))
                     (error
                      (message "[swarm-terminal] Timer error in vterm send-return: %s"
                               (error-message-string err))))))))

(defun hive-mcp-swarm-terminal--send-eat (buffer text)
  "Send TEXT to eat BUFFER, then return. Non-blocking."
  (with-current-buffer buffer
    (when (and (boundp 'eat-terminal) eat-terminal)
      (eat-term-send-string eat-terminal text)
      ;; Schedule return via timer - never blocks
      (run-at-time hive-mcp-swarm-terminal-send-delay nil
                   (lambda ()
                     (condition-case err
                         (when (and (buffer-live-p buffer)
                                    (boundp 'eat-terminal)
                                    eat-terminal)
                           (with-current-buffer buffer
                             (eat-term-send-string eat-terminal "\r")))
                       (error
                        (message "[swarm-terminal] Timer error in eat send-return: %s"
                                 (error-message-string err)))))))))

(defun hive-mcp-swarm-terminal--send-ollama (buffer text)
  "Send TEXT to Ollama via ellama. Non-blocking.
BUFFER is the swarm worker buffer (used for logging).
Routes to hive-mcp-ellama for actual inference."
  (let* ((slave-id (hive-mcp-swarm-terminal--find-slave-by-buffer buffer))
         (slave (and slave-id (gethash slave-id hive-mcp-swarm--slaves)))
         (model (or (and slave (plist-get slave :model)) "devstral-small-2")))
    (if (fboundp 'hive-mcp-ellama-dispatch)
        (hive-mcp-ellama-dispatch
         text
         model
         (lambda (response)
           ;; Log response to worker buffer
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (goto-char (point-max))
               (insert (format "\n--- Ollama Response ---\n%s\n" response))))
           ;; Update slave status
           (when slave
             (plist-put slave :last-response response)
             (plist-put slave :status 'idle))))
      (error "hive-mcp-ellama not available"))))

(defun hive-mcp-swarm-terminal--send-claude-code-ide (buffer text)
  "Send TEXT via claude-code-ide abstraction.
Uses synchronous sit-for like claude-code-ide-send-prompt does.
Blocking is OK for individual slaves since they're isolated.
Delay scales with text length to allow vterm to process large pastes."
  (with-current-buffer buffer
    (if (fboundp 'claude-code-ide--terminal-send-string)
        (let* ((text-lines (length (split-string text "\n")))
               ;; Base delay + 0.01s per line, max 2s
               (paste-delay (min 2.0 (+ 0.1 (* 0.01 text-lines)))))
          (claude-code-ide--terminal-send-string text)
          ;; Adaptive delay based on text size
          (sit-for paste-delay)
          (when (fboundp 'claude-code-ide--terminal-send-return)
            (claude-code-ide--terminal-send-return)))
      (error "claude-code-ide terminal functions not available"))))

;;;; Public API:

(defun hive-mcp-swarm-terminal-send (buffer text &optional backend)
  "Send TEXT to terminal BUFFER using BACKEND. NON-BLOCKING.
BACKEND defaults to auto-detection or `hive-mcp-swarm-terminal-backend'.
Returns immediately - uses timers for delayed operations.

Side effects:
- Sets buffer-local `hive-mcp-swarm-terminal--working-p' to t
- Records start time in `hive-mcp-swarm-terminal--task-start-time'
- Stores prompt in `hive-mcp-swarm-terminal--pending-prompt'
- Emits auto-started event for task tracking

These are used by the completion watcher to detect task completion
and emit auto-shout events."
  (unless (buffer-live-p buffer)
    (error "Terminal buffer is dead"))
  ;; Set working state for completion detection
  (with-current-buffer buffer
    (setq-local hive-mcp-swarm-terminal--working-p t)
    (setq-local hive-mcp-swarm-terminal--task-start-time (float-time))
    (setq-local hive-mcp-swarm-terminal--pending-prompt text))
  ;; Emit auto-started event for task tracking and record activity for Layer 2
  (when hive-mcp-swarm-terminal-auto-shout
    (when-let* ((slave-id (hive-mcp-swarm-terminal--find-slave-by-buffer buffer)))
      ;; Layer 2: Record activity timestamp for idle detection
      (hive-mcp-swarm-terminal--record-activity slave-id)
      (let ((task-preview (hive-mcp-swarm-terminal--truncate-for-preview text 100)))
        (when (fboundp 'hive-mcp-swarm-events-emit-auto-started)
          (hive-mcp-swarm-events-emit-auto-started slave-id task-preview))
        (message "[swarm-terminal] Auto-shout: %s started task" slave-id))))
  (let ((backend (or backend
                     (hive-mcp-swarm-terminal-detect-buffer-backend buffer)
                     hive-mcp-swarm-terminal-backend)))
    (pcase backend
      ('claude-code-ide
       (hive-mcp-swarm-terminal--send-claude-code-ide buffer text))
      ('vterm
       (hive-mcp-swarm-terminal--send-vterm buffer text))
      ('eat
       (hive-mcp-swarm-terminal--send-eat buffer text))
      ('ollama
       (hive-mcp-swarm-terminal--send-ollama buffer text))
      (_
       (error "Unknown terminal backend: %s" backend)))))

(defun hive-mcp-swarm-terminal-ready-p (buffer)
  "Check if terminal BUFFER is ready for input (non-blocking check).
Searches for the Claude Code prompt marker (❯) within the last
`hive-mcp-swarm-terminal-ready-search-window' characters.

Claude Code UI structure can push the ❯ marker 600+ chars from buffer end:
  ❯ [prompt line with padding]
  ──────────────── [separator line ~200 chars]
  [status/mode info]
  [trailing whitespace]

Uses a 1500 char search window by default to reliably find the marker."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (let ((search-start (max (point-min)
                                 (- (point-max) hive-mcp-swarm-terminal-ready-search-window))))
          (search-backward hive-mcp-swarm-terminal-prompt-marker
                           search-start t))))))

(defun hive-mcp-swarm-terminal-wait-ready (buffer callback &optional timeout-secs)
  "Wait for BUFFER to be ready, then call CALLBACK. NON-BLOCKING.
Uses timer-based polling instead of blocking sit-for loop.
TIMEOUT-SECS defaults to `hive-mcp-swarm-terminal-ready-timeout'.
CALLBACK receives (buffer success-p) arguments."
  (let* ((timeout (or timeout-secs hive-mcp-swarm-terminal-ready-timeout))
         (start-time (float-time))
         (check-interval 0.2)
         timer)
    (setq timer
          (run-with-timer
           0 check-interval
           (lambda ()
             (condition-case err
                 (cond
                  ;; Buffer died
                  ((not (buffer-live-p buffer))
                   (cancel-timer timer)
                   (funcall callback buffer nil))
                  ;; Ready!
                  ((hive-mcp-swarm-terminal-ready-p buffer)
                   (cancel-timer timer)
                   (funcall callback buffer t))
                  ;; Timeout
                  ((> (- (float-time) start-time) timeout)
                   (cancel-timer timer)
                   (message "[swarm-terminal] Timeout waiting for ready: %s"
                            (buffer-name buffer))
                   (funcall callback buffer nil)))
               (error
                (cancel-timer timer)
                (message "[swarm-terminal] Timer error in wait-ready: %s"
                         (error-message-string err))
                (ignore-errors (funcall callback buffer nil)))))))))

;;;; Buffer Creation:

(defun hive-mcp-swarm-terminal-create-buffer (name dir backend &optional slave-id)
  "Create terminal buffer NAME in DIR using BACKEND.
SLAVE-ID is optional identifier for claude-code-ide sessions.
Returns the buffer (async startup - may not be immediately ready)."
  (unless (hive-mcp-swarm-terminal-backend-available-p backend)
    (error "Terminal backend not available: %s" backend))
  (let ((default-directory dir))
    (pcase backend
      ('claude-code-ide
       (let* ((port (when (fboundp 'claude-code-ide-mcp-server-ensure-server)
                      (claude-code-ide-mcp-server-ensure-server)))
              (_ (unless port
                   (error "Failed to start MCP server")))
              (result (claude-code-ide--create-terminal-session
                       name dir port nil nil slave-id)))
         (car result)))  ; Return buffer
      ('vterm
       (let ((buf (generate-new-buffer name)))
         (with-current-buffer buf
           (vterm-mode))
         buf))
      ('eat
       (let ((buf (generate-new-buffer name)))
         (with-current-buffer buf
           (eat-mode)
           (eat-exec buf "swarm-shell" "/bin/bash" nil '("-l")))
         buf)))))

(defun hive-mcp-swarm-terminal-kill-buffer (buffer)
  "Kill terminal BUFFER without prompts.
Handles process cleanup to prevent confirmation dialogs."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Prevent save prompts
      (set-buffer-modified-p nil)
      ;; Disable process query-on-exit
      (when-let* ((proc (get-buffer-process buffer)))
        (set-process-query-on-exit-flag proc nil))
      ;; Handle vterm's internal process
      (when (and (boundp 'vterm--process)
                 (processp vterm--process)
                 (process-live-p vterm--process))
        (set-process-query-on-exit-flag vterm--process nil)))
    ;; Kill with all hooks disabled
    (let ((kill-buffer-query-functions nil)
          (kill-buffer-hook nil)
          (vterm-exit-functions nil))
      (kill-buffer buffer))))

;;;; Completion Watcher (Auto-Shout):

(defun hive-mcp-swarm-terminal--check-buffer-completion (buffer)
  "Check if BUFFER has completed its task (working → ready transition).
Returns a plist with completion info if done, nil otherwise.

Return format:
  (:slave-id ID :duration SECS :status STATUS [:error-type TYPE :error-preview TEXT])

STATUS is one of:
- \"completed\" - task finished normally
- \"error\" - error pattern detected in output"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Flush vterm process output before inspecting buffer content
      ;; Without this, events only fire when manually displaying the buffer
      (when-let* ((proc (get-buffer-process buffer)))
        (accept-process-output proc 0.01))
      (when (and hive-mcp-swarm-terminal--working-p
                 (hive-mcp-swarm-terminal-ready-p buffer))
        ;; Transition detected: working → ready
        (let* ((slave-id (hive-mcp-swarm-terminal--find-slave-by-buffer buffer))
               (start-time hive-mcp-swarm-terminal--task-start-time)
               (duration (when start-time
                           (- (float-time) start-time)))
               ;; Check for error patterns in output
               (error-info (hive-mcp-swarm-terminal--detect-error buffer))
               (status (if error-info "error" "completed")))
          ;; Clear working state
          (setq-local hive-mcp-swarm-terminal--working-p nil)
          (setq-local hive-mcp-swarm-terminal--task-start-time nil)
          (setq-local hive-mcp-swarm-terminal--pending-prompt nil)
          ;; Return completion info with status
          (when slave-id
            (if error-info
                (list :slave-id slave-id
                      :duration duration
                      :status status
                      :error-type (car error-info)
                      :error-preview (cdr error-info))
              (list :slave-id slave-id
                    :duration duration
                    :status status))))))))

(defun hive-mcp-swarm-terminal--check-buffer-activity (slave-id buffer)
  "Check if BUFFER has new output since last check for SLAVE-ID.
Updates activity timestamp if output detected.
Returns t if activity was recorded, nil otherwise."
  (when (buffer-live-p buffer)
    ;; Flush vterm process output before checking buffer size
    ;; Without this, activity detection only works when buffer is displayed
    (when-let* ((proc (get-buffer-process buffer)))
      (accept-process-output proc 0.01))
    (let* ((current-size (buffer-size buffer))
           (last-size (gethash slave-id hive-mcp-swarm-terminal--buffer-sizes 0)))
      (puthash slave-id current-size hive-mcp-swarm-terminal--buffer-sizes)
      (when (> current-size last-size)
        ;; Buffer grew - record activity
        (hive-mcp-swarm-terminal--record-activity slave-id)
        t))))

(defun hive-mcp-swarm-terminal--completion-watcher-tick ()
  "Check all working slave buffers for completion.
Called periodically by the completion watcher timer.

Detects both successful completions and errors, emitting the
appropriate auto-shout event for each.

Also tracks buffer output for Layer 2 activity detection."
  (condition-case err
      (when (and hive-mcp-swarm-terminal-auto-shout
                 (boundp 'hive-mcp-swarm--slaves)
                 (hash-table-p hive-mcp-swarm--slaves))
        (maphash
     (lambda (slave-id slave)
       (when-let* ((buffer (plist-get slave :buffer)))
         ;; Layer 2: Check for buffer activity (output)
         (hive-mcp-swarm-terminal--check-buffer-activity slave-id buffer))
       ;; Check for completion
       (when-let* ((buffer (plist-get slave :buffer))
                   (completion-info (hive-mcp-swarm-terminal--check-buffer-completion buffer)))
         (let* ((completed-slave-id (plist-get completion-info :slave-id))
                (duration (plist-get completion-info :duration))
                (status (plist-get completion-info :status))
                (error-type (plist-get completion-info :error-type))
                (error-preview (plist-get completion-info :error-preview)))
           ;; Emit appropriate auto-shout event
           (if (string= status "error")
               (progn
                 ;; Emit auto-error event
                 (when (fboundp 'hive-mcp-swarm-events-emit-auto-error)
                   (hive-mcp-swarm-events-emit-auto-error
                    completed-slave-id duration error-type error-preview))
                 (message "[swarm-terminal] Auto-shout: %s error detected (%s, %.1fs)"
                          completed-slave-id error-type (or duration 0)))
             ;; Emit auto-completed event
             (when (fboundp 'hive-mcp-swarm-events-emit-auto-completed)
               (hive-mcp-swarm-events-emit-auto-completed
                completed-slave-id duration status))
             (message "[swarm-terminal] Auto-shout: %s completed task (%.1fs)"
                      completed-slave-id (or duration 0))
             ;; Auto-wrap hook: trigger wrap on task completion
             ;; Only for successful completions (not errors)
             (hive-mcp-swarm-terminal-trigger-auto-wrap
              completed-slave-id "task-completed"))
           ;; Invoke callback if registered (for backward compatibility)
           (when (functionp hive-mcp-swarm-terminal--completion-callback)
             (funcall hive-mcp-swarm-terminal--completion-callback
                      buffer completed-slave-id duration status
                      error-type error-preview)))))
     hive-mcp-swarm--slaves))
    (error
     (message "[swarm-terminal] Completion watcher tick error: %s"
              (error-message-string err)))))

(defun hive-mcp-swarm-terminal-start-completion-watcher (callback)
  "Start the completion watcher timer with CALLBACK.
CALLBACK is called with (buffer slave-id duration-secs) on completion."
  (hive-mcp-swarm-terminal-stop-completion-watcher)
  (setq hive-mcp-swarm-terminal--completion-callback callback)
  (setq hive-mcp-swarm-terminal--completion-timer
        (run-with-timer
         hive-mcp-swarm-terminal-completion-poll-interval
         hive-mcp-swarm-terminal-completion-poll-interval
         #'hive-mcp-swarm-terminal--completion-watcher-tick))
  (message "[swarm-terminal] Completion watcher started (interval: %.1fs)"
           hive-mcp-swarm-terminal-completion-poll-interval))

(defun hive-mcp-swarm-terminal-stop-completion-watcher ()
  "Stop the completion watcher timer."
  (when hive-mcp-swarm-terminal--completion-timer
    (cancel-timer hive-mcp-swarm-terminal--completion-timer)
    (setq hive-mcp-swarm-terminal--completion-timer nil)
    (setq hive-mcp-swarm-terminal--completion-callback nil)
    (message "[swarm-terminal] Completion watcher stopped")))

(defun hive-mcp-swarm-terminal-reset-working-state (buffer)
  "Reset working state for BUFFER.
Use this to manually clear stale working state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local hive-mcp-swarm-terminal--working-p nil)
      (setq-local hive-mcp-swarm-terminal--task-start-time nil)
      (setq-local hive-mcp-swarm-terminal--pending-prompt nil))))

;;;; Layer 2: Idle Detection (Terminal Introspection)
;;
;; This implements the 4-Layer Convergence Pattern Layer 2:
;; Detect when lings go idle without shouting by monitoring terminal activity.
;;
;; The logic:
;; 1. Track last terminal activity timestamp per slave
;; 2. Track last shout timestamp per slave (populated by shout handlers)
;; 3. If no activity for N seconds AND no recent shout → emit idle-timeout event
;; 4. This catches lings that crash, hang, or forget to shout completion

(defun hive-mcp-swarm-terminal--record-activity (slave-id)
  "Record that SLAVE-ID had terminal activity (output).
Called when terminal buffer content changes."
  (puthash slave-id (float-time) hive-mcp-swarm-terminal--activity-timestamps)
  ;; Clear any previous idle-emitted flag since there's new activity
  (remhash slave-id hive-mcp-swarm-terminal--idle-emitted))

(defun hive-mcp-swarm-terminal--get-activity-timestamp (slave-id)
  "Get the last activity timestamp for SLAVE-ID, or nil if none."
  (gethash slave-id hive-mcp-swarm-terminal--activity-timestamps))

(defun hive-mcp-swarm-terminal--record-shout (slave-id)
  "Record that SLAVE-ID sent a hivemind_shout.
Called when shout events are received from the hivemind."
  (puthash slave-id (float-time) hive-mcp-swarm-terminal--last-shout-timestamps)
  ;; Clear idle-emitted flag since ling is communicating
  (remhash slave-id hive-mcp-swarm-terminal--idle-emitted))

(defun hive-mcp-swarm-terminal--get-shout-timestamp (slave-id)
  "Get the last shout timestamp for SLAVE-ID, or nil if none."
  (gethash slave-id hive-mcp-swarm-terminal--last-shout-timestamps))

(defun hive-mcp-swarm-terminal--slave-idle-p (slave-id)
  "Check if SLAVE-ID is considered idle based on timestamps.
Returns t if:
- Activity timestamp exists AND is older than `hive-mcp-swarm-terminal-idle-timeout'
- AND (shout timestamp doesn't exist OR is also old)

Returns nil if:
- No activity timestamp (unknown slave)
- Activity is recent
- Shout is recent (ling communicated recently)"
  (let* ((now (float-time))
         (activity-time (hive-mcp-swarm-terminal--get-activity-timestamp slave-id))
         (shout-time (hive-mcp-swarm-terminal--get-shout-timestamp slave-id))
         (timeout hive-mcp-swarm-terminal-idle-timeout))
    ;; Must have activity timestamp to be considered
    (when activity-time
      (let ((activity-age (- now activity-time))
            (shout-age (if shout-time (- now shout-time) most-positive-fixnum)))
        ;; Idle if BOTH activity AND shout are old
        (and (> activity-age timeout)
             (> shout-age timeout))))))

(defun hive-mcp-swarm-terminal--slave-needs-idle-event-p (slave-id)
  "Check if SLAVE-ID needs an idle-timeout event emitted.
Returns t only if:
- Slave is in 'working status
- Slave is idle (activity + shout both old)
- Haven't already emitted idle event for this stall"
  (and (not (gethash slave-id hive-mcp-swarm-terminal--idle-emitted))
       (hive-mcp-swarm-terminal--slave-idle-p slave-id)
       ;; Must be in working state
       (when (and (boundp 'hive-mcp-swarm--slaves)
                  (hash-table-p hive-mcp-swarm--slaves))
         (let ((slave (gethash slave-id hive-mcp-swarm--slaves)))
           (eq (plist-get slave :status) 'working)))))

(defun hive-mcp-swarm-terminal--clear-slave-timestamps (slave-id)
  "Clear all timestamp records for SLAVE-ID.
Called when a slave is killed."
  (remhash slave-id hive-mcp-swarm-terminal--activity-timestamps)
  (remhash slave-id hive-mcp-swarm-terminal--last-shout-timestamps)
  (remhash slave-id hive-mcp-swarm-terminal--idle-emitted)
  (remhash slave-id hive-mcp-swarm-terminal--buffer-sizes))

(defun hive-mcp-swarm-terminal--idle-watcher-tick ()
  "Check all working slaves for idle timeout.
Called periodically by the idle watcher timer.

For each slave that has gone idle without shouting, emits
an idle-timeout event to alert the coordinator.

If the slave has a pending prompt (from hive-mcp-swarm-prompts),
emits a prompt-stall event instead - this is more urgent as it
indicates the coordinator needs to respond to unblock the ling."
  (condition-case err
      (when (and (boundp 'hive-mcp-swarm--slaves)
                 (hash-table-p hive-mcp-swarm--slaves))
    ;; First pass: flush all process output so buffer content is fresh
    ;; Without this, idle detection only works when hovering over buffers
    (maphash
     (lambda (_slave-id slave)
       (when-let* ((buffer (plist-get slave :buffer)))
         (when (buffer-live-p buffer)
           (when-let* ((proc (get-buffer-process buffer)))
             (accept-process-output proc 0.01)))))
     hive-mcp-swarm--slaves)
    ;; Second pass: check for idle timeouts with fresh data
    (maphash
     (lambda (slave-id _slave)
       (when (hive-mcp-swarm-terminal--slave-needs-idle-event-p slave-id)
         ;; Mark as emitted to prevent duplicates
         (puthash slave-id t hive-mcp-swarm-terminal--idle-emitted)
         ;; Calculate how long they've been idle
         (let* ((activity-time (hive-mcp-swarm-terminal--get-activity-timestamp slave-id))
                (idle-duration (if activity-time
                                   (- (float-time) activity-time)
                                 0))
                ;; Check for pending prompt (GAP 2: idle + prompt = prompt-stall)
                (pending-prompt (when (fboundp 'hive-mcp-swarm-prompts-find-pending)
                                  (hive-mcp-swarm-prompts-find-pending slave-id)))
                (prompt-text (and pending-prompt (plist-get pending-prompt :prompt))))
           (if pending-prompt
               ;; URGENT: Idle with pending prompt - coordinator needs to respond!
               (progn
                 (when (fboundp 'hive-mcp-swarm-events-emit-prompt-stall)
                   (hive-mcp-swarm-events-emit-prompt-stall slave-id idle-duration prompt-text))
                 ;; Send urgent desktop notification
                 (when (fboundp 'hive-mcp-swarm-prompts--send-desktop-notification)
                   (hive-mcp-swarm-prompts--send-desktop-notification
                    (format "⚠️ STALLED: %s" slave-id)
                    (format "Waiting %.0fs for response: %s"
                            idle-duration
                            (truncate-string-to-width (or prompt-text "") 80))))
                 (message "[swarm-terminal] Layer 2: %s PROMPT-STALL for %.1fs - coordinator must respond!"
                          slave-id idle-duration))
             ;; Regular idle-timeout (no pending prompt)
             (progn
               (when (fboundp 'hive-mcp-swarm-events-emit-idle-timeout)
                 (hive-mcp-swarm-events-emit-idle-timeout slave-id idle-duration))
               (message "[swarm-terminal] Layer 2: %s idle for %.1fs without shout"
                        slave-id idle-duration))))))
     hive-mcp-swarm--slaves))
    (error
     (message "[swarm-terminal] Idle watcher tick error: %s"
              (error-message-string err)))))

(defun hive-mcp-swarm-terminal-start-idle-watcher ()
  "Start the idle detection watcher timer."
  (hive-mcp-swarm-terminal-stop-idle-watcher)
  (setq hive-mcp-swarm-terminal--idle-timer
        (run-with-timer
         hive-mcp-swarm-terminal-idle-poll-interval
         hive-mcp-swarm-terminal-idle-poll-interval
         #'hive-mcp-swarm-terminal--idle-watcher-tick))
  (message "[swarm-terminal] Layer 2 idle watcher started (timeout: %.0fs, interval: %.0fs)"
           hive-mcp-swarm-terminal-idle-timeout
           hive-mcp-swarm-terminal-idle-poll-interval))

(defun hive-mcp-swarm-terminal-stop-idle-watcher ()
  "Stop the idle detection watcher timer."
  (when hive-mcp-swarm-terminal--idle-timer
    (cancel-timer hive-mcp-swarm-terminal--idle-timer)
    (setq hive-mcp-swarm-terminal--idle-timer nil)
    (message "[swarm-terminal] Layer 2 idle watcher stopped")))

(defun hive-mcp-swarm-terminal-reset-idle-state ()
  "Reset all idle detection state.
Use this when starting a new session."
  (clrhash hive-mcp-swarm-terminal--activity-timestamps)
  (clrhash hive-mcp-swarm-terminal--last-shout-timestamps)
  (clrhash hive-mcp-swarm-terminal--idle-emitted)
  (clrhash hive-mcp-swarm-terminal--buffer-sizes))

;;;; Auto-Wrap Hook (Session Crystallization on Completion)
;;
;; This implements the auto-wrap hook that triggers /wrap when a ling
;; completes its work:
;; 1. Detects when a ling becomes idle (no pending tasks)
;; 2. Automatically triggers wrap workflow
;; 3. Only wraps once per session (prevents duplicates)
;; 4. Emits wrap_notify event for coordinator permeation

(defun hive-mcp-swarm-terminal--session-wrapped-p (slave-id)
  "Check if SLAVE-ID has already been auto-wrapped this session."
  (gethash slave-id hive-mcp-swarm-terminal--wrapped-sessions))

(defun hive-mcp-swarm-terminal--mark-session-wrapped (slave-id)
  "Mark SLAVE-ID as having been auto-wrapped."
  (puthash slave-id t hive-mcp-swarm-terminal--wrapped-sessions))

(defun hive-mcp-swarm-terminal--should-auto-wrap-p (slave-id)
  "Check if SLAVE-ID should trigger auto-wrap.
Returns t only if:
- Auto-wrap is enabled (`hive-mcp-swarm-terminal-auto-wrap')
- Session hasn't already been wrapped
- Slave exists and is not currently working"
  (and hive-mcp-swarm-terminal-auto-wrap
       (not (hive-mcp-swarm-terminal--session-wrapped-p slave-id))
       ;; Check slave isn't currently working on something
       (when (and (boundp 'hive-mcp-swarm--slaves)
                  (hash-table-p hive-mcp-swarm--slaves))
         (let ((slave (gethash slave-id hive-mcp-swarm--slaves)))
           (and slave
                (not (eq (plist-get slave :status) 'working)))))))

(defun hive-mcp-swarm-terminal-trigger-auto-wrap (slave-id reason)
  "Trigger auto-wrap for SLAVE-ID with REASON.
Emits ling-ready-for-wrap event and marks session as wrapped.
Returns t if wrap was triggered, nil if already wrapped or disabled."
  (when (hive-mcp-swarm-terminal--should-auto-wrap-p slave-id)
    ;; Mark as wrapped immediately to prevent duplicates
    (hive-mcp-swarm-terminal--mark-session-wrapped slave-id)
    ;; Emit the event to trigger wrap workflow
    (when (fboundp 'hive-mcp-swarm-events-emit-ling-ready-for-wrap)
      (hive-mcp-swarm-events-emit-ling-ready-for-wrap slave-id reason))
    (message "[swarm-terminal] Auto-wrap triggered for %s (reason: %s)"
             slave-id reason)
    t))

(defun hive-mcp-swarm-terminal-reset-wrap-state ()
  "Reset all auto-wrap state.
Use this when starting a completely new session."
  (clrhash hive-mcp-swarm-terminal--wrapped-sessions))

(defun hive-mcp-swarm-terminal-clear-slave-wrap-state (slave-id)
  "Clear wrap state for SLAVE-ID.
Called when a slave is killed, allowing re-wrap if respawned."
  (remhash slave-id hive-mcp-swarm-terminal--wrapped-sessions))

(provide 'hive-mcp-swarm-terminal)
;;; hive-mcp-swarm-terminal.el ends here
