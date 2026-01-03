;;; emacs-mcp-swarm.el --- Claude swarm orchestration for parallel task execution -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Optional-Requires: vterm OR eat (one terminal emulator required)
;; Keywords: tools, ai, claude, orchestration, parallel
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Swarm orchestration for Claude Code instances.
;;
;; Enables a Master Claude to spawn and control multiple Slave Claude
;; instances for parallel task execution.  Each slave runs in a terminal
;; buffer (vterm or eat) and can be configured with presets (system
;; prompts loaded from markdown files).
;;
;; Terminal choice:
;;   - vterm (recommended): Native terminal, reliable input handling
;;   - eat (experimental): Pure Emacs Lisp, may have input submission issues
;;
;; Set via: (setq emacs-mcp-swarm-terminal 'vterm)  ; default
;;
;; Architecture:
;;
;;   Master Claude (you)
;;         │ MCP tools
;;         v
;;   emacs-mcp-swarm.el (this file)
;;         │ vterm-send-string
;;         v
;;   ┌─────┴─────┐
;;   │  Slaves   │ (vterm buffers running `claude`)
;;   └───────────┘
;;
;; Features:
;; - Spawn slaves with roles/presets
;; - Dispatch prompts to slaves
;; - Collect responses
;; - Load presets from markdown files
;; - Custom preset directories (recursive .md scan)
;; - Combine multiple presets per slave
;;
;; Usage:
;;   ;; Load the addon
;;   (require 'emacs-mcp-swarm)
;;   (emacs-mcp-swarm-mode 1)
;;
;;   ;; Spawn a slave with preset
;;   (emacs-mcp-swarm-spawn "tester" :presets '("tdd" "clarity"))
;;
;;   ;; Dispatch a task
;;   (emacs-mcp-swarm-dispatch "swarm-tester-xxx" "Run all tests")
;;
;;   ;; Check status
;;   (emacs-mcp-swarm-status)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'emacs-mcp-graceful)

;; Soft dependency on channel for push events
(declare-function emacs-mcp-channel-connected-p "emacs-mcp-channel")
(declare-function emacs-mcp-channel-send "emacs-mcp-channel")

;; Soft dependency on vterm
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

;; Soft dependency on eat
(declare-function eat "eat")
(declare-function eat-mode "eat")
(declare-function eat-exec "eat")
(declare-function eat-term-send-string "eat")

;;;; Customization:

(defgroup emacs-mcp-swarm nil
  "Claude swarm orchestration."
  :group 'emacs-mcp
  :prefix "emacs-mcp-swarm-")

(defcustom emacs-mcp-swarm-presets-dir
  (expand-file-name "presets" (file-name-directory
                               (or load-file-name buffer-file-name
                                   default-directory)))
  "Directory containing built-in preset markdown files."
  :type 'directory
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-custom-presets-dirs nil
  "List of custom directories to scan for preset .md files.
Directories are scanned recursively for .md files only."
  :type '(repeat directory)
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-claude-command "claude"
  "Command to invoke Claude Code CLI."
  :type 'string
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-terminal 'vterm
  "Terminal emulator to use for slave sessions.
- `vterm': Use vterm (requires native compilation) - RECOMMENDED
- `eat': Use eat (pure Emacs Lisp) - EXPERIMENTAL, may have input issues"
  :type '(choice (const :tag "vterm (recommended)" vterm)
                 (const :tag "eat (experimental)" eat))
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-max-slaves 30
  "Maximum number of concurrent slave instances."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-max-depth 3
  "Maximum recursion depth (slaves spawning slaves).
Depth 0 = master, 1 = child, 2 = grandchild, 3 = great-grandchild.
Beyond this depth, spawn attempts are blocked to prevent runaway recursion."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-rate-limit-window 60
  "Time window in seconds for rate limiting spawn attempts."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-rate-limit-max-spawns 10
  "Maximum number of spawns allowed within `emacs-mcp-swarm-rate-limit-window'."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-default-timeout 300000
  "Default task timeout in milliseconds (5 minutes)."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-send-verify t
  "If non-nil, verify that prompts were sent to the terminal.
When enabled, dispatch will check the buffer for the prompt text
and retry if not found."
  :type 'boolean
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-send-retries 3
  "Number of retry attempts for sending prompts."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-send-delay 0.2
  "Base delay in seconds between send and return.
This delay increases with each retry (exponential backoff)."
  :type 'float
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-return-delay 0.05
  "Delay in seconds between sending text and sending return.
Helps ensure vterm processes the text before return is sent."
  :type 'float
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-auto-approve t
  "If non-nil, automatically approve tool permission prompts.
Claude Code asks for permission before running tools. This setting
auto-sends 'y' when permission prompts are detected."
  :type 'boolean
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-auto-approve-patterns
  '("Allow\\|Deny" "Yes\\|No" "(y/n)" "[Y/n]" "[y/N]"
    "Do you want to" "Would you like to" "Proceed\\?")
  "Patterns that indicate a permission/confirmation prompt.
When any of these are found in the buffer, and `emacs-mcp-swarm-auto-approve'
is non-nil, automatically send 'y' to approve."
  :type '(repeat string)
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-prompt-mode 'bypass
  "How to handle permission prompts in slaves.
- `bypass': Use --permission-mode bypassPermissions (no prompts)
- `auto': Timer-based auto-approve (legacy behavior)
- `human': Forward prompts to master for human decision"
  :type '(choice (const :tag "Bypass permissions (CLI flag)" bypass)
                 (const :tag "Auto-approve (timer)" auto)
                 (const :tag "Human decision (hooks)" human))
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-prompt-notify t
  "If non-nil, show notification when prompts are pending in human mode."
  :type 'boolean
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-desktop-notify t
  "If non-nil, send desktop notification via notify-send for prompts.
This alerts the human even when running master Claude in a terminal."
  :type 'boolean
  :group 'emacs-mcp-swarm)

(defvar emacs-mcp-swarm-prompts-buffer-name "*Swarm Prompts*"
  "Buffer name for displaying pending prompts.")

(defcustom emacs-mcp-swarm-prompt-marker "❯"
  "Marker indicating Claude is ready for input."
  :type 'string
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-buffer-prefix "*swarm-"
  "Prefix for swarm buffer names."
  :type 'string
  :group 'emacs-mcp-swarm)

;;;; Internal State:

(defvar emacs-mcp-swarm--slaves (make-hash-table :test 'equal)
  "Hash table of slave-id -> slave plist.")

(defvar emacs-mcp-swarm--tasks (make-hash-table :test 'equal)
  "Hash table of task-id -> task plist.")

(defvar emacs-mcp-swarm--presets-cache nil
  "Cache of loaded presets (name -> content).")

(defvar emacs-mcp-swarm--task-counter 0
  "Counter for generating unique task IDs.")

(defvar emacs-mcp-swarm--session-id nil
  "Current swarm session ID.")

(defvar emacs-mcp-swarm--current-depth 0
  "Current recursion depth (set via environment).")

(defvar emacs-mcp-swarm--spawn-timestamps nil
  "List of recent spawn timestamps for rate limiting.")

(defvar emacs-mcp-swarm--ancestry nil
  "Ancestry chain: list of (slave-id . master-id) for loop detection.")

(defvar emacs-mcp-swarm--auto-approve-timer nil
  "Timer for auto-approve watcher.")

(defvar emacs-mcp-swarm--last-approve-positions (make-hash-table :test 'equal)
  "Hash of slave-id -> last checked position to avoid re-approving.")

(defvar emacs-mcp-swarm--pending-prompts nil
  "List of pending prompts awaiting human decision.
Each entry is a plist: (:slave-id ID :prompt TEXT :buffer BUF :timestamp TIME)")

;;;; Channel Event Emission (Push-based updates):

(defun emacs-mcp-swarm--channel-available-p ()
  "Check if the bidirectional channel is available and connected."
  (and (require 'emacs-mcp-channel nil t)
       (fboundp 'emacs-mcp-channel-connected-p)
       (emacs-mcp-channel-connected-p)))

(defun emacs-mcp-swarm--emit-event (event-type data)
  "Emit EVENT-TYPE with DATA through the channel if connected.
EVENT-TYPE should be a string like \"task-completed\".
DATA is an alist of additional event properties."
  (when (emacs-mcp-swarm--channel-available-p)
    (let ((event `(("type" . ,event-type)
                   ("timestamp" . ,(float-time))
                   ("session-id" . ,emacs-mcp-swarm--session-id)
                   ,@data)))
      (condition-case err
          (emacs-mcp-channel-send event)
        (error
         (message "[swarm] Channel emit error: %s" (error-message-string err)))))))

(defun emacs-mcp-swarm--emit-task-completed (task-id slave-id result)
  "Emit task-completed event for TASK-ID from SLAVE-ID with RESULT."
  (emacs-mcp-swarm--emit-event
   "task-completed"
   `(("task-id" . ,task-id)
     ("slave-id" . ,slave-id)
     ("result" . ,result))))

(defun emacs-mcp-swarm--emit-task-failed (task-id slave-id error-msg)
  "Emit task-failed event for TASK-ID from SLAVE-ID with ERROR-MSG."
  (emacs-mcp-swarm--emit-event
   "task-failed"
   `(("task-id" . ,task-id)
     ("slave-id" . ,slave-id)
     ("error" . ,error-msg))))

(defun emacs-mcp-swarm--emit-prompt-shown (slave-id prompt-text)
  "Emit prompt-shown event for SLAVE-ID with PROMPT-TEXT."
  (emacs-mcp-swarm--emit-event
   "prompt-shown"
   `(("slave-id" . ,slave-id)
     ("prompt" . ,prompt-text))))

(defun emacs-mcp-swarm--emit-state-changed (slave-id old-state new-state)
  "Emit state-changed event for SLAVE-ID from OLD-STATE to NEW-STATE."
  (emacs-mcp-swarm--emit-event
   "state-changed"
   `(("slave-id" . ,slave-id)
     ("old-state" . ,(symbol-name old-state))
     ("new-state" . ,(symbol-name new-state)))))

(defun emacs-mcp-swarm--emit-slave-spawned (slave-id name presets)
  "Emit slave-spawned event for SLAVE-ID with NAME and PRESETS."
  (emacs-mcp-swarm--emit-event
   "slave-spawned"
   `(("slave-id" . ,slave-id)
     ("name" . ,name)
     ("presets" . ,(or presets [])))))

(defun emacs-mcp-swarm--emit-slave-killed (slave-id)
  "Emit slave-killed event for SLAVE-ID."
  (emacs-mcp-swarm--emit-event
   "slave-killed"
   `(("slave-id" . ,slave-id))))

;;;; Auto-Approve Watcher:

(defun emacs-mcp-swarm--check-for-prompts (buffer)
  "Check BUFFER for permission prompts and return position if found."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        ;; Look back at most 500 chars for prompt
        (let ((search-start (max (point-min) (- (point-max) 500))))
          (goto-char search-start)
          (cl-loop for pattern in emacs-mcp-swarm-auto-approve-patterns
                   when (re-search-forward pattern nil t)
                   return (point)))))))

(defun emacs-mcp-swarm--send-approval (buffer term-type)
  "Send 'y' approval to BUFFER using TERM-TYPE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (pcase term-type
        ('vterm
         (vterm-send-string "y")
         (run-at-time 0.05 nil
                      (lambda ()
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (vterm-send-return))))))
        ('eat
         (when (and (boundp 'eat-terminal) eat-terminal)
           (eat-term-send-string eat-terminal "y")
           (run-at-time 0.05 nil
                        (lambda ()
                          (when (and (buffer-live-p buffer)
                                     (boundp 'eat-terminal)
                                     eat-terminal)
                            (eat-term-send-string eat-terminal "\r"))))))))))

(defun emacs-mcp-swarm--auto-approve-tick ()
  "Check all slave buffers for permission prompts.
Action depends on `emacs-mcp-swarm-prompt-mode':
- bypass: do nothing (CLI handles it)
- auto: auto-approve prompts
- human: forward prompts to master for decision"
  (pcase emacs-mcp-swarm-prompt-mode
    ('bypass nil)  ; Nothing to do, CLI bypasses permissions
    ('auto
     ;; Legacy timer-based auto-approve
     (when emacs-mcp-swarm-auto-approve
       (maphash
        (lambda (slave-id slave)
          (let* ((buffer (plist-get slave :buffer))
                 (term-type (or (plist-get slave :terminal) emacs-mcp-swarm-terminal))
                 (last-pos (gethash slave-id emacs-mcp-swarm--last-approve-positions 0)))
            (when (and (buffer-live-p buffer)
                       (eq (plist-get slave :status) 'working))
              (let ((prompt-pos (emacs-mcp-swarm--check-for-prompts buffer)))
                (when (and prompt-pos (> prompt-pos last-pos))
                  (message "[swarm] Auto-approving prompt in %s" slave-id)
                  (puthash slave-id prompt-pos emacs-mcp-swarm--last-approve-positions)
                  (emacs-mcp-swarm--send-approval buffer term-type))))))
        emacs-mcp-swarm--slaves)))
    ('human
     ;; Forward prompts to master for human decision
     (emacs-mcp-swarm--human-prompt-tick))))

(defun emacs-mcp-swarm-start-auto-approve ()
  "Start the auto-approve watcher timer."
  (interactive)
  (emacs-mcp-swarm-stop-auto-approve)
  (setq emacs-mcp-swarm--auto-approve-timer
        (run-with-timer 1 2 #'emacs-mcp-swarm--auto-approve-tick))
  (message "[swarm] Auto-approve watcher started"))

(defun emacs-mcp-swarm-stop-auto-approve ()
  "Stop the auto-approve watcher timer."
  (interactive)
  (when emacs-mcp-swarm--auto-approve-timer
    (cancel-timer emacs-mcp-swarm--auto-approve-timer)
    (setq emacs-mcp-swarm--auto-approve-timer nil)
    (message "[swarm] Auto-approve watcher stopped")))

;;;; Human Mode - Prompt Hooks:

(defun emacs-mcp-swarm--extract-prompt (buffer)
  "Extract prompt text and position from BUFFER.
Returns plist (:text TEXT :pos POS) or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (let ((search-start (max (point-min) (- (point-max) 500))))
          (when (re-search-backward
                 (regexp-opt emacs-mcp-swarm-auto-approve-patterns)
                 search-start t)
            (let ((line-start (line-beginning-position))
                  (line-end (line-end-position)))
              (list :text (buffer-substring-no-properties line-start line-end)
                    :pos (match-beginning 0)))))))))

(defun emacs-mcp-swarm--display-prompt (slave-id prompt-text)
  "Display PROMPT-TEXT from SLAVE-ID in the prompts buffer."
  (let ((buf (get-buffer-create emacs-mcp-swarm-prompts-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n[%s] %s\n  %s\n"
                      (format-time-string "%H:%M:%S")
                      slave-id
                      prompt-text))
      (insert "  → Awaiting response (C-c m s y/n/p or MCP tool)\n"))
    (display-buffer buf '(display-buffer-in-side-window
                          (side . bottom)
                          (slot . 0)
                          (window-height . 8)))))

(defun emacs-mcp-swarm--send-desktop-notification (title body)
  "Send desktop notification with TITLE and BODY via notify-send."
  (when (and emacs-mcp-swarm-desktop-notify
             (executable-find "notify-send"))
    (start-process "swarm-notify" nil "notify-send"
                   "--urgency=critical"
                   "--app-name=Swarm"
                   title body)))

(defun emacs-mcp-swarm--queue-prompt (slave-id prompt-text buffer)
  "Add a prompt to the pending queue and notify user.
Sends both Emacs message and desktop notification for immediate awareness.
Also emits prompt-shown event via channel for push-based updates."
  (push (list :slave-id slave-id
              :prompt prompt-text
              :buffer buffer
              :timestamp (current-time))
        emacs-mcp-swarm--pending-prompts)
  (emacs-mcp-swarm--display-prompt slave-id prompt-text)
  ;; Emacs message
  (when emacs-mcp-swarm-prompt-notify
    (message "[swarm] Prompt from %s: %s"
             slave-id
             (truncate-string-to-width prompt-text 50)))
  ;; Desktop notification (immediate hook - not polling!)
  (emacs-mcp-swarm--send-desktop-notification
   (format "🐝 Swarm Prompt: %s" slave-id)
   (truncate-string-to-width prompt-text 100))
  ;; Push event via channel (sub-100ms notification to master)
  (emacs-mcp-swarm--emit-prompt-shown slave-id prompt-text))

(defun emacs-mcp-swarm--update-prompts-buffer ()
  "Refresh the prompts buffer with current pending prompts."
  (when-let* ((buf (get-buffer emacs-mcp-swarm-prompts-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "=== Pending Swarm Prompts (%d) ===\n"
                      (length emacs-mcp-swarm--pending-prompts)))
      (if emacs-mcp-swarm--pending-prompts
          (dolist (prompt (reverse emacs-mcp-swarm--pending-prompts))
            (insert (format "\n[%s] %s\n  %s\n"
                            (format-time-string "%H:%M:%S"
                                                (plist-get prompt :timestamp))
                            (plist-get prompt :slave-id)
                            (plist-get prompt :prompt))))
        (insert "\nNo pending prompts.\n")))))

(defun emacs-mcp-swarm--human-prompt-tick ()
  "Check all slave buffers for prompts in human mode."
  (maphash
   (lambda (slave-id slave)
     (let* ((buffer (plist-get slave :buffer))
            (last-pos (gethash slave-id emacs-mcp-swarm--last-approve-positions 0)))
       (when (and (buffer-live-p buffer)
                  (eq (plist-get slave :status) 'working))
         (let ((prompt-info (emacs-mcp-swarm--extract-prompt buffer)))
           (when (and prompt-info
                      (> (plist-get prompt-info :pos) last-pos)
                      ;; Not already in queue
                      (not (cl-find slave-id emacs-mcp-swarm--pending-prompts
                                    :key (lambda (p) (plist-get p :slave-id))
                                    :test #'equal)))
             (puthash slave-id (plist-get prompt-info :pos)
                      emacs-mcp-swarm--last-approve-positions)
             (emacs-mcp-swarm--queue-prompt
              slave-id
              (plist-get prompt-info :text)
              buffer))))))
   emacs-mcp-swarm--slaves))

(defun emacs-mcp-swarm--send-response (buffer response)
  "Send RESPONSE to slave BUFFER."
  (when (buffer-live-p buffer)
    (let ((term-type (with-current-buffer buffer
                       (cond ((derived-mode-p 'vterm-mode) 'vterm)
                             ((derived-mode-p 'eat-mode) 'eat)))))
      (pcase term-type
        ('vterm
         (with-current-buffer buffer
           (vterm-send-string response)
           (sit-for 0.05)
           (vterm-send-return)))
        ('eat
         (with-current-buffer buffer
           (when (and (boundp 'eat-terminal) eat-terminal)
             (eat-term-send-string eat-terminal response)
             (eat-term-send-string eat-terminal "\r"))))))))

(defun emacs-mcp-swarm-respond ()
  "Respond to the next pending prompt interactively."
  (interactive)
  (if-let* ((prompt (car emacs-mcp-swarm--pending-prompts)))
      (let* ((slave-id (plist-get prompt :slave-id))
             (text (plist-get prompt :prompt))
             (response (read-string (format "[%s] %s → " slave-id text))))
        (pop emacs-mcp-swarm--pending-prompts)
        (emacs-mcp-swarm--send-response (plist-get prompt :buffer) response)
        (emacs-mcp-swarm--update-prompts-buffer)
        (message "[swarm] Sent response to %s" slave-id))
    (message "[swarm] No pending prompts")))

(defun emacs-mcp-swarm-approve ()
  "Approve (send \\='y\\=') to the next pending prompt."
  (interactive)
  (if-let* ((prompt (pop emacs-mcp-swarm--pending-prompts)))
      (progn
        (emacs-mcp-swarm--send-response (plist-get prompt :buffer) "y")
        (emacs-mcp-swarm--update-prompts-buffer)
        (message "[swarm] Approved: %s" (plist-get prompt :slave-id)))
    (message "[swarm] No pending prompts")))

(defun emacs-mcp-swarm-deny ()
  "Deny (send \\='n\\=') to the next pending prompt."
  (interactive)
  (if-let* ((prompt (pop emacs-mcp-swarm--pending-prompts)))
      (progn
        (emacs-mcp-swarm--send-response (plist-get prompt :buffer) "n")
        (emacs-mcp-swarm--update-prompts-buffer)
        (message "[swarm] Denied: %s" (plist-get prompt :slave-id)))
    (message "[swarm] No pending prompts")))

(defun emacs-mcp-swarm-list-prompts ()
  "List all pending prompts in the prompts buffer."
  (interactive)
  (emacs-mcp-swarm--update-prompts-buffer)
  (display-buffer (get-buffer-create emacs-mcp-swarm-prompts-buffer-name)))

;;;; Preset Management:

(defun emacs-mcp-swarm--scan-presets-dir (dir)
  "Recursively scan DIR for .md files, return alist of (name . path)."
  (when (and dir (file-directory-p dir))
    (let ((files (directory-files-recursively dir "\\.md$" nil)))
      (mapcar (lambda (f)
                (cons (file-name-sans-extension (file-name-nondirectory f)) f))
              files))))

(defun emacs-mcp-swarm--load-all-presets ()
  "Load all presets from built-in and custom directories.
Returns hash-table of name -> content."
  (let ((presets (make-hash-table :test 'equal)))
    ;; Built-in presets
    (dolist (entry (emacs-mcp-swarm--scan-presets-dir emacs-mcp-swarm-presets-dir))
      (puthash (car entry) (cdr entry) presets))
    ;; Custom presets (can override built-in)
    (dolist (dir emacs-mcp-swarm-custom-presets-dirs)
      (dolist (entry (emacs-mcp-swarm--scan-presets-dir dir))
        (puthash (car entry) (cdr entry) presets)))
    presets))

(defun emacs-mcp-swarm-reload-presets ()
  "Reload all presets from disk."
  (interactive)
  (setq emacs-mcp-swarm--presets-cache (emacs-mcp-swarm--load-all-presets))
  (message "Loaded %d presets" (hash-table-count emacs-mcp-swarm--presets-cache)))

(defun emacs-mcp-swarm--list-memory-presets ()
  "List preset names from memory system (conventions tagged swarm-preset)."
  (when (fboundp 'emacs-mcp-memory-query)
    (let ((entries (emacs-mcp-memory-query 'convention
                                            '("swarm-preset")
                                            nil 100 nil nil)))
      (cl-remove-duplicates
       (cl-remove-if-not
        #'identity
        (mapcar (lambda (e)
                  (cl-find-if (lambda (tag)
                                (and (not (string-prefix-p "scope:" tag))
                                     (not (string= tag "swarm-preset"))))
                              (plist-get e :tags)))
                entries))
       :test #'string=))))

(defun emacs-mcp-swarm-list-presets ()
  "List all available presets (file-based + memory-based)."
  (interactive)
  (unless emacs-mcp-swarm--presets-cache
    (emacs-mcp-swarm-reload-presets))
  (let* ((file-presets (hash-table-keys emacs-mcp-swarm--presets-cache))
         (memory-presets (emacs-mcp-swarm--list-memory-presets))
         (all-names (cl-remove-duplicates
                     (append file-presets memory-presets)
                     :test #'string=)))
    (if (called-interactively-p 'any)
        (message "Available presets: %s (file: %d, memory: %d)"
                 (string-join (sort all-names #'string<) ", ")
                 (length file-presets)
                 (length memory-presets))
      all-names)))

(defun emacs-mcp-swarm--get-preset-from-memory (name)
  "Get preset NAME from memory system (conventions tagged swarm-preset).
Memory-based presets allow project-scoped and semantically searchable presets."
  (when (fboundp 'emacs-mcp-memory-query)
    (let ((entries (emacs-mcp-memory-query 'convention
                                            (list "swarm-preset" name)
                                            nil 1 nil nil)))
      (when entries
        (plist-get (car entries) :content)))))

(defun emacs-mcp-swarm--get-preset-content (name)
  "Get content of preset NAME.
Priority: memory-based (project-scoped) → file-based (.md fallback).
This allows project-specific overrides of global file presets."
  ;; Try memory first (supports project scope)
  (or (emacs-mcp-swarm--get-preset-from-memory name)
      ;; Fallback to file-based preset
      (progn
        (unless emacs-mcp-swarm--presets-cache
          (emacs-mcp-swarm-reload-presets))
        (when-let* ((path (gethash name emacs-mcp-swarm--presets-cache)))
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string))))))

(defun emacs-mcp-swarm--build-system-prompt (presets)
  "Build combined system prompt from list of PRESETS."
  (let ((contents '()))
    (dolist (preset presets)
      (when-let* ((content (emacs-mcp-swarm--get-preset-content preset)))
        (push content contents)))
    (if contents
        (mapconcat #'identity (nreverse contents) "\n\n---\n\n")
      nil)))

(defun emacs-mcp-swarm-add-custom-presets-dir (dir)
  "Add DIR to custom preset directories and reload."
  (interactive "DPresets directory: ")
  (add-to-list 'emacs-mcp-swarm-custom-presets-dirs dir)
  (emacs-mcp-swarm-reload-presets))

;;;; Slave Management:

(defun emacs-mcp-swarm--generate-slave-id (name)
  "Generate unique slave ID for NAME."
  (format "swarm-%s-%d" name (floor (float-time))))

(defun emacs-mcp-swarm--generate-task-id (slave-id)
  "Generate unique task ID for SLAVE-ID."
  (cl-incf emacs-mcp-swarm--task-counter)
  (format "task-%s-%03d"
          (replace-regexp-in-string "^swarm-" "" slave-id)
          emacs-mcp-swarm--task-counter))

(defun emacs-mcp-swarm--depth-label (depth)
  "Return human-readable label for DEPTH level."
  (pcase depth
    (0 "master")
    (1 "child")
    (2 "grandchild")
    (3 "great-grandchild")
    (_ (format "depth-%d" depth))))

(defun emacs-mcp-swarm--check-depth ()
  "Check if we can spawn at current depth.
Returns the current depth if allowed, signals error if blocked."
  (let* ((depth (string-to-number (or (getenv "CLAUDE_SWARM_DEPTH") "0")))
         (master-id (getenv "CLAUDE_SWARM_MASTER"))
         (my-id (getenv "CLAUDE_SWARM_SLAVE_ID")))
    (setq emacs-mcp-swarm--current-depth depth)
    ;; Track ancestry for loop detection
    (when (and my-id master-id)
      (push (cons my-id master-id) emacs-mcp-swarm--ancestry))
    (when (>= depth emacs-mcp-swarm-max-depth)
      (error "Recursion limit reached: %s (depth %d) cannot spawn children.
Maximum depth is %d (master → child → grandchild → great-grandchild).
This limit prevents runaway recursive spawning."
             (emacs-mcp-swarm--depth-label depth)
             depth
             emacs-mcp-swarm-max-depth))
    depth))

(defun emacs-mcp-swarm--check-rate-limit ()
  "Check if spawn rate limit allows a new spawn.
Removes old timestamps and checks count within window."
  (let* ((now (float-time))
         (window-start (- now emacs-mcp-swarm-rate-limit-window)))
    ;; Prune old timestamps
    (setq emacs-mcp-swarm--spawn-timestamps
          (cl-remove-if (lambda (ts) (< ts window-start))
                        emacs-mcp-swarm--spawn-timestamps))
    ;; Check limit
    (when (>= (length emacs-mcp-swarm--spawn-timestamps)
              emacs-mcp-swarm-rate-limit-max-spawns)
      (error "Rate limit exceeded: %d spawns in %d seconds.
Wait before spawning more slaves to prevent spawn storms."
             emacs-mcp-swarm-rate-limit-max-spawns
             emacs-mcp-swarm-rate-limit-window))
    ;; Record this spawn attempt
    (push now emacs-mcp-swarm--spawn-timestamps)))

(defun emacs-mcp-swarm--check-slave-limit ()
  "Check if we can spawn more slaves."
  (when (>= (hash-table-count emacs-mcp-swarm--slaves) emacs-mcp-swarm-max-slaves)
    (error "Maximum slave count (%d) reached.
Kill some slaves with `emacs-mcp-swarm-kill' before spawning more."
           emacs-mcp-swarm-max-slaves)))

(cl-defun emacs-mcp-swarm-spawn (name &key presets cwd role terminal)
  "Spawn a new Claude slave with NAME - FULLY ASYNC.

PRESETS is a list of preset names to apply (e.g., '(\"tdd\" \"clarity\")).
CWD is the working directory (defaults to current project root).
ROLE is a predefined role that maps to presets.
TERMINAL overrides `emacs-mcp-swarm-terminal' for this spawn ('vterm or 'eat).

Returns the slave-id IMMEDIATELY.  The actual spawn happens async.
Poll the slave's :status to check progress: spawning -> starting -> idle."
  (interactive
   (list (read-string "Slave name: ")
         :presets (completing-read-multiple
                   "Presets: "
                   (emacs-mcp-swarm-list-presets))))
  ;; Safety checks (order matters: depth → rate → slave count)
  (emacs-mcp-swarm--check-depth)
  (emacs-mcp-swarm--check-rate-limit)
  (emacs-mcp-swarm--check-slave-limit)

  ;; Resolve role to presets if provided
  (when (and role (not presets))
    (setq presets (emacs-mcp-swarm--role-to-presets role)))

  (let* ((slave-id (emacs-mcp-swarm--generate-slave-id name))
         (work-dir (or cwd (emacs-mcp-swarm--project-root) default-directory))
         (term-backend (or terminal emacs-mcp-swarm-terminal))
         (parent-id (or (getenv "CLAUDE_SWARM_SLAVE_ID") "master"))
         (spawn-depth (1+ emacs-mcp-swarm--current-depth)))

    ;; Register slave IMMEDIATELY with "spawning" status - BEFORE any work
    (puthash slave-id
             (list :slave-id slave-id
                   :name name
                   :role role
                   :presets presets
                   :status 'spawning  ; Not starting yet - we return immediately
                   :buffer nil        ; Buffer created async
                   :terminal term-backend
                   :cwd work-dir
                   :depth spawn-depth
                   :parent-id parent-id
                   :current-task nil
                   :task-queue '()
                   :tasks-completed 0
                   :tasks-failed 0
                   :spawned-at (format-time-string "%FT%T%z")
                   :last-activity (format-time-string "%FT%T%z"))
             emacs-mcp-swarm--slaves)

    ;; Log spawn intent
    (message "[swarm] Spawning %s (%s) at depth %d, parent: %s (async)"
             slave-id
             (emacs-mcp-swarm--depth-label spawn-depth)
             spawn-depth
             parent-id)

    ;; Emit slave-spawned event via channel
    (emacs-mcp-swarm--emit-slave-spawned slave-id name presets)

    ;; FULLY ASYNC: Defer ALL work to timer so we return IMMEDIATELY
    (run-with-timer
     0 nil
     (lambda ()
       (condition-case err
           (emacs-mcp-swarm--do-spawn-async slave-id name presets work-dir term-backend)
         (error
          ;; Mark slave as errored
          (when-let* ((slave (gethash slave-id emacs-mcp-swarm--slaves)))
            (plist-put slave :status 'error)
            (plist-put slave :error (error-message-string err)))
          (message "[swarm] Spawn error for %s: %s" slave-id (error-message-string err))))))

    (when (called-interactively-p 'any)
      (message "Spawning slave: %s (async)" slave-id))

    slave-id))

(defun emacs-mcp-swarm--do-spawn-async (slave-id name presets work-dir term-backend)
  "Actually spawn the slave buffer for SLAVE-ID.
Called async from `emacs-mcp-swarm-spawn'.  Updates slave status as work progresses."
  (let* ((slave (gethash slave-id emacs-mcp-swarm--slaves))
         (buffer-name (format "%s%s*" emacs-mcp-swarm-buffer-prefix name))
         (system-prompt (emacs-mcp-swarm--build-system-prompt presets))
         buffer)

    (unless slave
      (error "Slave record not found: %s" slave-id))

    ;; Require terminal emulator (could potentially block on first load)
    (pcase term-backend
      ('vterm (unless (require 'vterm nil t)
                (error "Vterm is required but not available")))
      ('eat (unless (require 'eat nil t)
              (error "Eat is required but not available"))))

    ;; Update status to starting
    (plist-put slave :status 'starting)

    ;; Create terminal buffer
    (setq buffer (generate-new-buffer buffer-name))
    (plist-put slave :buffer buffer)

    (let* ((default-directory work-dir)
           (process-environment
            (append
             (list (format "CLAUDE_SWARM_DEPTH=%d" (plist-get slave :depth))
                   (format "CLAUDE_SWARM_MASTER=%s" (or emacs-mcp-swarm--session-id "direct"))
                   (format "CLAUDE_SWARM_SLAVE_ID=%s" slave-id))
             process-environment))
           (permission-flag (pcase emacs-mcp-swarm-prompt-mode
                              ('bypass "--permission-mode bypassPermissions")
                              (_ "")))
           (claude-cmd (if system-prompt
                          (let ((prompt-file (make-temp-file "swarm-prompt-" nil ".md")))
                            (with-temp-file prompt-file
                              (insert system-prompt))
                            (format "cd %s && %s %s --system-prompt %s"
                                    (shell-quote-argument work-dir)
                                    emacs-mcp-swarm-claude-command
                                    permission-flag
                                    (shell-quote-argument prompt-file)))
                        (format "cd %s && %s %s"
                                (shell-quote-argument work-dir)
                                emacs-mcp-swarm-claude-command
                                permission-flag))))

      (pcase term-backend
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
                              (eat-term-send-string eat-terminal "\r")))))))))

    ;; Log completion
    (message "[swarm] Spawned %s buffer created" slave-id)

    ;; Schedule status transition to idle
    (run-at-time
     3 nil
     (lambda ()
       (when-let* ((s (gethash slave-id emacs-mcp-swarm--slaves)))
         (when (memq (plist-get s :status) '(starting spawning))
           (plist-put s :status 'idle)))))))

(defun emacs-mcp-swarm--role-to-presets (role)
  "Convert ROLE to list of preset names."
  (pcase role
    ("tester" '("tester" "tdd"))
    ("reviewer" '("reviewer" "solid" "clarity"))
    ("documenter" '("documenter"))
    ("refactorer" '("refactorer" "solid" "clarity"))
    ("researcher" '("researcher"))
    ("fixer" '("fixer" "tdd"))
    ("clarity-dev" '("clarity" "solid" "ddd" "tdd"))
    ("coordinator" '("task-coordinator"))
    ("ling" '("ling" "minimal"))
    ("worker" '("ling"))
    (_ (list role))))

(defun emacs-mcp-swarm--project-root ()
  "Get current project root."
  (or (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun emacs-mcp-swarm-kill (slave-id)
  "Kill slave SLAVE-ID without prompts.
Force-kills the buffer to prevent blocking on process/unsaved prompts.
Handles vterm/eat process cleanup to ensure no confirmation dialogs.
Emits slave-killed event via channel for push-based updates."
  (interactive
   (list (completing-read "Kill slave: "
                          (hash-table-keys emacs-mcp-swarm--slaves))))
  (when-let* ((slave (gethash slave-id emacs-mcp-swarm--slaves)))
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
    (emacs-mcp-swarm--emit-slave-killed slave-id)
    (remhash slave-id emacs-mcp-swarm--slaves)
    (remhash slave-id emacs-mcp-swarm--last-approve-positions)
    (message "Killed slave: %s" slave-id)))

(defun emacs-mcp-swarm-kill-all ()
  "Kill all slaves."
  (interactive)
  (let ((count 0))
    (maphash (lambda (id _)
               (emacs-mcp-swarm-kill id)
               (cl-incf count))
             emacs-mcp-swarm--slaves)
    (message "Killed %d slaves" count)))

;;;; Terminal Send Functions:

(defun emacs-mcp-swarm--buffer-contains-p (buffer text &optional start-point)
  "Check if BUFFER contains TEXT after START-POINT.
Uses first 40 chars as signature. Returns position if found, nil otherwise."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start-point (point-min)))
        (let ((sig (substring text 0 (min 40 (length text)))))
          (search-forward sig nil t))))))

(defun emacs-mcp-swarm--claude-responded-p (buffer text &optional start-point)
  "Check if Claude has responded to TEXT in BUFFER after START-POINT.
Looks for the prompt followed by Claude's response marker (●)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start-point (point-min)))
        (let ((sig (substring text 0 (min 40 (length text)))))
          ;; Find our prompt
          (when (search-forward sig nil t)
            ;; Check if there's a response marker after it
            (search-forward "●" nil t)))))))

(defun emacs-mcp-swarm--send-to-terminal (buffer text term-type)
  "Send TEXT to terminal BUFFER using TERM-TYPE backend.
Returns the point-max before sending for verification.
For vterm, adds a small delay between text and return to ensure proper processing."
  (let ((start-point (with-current-buffer buffer (point-max))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (pcase term-type
        ('vterm
         (vterm-send-string text)
         ;; Delay before return to ensure vterm processes the text
         (run-at-time emacs-mcp-swarm-return-delay nil
                      (lambda ()
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (vterm-send-return))))))
        ('eat
         (if (and (boundp 'eat-terminal) eat-terminal)
             (progn
               (eat-term-send-string eat-terminal text)
               ;; Small delay for eat as well
               (run-at-time emacs-mcp-swarm-return-delay nil
                            (lambda ()
                              (when (and (buffer-live-p buffer)
                                         (boundp 'eat-terminal)
                                         eat-terminal)
                                (eat-term-send-string eat-terminal "\r")))))
           (error "Eat-terminal not available in buffer %s" (buffer-name buffer))))))
    start-point))

(defun emacs-mcp-swarm--send-with-retry (buffer text term-type &optional attempt start-point)
  "Send TEXT to BUFFER with retry logic.
TERM-TYPE is 'vterm or 'eat.  ATTEMPT is current attempt number.
START-POINT is the buffer position before first send (for verification).
Returns t on success, signals error on failure after all retries."
  (let* ((attempt (or attempt 1))
         (delay (* emacs-mcp-swarm-send-delay attempt))  ; exponential backoff
         (max-retries emacs-mcp-swarm-send-retries)
         (start-pt (or start-point (with-current-buffer buffer (point-max)))))
    
    (unless (buffer-live-p buffer)
      (error "Buffer is dead, cannot send"))
    
    ;; Only send on first attempt - retries just re-verify
    (when (= attempt 1)
      (emacs-mcp-swarm--send-to-terminal buffer text term-type))
    
    ;; If verification is disabled, assume success
    (unless emacs-mcp-swarm-send-verify
      (cl-return-from emacs-mcp-swarm--send-with-retry t))
    
    ;; Wait and verify the text appeared
    (run-at-time delay nil
                 (lambda ()
                   (cond
                    ;; Claude already responded - success, no retry needed
                    ((emacs-mcp-swarm--claude-responded-p buffer text start-pt)
                     (message "[swarm] Send verified - Claude responded (attempt %d)" attempt))
                    ;; Text is in buffer - success
                    ((emacs-mcp-swarm--buffer-contains-p buffer text start-pt)
                     (message "[swarm] Send verified (attempt %d)" attempt))
                    ;; Text not found, retry verification (not re-send!)
                    ((< attempt max-retries)
                     (message "[swarm] Verifying send (%d/%d)..." attempt max-retries)
                     (emacs-mcp-swarm--send-with-retry buffer text term-type 
                                                       (1+ attempt) start-pt))
                    ;; All retries exhausted
                    (t
                     (message "[swarm] WARNING: Send verification failed after %d attempts" 
                              max-retries)))))
    t))

;;;; Task Dispatch and Collection:

(cl-defun emacs-mcp-swarm-dispatch (slave-id prompt &key timeout priority context)
  "Dispatch PROMPT to SLAVE-ID.

TIMEOUT is milliseconds (default: `emacs-mcp-swarm-default-timeout').
PRIORITY is one of: critical, high, normal, low.
CONTEXT is additional context plist.

Returns task-id."
  (interactive
   (list (completing-read "Slave: " (hash-table-keys emacs-mcp-swarm--slaves))
         (read-string "Prompt: ")))

  (let* ((slave (gethash slave-id emacs-mcp-swarm--slaves))
         (task-id (emacs-mcp-swarm--generate-task-id slave-id))
         (buffer (plist-get slave :buffer)))

    (unless slave
      (error "Slave not found: %s" slave-id))

    (unless (buffer-live-p buffer)
      (error "Slave buffer is dead: %s" slave-id))

    ;; Create task record
    (puthash task-id
             (list :task-id task-id
                   :slave-id slave-id
                   :prompt prompt
                   :status 'dispatched
                   :priority (or priority 'normal)
                   :timeout (or timeout emacs-mcp-swarm-default-timeout)
                   :context context
                   :dispatched-at (format-time-string "%FT%T%z")
                   :completed-at nil
                   :result nil
                   :error nil)
             emacs-mcp-swarm--tasks)

    ;; Update slave state
    (plist-put slave :status 'working)
    (plist-put slave :current-task task-id)
    (plist-put slave :last-activity (format-time-string "%FT%T%z"))
    (plist-put slave :task-start-point
               (with-current-buffer buffer (point-max)))

    ;; Send prompt to slave with verification and retry
    (let ((target-buffer buffer)
          (term-type (or (plist-get slave :terminal) emacs-mcp-swarm-terminal))
          (prompt-text prompt))
      ;; Small initial delay to ensure terminal is ready
      (run-at-time 0.1 nil
                   (lambda ()
                     (when (buffer-live-p target-buffer)
                       (condition-case err
                           (emacs-mcp-swarm--send-with-retry
                            target-buffer prompt-text term-type)
                         (error
                          (message "[swarm] Dispatch error for %s: %s"
                                   task-id (error-message-string err))))))))

    (when (called-interactively-p 'any)
      (message "Dispatched task %s to %s" task-id slave-id))

    task-id))

(defun emacs-mcp-swarm-collect (task-id &optional timeout-ms)
  "Collect response for TASK-ID.

TIMEOUT-MS is how long to wait (default: 5000ms).
Returns the task plist with :result populated."
  (interactive
   (list (completing-read "Task: " (hash-table-keys emacs-mcp-swarm--tasks))))

  (let* ((task (gethash task-id emacs-mcp-swarm--tasks))
         (slave-id (plist-get task :slave-id))
         (slave (gethash slave-id emacs-mcp-swarm--slaves))
         (buffer (plist-get slave :buffer))
         (prompt (plist-get task :prompt))
         (timeout (/ (or timeout-ms 5000) 1000.0))
         (start-time (float-time))
         result)

    (unless task
      (error "Task not found: %s" task-id))

    ;; Wait for completion by searching for prompt text + response marker
    ;; vterm buffers have complex structure, so we search for our prompt
    ;; then look for Claude's response marker (●) after it
    (while (and (< (- (float-time) start-time) timeout)
                (not result))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            ;; Find our prompt in the buffer (use first 50 chars as search key)
            (let ((search-key (substring prompt 0 (min 50 (length prompt)))))
              (when (search-forward search-key nil t)
                ;; Found our prompt, now look for response marker
                (when (search-forward "●" nil t)
                  (let ((response-start (point)))
                    ;; Find end: next prompt "> " or horizontal line or double newline
                    (cond
                     ((search-forward "\n> " nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 3)))))
                     ((search-forward "\n────" nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 5)))))
                     ((search-forward "\n\n\n" nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 3)))))))))))))
      (unless result
        (sleep-for 0.5)))

    ;; Update task
    (if result
        (progn
          (plist-put task :status 'completed)
          (plist-put task :result result)
          (plist-put task :completed-at (format-time-string "%FT%T%z"))
          ;; Update slave stats
          (plist-put slave :status 'idle)
          (plist-put slave :current-task nil)
          (plist-put slave :tasks-completed (1+ (plist-get slave :tasks-completed))))
      (plist-put task :status 'timeout)
      (plist-put task :error "Collection timed out"))

    (when (called-interactively-p 'any)
      (if result
          (message "Collected result (%d chars)" (length result))
        (message "Collection timed out")))

    task))

(defun emacs-mcp-swarm-broadcast (prompt &optional slave-filter)
  "Send PROMPT to all slaves (or those matching SLAVE-FILTER).
SLAVE-FILTER is a plist like (:role \"tester\").
Returns list of task-ids."
  (let ((task-ids '()))
    (maphash
     (lambda (slave-id slave)
       (when (or (not slave-filter)
                 (emacs-mcp-swarm--slave-matches-filter slave slave-filter))
         (push (emacs-mcp-swarm-dispatch slave-id prompt) task-ids)))
     emacs-mcp-swarm--slaves)
    (nreverse task-ids)))

(defun emacs-mcp-swarm--slave-matches-filter (slave filter)
  "Check if SLAVE matches FILTER criteria."
  (let ((matches t))
    (when-let* ((role (plist-get filter :role)))
      (unless (equal (plist-get slave :role) role)
        (setq matches nil)))
    (when-let* ((status (plist-get filter :status)))
      (unless (eq (plist-get slave :status) status)
        (setq matches nil)))
    matches))

;;;; Status and Monitoring:

(defun emacs-mcp-swarm-status (&optional slave-id)
  "Get swarm status.
If SLAVE-ID is provided, get that slave's status.
Otherwise return aggregate status."
  (interactive)
  (if slave-id
      (gethash slave-id emacs-mcp-swarm--slaves)
    (let ((total 0) (idle 0) (working 0) (error-count 0)
          (slaves-detail '()))
      (maphash
       (lambda (id slave)
         (cl-incf total)
         (pcase (plist-get slave :status)
           ('idle (cl-incf idle))
           ('working (cl-incf working))
           ('error (cl-incf error-count)))
         (push (list :slave-id id
                     :name (plist-get slave :name)
                     :status (plist-get slave :status)
                     :depth (plist-get slave :depth)
                     :parent-id (plist-get slave :parent-id)
                     :current-task (plist-get slave :current-task)
                     :tasks-completed (plist-get slave :tasks-completed))
               slaves-detail))
       emacs-mcp-swarm--slaves)

      (let ((status `(:session-id ,emacs-mcp-swarm--session-id
                      :status ,(if (> total 0) "active" "inactive")
                      :current-depth ,emacs-mcp-swarm--current-depth
                      :safeguards (:max-depth ,emacs-mcp-swarm-max-depth
                                   :max-slaves ,emacs-mcp-swarm-max-slaves
                                   :rate-limit (:window-seconds ,emacs-mcp-swarm-rate-limit-window
                                                :max-spawns ,emacs-mcp-swarm-rate-limit-max-spawns
                                                :recent-spawns ,(length emacs-mcp-swarm--spawn-timestamps)))
                      :slaves (:total ,total :idle ,idle :working ,working :error ,error-count)
                      :tasks (:total ,(hash-table-count emacs-mcp-swarm--tasks))
                      :slaves-detail ,(nreverse slaves-detail))))
        (when (called-interactively-p 'any)
          (message "Swarm: %d slaves (%d idle, %d working), %d tasks"
                   total idle working (hash-table-count emacs-mcp-swarm--tasks)))
        status))))

(defun emacs-mcp-swarm-show-slave (slave-id)
  "Switch to buffer for SLAVE-ID."
  (interactive
   (list (completing-read "Show slave: "
                          (hash-table-keys emacs-mcp-swarm--slaves))))
  (when-let* ((slave (gethash slave-id emacs-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer)
      (message "Slave buffer is dead: %s" slave-id))))

;;;; API for MCP Tools:

(defun emacs-mcp-swarm-api-spawn (name presets &optional cwd terminal)
  "API: Spawn slave NAME with PRESETS in CWD using TERMINAL backend.
Returns slave-id on success, or error plist on failure."
  (emacs-mcp-with-fallback
      (emacs-mcp-swarm-spawn name :presets presets :cwd cwd
                             :terminal (when terminal (intern terminal)))
    `(:error "spawn-failed" :name ,name :reason "unknown")))

(defun emacs-mcp-swarm-api-dispatch (slave-id prompt &optional timeout-ms)
  "API: Dispatch PROMPT to SLAVE-ID with TIMEOUT-MS.
Returns task-id on success, or error plist on failure."
  (emacs-mcp-with-fallback
      (emacs-mcp-swarm-dispatch slave-id prompt :timeout timeout-ms)
    `(:error "dispatch-failed" :slave-id ,slave-id :reason "unknown")))

(defun emacs-mcp-swarm-api-status ()
  "API: Get swarm status as JSON-serializable plist."
  (emacs-mcp-swarm-status))

(defun emacs-mcp-swarm--check-task-completion (task-id)
  "Check if TASK-ID has completed without blocking.
Returns result if complete, nil if still running."
  (when-let* ((task (gethash task-id emacs-mcp-swarm--tasks))
              (slave-id (plist-get task :slave-id))
              (slave (gethash slave-id emacs-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer))
              (prompt (plist-get task :prompt)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (let ((search-key (substring prompt 0 (min 50 (length prompt)))))
            (when (search-forward search-key nil t)
              (when (search-forward "●" nil t)
                (let ((response-start (point)))
                  (cond
                   ((search-forward "\n> " nil t)
                    (string-trim
                     (buffer-substring-no-properties
                      response-start (- (point) 3))))
                   ((search-forward "\n────" nil t)
                    (string-trim
                     (buffer-substring-no-properties
                      response-start (- (point) 5))))
                   ((search-forward "\n\n\n" nil t)
                    (string-trim
                     (buffer-substring-no-properties
                      response-start (- (point) 3))))))))))))))

(defun emacs-mcp-swarm-api-collect (task-id &optional timeout-ms)
  "API: Collect result for TASK-ID - NON-BLOCKING with graceful fallback.
Returns immediately with current status:
- :status \"completed\" with :result if done
- :status \"polling\" if still running (client should poll again)
- :status \"timeout\" if task timed out
- :status \"error\" if task failed

TIMEOUT-MS is used to check if task has exceeded its timeout,
but this function never blocks. Wraps implementation in graceful
fallback to ensure MCP clients never receive errors."
  (emacs-mcp-with-fallback
      (let* ((task (gethash task-id emacs-mcp-swarm--tasks))
             (slave-id (and task (plist-get task :slave-id)))
             (slave (and slave-id (gethash slave-id emacs-mcp-swarm--slaves)))
             (dispatched-at (and task (plist-get task :dispatched-at)))
             (task-timeout (or timeout-ms
                               (plist-get task :timeout)
                               emacs-mcp-swarm-default-timeout))
             (elapsed-ms (when dispatched-at
                           (* 1000 (- (float-time)
                                      (float-time (date-to-time dispatched-at)))))))
        (cond
         ;; Task not found
         ((not task)
          `(:task-id ,task-id :status "error" :error "Task not found"))

         ;; Already completed
         ((eq (plist-get task :status) 'completed)
          `(:task-id ,task-id
            :status "completed"
            :result ,(plist-get task :result)))

         ;; Already timed out
         ((eq (plist-get task :status) 'timeout)
          `(:task-id ,task-id
            :status "timeout"
            :error ,(plist-get task :error)))

         ;; Check if timed out now
         ((and elapsed-ms (> elapsed-ms task-timeout))
          (plist-put task :status 'timeout)
          (plist-put task :error (format "Timed out after %dms" elapsed-ms))
          (when slave
            (plist-put slave :status 'idle)
            (plist-put slave :current-task nil)
            (plist-put slave :tasks-failed (1+ (or (plist-get slave :tasks-failed) 0))))
          ;; Emit task-failed event via channel
          (emacs-mcp-swarm--emit-task-failed
           task-id slave-id (format "Timed out after %dms" elapsed-ms))
          `(:task-id ,task-id
            :status "timeout"
            :error ,(format "Timed out after %dms" elapsed-ms)
            :elapsed-ms ,elapsed-ms))

         ;; Try to get result (non-blocking check)
         (t
          (if-let* ((result (emacs-mcp-swarm--check-task-completion task-id)))
              (progn
                ;; Update task
                (plist-put task :status 'completed)
                (plist-put task :result result)
                (plist-put task :completed-at (format-time-string "%FT%T%z"))
                ;; Update slave
                (when slave
                  (plist-put slave :status 'idle)
                  (plist-put slave :current-task nil)
                  (plist-put slave :tasks-completed
                             (1+ (or (plist-get slave :tasks-completed) 0))))
                ;; Emit task-completed event via channel (sub-100ms push!)
                (emacs-mcp-swarm--emit-task-completed task-id slave-id result)
                `(:task-id ,task-id
                  :status "completed"
                  :result ,result
                  :elapsed-ms ,elapsed-ms))
            ;; Still running - return polling status
            `(:task-id ,task-id
              :status "polling"
              :elapsed-ms ,elapsed-ms
              :timeout-ms ,task-timeout
              :message "Task still running, poll again")))))
    ;; Graceful fallback on any error
    `(:task-id ,task-id :status "error" :result nil
      :error "collection-failed")))

(defun emacs-mcp-swarm-api-list-presets ()
  "API: List available presets."
  (emacs-mcp-swarm-list-presets))

(defun emacs-mcp-swarm-api-kill (slave-id)
  "API: Kill SLAVE-ID.
Returns result plist. Never fails."
  (emacs-mcp-with-fallback
      (progn
        (emacs-mcp-swarm-kill slave-id)
        `(:killed ,slave-id))
    `(:error "kill-failed" :slave-id ,slave-id)))

(defun emacs-mcp-swarm-api-kill-all ()
  "API: Kill all slaves.
Returns result plist. Never fails."
  (emacs-mcp-with-fallback
      (let ((count (hash-table-count emacs-mcp-swarm--slaves)))
        (emacs-mcp-swarm-kill-all)
        `(:killed-count ,count))
    `(:error "kill-all-failed" :killed-count 0)))

(defun emacs-mcp-swarm-api-pending-prompts ()
  "API: Get list of pending prompts awaiting human decision.
Returns list of prompts with slave-id, prompt text, and timestamp."
  (emacs-mcp-with-fallback
      (let ((prompts
             (mapcar (lambda (p)
                       `(:slave-id ,(plist-get p :slave-id)
                         :prompt ,(plist-get p :prompt)
                         :timestamp ,(format-time-string
                                      "%Y-%m-%dT%H:%M:%S"
                                      (plist-get p :timestamp))))
                     emacs-mcp-swarm--pending-prompts)))
        `(:count ,(length prompts)
          :prompts ,prompts
          :mode ,emacs-mcp-swarm-prompt-mode))
    `(:error "pending-prompts-failed" :count 0 :prompts nil)))

(defun emacs-mcp-swarm-api-respond-prompt (slave-id response)
  "API: Send RESPONSE to the pending prompt from SLAVE-ID.
Returns result plist indicating success or failure."
  (emacs-mcp-with-fallback
      (if-let* ((prompt (cl-find slave-id emacs-mcp-swarm--pending-prompts
                                 :key (lambda (p) (plist-get p :slave-id))
                                 :test #'equal)))
          (let ((buffer (plist-get prompt :buffer)))
            (setq emacs-mcp-swarm--pending-prompts
                  (cl-remove slave-id emacs-mcp-swarm--pending-prompts
                             :key (lambda (p) (plist-get p :slave-id))
                             :test #'equal))
            (emacs-mcp-swarm--send-response buffer response)
            (emacs-mcp-swarm--update-prompts-buffer)
            `(:success t :slave-id ,slave-id :response ,response))
        `(:success nil :error "no-pending-prompt" :slave-id ,slave-id))
    `(:error "respond-prompt-failed" :slave-id ,slave-id)))

;;;; Transient Menu:

(require 'transient nil t)

(when (featurep 'transient)
  (transient-define-prefix emacs-mcp-swarm-transient ()
    "Swarm orchestration menu."
    ["emacs-mcp Swarm"
     ["Slaves"
      ("s" "Spawn slave" emacs-mcp-swarm-spawn)
      ("k" "Kill slave" emacs-mcp-swarm-kill)
      ("K" "Kill all" emacs-mcp-swarm-kill-all)
      ("v" "View slave" emacs-mcp-swarm-show-slave)]
     ["Tasks"
      ("d" "Dispatch" emacs-mcp-swarm-dispatch)
      ("c" "Collect" emacs-mcp-swarm-collect)
      ("b" "Broadcast" emacs-mcp-swarm-broadcast)]
     ["Prompts (human mode)"
      ("y" "Approve next" emacs-mcp-swarm-approve)
      ("n" "Deny next" emacs-mcp-swarm-deny)
      ("p" "Respond custom" emacs-mcp-swarm-respond)
      ("l" "List pending" emacs-mcp-swarm-list-prompts)]
     ["Info"
      ("?" "Status" emacs-mcp-swarm-status)
      ("P" "List presets" emacs-mcp-swarm-list-presets)
      ("r" "Reload presets" emacs-mcp-swarm-reload-presets)
      ("a" "Add presets dir" emacs-mcp-swarm-add-custom-presets-dir)]]))

;;;; Minor Mode:

(defvar emacs-mcp-swarm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Use C-c M-s (Meta modifier) - allowed for minor modes per Emacs conventions
    (define-key map (kbd "C-c M-s") #'emacs-mcp-swarm-transient)
    map)
  "Keymap for `emacs-mcp-swarm-mode'.")

;;;###autoload
(define-minor-mode emacs-mcp-swarm-mode
  "Minor mode for Claude swarm orchestration.

\\{emacs-mcp-swarm-mode-map}"
  :init-value nil
  :lighter " Swarm"
  :keymap emacs-mcp-swarm-mode-map
  :global t
  :group 'emacs-mcp-swarm
  (if emacs-mcp-swarm-mode
      (progn
        (setq emacs-mcp-swarm--session-id
              (format "session-%s-%04x"
                      (format-time-string "%Y%m%d")
                      (random 65535)))
        (emacs-mcp-swarm-reload-presets)
        (when emacs-mcp-swarm-auto-approve
          (emacs-mcp-swarm-start-auto-approve))
        (message "Emacs-mcp-swarm enabled (session: %s, %d presets, auto-approve: %s)"
                 emacs-mcp-swarm--session-id
                 (hash-table-count emacs-mcp-swarm--presets-cache)
                 (if emacs-mcp-swarm-auto-approve "on" "off")))
    (emacs-mcp-swarm-stop-auto-approve)
    (emacs-mcp-swarm-kill-all)
    (message "Emacs-mcp-swarm disabled")))

;;;; Addon Lifecycle:

(defun emacs-mcp-swarm--addon-init ()
  "Initialize swarm addon."
  (emacs-mcp-swarm-reload-presets)
  (setq emacs-mcp-swarm--session-id
        (format "session-%s-%04x"
                (format-time-string "%Y%m%d")
                (random 65535)))
  (when emacs-mcp-swarm-auto-approve
    (emacs-mcp-swarm-start-auto-approve)))

(defun emacs-mcp-swarm--addon-shutdown ()
  "Shutdown swarm addon - kill all slaves."
  (emacs-mcp-swarm-stop-auto-approve)
  (emacs-mcp-swarm-kill-all)
  (clrhash emacs-mcp-swarm--last-approve-positions)
  (setq emacs-mcp-swarm--presets-cache nil)
  (setq emacs-mcp-swarm--session-id nil))

;; Register with addon system
(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'swarm
   :version "0.1.0"
   :description "Claude swarm orchestration for parallel task execution"
   :requires '()  ; vterm or eat checked at spawn time
   :provides '(emacs-mcp-swarm-mode emacs-mcp-swarm-transient)
   :init #'emacs-mcp-swarm--addon-init
   :shutdown #'emacs-mcp-swarm--addon-shutdown))

(provide 'emacs-mcp-swarm)
;;; emacs-mcp-swarm.el ends here
