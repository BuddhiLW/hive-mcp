;;; hive-mcp-swarm.el --- Claude swarm orchestration for parallel task execution -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Optional-Requires: claude-code-ide (recommended), vterm, or eat
;; Keywords: tools, ai, claude, orchestration, parallel
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Swarm orchestration for Claude Code instances.
;;
;; Enables a Master Claude to spawn and control multiple Slave Claude
;; instances for parallel task execution.  Each slave runs in a terminal
;; buffer and can be configured with presets (system prompts loaded from
;; markdown files).
;;
;; Terminal backend choice:
;;   - claude-code-ide (recommended): Uses claude-code-ide.el's terminal
;;     abstraction with optimized timing and anti-flicker support.  Most
;;     reliable for command submission.
;;   - vterm: Native terminal, reliable input handling.
;;   - eat (experimental): Pure Emacs Lisp, may have input submission issues.
;;
;; Set via: (setq hive-mcp-swarm-terminal 'claude-code-ide)  ; default
;;
;; Architecture:
;;
;;   Master Claude (you)
;;         │ MCP tools
;;         v
;;   hive-mcp-swarm.el (this file)
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
;;   (require 'hive-mcp-swarm)
;;   (hive-mcp-swarm-mode 1)
;;
;;   ;; Spawn a slave with preset
;;   (hive-mcp-swarm-spawn "tester" :presets '("tdd" "clarity"))
;;
;;   ;; Dispatch a task
;;   (hive-mcp-swarm-dispatch "swarm-tester-xxx" "Run all tests")
;;
;;   ;; Check status
;;   (hive-mcp-swarm-status)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'hive-mcp-graceful)

;; Add swarm/ subdirectory to load-path for modular components
(let ((swarm-dir (expand-file-name "swarm" (file-name-directory
                                            (or load-file-name buffer-file-name)))))
  (when (file-directory-p swarm-dir)
    (add-to-list 'load-path swarm-dir)))

;; Load modular components from swarm/ directory
(require 'hive-mcp-swarm-events)
(require 'hive-mcp-swarm-prompts)
(require 'hive-mcp-swarm-presets)
(require 'hive-mcp-swarm-terminal)
(require 'hive-mcp-swarm-slaves)
(require 'hive-mcp-swarm-tasks)
(require 'hive-mcp-swarm-hooks)

;; Forward declarations for terminal completion watcher
(declare-function hive-mcp-swarm-terminal-start-completion-watcher "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-terminal-stop-completion-watcher "hive-mcp-swarm-terminal")

;; Forward declarations for Layer 2: Idle detection
(declare-function hive-mcp-swarm-terminal-start-idle-watcher "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-terminal-stop-idle-watcher "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-terminal-reset-idle-state "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-terminal--record-activity "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-terminal--record-shout "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-terminal--clear-slave-timestamps "hive-mcp-swarm-terminal")

;; Soft dependency on channel for push events
(declare-function hive-mcp-channel-connected-p "hive-mcp-channel")
(declare-function hive-mcp-channel-send "hive-mcp-channel")

;; Soft dependency on vterm
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

;; Soft dependency on eat
(declare-function eat "eat")
(declare-function eat-mode "eat")
(declare-function eat-exec "eat")
(declare-function eat-term-send-string "eat")

;; Soft dependency on claude-code-ide
(declare-function claude-code-ide "claude-code-ide")
(declare-function claude-code-ide--configure-vterm-buffer "claude-code-ide")
(declare-function claude-code-ide--terminal-send-string "claude-code-ide")
(declare-function claude-code-ide--terminal-send-return "claude-code-ide")
(declare-function claude-code-ide-mcp-server-get-session-context "claude-code-ide-mcp-server")
(defvar claude-code-ide-terminal-backend)

;;;; Customization:

(defgroup hive-mcp-swarm nil
  "Claude swarm orchestration."
  :group 'hive-mcp
  :prefix "hive-mcp-swarm-")

(defcustom hive-mcp-swarm-presets-dir
  (expand-file-name "presets" (file-name-directory
                               (or load-file-name buffer-file-name
                                   default-directory)))
  "Directory containing built-in preset markdown files."
  :type 'directory
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-custom-presets-dirs nil
  "List of custom directories to scan for preset .md files.
Directories are scanned recursively for .md files only."
  :type '(repeat directory)
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-claude-command "claude"
  "Command to invoke Claude Code CLI."
  :type 'string
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-terminal 'claude-code-ide
  "Terminal emulator to use for slave sessions.
- `claude-code-ide': Use claude-code-ide.el WebSocket integration - RECOMMENDED
  Provides MCP integration, robust session management, no terminal quirks.
- `vterm': Use vterm (requires native compilation) - ALTERNATIVE
- `eat': Use eat (pure Emacs Lisp) - EXPERIMENTAL, may have input issues"
  :type '(choice (const :tag "claude-code-ide (recommended)" claude-code-ide)
          (const :tag "vterm (native terminal)" vterm)
          (const :tag "eat (experimental)" eat))
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-max-slaves 30
  "Maximum number of concurrent slave instances."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-max-depth 3
  "Maximum recursion depth (slaves spawning slaves).
Depth 0 = master, 1 = child, 2 = grandchild, 3 = great-grandchild.
Beyond this depth, spawn attempts are blocked to prevent runaway recursion."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-rate-limit-window 60
  "Time window in seconds for rate limiting spawn attempts."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-rate-limit-max-spawns 10
  "Maximum number of spawns allowed within `hive-mcp-swarm-rate-limit-window'."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-default-timeout 300000
  "Default task timeout in milliseconds (5 minutes)."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-send-verify t
  "If non-nil, verify that prompts were sent to the terminal.
When enabled, dispatch will check the buffer for the prompt text
and retry if not found."
  :type 'boolean
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-send-retries 3
  "Number of retry attempts for sending prompts."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-send-delay 0.2
  "Base delay in seconds between send and return.
This delay increases with each retry (exponential backoff)."
  :type 'float
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-return-delay 0.05
  "Delay in seconds between sending text and sending return.
Helps ensure vterm processes the text before return is sent."
  :type 'float
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-auto-approve t
  "If non-nil, automatically approve tool permission prompts.
Claude Code asks for permission before running tools. This setting
auto-sends 'y' when permission prompts are detected."
  :type 'boolean
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-auto-approve-patterns
  '("Allow\\|Deny" "Yes\\|No" "(y/n)" "[Y/n]" "[y/N]"
    "Do you want to" "Would you like to" "Proceed\\?")
  "Patterns that indicate a permission/confirmation prompt.
When any of these are found in the buffer, and `hive-mcp-swarm-auto-approve'
is non-nil, automatically send 'y' to approve."
  :type '(repeat string)
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-prompt-mode 'human
  "How to handle permission prompts in slaves.
- `human': Forward prompts to master for human decision (RECOMMENDED)
- `auto': Timer-based auto-approve (use with caution)
- `bypass': Use --permission-mode bypassPermissions (dangerous, sandboxed only)"
  :type '(choice (const :tag "Bypass permissions (CLI flag)" bypass)
          (const :tag "Auto-approve (timer)" auto)
          (const :tag "Human decision (hooks)" human))
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-prompt-notify t
  "If non-nil, show notification when prompts are pending in human mode."
  :type 'boolean
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-desktop-notify t
  "If non-nil, send desktop notification via notify-send for prompts.
This alerts the human even when running master Claude in a terminal."
  :type 'boolean
  :group 'hive-mcp-swarm)

(defvar hive-mcp-swarm-prompts-buffer-name "*Swarm Prompts*"
  "Buffer name for displaying pending prompts.")

(defcustom hive-mcp-swarm-prompt-marker "❯"
  "Marker indicating Claude is ready for input."
  :type 'string
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-buffer-prefix "*swarm-"
  "Prefix for swarm buffer names."
  :type 'string
  :group 'hive-mcp-swarm)

;;;; Internal State:

(defvar hive-mcp-swarm--slaves (make-hash-table :test 'equal)
  "Hash table of slave-id -> slave plist.")

(defvar hive-mcp-swarm--tasks (make-hash-table :test 'equal)
  "Hash table of task-id -> task plist.")

(defvar hive-mcp-swarm--task-counter 0
  "Counter for generating unique task IDs.")

(defvar hive-mcp-swarm--session-id nil
  "Current swarm session ID.")

(defvar hive-mcp-swarm--current-depth 0
  "Current recursion depth (set via environment).")

(defvar hive-mcp-swarm--spawn-timestamps nil
  "List of recent spawn timestamps for rate limiting.")

(defvar hive-mcp-swarm--ancestry nil
  "Ancestry chain: list of (slave-id . master-id) for loop detection.")

;; NOTE: The following state is now managed by submodules:
;; - Presets cache: hive-mcp-swarm-presets module
;; - Auto-approve timer: hive-mcp-swarm-prompts module
;; - Last approve positions: hive-mcp-swarm-prompts module
;; - Pending prompts: hive-mcp-swarm-prompts module

;;;; Channel Event Emission (Push-based updates):
;; Delegated to hive-mcp-swarm-events module.
;; Aliases for backward compatibility with internal callers:

(defalias 'hive-mcp-swarm--channel-available-p 'hive-mcp-swarm-events-channel-available-p)
(defalias 'hive-mcp-swarm--emit-event 'hive-mcp-swarm-events-emit)
(defalias 'hive-mcp-swarm--emit-task-completed 'hive-mcp-swarm-events-emit-task-completed)
(defalias 'hive-mcp-swarm--emit-task-failed 'hive-mcp-swarm-events-emit-task-failed)
(defalias 'hive-mcp-swarm--emit-prompt-shown 'hive-mcp-swarm-events-emit-prompt-shown)
(defalias 'hive-mcp-swarm--emit-state-changed 'hive-mcp-swarm-events-emit-state-changed)
(defalias 'hive-mcp-swarm--emit-slave-spawned 'hive-mcp-swarm-events-emit-slave-spawned)
(defalias 'hive-mcp-swarm--emit-slave-killed 'hive-mcp-swarm-events-emit-slave-killed)
(defalias 'hive-mcp-swarm--emit-auto-completed 'hive-mcp-swarm-events-emit-auto-completed)
(defalias 'hive-mcp-swarm--emit-auto-started 'hive-mcp-swarm-events-emit-auto-started)
(defalias 'hive-mcp-swarm--emit-auto-error 'hive-mcp-swarm-events-emit-auto-error)

;;;; Hivemind State Sync Handler:
;; Updates swarm slave state based on hivemind shout events.
;; Event-to-state mapping:
;; - hivemind-started → slave_status: working
;; - hivemind-progress → no state change (maintains working)
;; - hivemind-completed → slave_status: idle, increment tasks-completed
;; - hivemind-error → slave_status: idle, increment tasks-failed
;; - hivemind-blocked → slave_status: blocked

(defun hive-mcp-swarm--sync-state-from-hivemind (event-data)
  "Sync swarm slave state from hivemind EVENT-DATA.
Called via hooks when hivemind receives shout events.
Maps agent-id to slave-id and updates status accordingly.

Also records shout timestamp for Layer 2 idle detection."
  (let* ((agent-id (plist-get event-data :agent-id))
         (event-type (plist-get event-data :event-type))
         (slave (gethash agent-id hive-mcp-swarm--slaves)))
    (when slave
      ;; Layer 2: Record shout for idle detection
      ;; This proves the ling is communicating (not dead/hung)
      (when (fboundp 'hive-mcp-swarm-terminal--record-shout)
        (hive-mcp-swarm-terminal--record-shout agent-id))
      (let ((old-status (plist-get slave :status)))
        (pcase event-type
          ;; Started -> working
          ((or "hivemind-started" ":hivemind-started")
           (plist-put slave :status 'working)
           (plist-put slave :current-task (plist-get event-data :task)))

          ;; Progress -> maintain working status
          ((or "hivemind-progress" ":hivemind-progress")
           (plist-put slave :status 'working))

          ;; Completed -> idle, increment completed count
          ((or "hivemind-completed" ":hivemind-completed")
           (plist-put slave :status 'idle)
           (plist-put slave :current-task nil)
           (plist-put slave :tasks-completed
                      (1+ (or (plist-get slave :tasks-completed) 0))))

          ;; Error -> idle, increment failed count
          ((or "hivemind-error" ":hivemind-error")
           (plist-put slave :status 'idle)
           (plist-put slave :current-task nil)
           (plist-put slave :tasks-failed
                      (1+ (or (plist-get slave :tasks-failed) 0))))

          ;; Blocked -> blocked status
          ((or "hivemind-blocked" ":hivemind-blocked")
           (plist-put slave :status 'blocked)))

        ;; Emit state change event if status changed
        (let ((new-status (plist-get slave :status)))
          (unless (eq old-status new-status)
            (hive-mcp-swarm--emit-state-changed agent-id old-status new-status)))))))

;;;; Auto-Approve Watcher:
;; Delegated to hive-mcp-swarm-prompts module for centralized prompt handling.

(defun hive-mcp-swarm--get-terminal-type (slave)
  "Get terminal type for SLAVE plist."
  (or (plist-get slave :terminal) hive-mcp-swarm-terminal))

(defun hive-mcp-swarm-start-auto-approve ()
  "Start the auto-approve watcher timer."
  (interactive)
  (hive-mcp-swarm-prompts-start-watcher
   hive-mcp-swarm--slaves
   #'hive-mcp-swarm--get-terminal-type)
  (message "[swarm] Auto-approve watcher started"))

(defun hive-mcp-swarm-stop-auto-approve ()
  "Stop the auto-approve watcher timer."
  (interactive)
  (hive-mcp-swarm-prompts-stop-watcher)
  (message "[swarm] Auto-approve watcher stopped"))

;;;; Auto-Shout Completion Watcher:
;; Automatically detects when lings finish tasks and emits hivemind shout.
;; Uses hive-mcp-swarm-terminal completion detection.

(defun hive-mcp-swarm--on-task-completion (_buffer slave-id duration-secs
                                                  &optional status error-type error-preview)
  "Callback when task completion is auto-detected.
BUFFER is the terminal buffer, SLAVE-ID identifies the ling,
DURATION-SECS is how long the task took.
STATUS is \"completed\" or \"error\" (default: \"completed\").
ERROR-TYPE and ERROR-PREVIEW are set when STATUS is \"error\".

This function:
1. Updates slave status based on completion type
2. Increments appropriate counter (tasks-completed or tasks-failed)
3. Emits state-changed event if status changed

Note: Event emission (auto-completed/auto-error) is now handled by
the completion watcher tick function directly."
  (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves)))
    (let ((old-status (plist-get slave :status))
          (is-error (string= status "error")))
      ;; Update slave state
      (plist-put slave :status 'idle)
      (plist-put slave :current-task nil)
      (plist-put slave :last-activity (format-time-string "%FT%T%z"))
      ;; Update appropriate counter
      (if is-error
          (progn
            (plist-put slave :tasks-failed
                       (1+ (or (plist-get slave :tasks-failed) 0)))
            (plist-put slave :last-error
                       (list :type error-type
                             :preview error-preview
                             :timestamp (format-time-string "%FT%T%z"))))
        (plist-put slave :tasks-completed
                   (1+ (or (plist-get slave :tasks-completed) 0))))
      ;; Emit state-changed event if status changed
      (unless (eq old-status 'idle)
        (hive-mcp-swarm--emit-state-changed slave-id old-status 'idle))
      ;; Log (event emission is now handled by terminal watcher)
      (if is-error
          (message "[swarm] Task error: %s (%s, %.1fs)"
                   slave-id (or error-type "unknown") (or duration-secs 0))
        (message "[swarm] Task completed: %s (%.1fs)"
                 slave-id (or duration-secs 0))))))

(defun hive-mcp-swarm-start-completion-watcher ()
  "Start the auto-shout completion watcher timer.
Also starts Layer 2 idle detection watcher."
  (interactive)
  (when (fboundp 'hive-mcp-swarm-terminal-start-completion-watcher)
    (hive-mcp-swarm-terminal-start-completion-watcher
     #'hive-mcp-swarm--on-task-completion))
  ;; Layer 2: Start idle detection watcher
  (when (fboundp 'hive-mcp-swarm-terminal-start-idle-watcher)
    (hive-mcp-swarm-terminal-start-idle-watcher))
  (message "[swarm] Completion + idle watchers started (auto-shout + Layer 2)"))

(defun hive-mcp-swarm-stop-completion-watcher ()
  "Stop the auto-shout completion watcher timer.
Also stops Layer 2 idle detection watcher."
  (interactive)
  (when (fboundp 'hive-mcp-swarm-terminal-stop-completion-watcher)
    (hive-mcp-swarm-terminal-stop-completion-watcher))
  ;; Layer 2: Stop idle detection watcher
  (when (fboundp 'hive-mcp-swarm-terminal-stop-idle-watcher)
    (hive-mcp-swarm-terminal-stop-idle-watcher))
  ;; Reset idle state
  (when (fboundp 'hive-mcp-swarm-terminal-reset-idle-state)
    (hive-mcp-swarm-terminal-reset-idle-state))
  (message "[swarm] Completion + idle watchers stopped"))

;;;; Human Mode - Prompt Hooks:
;; Delegated to hive-mcp-swarm-prompts module.

(defalias 'hive-mcp-swarm--update-prompts-buffer 'hive-mcp-swarm-prompts-update-buffer)
(defalias 'hive-mcp-swarm-respond 'hive-mcp-swarm-prompts-respond)
(defalias 'hive-mcp-swarm-approve 'hive-mcp-swarm-prompts-approve)
(defalias 'hive-mcp-swarm-deny 'hive-mcp-swarm-prompts-deny)
(defalias 'hive-mcp-swarm-list-prompts 'hive-mcp-swarm-prompts-list)

;;;; Preset Management:
;; Delegated to hive-mcp-swarm-presets module.

(defalias 'hive-mcp-swarm-reload-presets 'hive-mcp-swarm-presets-reload)
(defalias 'hive-mcp-swarm-list-presets 'hive-mcp-swarm-presets-list)
(defalias 'hive-mcp-swarm--build-system-prompt 'hive-mcp-swarm-presets-build-system-prompt)
(defalias 'hive-mcp-swarm-add-custom-presets-dir 'hive-mcp-swarm-presets-add-custom-dir)
(defalias 'hive-mcp-swarm--role-to-presets 'hive-mcp-swarm-presets-role-to-presets)

;;;; Slave Management:
;; Delegated to hive-mcp-swarm-slaves module.
;; Public API aliases for backward compatibility:

(defalias 'hive-mcp-swarm-spawn 'hive-mcp-swarm-slaves-spawn)
(defalias 'hive-mcp-swarm-kill 'hive-mcp-swarm-slaves-kill)
(defalias 'hive-mcp-swarm-kill-all 'hive-mcp-swarm-slaves-kill-all)

;; Internal function aliases for test compatibility (pre-decomposition API)
(defalias 'hive-mcp-swarm--generate-slave-id 'hive-mcp-swarm-slaves-generate-id)
(defalias 'hive-mcp-swarm--depth-label 'hive-mcp-swarm-slaves--depth-label)

;;;; Task Dispatch and Collection:
;; Delegated to hive-mcp-swarm-tasks module.
;; Public API aliases for backward compatibility:

(defalias 'hive-mcp-swarm-dispatch 'hive-mcp-swarm-tasks-dispatch)
(defalias 'hive-mcp-swarm-collect 'hive-mcp-swarm-tasks-collect)
(defalias 'hive-mcp-swarm-broadcast 'hive-mcp-swarm-tasks-broadcast)

;; Internal function aliases for test compatibility (pre-decomposition API)
(defalias 'hive-mcp-swarm--slave-matches-filter 'hive-mcp-swarm-tasks--slave-matches-filter)

;;;; Status and Monitoring:

;; Helper functions for formatting slave/ling data (CLARITY-L: self-documenting code)

(defun hive-mcp-swarm--get-slave-status-name (slave)
  "Get status name string from SLAVE plist, defaulting to \"unknown\"."
  (symbol-name (or (plist-get slave :status) 'unknown)))

(defun hive-mcp-swarm--get-presets-or-empty (slave)
  "Get presets from SLAVE plist, defaulting to empty vector for JSON."
  (or (plist-get slave :presets) []))

(defun hive-mcp-swarm--format-slave-detail (id slave)
  "Format SLAVE with ID as alist for status API response.
Returns an alist suitable for JSON serialization as an object.
Includes cwd for bootstrap sync to derive project-id (ADR-001 Phase 2)."
  `((slave-id . ,id)
    (name . ,(plist-get slave :name))
    (status . ,(hive-mcp-swarm--get-slave-status-name slave))
    (depth . ,(plist-get slave :depth))
    (parent-id . ,(plist-get slave :parent-id))
    (cwd . ,(plist-get slave :cwd))
    (current-task . ,(plist-get slave :current-task))
    (tasks-completed . ,(plist-get slave :tasks-completed))))

(defun hive-mcp-swarm--format-ling-data (id slave)
  "Format SLAVE with ID as alist for lings list API response.
Returns an alist with slave-id, name, presets, cwd, and status."
  `((slave-id . ,id)
    (name . ,(plist-get slave :name))
    (presets . ,(hive-mcp-swarm--get-presets-or-empty slave))
    (cwd . ,(plist-get slave :cwd))
    (status . ,(hive-mcp-swarm--get-slave-status-name slave))))

(defun hive-mcp-swarm--format-orphan-detail (orphan)
  "Format ORPHAN plist as alist for status API response.
Includes orphan marker and discovery method."
  `((slave-id . ,(plist-get orphan :slave-id))
    (name . ,(plist-get orphan :name))
    (status . "orphan")
    (depth . nil)
    (parent-id . nil)
    (current-task . nil)
    (tasks-completed . nil)
    (orphan . t)
    (discovered-via . "buffer-introspection")))

(defun hive-mcp-swarm--format-orphan-ling (orphan)
  "Format ORPHAN plist as alist for lings list API response."
  `((slave-id . ,(plist-get orphan :slave-id))
    (name . ,(plist-get orphan :name))
    (presets . [])
    (cwd . nil)
    (status . "orphan")
    (orphan . t)))

(defun hive-mcp-swarm--count-slaves-by-status (slaves-hash)
  "Count slaves in SLAVES-HASH by status.
Returns plist with :total, :idle, :working, :error counts."
  (let ((total 0) (idle 0) (working 0) (error-count 0))
    (maphash
     (lambda (_id slave)
       (cl-incf total)
       (pcase (plist-get slave :status)
         ('idle (cl-incf idle))
         ('working (cl-incf working))
         ('error (cl-incf error-count))))
     slaves-hash)
    (list :total total :idle idle :working working :error error-count)))

(defun hive-mcp-swarm--collect-slave-details (slaves-hash)
  "Collect formatted details from all slaves in SLAVES-HASH.
Returns list of alists suitable for JSON serialization."
  (let ((details '()))
    (maphash
     (lambda (id slave)
       (push (hive-mcp-swarm--format-slave-detail id slave) details))
     slaves-hash)
    details))

(defun hive-mcp-swarm--format-pending-prompt (prompt)
  "Format PROMPT plist for API response with ISO timestamp."
  `(:slave-id ,(plist-get prompt :slave-id)
    :prompt ,(plist-get prompt :prompt)
    :timestamp ,(format-time-string "%Y-%m-%dT%H:%M:%S"
                                    (plist-get prompt :timestamp))))

(defun hive-mcp-swarm--introspect-terminal-buffers ()
  "Introspect all swarm terminal buffers to find potentially unregistered lings.
Returns a list of buffer-info plists for buffers not in `hive-mcp-swarm--slaves'.

This catches lings that may have been spawned but failed to register properly,
or whose registration was lost due to hash table issues.

CLARITY: Y - Yield safe failure with fallback discovery."
  (let ((orphan-buffers '())
        (registered-buffers (make-hash-table :test 'equal)))
    ;; Collect all registered buffer names for quick lookup
    (maphash (lambda (_id slave)
               (when-let* ((buf (plist-get slave :buffer)))
                 (when (buffer-live-p buf)
                   (puthash (buffer-name buf) t registered-buffers))))
             hive-mcp-swarm--slaves)
    ;; Find swarm buffers not in registry
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (and (string-prefix-p hive-mcp-swarm-buffer-prefix name)
                   (buffer-live-p buf)
                   (not (gethash name registered-buffers)))
          ;; Extract slave name from buffer name: *swarm-NAME* -> NAME
          (let* ((prefix-len (length hive-mcp-swarm-buffer-prefix))
                 (raw-name (substring name prefix-len))
                 ;; Remove trailing * if present
                 (slave-name (if (string-suffix-p "*" raw-name)
                                 (substring raw-name 0 -1)
                               raw-name))
                 ;; Generate a synthetic slave-id for this orphan
                 (synthetic-id (format "swarm-%s-orphan" slave-name)))
            (push (list :slave-id synthetic-id
                        :name slave-name
                        :buffer buf
                        :status 'unknown
                        :orphan t
                        :discovered-via "buffer-introspection")
                  orphan-buffers)))))
    orphan-buffers))

(defun hive-mcp-swarm-status (&optional slave-id)
  "Get swarm status.
If SLAVE-ID is provided, get that slave's status.
Otherwise return aggregate status.

Uses terminal buffer introspection as fallback to discover any lings
that may have been spawned but not properly registered in the hash table.

CLARITY: Y - Yield safe failure with fallback discovery.
CLARITY: L - Bottom-up reading via extracted helpers."
  (interactive)
  (if slave-id
      (gethash slave-id hive-mcp-swarm--slaves)
    (let* ((counts (hive-mcp-swarm--count-slaves-by-status hive-mcp-swarm--slaves))
           (total (plist-get counts :total))
           (idle (plist-get counts :idle))
           (working (plist-get counts :working))
           (error-count (plist-get counts :error))
           (orphans (hive-mcp-swarm--introspect-terminal-buffers))
           (orphan-count (length orphans))
           (slaves-detail (hive-mcp-swarm--collect-slave-details hive-mcp-swarm--slaves)))
      ;; Add orphaned lings to details and total
      (dolist (orphan orphans)
        (push (hive-mcp-swarm--format-orphan-detail orphan) slaves-detail))
      (cl-incf total orphan-count)

      ;; Convert to vector for consistent JSON array encoding (like hive-mcp-swarm-list-lings)
      (let ((status (hive-mcp-swarm--build-status-response
                     total idle working error-count orphan-count
                     (vconcat (nreverse slaves-detail)))))
        (when (called-interactively-p 'any)
          (message "Swarm: %d slaves (%d idle, %d working%s), %d tasks"
                   total idle working
                   (if (> orphan-count 0)
                       (format ", %d orphan" orphan-count)
                     "")
                   (hash-table-count hive-mcp-swarm--tasks)))
        status))))

(defun hive-mcp-swarm--build-status-response (total idle working error-count orphan-count slaves-detail)
  "Build the complete status response plist.
Arguments are counts and SLAVES-DETAIL is a vector of slave alists.
SLAVES-DETAIL must be a vector for consistent JSON array encoding."
  `(:session-id ,hive-mcp-swarm--session-id
    :status ,(if (> total 0) "active" "inactive")
    :current-depth ,hive-mcp-swarm--current-depth
    :safeguards (:max-depth ,hive-mcp-swarm-max-depth
                 :max-slaves ,hive-mcp-swarm-max-slaves
                 :rate-limit (:window-seconds ,hive-mcp-swarm-rate-limit-window
                              :max-spawns ,hive-mcp-swarm-rate-limit-max-spawns
                              :recent-spawns ,(length hive-mcp-swarm--spawn-timestamps)))
    :slaves (:total ,total :idle ,idle :working ,working
             :error ,error-count :orphan ,orphan-count)
    :tasks (:total ,(hash-table-count hive-mcp-swarm--tasks))
    :slaves-detail ,slaves-detail))

(defun hive-mcp-swarm-list-lings ()
  "List all lings (spawned slaves) with their metadata.
Returns a vector of alists suitable for JSON encoding.
Used by Clojure's query-elisp-lings fallback when registry is empty.

Uses terminal buffer introspection as fallback to discover any lings
that may exist as buffers but weren't registered in the hash table.

Each ling alist contains:
- slave-id: Unique identifier
- name: Display name
- presets: List of applied presets
- cwd: Working directory
- status: Current status (idle/working/error/orphan)

CLARITY: Y - Yield safe failure with fallback discovery.
CLARITY: L - Bottom-up reading via extracted helpers."
  (let ((lings (hive-mcp-swarm--collect-ling-data hive-mcp-swarm--slaves)))
    ;; Include introspected orphan buffers
    (dolist (orphan (hive-mcp-swarm--introspect-terminal-buffers))
      (push (hive-mcp-swarm--format-orphan-ling orphan) lings))
    ;; Return as vector for consistent JSON array encoding
    (vconcat (nreverse lings))))

(defun hive-mcp-swarm--collect-ling-data (slaves-hash)
  "Collect formatted ling data from all slaves in SLAVES-HASH.
Returns list of alists suitable for JSON serialization."
  (let ((lings '()))
    (maphash
     (lambda (id slave)
       (push (hive-mcp-swarm--format-ling-data id slave) lings))
     slaves-hash)
    lings))

(defun hive-mcp-swarm-show-slave (slave-id)
  "Switch to buffer for SLAVE-ID."
  (interactive
   (list (completing-read "Show slave: "
                          (hash-table-keys hive-mcp-swarm--slaves))))
  (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer)
      (message "Slave buffer is dead: %s" slave-id))))

;;;; API for MCP Tools:

(defun hive-mcp-swarm-api-spawn (name presets &optional cwd terminal kanban-task-id context-file)
  "API: Spawn slave NAME with PRESETS in CWD using TERMINAL backend.
KANBAN-TASK-ID optionally links this ling to a kanban task for lifecycle tracking.
CONTEXT-FILE is an optional path to a temp file containing pre-generated
catchup context to inject into the system prompt (Architecture > LLM behavior).
The file is read and deleted after use.
Returns slave-id on success, or error plist on failure."
  (let ((injected-context
         (when (and context-file (file-exists-p context-file))
           (prog1
               (with-temp-buffer
                 (insert-file-contents context-file)
                 (buffer-string))
             ;; Clean up temp file after reading
             (ignore-errors (delete-file context-file))))))
    (hive-mcp-with-fallback
        (hive-mcp-swarm-spawn name :presets presets :cwd cwd
                              :terminal (when terminal (intern terminal))
                              :kanban-task-id kanban-task-id
                              :injected-context injected-context)
      `(:error "spawn-failed" :name ,name :reason "unknown"))))

(defun hive-mcp-swarm-api-dispatch (slave-id prompt &optional timeout-ms)
  "API: Dispatch PROMPT to SLAVE-ID with TIMEOUT-MS.
Returns task-id on success, or error plist on failure."
  (hive-mcp-with-fallback
      (hive-mcp-swarm-dispatch slave-id prompt :timeout timeout-ms)
    `(:error "dispatch-failed" :slave-id ,slave-id :reason "unknown")))

(defun hive-mcp-swarm-api-status ()
  "API: Get swarm status as JSON-serializable plist."
  (hive-mcp-swarm-status))

;; Alias for task completion check - delegated to tasks module
(defalias 'hive-mcp-swarm--check-task-completion 'hive-mcp-swarm-tasks-check-completion)

(defun hive-mcp-swarm-api-collect (task-id &optional timeout-ms)
  "API: Collect result for TASK-ID - NON-BLOCKING with graceful fallback.
Returns immediately with current status:
- :status \"completed\" with :result if done
- :status \"polling\" if still running (client should poll again)
- :status \"timeout\" if task timed out
- :status \"error\" if task failed

TIMEOUT-MS is used to check if task has exceeded its timeout,
but this function never blocks. Wraps implementation in graceful
fallback to ensure MCP clients never receive errors."
  (hive-mcp-with-fallback
      (let* ((task (gethash task-id hive-mcp-swarm--tasks))
             (slave-id (and task (plist-get task :slave-id)))
             (slave (and slave-id (gethash slave-id hive-mcp-swarm--slaves)))
             (dispatched-at (and task (plist-get task :dispatched-at)))
             (task-timeout (or timeout-ms
                               (plist-get task :timeout)
                               hive-mcp-swarm-default-timeout))
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
          (hive-mcp-swarm--emit-task-failed
           task-id slave-id (format "Timed out after %dms" elapsed-ms))
          `(:task-id ,task-id
            :status "timeout"
            :error ,(format "Timed out after %dms" elapsed-ms)
            :elapsed-ms ,elapsed-ms))

         ;; Try to get result (non-blocking check)
         (t
          (if-let* ((result (hive-mcp-swarm--check-task-completion task-id)))
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
                (hive-mcp-swarm--emit-task-completed task-id slave-id result)
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

(defun hive-mcp-swarm-api-list-presets ()
  "API: List available presets."
  (hive-mcp-swarm-list-presets))

(defun hive-mcp-swarm-api-kill (slave-id)
  "API: Kill SLAVE-ID.
Returns result plist. Never fails."
  (hive-mcp-with-fallback
      (progn
        (hive-mcp-swarm-kill slave-id)
        `(:killed ,slave-id))
    `(:error "kill-failed" :slave-id ,slave-id)))

(defun hive-mcp-swarm-api-kill-all ()
  "API: Kill all slaves.
Returns result plist. Never fails."
  (hive-mcp-with-fallback
      (let ((count (hash-table-count hive-mcp-swarm--slaves)))
        (hive-mcp-swarm-kill-all)
        `(:killed-count ,count))
    `(:error "kill-all-failed" :killed-count 0)))

(defun hive-mcp-swarm-api-pending-prompts ()
  "API: Get list of pending prompts awaiting human decision.
Returns list of prompts with slave-id, prompt text, and timestamp.
CLARITY: L - Uses extracted helper for formatting."
  (hive-mcp-with-fallback
      (let* ((pending (hive-mcp-swarm-prompts-get-pending))
             (prompts (mapcar #'hive-mcp-swarm--format-pending-prompt pending)))
        `(:count ,(length prompts)
          :prompts ,prompts
          :mode ,hive-mcp-swarm-prompts-mode))
    `(:error "pending-prompts-failed" :count 0 :prompts nil)))

(defun hive-mcp-swarm-api-respond-prompt (slave-id response)
  "API: Send RESPONSE to the pending prompt from SLAVE-ID.
Returns result plist indicating success or failure."
  (hive-mcp-with-fallback
      (if (hive-mcp-swarm-prompts-respond-to slave-id response)
          `(:success t :slave-id ,slave-id :response ,response)
        `(:success nil :error "no-pending-prompt" :slave-id ,slave-id))
    `(:error "respond-prompt-failed" :slave-id ,slave-id)))

;;;; Transient Menu:

(require 'transient nil t)

(when (featurep 'transient)
  (transient-define-prefix hive-mcp-swarm-transient ()
    "Swarm orchestration menu."
    ["hive-mcp Swarm"
     ["Slaves"
      ("s" "Spawn slave" hive-mcp-swarm-spawn)
      ("k" "Kill slave" hive-mcp-swarm-kill)
      ("K" "Kill all" hive-mcp-swarm-kill-all)
      ("v" "View slave" hive-mcp-swarm-show-slave)]
     ["Tasks"
      ("d" "Dispatch" hive-mcp-swarm-dispatch)
      ("c" "Collect" hive-mcp-swarm-collect)
      ("b" "Broadcast" hive-mcp-swarm-broadcast)]
     ["Prompts (human mode)"
      ("y" "Approve next" hive-mcp-swarm-approve)
      ("n" "Deny next" hive-mcp-swarm-deny)
      ("p" "Respond custom" hive-mcp-swarm-respond)
      ("l" "List pending" hive-mcp-swarm-list-prompts)]
     ["Info"
      ("?" "Status" hive-mcp-swarm-status)
      ("P" "List presets" hive-mcp-swarm-list-presets)
      ("r" "Reload presets" hive-mcp-swarm-reload-presets)
      ("a" "Add presets dir" hive-mcp-swarm-add-custom-presets-dir)]]))

;;;; Minor Mode:

(defvar hive-mcp-swarm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Use C-c M-s (Meta modifier) - allowed for minor modes per Emacs conventions
    (define-key map (kbd "C-c M-s") #'hive-mcp-swarm-transient)
    map)
  "Keymap for `hive-mcp-swarm-mode'.")

;;;###autoload
(define-minor-mode hive-mcp-swarm-mode
  "Minor mode for Claude swarm orchestration.

\\{hive-mcp-swarm-mode-map}"
  :init-value nil
  :lighter " Swarm"
  :keymap hive-mcp-swarm-mode-map
  :global t
  :group 'hive-mcp-swarm
  (if hive-mcp-swarm-mode
      (progn
        (setq hive-mcp-swarm--session-id
              (format "session-%s-%04x"
                      (format-time-string "%Y%m%d")
                      (random 65535)))
        ;; Initialize modules
        (hive-mcp-swarm-hooks-init)
        (hive-mcp-swarm-events-init hive-mcp-swarm--session-id)
        (hive-mcp-swarm-presets-init)
        (hive-mcp-swarm-prompts-init)
        ;; Register hivemind state sync hooks
        (hive-mcp-swarm--register-hivemind-sync-hooks)
        ;; Start watchers if enabled
        (when hive-mcp-swarm-auto-approve
          (hive-mcp-swarm-start-auto-approve))
        ;; Start completion watcher for auto-shout
        (hive-mcp-swarm-start-completion-watcher)
        (message "hive-mcp-swarm enabled (session: %s, auto-approve: %s, auto-shout: on)"
                 hive-mcp-swarm--session-id
                 (if hive-mcp-swarm-auto-approve "on" "off")))
    ;; Shutdown
    (hive-mcp-swarm-stop-auto-approve)
    (hive-mcp-swarm-stop-completion-watcher)
    (hive-mcp-swarm-kill-all)
    ;; Unregister hivemind sync hooks
    (hive-mcp-swarm--unregister-hivemind-sync-hooks)
    ;; Shutdown modules
    (hive-mcp-swarm-hooks-shutdown)
    (hive-mcp-swarm-prompts-shutdown)
    (hive-mcp-swarm-presets-shutdown)
    (hive-mcp-swarm-events-shutdown)
    (message "hive-mcp-swarm disabled")))

;;;; Addon Lifecycle:

(defun hive-mcp-swarm--register-hivemind-sync-hooks ()
  "Register sync handler with all hivemind event types."
  (dolist (event-type '(:hivemind-started :hivemind-progress
                        :hivemind-completed :hivemind-error
                        :hivemind-blocked))
    (hive-mcp-swarm-hooks-register event-type #'hive-mcp-swarm--sync-state-from-hivemind)))

(defun hive-mcp-swarm--unregister-hivemind-sync-hooks ()
  "Unregister sync handler from all hivemind event types."
  (dolist (event-type '(:hivemind-started :hivemind-progress
                        :hivemind-completed :hivemind-error
                        :hivemind-blocked))
    (hive-mcp-swarm-hooks-unregister event-type #'hive-mcp-swarm--sync-state-from-hivemind)))

(defun hive-mcp-swarm--addon-init ()
  "Initialize swarm addon."
  (setq hive-mcp-swarm--session-id
        (format "session-%s-%04x"
                (format-time-string "%Y%m%d")
                (random 65535)))
  ;; Initialize modules
  (hive-mcp-swarm-hooks-init)
  (hive-mcp-swarm-events-init hive-mcp-swarm--session-id)
  (hive-mcp-swarm-presets-init)
  (hive-mcp-swarm-prompts-init)
  ;; Register hivemind state sync hooks
  (hive-mcp-swarm--register-hivemind-sync-hooks)
  ;; Start watchers if enabled
  (when hive-mcp-swarm-auto-approve
    (hive-mcp-swarm-start-auto-approve))
  ;; Start completion watcher for auto-shout
  (hive-mcp-swarm-start-completion-watcher))

(defun hive-mcp-swarm--addon-shutdown ()
  "Shutdown swarm addon - kill all slaves."
  (hive-mcp-swarm-stop-auto-approve)
  (hive-mcp-swarm-stop-completion-watcher)
  (hive-mcp-swarm-kill-all)
  ;; Unregister hivemind sync hooks
  (hive-mcp-swarm--unregister-hivemind-sync-hooks)
  ;; Shutdown modules
  (hive-mcp-swarm-hooks-shutdown)
  (hive-mcp-swarm-prompts-shutdown)
  (hive-mcp-swarm-presets-shutdown)
  (hive-mcp-swarm-events-shutdown)
  (setq hive-mcp-swarm--session-id nil))

;; Register with addon system
(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'swarm
   :version "0.1.0"
   :description "Claude swarm orchestration for parallel task execution"
   :requires '()  ; vterm or eat checked at spawn time
   :provides '(hive-mcp-swarm-mode hive-mcp-swarm-transient)
   :init #'hive-mcp-swarm--addon-init
   :shutdown #'hive-mcp-swarm--addon-shutdown))

(provide 'hive-mcp-swarm)
;;; hive-mcp-swarm.el ends here
