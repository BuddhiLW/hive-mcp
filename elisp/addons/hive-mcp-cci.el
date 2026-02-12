;;; hive-mcp-cci.el --- Swarm integration via claude-code-ide.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, mcp, swarm
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon provides swarm ling orchestration using claude-code-ide.el
;; instead of raw vterm/eat terminal scraping.
;;
;; Benefits over vterm-based swarm:
;; - Structured session management via claude-code-ide
;; - Reliable prompt dispatch (no terminal timing issues)
;; - Task completion via hivemind (lings use hivemind_shout :completed)
;; - No terminal output scraping for response collection
;;
;; Architecture:
;;
;;   Master Claude (you)
;;         │ hive-mcp MCP tools
;;         v
;;   hive-mcp-cci.el (this file)
;;         │ claude-code-ide API
;;         v
;;   ┌─────┴─────┐
;;   │  Lings    │ (claude-code-ide sessions)
;;   │           │ ← hivemind_shout :completed → hivemind coordinator
;;   └───────────┘
;;
;; Completion Flow:
;;   1. Master dispatches task with task-id to ling
;;   2. Ling executes task
;;   3. Ling calls hivemind_shout with event_type="completed"
;;   4. Hivemind coordinator receives structured completion data
;;   5. This addon polls hivemind to update task status
;;
;; Usage:
;;   (require 'hive-mcp-cci)
;;   (hive-mcp-cci-mode 1)
;;
;;   ;; Spawn a ling via claude-code-ide
;;   (hive-mcp-cci-spawn "worker-1" :presets '("hivemind"))
;;
;;   ;; Dispatch task (completion tracked via hivemind)
;;   (hive-mcp-cci-dispatch "ling-worker-1-xxx" "Implement feature X")
;;
;;   ;; Check hivemind for completion status
;;   (hive-mcp-cci-sync-from-hivemind)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;; Soft dependency on claude-code-ide
(declare-function claude-code-ide "claude-code-ide")
(declare-function claude-code-ide--start-session "claude-code-ide")
(declare-function claude-code-ide-send-prompt "claude-code-ide")
(declare-function claude-code-ide-stop "claude-code-ide")

;; Soft dependency on hive-mcp-swarm for preset loading
(declare-function hive-mcp-swarm--build-system-prompt "hive-mcp-swarm")

;; Soft dependency on hive-mcp-hivemind for completion tracking
(declare-function hive-mcp-hivemind-status "hive-mcp-hivemind")
(declare-function hive-mcp-hivemind-agent-messages "hive-mcp-hivemind")

;; Channel for push events
(declare-function hive-mcp-channel-connected-p "hive-mcp-channel")
(declare-function hive-mcp-channel-send "hive-mcp-channel")

;;;; Customization:

(defgroup hive-mcp-cci nil
  "Swarm orchestration via claude-code-ide."
  :group 'hive-mcp
  :prefix "hive-mcp-cci-")

(defcustom hive-mcp-cci-default-timeout 300000
  "Default task timeout in milliseconds (5 minutes)."
  :type 'integer
  :group 'hive-mcp-cci)

(defcustom hive-mcp-cci-max-lings 10
  "Maximum number of concurrent lings."
  :type 'integer
  :group 'hive-mcp-cci)

(defcustom hive-mcp-cci-hivemind-poll-interval 5
  "Interval in seconds for polling hivemind for task completions."
  :type 'integer
  :group 'hive-mcp-cci)

(defcustom hive-mcp-cci-auto-sync t
  "If non-nil, automatically sync task status from hivemind."
  :type 'boolean
  :group 'hive-mcp-cci)

;;;; Internal State:

(defvar hive-mcp-cci--lings (make-hash-table :test 'equal)
  "Hash table of ling-id -> ling plist.")

(defvar hive-mcp-cci--tasks (make-hash-table :test 'equal)
  "Hash table of task-id -> task plist.")

(defvar hive-mcp-cci--pending-completions (make-hash-table :test 'equal)
  "Hash table of task-id -> completion callback.")

(defvar hive-mcp-cci--task-counter 0
  "Counter for generating unique task IDs.")

(defvar hive-mcp-cci--sync-timer nil
  "Timer for automatic hivemind sync.")

;;;; ID Generation:

(defun hive-mcp-cci--generate-ling-id (name)
  "Generate unique ling ID for NAME."
  (format "ling-%s-%d" name (random 999999999)))

(defun hive-mcp-cci--generate-task-id (ling-id)
  "Generate unique task ID for LING-ID."
  (cl-incf hive-mcp-cci--task-counter)
  (format "task-%s-%03d"
          (replace-regexp-in-string "^ling-" "" ling-id)
          hive-mcp-cci--task-counter))

;;;; Hivemind Integration:

(defun hive-mcp-cci--get-hivemind-status ()
  "Get hivemind status, handling both elisp and MCP responses."
  (condition-case nil
      (if (fboundp 'hive-mcp-hivemind-status)
          (hive-mcp-hivemind-status)
        ;; Fallback: query via MCP
        nil)
    (error nil)))

(defun hive-mcp-cci-sync-from-hivemind ()
  "Sync task completion status from hivemind coordinator.
Updates local task records based on hivemind agent messages."
  (interactive)
  (condition-case err
      (let ((hivemind-status (hive-mcp-cci--get-hivemind-status))
            (updated 0))
    (when hivemind-status
      ;; Check each task for completion in hivemind
      (maphash
       (lambda (task-id task)
         (when (eq (plist-get task :status) 'dispatched)
           ;; Look for completion message in hivemind
           (let* ((ling-id (plist-get task :ling-id))
                  (agent-id (plist-get (gethash ling-id hive-mcp-cci--lings) :hivemind-agent))
                  (agents (alist-get 'agents hivemind-status))
                  (agent-data (when (and agent-id agents)
                               (alist-get (intern agent-id) agents))))
             (when agent-data
               (let ((agent-status (alist-get 'status agent-data))
                     (messages (alist-get 'messages agent-data)))
                 ;; Check if agent reported completion for this task
                 (when (and (equal agent-status "completed")
                            messages)
                   (dolist (msg messages)
                     (let ((event-type (alist-get 'event-type msg))
                           (msg-task (alist-get 'task msg))
                           (data (alist-get 'data msg)))
                       (when (and (equal event-type "completed")
                                  (or (equal msg-task task-id)
                                      (equal (alist-get 'task_id data) task-id)))
                         ;; Found completion!
                         (hive-mcp-cci--handle-hivemind-completion
                          task-id
                          (or (alist-get 'status data) "success")
                          (or (alist-get 'result data) (alist-get 'message msg))
                          (alist-get 'files_modified data))
                         (cl-incf updated))))))))))
       hive-mcp-cci--tasks))
    (when (and (called-interactively-p 'any) (> updated 0))
      (message "[cci] Synced %d task completions from hivemind" updated))
    updated)
    (error
     (message "[cci] Hivemind sync error: %s"
              (error-message-string err))
     0)))

(defun hive-mcp-cci--handle-hivemind-completion (task-id status result files)
  "Handle completion of TASK-ID with STATUS, RESULT, and FILES from hivemind."
  (when-let* ((task (gethash task-id hive-mcp-cci--tasks)))
    ;; Update task record
    (plist-put task :status (intern (or status "success")))
    (plist-put task :result result)
    (plist-put task :files-modified files)
    (plist-put task :completed-at (format-time-string "%FT%T%z"))

    ;; Update ling state
    (when-let* ((ling-id (plist-get task :ling-id))
                (ling (gethash ling-id hive-mcp-cci--lings)))
      (plist-put ling :status 'idle)
      (plist-put ling :current-task nil)
      (cl-incf (plist-get ling :tasks-completed)))

    ;; Call completion callback if registered
    (when-let* ((callback (gethash task-id hive-mcp-cci--pending-completions)))
      (remhash task-id hive-mcp-cci--pending-completions)
      (funcall callback task))

    ;; Emit channel event
    (hive-mcp-cci--emit-event "task-completed"
                                `(("task-id" . ,task-id)
                                  ("status" . ,status)
                                  ("ling-id" . ,(plist-get task :ling-id))))

    (message "[cci] Task %s completed via hivemind: %s" task-id status)))

(defun hive-mcp-cci--start-sync-timer ()
  "Start the automatic hivemind sync timer."
  (when (and hive-mcp-cci-auto-sync (not hive-mcp-cci--sync-timer))
    (setq hive-mcp-cci--sync-timer
          (run-with-timer hive-mcp-cci-hivemind-poll-interval
                          hive-mcp-cci-hivemind-poll-interval
                          #'hive-mcp-cci-sync-from-hivemind))))

(defun hive-mcp-cci--stop-sync-timer ()
  "Stop the automatic hivemind sync timer."
  (when hive-mcp-cci--sync-timer
    (cancel-timer hive-mcp-cci--sync-timer)
    (setq hive-mcp-cci--sync-timer nil)))

;;;; Event Emission:

(defun hive-mcp-cci--emit-event (event-type data)
  "Emit EVENT-TYPE with DATA via channel if connected."
  (when (and (fboundp 'hive-mcp-channel-connected-p)
             (hive-mcp-channel-connected-p))
    (ignore-errors
      (hive-mcp-channel-send
       `(("type" . ,(format "cci-%s" event-type))
         ("data" . ,data)
         ("timestamp" . ,(format-time-string "%FT%T%z")))))))

;;;; Ling Management:

;;;###autoload
(cl-defun hive-mcp-cci-spawn (name &key presets cwd hivemind-agent)
  "Spawn a new ling with NAME using claude-code-ide.

PRESETS is a list of preset names (uses hive-mcp-swarm presets).
CWD is the working directory.
HIVEMIND-AGENT is the agent ID to use for hivemind tracking (defaults to NAME).

Returns ling-id immediately.  Session starts async."
  (interactive (list (read-string "Ling name: ")))

  (unless (featurep 'claude-code-ide)
    (error "claude-code-ide not available"))

  (when (>= (hash-table-count hive-mcp-cci--lings) hive-mcp-cci-max-lings)
    (error "Maximum ling count (%d) reached" hive-mcp-cci-max-lings))

  (let* ((ling-id (hive-mcp-cci--generate-ling-id name))
         (work-dir (or cwd (when (fboundp 'project-root)
                            (when-let* ((proj (project-current)))
                              (project-root proj)))
                       default-directory))
         (agent-id (or hivemind-agent name))
         (system-prompt (when (and presets (featurep 'hive-mcp-swarm))
                          (hive-mcp-swarm--build-system-prompt presets))))

    ;; Register ling immediately
    (puthash ling-id
             (list :ling-id ling-id
                   :name name
                   :presets presets
                   :hivemind-agent agent-id
                   :status 'starting
                   :cwd work-dir
                   :session nil
                   :current-task nil
                   :tasks-completed 0
                   :spawned-at (format-time-string "%FT%T%z"))
             hive-mcp-cci--lings)

    ;; Start claude-code-ide session async
    (run-with-timer
     0 nil
     (lambda ()
       (condition-case err
           (let ((default-directory work-dir))
             ;; Start session
             (claude-code-ide)
             ;; Wait for session to initialize
             (run-with-timer
              2 nil
              (lambda ()
                (condition-case err
                    (when-let* ((ling (gethash ling-id hive-mcp-cci--lings)))
                      ;; Send system prompt if we have one
                      (when system-prompt
                        (claude-code-ide-send-prompt system-prompt))
                      ;; Update status
                      (plist-put ling :status 'idle)
                      (message "[cci] Ling %s ready (hivemind: %s)" ling-id agent-id))
                  (error
                   (message "[cci] Timer error initializing ling %s: %s"
                            ling-id (error-message-string err)))))))
         (error
          (when-let* ((ling (gethash ling-id hive-mcp-cci--lings)))
            (plist-put ling :status 'error)
            (plist-put ling :error (error-message-string err)))
          (message "[cci] Spawn error: %s" (error-message-string err))))))

    (message "[cci] Spawning ling %s..." ling-id)
    ling-id))

;;;###autoload
(defun hive-mcp-cci-kill (ling-id)
  "Kill ling LING-ID."
  (interactive
   (list (completing-read "Kill ling: "
                          (hash-table-keys hive-mcp-cci--lings))))
  (when-let* ((ling (gethash ling-id hive-mcp-cci--lings)))
    (let ((cwd (plist-get ling :cwd)))
      (when cwd
        (let ((default-directory cwd))
          (ignore-errors (claude-code-ide-stop)))))
    (remhash ling-id hive-mcp-cci--lings)
    (message "[cci] Killed ling: %s" ling-id)))

;;;###autoload
(defun hive-mcp-cci-kill-all ()
  "Kill all lings."
  (interactive)
  (maphash (lambda (id _) (hive-mcp-cci-kill id)) hive-mcp-cci--lings)
  (clrhash hive-mcp-cci--lings)
  (message "[cci] Killed all lings"))

;;;; Task Dispatch:

;;;###autoload
(cl-defun hive-mcp-cci-dispatch (ling-id prompt &key timeout callback)
  "Dispatch PROMPT to LING-ID.

TIMEOUT is milliseconds (default `hive-mcp-cci-default-timeout').
CALLBACK is called with task plist when complete (via hivemind sync).

Returns task-id.

The ling should call hivemind_shout with event_type=completed when done.
Use `hive-mcp-cci-sync-from-hivemind' to poll for completion, or rely
on automatic sync if `hive-mcp-cci-auto-sync' is enabled."
  (let* ((ling (gethash ling-id hive-mcp-cci--lings))
         (task-id (hive-mcp-cci--generate-task-id ling-id))
         (agent-id (plist-get ling :hivemind-agent)))

    (unless ling
      (error "Ling not found: %s" ling-id))

    (unless (eq (plist-get ling :status) 'idle)
      (error "Ling %s is busy (status: %s)" ling-id (plist-get ling :status)))

    ;; Create task record
    (puthash task-id
             (list :task-id task-id
                   :ling-id ling-id
                   :prompt prompt
                   :status 'dispatched
                   :timeout (or timeout hive-mcp-cci-default-timeout)
                   :dispatched-at (format-time-string "%FT%T%z")
                   :completed-at nil
                   :result nil)
             hive-mcp-cci--tasks)

    ;; Register callback if provided
    (when callback
      (puthash task-id callback hive-mcp-cci--pending-completions))

    ;; Update ling state
    (plist-put ling :status 'working)
    (plist-put ling :current-task task-id)

    ;; Build prompt with hivemind completion instructions
    (let* ((cwd (plist-get ling :cwd))
           (task-prompt (format "## Task ID: %s

%s

---
**IMPORTANT - Completion Reporting:**
When you complete this task, use `hivemind_shout` to report:
```
hivemind_shout(
  agent_id: \"%s\",
  event_type: \"completed\",
  task: \"%s\",
  message: \"<brief summary>\",
  data: {
    \"task_id\": \"%s\",
    \"status\": \"success\" | \"error\" | \"partial\",
    \"result\": \"<your findings/output>\",
    \"files_modified\": [\"<list of changed files>\"]
  }
)
```"
                                task-id prompt agent-id task-id task-id)))

      ;; Send to ling via claude-code-ide
      (let ((default-directory (or cwd default-directory)))
        (claude-code-ide-send-prompt task-prompt)))

    (message "[cci] Dispatched task %s to %s" task-id ling-id)
    task-id))

;;;; Status:

;;;###autoload
(defun hive-mcp-cci-status ()
  "Get swarm status."
  (interactive)
  (let ((total 0) (idle 0) (working 0) (error-count 0)
        (lings-detail '()))

    (maphash
     (lambda (id ling)
       (cl-incf total)
       (pcase (plist-get ling :status)
         ('idle (cl-incf idle))
         ('working (cl-incf working))
         ('error (cl-incf error-count)))
       (push (list :id id
                   :name (plist-get ling :name)
                   :hivemind-agent (plist-get ling :hivemind-agent)
                   :status (plist-get ling :status)
                   :current-task (plist-get ling :current-task)
                   :tasks-completed (plist-get ling :tasks-completed))
             lings-detail))
     hive-mcp-cci--lings)

    (let ((status `(:backend "claude-code-ide"
                    :completion-mechanism "hivemind"
                    :auto-sync ,hive-mcp-cci-auto-sync
                    :lings (:total ,total :idle ,idle :working ,working :error ,error-count)
                    :tasks (:total ,(hash-table-count hive-mcp-cci--tasks)
                            :pending ,(hash-table-count hive-mcp-cci--pending-completions))
                    :lings-detail ,(nreverse lings-detail))))
      (when (called-interactively-p 'any)
        (message "CCI Swarm: %d lings (%d idle, %d working), %d tasks, completion: hivemind"
                 total idle working (hash-table-count hive-mcp-cci--tasks)))
      status)))

;;;; API for MCP Tools:

(defun hive-mcp-cci-api-spawn (name presets &optional cwd)
  "API: Spawn ling NAME with PRESETS in CWD."
  (condition-case err
      (hive-mcp-cci-spawn name :presets presets :cwd cwd)
    (error `(:error "spawn-failed" :reason ,(error-message-string err)))))

(defun hive-mcp-cci-api-dispatch (ling-id prompt &optional timeout-ms)
  "API: Dispatch PROMPT to LING-ID with TIMEOUT-MS."
  (condition-case err
      (hive-mcp-cci-dispatch ling-id prompt :timeout timeout-ms)
    (error `(:error "dispatch-failed" :reason ,(error-message-string err)))))

(defun hive-mcp-cci-api-status ()
  "API: Get status."
  (hive-mcp-cci-status))

(defun hive-mcp-cci-api-collect (task-id)
  "API: Get task TASK-ID result."
  (if-let* ((task (gethash task-id hive-mcp-cci--tasks)))
      task
    `(:error "task-not-found" :task-id ,task-id)))

(defun hive-mcp-cci-api-sync ()
  "API: Sync task status from hivemind."
  (hive-mcp-cci-sync-from-hivemind))

;;;; Minor Mode:

;;;###autoload
(define-minor-mode hive-mcp-cci-mode
  "Minor mode for swarm orchestration via claude-code-ide.

Provides structured communication with ling Claude instances
using hivemind for task completion tracking.

Key features:
- Spawn lings via claude-code-ide (not raw vterm)
- Dispatch tasks with structured prompts
- Completion tracking via hivemind_shout
- Automatic sync from hivemind coordinator"
  :init-value nil
  :lighter " CCI"
  :global t
  :group 'hive-mcp-cci

  (if hive-mcp-cci-mode
      (progn
        (unless (require 'claude-code-ide nil t)
          (setq hive-mcp-cci-mode nil)
          (error "claude-code-ide not available"))
        ;; Start auto-sync timer
        (hive-mcp-cci--start-sync-timer)
        (message "hive-mcp-cci enabled (hivemind completion)"))
    ;; Cleanup
    (hive-mcp-cci--stop-sync-timer)
    (hive-mcp-cci-kill-all)
    (message "hive-mcp-cci disabled")))

;; Backwards compatibility alias (use prefix for package-lint compliance)
(defalias 'hive-mcp-cci-claude-code-ide-mode 'hive-mcp-cci-mode
  "Backwards compatibility alias for `hive-mcp-cci-mode'.")

;;;; Addon Registration:

(when (fboundp 'hive-mcp-addon-register)
  (hive-mcp-addon-register
   'cci
   :version "0.1.0"
   :description "Swarm orchestration via claude-code-ide.el with hivemind completion"
   :requires '()  ; claude-code-ide is a soft dependency
   :provides '(hive-mcp-cci-mode)))

(provide 'hive-mcp-cci)
;;; hive-mcp-cci.el ends here
