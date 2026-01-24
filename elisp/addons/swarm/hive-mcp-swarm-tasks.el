;;; hive-mcp-swarm-tasks.el --- Task dispatch and collection for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Task dispatch and collection for hive-mcp-swarm.
;; Handles sending prompts to slaves and collecting responses.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles task dispatch/collection
;; - Open/Closed: Response extraction extensible via patterns
;; - Dependency Inversion: Callers depend on dispatch/collect API

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Dependencies on sibling modules
(declare-function hive-mcp-swarm-terminal-send "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-terminal-ready-p "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-terminal-wait-ready "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-slaves-generate-task-id "hive-mcp-swarm-slaves")
(declare-function hive-mcp-swarm-events-emit-dispatch-dropped "hive-mcp-swarm-events")

;; Dependencies on main module (variables)
(defvar hive-mcp-swarm--slaves)
(defvar hive-mcp-swarm--tasks)
(defvar hive-mcp-swarm-terminal)
(defvar hive-mcp-swarm-default-timeout)

;;;; ACID Dispatch Queue:
;;
;; Solves the race condition where spawn (async) completes after dispatch,
;; causing the prompt to be lost. The queue ensures ACID properties:
;; - Atomic: Dispatch either succeeds or retries
;; - Consistent: Queue state always valid
;; - Isolated: Multiple dispatches don't interfere
;; - Durable: Queued dispatches survive Emacs blocking
;;
;; Flow: Spawn → Queue(dispatch) → Wait-Ready → Dequeue → Send

(defvar hive-mcp-swarm-dispatch-queue '()
  "ACID queue for pending dispatches.
Each element is a plist:
  (:slave-id ID :prompt TEXT :timeout MS :priority PRI
   :context CTX :queued-at TIME :retries N)")

(defvar hive-mcp-swarm-dispatch-queue-timer nil
  "Timer for processing the dispatch queue.")

(defcustom hive-mcp-swarm-dispatch-queue-interval 0.5
  "Interval in seconds between queue processing attempts."
  :type 'float
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-dispatch-max-retries 20
  "Maximum retries before dropping a queued dispatch.
At 0.5s intervals, 20 retries = 10 seconds max wait."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-dispatch-queue-enabled t
  "If non-nil, use ACID queue for dispatches.
When enabled, dispatches go to queue and are processed when terminal ready.
When disabled, dispatches are sent immediately (legacy behavior)."
  :type 'boolean
  :group 'hive-mcp-swarm)

;;;; Buffer Detection Helpers:

(defun hive-mcp-swarm-tasks--buffer-contains-p (buffer text &optional start-point)
  "Check if BUFFER contains TEXT after START-POINT.
Uses first 40 chars as signature.  Returns position if found, nil otherwise."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start-point (point-min)))
        (search-forward (substring text 0 (min 40 (length text))) nil t)))))

(defun hive-mcp-swarm-tasks--claude-responded-p (buffer text &optional start-point)
  "Check if Claude has responded to TEXT in BUFFER after START-POINT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start-point (point-min)))
        (when (search-forward (substring text 0 (min 40 (length text))) nil t)
          (search-forward "●" nil t))))))

;;;; Response Extraction (DRY helper):

(defun hive-mcp-swarm-tasks--extract-response (buffer prompt)
  "Extract Claude's response to PROMPT from BUFFER.
Returns response string if complete, nil if still running."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((search-key (substring prompt 0 (min 50 (length prompt)))))
          (when (and (search-forward search-key nil t)
                     (search-forward "●" nil t))
            (let ((response-start (point)))
              (cond
               ((search-forward "\n> " nil t)
                (string-trim (buffer-substring-no-properties response-start (- (point) 3))))
               ((search-forward "\n────" nil t)
                (string-trim (buffer-substring-no-properties response-start (- (point) 5))))
               ((search-forward "\n\n\n" nil t)
                (string-trim (buffer-substring-no-properties response-start (- (point) 3))))))))))))

;;;; Filter Matching:

(defun hive-mcp-swarm-tasks--slave-matches-filter (slave filter)
  "Check if SLAVE matches FILTER criteria (:role, :status)."
  (and (or (not (plist-get filter :role))
           (equal (plist-get slave :role) (plist-get filter :role)))
       (or (not (plist-get filter :status))
           (eq (plist-get slave :status) (plist-get filter :status)))))

;;;; Queue Operations:

(defun hive-mcp-swarm-enqueue-dispatch (slave-id prompt &optional timeout priority context)
  "Add dispatch to ACID queue atomically.
SLAVE-ID is the target slave, PROMPT is the text to send.
Optional TIMEOUT, PRIORITY, and CONTEXT are passed to actual dispatch.
Returns the queue entry plist."
  (let ((entry (list :slave-id slave-id
                     :prompt prompt
                     :timeout timeout
                     :priority (or priority 'normal)
                     :context context
                     :queued-at (float-time)
                     :retries 0)))
    ;; Atomic append (thread-safe in Emacs single-threaded model)
    (setq hive-mcp-swarm-dispatch-queue
          (append hive-mcp-swarm-dispatch-queue (list entry)))
    (message "[swarm-tasks] Queued dispatch for %s (queue depth: %d)"
             slave-id (length hive-mcp-swarm-dispatch-queue))
    ;; Ensure queue processor is running
    (hive-mcp-swarm-start-queue-processor)
    entry))

(defun hive-mcp-swarm-dequeue-dispatch ()
  "Remove and return the first queue entry (FIFO).
Returns nil if queue is empty."
  (when hive-mcp-swarm-dispatch-queue
    (let ((entry (car hive-mcp-swarm-dispatch-queue)))
      (setq hive-mcp-swarm-dispatch-queue (cdr hive-mcp-swarm-dispatch-queue))
      entry)))

(defun hive-mcp-swarm-peek-queue ()
  "Return first queue entry without removing it."
  (car hive-mcp-swarm-dispatch-queue))

(defun hive-mcp-swarm-queue-depth ()
  "Return current queue depth."
  (length hive-mcp-swarm-dispatch-queue))

(defun hive-mcp-swarm--slave-ready-p (slave-id)
  "Check if SLAVE-ID terminal is ready to receive input."
  (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer)))
    (and (buffer-live-p buffer)
         (hive-mcp-swarm-terminal-ready-p buffer))))

(defun hive-mcp-swarm--notify-dispatch-dropped (slave-id reason prompt retries queued-at)
  "Notify coordinator and user that a dispatch was dropped.
SLAVE-ID is the target slave.
REASON is why the dispatch was dropped (\"max-retries-exceeded\" or \"slave-dead\").
PROMPT is the original prompt text.
RETRIES is the number of retry attempts.
QUEUED-AT is when the dispatch was queued (epoch time).

This fixes the silent drop bug by:
1. Emitting a dispatch-dropped event via channel (for MCP/coordinator)
2. Displaying a warning to the user (visible in *Warnings* buffer)"
  ;; Emit event for MCP/coordinator (load events module if available)
  (when (require 'hive-mcp-swarm-events nil t)
    (hive-mcp-swarm-events-emit-dispatch-dropped
     slave-id reason prompt retries queued-at))
  ;; Show user-facing warning
  (display-warning
   'hive-mcp-swarm
   (format "DISPATCH DROPPED: %s - %s after %d retries. Prompt: %.50s..."
           slave-id reason retries (or prompt ""))
   :warning))

(defun hive-mcp-swarm--process-queue-entry (entry)
  "Process a single queue ENTRY, dispatching if terminal ready.
Returns:
  :success - dispatch succeeded
  :retry   - terminal not ready, should retry
  :failed  - max retries exceeded or slave dead"
  (let* ((slave-id (plist-get entry :slave-id))
         (prompt (plist-get entry :prompt))
         (retries (plist-get entry :retries))
         (timeout (plist-get entry :timeout))
         (priority (plist-get entry :priority))
         (context (plist-get entry :context))
         (queued-at (plist-get entry :queued-at))
         (slave (gethash slave-id hive-mcp-swarm--slaves)))
    (cond
     ;; Slave doesn't exist or buffer dead - fail
     ((or (not slave)
          (not (buffer-live-p (plist-get slave :buffer))))
      (message "[swarm-tasks] Queue: %s slave dead, dropping dispatch" slave-id)
      (hive-mcp-swarm--notify-dispatch-dropped
       slave-id "slave-dead" prompt retries queued-at)
      :failed)
     ;; Max retries exceeded - fail
     ((>= retries hive-mcp-swarm-dispatch-max-retries)
      (message "[swarm-tasks] Queue: %s max retries (%d) exceeded, dropping"
               slave-id retries)
      (hive-mcp-swarm--notify-dispatch-dropped
       slave-id "max-retries-exceeded" prompt retries queued-at)
      :failed)
     ;; Terminal ready - dispatch!
     ((hive-mcp-swarm--slave-ready-p slave-id)
      (condition-case err
          (progn
            (hive-mcp-swarm-tasks-dispatch-immediate
             slave-id prompt
             :timeout timeout :priority priority :context context)
            (message "[swarm-tasks] Queue: %s dispatched after %d retries"
                     slave-id retries)
            :success)
        (error
         (message "[swarm-tasks] Queue: %s dispatch error: %s"
                  slave-id (error-message-string err))
         (hive-mcp-swarm--notify-dispatch-dropped
          slave-id "dispatch-error" prompt retries queued-at)
         :failed)))
     ;; Not ready - retry
     (t
      (message "[swarm-tasks] Queue: %s not ready (retry %d/%d)"
               slave-id (1+ retries) hive-mcp-swarm-dispatch-max-retries)
      :retry))))

(defun hive-mcp-swarm-process-queue ()
  "Process all ready entries in the dispatch queue.
Entries for ready terminals are dispatched.
Entries for non-ready terminals are re-queued with incremented retry count.
Called periodically by the queue timer."
  (when hive-mcp-swarm-dispatch-queue
    (let ((current-queue hive-mcp-swarm-dispatch-queue)
          (new-queue '()))
      ;; Clear queue atomically before processing
      (setq hive-mcp-swarm-dispatch-queue nil)
      ;; Process each entry
      (dolist (entry current-queue)
        (pcase (hive-mcp-swarm--process-queue-entry entry)
          (:retry
           ;; Re-queue with incremented retry count
           (plist-put entry :retries (1+ (plist-get entry :retries)))
           (push entry new-queue))
          (:success nil)  ; Done, don't re-queue
          (:failed nil))) ; Failed, don't re-queue
      ;; Add remaining entries back to queue (in original order)
      (setq hive-mcp-swarm-dispatch-queue (nreverse new-queue))
      ;; Stop timer if queue empty
      (when (null hive-mcp-swarm-dispatch-queue)
        (hive-mcp-swarm-stop-queue-processor)))))

(defun hive-mcp-swarm-start-queue-processor ()
  "Start the queue processor timer if not already running."
  (unless hive-mcp-swarm-dispatch-queue-timer
    (setq hive-mcp-swarm-dispatch-queue-timer
          (run-with-timer
           hive-mcp-swarm-dispatch-queue-interval
           hive-mcp-swarm-dispatch-queue-interval
           #'hive-mcp-swarm-process-queue))
    (message "[swarm-tasks] Queue processor started (interval: %.1fs)"
             hive-mcp-swarm-dispatch-queue-interval)))

(defun hive-mcp-swarm-stop-queue-processor ()
  "Stop the queue processor timer."
  (when hive-mcp-swarm-dispatch-queue-timer
    (cancel-timer hive-mcp-swarm-dispatch-queue-timer)
    (setq hive-mcp-swarm-dispatch-queue-timer nil)
    (message "[swarm-tasks] Queue processor stopped")))

(defun hive-mcp-swarm-clear-queue ()
  "Clear all pending dispatches from the queue.
Use with caution - dispatches will be lost."
  (interactive)
  (let ((count (length hive-mcp-swarm-dispatch-queue)))
    (setq hive-mcp-swarm-dispatch-queue nil)
    (hive-mcp-swarm-stop-queue-processor)
    (message "[swarm-tasks] Cleared %d queued dispatches" count)))

(defun hive-mcp-swarm-queue-status ()
  "Return queue status as a plist for diagnostics."
  (list :depth (length hive-mcp-swarm-dispatch-queue)
        :processor-running (not (null hive-mcp-swarm-dispatch-queue-timer))
        :entries (mapcar (lambda (e)
                           (list :slave-id (plist-get e :slave-id)
                                 :retries (plist-get e :retries)
                                 :queued-at (plist-get e :queued-at)))
                         hive-mcp-swarm-dispatch-queue)))

;;;; Task Dispatch:

(cl-defun hive-mcp-swarm-tasks-dispatch-immediate (slave-id prompt &key timeout priority context)
  "Dispatch PROMPT to SLAVE-ID (NON-BLOCKING).
TIMEOUT is milliseconds, PRIORITY is critical/high/normal/low.
Returns task-id."
  (interactive
   (list (completing-read "Slave: " (hash-table-keys hive-mcp-swarm--slaves))
         (read-string "Prompt: ")))
  (let* ((slave (gethash slave-id hive-mcp-swarm--slaves))
         (task-id (hive-mcp-swarm-slaves-generate-task-id slave-id))
         (buffer (plist-get slave :buffer)))
    (unless slave (error "Slave not found: %s" slave-id))
    (unless (buffer-live-p buffer) (error "Slave buffer is dead: %s" slave-id))
    ;; Create task record
    (puthash task-id
             (list :task-id task-id :slave-id slave-id :prompt prompt
                   :status 'dispatched :priority (or priority 'normal)
                   :timeout (or timeout hive-mcp-swarm-default-timeout)
                   :context context :dispatched-at (format-time-string "%FT%T%z")
                   :completed-at nil :result nil :error nil)
             hive-mcp-swarm--tasks)
    ;; Update slave state
    (plist-put slave :status 'working)
    (plist-put slave :current-task task-id)
    (plist-put slave :last-activity (format-time-string "%FT%T%z"))
    (plist-put slave :task-start-point (with-current-buffer buffer (point-max)))
    ;; Send prompt - NON-BLOCKING
    (condition-case err
        (hive-mcp-swarm-terminal-send buffer prompt
                                      (or (plist-get slave :terminal) hive-mcp-swarm-terminal))
      (error (message "[swarm-tasks] Dispatch error: %s" (error-message-string err))))
    (when (called-interactively-p 'any)
      (message "Dispatched task %s to %s" task-id slave-id))
    task-id))

(cl-defun hive-mcp-swarm-tasks-dispatch (slave-id prompt &key timeout priority context)
  "Dispatch PROMPT to SLAVE-ID with ACID queue support.
If `hive-mcp-swarm-dispatch-queue-enabled' is non-nil AND terminal not ready,
queues the dispatch for later processing when terminal becomes ready.
Otherwise dispatches immediately.

This fixes the race condition where spawn (async) completes after dispatch,
causing prompts to be lost.

TIMEOUT is milliseconds, PRIORITY is critical/high/normal/low.
Returns task-id (immediate) or queue entry (queued)."
  (interactive
   (list (completing-read "Slave: " (hash-table-keys hive-mcp-swarm--slaves))
         (read-string "Prompt: ")))
  (if (and hive-mcp-swarm-dispatch-queue-enabled
           (not (hive-mcp-swarm--slave-ready-p slave-id)))
      ;; Queue for later - terminal not ready
      (progn
        (message "[swarm-tasks] Terminal not ready, queuing dispatch for %s" slave-id)
        (hive-mcp-swarm-enqueue-dispatch slave-id prompt timeout priority context))
    ;; Dispatch immediately - terminal ready or queue disabled
    (hive-mcp-swarm-tasks-dispatch-immediate slave-id prompt
                                              :timeout timeout
                                              :priority priority
                                              :context context)))

;;;; Task Collection:

(defun hive-mcp-swarm-tasks-collect (task-id &optional timeout-ms)
  "Collect response for TASK-ID (BLOCKING).
TIMEOUT-MS defaults to 5000.  Returns task plist with :result."
  (interactive
   (list (completing-read "Task: " (hash-table-keys hive-mcp-swarm--tasks))))
  (let* ((task (or (gethash task-id hive-mcp-swarm--tasks)
                   (error "Task not found: %s" task-id)))
         (slave (gethash (plist-get task :slave-id) hive-mcp-swarm--slaves))
         (buffer (plist-get slave :buffer))
         (timeout (/ (or timeout-ms 5000) 1000.0))
         (start-time (float-time))
         result)
    ;; Poll for completion
    (while (and (< (- (float-time) start-time) timeout) (not result))
      (setq result (hive-mcp-swarm-tasks--extract-response buffer (plist-get task :prompt)))
      (unless result (sleep-for 0.5)))
    ;; Update task and slave
    (if result
        (progn
          (plist-put task :status 'completed)
          (plist-put task :result result)
          (plist-put task :completed-at (format-time-string "%FT%T%z"))
          (plist-put slave :status 'idle)
          (plist-put slave :current-task nil)
          (plist-put slave :tasks-completed (1+ (plist-get slave :tasks-completed))))
      (plist-put task :status 'timeout)
      (plist-put task :error "Collection timed out"))
    (when (called-interactively-p 'any)
      (message (if result "Collected result (%d chars)" "Collection timed out")
               (length (or result ""))))
    task))

;;;; Broadcast:

(defun hive-mcp-swarm-tasks-broadcast (prompt &optional slave-filter)
  "Send PROMPT to all slaves matching SLAVE-FILTER (:role, :status).
Returns list of task-ids."
  (let ((task-ids '()))
    (maphash (lambda (slave-id slave)
               (when (or (not slave-filter)
                         (hive-mcp-swarm-tasks--slave-matches-filter slave slave-filter))
                 (push (hive-mcp-swarm-tasks-dispatch slave-id prompt) task-ids)))
             hive-mcp-swarm--slaves)
    (nreverse task-ids)))

;;;; Non-blocking Completion Check:

(defun hive-mcp-swarm-tasks-check-completion (task-id)
  "Check if TASK-ID has completed (NON-BLOCKING).
Returns result string if complete, nil if still running."
  (when-let* ((task (gethash task-id hive-mcp-swarm--tasks))
              (slave (gethash (plist-get task :slave-id) hive-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer)))
    (hive-mcp-swarm-tasks--extract-response buffer (plist-get task :prompt))))

;;;; Lifecycle:

(defun hive-mcp-swarm-tasks-init ()
  "Initialize tasks module.
Clears any stale queue state from previous sessions."
  (setq hive-mcp-swarm-dispatch-queue nil)
  (message "[swarm-tasks] Tasks module initialized (ACID queue: %s)"
           (if hive-mcp-swarm-dispatch-queue-enabled "enabled" "disabled")))

(defun hive-mcp-swarm-tasks-shutdown ()
  "Shutdown tasks module.
Stops queue processor and clears queue."
  (hive-mcp-swarm-stop-queue-processor)
  (let ((dropped (length hive-mcp-swarm-dispatch-queue)))
    (setq hive-mcp-swarm-dispatch-queue nil)
    (when (> dropped 0)
      (message "[swarm-tasks] Warning: dropped %d queued dispatches on shutdown"
               dropped))))

(provide 'hive-mcp-swarm-tasks)
;;; hive-mcp-swarm-tasks.el ends here
