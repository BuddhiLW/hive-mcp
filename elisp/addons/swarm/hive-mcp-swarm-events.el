;;; hive-mcp-swarm-events.el --- Channel event emission for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Centralized channel event emission for hive-mcp-swarm.
;; All push-based updates flow through this module.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles event emission
;; - Open/Closed: New event types via new functions, not modification
;; - Dependency Inversion: Callers depend on emit API, not channel internals
;;
;; Event types emitted:
;; - slave-spawned: New slave created
;; - slave-killed: Slave terminated
;; - task-completed: Task finished successfully
;; - task-failed: Task failed or timed out
;; - prompt-shown: Permission prompt detected (human mode)
;; - state-changed: Slave status transition
;; - auto-started: Task auto-detected as starting (terminal send)
;; - auto-completed: Task auto-detected as finished (terminal ready transition)
;; - auto-error: Error auto-detected in terminal output (pattern matching)
;; - idle-timeout: Ling went silent without shouting (Layer 2 convergence)
;; - prompt-stall: Ling idle WITH pending prompt (coordinator hasn't responded)
;; - ling-ready-for-wrap: Ling completed work and is ready for auto-wrap

;;; Code:

(require 'cl-lib)

;; Soft dependency on channel
(declare-function hive-mcp-channel-connected-p "hive-mcp-channel")
(declare-function hive-mcp-channel-send "hive-mcp-channel")

;;;; Internal State:

(defvar hive-mcp-swarm-events--session-id nil
  "Current session ID for event correlation.")

;;;; Channel Availability:

(defun hive-mcp-swarm-events-channel-available-p ()
  "Check if the bidirectional channel is available and connected."
  (and (require 'hive-mcp-channel nil t)
       (fboundp 'hive-mcp-channel-connected-p)
       (hive-mcp-channel-connected-p)))

;;;; Core Event Emission:

(defun hive-mcp-swarm-events-emit (event-type data)
  "Emit EVENT-TYPE with DATA through the channel if connected.
EVENT-TYPE should be a string like \"task-completed\".
DATA is an alist of additional event properties."
  (when (hive-mcp-swarm-events-channel-available-p)
    (let ((event `(("type" . ,event-type)
                   ("timestamp" . ,(truncate (float-time)))
                   ("session-id" . ,hive-mcp-swarm-events--session-id)
                   ,@data)))
      (condition-case err
          (hive-mcp-channel-send event)
        (error
         (message "[swarm-events] Channel emit error: %s"
                  (error-message-string err)))))))

;;;; Typed Event Emitters:

(defun hive-mcp-swarm-events-emit-slave-spawned (slave-id name presets &optional cwd)
  "Emit slave-spawned event for SLAVE-ID with NAME, PRESETS, and CWD.
CWD is the working directory of the slave (optional but recommended for
registry sync per ADR-001 Phase 2)."
  (hive-mcp-swarm-events-emit
   "slave-spawned"
   `(("slave-id" . ,slave-id)
     ("name" . ,name)
     ("presets" . ,(or presets []))
     ("cwd" . ,(or cwd "")))))

(defun hive-mcp-swarm-events-emit-slave-killed (slave-id)
  "Emit slave-killed event for SLAVE-ID."
  (hive-mcp-swarm-events-emit
   "slave-killed"
   `(("slave-id" . ,slave-id))))

(defun hive-mcp-swarm-events-emit-task-completed (task-id slave-id result)
  "Emit task-completed event for TASK-ID from SLAVE-ID with RESULT."
  (hive-mcp-swarm-events-emit
   "task-completed"
   `(("task-id" . ,task-id)
     ("slave-id" . ,slave-id)
     ("result" . ,result))))

(defun hive-mcp-swarm-events-emit-task-failed (task-id slave-id error-msg)
  "Emit task-failed event for TASK-ID from SLAVE-ID with ERROR-MSG."
  (hive-mcp-swarm-events-emit
   "task-failed"
   `(("task-id" . ,task-id)
     ("slave-id" . ,slave-id)
     ("error" . ,error-msg))))

(defun hive-mcp-swarm-events-emit-prompt-shown (slave-id prompt-text)
  "Emit prompt-shown event for SLAVE-ID with PROMPT-TEXT."
  (hive-mcp-swarm-events-emit
   "prompt-shown"
   `(("slave-id" . ,slave-id)
     ("prompt" . ,prompt-text))))

(defun hive-mcp-swarm-events-emit-state-changed (slave-id old-state new-state)
  "Emit state-changed event for SLAVE-ID from OLD-STATE to NEW-STATE."
  (hive-mcp-swarm-events-emit
   "state-changed"
   `(("slave-id" . ,slave-id)
     ("old-state" . ,(symbol-name old-state))
     ("new-state" . ,(symbol-name new-state)))))

(defun hive-mcp-swarm-events-emit-auto-completed (slave-id duration-secs status)
  "Emit auto-completed event for SLAVE-ID with DURATION-SECS and STATUS.
This is emitted when the completion watcher detects a task has finished
without explicit hivemind_shout from the ling.  Used for automatic
progress tracking of ling work.

STATUS should be a string like \"completed\", \"unknown\", etc.
DURATION-SECS is the task duration in seconds (float)."
  (hive-mcp-swarm-events-emit
   "auto-completed"
   `(("slave-id" . ,slave-id)
     ("duration-secs" . ,(or duration-secs 0))
     ("status" . ,(or status "completed"))
     ("detection-method" . "terminal-ready-transition"))))

(defun hive-mcp-swarm-events-emit-auto-started (slave-id task-preview)
  "Emit auto-started event for SLAVE-ID with TASK-PREVIEW.
This is emitted when a task is dispatched to a ling, marking the
beginning of work.  Used for automatic progress tracking.

TASK-PREVIEW is a truncated version of the task prompt (first 100 chars)."
  (hive-mcp-swarm-events-emit
   "auto-started"
   `(("slave-id" . ,slave-id)
     ("task-preview" . ,(or task-preview ""))
     ("detection-method" . "terminal-send"))))

(defun hive-mcp-swarm-events-emit-auto-error (slave-id duration-secs error-type error-preview)
  "Emit auto-error event for SLAVE-ID with error details.
This is emitted when the completion watcher detects an error pattern
in the terminal output.  Used for automatic error tracking.

DURATION-SECS is the task duration before error (float).
ERROR-TYPE is a string like \"cli-error\", \"tool-error\", \"timeout\".
ERROR-PREVIEW is a truncated version of the error text (first 200 chars)."
  (hive-mcp-swarm-events-emit
   "auto-error"
   `(("slave-id" . ,slave-id)
     ("duration-secs" . ,(or duration-secs 0))
     ("error-type" . ,(or error-type "unknown"))
     ("error-preview" . ,(or error-preview ""))
     ("detection-method" . "terminal-pattern-match"))))

(defun hive-mcp-swarm-events-emit-idle-timeout (slave-id idle-duration-secs)
  "Emit idle-timeout event for SLAVE-ID (Layer 2 convergence).
This is emitted when a working ling goes silent without shouting.
Used to detect hung or crashed lings that forgot to report completion.

IDLE-DURATION-SECS is how long the slave has been idle (float).

This event alerts the coordinator that a ling may need attention:
- Manual check of the terminal buffer
- Potential restart of the task
- Investigation of why the ling went silent"
  (hive-mcp-swarm-events-emit
   "idle-timeout"
   `(("slave-id" . ,slave-id)
     ("idle-duration-secs" . ,(or idle-duration-secs 0))
     ("detection-method" . "terminal-introspection")
     ("layer" . 2)
     ("convergence-pattern" . "4-layer-convergence"))))

(defun hive-mcp-swarm-events-emit-prompt-stall (slave-id idle-duration-secs prompt-text)
  "Emit prompt-stall event for SLAVE-ID (Layer 2 convergence).
This is emitted when a ling is idle AND has a pending prompt awaiting response.
This is more specific than idle-timeout - the ling is blocked on the coordinator.

IDLE-DURATION-SECS is how long the slave has been idle (float).
PROMPT-TEXT is the pending prompt text (truncated if long).

This event is URGENT - the coordinator needs to respond via swarm_respond_prompt
to unblock the ling. Desktop notification is also sent via prompts module."
  (hive-mcp-swarm-events-emit
   "prompt-stall"
   `(("slave-id" . ,slave-id)
     ("idle-duration-secs" . ,(or idle-duration-secs 0))
     ("prompt-preview" . ,(if (> (length prompt-text) 100)
                              (concat (substring prompt-text 0 97) "...")
                            prompt-text))
     ("detection-method" . "idle-with-pending-prompt")
     ("layer" . 2)
     ("convergence-pattern" . "4-layer-convergence")
     ("urgency" . "high"))))

(defun hive-mcp-swarm-events-emit-ling-ready-for-wrap (slave-id reason)
  "Emit ling-ready-for-wrap event for SLAVE-ID with REASON.
This is emitted when a ling completes its work and is ready for auto-wrap.
Used for auto-wrap hook that crystallizes session learnings before termination.

REASON is a string describing why wrap is triggered:
- \"task-completed\": Ling finished task and went idle
- \"idle-detected\": Ling has been idle for timeout period
- \"manual\": User requested wrap

This event triggers the wrap workflow in the Clojure backend,
which will:
1. Run wrap-gather to collect session data
2. Run wrap-crystallize to store to memory
3. Emit wrap_notify for coordinator permeation"
  (hive-mcp-swarm-events-emit
   "ling-ready-for-wrap"
   `(("slave-id" . ,slave-id)
     ("reason" . ,(or reason "task-completed"))
     ("session-id" . ,hive-mcp-swarm-events--session-id))))

;;;; Session Management:

(defun hive-mcp-swarm-events-set-session-id (session-id)
  "Set the SESSION-ID for event correlation."
  (setq hive-mcp-swarm-events--session-id session-id))

(defun hive-mcp-swarm-events-get-session-id ()
  "Get the current session ID."
  hive-mcp-swarm-events--session-id)

;;;; Lifecycle:

(defun hive-mcp-swarm-events-init (session-id)
  "Initialize events module with SESSION-ID."
  (setq hive-mcp-swarm-events--session-id session-id))

(defun hive-mcp-swarm-events-shutdown ()
  "Shutdown events module."
  (setq hive-mcp-swarm-events--session-id nil))

(provide 'hive-mcp-swarm-events)
;;; hive-mcp-swarm-events.el ends here
