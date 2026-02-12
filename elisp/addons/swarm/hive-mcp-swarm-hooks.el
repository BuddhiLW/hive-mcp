;;; hive-mcp-swarm-hooks.el --- Event hook registry for swarm state sync -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Centralized hook registry for swarm event-driven state synchronization.
;; Enables hivemind events to trigger state updates in swarm module.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only manages hook registration and dispatch
;; - Open/Closed: New event handlers via register, not modification
;; - Dependency Inversion: Decouples hivemind from swarm state updates
;;
;; Event-to-state mapping (handled by registered sync handlers):
;; - hivemind-completed → slave_status: idle, increment tasks-completed
;; - hivemind-error → slave_status: idle, increment tasks-failed
;; - hivemind-blocked → slave_status: blocked
;; - hivemind-started → slave_status: working
;; - hivemind-progress → no state change (maintains working status)
;;
;; Usage:
;;   ;; Register a handler for an event type
;;   (hive-mcp-swarm-hooks-register :hivemind-completed #'my-completion-handler)
;;
;;   ;; Dispatch an event (called from hivemind)
;;   (hive-mcp-swarm-hooks-dispatch :hivemind-completed event-data)

;;; Code:

(require 'cl-lib)

;;;; Internal State:

(defvar hive-mcp-swarm-hooks--registry nil
  "Alist of event-type -> list of handler functions.
Each handler receives the event data as its argument.
Structure: ((event-type . (handler1 handler2 ...)) ...)")

;;;; Core API:

(defun hive-mcp-swarm-hooks-register (event-type handler)
  "Register HANDLER function for EVENT-TYPE events.
HANDLER should be a function that accepts one argument: event data.
Multiple handlers can be registered for the same event type.
Returns t on successful registration."
  (unless (functionp handler)
    (error "Handler must be a function"))
  (let ((existing (assq event-type hive-mcp-swarm-hooks--registry)))
    (if existing
        ;; Add handler to existing list (avoid duplicates)
        (unless (memq handler (cdr existing))
          (setcdr existing (cons handler (cdr existing))))
      ;; Create new entry
      (push (cons event-type (list handler)) hive-mcp-swarm-hooks--registry)))
  t)

(defun hive-mcp-swarm-hooks-unregister (event-type handler)
  "Unregister HANDLER from EVENT-TYPE events.
Returns t if handler was found and removed, nil otherwise."
  (let ((existing (assq event-type hive-mcp-swarm-hooks--registry)))
    (when existing
      (let ((handlers (cdr existing)))
        (when (memq handler handlers)
          (setcdr existing (delq handler handlers))
          ;; Clean up empty entries
          (when (null (cdr existing))
            (setq hive-mcp-swarm-hooks--registry
                  (assq-delete-all event-type hive-mcp-swarm-hooks--registry)))
          t)))))

(defun hive-mcp-swarm-hooks-dispatch (event-type data)
  "Dispatch EVENT-TYPE event with DATA to all registered handlers.
Returns the number of handlers that were invoked.
Errors in handlers are caught and logged to *Messages*."
  (let ((existing (assq event-type hive-mcp-swarm-hooks--registry))
        (count 0))
    (when existing
      (dolist (handler (cdr existing))
        (condition-case err
            (progn
              (funcall handler data)
              (cl-incf count))
          (error
           (message "[swarm-hooks] Handler error for %s: %s"
                    event-type (error-message-string err))))))
    count))

(defun hive-mcp-swarm-hooks-clear ()
  "Clear all registered hooks.
Use for testing or shutdown."
  (setq hive-mcp-swarm-hooks--registry nil))

(defun hive-mcp-swarm-hooks-list ()
  "Return the current hook registry for inspection.
Useful for debugging."
  hive-mcp-swarm-hooks--registry)

;;;; Lifecycle:

(defun hive-mcp-swarm-hooks-init ()
  "Initialize hooks module.
Called during swarm addon startup."
  ;; Registry is already initialized as nil
  nil)

(defun hive-mcp-swarm-hooks-shutdown ()
  "Shutdown hooks module.
Clears all registered hooks."
  (hive-mcp-swarm-hooks-clear))

(provide 'hive-mcp-swarm-hooks)
;;; hive-mcp-swarm-hooks.el ends here
