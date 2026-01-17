;;; hive-mcp-log.el --- Structured logging infrastructure for hive-mcp  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "1.0"))
;; Keywords: tools, ai, logging, telemetry
;; SPDX-License-Identifier: MIT

;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Structured logging infrastructure for hive-mcp.
;;
;; This module implements CLARITY-T (Telemetry first) patterns:
;;
;;   - Structured log entries with levels: debug, info, warn, error
;;   - Component-based filtering (swarm, memory, channel, api, etc.)
;;   - Timing instrumentation for MCP calls
;;   - Ring buffer for log history with optional persistence
;;
;; Design principles:
;;
;;   1. Zero-cost when disabled - checks happen before formatting
;;   2. Structured data - logs contain metadata, not just strings
;;   3. Component isolation - filter noise from irrelevant subsystems
;;   4. Performance awareness - timing macros for instrumentation
;;
;; Usage:
;;
;;   ;; Basic logging
;;   (hive-mcp-log-info "swarm" "Spawned slave %s" slave-id)
;;   (hive-mcp-log-error "channel" "WebSocket disconnected: %s" reason)
;;
;;   ;; Timing instrumentation
;;   (hive-mcp-log-with-timing "api" "Memory query"
;;     (hive-mcp-api-memory-query "note"))
;;
;;   ;; Component filtering
;;   (setq hive-mcp-log-components '(swarm channel))  ; Only these
;;   (setq hive-mcp-log-components nil)               ; All components
;;
;;   ;; Log history access
;;   (hive-mcp-log-recent 10)  ; Last 10 entries
;;   (hive-mcp-log-filter-by-component "swarm")

;;; Code:

(require 'cl-lib)
(require 'ring)

;;; Customization

(defgroup hive-mcp-log nil
  "Structured logging settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-log-")

(defcustom hive-mcp-log-level 'info
  "Minimum log level to emit.
Levels in order: debug < info < warn < error.
Set to \\='debug for verbose output, \\='error for minimal."
  :type '(choice (const :tag "Debug (all messages)" debug)
                 (const :tag "Info (normal operation)" info)
                 (const :tag "Warn (potential issues)" warn)
                 (const :tag "Error (failures only)" error))
  :group 'hive-mcp-log)

(defcustom hive-mcp-log-components nil
  "List of components to log, or nil to log all.
Valid components: swarm, memory, channel, api, context, workflow,
kanban, hivemind, graceful, etc.
When nil, all components are logged."
  :type '(choice (const :tag "All components" nil)
                 (repeat :tag "Specific components" symbol))
  :group 'hive-mcp-log)

(defcustom hive-mcp-log-to-messages t
  "If non-nil, emit log entries to *Messages* buffer."
  :type 'boolean
  :group 'hive-mcp-log)

(defcustom hive-mcp-log-to-buffer nil
  "If non-nil, emit log entries to dedicated *hive-mcp-log* buffer."
  :type 'boolean
  :group 'hive-mcp-log)

(defcustom hive-mcp-log-history-size 500
  "Number of log entries to keep in ring buffer history."
  :type 'integer
  :group 'hive-mcp-log)

(defcustom hive-mcp-log-include-timestamp t
  "If non-nil, include timestamps in log output."
  :type 'boolean
  :group 'hive-mcp-log)

(defcustom hive-mcp-log-timing-threshold-ms 100
  "Log timing warnings when operations exceed this threshold (milliseconds).
Set to 0 to always log timing, or a large value to only log slow operations."
  :type 'integer
  :group 'hive-mcp-log)

;;; Internal State

(defvar hive-mcp-log--history nil
  "Ring buffer storing recent log entries.")

(defvar hive-mcp-log--level-priority
  '((debug . 0)
    (info . 1)
    (warn . 2)
    (error . 3))
  "Priority mapping for log levels.")

(defvar hive-mcp-log--level-labels
  '((debug . "DEBUG")
    (info  . "INFO ")
    (warn  . "WARN ")
    (error . "ERROR"))
  "Display labels for log levels.")

(defvar hive-mcp-log--buffer-name "*hive-mcp-log*"
  "Name of the dedicated log buffer.")

;;; Internal Functions

(defun hive-mcp-log--ensure-history ()
  "Ensure the log history ring buffer exists."
  (unless hive-mcp-log--history
    (setq hive-mcp-log--history (make-ring hive-mcp-log-history-size))))

(defun hive-mcp-log--level-enabled-p (level)
  "Return non-nil if LEVEL is enabled based on current log level."
  (let ((level-pri (alist-get level hive-mcp-log--level-priority 1))
        (min-pri (alist-get hive-mcp-log-level hive-mcp-log--level-priority 1)))
    (>= level-pri min-pri)))

(defun hive-mcp-log--component-enabled-p (component)
  "Return non-nil if COMPONENT should be logged."
  (or (null hive-mcp-log-components)
      (memq (if (stringp component) (intern component) component)
            hive-mcp-log-components)))

(defun hive-mcp-log--format-timestamp ()
  "Return formatted timestamp string."
  (format-time-string "%H:%M:%S.%3N"))

(defun hive-mcp-log--format-entry (level component message)
  "Format a log entry with LEVEL, COMPONENT, and MESSAGE."
  (let ((level-label (alist-get level hive-mcp-log--level-labels "?????"))
        (comp-str (if (stringp component) component (symbol-name component))))
    (if hive-mcp-log-include-timestamp
        (format "[%s] [%s] [%s] %s"
                (hive-mcp-log--format-timestamp)
                level-label
                comp-str
                message)
      (format "[%s] [%s] %s" level-label comp-str message))))

(defun hive-mcp-log--emit (level component message)
  "Emit a log entry with LEVEL, COMPONENT, and MESSAGE."
  (let ((entry (list :timestamp (current-time)
                     :level level
                     :component (if (stringp component)
                                    (intern component)
                                  component)
                     :message message)))
    ;; Store in history
    (hive-mcp-log--ensure-history)
    (ring-insert hive-mcp-log--history entry)

    ;; Format for output
    (let ((formatted (hive-mcp-log--format-entry level component message)))
      ;; Output to *Messages*
      (when hive-mcp-log-to-messages
        (message "%s" formatted))

      ;; Output to dedicated buffer
      (when hive-mcp-log-to-buffer
        (hive-mcp-log--write-to-buffer formatted level)))))

(defun hive-mcp-log--write-to-buffer (formatted-msg level)
  "Write FORMATTED-MSG with LEVEL to the dedicated log buffer."
  (let ((buf (get-buffer-create hive-mcp-log--buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (propertize (concat formatted-msg "\n")
                            'face (hive-mcp-log--level-face level)))))))

(defun hive-mcp-log--level-face (level)
  "Return face for LEVEL."
  (pcase level
    ('debug 'font-lock-comment-face)
    ('info  'default)
    ('warn  'warning)
    ('error 'error)
    (_      'default)))

;;; Public Logging Functions

(defun hive-mcp-log (level component format-string &rest args)
  "Log a message at LEVEL for COMPONENT using FORMAT-STRING and ARGS.

LEVEL is one of: debug, info, warn, error.
COMPONENT is a symbol or string identifying the subsystem.
FORMAT-STRING and ARGS work like `format'."
  (when (and (hive-mcp-log--level-enabled-p level)
             (hive-mcp-log--component-enabled-p component))
    (let ((message (apply #'format format-string args)))
      (hive-mcp-log--emit level component message))))

(defun hive-mcp-log-debug (component format-string &rest args)
  "Log a debug message for COMPONENT using FORMAT-STRING and ARGS."
  (apply #'hive-mcp-log 'debug component format-string args))

(defun hive-mcp-log-info (component format-string &rest args)
  "Log an info message for COMPONENT using FORMAT-STRING and ARGS."
  (apply #'hive-mcp-log 'info component format-string args))

(defun hive-mcp-log-warn (component format-string &rest args)
  "Log a warning message for COMPONENT using FORMAT-STRING and ARGS."
  (apply #'hive-mcp-log 'warn component format-string args))

(defun hive-mcp-log-error (component format-string &rest args)
  "Log an error message for COMPONENT using FORMAT-STRING and ARGS."
  (apply #'hive-mcp-log 'error component format-string args))

;;; Timing Instrumentation

(defmacro hive-mcp-log-with-timing (component operation &rest body)
  "Execute BODY, logging timing for OPERATION under COMPONENT.

Logs at debug level if timing is below threshold, warn if above.
Returns the result of BODY.

Example:
  (hive-mcp-log-with-timing \"api\" \"Memory query\"
    (expensive-operation))"
  (declare (indent 2) (debug (sexp sexp body)))
  (let ((start-sym (gensym "start-"))
        (result-sym (gensym "result-"))
        (elapsed-sym (gensym "elapsed-")))
    `(let ((,start-sym (current-time)))
       (prog1
           (let ((,result-sym (progn ,@body)))
             ,result-sym)
         (let* ((,elapsed-sym (* 1000.0 (float-time
                                          (time-subtract (current-time) ,start-sym)))))
           (if (> ,elapsed-sym hive-mcp-log-timing-threshold-ms)
               (hive-mcp-log-warn ,component "%s took %.1fms (threshold: %dms)"
                                  ,operation ,elapsed-sym hive-mcp-log-timing-threshold-ms)
             (hive-mcp-log-debug ,component "%s completed in %.1fms"
                                 ,operation ,elapsed-sym)))))))

(defmacro hive-mcp-log-timed-call (component fn &rest args)
  "Call FN with ARGS, logging timing under COMPONENT.

Example:
  (hive-mcp-log-timed-call \"memory\" #'hive-mcp-memory-query \"note\")"
  (declare (indent 1) (debug (sexp sexp &rest form)))
  (let ((fn-name (if (and (listp fn) (eq (car fn) 'function))
                     (format "%s" (cadr fn))
                   (format "%s" fn))))
    `(hive-mcp-log-with-timing ,component ,fn-name
       (funcall ,fn ,@args))))

;;; Log History Access

(defun hive-mcp-log-recent (&optional count)
  "Return the COUNT most recent log entries (default: 10).
Returns a list of plists with :timestamp, :level, :component, :message."
  (hive-mcp-log--ensure-history)
  (let ((n (or count 10))
        (len (ring-length hive-mcp-log--history))
        (result nil))
    (dotimes (i (min n len))
      (push (ring-ref hive-mcp-log--history i) result))
    (nreverse result)))

(defun hive-mcp-log-filter-by-component (component)
  "Return all log entries for COMPONENT from history."
  (hive-mcp-log--ensure-history)
  (let ((comp-sym (if (stringp component) (intern component) component))
        (result nil)
        (len (ring-length hive-mcp-log--history)))
    (dotimes (i len)
      (let ((entry (ring-ref hive-mcp-log--history i)))
        (when (eq (plist-get entry :component) comp-sym)
          (push entry result))))
    (nreverse result)))

(defun hive-mcp-log-filter-by-level (level)
  "Return all log entries at or above LEVEL from history."
  (hive-mcp-log--ensure-history)
  (let ((min-pri (alist-get level hive-mcp-log--level-priority 1))
        (result nil)
        (len (ring-length hive-mcp-log--history)))
    (dotimes (i len)
      (let* ((entry (ring-ref hive-mcp-log--history i))
             (entry-level (plist-get entry :level))
             (entry-pri (alist-get entry-level hive-mcp-log--level-priority 1)))
        (when (>= entry-pri min-pri)
          (push entry result))))
    (nreverse result)))

(defun hive-mcp-log-clear-history ()
  "Clear the log history ring buffer."
  (interactive)
  (setq hive-mcp-log--history nil)
  (hive-mcp-log--ensure-history)
  (message "Log history cleared"))

;;; Buffer Management

(defun hive-mcp-log-show-buffer ()
  "Show the dedicated log buffer."
  (interactive)
  (let ((buf (get-buffer-create hive-mcp-log--buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'hive-mcp-log-mode)
        (hive-mcp-log-mode)))
    (display-buffer buf)))

(define-derived-mode hive-mcp-log-mode special-mode "Hive-Log"
  "Major mode for viewing hive-mcp logs."
  :group 'hive-mcp-log
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t))

;;; MCP-Specific Instrumentation

(defvar hive-mcp-log--mcp-call-stats (make-hash-table :test 'equal)
  "Hash table tracking MCP call statistics.
Keys are tool names, values are plists with :count, :total-ms, :max-ms.")

(defun hive-mcp-log-mcp-call-start (tool-name)
  "Record the start of an MCP call to TOOL-NAME.
Returns a timing token to pass to `hive-mcp-log-mcp-call-end'."
  (hive-mcp-log-debug 'mcp "Starting MCP call: %s" tool-name)
  (list :tool tool-name :start (current-time)))

(defun hive-mcp-log-mcp-call-end (timing-token &optional success)
  "Record the end of an MCP call using TIMING-TOKEN.
SUCCESS indicates whether the call succeeded."
  (let* ((tool-name (plist-get timing-token :tool))
         (start-time (plist-get timing-token :start))
         (elapsed-ms (* 1000.0 (float-time (time-subtract (current-time) start-time))))
         (stats (gethash tool-name hive-mcp-log--mcp-call-stats)))
    ;; Update statistics
    (if stats
        (progn
          (plist-put stats :count (1+ (plist-get stats :count)))
          (plist-put stats :total-ms (+ (plist-get stats :total-ms) elapsed-ms))
          (when (> elapsed-ms (plist-get stats :max-ms))
            (plist-put stats :max-ms elapsed-ms)))
      (puthash tool-name
               (list :count 1 :total-ms elapsed-ms :max-ms elapsed-ms)
               hive-mcp-log--mcp-call-stats))
    ;; Log the call
    (let ((level (if (> elapsed-ms hive-mcp-log-timing-threshold-ms) 'warn 'debug)))
      (hive-mcp-log level 'mcp "MCP call %s: %.1fms%s"
                    tool-name elapsed-ms
                    (if success "" " (FAILED)")))))

(defun hive-mcp-log-mcp-stats ()
  "Return MCP call statistics as an alist.
Each entry is (TOOL-NAME . (:count N :total-ms M :max-ms X :avg-ms A))."
  (let ((result nil))
    (maphash
     (lambda (tool stats)
       (let* ((count (plist-get stats :count))
              (total (plist-get stats :total-ms))
              (avg (if (> count 0) (/ total count) 0)))
         (push (cons tool
                     (list :count count
                           :total-ms total
                           :max-ms (plist-get stats :max-ms)
                           :avg-ms avg))
               result)))
     hive-mcp-log--mcp-call-stats)
    (sort result (lambda (a b) (> (plist-get (cdr a) :total-ms)
                                  (plist-get (cdr b) :total-ms))))))

(defun hive-mcp-log-mcp-stats-reset ()
  "Reset MCP call statistics."
  (interactive)
  (clrhash hive-mcp-log--mcp-call-stats)
  (message "MCP statistics reset"))

(defun hive-mcp-log-mcp-stats-report ()
  "Display a report of MCP call statistics."
  (interactive)
  (let ((stats (hive-mcp-log-mcp-stats)))
    (if (null stats)
        (message "No MCP call statistics recorded")
      (with-output-to-temp-buffer "*hive-mcp-stats*"
        (princ "MCP Call Statistics\n")
        (princ "====================\n\n")
        (princ (format "%-30s %8s %10s %10s %10s\n"
                       "Tool" "Count" "Total(ms)" "Max(ms)" "Avg(ms)"))
        (princ (make-string 70 ?-))
        (princ "\n")
        (dolist (entry stats)
          (let ((tool (car entry))
                (data (cdr entry)))
            (princ (format "%-30s %8d %10.1f %10.1f %10.1f\n"
                           tool
                           (plist-get data :count)
                           (plist-get data :total-ms)
                           (plist-get data :max-ms)
                           (plist-get data :avg-ms)))))))))

;;; Convenience Macros for MCP Instrumentation

(defmacro hive-mcp-log-instrument-mcp (tool-name &rest body)
  "Execute BODY as an MCP call to TOOL-NAME with timing instrumentation.

Example:
  (hive-mcp-log-instrument-mcp \"memory_query\"
    (hive-mcp--send-request ...))"
  (declare (indent 1) (debug (sexp body)))
  (let ((token-sym (gensym "token-"))
        (result-sym (gensym "result-"))
        (success-sym (gensym "success-")))
    `(let ((,token-sym (hive-mcp-log-mcp-call-start ,tool-name))
           (,success-sym nil)
           (,result-sym nil))
       (unwind-protect
           (progn
             (setq ,result-sym (progn ,@body))
             (setq ,success-sym t)
             ,result-sym)
         (hive-mcp-log-mcp-call-end ,token-sym ,success-sym)))))

(provide 'hive-mcp-log)
;;; hive-mcp-log.el ends here
