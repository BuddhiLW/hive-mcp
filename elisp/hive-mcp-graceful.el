;;; hive-mcp-graceful.el --- Safe failure patterns for hive-mcp  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "1.0"))
;; Keywords: tools, ai, error-handling, resilience
;; SPDX-License-Identifier: MIT

;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Safe failure and graceful degradation patterns for hive-mcp.
;;
;; This module implements CLARITY-Y (Yield safe failure) patterns:
;;
;;   - `hive-mcp-with-fallback': Execute primary expression with fallback
;;   - `hive-mcp-with-timeout': Execute body with timeout protection
;;   - `hive-mcp-safe-call': Call function catching all errors
;;   - `hive-mcp-retry': Retry with exponential backoff
;;
;; Design principles:
;;
;;   1. Never crash - always return something usable
;;   2. Log errors for debugging without disrupting flow
;;   3. Provide fallback values when operations fail
;;   4. Timeout protection for blocking operations
;;   5. Retry transient failures with exponential backoff
;;
;; Usage:
;;
;;   ;; Fallback pattern
;;   (hive-mcp-with-fallback
;;       (potentially-failing-operation)
;;     "safe default value")
;;
;;   ;; Timeout pattern
;;   (hive-mcp-with-timeout 5000
;;     (long-running-operation)
;;     :on-timeout (message "Operation timed out"))
;;
;;   ;; Safe call pattern
;;   (hive-mcp-safe-call #'risky-function arg1 arg2)  ; => result or nil
;;
;;   ;; Retry pattern
;;   (hive-mcp-retry #'flaky-network-call 3 100)  ; 3 retries, 100ms initial delay

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup hive-mcp-graceful nil
  "Graceful degradation settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-graceful-")

(defcustom hive-mcp-graceful-log-errors t
  "If non-nil, log errors to *Messages* buffer.
Set to nil to suppress error logging."
  :type 'boolean
  :group 'hive-mcp-graceful)

(defcustom hive-mcp-graceful-default-timeout 5000
  "Default timeout in milliseconds for `hive-mcp-with-timeout'."
  :type 'integer
  :group 'hive-mcp-graceful)

(defcustom hive-mcp-graceful-default-retries 3
  "Default number of retries for `hive-mcp-retry'."
  :type 'integer
  :group 'hive-mcp-graceful)

(defcustom hive-mcp-graceful-default-delay 100
  "Default initial delay in milliseconds for `hive-mcp-retry'."
  :type 'integer
  :group 'hive-mcp-graceful)

(defcustom hive-mcp-graceful-backoff-multiplier 2.0
  "Multiplier for exponential backoff in `hive-mcp-retry'."
  :type 'float
  :group 'hive-mcp-graceful)

(defcustom hive-mcp-graceful-max-delay 10000
  "Maximum delay in milliseconds between retries."
  :type 'integer
  :group 'hive-mcp-graceful)

;;; Internal Functions

(defun hive-mcp-graceful--log-error (context error-data)
  "Log error with CONTEXT and ERROR-DATA if logging is enabled."
  (when hive-mcp-graceful-log-errors
    (message "[hive-mcp-graceful] %s: %s"
             context
             (error-message-string error-data))))

(defun hive-mcp-graceful--calculate-delay (attempt base-delay)
  "Calculate delay for ATTEMPT with BASE-DELAY using exponential backoff."
  (let ((delay (* base-delay
                  (expt hive-mcp-graceful-backoff-multiplier
                        (1- attempt)))))
    (min delay hive-mcp-graceful-max-delay)))

;;; Macros

(defmacro hive-mcp-with-fallback (primary-expr fallback-expr)
  "Execute PRIMARY-EXPR, returning FALLBACK-EXPR on any error.

This macro provides a simple way to ensure an expression always returns
a value, even when the primary operation fails.

PRIMARY-EXPR is evaluated first.  If it succeeds, its value is returned.
If PRIMARY-EXPR signals an error, the error is logged (if logging is
enabled) and FALLBACK-EXPR is evaluated and returned instead.

FALLBACK-EXPR is only evaluated if PRIMARY-EXPR fails.

Examples:

  ;; Simple fallback
  (hive-mcp-with-fallback
      (json-read-from-string potentially-invalid-json)
    \\='())  ; Return empty list on parse error

  ;; Network operation with default
  (hive-mcp-with-fallback
      (url-retrieve-synchronously url)
    nil)  ; Return nil if network fails

  ;; Chained fallbacks
  (hive-mcp-with-fallback
      (hive-mcp-with-fallback
          (primary-source)
        (secondary-source))
    (final-fallback))"
  (declare (indent 1) (debug (form form)))
  (let ((err-sym (gensym "err-")))
    `(condition-case ,err-sym
         ,primary-expr
       (error
        (hive-mcp-graceful--log-error "fallback triggered" ,err-sym)
        ,fallback-expr))))

(defmacro hive-mcp-with-timeout (timeout-ms &rest body)
  "Execute BODY with a timeout of TIMEOUT-MS milliseconds.

If BODY completes within TIMEOUT-MS, return the result.
If BODY times out, return nil (or execute :on-timeout handler if provided).

TIMEOUT-MS is the timeout in milliseconds.
BODY can contain an optional :on-timeout keyword followed by a form
to execute when timeout occurs.

Note: This uses `with-timeout' which works by setting up a timer.
It may not interrupt certain blocking operations (like synchronous
subprocess calls).  For truly blocking operations, consider using
async patterns instead.

Examples:

  ;; Simple timeout
  (hive-mcp-with-timeout 5000
    (long-computation))

  ;; Timeout with handler
  (hive-mcp-with-timeout 3000
    (network-request url)
    :on-timeout (progn
                  (message \"Request timed out\")
                  \\='(:error \"timeout\")))

  ;; Variable timeout
  (let ((ms 1000))
    (hive-mcp-with-timeout ms
      (quick-operation)))"
  (declare (indent 1) (debug (form &rest form)))
  (let ((main-forms nil)
        (on-timeout nil)
        (timeout-val (gensym "timeout-")))
    ;; Parse body for :on-timeout
    (let ((rest body))
      (while rest
        (if (eq (car rest) :on-timeout)
            (progn
              (setq on-timeout (cadr rest))
              (setq rest (cddr rest)))
          (push (car rest) main-forms)
          (setq rest (cdr rest)))))
    (setq main-forms (nreverse main-forms))
    `(let ((,timeout-val (/ ,timeout-ms 1000.0)))
       (with-timeout (,timeout-val
                      ,(or on-timeout
                           '(progn
                              (when hive-mcp-graceful-log-errors
                                (message "[hive-mcp-graceful] timeout exceeded"))
                              nil)))
         ,@main-forms))))

;;; Functions

(defun hive-mcp-safe-call (fn &rest args)
  "Call FN with ARGS, catching any errors and returning nil.

This function provides a safe wrapper for calling potentially
failing functions.  If FN signals an error, it is caught and
logged (if logging is enabled), and nil is returned.

FN is the function to call.
ARGS are the arguments to pass to FN.

Returns the result of (apply FN ARGS), or nil on error.

Examples:

  ;; Safe file read
  (hive-mcp-safe-call #\\='insert-file-contents \"/maybe/missing/file\")

  ;; Safe JSON parse
  (hive-mcp-safe-call #\\='json-read-from-string user-input)

  ;; Chain with or
  (or (hive-mcp-safe-call #\\='primary-method data)
      (hive-mcp-safe-call #\\='fallback-method data)
      \"default\")"
  (condition-case err
      (apply fn args)
    (error
     (hive-mcp-graceful--log-error
      (format "safe-call %s" (if (symbolp fn) fn "lambda"))
      err)
     nil)))

(defun hive-mcp-retry (fn &optional max-retries delay-ms)
  "Call FN with retries on failure using exponential backoff.

FN is called with no arguments.  If it signals an error, wait
and retry up to MAX-RETRIES times.  The delay between retries
increases exponentially based on `hive-mcp-graceful-backoff-multiplier\\='.

MAX-RETRIES is the maximum number of retry attempts (default:
`hive-mcp-graceful-default-retries\\=').

DELAY-MS is the initial delay in milliseconds (default:
`hive-mcp-graceful-default-delay\\=').

Returns the result of FN if successful, or nil if all retries fail.

The actual delay follows this pattern (with default multiplier 2.0):
  Attempt 1: delay-ms
  Attempt 2: delay-ms * 2
  Attempt 3: delay-ms * 4
  ...up to `hive-mcp-graceful-max-delay\\='

Examples:

  ;; Simple retry
  (hive-mcp-retry #\\='flaky-operation)

  ;; Custom retries and delay
  (hive-mcp-retry #\\='network-call 5 200)  ; 5 retries, 200ms initial

  ;; Retry with lambda
  (hive-mcp-retry
   (lambda ()
     (url-retrieve-synchronously api-endpoint))
   3 500)"
  (let ((max-attempts (or max-retries hive-mcp-graceful-default-retries))
        (base-delay (or delay-ms hive-mcp-graceful-default-delay))
        (attempt 1)
        (result nil)
        (success nil))
    (while (and (not success) (<= attempt max-attempts))
      (condition-case err
          (progn
            (setq result (funcall fn))
            (setq success t))
        (error
         (hive-mcp-graceful--log-error
          (format "retry attempt %d/%d" attempt max-attempts)
          err)
         (when (< attempt max-attempts)
           (let ((delay (hive-mcp-graceful--calculate-delay attempt base-delay)))
             (sleep-for (/ delay 1000.0))))
         (cl-incf attempt))))
    (if success result nil)))

(cl-defun hive-mcp-retry-async (fn callback &key max-retries delay-ms on-error)
  "Call FN asynchronously with retries, invoking CALLBACK on success.

FN is called with no arguments.  If it succeeds, CALLBACK is called
with the result.  If it fails, retry up to MAX-RETRIES times with
exponential backoff.

CALLBACK is called with a single argument: the result of FN.

Keyword arguments:
  :max-retries  Maximum retry attempts
  :delay-ms     Initial delay in milliseconds
  :on-error     Function to call if all retries fail

This is non-blocking - control returns immediately while retries
happen in the background via timers.

Examples:

  ;; Async retry with callback
  (hive-mcp-retry-async
   #\\='fetch-data
   (lambda (data) (process-data data))
   :max-retries 3
   :on-error (lambda (err) (message \"Failed: %s\" err)))"
  (let ((max-attempts (or max-retries hive-mcp-graceful-default-retries))
        (base-delay (or delay-ms hive-mcp-graceful-default-delay)))
    (hive-mcp-retry-async--attempt fn callback on-error 1 max-attempts base-delay)))

(defun hive-mcp-retry-async--attempt (fn callback on-error attempt max-attempts base-delay)
  "Internal: Execute retry ATTEMPT for `hive-mcp-retry-async'.
FN, CALLBACK, ON-ERROR, ATTEMPT, MAX-ATTEMPTS, BASE-DELAY as described."
  (condition-case err
      (let ((result (funcall fn)))
        (when callback
          (funcall callback result)))
    (error
     (hive-mcp-graceful--log-error
      (format "async retry attempt %d/%d" attempt max-attempts)
      err)
     (if (< attempt max-attempts)
         (let ((delay (hive-mcp-graceful--calculate-delay attempt base-delay)))
           (run-at-time
            (/ delay 1000.0) nil
            #'hive-mcp-retry-async--attempt
            fn callback on-error (1+ attempt) max-attempts base-delay))
       ;; All retries exhausted
       (when on-error
         (funcall on-error err))))))

;;; Result Type

(cl-defstruct (hive-mcp-result
               (:constructor hive-mcp-result--create)
               (:copier nil))
  "Represents an operation result that may have succeeded or failed.

This is similar to Result<T, E> in Rust or Either in Haskell.
Use this when you need to distinguish between success/failure
while carrying error information.

Slots:
  ok      - Non-nil if the operation succeeded
  value   - The result value on success, nil on failure
  error   - The error object on failure, nil on success"
  (ok nil :type boolean :documentation "Whether operation succeeded.")
  (value nil :documentation "Success value.")
  (error nil :documentation "Error on failure."))

(defun hive-mcp-result-success (value)
  "Create a successful result containing VALUE."
  (hive-mcp-result--create :ok t :value value :error nil))

(defun hive-mcp-result-failure (error)
  "Create a failed result containing ERROR."
  (hive-mcp-result--create :ok nil :value nil :error error))

(defun hive-mcp-result-try (fn &rest args)
  "Call FN with ARGS, returning an `hive-mcp-result\\='.

Returns success result with the value on success,
or failure result with the error on failure.

Example:
  (let ((result (hive-mcp-result-try #\\='json-parse-string input)))
    (if (hive-mcp-result-ok result)
        (process (hive-mcp-result-value result))
      (handle-error (hive-mcp-result-error result))))"
  (condition-case err
      (hive-mcp-result-success (apply fn args))
    (error
     (hive-mcp-result-failure err))))

(defun hive-mcp-result-map (result fn)
  "Apply FN to RESULT\\='s value if ok, returning a new result.

If RESULT is an error, returns RESULT unchanged.
If FN throws, returns an error result."
  (if (hive-mcp-result-ok result)
      (hive-mcp-result-try fn (hive-mcp-result-value result))
    result))

(defun hive-mcp-result-unwrap-or (result default)
  "Return RESULT\\='s value if ok, otherwise DEFAULT."
  (if (hive-mcp-result-ok result)
      (hive-mcp-result-value result)
    default))

;;; Circuit Breaker (for repeated failures)

(cl-defstruct (hive-mcp-circuit-breaker
               (:constructor hive-mcp-circuit-breaker--create)
               (:copier nil))
  "Circuit breaker for protecting against repeated failures.

When failures exceed a threshold, the circuit opens and calls
fail fast without attempting the operation.

Slots:
  name           - Identifier for logging
  failure-count  - Current failure count
  threshold      - Failures before opening circuit
  state          - :closed, :open, or :half-open
  last-failure   - Timestamp of last failure
  reset-timeout  - Seconds before attempting reset"
  (name "default" :type string)
  (failure-count 0 :type integer)
  (threshold 5 :type integer)
  (state :closed :type symbol)
  (last-failure nil :type (or number null))
  (reset-timeout 60 :type number))

(defun hive-mcp-circuit-breaker-create (name &optional threshold reset-timeout)
  "Create a circuit breaker named NAME.
THRESHOLD is failures before opening (default 5).
RESET-TIMEOUT is seconds before trying again (default 60)."
  (hive-mcp-circuit-breaker--create
   :name name
   :threshold (or threshold 5)
   :reset-timeout (or reset-timeout 60)))

(defun hive-mcp-circuit-breaker-call (breaker fn &rest args)
  "Call FN with ARGS through circuit BREAKER.

If circuit is open and reset timeout hasn't elapsed, fails immediately.
If circuit is open and reset timeout has elapsed, tries once (half-open).
If call succeeds, resets the breaker.
If call fails, increments failure count and may open the circuit."
  (let ((state (hive-mcp-circuit-breaker-state breaker))
        (last-fail (hive-mcp-circuit-breaker-last-failure breaker))
        (reset-time (hive-mcp-circuit-breaker-reset-timeout breaker)))
    ;; Check if we should try to reset
    (when (and (eq state :open)
               last-fail
               (> (- (float-time) last-fail) reset-time))
      (setf (hive-mcp-circuit-breaker-state breaker) :half-open)
      (setq state :half-open))

    (cond
     ;; Circuit is open - fail fast
     ((eq state :open)
      (when hive-mcp-graceful-log-errors
        (message "[hive-mcp-graceful] circuit %s is open, failing fast"
                 (hive-mcp-circuit-breaker-name breaker)))
      nil)

     ;; Circuit is closed or half-open - try the call
     (t
      (condition-case err
          (let ((result (apply fn args)))
            ;; Success - reset breaker
            (setf (hive-mcp-circuit-breaker-failure-count breaker) 0)
            (setf (hive-mcp-circuit-breaker-state breaker) :closed)
            result)
        (error
         ;; Failure - record and maybe open
         (cl-incf (hive-mcp-circuit-breaker-failure-count breaker))
         (setf (hive-mcp-circuit-breaker-last-failure breaker) (float-time))
         (when (>= (hive-mcp-circuit-breaker-failure-count breaker)
                   (hive-mcp-circuit-breaker-threshold breaker))
           (setf (hive-mcp-circuit-breaker-state breaker) :open)
           (when hive-mcp-graceful-log-errors
             (message "[hive-mcp-graceful] circuit %s opened after %d failures"
                      (hive-mcp-circuit-breaker-name breaker)
                      (hive-mcp-circuit-breaker-failure-count breaker))))
         (hive-mcp-graceful--log-error
          (format "circuit %s" (hive-mcp-circuit-breaker-name breaker))
          err)
         nil))))))

(provide 'hive-mcp-graceful)
;;; hive-mcp-graceful.el ends here
