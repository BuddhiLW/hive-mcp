;;; hive-mcp-swarm-notify.el --- Robust notification system for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Unified, robust notification system for hive-mcp-swarm.
;; Ensures 100% notification delivery through multi-layer fallbacks.
;;
;; Notification layers (in order):
;; 1. notify-send (Linux CLI tool) - most reliable
;; 2. notifications-notify (D-Bus) - Emacs integration
;; 3. message + ding (always works)
;;
;; All notification attempts are logged for debugging.
;;
;; Design principles (CLARITY):
;; - Yield safe failure: Multiple fallback layers ensure notification

;;; Code:

(require 'cl-lib)

;;;; Customization:

(defgroup hive-mcp-swarm-notify nil
  "Notification settings for swarm."
  :group 'hive-mcp-swarm
  :prefix "hive-mcp-swarm-notify-")

(defcustom hive-mcp-swarm-notify-enabled t
  "If non-nil, send desktop notifications for swarm events.
Set to nil to disable all desktop notifications."
  :type 'boolean
  :group 'hive-mcp-swarm-notify)

(defcustom hive-mcp-swarm-notify-timeout 10000
  "Notification timeout in milliseconds (10 seconds default)."
  :type 'integer
  :group 'hive-mcp-swarm-notify)

(defcustom hive-mcp-swarm-notify-sound t
  "If non-nil, play alert sound with urgent notifications."
  :type 'boolean
  :group 'hive-mcp-swarm-notify)

(defcustom hive-mcp-swarm-notify-log-buffer "*Swarm Notifications*"
  "Buffer name for notification log."
  :type 'string
  :group 'hive-mcp-swarm-notify)

(defcustom hive-mcp-swarm-notify-urgency-mapping
  '((critical . "critical")
    (high . "critical")
    (normal . "normal")
    (low . "low"))
  "Mapping from urgency symbols to notify-send urgency strings."
  :type '(alist :key-type symbol :value-type string)
  :group 'hive-mcp-swarm-notify)

;;;; Internal State:

(defvar hive-mcp-swarm-notify--stats
  '(:sent 0 :failed 0 :fallback 0)
  "Notification statistics for debugging.")

(defvar hive-mcp-swarm-notify--last-notification nil
  "Last notification sent (for dedup).")

(defvar hive-mcp-swarm-notify--dedup-window 2.0
  "Seconds to deduplicate identical notifications.")

;;;; Logging:

(defun hive-mcp-swarm-notify--log (level message &rest args)
  "Log MESSAGE with LEVEL to notification buffer.
LEVEL is a symbol like `info', `warn', `error'."
  (let ((formatted (apply #'format message args))
        (timestamp (format-time-string "%H:%M:%S")))
    (with-current-buffer (get-buffer-create hive-mcp-swarm-notify-log-buffer)
      (goto-char (point-max))
      (insert (format "[%s] %s: %s\n" timestamp (upcase (symbol-name level)) formatted)))))

;;;; Deduplication:

(defun hive-mcp-swarm-notify--should-send-p (title message)
  "Return t if notification should be sent (not a duplicate)."
  (let* ((key (format "%s|%s" title message))
         (now (float-time))
         (last hive-mcp-swarm-notify--last-notification)
         (last-key (car last))
         (last-time (cdr last)))
    (if (and last-key last-time
             (string= key last-key)
             (< (- now last-time) hive-mcp-swarm-notify--dedup-window))
        (progn
          (hive-mcp-swarm-notify--log 'info "Deduplicated: %s" title)
          nil)
      (setq hive-mcp-swarm-notify--last-notification (cons key now))
      t)))

;;;; Notification Backends:

(defun hive-mcp-swarm-notify--try-notify-send (title message urgency)
  "Try to send notification via notify-send.
Returns t on success, nil on failure."
  (condition-case err
      (let* ((urgency-str (or (alist-get urgency hive-mcp-swarm-notify-urgency-mapping)
                              "normal"))
             (timeout-str (number-to-string hive-mcp-swarm-notify-timeout)))
        (if (executable-find "notify-send")
            (progn
              (start-process "hive-notify" nil "notify-send"
                             "-u" urgency-str
                             "-t" timeout-str
                             "-a" "Hive-MCP Swarm"
                             "-i" "dialog-information"
                             title
                             message)
              (hive-mcp-swarm-notify--log 'info "notify-send: %s - %s" title message)
              t)
          (hive-mcp-swarm-notify--log 'warn "notify-send not found in PATH")
          nil))
    (error
     (hive-mcp-swarm-notify--log 'error "notify-send failed: %s"
                                  (error-message-string err))
     nil)))

(defun hive-mcp-swarm-notify--try-dbus (title message urgency)
  "Try to send notification via D-Bus (notifications-notify).
Returns t on success, nil on failure."
  (condition-case err
      (if (fboundp 'notifications-notify)
          (progn
            (notifications-notify
             :title title
             :body message
             :urgency urgency
             :timeout hive-mcp-swarm-notify-timeout
             :app-name "Hive-MCP Swarm")
            (hive-mcp-swarm-notify--log 'info "D-Bus notify: %s - %s" title message)
            t)
        (hive-mcp-swarm-notify--log 'warn "notifications-notify not available")
        nil)
    (error
     (hive-mcp-swarm-notify--log 'error "D-Bus notify failed: %s"
                                  (error-message-string err))
     nil)))

(defun hive-mcp-swarm-notify--fallback (title message urgency)
  "Fallback notification via message and optional ding.
Always succeeds."
  (message "HIVE ALERT [%s]: %s - %s"
           (upcase (symbol-name urgency)) title message)
  (when (and hive-mcp-swarm-notify-sound
             (memq urgency '(critical high)))
    (ding t))
  (hive-mcp-swarm-notify--log 'info "Fallback notify: %s - %s" title message)
  t)

;;;; Core Notification Function:

(defun hive-mcp-swarm-notify (title message &optional urgency)
  "Send notification with TITLE and MESSAGE.
URGENCY is a symbol: `critical', `high', `normal', or `low' (default: normal).

This function guarantees notification delivery through multiple fallback layers:
1. notify-send (CLI tool)
2. D-Bus notifications
3. Emacs message + ding

Returns t if notification was sent, nil if disabled or deduplicated."
  (unless hive-mcp-swarm-notify-enabled
    (hive-mcp-swarm-notify--log 'info "Notifications disabled, skipping: %s" title)
    (cl-return-from hive-mcp-swarm-notify nil))

  (let ((urgency (or urgency 'normal)))
    ;; Deduplication check
    (unless (hive-mcp-swarm-notify--should-send-p title message)
      (cl-return-from hive-mcp-swarm-notify nil))

    ;; Try notification backends in order
    (let ((sent nil))
      ;; Layer 1: notify-send
      (setq sent (hive-mcp-swarm-notify--try-notify-send title message urgency))

      ;; Layer 2: D-Bus (if layer 1 failed)
      (unless sent
        (setq sent (hive-mcp-swarm-notify--try-dbus title message urgency))
        (when sent
          (cl-incf (plist-get hive-mcp-swarm-notify--stats :fallback))))

      ;; Layer 3: Fallback (always works)
      (unless sent
        (hive-mcp-swarm-notify--fallback title message urgency)
        (cl-incf (plist-get hive-mcp-swarm-notify--stats :fallback))
        (setq sent t))

      ;; Update stats
      (if sent
          (cl-incf (plist-get hive-mcp-swarm-notify--stats :sent))
        (cl-incf (plist-get hive-mcp-swarm-notify--stats :failed)))

      sent)))

;;;; Convenience Functions for Common Notification Types:

(defun hive-mcp-swarm-notify-prompt (slave-id prompt-text)
  "Notify about permission PROMPT-TEXT from SLAVE-ID."
  (hive-mcp-swarm-notify
   (format "Swarm Prompt: %s" slave-id)
   (truncate-string-to-width prompt-text 100)
   'critical))

(defun hive-mcp-swarm-notify-ask (agent-id question)
  "Notify about hivemind QUESTION from AGENT-ID."
  (hive-mcp-swarm-notify
   (format "NEEDS INPUT: %s" agent-id)
   (truncate-string-to-width question 100)
   'critical))

(defun hive-mcp-swarm-notify-idle (slave-id idle-secs)
  "Notify about SLAVE-ID being idle for IDLE-SECS seconds."
  (hive-mcp-swarm-notify
   (format "Ling Idle: %s" slave-id)
   (format "No activity for %.0f seconds" idle-secs)
   'high))

(defun hive-mcp-swarm-notify-error (slave-id error-type error-preview)
  "Notify about ERROR-TYPE from SLAVE-ID with ERROR-PREVIEW."
  (hive-mcp-swarm-notify
   (format "Ling Error: %s" slave-id)
   (format "[%s] %s" error-type (truncate-string-to-width (or error-preview "") 80))
   'critical))

(defun hive-mcp-swarm-notify-completed (slave-id &optional duration-secs)
  "Notify about SLAVE-ID completing task with optional DURATION-SECS."
  (hive-mcp-swarm-notify
   (format "Task Completed: %s" slave-id)
   (if duration-secs
       (format "Finished in %.1f seconds" duration-secs)
     "Task finished successfully")
   'normal))

(defun hive-mcp-swarm-notify-blocked (slave-id reason)
  "Notify about SLAVE-ID being blocked for REASON."
  (hive-mcp-swarm-notify
   (format "Ling Blocked: %s" slave-id)
   (truncate-string-to-width (or reason "Unknown reason") 100)
   'high))

(defun hive-mcp-swarm-notify-prompt-stall (slave-id idle-secs prompt-text)
  "Notify about SLAVE-ID stalled on unanswered prompt for IDLE-SECS.
PROMPT-TEXT is the pending prompt."
  (hive-mcp-swarm-notify
   (format "URGENT: %s Waiting!" slave-id)
   (format "Stalled %.0fs on: %s"
           idle-secs
           (truncate-string-to-width (or prompt-text "") 60))
   'critical))

;;;; Statistics and Debugging:

(defun hive-mcp-swarm-notify-stats ()
  "Return notification statistics."
  hive-mcp-swarm-notify--stats)

(defun hive-mcp-swarm-notify-show-log ()
  "Display the notification log buffer."
  (interactive)
  (display-buffer (get-buffer-create hive-mcp-swarm-notify-log-buffer)))

(defun hive-mcp-swarm-notify-test ()
  "Send a test notification to verify the system works."
  (interactive)
  (hive-mcp-swarm-notify
   "Hive-MCP Test"
   "If you see this, notifications are working!"
   'normal)
  (message "Test notification sent. Check desktop and *Swarm Notifications* buffer."))

;;;; Lifecycle:

(defun hive-mcp-swarm-notify-init ()
  "Initialize notification module."
  (setq hive-mcp-swarm-notify--stats '(:sent 0 :failed 0 :fallback 0))
  (setq hive-mcp-swarm-notify--last-notification nil)
  (hive-mcp-swarm-notify--log 'info "Notification module initialized")
  ;; Check notify-send availability at startup
  (unless (executable-find "notify-send")
    (hive-mcp-swarm-notify--log 'warn
                                 "notify-send not found - install libnotify-bin for best results")))

(defun hive-mcp-swarm-notify-shutdown ()
  "Shutdown notification module."
  (hive-mcp-swarm-notify--log 'info
                               "Notification module shutdown. Stats: sent=%d failed=%d fallback=%d"
                               (plist-get hive-mcp-swarm-notify--stats :sent)
                               (plist-get hive-mcp-swarm-notify--stats :failed)
                               (plist-get hive-mcp-swarm-notify--stats :fallback)))

(provide 'hive-mcp-swarm-notify)
;;; hive-mcp-swarm-notify.el ends here
