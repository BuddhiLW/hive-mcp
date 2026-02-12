;;; hive-mcp-swarm-prompts.el --- Prompt handling for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Prompt handling for hive-mcp-swarm.
;; Manages auto-approve watcher, human mode prompts, and notifications.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles prompt detection and responses
;; - Open/Closed: New prompt modes via dispatch, not modification
;; - Interface Segregation: Separate APIs for auto/human modes
;;
;; Prompt modes:
;; - bypass: CLI handles permissions (--permission-mode bypassPermissions)
;; - auto: Timer-based auto-approve (legacy)
;; - human: Forward prompts to master for decision

;;; Code:

(require 'cl-lib)

;; Soft dependencies
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function eat-term-send-string "eat")

;; Dependency on events module for push notifications
(declare-function hive-mcp-swarm-events-emit-prompt-shown "hive-mcp-swarm-events")

;;;; Customization:

(defgroup hive-mcp-swarm-prompts nil
  "Prompt handling for swarm."
  :group 'hive-mcp-swarm
  :prefix "hive-mcp-swarm-prompts-")

(defcustom hive-mcp-swarm-prompts-auto-approve t
  "If non-nil, automatically approve tool permission prompts in auto mode."
  :type 'boolean
  :group 'hive-mcp-swarm-prompts)

(defcustom hive-mcp-swarm-prompts-patterns
  '("Allow\\|Deny" "Yes\\|No" "(y/n)" "[Y/n]" "[y/N]"
    "Do you want to" "Would you like to" "Proceed\\?"
    "Allow .* tool"                ;; Claude Code tool prompts
    "permission"                   ;; Generic permission requests
    "approve\\|reject"             ;; Approve/reject variants
    "continue\\?"                  ;; Continue prompts
    "Accept"                       ;; Accept prompts
    "Allow once\\|Allow always"    ;; Claude Code specific
    "Press Enter\\|Press any key") ;; Interactive prompts
  "Patterns that indicate a permission/confirmation prompt."
  :type '(repeat string)
  :group 'hive-mcp-swarm-prompts)

(defcustom hive-mcp-swarm-prompts-mode 'human
  "How to handle permission prompts in slaves.
- `bypass': Use --permission-mode bypassPermissions (no prompts)
- `auto': Timer-based auto-approve (legacy behavior)
- `human': Forward prompts to master for human decision (default)

Default changed to `human' in v0.7.0 for hivemind prompt forwarding.
Prompts are forwarded via channel to hivemind for coordinator visibility."
  :type '(choice (const :tag "Bypass permissions (CLI flag)" bypass)
                 (const :tag "Auto-approve (timer)" auto)
                 (const :tag "Human decision (hooks, default)" human))
  :group 'hive-mcp-swarm-prompts)

(defcustom hive-mcp-swarm-prompts-notify t
  "If non-nil, show notification when prompts are pending in human mode."
  :type 'boolean
  :group 'hive-mcp-swarm-prompts)

(defcustom hive-mcp-swarm-prompts-desktop-notify t
  "If non-nil, send desktop notification via notify-send for prompts."
  :type 'boolean
  :group 'hive-mcp-swarm-prompts)

(defcustom hive-mcp-swarm-prompts-buffer-name "*Swarm Prompts*"
  "Buffer name for displaying pending prompts."
  :type 'string
  :group 'hive-mcp-swarm-prompts)

;;;; Internal State:

(defvar hive-mcp-swarm-prompts--timer nil
  "Timer for auto-approve watcher.")

(defvar hive-mcp-swarm-prompts--last-positions (make-hash-table :test 'equal)
  "Hash of slave-id -> last checked position to avoid re-approving.")

(defvar hive-mcp-swarm-prompts--pending nil
  "List of pending prompts awaiting human decision.
Each entry is plist: (:slave-id ID :prompt TEXT :buffer BUF :timestamp TIME)")

;;;; Prompt Detection:

(defun hive-mcp-swarm-prompts--check-for-prompt (buffer)
  "Check BUFFER for permission prompts and return position if found."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        ;; Look back at most 500 chars for prompt
        (let ((search-start (max (point-min) (- (point-max) 500))))
          (goto-char search-start)
          (cl-loop for pattern in hive-mcp-swarm-prompts-patterns
                   when (re-search-forward pattern nil t)
                   return (point)))))))

(defun hive-mcp-swarm-prompts--extract-prompt (buffer)
  "Extract prompt text and position from BUFFER.
Returns plist (:text TEXT :pos POS) or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (let ((search-start (max (point-min) (- (point-max) 500))))
          (when (re-search-backward
                 (regexp-opt hive-mcp-swarm-prompts-patterns)
                 search-start t)
            (let ((line-start (line-beginning-position))
                  (line-end (line-end-position)))
              (list :text (buffer-substring-no-properties line-start line-end)
                    :pos (match-beginning 0)))))))))

;;;; Auto-Approve Mode:

;; Helper predicates for prompt processing (CLARITY-L: self-documenting code)

(defun hive-mcp-swarm-prompts--slave-active-p (slave buffer)
  "Return t if SLAVE with BUFFER is ready for prompts.
Accepts working, idle, or spawning status - DataScript updates are async
so status may lag behind actual terminal state."
  (and (buffer-live-p buffer)
       (memq (plist-get slave :status) '(working idle spawning))))

(defun hive-mcp-swarm-prompts--prompt-is-new-p (prompt-pos last-pos)
  "Return t if PROMPT-POS indicates a new prompt after LAST-POS."
  (and prompt-pos (> prompt-pos last-pos)))

(defun hive-mcp-swarm-prompts--already-queued-p (slave-id)
  "Return t if SLAVE-ID already has a pending prompt in queue."
  (cl-find slave-id hive-mcp-swarm-prompts--pending
           :key (lambda (p) (plist-get p :slave-id))
           :test #'equal))

(defun hive-mcp-swarm-prompts--send-approval (buffer term-type)
  "Send \\='y\\=' approval to BUFFER using TERM-TYPE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (pcase term-type
        ('vterm
         (vterm-send-string "y")
         (run-at-time 0.05 nil
                      (lambda ()
                        (condition-case err
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (vterm-send-return)))
                          (error
                           (message "[swarm-prompts] Timer error in vterm approval: %s"
                                    (error-message-string err)))))))
        ('eat
         (when (and (boundp 'eat-terminal) eat-terminal)
           (eat-term-send-string eat-terminal "y")
           (run-at-time 0.05 nil
                        (lambda ()
                          (condition-case err
                              (when (and (buffer-live-p buffer)
                                         (boundp 'eat-terminal)
                                         eat-terminal)
                                (eat-term-send-string eat-terminal "\r"))
                            (error
                             (message "[swarm-prompts] Timer error in eat approval: %s"
                                      (error-message-string err))))))))))))

(defun hive-mcp-swarm-prompts--process-auto-approve (slave-id slave get-terminal-fn)
  "Process auto-approve for SLAVE with SLAVE-ID.
GET-TERMINAL-FN extracts terminal type from slave plist.
Returns t if a prompt was auto-approved, nil otherwise."
  (let* ((buffer (plist-get slave :buffer))
         (term-type (funcall get-terminal-fn slave))
         (last-pos (gethash slave-id hive-mcp-swarm-prompts--last-positions 0)))
    (when (hive-mcp-swarm-prompts--slave-active-p slave buffer)
      (let ((prompt-pos (hive-mcp-swarm-prompts--check-for-prompt buffer)))
        (when (hive-mcp-swarm-prompts--prompt-is-new-p prompt-pos last-pos)
          (message "[swarm-prompts] Auto-approving prompt in %s" slave-id)
          (puthash slave-id prompt-pos hive-mcp-swarm-prompts--last-positions)
          (hive-mcp-swarm-prompts--send-approval buffer term-type)
          t)))))

(defun hive-mcp-swarm-prompts--auto-tick (slaves-hash get-terminal-fn)
  "Check all slave buffers for prompts in auto mode.
SLAVES-HASH is hash table of slave-id -> slave plist.
GET-TERMINAL-FN takes slave plist and returns terminal type symbol.
CLARITY: L - Bottom-up reading via extracted per-slave helper."
  (when hive-mcp-swarm-prompts-auto-approve
    (maphash
     (lambda (slave-id slave)
       (hive-mcp-swarm-prompts--process-auto-approve slave-id slave get-terminal-fn))
     slaves-hash)))

;;;; Human Mode:

(defun hive-mcp-swarm-prompts--send-desktop-notification (title body)
  "Send desktop notification with TITLE and BODY via notify-send.
Falls back to beep + message if notify-send unavailable."
  (if (and hive-mcp-swarm-prompts-desktop-notify
           (executable-find "notify-send"))
      (start-process "swarm-notify" nil "notify-send"
                     "--urgency=critical"
                     "--app-name=Swarm"
                     title body)
    ;; Fallback: beep + message
    (ding t)
    (message "🐝 SWARM: %s - %s" title body)))

(defun hive-mcp-swarm-prompts--display-prompt (slave-id prompt-text)
  "Display PROMPT-TEXT from SLAVE-ID in the prompts buffer."
  (let ((buf (get-buffer-create hive-mcp-swarm-prompts-buffer-name)))
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

(defun hive-mcp-swarm-prompts--queue-prompt (slave-id prompt-text buffer)
  "Add a prompt to the pending queue and notify user.
Emits prompt-shown event via channel for push-based updates."
  (push (list :slave-id slave-id
              :prompt prompt-text
              :buffer buffer
              :timestamp (current-time))
        hive-mcp-swarm-prompts--pending)
  (hive-mcp-swarm-prompts--display-prompt slave-id prompt-text)
  ;; Emacs message
  (when hive-mcp-swarm-prompts-notify
    (message "[swarm-prompts] Prompt from %s: %s"
             slave-id
             (truncate-string-to-width prompt-text 50)))
  ;; Desktop notification
  (hive-mcp-swarm-prompts--send-desktop-notification
   (format "🐝 Swarm Prompt: %s" slave-id)
   (truncate-string-to-width prompt-text 100))
  ;; Push event via channel
  (when (fboundp 'hive-mcp-swarm-events-emit-prompt-shown)
    (hive-mcp-swarm-events-emit-prompt-shown slave-id prompt-text)))

(defun hive-mcp-swarm-prompts--process-human-mode (slave-id slave)
  "Process human-mode prompt detection for SLAVE with SLAVE-ID.
Queues prompt for human decision if new and not already queued.
Returns t if a prompt was queued, nil otherwise."
  (let* ((buffer (plist-get slave :buffer))
         (last-pos (gethash slave-id hive-mcp-swarm-prompts--last-positions 0)))
    (when (hive-mcp-swarm-prompts--slave-active-p slave buffer)
      (let ((prompt-info (hive-mcp-swarm-prompts--extract-prompt buffer)))
        (when (and prompt-info
                   (hive-mcp-swarm-prompts--prompt-is-new-p
                    (plist-get prompt-info :pos) last-pos)
                   (not (hive-mcp-swarm-prompts--already-queued-p slave-id)))
          (puthash slave-id (plist-get prompt-info :pos)
                   hive-mcp-swarm-prompts--last-positions)
          (hive-mcp-swarm-prompts--queue-prompt
           slave-id
           (plist-get prompt-info :text)
           buffer)
          t)))))

(defun hive-mcp-swarm-prompts--human-tick (slaves-hash)
  "Check all slave buffers for prompts in human mode.
SLAVES-HASH is hash table of slave-id -> slave plist.
CLARITY: L - Bottom-up reading via extracted per-slave helper."
  (maphash
   (lambda (slave-id slave)
     (hive-mcp-swarm-prompts--process-human-mode slave-id slave))
   slaves-hash))

;;;; Response Sending:

(defun hive-mcp-swarm-prompts--send-response (buffer response)
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

;;;; Prompts Buffer UI:

(defun hive-mcp-swarm-prompts-update-buffer ()
  "Refresh the prompts buffer with current pending prompts."
  (when-let* ((buf (get-buffer hive-mcp-swarm-prompts-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "=== Pending Swarm Prompts (%d) ===\n"
                      (length hive-mcp-swarm-prompts--pending)))
      (if hive-mcp-swarm-prompts--pending
          (dolist (prompt (reverse hive-mcp-swarm-prompts--pending))
            (insert (format "\n[%s] %s\n  %s\n"
                            (format-time-string "%H:%M:%S"
                                                (plist-get prompt :timestamp))
                            (plist-get prompt :slave-id)
                            (plist-get prompt :prompt))))
        (insert "\nNo pending prompts.\n")))))

;;;; Public API - Interactive Commands:

(defun hive-mcp-swarm-prompts-respond ()
  "Respond to the next pending prompt interactively."
  (interactive)
  (if-let* ((prompt (car hive-mcp-swarm-prompts--pending)))
      (let* ((slave-id (plist-get prompt :slave-id))
             (text (plist-get prompt :prompt))
             (response (read-string (format "[%s] %s → " slave-id text))))
        (pop hive-mcp-swarm-prompts--pending)
        (hive-mcp-swarm-prompts--send-response (plist-get prompt :buffer) response)
        (hive-mcp-swarm-prompts-update-buffer)
        (message "[swarm-prompts] Sent response to %s" slave-id))
    (message "[swarm-prompts] No pending prompts")))

(defun hive-mcp-swarm-prompts-approve ()
  "Approve (send \\='y\\=') to the next pending prompt."
  (interactive)
  (if-let* ((prompt (pop hive-mcp-swarm-prompts--pending)))
      (progn
        (hive-mcp-swarm-prompts--send-response (plist-get prompt :buffer) "y")
        (hive-mcp-swarm-prompts-update-buffer)
        (message "[swarm-prompts] Approved: %s" (plist-get prompt :slave-id)))
    (message "[swarm-prompts] No pending prompts")))

(defun hive-mcp-swarm-prompts-deny ()
  "Deny (send \\='n\\=') to the next pending prompt."
  (interactive)
  (if-let* ((prompt (pop hive-mcp-swarm-prompts--pending)))
      (progn
        (hive-mcp-swarm-prompts--send-response (plist-get prompt :buffer) "n")
        (hive-mcp-swarm-prompts-update-buffer)
        (message "[swarm-prompts] Denied: %s" (plist-get prompt :slave-id)))
    (message "[swarm-prompts] No pending prompts")))

(defun hive-mcp-swarm-prompts-list ()
  "List all pending prompts in the prompts buffer."
  (interactive)
  (hive-mcp-swarm-prompts-update-buffer)
  (display-buffer (get-buffer-create hive-mcp-swarm-prompts-buffer-name)))

;;;; Public API - Programmatic:

(defun hive-mcp-swarm-prompts-get-pending ()
  "Get list of pending prompts."
  hive-mcp-swarm-prompts--pending)

(defun hive-mcp-swarm-prompts-find-pending (slave-id)
  "Find pending prompt for SLAVE-ID."
  (cl-find slave-id hive-mcp-swarm-prompts--pending
           :key (lambda (p) (plist-get p :slave-id))
           :test #'equal))

(defun hive-mcp-swarm-prompts-respond-to (slave-id response)
  "Send RESPONSE to pending prompt from SLAVE-ID.
Returns t on success, nil if no pending prompt found."
  (if-let* ((prompt (hive-mcp-swarm-prompts-find-pending slave-id)))
      (let ((buffer (plist-get prompt :buffer)))
        (setq hive-mcp-swarm-prompts--pending
              (cl-remove slave-id hive-mcp-swarm-prompts--pending
                         :key (lambda (p) (plist-get p :slave-id))
                         :test #'equal))
        (hive-mcp-swarm-prompts--send-response buffer response)
        (hive-mcp-swarm-prompts-update-buffer)
        t)
    nil))

(defun hive-mcp-swarm-prompts-clear-slave (slave-id)
  "Clear last position tracking for SLAVE-ID."
  (remhash slave-id hive-mcp-swarm-prompts--last-positions))

;;;; Timer Management:

(defun hive-mcp-swarm-prompts-start-watcher (slaves-hash get-terminal-fn)
  "Start the prompt watcher timer.
SLAVES-HASH is the hash table of slaves to monitor.
GET-TERMINAL-FN extracts terminal type from slave plist."
  (hive-mcp-swarm-prompts-stop-watcher)
  (setq hive-mcp-swarm-prompts--timer
        (run-with-timer
         1 2
         (lambda ()
           (condition-case err
               (pcase hive-mcp-swarm-prompts-mode
                 ('bypass nil)  ; CLI handles permissions
                 ('auto (hive-mcp-swarm-prompts--auto-tick slaves-hash get-terminal-fn))
                 ('human (hive-mcp-swarm-prompts--human-tick slaves-hash)))
             (error
              (message "[swarm-prompts] Watcher tick error: %s"
                       (error-message-string err)))))))
  (message "[swarm-prompts] Watcher started (mode: %s)" hive-mcp-swarm-prompts-mode))

(defun hive-mcp-swarm-prompts-stop-watcher ()
  "Stop the prompt watcher timer."
  (when hive-mcp-swarm-prompts--timer
    (cancel-timer hive-mcp-swarm-prompts--timer)
    (setq hive-mcp-swarm-prompts--timer nil)
    (message "[swarm-prompts] Watcher stopped")))

;;;; Lifecycle:

(defun hive-mcp-swarm-prompts-init ()
  "Initialize prompts module."
  (clrhash hive-mcp-swarm-prompts--last-positions)
  (setq hive-mcp-swarm-prompts--pending nil))

(defun hive-mcp-swarm-prompts-shutdown ()
  "Shutdown prompts module."
  (hive-mcp-swarm-prompts-stop-watcher)
  (clrhash hive-mcp-swarm-prompts--last-positions)
  (setq hive-mcp-swarm-prompts--pending nil))

(provide 'hive-mcp-swarm-prompts)
;;; hive-mcp-swarm-prompts.el ends here
