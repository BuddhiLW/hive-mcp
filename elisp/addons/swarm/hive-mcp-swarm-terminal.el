;;; hive-mcp-swarm-terminal.el --- Terminal backend abstraction for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Terminal backend abstraction layer for hive-mcp-swarm.
;; Provides unified interface for vterm, eat, and claude-code-ide backends.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles terminal I/O
;; - Open/Closed: New backends via dispatch, not modification
;; - Dependency Inversion: Callers depend on abstraction, not concrete backends
;;
;; All operations are NON-BLOCKING to prevent freezing single-threaded Emacs.

;;; Code:

(require 'cl-lib)

;; Soft dependencies - checked at runtime
(declare-function vterm "vterm")
(declare-function vterm-mode "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

(declare-function eat "eat")
(declare-function eat-mode "eat")
(declare-function eat-exec "eat")
(declare-function eat-term-send-string "eat")

(declare-function claude-code-ide--terminal-send-string "claude-code-ide")
(declare-function claude-code-ide--terminal-send-return "claude-code-ide")
(declare-function claude-code-ide--create-terminal-session "claude-code-ide")
(declare-function claude-code-ide-mcp-server-ensure-server "claude-code-ide-mcp-server")

(declare-function hive-mcp-ellama-dispatch "hive-mcp-ellama")

;; External state from swarm module
(defvar hive-mcp-swarm--slaves)

;;;; Customization:

(defgroup hive-mcp-swarm-terminal nil
  "Terminal backend settings for swarm."
  :group 'hive-mcp-swarm
  :prefix "hive-mcp-swarm-terminal-")

(defcustom hive-mcp-swarm-terminal-backend 'claude-code-ide
  "Terminal backend for slave sessions.
- `claude-code-ide': MCP WebSocket integration (recommended)
- `vterm': Native terminal (requires native compilation)
- `eat': Pure Emacs Lisp (experimental)"
  :type '(choice (const :tag "claude-code-ide (recommended)" claude-code-ide)
                 (const :tag "vterm" vterm)
                 (const :tag "eat (experimental)" eat))
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-send-delay 0.1
  "Delay in seconds between send-string and send-return.
Ensures terminal processes text before return is sent."
  :type 'float
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-ready-timeout 10
  "Seconds to wait for terminal to become ready."
  :type 'integer
  :group 'hive-mcp-swarm-terminal)

(defcustom hive-mcp-swarm-terminal-prompt-marker "❯"
  "Marker indicating Claude CLI is ready for input."
  :type 'string
  :group 'hive-mcp-swarm-terminal)

;;;; Backend Detection:

(defun hive-mcp-swarm-terminal-backend-available-p (backend)
  "Check if BACKEND is available."
  (pcase backend
    ('claude-code-ide (require 'claude-code-ide nil t))
    ('vterm (require 'vterm nil t))
    ('eat (require 'eat nil t))
    (_ nil)))

(defun hive-mcp-swarm-terminal-detect-buffer-backend (buffer)
  "Detect terminal backend used in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((derived-mode-p 'vterm-mode) 'vterm)
       ((derived-mode-p 'eat-mode) 'eat)
       ;; claude-code-ide uses vterm internally but has special markers
       ((and (derived-mode-p 'vterm-mode)
             (bound-and-true-p claude-code-ide-session-id))
        'claude-code-ide)
       (t nil)))))

;;;; Helpers:

(defun hive-mcp-swarm-terminal--find-slave-by-buffer (buffer)
  "Find slave-id that owns BUFFER by iterating slaves hash."
  (when (and (buffer-live-p buffer)
             (boundp 'hive-mcp-swarm--slaves)
             (hash-table-p hive-mcp-swarm--slaves))
    (catch 'found
      (maphash (lambda (id slave)
                 (when (eq (plist-get slave :buffer) buffer)
                   (throw 'found id)))
               hive-mcp-swarm--slaves)
      nil)))

;;;; Non-Blocking Send Operations:

(defun hive-mcp-swarm-terminal--send-vterm (buffer text)
  "Send TEXT to vterm BUFFER, then return. Non-blocking."
  (with-current-buffer buffer
    (vterm-send-string text)
    ;; Schedule return via timer - never blocks
    (run-at-time hive-mcp-swarm-terminal-send-delay nil
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (vterm-send-return)))))))

(defun hive-mcp-swarm-terminal--send-eat (buffer text)
  "Send TEXT to eat BUFFER, then return. Non-blocking."
  (with-current-buffer buffer
    (when (and (boundp 'eat-terminal) eat-terminal)
      (eat-term-send-string eat-terminal text)
      ;; Schedule return via timer - never blocks
      (run-at-time hive-mcp-swarm-terminal-send-delay nil
                   (lambda ()
                     (when (and (buffer-live-p buffer)
                                (boundp 'eat-terminal)
                                eat-terminal)
                       (with-current-buffer buffer
                         (eat-term-send-string eat-terminal "\r"))))))))

(defun hive-mcp-swarm-terminal--send-ollama (buffer text)
  "Send TEXT to Ollama via ellama. Non-blocking.
BUFFER is the swarm worker buffer (used for logging).
Routes to hive-mcp-ellama for actual inference."
  (let* ((slave-id (hive-mcp-swarm-terminal--find-slave-by-buffer buffer))
         (slave (and slave-id (gethash slave-id hive-mcp-swarm--slaves)))
         (model (or (and slave (plist-get slave :model)) "devstral-small-2")))
    (if (fboundp 'hive-mcp-ellama-dispatch)
        (hive-mcp-ellama-dispatch
         text
         model
         (lambda (response)
           ;; Log response to worker buffer
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (goto-char (point-max))
               (insert (format "\n--- Ollama Response ---\n%s\n" response))))
           ;; Update slave status
           (when slave
             (plist-put slave :last-response response)
             (plist-put slave :status 'idle))))
      (error "hive-mcp-ellama not available"))))

(defun hive-mcp-swarm-terminal--send-claude-code-ide (buffer text)
  "Send TEXT via claude-code-ide abstraction.
Uses synchronous sit-for like claude-code-ide-send-prompt does.
Blocking is OK for individual slaves since they're isolated.
Delay scales with text length to allow vterm to process large pastes."
  (with-current-buffer buffer
    (if (fboundp 'claude-code-ide--terminal-send-string)
        (let* ((text-lines (length (split-string text "\n")))
               ;; Base delay + 0.01s per line, max 2s
               (paste-delay (min 2.0 (+ 0.1 (* 0.01 text-lines)))))
          (claude-code-ide--terminal-send-string text)
          ;; Adaptive delay based on text size
          (sit-for paste-delay)
          (when (fboundp 'claude-code-ide--terminal-send-return)
            (claude-code-ide--terminal-send-return)))
      (error "claude-code-ide terminal functions not available"))))

;;;; Public API:

(defun hive-mcp-swarm-terminal-send (buffer text &optional backend)
  "Send TEXT to terminal BUFFER using BACKEND. NON-BLOCKING.
BACKEND defaults to auto-detection or `hive-mcp-swarm-terminal-backend'.
Returns immediately - uses timers for delayed operations."
  (unless (buffer-live-p buffer)
    (error "Terminal buffer is dead"))
  (let ((backend (or backend
                     (hive-mcp-swarm-terminal-detect-buffer-backend buffer)
                     hive-mcp-swarm-terminal-backend)))
    (pcase backend
      ('claude-code-ide
       (hive-mcp-swarm-terminal--send-claude-code-ide buffer text))
      ('vterm
       (hive-mcp-swarm-terminal--send-vterm buffer text))
      ('eat
       (hive-mcp-swarm-terminal--send-eat buffer text))
      ('ollama
       (hive-mcp-swarm-terminal--send-ollama buffer text))
      (_
       (error "Unknown terminal backend: %s" backend)))))

(defun hive-mcp-swarm-terminal-ready-p (buffer)
  "Check if terminal BUFFER is ready for input (non-blocking check)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (let ((search-start (max (point-min) (- (point-max) 500))))
          (search-backward hive-mcp-swarm-terminal-prompt-marker
                           search-start t))))))

(defun hive-mcp-swarm-terminal-wait-ready (buffer callback &optional timeout-secs)
  "Wait for BUFFER to be ready, then call CALLBACK. NON-BLOCKING.
Uses timer-based polling instead of blocking sit-for loop.
TIMEOUT-SECS defaults to `hive-mcp-swarm-terminal-ready-timeout'.
CALLBACK receives (buffer success-p) arguments."
  (let* ((timeout (or timeout-secs hive-mcp-swarm-terminal-ready-timeout))
         (start-time (float-time))
         (check-interval 0.2)
         timer)
    (setq timer
          (run-with-timer
           0 check-interval
           (lambda ()
             (cond
              ;; Buffer died
              ((not (buffer-live-p buffer))
               (cancel-timer timer)
               (funcall callback buffer nil))
              ;; Ready!
              ((hive-mcp-swarm-terminal-ready-p buffer)
               (cancel-timer timer)
               (funcall callback buffer t))
              ;; Timeout
              ((> (- (float-time) start-time) timeout)
               (cancel-timer timer)
               (message "[swarm-terminal] Timeout waiting for ready: %s"
                        (buffer-name buffer))
               (funcall callback buffer nil))))))))

;;;; Buffer Creation:

(defun hive-mcp-swarm-terminal-create-buffer (name dir backend &optional slave-id)
  "Create terminal buffer NAME in DIR using BACKEND.
SLAVE-ID is optional identifier for claude-code-ide sessions.
Returns the buffer (async startup - may not be immediately ready)."
  (unless (hive-mcp-swarm-terminal-backend-available-p backend)
    (error "Terminal backend not available: %s" backend))
  (let ((default-directory dir))
    (pcase backend
      ('claude-code-ide
       (let* ((port (when (fboundp 'claude-code-ide-mcp-server-ensure-server)
                      (claude-code-ide-mcp-server-ensure-server)))
              (_ (unless port
                   (error "Failed to start MCP server")))
              (result (claude-code-ide--create-terminal-session
                       name dir port nil nil slave-id)))
         (car result)))  ; Return buffer
      ('vterm
       (let ((buf (generate-new-buffer name)))
         (with-current-buffer buf
           (vterm-mode))
         buf))
      ('eat
       (let ((buf (generate-new-buffer name)))
         (with-current-buffer buf
           (eat-mode)
           (eat-exec buf "swarm-shell" "/bin/bash" nil '("-l")))
         buf)))))

(defun hive-mcp-swarm-terminal-kill-buffer (buffer)
  "Kill terminal BUFFER without prompts.
Handles process cleanup to prevent confirmation dialogs."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Prevent save prompts
      (set-buffer-modified-p nil)
      ;; Disable process query-on-exit
      (when-let* ((proc (get-buffer-process buffer)))
        (set-process-query-on-exit-flag proc nil))
      ;; Handle vterm's internal process
      (when (and (boundp 'vterm--process)
                 (processp vterm--process)
                 (process-live-p vterm--process))
        (set-process-query-on-exit-flag vterm--process nil)))
    ;; Kill with all hooks disabled
    (let ((kill-buffer-query-functions nil)
          (kill-buffer-hook nil)
          (vterm-exit-functions nil))
      (kill-buffer buffer))))

(provide 'hive-mcp-swarm-terminal)
;;; hive-mcp-swarm-terminal.el ends here
