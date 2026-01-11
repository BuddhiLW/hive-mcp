;;; hive-mcp-channel-ws.el --- WebSocket channel for hive-mcp  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; WebSocket-based bidirectional channel using Aleph on the server side.
;; Replaces the unreliable bencode TCP/Unix socket implementation.
;;
;; Uses websocket.el (https://github.com/ahyatt/emacs-websocket)
;;
;; CLARITY principles:
;; - Composition over modification: Uses battle-tested websocket.el
;; - Yield safe failure: Auto-reconnect with exponential backoff
;;
;; Usage:
;;   (hive-mcp-channel-ws-connect)
;;   (hive-mcp-channel-ws-on :hivemind-progress #'my-handler)
;;   (hive-mcp-channel-ws-send '(:type "ping"))

;;; Code:

(require 'websocket)
(require 'json)

;;;; Customization

(defgroup hive-mcp-channel-ws nil
  "WebSocket channel settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-channel-ws-")

(defcustom hive-mcp-channel-ws-url "ws://localhost:9999"
  "WebSocket server URL."
  :type 'string
  :group 'hive-mcp-channel-ws)

(defcustom hive-mcp-channel-ws-reconnect-interval 5.0
  "Base seconds between reconnection attempts."
  :type 'number
  :group 'hive-mcp-channel-ws)

(defcustom hive-mcp-channel-ws-max-reconnect-interval 120.0
  "Maximum seconds between reconnection attempts."
  :type 'number
  :group 'hive-mcp-channel-ws)

(defcustom hive-mcp-channel-ws-max-reconnects 10
  "Maximum reconnection attempts (0 = unlimited)."
  :type 'integer
  :group 'hive-mcp-channel-ws)

(defcustom hive-mcp-channel-ws-auto-connect t
  "Whether to auto-connect at Emacs startup."
  :type 'boolean
  :group 'hive-mcp-channel-ws)

(defcustom hive-mcp-channel-ws-startup-delay 3.0
  "Seconds to wait after Emacs startup before connecting."
  :type 'number
  :group 'hive-mcp-channel-ws)

(defcustom hive-mcp-channel-ws-keepalive-interval 25.0
  "Seconds between keepalive pings (0 to disable)."
  :type 'number
  :group 'hive-mcp-channel-ws)

;;;; Internal State

(defvar hive-mcp-channel-ws--connection nil
  "WebSocket connection object.")

(defvar hive-mcp-channel-ws--handlers (make-hash-table :test 'equal)
  "Event handlers indexed by event type string.")

(defvar hive-mcp-channel-ws--reconnect-timer nil
  "Timer for reconnection attempts.")

(defvar hive-mcp-channel-ws--reconnect-count 0
  "Current reconnection attempt count.")

(defvar hive-mcp-channel-ws--degraded-mode nil
  "Non-nil when in graceful degradation mode.")

(defvar hive-mcp-channel-ws--message-queue nil
  "Queue of messages pending send.")

(defvar hive-mcp-channel-ws--keepalive-timer nil
  "Timer for keepalive pings.")

;;;; Keepalive

(defun hive-mcp-channel-ws--start-keepalive ()
  "Start the keepalive timer."
  (hive-mcp-channel-ws--stop-keepalive)
  (when (> hive-mcp-channel-ws-keepalive-interval 0)
    (setq hive-mcp-channel-ws--keepalive-timer
          (run-with-timer hive-mcp-channel-ws-keepalive-interval
                          hive-mcp-channel-ws-keepalive-interval
                          #'hive-mcp-channel-ws--send-keepalive))))

(defun hive-mcp-channel-ws--stop-keepalive ()
  "Stop the keepalive timer."
  (when hive-mcp-channel-ws--keepalive-timer
    (cancel-timer hive-mcp-channel-ws--keepalive-timer)
    (setq hive-mcp-channel-ws--keepalive-timer nil)))

(defun hive-mcp-channel-ws--send-keepalive ()
  "Send a keepalive ping if connected."
  (when (hive-mcp-channel-ws-connected-p)
    (condition-case nil
        (websocket-send-text hive-mcp-channel-ws--connection "ping")
      (error nil))))

;;;; Event Dispatch

(defun hive-mcp-channel-ws--dispatch (msg)
  "Dispatch parsed JSON MSG to registered handlers."
  (let* ((type (cdr (assoc 'type msg)))
         (handlers (gethash type hive-mcp-channel-ws--handlers)))
    (dolist (handler handlers)
      (condition-case err
          (funcall handler msg)
        (error (message "hive-mcp-channel-ws: Handler error for %s: %s"
                        type (error-message-string err)))))))

;;;; WebSocket Callbacks

(defun hive-mcp-channel-ws--on-message (_ws frame)
  "Handle incoming WebSocket FRAME."
  (let ((text (websocket-frame-text frame)))
    (cond
     ;; Handle pong (response to our ping keepalive)
     ((string= text "pong") nil)
     ;; Handle ping from server (respond with pong)
     ((string= text "ping")
      (when (hive-mcp-channel-ws-connected-p)
        (websocket-send-text hive-mcp-channel-ws--connection "pong")))
     ;; Parse JSON messages
     (t (condition-case err
            (let ((msg (json-read-from-string text)))
              (hive-mcp-channel-ws--dispatch msg))
          (error (message "hive-mcp-channel-ws: Parse error: %s"
                          (error-message-string err))))))))

(defun hive-mcp-channel-ws--on-open (_ws)
  "Handle WebSocket connection opened."
  (condition-case err
      (progn
        (message "hive-mcp-channel-ws: Connected to %s" hive-mcp-channel-ws-url)
        (setq hive-mcp-channel-ws--reconnect-count 0
              hive-mcp-channel-ws--degraded-mode nil)
        ;; Start keepalive to prevent connection drops
        (hive-mcp-channel-ws--start-keepalive)
        ;; Flush queued messages
        (while hive-mcp-channel-ws--message-queue
          (hive-mcp-channel-ws-send (pop hive-mcp-channel-ws--message-queue))))
    (error (message "hive-mcp-channel-ws: on-open ERROR: %s" (error-message-string err)))))

(defun hive-mcp-channel-ws--on-close (_ws)
  "Handle WebSocket connection closed."
  (hive-mcp-channel-ws--stop-keepalive)
  (setq hive-mcp-channel-ws--connection nil)
  (cond
   ;; Already in degraded mode - stay silent
   (hive-mcp-channel-ws--degraded-mode nil)
   ;; Exhausted reconnect attempts
   ((and (> hive-mcp-channel-ws-max-reconnects 0)
         (>= hive-mcp-channel-ws--reconnect-count
             hive-mcp-channel-ws-max-reconnects))
    (setq hive-mcp-channel-ws--degraded-mode t)
    (message "hive-mcp-channel-ws: Graceful degradation (server unavailable)"))
   ;; Schedule reconnect
   (t
    (message "hive-mcp-channel-ws: Connection closed")
    (hive-mcp-channel-ws--schedule-reconnect))))

(defun hive-mcp-channel-ws--on-error (_ws _type err)
  "Handle WebSocket error ERR."
  (message "hive-mcp-channel-ws: Error: %s" err))

;;;; Reconnection

(defun hive-mcp-channel-ws--calculate-backoff ()
  "Calculate reconnect interval with exponential backoff."
  (let ((interval (* hive-mcp-channel-ws-reconnect-interval
                     (expt 2 (max 0 (1- hive-mcp-channel-ws--reconnect-count))))))
    (min interval hive-mcp-channel-ws-max-reconnect-interval)))

(defun hive-mcp-channel-ws--schedule-reconnect ()
  "Schedule a reconnection attempt."
  (hive-mcp-channel-ws--cancel-reconnect)
  (cl-incf hive-mcp-channel-ws--reconnect-count)
  (let ((interval (hive-mcp-channel-ws--calculate-backoff)))
    (message "hive-mcp-channel-ws: Reconnecting in %.1fs (attempt %d/%s)"
             interval
             hive-mcp-channel-ws--reconnect-count
             (if (= 0 hive-mcp-channel-ws-max-reconnects)
                 "unlimited"
               (number-to-string hive-mcp-channel-ws-max-reconnects)))
    (setq hive-mcp-channel-ws--reconnect-timer
          (run-with-timer interval nil #'hive-mcp-channel-ws-connect))))

(defun hive-mcp-channel-ws--cancel-reconnect ()
  "Cancel pending reconnection."
  (when hive-mcp-channel-ws--reconnect-timer
    (cancel-timer hive-mcp-channel-ws--reconnect-timer)
    (setq hive-mcp-channel-ws--reconnect-timer nil)))

;;;; Public API

;;;###autoload
(defun hive-mcp-channel-ws-connect ()
  "Connect to the WebSocket channel server."
  (interactive)
  (hive-mcp-channel-ws--cancel-reconnect)
  (when (and hive-mcp-channel-ws--connection
             (websocket-openp hive-mcp-channel-ws--connection))
    (websocket-close hive-mcp-channel-ws--connection))
  (condition-case err
      (setq hive-mcp-channel-ws--connection
            (websocket-open hive-mcp-channel-ws-url
                            :on-message #'hive-mcp-channel-ws--on-message
                            :on-open #'hive-mcp-channel-ws--on-open
                            :on-close #'hive-mcp-channel-ws--on-close
                            :on-error #'hive-mcp-channel-ws--on-error))
    (error
     (message "hive-mcp-channel-ws: Connect failed: %s"
              (error-message-string err))
     (hive-mcp-channel-ws--schedule-reconnect))))

;;;###autoload
(defun hive-mcp-channel-ws-disconnect ()
  "Disconnect from the WebSocket channel server."
  (interactive)
  (hive-mcp-channel-ws--cancel-reconnect)
  (when hive-mcp-channel-ws--connection
    (websocket-close hive-mcp-channel-ws--connection)
    (setq hive-mcp-channel-ws--connection nil))
  (message "hive-mcp-channel-ws: Disconnected"))

;;;###autoload
(defun hive-mcp-channel-ws-connected-p ()
  "Return t if WebSocket is connected."
  (and hive-mcp-channel-ws--connection
       (websocket-openp hive-mcp-channel-ws--connection)))

;;;###autoload
(defun hive-mcp-channel-ws-send (msg)
  "Send MSG through the WebSocket channel.
MSG should be a plist or alist that will be JSON-encoded."
  (if (hive-mcp-channel-ws-connected-p)
      (let ((json-str (json-encode msg)))
        (websocket-send-text hive-mcp-channel-ws--connection json-str)
        t)
    (push msg hive-mcp-channel-ws--message-queue)
    nil))

;;;###autoload
(defun hive-mcp-channel-ws-on (event-type handler)
  "Register HANDLER for EVENT-TYPE.
EVENT-TYPE should be a string like \"hivemind-progress\".
HANDLER receives the parsed JSON message as an alist."
  (let ((type-str (if (keywordp event-type)
                      (substring (symbol-name event-type) 1)
                    (if (symbolp event-type)
                        (symbol-name event-type)
                      event-type))))
    (let ((handlers (gethash type-str hive-mcp-channel-ws--handlers)))
      (puthash type-str (cons handler handlers)
               hive-mcp-channel-ws--handlers))))

;;;###autoload
(defun hive-mcp-channel-ws-off (event-type &optional handler)
  "Unregister HANDLER from EVENT-TYPE.
If HANDLER is nil, remove all handlers for EVENT-TYPE."
  (let ((type-str (if (keywordp event-type)
                      (substring (symbol-name event-type) 1)
                    event-type)))
    (if handler
        (let ((handlers (gethash type-str hive-mcp-channel-ws--handlers)))
          (puthash type-str (delete handler handlers)
                   hive-mcp-channel-ws--handlers))
      (remhash type-str hive-mcp-channel-ws--handlers))))

;;;###autoload
(defun hive-mcp-channel-ws-status ()
  "Return status of the WebSocket channel."
  (interactive)
  (let ((status `((connected . ,(hive-mcp-channel-ws-connected-p))
                  (degraded . ,hive-mcp-channel-ws--degraded-mode)
                  (reconnect-count . ,hive-mcp-channel-ws--reconnect-count)
                  (queued . ,(length hive-mcp-channel-ws--message-queue))
                  (url . ,hive-mcp-channel-ws-url))))
    (when (called-interactively-p 'any)
      (message "hive-mcp-channel-ws: %s"
               (if (hive-mcp-channel-ws-connected-p)
                   "Connected"
                 (if hive-mcp-channel-ws--degraded-mode
                     "Degraded"
                   "Disconnected"))))
    status))

;;;; Startup

(defun hive-mcp-channel-ws--startup-connect ()
  "Attempt connection at startup."
  (when hive-mcp-channel-ws-auto-connect
    (setq hive-mcp-channel-ws--degraded-mode nil
          hive-mcp-channel-ws--reconnect-count 0)
    (hive-mcp-channel-ws-connect)))

;;;###autoload
(defun hive-mcp-channel-ws-setup ()
  "Set up WebSocket channel auto-connection at startup."
  (when hive-mcp-channel-ws-auto-connect
    (run-with-timer hive-mcp-channel-ws-startup-delay nil
                    #'hive-mcp-channel-ws--startup-connect)))

;; Auto-setup when package loads
(if after-init-time
    (hive-mcp-channel-ws-setup)
  (add-hook 'emacs-startup-hook #'hive-mcp-channel-ws-setup))

(provide 'hive-mcp-channel-ws)
;;; hive-mcp-channel-ws.el ends here
