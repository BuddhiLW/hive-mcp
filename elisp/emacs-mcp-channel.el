;;; emacs-mcp-channel.el --- Bidirectional channel for MCP communication  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of emacs-mcp.

;;; Commentary:
;;
;; Bidirectional communication channel between Emacs and the MCP Clojure server.
;; Replaces polling-based communication with push-based events.
;;
;; Architecture:
;; - Uses Unix domain sockets (default) or TCP
;; - Bencode message format (compatible with nREPL)
;; - Async message dispatch via process-filter
;;
;; Usage:
;;   (emacs-mcp-channel-connect)
;;   (emacs-mcp-channel-send '(("type" . "ping") ("data" . "hello")))
;;   (emacs-mcp-channel-on :task-completed #'my-handler)
;;   (emacs-mcp-channel-disconnect)
;;

;;; Code:

(require 'cl-lib)

;; Soft dependency on nrepl-bencode (from CIDER)
(declare-function nrepl-bencode "nrepl-client")
(declare-function nrepl-bdecode "nrepl-client")
(declare-function nrepl-bdecode-string "nrepl-client")

;;;; Customization

(defgroup emacs-mcp-channel nil
  "Bidirectional channel settings for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-channel-")

(defcustom emacs-mcp-channel-type 'unix
  "Channel transport type."
  :type '(choice (const :tag "Unix socket" unix)
                 (const :tag "TCP" tcp))
  :group 'emacs-mcp-channel)

(defcustom emacs-mcp-channel-socket-path "/tmp/emacs-mcp-channel.sock"
  "Path to Unix domain socket."
  :type 'string
  :group 'emacs-mcp-channel)

(defcustom emacs-mcp-channel-host "localhost"
  "Host for TCP channel."
  :type 'string
  :group 'emacs-mcp-channel)

(defcustom emacs-mcp-channel-port 9999
  "Port for TCP channel."
  :type 'integer
  :group 'emacs-mcp-channel)

(defcustom emacs-mcp-channel-reconnect-interval 5.0
  "Seconds between reconnection attempts."
  :type 'number
  :group 'emacs-mcp-channel)

(defcustom emacs-mcp-channel-max-reconnects 10
  "Maximum reconnection attempts (0 = unlimited)."
  :type 'integer
  :group 'emacs-mcp-channel)

;;;; Internal State

(defvar emacs-mcp-channel--process nil
  "Network process for channel connection.")

(defvar emacs-mcp-channel--buffer nil
  "Buffer for accumulating incoming data.")

(defvar emacs-mcp-channel--handlers (make-hash-table :test 'eq)
  "Event handlers indexed by event type (keyword).")

(defvar emacs-mcp-channel--reconnect-timer nil
  "Timer for reconnection attempts.")

(defvar emacs-mcp-channel--reconnect-count 0
  "Current reconnection attempt count.")

(defvar emacs-mcp-channel--message-queue nil
  "Queue of messages pending send (when disconnected).")

;;;; Bencode Helpers

(defun emacs-mcp-channel--bencode-available-p ()
  "Check if CIDER's bencode is available."
  (and (require 'nrepl-client nil t)
       (fboundp 'nrepl-bencode)))

(defun emacs-mcp-channel--alist-to-nrepl-dict (alist)
  "Convert ALIST to nrepl-dict format for bencode encoding."
  (require 'nrepl-dict)
  (let ((result (list 'dict)))
    (dolist (pair alist)
      (setq result (append result (list (car pair) (cdr pair)))))
    result))

(defun emacs-mcp-channel--encode (msg)
  "Encode MSG to bencode string.
MSG should be an alist like \\='((\"type\" . \"event\"))."
  (if (emacs-mcp-channel--bencode-available-p)
      (nrepl-bencode (emacs-mcp-channel--alist-to-nrepl-dict msg))
    ;; Fallback: simple bencode implementation
    (emacs-mcp-channel--bencode-fallback msg)))

(defun emacs-mcp-channel--decode (str)
  "Decode bencode STR to plist/alist."
  (if (emacs-mcp-channel--bencode-available-p)
      (car (nrepl-bdecode-string str))
    ;; Fallback: simple bencode parser
    (emacs-mcp-channel--bdecode-fallback str)))

;; Fallback bencode implementation (if CIDER not available)
(defun emacs-mcp-channel--bencode-fallback (obj)
  "Encode OBJ to bencode string (fallback)."
  (cond
   ((stringp obj)
    (format "%d:%s" (length obj) obj))
   ((integerp obj)
    (format "i%de" obj))
   ((listp obj)
    (if (and (consp (car obj)) (not (listp (cdar obj))))
        ;; Dictionary
        (concat "d"
                (mapconcat (lambda (pair)
                             (concat (emacs-mcp-channel--bencode-fallback
                                      (if (symbolp (car pair))
                                          (symbol-name (car pair))
                                        (car pair)))
                                     (emacs-mcp-channel--bencode-fallback (cdr pair))))
                           obj "")
                "e")
      ;; List
      (concat "l"
              (mapconcat #'emacs-mcp-channel--bencode-fallback obj "")
              "e")))
   ((vectorp obj)
    (concat "l"
            (mapconcat #'emacs-mcp-channel--bencode-fallback (append obj nil) "")
            "e"))
   (t (error "Cannot bencode: %S" obj))))

(defun emacs-mcp-channel--bdecode-fallback (str)
  "Decode bencode STR (fallback)."
  (let ((pos 0))
    (cl-labels
        ((decode-one ()
           (let ((c (aref str pos)))
             (cond
              ;; Integer
              ((= c ?i)
               (cl-incf pos)
               (let ((end (cl-position ?e str :start pos)))
                 (prog1 (string-to-number (substring str pos end))
                   (setq pos (1+ end)))))
              ;; String
              ((and (>= c ?0) (<= c ?9))
               (let ((colon (cl-position ?: str :start pos)))
                 (let ((len (string-to-number (substring str pos colon))))
                   (setq pos (1+ colon))
                   (prog1 (substring str pos (+ pos len))
                     (cl-incf pos len)))))
              ;; List
              ((= c ?l)
               (cl-incf pos)
               (let (items)
                 (while (/= (aref str pos) ?e)
                   (push (decode-one) items))
                 (cl-incf pos)
                 (nreverse items)))
              ;; Dictionary
              ((= c ?d)
               (cl-incf pos)
               (let (pairs)
                 (while (/= (aref str pos) ?e)
                   (let ((key (decode-one))
                         (val (decode-one)))
                     (push (cons key val) pairs)))
                 (cl-incf pos)
                 (nreverse pairs)))
              (t (error "Invalid bencode at pos %d" pos))))))
      (decode-one))))

;;;; Process Filter & Sentinel

(defun emacs-mcp-channel--filter (proc str)
  "Process filter for channel PROC receiving STR."
  (with-current-buffer (or emacs-mcp-channel--buffer
                           (setq emacs-mcp-channel--buffer
                                 (get-buffer-create " *mcp-channel*")))
    (goto-char (point-max))
    (insert str)
    ;; Try to decode complete messages
    (goto-char (point-min))
    (while (and (not (eobp))
                (emacs-mcp-channel--try-decode-message)))))

(defun emacs-mcp-channel--try-decode-message ()
  "Try to decode a complete bencode message from buffer.
Returns t if a message was decoded, nil otherwise."
  (condition-case nil
      (let ((start (point))
            (msg (emacs-mcp-channel--decode (buffer-substring (point) (point-max)))))
        ;; If we get here, decoding succeeded
        ;; Calculate how much was consumed (approximate by re-encoding)
        (let ((encoded-len (length (emacs-mcp-channel--encode msg))))
          (delete-region start (+ start encoded-len)))
        ;; Dispatch the message
        (emacs-mcp-channel--dispatch msg)
        t)
    (error nil)))

(defun emacs-mcp-channel--sentinel (proc event)
  "Sentinel for channel PROC handling EVENT."
  (cond
   ((string-match-p "deleted\\|connection broken\\|exited\\|failed" event)
    (message "emacs-mcp-channel: Connection lost: %s" (string-trim event))
    (setq emacs-mcp-channel--process nil)
    (when (and emacs-mcp-channel-reconnect-interval
               (or (= 0 emacs-mcp-channel-max-reconnects)
                   (< emacs-mcp-channel--reconnect-count emacs-mcp-channel-max-reconnects)))
      (emacs-mcp-channel--schedule-reconnect)))
   ((string-match-p "open" event)
    (message "emacs-mcp-channel: Connected")
    (setq emacs-mcp-channel--reconnect-count 0)
    ;; Flush queued messages
    (while emacs-mcp-channel--message-queue
      (emacs-mcp-channel-send (pop emacs-mcp-channel--message-queue))))))

;;;; Event Dispatch

(defun emacs-mcp-channel--dispatch (msg)
  "Dispatch MSG to registered handlers."
  (let* ((type-str (or (cdr (assoc "type" msg))
                       (cdr (assoc 'type msg))))
         (type (when type-str
                 (intern (concat ":" type-str)))))
    (when type
      (let ((handlers (gethash type emacs-mcp-channel--handlers)))
        (dolist (handler handlers)
          (condition-case err
              (funcall handler msg)
            (error (message "emacs-mcp-channel: Handler error for %s: %s"
                            type (error-message-string err)))))))))

;;;; Public API

;;;###autoload
(defun emacs-mcp-channel-connect ()
  "Connect to the MCP channel server."
  (interactive)
  (when emacs-mcp-channel--process
    (emacs-mcp-channel-disconnect))
  (condition-case err
      (let ((proc (pcase emacs-mcp-channel-type
                    ('unix
                     (make-network-process
                      :name "emacs-mcp-channel"
                      :family 'local
                      :service emacs-mcp-channel-socket-path
                      :coding 'binary
                      :filter #'emacs-mcp-channel--filter
                      :sentinel #'emacs-mcp-channel--sentinel))
                    ('tcp
                     (make-network-process
                      :name "emacs-mcp-channel"
                      :host emacs-mcp-channel-host
                      :service emacs-mcp-channel-port
                      :coding 'binary
                      :filter #'emacs-mcp-channel--filter
                      :sentinel #'emacs-mcp-channel--sentinel)))))
        (setq emacs-mcp-channel--process proc)
        (message "emacs-mcp-channel: Connecting to %s..."
                 (if (eq emacs-mcp-channel-type 'unix)
                     emacs-mcp-channel-socket-path
                   (format "%s:%d" emacs-mcp-channel-host emacs-mcp-channel-port))))
    (file-error
     (message "emacs-mcp-channel: Connect failed: %s" (error-message-string err))
     (when (and emacs-mcp-channel-reconnect-interval
                (or (= 0 emacs-mcp-channel-max-reconnects)
                    (< emacs-mcp-channel--reconnect-count emacs-mcp-channel-max-reconnects)))
       (emacs-mcp-channel--schedule-reconnect)))))

;;;###autoload
(defun emacs-mcp-channel-disconnect ()
  "Disconnect from the MCP channel server."
  (interactive)
  (emacs-mcp-channel--cancel-reconnect)
  (when emacs-mcp-channel--process
    (delete-process emacs-mcp-channel--process)
    (setq emacs-mcp-channel--process nil))
  (when emacs-mcp-channel--buffer
    (kill-buffer emacs-mcp-channel--buffer)
    (setq emacs-mcp-channel--buffer nil))
  (message "emacs-mcp-channel: Disconnected"))

;;;###autoload
(defun emacs-mcp-channel-connected-p ()
  "Return t if channel is connected."
  (and emacs-mcp-channel--process
       (process-live-p emacs-mcp-channel--process)))

;;;###autoload
(defun emacs-mcp-channel-send (msg)
  "Send MSG through the channel.
MSG should be an alist like '((\"type\" . \"event\") (\"data\" . \"value\")).
If not connected, queues the message for later sending."
  (if (emacs-mcp-channel-connected-p)
      (let ((encoded (emacs-mcp-channel--encode msg)))
        (process-send-string emacs-mcp-channel--process encoded)
        t)
    (push msg emacs-mcp-channel--message-queue)
    nil))

;;;###autoload
(defun emacs-mcp-channel-on (event-type handler)
  "Register HANDLER for EVENT-TYPE.
EVENT-TYPE should be a keyword like :task-completed.
HANDLER receives the decoded message as an alist."
  (let ((handlers (gethash event-type emacs-mcp-channel--handlers)))
    (puthash event-type (cons handler handlers) emacs-mcp-channel--handlers)))

;;;###autoload
(defun emacs-mcp-channel-off (event-type &optional handler)
  "Unregister HANDLER from EVENT-TYPE.
If HANDLER is nil, remove all handlers for EVENT-TYPE."
  (if handler
      (let ((handlers (gethash event-type emacs-mcp-channel--handlers)))
        (puthash event-type (delete handler handlers) emacs-mcp-channel--handlers))
    (remhash event-type emacs-mcp-channel--handlers)))

;;;; Reconnection

(defun emacs-mcp-channel--schedule-reconnect ()
  "Schedule a reconnection attempt."
  (emacs-mcp-channel--cancel-reconnect)
  (cl-incf emacs-mcp-channel--reconnect-count)
  (message "emacs-mcp-channel: Reconnecting in %.1fs (attempt %d)..."
           emacs-mcp-channel-reconnect-interval
           emacs-mcp-channel--reconnect-count)
  (setq emacs-mcp-channel--reconnect-timer
        (run-with-timer emacs-mcp-channel-reconnect-interval nil
                        #'emacs-mcp-channel-connect)))

(defun emacs-mcp-channel--cancel-reconnect ()
  "Cancel pending reconnection."
  (when emacs-mcp-channel--reconnect-timer
    (cancel-timer emacs-mcp-channel--reconnect-timer)
    (setq emacs-mcp-channel--reconnect-timer nil)))

;;;; Swarm Integration Helpers

;;;###autoload
(defun emacs-mcp-channel-emit-swarm-event (event-type data)
  "Emit a swarm event of EVENT-TYPE with DATA."
  (emacs-mcp-channel-send
   `(("type" . ,(if (keywordp event-type)
                    (substring (symbol-name event-type) 1)
                  event-type))
     ("timestamp" . ,(float-time))
     ,@data)))

;;;###autoload
(defun emacs-mcp-channel-subscribe-swarm-events (handler)
  "Subscribe HANDLER to all swarm-related events."
  (dolist (type '(:task-completed :task-failed :prompt-shown
                  :state-changed :slave-spawned :slave-killed))
    (emacs-mcp-channel-on type handler)))

(provide 'emacs-mcp-channel)
;;; emacs-mcp-channel.el ends here
