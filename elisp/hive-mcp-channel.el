;;; hive-mcp-channel.el --- Bidirectional channel for MCP communication  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

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
;;   (hive-mcp-channel-connect)
;;   (hive-mcp-channel-send '(("type" . "ping") ("data" . "hello")))
;;   (hive-mcp-channel-on :task-completed #'my-handler)
;;   (hive-mcp-channel-disconnect)
;;

;;; Code:

(require 'cl-lib)

;; Soft dependency on nrepl-bencode (from CIDER)
(declare-function nrepl-bencode "nrepl-client")
(declare-function nrepl-bdecode "nrepl-client")
(declare-function nrepl-bdecode-string "nrepl-client")

;;;; Customization

(defgroup hive-mcp-channel nil
  "Bidirectional channel settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-channel-")

(defcustom hive-mcp-channel-type 'unix
  "Channel transport type."
  :type '(choice (const :tag "Unix socket" unix)
                 (const :tag "TCP" tcp))
  :group 'hive-mcp-channel)

(defcustom hive-mcp-channel-socket-path "/tmp/hive-mcp-channel.sock"
  "Path to Unix domain socket."
  :type 'string
  :group 'hive-mcp-channel)

(defcustom hive-mcp-channel-host "localhost"
  "Host for TCP channel."
  :type 'string
  :group 'hive-mcp-channel)

(defcustom hive-mcp-channel-port 9999
  "Port for TCP channel."
  :type 'integer
  :group 'hive-mcp-channel)

(defcustom hive-mcp-channel-reconnect-interval 5.0
  "Seconds between reconnection attempts."
  :type 'number
  :group 'hive-mcp-channel)

(defcustom hive-mcp-channel-max-reconnects 10
  "Maximum reconnection attempts (0 = unlimited)."
  :type 'integer
  :group 'hive-mcp-channel)

;;;; Internal State

(defvar hive-mcp-channel--process nil
  "Network process for channel connection.")

(defvar hive-mcp-channel--buffer nil
  "Buffer for accumulating incoming data.")

(defvar hive-mcp-channel--handlers (make-hash-table :test 'eq)
  "Event handlers indexed by event type (keyword).")

(defvar hive-mcp-channel--reconnect-timer nil
  "Timer for reconnection attempts.")

(defvar hive-mcp-channel--reconnect-count 0
  "Current reconnection attempt count.")

(defvar hive-mcp-channel--message-queue nil
  "Queue of messages pending send (when disconnected).")

;;;; Bencode Helpers

(defun hive-mcp-channel--bencode-available-p ()
  "Check if CIDER's bencode is available."
  (and (require 'nrepl-client nil t)
       (fboundp 'nrepl-bencode)))

(defun hive-mcp-channel--alist-to-nrepl-dict (alist)
  "Convert ALIST to nrepl-dict format for bencode encoding."
  (require 'nrepl-dict)
  (let ((result (list 'dict)))
    (dolist (pair alist)
      (setq result (append result (list (car pair) (cdr pair)))))
    result))

(defun hive-mcp-channel--encode (msg)
  "Encode MSG to bencode string.
MSG should be an alist like \\='((\"type\" . \"event\"))."
  (if (hive-mcp-channel--bencode-available-p)
      (nrepl-bencode (hive-mcp-channel--alist-to-nrepl-dict msg))
    ;; Fallback: simple bencode implementation
    (hive-mcp-channel--bencode-fallback msg)))

(defun hive-mcp-channel--decode (str)
  "Decode bencode STR to plist/alist."
  (if (hive-mcp-channel--bencode-available-p)
      (car (nrepl-bdecode-string str))
    ;; Fallback: simple bencode parser
    (hive-mcp-channel--bdecode-fallback str)))

;; Fallback bencode implementation (if CIDER not available)
(defun hive-mcp-channel--bencode-fallback (obj)
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
                             (concat (hive-mcp-channel--bencode-fallback
                                      (if (symbolp (car pair))
                                          (symbol-name (car pair))
                                        (car pair)))
                                     (hive-mcp-channel--bencode-fallback (cdr pair))))
                           obj "")
                "e")
      ;; List
      (concat "l"
              (mapconcat #'hive-mcp-channel--bencode-fallback obj "")
              "e")))
   ((vectorp obj)
    (concat "l"
            (mapconcat #'hive-mcp-channel--bencode-fallback (append obj nil) "")
            "e"))
   (t (error "Cannot bencode: %S" obj))))

(defun hive-mcp-channel--bdecode-fallback (str)
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

(defun hive-mcp-channel--filter (_proc str)
  "Process filter for channel receiving STR.
_PROC is the process (unused, required by Emacs process API)."
  (with-current-buffer (or hive-mcp-channel--buffer
                           (setq hive-mcp-channel--buffer
                                 (get-buffer-create " *mcp-channel*")))
    (goto-char (point-max))
    (insert str)
    ;; Try to decode complete messages
    (goto-char (point-min))
    (while (and (not (eobp))
                (hive-mcp-channel--try-decode-message)))))

(defun hive-mcp-channel--try-decode-message ()
  "Try to decode a complete bencode message from buffer.
Returns t if a message was decoded, nil otherwise."
  (condition-case nil
      (let ((start (point))
            (msg (hive-mcp-channel--decode (buffer-substring (point) (point-max)))))
        ;; If we get here, decoding succeeded
        ;; Calculate how much was consumed (approximate by re-encoding)
        (let ((encoded-len (length (hive-mcp-channel--encode msg))))
          (delete-region start (+ start encoded-len)))
        ;; Dispatch the message
        (hive-mcp-channel--dispatch msg)
        t)
    (error nil)))

(defun hive-mcp-channel--sentinel (_proc event)
  "Sentinel for channel handling EVENT.
_PROC is the process (unused, required by Emacs process API)."
  (cond
   ((string-match-p "deleted\\|connection broken\\|exited\\|failed" event)
    (message "hive-mcp-channel: Connection lost: %s" (string-trim event))
    (setq hive-mcp-channel--process nil)
    (when (and hive-mcp-channel-reconnect-interval
               (or (= 0 hive-mcp-channel-max-reconnects)
                   (< hive-mcp-channel--reconnect-count hive-mcp-channel-max-reconnects)))
      (hive-mcp-channel--schedule-reconnect)))
   ((string-match-p "open" event)
    (message "hive-mcp-channel: Connected")
    (setq hive-mcp-channel--reconnect-count 0)
    ;; Flush queued messages
    (while hive-mcp-channel--message-queue
      (hive-mcp-channel-send (pop hive-mcp-channel--message-queue))))))

;;;; Event Dispatch

(defun hive-mcp-channel--dispatch (msg)
  "Dispatch MSG to registered handlers."
  (let* ((type-str (or (cdr (assoc "type" msg))
                       (cdr (assoc 'type msg))))
         (type (when type-str
                 (intern (concat ":" type-str)))))
    (when type
      (let ((handlers (gethash type hive-mcp-channel--handlers)))
        (dolist (handler handlers)
          (condition-case err
              (funcall handler msg)
            (error (message "hive-mcp-channel: Handler error for %s: %s"
                            type (error-message-string err)))))))))

;;;; Public API

;;;###autoload
(defun hive-mcp-channel-connect ()
  "Connect to the MCP channel server."
  (interactive)
  (when hive-mcp-channel--process
    (hive-mcp-channel-disconnect))
  (condition-case err
      (let ((proc (pcase hive-mcp-channel-type
                    ('unix
                     (make-network-process
                      :name "hive-mcp-channel"
                      :family 'local
                      :service hive-mcp-channel-socket-path
                      :coding 'binary
                      :filter #'hive-mcp-channel--filter
                      :sentinel #'hive-mcp-channel--sentinel))
                    ('tcp
                     (make-network-process
                      :name "hive-mcp-channel"
                      :host hive-mcp-channel-host
                      :service hive-mcp-channel-port
                      :coding 'binary
                      :filter #'hive-mcp-channel--filter
                      :sentinel #'hive-mcp-channel--sentinel)))))
        (setq hive-mcp-channel--process proc)
        (message "hive-mcp-channel: Connecting to %s..."
                 (if (eq hive-mcp-channel-type 'unix)
                     hive-mcp-channel-socket-path
                   (format "%s:%d" hive-mcp-channel-host hive-mcp-channel-port))))
    (file-error
     (message "hive-mcp-channel: Connect failed: %s" (error-message-string err))
     (when (and hive-mcp-channel-reconnect-interval
                (or (= 0 hive-mcp-channel-max-reconnects)
                    (< hive-mcp-channel--reconnect-count hive-mcp-channel-max-reconnects)))
       (hive-mcp-channel--schedule-reconnect)))))

;;;###autoload
(defun hive-mcp-channel-disconnect ()
  "Disconnect from the MCP channel server."
  (interactive)
  (hive-mcp-channel--cancel-reconnect)
  (when hive-mcp-channel--process
    (delete-process hive-mcp-channel--process)
    (setq hive-mcp-channel--process nil))
  (when hive-mcp-channel--buffer
    (kill-buffer hive-mcp-channel--buffer)
    (setq hive-mcp-channel--buffer nil))
  (message "hive-mcp-channel: Disconnected"))

;;;###autoload
(defun hive-mcp-channel-connected-p ()
  "Return t if channel is connected."
  (and hive-mcp-channel--process
       (process-live-p hive-mcp-channel--process)))

;;;###autoload
(defun hive-mcp-channel-send (msg)
  "Send MSG through the channel.
MSG should be an alist like \\='((\"type\" . \"event\") ...).
If not connected, queues the message for later sending."
  (if (hive-mcp-channel-connected-p)
      (let ((encoded (hive-mcp-channel--encode msg)))
        (process-send-string hive-mcp-channel--process encoded)
        t)
    (push msg hive-mcp-channel--message-queue)
    nil))

;;;###autoload
(defun hive-mcp-channel-on (event-type handler)
  "Register HANDLER for EVENT-TYPE.
EVENT-TYPE should be a keyword like :task-completed.
HANDLER receives the decoded message as an alist."
  (let ((handlers (gethash event-type hive-mcp-channel--handlers)))
    (puthash event-type (cons handler handlers) hive-mcp-channel--handlers)))

;;;###autoload
(defun hive-mcp-channel-off (event-type &optional handler)
  "Unregister HANDLER from EVENT-TYPE.
If HANDLER is nil, remove all handlers for EVENT-TYPE."
  (if handler
      (let ((handlers (gethash event-type hive-mcp-channel--handlers)))
        (puthash event-type (delete handler handlers) hive-mcp-channel--handlers))
    (remhash event-type hive-mcp-channel--handlers)))

;;;; Reconnection

(defun hive-mcp-channel--schedule-reconnect ()
  "Schedule a reconnection attempt."
  (hive-mcp-channel--cancel-reconnect)
  (cl-incf hive-mcp-channel--reconnect-count)
  (message "hive-mcp-channel: Reconnecting in %.1fs (attempt %d)..."
           hive-mcp-channel-reconnect-interval
           hive-mcp-channel--reconnect-count)
  (setq hive-mcp-channel--reconnect-timer
        (run-with-timer hive-mcp-channel-reconnect-interval nil
                        #'hive-mcp-channel-connect)))

(defun hive-mcp-channel--cancel-reconnect ()
  "Cancel pending reconnection."
  (when hive-mcp-channel--reconnect-timer
    (cancel-timer hive-mcp-channel--reconnect-timer)
    (setq hive-mcp-channel--reconnect-timer nil)))

;;;; Swarm Integration Helpers

;;;###autoload
(defun hive-mcp-channel-emit-swarm-event (event-type data)
  "Emit a swarm event of EVENT-TYPE with DATA."
  (hive-mcp-channel-send
   `(("type" . ,(if (keywordp event-type)
                    (substring (symbol-name event-type) 1)
                  event-type))
     ("timestamp" . ,(float-time))
     ,@data)))

;;;###autoload
(defun hive-mcp-channel-subscribe-swarm-events (handler)
  "Subscribe HANDLER to all swarm-related events."
  (dolist (type '(:task-completed :task-failed :prompt-shown
                  :state-changed :slave-spawned :slave-killed))
    (hive-mcp-channel-on type handler)))

(provide 'hive-mcp-channel)
;;; hive-mcp-channel.el ends here
