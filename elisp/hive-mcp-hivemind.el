;;; hive-mcp-hivemind.el --- Hivemind coordinator UI for swarm agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho

;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; Keywords: tools, ai, swarm

;;; Commentary:
;;
;; Provides the Emacs UI for the hivemind coordinator.
;; Receives events from swarm agents and displays them in real-time.
;; Handles human-in-the-loop decisions via ask/respond flow.
;;
;; Features:
;; - Real-time agent status display
;; - Notification popups for important events
;; - Pending questions buffer with quick response
;; - Agent activity log

;;; Code:

(require 'hive-mcp-channel)
(require 'hive-mcp-channel-ws)

;; Soft dependency on swarm hooks (for state sync)
(declare-function hive-mcp-swarm-hooks-dispatch "hive-mcp-swarm-hooks")

;;; Customization

(defgroup hive-mcp-hivemind nil
  "Hivemind coordinator for swarm agents."
  :group 'hive-mcp
  :prefix "hive-mcp-hivemind-")

(defcustom hive-mcp-hivemind-notify-events '(:hivemind-ask :hivemind-error :hivemind-blocked)
  "Event types that trigger desktop notifications."
  :type '(repeat symbol)
  :group 'hive-mcp-hivemind)

(defcustom hive-mcp-hivemind-log-buffer "*Hivemind Log*"
  "Buffer name for hivemind activity log."
  :type 'string
  :group 'hive-mcp-hivemind)

;;; State

(defvar hive-mcp-hivemind--agents (make-hash-table :test 'equal)
  "Hash table of agent-id -> status info.")

(defvar hive-mcp-hivemind--pending-asks nil
  "List of pending ask events awaiting response.")

;;; Event Handlers

(defun hive-mcp-hivemind--handle-shout (event)
  "Handle a hivemind shout EVENT from an agent.
EVENT may have symbol keys (from WebSocket JSON) or string keys (from bencode)."
  (let* ((agent-id (or (alist-get 'agent-id event)
                       (cdr (assoc "agent-id" event))))
         (data (or (alist-get 'data event)
                   (cdr (assoc "data" event))))
         (event-type (or (alist-get 'type event)
                         (cdr (assoc "type" event))))
         (timestamp (or (alist-get 'timestamp event)
                        (cdr (assoc "timestamp" event))))
         (task (or (alist-get 'task data)
                   (cdr (assoc "task" data))))
         (message (or (alist-get 'message data)
                      (cdr (assoc "message" data)))))
    ;; Update agent registry
    (puthash agent-id
             (list :status event-type
                   :task task
                   :message message
                   :last-seen (or timestamp (float-time)))
             hive-mcp-hivemind--agents)
    ;; Log the event
    (hive-mcp-hivemind--log event-type agent-id (or message task))
    ;; Notify if important
    (when (member (intern event-type) hive-mcp-hivemind-notify-events)
      (hive-mcp-hivemind--notify agent-id event-type message))
    ;; Dispatch to hooks for state sync (if hooks module is loaded)
    (when (fboundp 'hive-mcp-swarm-hooks-dispatch)
      (hive-mcp-swarm-hooks-dispatch
       (intern (concat ":" event-type))
       (list :agent-id agent-id
             :event-type event-type
             :task task
             :message message
             :data data
             :timestamp (or timestamp (float-time)))))))

(defun hive-mcp-hivemind--handle-ask (event)
  "Handle a hivemind ask EVENT requiring human response.
EVENT may have symbol keys (from WebSocket JSON) or string keys (from bencode)."
  (let* ((ask-id (or (alist-get 'ask-id event)
                     (cdr (assoc "ask-id" event))))
         (agent-id (or (alist-get 'agent-id event)
                       (cdr (assoc "agent-id" event))))
         (question (or (alist-get 'question event)
                       (cdr (assoc "question" event))))
         (options (or (alist-get 'options event)
                      (cdr (assoc "options" event)))))
    ;; Store pending ask
    (push (list :ask-id ask-id
                :agent-id agent-id
                :question question
                :options options
                :timestamp (float-time))
          hive-mcp-hivemind--pending-asks)
    ;; Log it
    (hive-mcp-hivemind--log "ASK" agent-id question)
    ;; Urgent notification
    (hive-mcp-hivemind--notify agent-id "NEEDS INPUT" question)
    ;; Show in minibuffer
    (message "HIVEMIND ASK [%s]: %s" agent-id question)))

(defun hive-mcp-hivemind--log (event-type agent-id message)
  "Log EVENT-TYPE from AGENT-ID with MESSAGE to the hivemind log buffer."
  (with-current-buffer (get-buffer-create hive-mcp-hivemind-log-buffer)
    (goto-char (point-max))
    (insert (format "[%s] %s (%s): %s\n"
                    (format-time-string "%H:%M:%S")
                    event-type
                    (or agent-id "unknown")
                    (or message "")))))

(defun hive-mcp-hivemind--notify (agent-id event-type message)
  "Show desktop notification for EVENT-TYPE from AGENT-ID."
  (when (fboundp 'notifications-notify)
    (notifications-notify
     :title (format "Hivemind: %s" event-type)
     :body (format "[%s] %s" agent-id (or message ""))
     :urgency (if (string-match-p "ask\\|error\\|blocked" (downcase (or event-type "")))
                  'critical
                'normal))))

;;; Public API

;;;###autoload
(defun hive-mcp-hivemind-status ()
  "Show current hivemind status in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Hivemind Status*")
    (erase-buffer)
    (insert "=== HIVEMIND STATUS ===\n\n")
    ;; Agents
    (insert "AGENTS:\n")
    (if (zerop (hash-table-count hive-mcp-hivemind--agents))
        (insert "  (no agents registered)\n")
      (maphash (lambda (id info)
                 (insert (format "  %s: %s - %s\n"
                                 id
                                 (plist-get info :status)
                                 (or (plist-get info :task) ""))))
               hive-mcp-hivemind--agents))
    ;; Pending asks
    (insert "\nPENDING QUESTIONS:\n")
    (if (null hive-mcp-hivemind--pending-asks)
        (insert "  (none)\n")
      (dolist (ask hive-mcp-hivemind--pending-asks)
        (insert (format "  [%s] %s: %s\n"
                        (plist-get ask :ask-id)
                        (plist-get ask :agent-id)
                        (plist-get ask :question)))))
    (display-buffer (current-buffer))))

;;;###autoload
(defun hive-mcp-hivemind-respond (ask-id response)
  "Respond to ASK-ID with RESPONSE.
Sends the response to the Clojure side via channel."
  (interactive
   (let* ((asks hive-mcp-hivemind--pending-asks)
          (ask (if (= 1 (length asks))
                   (car asks)
                 (let ((id (completing-read "Ask ID: "
                                            (mapcar (lambda (a) (plist-get a :ask-id)) asks))))
                   (seq-find (lambda (a) (string= (plist-get a :ask-id) id)) asks))))
          (options (plist-get ask :options))
          (resp (if options
                    (completing-read (format "%s: " (plist-get ask :question)) options)
                  (read-string (format "%s: " (plist-get ask :question))))))
     (list (plist-get ask :ask-id) resp)))
  ;; Send response via WebSocket channel (primary)
  (if (hive-mcp-channel-ws-connected-p)
      (hive-mcp-channel-ws-send
       `((type . "hivemind-response")
         (ask-id . ,ask-id)
         (decision . ,response)
         (by . "human")))
    ;; Fallback to old bencode channel
    (hive-mcp-channel-send
     `(("type" . "hivemind-response")
       ("ask-id" . ,ask-id)
       ("decision" . ,response)
       ("by" . "human"))))
  ;; Remove from pending
  (setq hive-mcp-hivemind--pending-asks
        (seq-remove (lambda (a) (string= (plist-get a :ask-id) ask-id))
                    hive-mcp-hivemind--pending-asks))
  (message "Responded to %s: %s" ask-id response))

;;;###autoload
(defun hive-mcp-hivemind-show-log ()
  "Show the hivemind activity log."
  (interactive)
  (display-buffer (get-buffer-create hive-mcp-hivemind-log-buffer)))

;;;###autoload
(defun hive-mcp-hivemind-clear ()
  "Clear hivemind state (for debugging)."
  (interactive)
  (clrhash hive-mcp-hivemind--agents)
  (setq hive-mcp-hivemind--pending-asks nil)
  (message "Hivemind state cleared"))

;;; Event Registration

(defun hive-mcp-hivemind-register-handlers ()
  "Register hivemind event handlers with the channel."
  ;; Register with WebSocket channel (primary - recommended)
  ;; WebSocket uses string event types like "hivemind-progress"
  (dolist (type '("hivemind-progress" "hivemind-completed" "hivemind-error"
                  "hivemind-blocked" "hivemind-started"))
    (hive-mcp-channel-ws-on type #'hive-mcp-hivemind--handle-shout))
  (hive-mcp-channel-ws-on "hivemind-ask" #'hive-mcp-hivemind--handle-ask)
  ;; Also register with old bencode channel for backwards compatibility
  (dolist (type '(:hivemind-progress :hivemind-completed :hivemind-error
                  :hivemind-blocked :hivemind-started))
    (hive-mcp-channel-on type #'hive-mcp-hivemind--handle-shout))
  (hive-mcp-channel-on :hivemind-ask #'hive-mcp-hivemind--handle-ask))

;; Register on load
(hive-mcp-hivemind-register-handlers)

(provide 'hive-mcp-hivemind)
;;; hive-mcp-hivemind.el ends here
