;;; emacs-mcp-hivemind.el --- Hivemind coordinator UI for swarm agents  -*- lexical-binding: t; -*-

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

(require 'emacs-mcp-channel)

;;; Customization

(defgroup emacs-mcp-hivemind nil
  "Hivemind coordinator for swarm agents."
  :group 'emacs-mcp
  :prefix "emacs-mcp-hivemind-")

(defcustom emacs-mcp-hivemind-notify-events '(:hivemind-ask :hivemind-error :hivemind-blocked)
  "Event types that trigger desktop notifications."
  :type '(repeat symbol)
  :group 'emacs-mcp-hivemind)

(defcustom emacs-mcp-hivemind-log-buffer "*Hivemind Log*"
  "Buffer name for hivemind activity log."
  :type 'string
  :group 'emacs-mcp-hivemind)

;;; State

(defvar emacs-mcp-hivemind--agents (make-hash-table :test 'equal)
  "Hash table of agent-id -> status info.")

(defvar emacs-mcp-hivemind--pending-asks nil
  "List of pending ask events awaiting response.")

;;; Event Handlers

(defun emacs-mcp-hivemind--handle-shout (event)
  "Handle a hivemind shout EVENT from an agent."
  (let* ((agent-id (cdr (assoc "agent-id" event)))
         (data (cdr (assoc "data" event)))
         (event-type (cdr (assoc "type" event)))
         (timestamp (cdr (assoc "timestamp" event)))
         (task (cdr (assoc "task" data)))
         (message (cdr (assoc "message" data))))
    ;; Update agent registry
    (puthash agent-id
             (list :status event-type
                   :task task
                   :message message
                   :last-seen (or timestamp (float-time)))
             emacs-mcp-hivemind--agents)
    ;; Log the event
    (emacs-mcp-hivemind--log event-type agent-id (or message task))
    ;; Notify if important
    (when (member (intern event-type) emacs-mcp-hivemind-notify-events)
      (emacs-mcp-hivemind--notify agent-id event-type message))))

(defun emacs-mcp-hivemind--handle-ask (event)
  "Handle a hivemind ask EVENT requiring human response."
  (let* ((ask-id (cdr (assoc "ask-id" event)))
         (agent-id (cdr (assoc "agent-id" event)))
         (question (cdr (assoc "question" event)))
         (options (cdr (assoc "options" event))))
    ;; Store pending ask
    (push (list :ask-id ask-id
                :agent-id agent-id
                :question question
                :options options
                :timestamp (float-time))
          emacs-mcp-hivemind--pending-asks)
    ;; Log it
    (emacs-mcp-hivemind--log "ASK" agent-id question)
    ;; Urgent notification
    (emacs-mcp-hivemind--notify agent-id "NEEDS INPUT" question)
    ;; Show in minibuffer
    (message "HIVEMIND ASK [%s]: %s" agent-id question)))

(defun emacs-mcp-hivemind--log (event-type agent-id message)
  "Log EVENT-TYPE from AGENT-ID with MESSAGE to the hivemind log buffer."
  (with-current-buffer (get-buffer-create emacs-mcp-hivemind-log-buffer)
    (goto-char (point-max))
    (insert (format "[%s] %s (%s): %s\n"
                    (format-time-string "%H:%M:%S")
                    event-type
                    (or agent-id "unknown")
                    (or message "")))))

(defun emacs-mcp-hivemind--notify (agent-id event-type message)
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
(defun emacs-mcp-hivemind-status ()
  "Show current hivemind status in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Hivemind Status*")
    (erase-buffer)
    (insert "=== HIVEMIND STATUS ===\n\n")
    ;; Agents
    (insert "AGENTS:\n")
    (if (zerop (hash-table-count emacs-mcp-hivemind--agents))
        (insert "  (no agents registered)\n")
      (maphash (lambda (id info)
                 (insert (format "  %s: %s - %s\n"
                                 id
                                 (plist-get info :status)
                                 (or (plist-get info :task) ""))))
               emacs-mcp-hivemind--agents))
    ;; Pending asks
    (insert "\nPENDING QUESTIONS:\n")
    (if (null emacs-mcp-hivemind--pending-asks)
        (insert "  (none)\n")
      (dolist (ask emacs-mcp-hivemind--pending-asks)
        (insert (format "  [%s] %s: %s\n"
                        (plist-get ask :ask-id)
                        (plist-get ask :agent-id)
                        (plist-get ask :question)))))
    (display-buffer (current-buffer))))

;;;###autoload
(defun emacs-mcp-hivemind-respond (ask-id response)
  "Respond to ASK-ID with RESPONSE.
Sends the response to the Clojure side via channel."
  (interactive
   (let* ((asks emacs-mcp-hivemind--pending-asks)
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
  ;; Send response via channel
  (emacs-mcp-channel-send
   `(("type" . "hivemind-response")
     ("ask-id" . ,ask-id)
     ("decision" . ,response)
     ("by" . "human")))
  ;; Remove from pending
  (setq emacs-mcp-hivemind--pending-asks
        (seq-remove (lambda (a) (string= (plist-get a :ask-id) ask-id))
                    emacs-mcp-hivemind--pending-asks))
  (message "Responded to %s: %s" ask-id response))

;;;###autoload
(defun emacs-mcp-hivemind-show-log ()
  "Show the hivemind activity log."
  (interactive)
  (display-buffer (get-buffer-create emacs-mcp-hivemind-log-buffer)))

;;;###autoload
(defun emacs-mcp-hivemind-clear ()
  "Clear hivemind state (for debugging)."
  (interactive)
  (clrhash emacs-mcp-hivemind--agents)
  (setq emacs-mcp-hivemind--pending-asks nil)
  (message "Hivemind state cleared"))

;;; Event Registration

(defun emacs-mcp-hivemind-register-handlers ()
  "Register hivemind event handlers with the channel."
  ;; Shout events (progress, completed, error, blocked, started)
  (dolist (type '(:hivemind-progress :hivemind-completed :hivemind-error
                  :hivemind-blocked :hivemind-started))
    (emacs-mcp-channel-on type #'emacs-mcp-hivemind--handle-shout))
  ;; Ask events
  (emacs-mcp-channel-on :hivemind-ask #'emacs-mcp-hivemind--handle-ask))

;; Register on load
(emacs-mcp-hivemind-register-handlers)

(provide 'emacs-mcp-hivemind)
;;; emacs-mcp-hivemind.el ends here
