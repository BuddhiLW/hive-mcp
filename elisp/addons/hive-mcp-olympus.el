;;; hive-mcp-olympus.el --- Swarm grid view for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW)
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Olympus provides a grid view for managing multiple ling buffers.
;; It arranges ling terminal buffers in an optimal grid layout and
;; provides keybindings for quick navigation.
;;
;; P0 Feature: Real-Time Notifications
;; - Global notifications for stuck/blocked lings (no buffer visit needed)
;; - Background monitor that polls every 30s for stuck states
;; - Unified dashboard buffer showing ALL lings with color-coded status
;;
;; Layout algorithm:
;; - n=1: Full screen
;; - n=2: Side-by-side (horizontal split)
;; - n=3: 2x2 with one empty cell
;; - n=4: 2x2 perfect grid
;; - n=5+: Tabbed, 4 per tab
;;
;; Keybindings (hive-mcp-olympus-mode):
;; - C-c h o   : Arrange all lings in grid (hive-olympus)
;; - C-c h d   : Open unified dashboard (hive-mcp-olympus-dashboard)
;; - C-c h 1-4 : Focus ling at position (hive-olympus-focus)
;; - C-c h n   : Next tab (hive-olympus-tab-next)
;; - C-c h p   : Previous tab (hive-olympus-tab-prev)
;; - C-c h r   : Restore grid from focused view
;; - C-c h m   : Start background monitor
;; - C-c h M   : Stop background monitor
;;
;; Dashboard keybindings:
;; - g     : Refresh dashboard
;; - RET   : Switch to ling buffer at point
;; - f     : Focus ling in grid view
;; - o     : Arrange all lings in grid
;; - q     : Quit dashboard
;;
;; Status colors in dashboard:
;; - Green  : Working normally
;; - Gray   : Idle
;; - Orange : Prompt pending (waiting for human input)
;; - Yellow : Blocked (needs attention)
;; - Red    : Stuck (working > 2min) or Error

;;; Code:

(require 'hive-mcp)

;; Declare swarm dependencies
(defvar hive-mcp-swarm--slaves)
(declare-function hive-mcp-channel-on "hive-mcp-channel")
(declare-function hive-mcp-channel-off "hive-mcp-channel")
(declare-function hive-mcp-channel-connected-p "hive-mcp-channel")

;;; =============================================================================
;;; Customization
;;; =============================================================================

(defgroup hive-mcp-olympus nil
  "Olympus grid view for swarm lings."
  :group 'hive-mcp
  :prefix "hive-mcp-olympus-")

(defcustom hive-mcp-olympus-lings-per-tab 4
  "Maximum lings per tab before creating new tab."
  :type 'integer
  :group 'hive-mcp-olympus)

(defcustom hive-mcp-olympus-default-mode 'auto
  "Default layout mode: auto, manual, or stacked."
  :type '(choice (const :tag "Auto-arrange" auto)
                 (const :tag "Manual positioning" manual)
                 (const :tag "Stacked/overlapping" stacked))
  :group 'hive-mcp-olympus)

;;; =============================================================================
;;; State
;;; =============================================================================

(defvar hive-mcp-olympus--current-tab 0
  "Currently active tab index (0-based).")

(defvar hive-mcp-olympus--layout-mode 'auto
  "Current layout mode: auto, manual, or stacked.")

(defvar hive-mcp-olympus--focused-ling nil
  "Currently focused/maximized ling ID, or nil for grid view.")

(defvar hive-mcp-olympus--positions nil
  "Alist of (ling-id . (:row R :col C :tab T)) positions.")

;;; =============================================================================
;;; Layout Helpers
;;; =============================================================================

(defun hive-mcp-olympus--get-ling-buffers ()
  "Get list of all ling terminal buffers.
Returns list of (ling-id . buffer) pairs, sorted by spawn time.
Only returns lings with live buffers."
  (let (result)
    ;; Check if swarm module is loaded and has slaves
    (when (and (boundp 'hive-mcp-swarm--slaves)
               (hash-table-p hive-mcp-swarm--slaves))
      (maphash
       (lambda (slave-id slave-plist)
         (let ((buffer (plist-get slave-plist :buffer)))
           ;; Only include slaves with live buffers
           (when (and buffer (buffer-live-p buffer))
             (push (cons slave-id buffer) result))))
       hive-mcp-swarm--slaves))
    ;; Sort by slave-id (which includes timestamp) for consistent ordering
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun hive-mcp-olympus--calculate-layout (n)
  "Calculate layout for N lings.
Returns plist with :rows :cols or :tabs :per-tab."
  (cond
   ((zerop n) '(:rows 0 :cols 0))
   ((= n 1) '(:rows 1 :cols 1))
   ((= n 2) '(:rows 1 :cols 2))
   ((= n 3) '(:rows 2 :cols 2 :empty-cells ((1 . 1))))
   ((= n 4) '(:rows 2 :cols 2))
   (t `(:tabs ,(ceiling n hive-mcp-olympus-lings-per-tab)
        :per-tab ,hive-mcp-olympus-lings-per-tab))))

(defun hive-mcp-olympus--arrange-windows (layout ling-buffers)
  "Arrange windows according to LAYOUT for LING-BUFFERS.
LAYOUT is plist from `hive-mcp-olympus--calculate-layout'.
LING-BUFFERS is list of (ling-id . buffer) pairs."
  ;; TODO: Implement window arrangement
  ;; This will use `split-window', `set-window-buffer', etc.
  (delete-other-windows)
  (let ((rows (plist-get layout :rows))
        (cols (plist-get layout :cols))
        (tabs (plist-get layout :tabs)))
    (cond
     ;; Tabbed layout
     (tabs
      (hive-mcp-olympus--arrange-tabbed layout ling-buffers))
     ;; Grid layout
     ((and rows cols (> rows 0) (> cols 0))
      (hive-mcp-olympus--arrange-grid rows cols ling-buffers)))))

(defun hive-mcp-olympus--arrange-grid (rows cols ling-buffers)
  "Arrange LING-BUFFERS in ROWS x COLS grid.
Creates a grid of windows and assigns buffers to them.
Empty cells (when buffers < rows*cols) are left with current buffer."
  (delete-other-windows)
  (when (and (> rows 0) (> cols 0) ling-buffers)
    (let ((windows (list (selected-window)))
          (buf-index 0))
      ;; Step 1: Create columns by splitting horizontally (cols - 1) times
      (dotimes (_ (1- cols))
        (let ((new-win (split-window (car (last windows)) nil 'right)))
          (setq windows (append windows (list new-win)))))
      ;; Step 2: Split each column vertically to create rows
      (when (> rows 1)
        (let ((col-windows windows))
          (setq windows nil)
          (dolist (col-win col-windows)
            (push col-win windows)
            (dotimes (_ (1- rows))
              (let ((new-win (split-window (car windows) nil 'below)))
                (push new-win windows))))
          ;; Reverse to get top-left to bottom-right order
          (setq windows (nreverse windows))))
      ;; Step 3: Balance windows for even sizing
      (balance-windows)
      ;; Step 4: Assign buffers to windows in row-major order
      ;; Row-major: (0,0), (0,1), (1,0), (1,1) for 2x2
      (dolist (win windows)
        (when (and (< buf-index (length ling-buffers))
                   (window-live-p win))
          (let ((ling-entry (nth buf-index ling-buffers)))
            (when ling-entry
              (set-window-buffer win (cdr ling-entry))
              ;; Store position in olympus state
              (let* ((row (/ buf-index cols))
                     (col (mod buf-index cols)))
                (setf (alist-get (car ling-entry) hive-mcp-olympus--positions)
                      (list :row row :col col :tab hive-mcp-olympus--current-tab)))))
          (cl-incf buf-index))))))

(defun hive-mcp-olympus--arrange-tabbed (layout ling-buffers)
  "Arrange LING-BUFFERS in tabbed layout.
Shows only current tab's lings."
  ;; TODO: Implement tab-based arrangement
  ;; Would create tab-bar-mode tabs or use custom tab switching
  (let* ((per-tab (plist-get layout :per-tab))
         (start (* hive-mcp-olympus--current-tab per-tab))
         (end (min (+ start per-tab) (length ling-buffers)))
         (tab-buffers (seq-subseq ling-buffers start end)))
    (hive-mcp-olympus--arrange-grid 2 2 tab-buffers)))

;;; =============================================================================
;;; Interactive Commands
;;; =============================================================================

;;;###autoload
(defun hive-olympus ()
  "Arrange all ling buffers in optimal grid.
Queries MCP for current lings and arranges them according
to the layout algorithm."
  (interactive)
  ;; TODO: Call MCP olympus_arrange and apply layout
  (let ((ling-buffers (hive-mcp-olympus--get-ling-buffers)))
    (if (null ling-buffers)
        (message "No lings to arrange")
      (let ((layout (hive-mcp-olympus--calculate-layout (length ling-buffers))))
        (hive-mcp-olympus--arrange-windows layout ling-buffers)
        (setq hive-mcp-olympus--focused-ling nil)
        (message "Olympus: Arranged %d lings" (length ling-buffers))))))

;;;###autoload
(defun hive-olympus-focus (n)
  "Focus ling at position N (1-4).
Maximizes the ling's buffer to full frame."
  (interactive "p")
  (let* ((ling-buffers (hive-mcp-olympus--get-ling-buffers))
         (ling-entry (nth (1- n) ling-buffers)))
    (if ling-entry
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) (cdr ling-entry))
          (setq hive-mcp-olympus--focused-ling (car ling-entry))
          (message "Olympus: Focused ling %s" (car ling-entry)))
      (message "Olympus: No ling at position %d" n))))

;;;###autoload
(defun hive-olympus-restore ()
  "Restore grid view from focused ling."
  (interactive)
  (setq hive-mcp-olympus--focused-ling nil)
  (hive-olympus))

;;;###autoload
(defun hive-olympus-tab-next ()
  "Switch to next tab (for 5+ lings)."
  (interactive)
  (let* ((ling-buffers (hive-mcp-olympus--get-ling-buffers))
         (layout (hive-mcp-olympus--calculate-layout (length ling-buffers)))
         (max-tabs (or (plist-get layout :tabs) 1)))
    (setq hive-mcp-olympus--current-tab
          (mod (1+ hive-mcp-olympus--current-tab) max-tabs))
    (hive-olympus)
    (message "Olympus: Tab %d/%d" (1+ hive-mcp-olympus--current-tab) max-tabs)))

;;;###autoload
(defun hive-olympus-tab-prev ()
  "Switch to previous tab (for 5+ lings)."
  (interactive)
  (let* ((ling-buffers (hive-mcp-olympus--get-ling-buffers))
         (layout (hive-mcp-olympus--calculate-layout (length ling-buffers)))
         (max-tabs (or (plist-get layout :tabs) 1)))
    (setq hive-mcp-olympus--current-tab
          (mod (1- hive-mcp-olympus--current-tab) max-tabs))
    (hive-olympus)
    (message "Olympus: Tab %d/%d" (1+ hive-mcp-olympus--current-tab) max-tabs)))

;;;###autoload
(defun hive-olympus-tab-goto (n)
  "Jump to tab N (1-indexed)."
  (interactive "p")
  (setq hive-mcp-olympus--current-tab (1- n))
  (hive-olympus))

;;; =============================================================================
;;; Minor Mode
;;; =============================================================================

(defvar hive-mcp-olympus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h o") #'hive-olympus)
    (define-key map (kbd "C-c h d") #'hive-mcp-olympus-dashboard)
    (define-key map (kbd "C-c h 1") (lambda () (interactive) (hive-olympus-focus 1)))
    (define-key map (kbd "C-c h 2") (lambda () (interactive) (hive-olympus-focus 2)))
    (define-key map (kbd "C-c h 3") (lambda () (interactive) (hive-olympus-focus 3)))
    (define-key map (kbd "C-c h 4") (lambda () (interactive) (hive-olympus-focus 4)))
    (define-key map (kbd "C-c h n") #'hive-olympus-tab-next)
    (define-key map (kbd "C-c h p") #'hive-olympus-tab-prev)
    (define-key map (kbd "C-c h r") #'hive-olympus-restore)
    (define-key map (kbd "C-c h m") #'hive-mcp-olympus-start-monitor)
    (define-key map (kbd "C-c h M") #'hive-mcp-olympus-stop-monitor)
    map)
  "Keymap for `hive-mcp-olympus-mode'.")

;;;###autoload
(define-minor-mode hive-mcp-olympus-mode
  "Minor mode for Olympus grid view keybindings.

When enabled, subscribes to channel events for automatic layout
updates when lings are spawned/killed.

\\{hive-mcp-olympus-mode-map}"
  :lighter " Olympus"
  :keymap hive-mcp-olympus-mode-map
  :global t
  (if hive-mcp-olympus-mode
      (progn
        ;; Soft-require swarm module to ensure slaves hash table is initialized
        (require 'hive-mcp-swarm nil t)
        ;; Subscribe to events
        (hive-mcp-olympus--subscribe-events)
        (message "Olympus mode enabled"))
    ;; Mode disabled: unsubscribe and reset state
    (hive-mcp-olympus--unsubscribe-events)
    (setq hive-mcp-olympus--focused-ling nil
          hive-mcp-olympus--current-tab 0
          hive-mcp-olympus--positions nil)))

;;; =============================================================================
;;; Event Handlers (Channel Integration)
;;; =============================================================================

(defun hive-mcp-olympus--handle-layout-changed (event)
  "Handle :olympus/layout-changed EVENT from MCP.
Triggers window rearrangement.
EVENT contains layout hints from MCP coordinator."
  (let ((layout-mode (cdr (assoc "layout-mode" event)))
        (tab (cdr (assoc "tab" event))))
    ;; Apply layout mode if specified
    (when layout-mode
      (setq hive-mcp-olympus--layout-mode (intern layout-mode)))
    ;; Switch to specified tab if provided
    (when tab
      (setq hive-mcp-olympus--current-tab (if (stringp tab)
                                               (string-to-number tab)
                                             tab))))
  ;; Rearrange with new settings
  (hive-olympus))

(defun hive-mcp-olympus--handle-focus-changed (event)
  "Handle :olympus/focus-changed EVENT from MCP."
  (let ((ling-id (plist-get event :ling-id)))
    (if ling-id
        (hive-olympus-focus-by-id ling-id)
      (hive-olympus-restore))))

(defun hive-olympus-focus-by-id (ling-id)
  "Focus ling by LING-ID."
  (let* ((ling-buffers (hive-mcp-olympus--get-ling-buffers))
         (entry (assoc ling-id ling-buffers)))
    (when entry
      (delete-other-windows)
      (set-window-buffer (selected-window) (cdr entry))
      (setq hive-mcp-olympus--focused-ling ling-id))))

;;;; Channel Subscription Setup

(defun hive-mcp-olympus--subscribe-events ()
  "Subscribe to olympus-related channel events.
Registers handlers for layout and focus change events from MCP."
  (when (fboundp 'hive-mcp-channel-on)
    ;; Subscribe to layout changes (e.g., from MCP olympus_arrange calls)
    (hive-mcp-channel-on :olympus/layout-changed
                         #'hive-mcp-olympus--handle-layout-changed)
    ;; Subscribe to focus changes (e.g., from MCP olympus_focus calls)
    (hive-mcp-channel-on :olympus/focus-changed
                         #'hive-mcp-olympus--handle-focus-changed)
    ;; Also subscribe to swarm events to auto-refresh on spawn/kill
    (hive-mcp-channel-on :slave-spawned
                         #'hive-mcp-olympus--handle-slave-change)
    (hive-mcp-channel-on :slave-killed
                         #'hive-mcp-olympus--handle-slave-change)))

(defun hive-mcp-olympus--unsubscribe-events ()
  "Unsubscribe from olympus-related channel events."
  (when (fboundp 'hive-mcp-channel-off)
    (hive-mcp-channel-off :olympus/layout-changed
                          #'hive-mcp-olympus--handle-layout-changed)
    (hive-mcp-channel-off :olympus/focus-changed
                          #'hive-mcp-olympus--handle-focus-changed)
    (hive-mcp-channel-off :slave-spawned
                          #'hive-mcp-olympus--handle-slave-change)
    (hive-mcp-channel-off :slave-killed
                          #'hive-mcp-olympus--handle-slave-change)))

(defun hive-mcp-olympus--handle-slave-change (_event)
  "Handle slave spawn/kill events by refreshing the grid.
_EVENT is the event payload (unused, just triggers refresh)."
  ;; Small delay to let swarm registry update
  (run-at-time 0.5 nil
               (lambda ()
                 (condition-case err
                     (when (and hive-mcp-olympus-mode
                                (not hive-mcp-olympus--focused-ling))
                       (hive-olympus))
                   (error
                    (message "[olympus] Timer error in slave-change handler: %s"
                             (error-message-string err)))))))

;;; =============================================================================
;;; Global Notification Handlers (P0: Real-time alerts)
;;; =============================================================================

;; Soft dependency on notify module
(declare-function hive-mcp-swarm-notify "hive-mcp-swarm-notify")
(declare-function hive-mcp-swarm-notify-idle "hive-mcp-swarm-notify")
(declare-function hive-mcp-swarm-notify-prompt-stall "hive-mcp-swarm-notify")
(declare-function hive-mcp-swarm-notify-blocked "hive-mcp-swarm-notify")
(declare-function hive-mcp-swarm-notify-error "hive-mcp-swarm-notify")

(defcustom hive-mcp-olympus-notify-enabled t
  "If non-nil, enable global desktop notifications for stuck lings.
When enabled, Olympus subscribes to swarm events and fires
notifications regardless of which buffer is currently active."
  :type 'boolean
  :group 'hive-mcp-olympus)

(defcustom hive-mcp-olympus-stuck-threshold 120
  "Seconds before a working ling is considered stuck.
Default 120 seconds (2 minutes)."
  :type 'integer
  :group 'hive-mcp-olympus)

(defun hive-mcp-olympus--handle-idle-timeout (event)
  "Handle :idle-timeout EVENT by firing desktop notification."
  (when hive-mcp-olympus-notify-enabled
    (let* ((slave-id (cdr (assoc "slave-id" event)))
           (idle-secs (cdr (assoc "idle-duration-secs" event))))
      (when (and slave-id idle-secs (> idle-secs hive-mcp-olympus-stuck-threshold))
        (when (fboundp 'hive-mcp-swarm-notify-idle)
          (hive-mcp-swarm-notify-idle slave-id idle-secs))))))

(defun hive-mcp-olympus--handle-prompt-stall (event)
  "Handle :prompt-stall EVENT by firing urgent notification."
  (when hive-mcp-olympus-notify-enabled
    (let* ((slave-id (cdr (assoc "slave-id" event)))
           (idle-secs (cdr (assoc "idle-duration-secs" event)))
           (prompt-text (cdr (assoc "prompt-preview" event))))
      (when (and slave-id idle-secs)
        (when (fboundp 'hive-mcp-swarm-notify-prompt-stall)
          (hive-mcp-swarm-notify-prompt-stall slave-id idle-secs prompt-text))))))

(defun hive-mcp-olympus--handle-dispatch-dropped (event)
  "Handle :dispatch-dropped EVENT by firing critical notification."
  (when hive-mcp-olympus-notify-enabled
    (let* ((slave-id (cdr (assoc "slave-id" event)))
           (reason (cdr (assoc "reason" event)))
           (prompt-preview (cdr (assoc "prompt-preview" event))))
      (when slave-id
        (when (fboundp 'hive-mcp-swarm-notify)
          (hive-mcp-swarm-notify
           (format "DISPATCH DROPPED: %s" slave-id)
           (format "%s - %s" reason (or prompt-preview ""))
           'critical))))))

(defun hive-mcp-olympus--handle-auto-error (event)
  "Handle :auto-error EVENT by firing notification."
  (when hive-mcp-olympus-notify-enabled
    (let* ((slave-id (cdr (assoc "slave-id" event)))
           (error-type (cdr (assoc "error-type" event)))
           (error-preview (cdr (assoc "error-preview" event))))
      (when slave-id
        (when (fboundp 'hive-mcp-swarm-notify-error)
          (hive-mcp-swarm-notify-error slave-id error-type error-preview))))))

(defun hive-mcp-olympus--handle-prompt-pending (event)
  "Handle :ling/prompt-pending EVENT by firing urgent notification.
EVENT contains slave-id, prompt-preview, and pending-since.
Wave 2+3: Updates dashboard status and fires urgent desktop notification."
  (when hive-mcp-olympus-notify-enabled
    (let* ((slave-id (cdr (assoc "slave-id" event)))
           (prompt-preview (cdr (assoc "prompt-preview" event)))
           (pending-since (cdr (assoc "pending-since" event))))
      (when slave-id
        ;; Update slave status in local state for dashboard coloring
        (when (and (boundp 'hive-mcp-swarm--slaves)
                   (hash-table-p hive-mcp-swarm--slaves))
          (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves)))
            (puthash slave-id
                     (plist-put slave :status 'prompt-pending)
                     hive-mcp-swarm--slaves)))
        ;; Wave 3: Fire urgent desktop notification
        (when (fboundp 'hive-mcp-swarm-notify)
          (hive-mcp-swarm-notify
           (format "PROMPT PENDING: %s" slave-id)
           (or prompt-preview "Ling waiting for input")
           'urgent))
        ;; Also log to messages
        (message "[Olympus] Prompt pending from %s: %s"
                 slave-id (or prompt-preview "awaiting response"))))))

(defun hive-mcp-olympus--subscribe-notification-events ()
  "Subscribe to swarm events that should trigger global notifications."
  (when (fboundp 'hive-mcp-channel-on)
    ;; Layer 2: Idle timeout (ling went silent)
    (hive-mcp-channel-on :idle-timeout
                          #'hive-mcp-olympus--handle-idle-timeout)
    ;; Layer 2: Prompt stall (ling blocked on unanswered prompt)
    (hive-mcp-channel-on :prompt-stall
                          #'hive-mcp-olympus--handle-prompt-stall)
    ;; Critical: Dispatch dropped (task was lost)
    (hive-mcp-channel-on :dispatch-dropped
                          #'hive-mcp-olympus--handle-dispatch-dropped)
    ;; Error: Task failed with error
    (hive-mcp-channel-on :auto-error
                          #'hive-mcp-olympus--handle-auto-error)
    ;; Reactive prompt detection: ling waiting on prompt response
    (hive-mcp-channel-on :ling/prompt-pending
                          #'hive-mcp-olympus--handle-prompt-pending)))

(defun hive-mcp-olympus--unsubscribe-notification-events ()
  "Unsubscribe from notification events."
  (when (fboundp 'hive-mcp-channel-off)
    (hive-mcp-channel-off :idle-timeout
                           #'hive-mcp-olympus--handle-idle-timeout)
    (hive-mcp-channel-off :prompt-stall
                           #'hive-mcp-olympus--handle-prompt-stall)
    (hive-mcp-channel-off :dispatch-dropped
                           #'hive-mcp-olympus--handle-dispatch-dropped)
    (hive-mcp-channel-off :auto-error
                           #'hive-mcp-olympus--handle-auto-error)
    (hive-mcp-channel-off :ling/prompt-pending
                           #'hive-mcp-olympus--handle-prompt-pending)))

;;; =============================================================================
;;; Background Monitor (Timer-based stuck detection)
;;; =============================================================================

(defvar hive-mcp-olympus--monitor-timer nil
  "Timer for background monitoring of stuck lings.")

(defcustom hive-mcp-olympus-monitor-interval 30
  "Interval in seconds between monitor checks.
Default 30 seconds."
  :type 'integer
  :group 'hive-mcp-olympus)

(defvar hive-mcp-olympus--last-notified (make-hash-table :test 'equal)
  "Hash of ling-id -> timestamp of last notification.
Used to prevent notification spam.")

(defcustom hive-mcp-olympus-notify-cooldown 300
  "Seconds between repeated notifications for the same ling.
Default 5 minutes."
  :type 'integer
  :group 'hive-mcp-olympus)

(defun hive-mcp-olympus--should-notify-p (ling-id)
  "Check if LING-ID should be notified (respects cooldown)."
  (let ((last-time (gethash ling-id hive-mcp-olympus--last-notified 0)))
    (> (- (float-time) last-time) hive-mcp-olympus-notify-cooldown)))

(defun hive-mcp-olympus--mark-notified (ling-id)
  "Mark LING-ID as having been notified."
  (puthash ling-id (float-time) hive-mcp-olympus--last-notified))

(defun hive-mcp-olympus--get-ling-status-for-monitor ()
  "Get ling status data for monitoring.
Returns list of ling plists with :slave-id :name :status :duration."
  (let ((lings nil))
    (when (and (boundp 'hive-mcp-swarm--slaves)
               (hash-table-p hive-mcp-swarm--slaves))
      (maphash
       (lambda (id slave)
         (push (list :slave-id id
                     :name (plist-get slave :name)
                     :status (plist-get slave :status)
                     :current-task (plist-get slave :current-task)
                     :last-activity (plist-get slave :last-activity))
               lings))
       hive-mcp-swarm--slaves))
    lings))

(defun hive-mcp-olympus--monitor-tick ()
  "Check all lings for stuck state and fire notifications.
Called periodically by the monitor timer."
  (condition-case err
      (when hive-mcp-olympus-notify-enabled
        (let ((lings (hive-mcp-olympus--get-ling-status-for-monitor)))
      (dolist (ling lings)
        (let ((slave-id (plist-get ling :slave-id))
              (status (plist-get ling :status))
              (name (plist-get ling :name)))
          ;; Check for stuck states
          (when (and slave-id
                     (hive-mcp-olympus--should-notify-p slave-id))
            (pcase status
              ('blocked
               (when (fboundp 'hive-mcp-swarm-notify-blocked)
                 (hive-mcp-swarm-notify-blocked slave-id "Ling is blocked")
                 (hive-mcp-olympus--mark-notified slave-id)))
              ('error
               (when (fboundp 'hive-mcp-swarm-notify-error)
                 (hive-mcp-swarm-notify-error slave-id "error" "Ling in error state")
                 (hive-mcp-olympus--mark-notified slave-id)))))))))
    (error
     (message "[olympus] Monitor tick error: %s"
              (error-message-string err)))))

;;;###autoload
(defun hive-mcp-olympus-start-monitor ()
  "Start background monitoring for stuck lings.
Monitor runs every `hive-mcp-olympus-monitor-interval' seconds
and fires notifications for stuck/blocked lings."
  (interactive)
  (hive-mcp-olympus-stop-monitor)
  (setq hive-mcp-olympus--monitor-timer
        (run-with-timer
         hive-mcp-olympus-monitor-interval
         hive-mcp-olympus-monitor-interval
         #'hive-mcp-olympus--monitor-tick))
  (message "Olympus monitor started (interval: %ds)" hive-mcp-olympus-monitor-interval))

;;;###autoload
(defun hive-mcp-olympus-stop-monitor ()
  "Stop background monitoring."
  (interactive)
  (when hive-mcp-olympus--monitor-timer
    (cancel-timer hive-mcp-olympus--monitor-timer)
    (setq hive-mcp-olympus--monitor-timer nil)
    (message "Olympus monitor stopped")))

;;; =============================================================================
;;; Unified Dashboard Buffer (tabulated-list-mode)
;;; =============================================================================

(defvar hive-mcp-olympus-dashboard-buffer-name "*Olympus Dashboard*"
  "Name of the Olympus dashboard buffer.")

(defvar hive-mcp-olympus-dashboard-refresh-timer nil
  "Timer for auto-refreshing the dashboard.")

(defcustom hive-mcp-olympus-dashboard-refresh-interval 5
  "Interval in seconds between dashboard auto-refreshes.
Default 5 seconds."
  :type 'integer
  :group 'hive-mcp-olympus)

(defface hive-mcp-olympus-working-face
  '((t :foreground "green" :weight bold))
  "Face for working lings in dashboard."
  :group 'hive-mcp-olympus)

(defface hive-mcp-olympus-idle-face
  '((t :foreground "gray"))
  "Face for idle lings in dashboard."
  :group 'hive-mcp-olympus)

(defface hive-mcp-olympus-stuck-face
  '((t :foreground "red" :weight bold))
  "Face for stuck lings in dashboard."
  :group 'hive-mcp-olympus)

(defface hive-mcp-olympus-blocked-face
  '((t :foreground "yellow" :weight bold))
  "Face for blocked lings in dashboard."
  :group 'hive-mcp-olympus)

(defface hive-mcp-olympus-error-face
  '((t :foreground "red" :background "black" :weight bold))
  "Face for errored lings in dashboard."
  :group 'hive-mcp-olympus)

(defface hive-mcp-olympus-prompt-pending-face
  '((t :foreground "orange" :weight bold))
  "Face for lings waiting on prompt response."
  :group 'hive-mcp-olympus)

(defun hive-mcp-olympus--status-to-face (status duration)
  "Return face for STATUS and DURATION."
  (cond
   ((eq status 'error) 'hive-mcp-olympus-error-face)
   ((eq status 'prompt-pending) 'hive-mcp-olympus-prompt-pending-face)
   ((eq status 'blocked) 'hive-mcp-olympus-blocked-face)
   ((and (eq status 'working) (> (or duration 0) hive-mcp-olympus-stuck-threshold))
    'hive-mcp-olympus-stuck-face)
   ((eq status 'working) 'hive-mcp-olympus-working-face)
   (t 'hive-mcp-olympus-idle-face)))

(defun hive-mcp-olympus--format-duration (start-time)
  "Format duration from START-TIME to now as human-readable string."
  (if start-time
      (let* ((now (float-time))
             (elapsed (- now start-time))
             (mins (floor (/ elapsed 60)))
             (secs (mod (floor elapsed) 60)))
        (if (> mins 0)
            (format "%dm%02ds" mins secs)
          (format "%ds" secs)))
    "-"))

(defun hive-mcp-olympus--get-dashboard-entries ()
  "Get entries for the dashboard tabulated list.
Returns list of (ID [ID NAME STATUS DURATION TASK]) entries."
  (let ((entries nil))
    (when (and (boundp 'hive-mcp-swarm--slaves)
               (hash-table-p hive-mcp-swarm--slaves))
      (maphash
       (lambda (id slave)
         (let* ((name (or (plist-get slave :name) "unnamed"))
                (status (or (plist-get slave :status) 'unknown))
                (status-str (symbol-name status))
                (start-time (plist-get slave :task-start-time))
                (duration-str (hive-mcp-olympus--format-duration start-time))
                (duration-secs (if start-time (- (float-time) start-time) 0))
                (task (or (plist-get slave :current-task) "-"))
                (task-preview (truncate-string-to-width task 40))
                (face (hive-mcp-olympus--status-to-face status duration-secs)))
           (push (list id
                       (vector
                        (propertize (truncate-string-to-width id 25) 'face face)
                        (propertize name 'face face)
                        (propertize status-str 'face face)
                        (propertize duration-str 'face face)
                        (propertize task-preview 'face face)))
                 entries)))
       hive-mcp-swarm--slaves))
    (nreverse entries)))

(defun hive-mcp-olympus-dashboard-refresh ()
  "Refresh the Olympus dashboard buffer."
  (interactive)
  (condition-case err
      (when-let* ((buf (get-buffer hive-mcp-olympus-dashboard-buffer-name)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (setq tabulated-list-entries (hive-mcp-olympus--get-dashboard-entries))
            (tabulated-list-print t))))
    (error
     (message "[olympus] Dashboard refresh error: %s"
              (error-message-string err)))))

(defun hive-mcp-olympus-dashboard-focus-ling ()
  "Focus the ling at point in the dashboard."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (hive-olympus-focus-by-id id)))

(defun hive-mcp-olympus-dashboard-show-buffer ()
  "Switch to the buffer of ling at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (and (boundp 'hive-mcp-swarm--slaves)
               (hash-table-p hive-mcp-swarm--slaves))
      (when-let* ((slave (gethash id hive-mcp-swarm--slaves))
                  (buffer (plist-get slave :buffer)))
        (when (buffer-live-p buffer)
          (switch-to-buffer buffer))))))

(defvar hive-mcp-olympus-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'hive-mcp-olympus-dashboard-refresh)
    (define-key map (kbd "RET") #'hive-mcp-olympus-dashboard-show-buffer)
    (define-key map (kbd "f") #'hive-mcp-olympus-dashboard-focus-ling)
    (define-key map (kbd "o") #'hive-olympus)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `hive-mcp-olympus-dashboard-mode'.")

(define-derived-mode hive-mcp-olympus-dashboard-mode tabulated-list-mode
  "Olympus"
  "Major mode for viewing all lings in a unified dashboard.

\\{hive-mcp-olympus-dashboard-mode-map}"
  (setq tabulated-list-format
        [("ID" 25 t)
         ("Name" 15 t)
         ("Status" 10 t)
         ("Duration" 10 t)
         ("Task" 40 t)])
  (setq tabulated-list-sort-key '("Status" . nil))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  ;; Auto-refresh
  (add-hook 'kill-buffer-hook
            #'hive-mcp-olympus--stop-dashboard-refresh nil t))

(defun hive-mcp-olympus--start-dashboard-refresh ()
  "Start auto-refresh timer for dashboard."
  (hive-mcp-olympus--stop-dashboard-refresh)
  (setq hive-mcp-olympus-dashboard-refresh-timer
        (run-with-timer
         hive-mcp-olympus-dashboard-refresh-interval
         hive-mcp-olympus-dashboard-refresh-interval
         #'hive-mcp-olympus-dashboard-refresh)))

(defun hive-mcp-olympus--stop-dashboard-refresh ()
  "Stop auto-refresh timer for dashboard."
  (when hive-mcp-olympus-dashboard-refresh-timer
    (cancel-timer hive-mcp-olympus-dashboard-refresh-timer)
    (setq hive-mcp-olympus-dashboard-refresh-timer nil)))

;;;###autoload
(defun hive-mcp-olympus-dashboard ()
  "Open the unified Olympus dashboard showing all lings.

The dashboard displays:
- ID: Ling identifier
- Name: Display name
- Status: working/idle/blocked/error (color coded)
- Duration: How long in current state
- Task: Current task preview

Keybindings:
\\{hive-mcp-olympus-dashboard-mode-map}

Colors:
- Green: Working normally
- Gray: Idle
- Orange: Prompt pending (waiting for input)
- Yellow: Blocked (needs attention)
- Red: Stuck (working too long) or Error"
  (interactive)
  (let ((buf (get-buffer-create hive-mcp-olympus-dashboard-buffer-name)))
    (with-current-buffer buf
      (hive-mcp-olympus-dashboard-mode)
      (setq tabulated-list-entries (hive-mcp-olympus--get-dashboard-entries))
      (tabulated-list-print)
      (hive-mcp-olympus--start-dashboard-refresh))
    (switch-to-buffer buf)))

;;; =============================================================================
;;; Enhanced Mode Integration
;;; =============================================================================

;; Override the mode activation to include notification subscriptions
(defun hive-mcp-olympus--enhanced-subscribe ()
  "Subscribe to all events including notifications."
  (hive-mcp-olympus--subscribe-events)
  (hive-mcp-olympus--subscribe-notification-events)
  ;; Start background monitor
  (hive-mcp-olympus-start-monitor))

(defun hive-mcp-olympus--enhanced-unsubscribe ()
  "Unsubscribe from all events."
  (hive-mcp-olympus--unsubscribe-events)
  (hive-mcp-olympus--unsubscribe-notification-events)
  ;; Stop background monitor
  (hive-mcp-olympus-stop-monitor)
  ;; Stop dashboard refresh
  (hive-mcp-olympus--stop-dashboard-refresh))

;; Re-define the mode with enhanced subscriptions
;;;###autoload
(define-minor-mode hive-mcp-olympus-mode
  "Minor mode for Olympus grid view with global notifications.

When enabled:
- Subscribes to swarm events for automatic layout updates
- Fires global desktop notifications for stuck lings
- Runs background monitor to detect stuck states
- Does NOT require visiting ling buffers to receive alerts

\\{hive-mcp-olympus-mode-map}"
  :lighter " Olympus"
  :keymap hive-mcp-olympus-mode-map
  :global t
  (if hive-mcp-olympus-mode
      (progn
        (require 'hive-mcp-swarm nil t)
        (require 'hive-mcp-swarm-notify nil t)
        (hive-mcp-olympus--enhanced-subscribe)
        (message "Olympus mode enabled (notifications: %s, monitor: on)"
                 (if hive-mcp-olympus-notify-enabled "on" "off")))
    (hive-mcp-olympus--enhanced-unsubscribe)
    (setq hive-mcp-olympus--focused-ling nil
          hive-mcp-olympus--current-tab 0
          hive-mcp-olympus--positions nil)
    (message "Olympus mode disabled")))

(provide 'hive-mcp-olympus)
;;; hive-mcp-olympus.el ends here
