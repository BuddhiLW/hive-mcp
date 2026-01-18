;;; hive-mcp-olympus.el --- Swarm grid view for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW)
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Olympus provides a grid view for managing multiple ling buffers.
;; It arranges ling terminal buffers in an optimal grid layout and
;; provides keybindings for quick navigation.
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
;; - C-c h 1-4 : Focus ling at position (hive-olympus-focus)
;; - C-c h n   : Next tab (hive-olympus-tab-next)
;; - C-c h p   : Previous tab (hive-olympus-tab-prev)
;; - C-c h r   : Restore grid from focused view

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
    (define-key map (kbd "C-c h 1") (lambda () (interactive) (hive-olympus-focus 1)))
    (define-key map (kbd "C-c h 2") (lambda () (interactive) (hive-olympus-focus 2)))
    (define-key map (kbd "C-c h 3") (lambda () (interactive) (hive-olympus-focus 3)))
    (define-key map (kbd "C-c h 4") (lambda () (interactive) (hive-olympus-focus 4)))
    (define-key map (kbd "C-c h n") #'hive-olympus-tab-next)
    (define-key map (kbd "C-c h p") #'hive-olympus-tab-prev)
    (define-key map (kbd "C-c h r") #'hive-olympus-restore)
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
                 (when (and hive-mcp-olympus-mode
                            (not hive-mcp-olympus--focused-ling))
                   (hive-olympus)))))

(provide 'hive-mcp-olympus)
;;; hive-mcp-olympus.el ends here
