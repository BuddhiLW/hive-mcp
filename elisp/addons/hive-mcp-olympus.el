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
Returns list of (ling-id . buffer) pairs."
  ;; TODO: Integrate with hive-mcp-swarm to get actual ling buffers
  ;; Stub returns empty for now
  nil)

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
  "Arrange LING-BUFFERS in ROWS x COLS grid."
  ;; TODO: Implement grid arrangement
  ;; Stub: just show first buffer
  (when-let ((first-buf (cdar ling-buffers)))
    (set-window-buffer (selected-window) first-buf)))

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

\\{hive-mcp-olympus-mode-map}"
  :lighter " Olympus"
  :keymap hive-mcp-olympus-mode-map
  :global t)

;;; =============================================================================
;;; Event Handlers (Channel Integration)
;;; =============================================================================

(defun hive-mcp-olympus--handle-layout-changed (event)
  "Handle :olympus/layout-changed EVENT from MCP.
Triggers window rearrangement."
  ;; TODO: Parse event and apply layout
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

(provide 'hive-mcp-olympus)
;;; hive-mcp-olympus.el ends here
