;;; hive-mcp-kanban-board.el --- Interactive kanban board UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Interactive kanban board UI for hive-mcp-kanban.
;; Provides a visual, navigable board with keyboard-driven interaction.
;;
;; Features:
;; - Visual column layout (TODO, IN-PROGRESS, IN-REVIEW, DONE)
;; - Keyboard navigation (vim-style and arrow keys)
;; - Quick task movement between columns
;; - Task creation, editing, and deletion
;; - Statistics and progress tracking
;;
;; Keybindings:
;; - n/p or j/k: Navigate tasks
;; - h/l or TAB: Navigate columns
;; - >/<: Move task right/left
;; - RET: Open task in org
;; - c: Create task
;; - d: Delete task
;; - e: Edit task
;; - g: Refresh board
;; - q: Quit

;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'hive-mcp-kanban-protocol)

;; Forward declarations
(declare-function hive-mcp-kanban-list-tasks "hive-mcp-org-kanban")
(declare-function hive-mcp-kanban-create-task "hive-mcp-org-kanban")
(declare-function hive-mcp-kanban-update-task "hive-mcp-org-kanban")
(declare-function hive-mcp-kanban-delete-task "hive-mcp-org-kanban")
(declare-function hive-mcp-kanban-move-task "hive-mcp-org-kanban")
(declare-function hive-mcp-kanban-standalone-load-tasks-by-status "hive-mcp-kanban-standalone")
(declare-function cider-connected-p "cider")
(declare-function cider-nrepl-sync-request:eval "cider")
(declare-function nrepl-dict-get "nrepl-dict")

;; Customization reference
(defvar hive-mcp-kanban-org-file)

;;;; Faces:

(defface hive-mcp-kanban-column-header
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for kanban column headers."
  :group 'hive-mcp-kanban)

(defface hive-mcp-kanban-task
  '((t :inherit default))
  "Face for kanban task cards."
  :group 'hive-mcp-kanban)

(defface hive-mcp-kanban-task-highlight
  '((t :inherit highlight))
  "Face for highlighted kanban task."
  :group 'hive-mcp-kanban)

(defface hive-mcp-kanban-border
  '((t :inherit font-lock-comment-face))
  "Face for kanban board borders."
  :group 'hive-mcp-kanban)

(defface hive-mcp-kanban-stats
  '((t :inherit font-lock-doc-face))
  "Face for kanban statistics."
  :group 'hive-mcp-kanban)

;;;; Buffer-Local State:

(defvar-local hive-mcp-kanban-board--tasks nil
  "Current board tasks organized by column.")

(defvar-local hive-mcp-kanban-board--column-positions nil
  "Alist of (column-name . start-pos) for navigation.")

(defvar-local hive-mcp-kanban-board--task-positions nil
  "Alist of (task-id . (start-pos . end-pos)) for navigation.")

;;;; Keymap:

(defvar hive-mcp-kanban-board-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "n") #'hive-mcp-kanban-board-next-task)
    (define-key map (kbd "p") #'hive-mcp-kanban-board-prev-task)
    (define-key map (kbd "f") #'hive-mcp-kanban-board-next-column)
    (define-key map (kbd "b") #'hive-mcp-kanban-board-prev-column)
    (define-key map (kbd "TAB") #'hive-mcp-kanban-board-next-column)
    (define-key map (kbd "<backtab>") #'hive-mcp-kanban-board-prev-column)
    (define-key map (kbd "j") #'hive-mcp-kanban-board-next-task)
    (define-key map (kbd "k") #'hive-mcp-kanban-board-prev-task)
    (define-key map (kbd "l") #'hive-mcp-kanban-board-next-column)
    (define-key map (kbd "h") #'hive-mcp-kanban-board-prev-column)
    ;; Actions
    (define-key map (kbd "RET") #'hive-mcp-kanban-board-open-task)
    (define-key map (kbd "m") #'hive-mcp-kanban-board-move-task)
    (define-key map (kbd ">") #'hive-mcp-kanban-board-move-right)
    (define-key map (kbd "<") #'hive-mcp-kanban-board-move-left)
    (define-key map (kbd "c") #'hive-mcp-kanban-board-create-task)
    (define-key map (kbd "d") #'hive-mcp-kanban-board-delete-task)
    (define-key map (kbd "e") #'hive-mcp-kanban-board-edit-task)
    ;; View
    (define-key map (kbd "g") #'hive-mcp-kanban-board-refresh)
    (define-key map (kbd "o") #'hive-mcp-kanban-board-open-org-file)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'hive-mcp-kanban-board-help)
    map)
  "Keymap for `hive-mcp-kanban-board-mode'.")

;;;; Major Mode:

(define-derived-mode hive-mcp-kanban-board-mode special-mode "Kanban"
  "Major mode for interactive kanban board.

\\{hive-mcp-kanban-board-mode-map}"
  :group 'hive-mcp-kanban
  (setq-local truncate-lines t)
  (setq-local cursor-type 'bar)
  (setq-local line-spacing 0.2)
  (hl-line-mode 1))

;; Evil-mode integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'hive-mcp-kanban-board-mode 'emacs)
  ;; Also define keys in normal state for users who prefer staying in normal
  (evil-define-key 'normal hive-mcp-kanban-board-mode-map
    "n" #'hive-mcp-kanban-board-next-task
    "p" #'hive-mcp-kanban-board-prev-task
    "j" #'hive-mcp-kanban-board-next-task
    "k" #'hive-mcp-kanban-board-prev-task
    "l" #'hive-mcp-kanban-board-next-column
    "h" #'hive-mcp-kanban-board-prev-column
    (kbd "RET") #'hive-mcp-kanban-board-open-task
    "m" #'hive-mcp-kanban-board-move-task
    ">" #'hive-mcp-kanban-board-move-right
    "<" #'hive-mcp-kanban-board-move-left
    "c" #'hive-mcp-kanban-board-create-task
    "d" #'hive-mcp-kanban-board-delete-task
    "e" #'hive-mcp-kanban-board-edit-task
    "g" #'hive-mcp-kanban-board-refresh
    "o" #'hive-mcp-kanban-board-open-org-file
    "q" #'quit-window
    "?" #'hive-mcp-kanban-board-help))

;;;; Rendering:

(defun hive-mcp-kanban-board--render ()
  "Render the kanban board in the current buffer."
  (let* ((inhibit-read-only t)
         (file hive-mcp-kanban-org-file)
         (tasks-by-status (hive-mcp-kanban-board--load-tasks file))
         (col-width 30)
         (columns '(("TODO" . "📋")
                    ("IN-PROGRESS" . "🔄")
                    ("IN-REVIEW" . "👀")
                    ("DONE" . "✅")))
         (task-positions '())
         (column-positions '()))
    (erase-buffer)

    ;; Header
    (insert (propertize "╔══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗\n"
                        'face 'hive-mcp-kanban-border))
    (insert (propertize "║                                              " 'face 'hive-mcp-kanban-border))
    (insert (propertize " KANBAN BOARD " 'face '(:inherit hive-mcp-kanban-column-header :height 1.3)))
    (insert (propertize "                                              ║\n" 'face 'hive-mcp-kanban-border))
    (insert (propertize "╠══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣\n"
                        'face 'hive-mcp-kanban-border))

    ;; Stats line
    (let* ((total (apply #'+ (mapcar (lambda (col) (length (cdr (assoc (car col) tasks-by-status)))) columns)))
           (done (length (cdr (assoc "DONE" tasks-by-status))))
           (in-prog (length (cdr (assoc "IN-PROGRESS" tasks-by-status))))
           (todo (length (cdr (assoc "TODO" tasks-by-status)))))
      (insert (propertize "║ " 'face 'hive-mcp-kanban-border))
      (insert (propertize (format "Total: %d  │  Todo: %d  │  In Progress: %d  │  Done: %d  │  Progress: %d%%"
                                  total todo in-prog done
                                  (if (> total 0) (round (* 100 (/ (float done) total))) 0))
                          'face 'hive-mcp-kanban-stats))
      (insert (make-string (- 122 (current-column)) ?\s))
      (insert (propertize "║\n" 'face 'hive-mcp-kanban-border)))

    (insert (propertize "╠══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣\n"
                        'face 'hive-mcp-kanban-border))

    ;; Column headers
    (insert (propertize "║" 'face 'hive-mcp-kanban-border))
    (dolist (col columns)
      (let* ((name (car col))
             (emoji (cdr col))
             (count (length (cdr (assoc name tasks-by-status))))
             (header (format " %s %s (%d) " emoji name count)))
        (push (cons name (point)) column-positions)
        (insert (propertize header 'face 'hive-mcp-kanban-column-header
                            'column-name name))
        (insert (make-string (- col-width (length header)) ?\s))))
    (insert (propertize "║\n" 'face 'hive-mcp-kanban-border))

    (insert (propertize "╠" 'face 'hive-mcp-kanban-border))
    (dotimes (_ 4)
      (insert (propertize (make-string col-width ?─) 'face 'hive-mcp-kanban-border)))
    (insert (propertize "╣\n" 'face 'hive-mcp-kanban-border))

    ;; Find max tasks in any column
    (let ((max-tasks (apply #'max 1 (mapcar (lambda (col)
                                              (length (cdr (assoc (car col) tasks-by-status))))
                                            columns))))
      ;; Render task rows
      (dotimes (row (min max-tasks 15))
        (insert (propertize "║" 'face 'hive-mcp-kanban-border))
        (dolist (col columns)
          (let* ((col-name (car col))
                 (col-tasks (cdr (assoc col-name tasks-by-status)))
                 (task (nth row col-tasks)))
            (if task
                (let* ((title (or (cdr (assoc 'title task)) "Untitled"))
                       (id (cdr (assoc 'id task)))
                       (truncated (if (> (length title) (- col-width 4))
                                      (concat (substring title 0 (- col-width 7)) "...")
                                    title))
                       (start-pos (point)))
                  (insert (propertize (format " • %s" truncated)
                                      'face 'hive-mcp-kanban-task
                                      'task-id id
                                      'task-data task
                                      'column-name col-name
                                      'mouse-face 'hive-mcp-kanban-task-highlight
                                      'help-echo (format "Task: %s\nID: %s\nClick to open" title id)))
                  (insert (make-string (- col-width (+ 3 (length truncated))) ?\s))
                  (push (cons id (cons start-pos (point))) task-positions))
              (insert (make-string col-width ?\s)))))
        (insert (propertize "║\n" 'face 'hive-mcp-kanban-border)))

      ;; Show "more" indicator if needed
      (dolist (col columns)
        (let* ((col-name (car col))
               (col-tasks (cdr (assoc col-name tasks-by-status)))
               (extra (- (length col-tasks) 15)))
          (when (> extra 0)
            (insert (propertize "║" 'face 'hive-mcp-kanban-border))
            (dolist (c columns)
              (if (equal (car c) col-name)
                  (let ((more-text (format " ... +%d more" extra)))
                    (insert (propertize more-text 'face 'font-lock-comment-face))
                    (insert (make-string (- col-width (length more-text)) ?\s)))
                (insert (make-string col-width ?\s))))
            (insert (propertize "║\n" 'face 'hive-mcp-kanban-border))))))

    ;; Footer
    (insert (propertize "╠══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣\n"
                        'face 'hive-mcp-kanban-border))
    (insert (propertize "║ " 'face 'hive-mcp-kanban-border))
    (insert (propertize "[n/p] Navigate  [h/l] Columns  [RET] Open  [>/<] Move  [c] Create  [d] Delete  [g] Refresh  [q] Quit  [?] Help"
                        'face 'font-lock-comment-face))
    (insert (make-string (- 122 (current-column)) ?\s))
    (insert (propertize "║\n" 'face 'hive-mcp-kanban-border))
    (insert (propertize "╚══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝\n"
                        'face 'hive-mcp-kanban-border))

    ;; Store state
    (setq hive-mcp-kanban-board--tasks tasks-by-status)
    (setq hive-mcp-kanban-board--column-positions (nreverse column-positions))
    (setq hive-mcp-kanban-board--task-positions (nreverse task-positions))

    ;; Position cursor on first task
    (goto-char (point-min))
    (hive-mcp-kanban-board-next-task)))

;;;; Task Loading:

(defun hive-mcp-kanban-board--load-tasks (file)
  "Load tasks from FILE grouped by status."
  (if (and (fboundp 'cider-connected-p) (cider-connected-p))
      ;; Use Clojure renderer
      (hive-mcp-kanban-board--load-tasks-cider file)
    ;; Fallback to org-mode parsing
    (hive-mcp-kanban-standalone-load-tasks-by-status file)))

(defun hive-mcp-kanban-board--load-tasks-cider (file)
  "Load tasks from FILE using CIDER/Clojure."
  (let* ((code (format "(do (require '[hive-mcp.org-clj.parser :as p])
                           (require '[hive-mcp.org-clj.query :as q])
                           (let [doc (p/parse-document (slurp \"%s\"))]
                             {:todo (q/find-todo doc)
                              :in-progress (q/find-in-progress doc)
                              :in-review (q/find-by-status doc \"IN-REVIEW\")
                              :done (q/find-done doc)}))" file))
         (result (cider-nrepl-sync-request:eval code))
         (value (nrepl-dict-get result "value")))
    (when value
      (let ((data (read value)))
        `(("TODO" . ,(mapcar #'hive-mcp-kanban-board--convert-task (plist-get data :todo)))
          ("IN-PROGRESS" . ,(mapcar #'hive-mcp-kanban-board--convert-task (plist-get data :in-progress)))
          ("IN-REVIEW" . ,(mapcar #'hive-mcp-kanban-board--convert-task (plist-get data :in-review)))
          ("DONE" . ,(mapcar #'hive-mcp-kanban-board--convert-task (plist-get data :done))))))))

(defun hive-mcp-kanban-board--convert-task (task-plist)
  "Convert TASK-PLIST from Clojure to alist."
  `((id . ,(plist-get task-plist :id))
    (title . ,(plist-get task-plist :title))
    (status . ,(plist-get task-plist :status))
    (level . ,(plist-get task-plist :level))))

;;;; Navigation Commands:

(defun hive-mcp-kanban-board-next-task ()
  "Move to next task in board."
  (interactive)
  (let ((next-pos nil))
    (save-excursion
      (forward-char 1)
      (while (and (not (eobp))
                  (not (get-text-property (point) 'task-id)))
        (forward-char 1))
      (when (get-text-property (point) 'task-id)
        (setq next-pos (point))))
    (when next-pos
      (goto-char next-pos))))

(defun hive-mcp-kanban-board-prev-task ()
  "Move to previous task in board."
  (interactive)
  (let ((prev-pos nil))
    (save-excursion
      (backward-char 1)
      (while (and (not (bobp))
                  (not (get-text-property (point) 'task-id)))
        (backward-char 1))
      (when (get-text-property (point) 'task-id)
        (setq prev-pos (point))))
    (when prev-pos
      (goto-char prev-pos))))

(defun hive-mcp-kanban-board-next-column ()
  "Move to next column."
  (interactive)
  (let* ((current-col (get-text-property (point) 'column-name))
         (cols '("TODO" "IN-PROGRESS" "IN-REVIEW" "DONE"))
         (next-col (cadr (member current-col cols))))
    (when next-col
      ;; Find first task in next column
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (get-text-property (point) 'column-name) next-col)))
        (forward-char 1))
      (when (equal (get-text-property (point) 'column-name) next-col)
        (point)))))

(defun hive-mcp-kanban-board-prev-column ()
  "Move to previous column."
  (interactive)
  (let* ((current-col (get-text-property (point) 'column-name))
         (cols '("DONE" "IN-REVIEW" "IN-PROGRESS" "TODO"))
         (prev-col (cadr (member current-col cols))))
    (when prev-col
      ;; Find first task in prev column
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (get-text-property (point) 'column-name) prev-col)))
        (forward-char 1))
      (when (equal (get-text-property (point) 'column-name) prev-col)
        (point)))))

;;;; Action Commands:

(defun hive-mcp-kanban-board-open-task ()
  "Open task at point in org file."
  (interactive)
  (when-let* ((id (get-text-property (point) 'task-id)))
    (let ((marker (org-id-find id 'marker)))
      (when marker
        (switch-to-buffer (marker-buffer marker))
        (goto-char marker)
        (if (fboundp 'org-fold-show-entry)
            (org-fold-show-entry)
          (with-no-warnings (org-show-entry)))))))

(defun hive-mcp-kanban-board-move-right ()
  "Move task at point to next status column."
  (interactive)
  (hive-mcp-kanban-board--move-task 1))

(defun hive-mcp-kanban-board-move-left ()
  "Move task at point to previous status column."
  (interactive)
  (hive-mcp-kanban-board--move-task -1))

(defun hive-mcp-kanban-board--move-task (direction)
  "Move task at point in DIRECTION (+1 or -1).
Uses the addon's unified API for auto-sync and agent tracking."
  (when-let* ((id (get-text-property (point) 'task-id))
              (current-col (get-text-property (point) 'column-name)))
    (let* ((cols '("TODO" "IN-PROGRESS" "IN-REVIEW" "DONE"))
           (current-idx (cl-position current-col cols :test #'equal))
           (new-idx (+ current-idx direction))
           (new-col (and (>= new-idx 0) (< new-idx 4) (nth new-idx cols))))
      (when new-col
        ;; Use addon API - handles auto-sync and agent tracking
        (let ((vibe-status (hive-mcp-kanban--org-to-vibe-status new-col)))
          (hive-mcp-kanban-move-task id vibe-status))
        (hive-mcp-kanban-board-refresh)
        (message "Moved task to %s" new-col)))))

(defun hive-mcp-kanban-board-move-task ()
  "Move task at point to a new status (interactive).
Uses the addon's unified API for auto-sync and agent tracking."
  (interactive)
  (when-let* ((id (get-text-property (point) 'task-id)))
    (let* ((new-status (completing-read "Move to: "
                                        '("TODO" "IN-PROGRESS" "IN-REVIEW" "DONE")
                                        nil t))
           (vibe-status (hive-mcp-kanban--org-to-vibe-status new-status)))
      ;; Use addon API - handles auto-sync and agent tracking
      (hive-mcp-kanban-move-task id vibe-status)
      (hive-mcp-kanban-board-refresh)
      (message "Moved task to %s" new-status))))

(defun hive-mcp-kanban-board-create-task ()
  "Create a new task."
  (interactive)
  (let ((title (read-string "Task title: ")))
    (hive-mcp-kanban-create-task title)
    (hive-mcp-kanban-board-refresh)
    (message "Created task: %s" title)))

(defun hive-mcp-kanban-board-delete-task ()
  "Delete task at point.
Uses the addon's unified API for auto-sync and agent tracking."
  (interactive)
  (when-let* ((id (get-text-property (point) 'task-id))
              (task (get-text-property (point) 'task-data)))
    (when (yes-or-no-p (format "Delete task '%s'? " (cdr (assoc 'title task))))
      ;; Use addon API - handles deletion in current backend
      (hive-mcp-kanban-delete-task id)
      (hive-mcp-kanban-board-refresh)
      (message "Task deleted"))))

(defun hive-mcp-kanban-board-edit-task ()
  "Edit task title at point.
Uses the addon's unified API for auto-sync and agent tracking."
  (interactive)
  (when-let* ((id (get-text-property (point) 'task-id))
              (task (get-text-property (point) 'task-data)))
    (let* ((old-title (cdr (assoc 'title task)))
           (new-title (read-string "New title: " old-title)))
      (unless (string= old-title new-title)
        ;; Use addon API - handles auto-sync and agent tracking
        (hive-mcp-kanban-update-task id :title new-title)
        (hive-mcp-kanban-board-refresh)
        (message "Task updated")))))

(defun hive-mcp-kanban-board-refresh ()
  "Refresh the kanban board."
  (interactive)
  (when (eq major-mode 'hive-mcp-kanban-board-mode)
    (let ((pos (point)))
      (hive-mcp-kanban-board--render)
      (goto-char (min pos (point-max))))))

(defun hive-mcp-kanban-board-open-org-file ()
  "Open the kanban org file."
  (interactive)
  (find-file hive-mcp-kanban-org-file))

(defun hive-mcp-kanban-board-help ()
  "Show help for kanban board."
  (interactive)
  (message "Kanban: n/p=nav tasks, h/l=nav cols, RET=open, >/<= move, c=create, d=delete, g=refresh, q=quit"))

;;;; Entry Point:

;;;###autoload
(defun hive-mcp-kanban-open-board ()
  "Open interactive kanban board buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Kanban Board*")))
    (with-current-buffer buf
      (hive-mcp-kanban-board-mode)
      (hive-mcp-kanban-board--render))
    (switch-to-buffer buf)))

(provide 'hive-mcp-kanban-board)
;;; hive-mcp-kanban-board.el ends here
