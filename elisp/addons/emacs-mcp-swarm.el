;;; emacs-mcp-swarm.el --- Claude swarm orchestration for parallel task execution -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Optional-Requires: vterm OR eat (one terminal emulator required)
;; Keywords: tools, ai, claude, orchestration, parallel
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Swarm orchestration for Claude Code instances.
;;
;; Enables a Master Claude to spawn and control multiple Slave Claude
;; instances for parallel task execution.  Each slave runs in a terminal
;; buffer (vterm or eat) and can be configured with presets (system
;; prompts loaded from markdown files).
;;
;; Terminal choice:
;;   - vterm (recommended): Native terminal, reliable input handling
;;   - eat (experimental): Pure Emacs Lisp, may have input submission issues
;;
;; Set via: (setq emacs-mcp-swarm-terminal 'vterm)  ; default
;;
;; Architecture:
;;
;;   Master Claude (you)
;;         │ MCP tools
;;         v
;;   emacs-mcp-swarm.el (this file)
;;         │ vterm-send-string
;;         v
;;   ┌─────┴─────┐
;;   │  Slaves   │ (vterm buffers running `claude`)
;;   └───────────┘
;;
;; Features:
;; - Spawn slaves with roles/presets
;; - Dispatch prompts to slaves
;; - Collect responses
;; - Load presets from markdown files
;; - Custom preset directories (recursive .md scan)
;; - Combine multiple presets per slave
;;
;; Usage:
;;   ;; Load the addon
;;   (require 'emacs-mcp-swarm)
;;   (emacs-mcp-swarm-mode 1)
;;
;;   ;; Spawn a slave with preset
;;   (emacs-mcp-swarm-spawn "tester" :presets '("tdd" "clarity"))
;;
;;   ;; Dispatch a task
;;   (emacs-mcp-swarm-dispatch "swarm-tester-xxx" "Run all tests")
;;
;;   ;; Check status
;;   (emacs-mcp-swarm-status)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;; Soft dependency on vterm
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

;; Soft dependency on eat
(declare-function eat "eat")
(declare-function eat-mode "eat")
(declare-function eat-exec "eat")
(declare-function eat-term-send-string "eat")

;;;; Customization:

(defgroup emacs-mcp-swarm nil
  "Claude swarm orchestration."
  :group 'emacs-mcp
  :prefix "emacs-mcp-swarm-")

(defcustom emacs-mcp-swarm-presets-dir
  (expand-file-name "presets" (file-name-directory
                               (or load-file-name buffer-file-name
                                   default-directory)))
  "Directory containing built-in preset markdown files."
  :type 'directory
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-custom-presets-dirs nil
  "List of custom directories to scan for preset .md files.
Directories are scanned recursively for .md files only."
  :type '(repeat directory)
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-claude-command "claude"
  "Command to invoke Claude Code CLI."
  :type 'string
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-terminal 'vterm
  "Terminal emulator to use for slave sessions.
- `vterm': Use vterm (requires native compilation) - RECOMMENDED
- `eat': Use eat (pure Emacs Lisp) - EXPERIMENTAL, may have input issues"
  :type '(choice (const :tag "vterm (recommended)" vterm)
                 (const :tag "eat (experimental)" eat))
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-max-slaves 5
  "Maximum number of concurrent slave instances."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-max-depth 2
  "Maximum recursion depth (slaves spawning slaves)."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-default-timeout 300000
  "Default task timeout in milliseconds (5 minutes)."
  :type 'integer
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-prompt-marker "❯"
  "Marker indicating Claude is ready for input."
  :type 'string
  :group 'emacs-mcp-swarm)

(defcustom emacs-mcp-swarm-buffer-prefix "*swarm-"
  "Prefix for swarm buffer names."
  :type 'string
  :group 'emacs-mcp-swarm)

;;;; Internal State:

(defvar emacs-mcp-swarm--slaves (make-hash-table :test 'equal)
  "Hash table of slave-id -> slave plist.")

(defvar emacs-mcp-swarm--tasks (make-hash-table :test 'equal)
  "Hash table of task-id -> task plist.")

(defvar emacs-mcp-swarm--presets-cache nil
  "Cache of loaded presets (name -> content).")

(defvar emacs-mcp-swarm--task-counter 0
  "Counter for generating unique task IDs.")

(defvar emacs-mcp-swarm--session-id nil
  "Current swarm session ID.")

(defvar emacs-mcp-swarm--current-depth 0
  "Current recursion depth (set via environment).")

;;;; Preset Management:

(defun emacs-mcp-swarm--scan-presets-dir (dir)
  "Recursively scan DIR for .md files, return alist of (name . path)."
  (when (and dir (file-directory-p dir))
    (let ((files (directory-files-recursively dir "\\.md$" nil)))
      (mapcar (lambda (f)
                (cons (file-name-sans-extension (file-name-nondirectory f)) f))
              files))))

(defun emacs-mcp-swarm--load-all-presets ()
  "Load all presets from built-in and custom directories.
Returns hash-table of name -> content."
  (let ((presets (make-hash-table :test 'equal)))
    ;; Built-in presets
    (dolist (entry (emacs-mcp-swarm--scan-presets-dir emacs-mcp-swarm-presets-dir))
      (puthash (car entry) (cdr entry) presets))
    ;; Custom presets (can override built-in)
    (dolist (dir emacs-mcp-swarm-custom-presets-dirs)
      (dolist (entry (emacs-mcp-swarm--scan-presets-dir dir))
        (puthash (car entry) (cdr entry) presets)))
    presets))

(defun emacs-mcp-swarm-reload-presets ()
  "Reload all presets from disk."
  (interactive)
  (setq emacs-mcp-swarm--presets-cache (emacs-mcp-swarm--load-all-presets))
  (message "Loaded %d presets" (hash-table-count emacs-mcp-swarm--presets-cache)))

(defun emacs-mcp-swarm-list-presets ()
  "List all available presets."
  (interactive)
  (unless emacs-mcp-swarm--presets-cache
    (emacs-mcp-swarm-reload-presets))
  (let ((names (hash-table-keys emacs-mcp-swarm--presets-cache)))
    (if (called-interactively-p 'any)
        (message "Available presets: %s" (string-join (sort names #'string<) ", "))
      names)))

(defun emacs-mcp-swarm--get-preset-content (name)
  "Get content of preset NAME."
  (unless emacs-mcp-swarm--presets-cache
    (emacs-mcp-swarm-reload-presets))
  (when-let* ((path (gethash name emacs-mcp-swarm--presets-cache)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun emacs-mcp-swarm--build-system-prompt (presets)
  "Build combined system prompt from list of PRESETS."
  (let ((contents '()))
    (dolist (preset presets)
      (when-let* ((content (emacs-mcp-swarm--get-preset-content preset)))
        (push content contents)))
    (if contents
        (mapconcat #'identity (nreverse contents) "\n\n---\n\n")
      nil)))

(defun emacs-mcp-swarm-add-custom-presets-dir (dir)
  "Add DIR to custom preset directories and reload."
  (interactive "DPresets directory: ")
  (add-to-list 'emacs-mcp-swarm-custom-presets-dirs dir)
  (emacs-mcp-swarm-reload-presets))

;;;; Slave Management:

(defun emacs-mcp-swarm--generate-slave-id (name)
  "Generate unique slave ID for NAME."
  (format "swarm-%s-%d" name (floor (float-time))))

(defun emacs-mcp-swarm--generate-task-id (slave-id)
  "Generate unique task ID for SLAVE-ID."
  (cl-incf emacs-mcp-swarm--task-counter)
  (format "task-%s-%03d"
          (replace-regexp-in-string "^swarm-" "" slave-id)
          emacs-mcp-swarm--task-counter))

(defun emacs-mcp-swarm--check-depth ()
  "Check if we can spawn at current depth."
  (let ((depth (string-to-number (or (getenv "CLAUDE_SWARM_DEPTH") "0"))))
    (setq emacs-mcp-swarm--current-depth depth)
    (when (>= depth emacs-mcp-swarm-max-depth)
      (error "Maximum swarm depth (%d) reached. Cannot spawn more slaves"
             emacs-mcp-swarm-max-depth))))

(defun emacs-mcp-swarm--check-slave-limit ()
  "Check if we can spawn more slaves."
  (when (>= (hash-table-count emacs-mcp-swarm--slaves) emacs-mcp-swarm-max-slaves)
    (error "Maximum slave count (%d) reached" emacs-mcp-swarm-max-slaves)))

(cl-defun emacs-mcp-swarm-spawn (name &key presets cwd role)
  "Spawn a new Claude slave with NAME.

PRESETS is a list of preset names to apply (e.g., '(\"tdd\" \"clarity\")).
CWD is the working directory (defaults to current project root).
ROLE is a predefined role that maps to presets.

Returns the slave-id."
  (interactive
   (list (read-string "Slave name: ")
         :presets (completing-read-multiple
                   "Presets: "
                   (emacs-mcp-swarm-list-presets))))
  ;; Safety checks
  (emacs-mcp-swarm--check-depth)
  (emacs-mcp-swarm--check-slave-limit)

  ;; Resolve role to presets if provided
  (when (and role (not presets))
    (setq presets (emacs-mcp-swarm--role-to-presets role)))

  (let* ((slave-id (emacs-mcp-swarm--generate-slave-id name))
         (buffer-name (format "%s%s*" emacs-mcp-swarm-buffer-prefix name))
         (work-dir (or cwd (emacs-mcp-swarm--project-root) default-directory))
         (system-prompt (emacs-mcp-swarm--build-system-prompt presets))
         buffer process)

    ;; Require terminal emulator
    (pcase emacs-mcp-swarm-terminal
      ('vterm (unless (require 'vterm nil t)
                (error "Vterm is required but not available")))
      ('eat (unless (require 'eat nil t)
              (error "Eat is required but not available"))))

    ;; Create terminal buffer
    (setq buffer (generate-new-buffer buffer-name))
    (let ((default-directory work-dir)
          (process-environment
           (append
            (list (format "CLAUDE_SWARM_DEPTH=%d" (1+ emacs-mcp-swarm--current-depth))
                  (format "CLAUDE_SWARM_MASTER=%s" (or emacs-mcp-swarm--session-id "direct"))
                  (format "CLAUDE_SWARM_SLAVE_ID=%s" slave-id))
            process-environment))
          (claude-cmd (if system-prompt
                          (let ((prompt-file (make-temp-file "swarm-prompt-" nil ".md")))
                            (with-temp-file prompt-file
                              (insert system-prompt))
                            (format "cd %s && %s --system-prompt %s"
                                    (shell-quote-argument work-dir)
                                    emacs-mcp-swarm-claude-command
                                    (shell-quote-argument prompt-file)))
                        (format "cd %s && %s"
                                (shell-quote-argument work-dir)
                                emacs-mcp-swarm-claude-command))))

      (pcase emacs-mcp-swarm-terminal
        ('vterm
         (with-current-buffer buffer
           (vterm-mode)
           (run-at-time 0.5 nil
                        (lambda ()
                          (with-current-buffer buffer
                            (vterm-send-string claude-cmd)
                            (vterm-send-return))))))
        ('eat
         ;; Use eat-exec to start shell in our buffer
         (with-current-buffer buffer
           (eat-mode)
           (eat-exec buffer "swarm-shell" "/bin/bash" nil '("-l")))
         (run-at-time 0.5 nil
                      (lambda ()
                        (with-current-buffer buffer
                          (when (and (boundp 'eat-terminal) eat-terminal)
                            (eat-term-send-string eat-terminal claude-cmd)
                            ;; Use carriage return for eat terminals
                            (eat-term-send-string eat-terminal "\r"))))))))

    ;; Store slave state
    (puthash slave-id
             (list :slave-id slave-id
                   :name name
                   :role role
                   :presets presets
                   :status 'starting
                   :buffer buffer
                   :cwd work-dir
                   :current-task nil
                   :task-queue '()
                   :tasks-completed 0
                   :tasks-failed 0
                   :spawned-at (format-time-string "%FT%TZ")
                   :last-activity (format-time-string "%FT%TZ"))
             emacs-mcp-swarm--slaves)

    ;; Schedule status check
    (run-at-time
     3 nil
     (lambda ()
       (when-let* ((slave (gethash slave-id emacs-mcp-swarm--slaves)))
         (plist-put slave :status 'idle))))

    (when (called-interactively-p 'any)
      (message "Spawned slave: %s" slave-id)
      (display-buffer buffer))

    slave-id))

(defun emacs-mcp-swarm--role-to-presets (role)
  "Convert ROLE to list of preset names."
  (pcase role
    ("tester" '("tester" "tdd"))
    ("reviewer" '("reviewer" "solid" "clarity"))
    ("documenter" '("documenter"))
    ("refactorer" '("refactorer" "solid" "clarity"))
    ("researcher" '("researcher"))
    ("fixer" '("fixer" "tdd"))
    ("clarity-dev" '("clarity" "solid" "ddd" "tdd"))
    (_ (list role))))

(defun emacs-mcp-swarm--project-root ()
  "Get current project root."
  (or (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun emacs-mcp-swarm-kill (slave-id)
  "Kill slave SLAVE-ID."
  (interactive
   (list (completing-read "Kill slave: "
                          (hash-table-keys emacs-mcp-swarm--slaves))))
  (when-let* ((slave (gethash slave-id emacs-mcp-swarm--slaves)))
    (let ((buffer (plist-get slave :buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (remhash slave-id emacs-mcp-swarm--slaves)
    (message "Killed slave: %s" slave-id)))

(defun emacs-mcp-swarm-kill-all ()
  "Kill all slaves."
  (interactive)
  (let ((count 0))
    (maphash (lambda (id _)
               (emacs-mcp-swarm-kill id)
               (cl-incf count))
             emacs-mcp-swarm--slaves)
    (message "Killed %d slaves" count)))

;;;; Task Dispatch and Collection:

(cl-defun emacs-mcp-swarm-dispatch (slave-id prompt &key timeout priority context)
  "Dispatch PROMPT to SLAVE-ID.

TIMEOUT is milliseconds (default: `emacs-mcp-swarm-default-timeout').
PRIORITY is one of: critical, high, normal, low.
CONTEXT is additional context plist.

Returns task-id."
  (interactive
   (list (completing-read "Slave: " (hash-table-keys emacs-mcp-swarm--slaves))
         (read-string "Prompt: ")))

  (let* ((slave (gethash slave-id emacs-mcp-swarm--slaves))
         (task-id (emacs-mcp-swarm--generate-task-id slave-id))
         (buffer (plist-get slave :buffer)))

    (unless slave
      (error "Slave not found: %s" slave-id))

    (unless (buffer-live-p buffer)
      (error "Slave buffer is dead: %s" slave-id))

    ;; Create task record
    (puthash task-id
             (list :task-id task-id
                   :slave-id slave-id
                   :prompt prompt
                   :status 'dispatched
                   :priority (or priority 'normal)
                   :timeout (or timeout emacs-mcp-swarm-default-timeout)
                   :context context
                   :dispatched-at (format-time-string "%FT%TZ")
                   :completed-at nil
                   :result nil
                   :error nil)
             emacs-mcp-swarm--tasks)

    ;; Update slave state
    (plist-put slave :status 'working)
    (plist-put slave :current-task task-id)
    (plist-put slave :last-activity (format-time-string "%FT%TZ"))
    (plist-put slave :task-start-point
               (with-current-buffer buffer (point-max)))

    ;; Send prompt to slave (terminal-agnostic)
    (with-current-buffer buffer
      (goto-char (point-max))
      (pcase emacs-mcp-swarm-terminal
        ('vterm
         (vterm-send-string prompt)
         (vterm-send-return))
        ('eat
         (if (and (boundp 'eat-terminal) eat-terminal)
             (progn
               (eat-term-send-string eat-terminal prompt)
               ;; Use carriage return for eat terminals
               (eat-term-send-string eat-terminal "\r"))
           (error "Eat-terminal not available in buffer %s" (buffer-name buffer))))))

    (when (called-interactively-p 'any)
      (message "Dispatched task %s to %s" task-id slave-id))

    task-id))

(defun emacs-mcp-swarm-collect (task-id &optional timeout-ms)
  "Collect response for TASK-ID.

TIMEOUT-MS is how long to wait (default: 5000ms).
Returns the task plist with :result populated."
  (interactive
   (list (completing-read "Task: " (hash-table-keys emacs-mcp-swarm--tasks))))

  (let* ((task (gethash task-id emacs-mcp-swarm--tasks))
         (slave-id (plist-get task :slave-id))
         (slave (gethash slave-id emacs-mcp-swarm--slaves))
         (buffer (plist-get slave :buffer))
         (prompt (plist-get task :prompt))
         (timeout (/ (or timeout-ms 5000) 1000.0))
         (start-time (float-time))
         result)

    (unless task
      (error "Task not found: %s" task-id))

    ;; Wait for completion by searching for prompt text + response marker
    ;; vterm buffers have complex structure, so we search for our prompt
    ;; then look for Claude's response marker (●) after it
    (while (and (< (- (float-time) start-time) timeout)
                (not result))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            ;; Find our prompt in the buffer (use first 50 chars as search key)
            (let ((search-key (substring prompt 0 (min 50 (length prompt)))))
              (when (search-forward search-key nil t)
                ;; Found our prompt, now look for response marker
                (when (search-forward "●" nil t)
                  (let ((response-start (point)))
                    ;; Find end: next prompt "> " or horizontal line or double newline
                    (cond
                     ((search-forward "\n> " nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 3)))))
                     ((search-forward "\n────" nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 5)))))
                     ((search-forward "\n\n\n" nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 3)))))))))))))
      (unless result
        (sleep-for 0.5)))

    ;; Update task
    (if result
        (progn
          (plist-put task :status 'completed)
          (plist-put task :result result)
          (plist-put task :completed-at (format-time-string "%FT%TZ"))
          ;; Update slave stats
          (plist-put slave :status 'idle)
          (plist-put slave :current-task nil)
          (plist-put slave :tasks-completed (1+ (plist-get slave :tasks-completed))))
      (plist-put task :status 'timeout)
      (plist-put task :error "Collection timed out"))

    (when (called-interactively-p 'any)
      (if result
          (message "Collected result (%d chars)" (length result))
        (message "Collection timed out")))

    task))

(defun emacs-mcp-swarm-broadcast (prompt &optional slave-filter)
  "Send PROMPT to all slaves (or those matching SLAVE-FILTER).
SLAVE-FILTER is a plist like (:role \"tester\").
Returns list of task-ids."
  (let ((task-ids '()))
    (maphash
     (lambda (slave-id slave)
       (when (or (not slave-filter)
                 (emacs-mcp-swarm--slave-matches-filter slave slave-filter))
         (push (emacs-mcp-swarm-dispatch slave-id prompt) task-ids)))
     emacs-mcp-swarm--slaves)
    (nreverse task-ids)))

(defun emacs-mcp-swarm--slave-matches-filter (slave filter)
  "Check if SLAVE matches FILTER criteria."
  (let ((matches t))
    (when-let* ((role (plist-get filter :role)))
      (unless (equal (plist-get slave :role) role)
        (setq matches nil)))
    (when-let* ((status (plist-get filter :status)))
      (unless (eq (plist-get slave :status) status)
        (setq matches nil)))
    matches))

;;;; Status and Monitoring:

(defun emacs-mcp-swarm-status (&optional slave-id)
  "Get swarm status.
If SLAVE-ID is provided, get that slave's status.
Otherwise return aggregate status."
  (interactive)
  (if slave-id
      (gethash slave-id emacs-mcp-swarm--slaves)
    (let ((total 0) (idle 0) (working 0) (error-count 0)
          (slaves-detail '()))
      (maphash
       (lambda (id slave)
         (cl-incf total)
         (pcase (plist-get slave :status)
           ('idle (cl-incf idle))
           ('working (cl-incf working))
           ('error (cl-incf error-count)))
         (push (list :slave-id id
                     :name (plist-get slave :name)
                     :status (plist-get slave :status)
                     :current-task (plist-get slave :current-task)
                     :tasks-completed (plist-get slave :tasks-completed))
               slaves-detail))
       emacs-mcp-swarm--slaves)

      (let ((status `(:session-id ,emacs-mcp-swarm--session-id
                      :status ,(if (> total 0) "active" "inactive")
                      :slaves (:total ,total :idle ,idle :working ,working :error ,error-count)
                      :tasks (:total ,(hash-table-count emacs-mcp-swarm--tasks))
                      :slaves-detail ,(nreverse slaves-detail))))
        (when (called-interactively-p 'any)
          (message "Swarm: %d slaves (%d idle, %d working), %d tasks"
                   total idle working (hash-table-count emacs-mcp-swarm--tasks)))
        status))))

(defun emacs-mcp-swarm-show-slave (slave-id)
  "Switch to buffer for SLAVE-ID."
  (interactive
   (list (completing-read "Show slave: "
                          (hash-table-keys emacs-mcp-swarm--slaves))))
  (when-let* ((slave (gethash slave-id emacs-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer)
      (message "Slave buffer is dead: %s" slave-id))))

;;;; API for MCP Tools:

(defun emacs-mcp-swarm-api-spawn (name presets &optional cwd)
  "API: Spawn slave NAME with PRESETS in CWD."
  (emacs-mcp-swarm-spawn name :presets presets :cwd cwd))

(defun emacs-mcp-swarm-api-dispatch (slave-id prompt &optional timeout-ms)
  "API: Dispatch PROMPT to SLAVE-ID with TIMEOUT-MS."
  (emacs-mcp-swarm-dispatch slave-id prompt :timeout timeout-ms))

(defun emacs-mcp-swarm-api-status ()
  "API: Get swarm status as JSON-serializable plist."
  (emacs-mcp-swarm-status))

(defun emacs-mcp-swarm-api-collect (task-id &optional timeout-ms)
  "API: Collect result for TASK-ID with optional TIMEOUT-MS."
  (let ((task (emacs-mcp-swarm-collect task-id timeout-ms)))
    `(:task-id ,(plist-get task :task-id)
      :status ,(symbol-name (plist-get task :status))
      :result ,(plist-get task :result)
      :error ,(plist-get task :error))))

(defun emacs-mcp-swarm-api-list-presets ()
  "API: List available presets."
  (emacs-mcp-swarm-list-presets))

(defun emacs-mcp-swarm-api-kill (slave-id)
  "API: Kill SLAVE-ID."
  (emacs-mcp-swarm-kill slave-id)
  `(:killed ,slave-id))

(defun emacs-mcp-swarm-api-kill-all ()
  "API: Kill all slaves."
  (let ((count (hash-table-count emacs-mcp-swarm--slaves)))
    (emacs-mcp-swarm-kill-all)
    `(:killed-count ,count)))

;;;; Transient Menu:

(require 'transient nil t)

(when (featurep 'transient)
  (transient-define-prefix emacs-mcp-swarm-transient ()
    "Swarm orchestration menu."
    ["emacs-mcp Swarm"
     ["Slaves"
      ("s" "Spawn slave" emacs-mcp-swarm-spawn)
      ("k" "Kill slave" emacs-mcp-swarm-kill)
      ("K" "Kill all" emacs-mcp-swarm-kill-all)
      ("v" "View slave" emacs-mcp-swarm-show-slave)]
     ["Tasks"
      ("d" "Dispatch" emacs-mcp-swarm-dispatch)
      ("c" "Collect" emacs-mcp-swarm-collect)
      ("b" "Broadcast" emacs-mcp-swarm-broadcast)]
     ["Info"
      ("?" "Status" emacs-mcp-swarm-status)
      ("p" "List presets" emacs-mcp-swarm-list-presets)
      ("r" "Reload presets" emacs-mcp-swarm-reload-presets)
      ("a" "Add presets dir" emacs-mcp-swarm-add-custom-presets-dir)]]))

;;;; Minor Mode:

(defvar emacs-mcp-swarm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Use C-c M-s (Meta modifier) - allowed for minor modes per Emacs conventions
    (define-key map (kbd "C-c M-s") #'emacs-mcp-swarm-transient)
    map)
  "Keymap for `emacs-mcp-swarm-mode'.")

;;;###autoload
(define-minor-mode emacs-mcp-swarm-mode
  "Minor mode for Claude swarm orchestration.

\\{emacs-mcp-swarm-mode-map}"
  :init-value nil
  :lighter " Swarm"
  :keymap emacs-mcp-swarm-mode-map
  :global t
  :group 'emacs-mcp-swarm
  (if emacs-mcp-swarm-mode
      (progn
        (setq emacs-mcp-swarm--session-id
              (format "session-%s-%04x"
                      (format-time-string "%Y%m%d")
                      (random 65535)))
        (emacs-mcp-swarm-reload-presets)
        (message "Emacs-mcp-swarm enabled (session: %s, %d presets)"
                 emacs-mcp-swarm--session-id
                 (hash-table-count emacs-mcp-swarm--presets-cache)))
    (emacs-mcp-swarm-kill-all)
    (message "Emacs-mcp-swarm disabled")))

;;;; Addon Lifecycle:

(defun emacs-mcp-swarm--addon-init ()
  "Initialize swarm addon."
  (emacs-mcp-swarm-reload-presets)
  (setq emacs-mcp-swarm--session-id
        (format "session-%s-%04x"
                (format-time-string "%Y%m%d")
                (random 65535))))

(defun emacs-mcp-swarm--addon-shutdown ()
  "Shutdown swarm addon - kill all slaves."
  (emacs-mcp-swarm-kill-all)
  (setq emacs-mcp-swarm--presets-cache nil)
  (setq emacs-mcp-swarm--session-id nil))

;; Register with addon system
(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'swarm
   :version "0.1.0"
   :description "Claude swarm orchestration for parallel task execution"
   :requires '()  ; vterm or eat checked at spawn time
   :provides '(emacs-mcp-swarm-mode emacs-mcp-swarm-transient)
   :init #'emacs-mcp-swarm--addon-init
   :shutdown #'emacs-mcp-swarm--addon-shutdown))

(provide 'emacs-mcp-swarm)
;;; emacs-mcp-swarm.el ends here
