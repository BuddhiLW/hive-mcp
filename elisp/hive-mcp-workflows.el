;;; hive-mcp-workflows.el --- Workflow system for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; User-defined workflow system for hive-mcp.  Allows creating
;; multi-step automations with elisp, shell commands, prompts,
;; and conditions.
;;

;;; Code:

(require 'hive-mcp-memory)

;;; Customization

(defgroup hive-mcp-workflows nil
  "Workflow settings for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-workflow-")

(defcustom hive-mcp-workflows-file
  (expand-file-name "workflows.el"
                    (expand-file-name "hive-mcp" user-emacs-directory))
  "File storing user workflow definitions."
  :type 'file
  :group 'hive-mcp-workflows)

;;; Registry

(defvar hive-mcp-workflow-registry (make-hash-table :test 'equal)
  "Registry of user-defined workflows.
Each entry is NAME -> plist with :description :params :steps :on-error.")

(defvar hive-mcp-workflow-step-handlers (make-hash-table :test 'eq)
  "Handlers for custom workflow step types.
Each entry is TYPE -> function taking (step env) and returning env.")

(defvar hive-mcp-workflow-before-hook nil
  "Hook run before workflow execution.  Args: WORKFLOW-NAME ARGS.")

(defvar hive-mcp-workflow-after-hook nil
  "Hook run after workflow execution.  Args: WORKFLOW-NAME RESULT.")

;;; Workflow Structure
;;
;; Workflow spec:
;; (:name "string"
;;  :description "string"
;;  :params ((:name "param1" :type string :default "value" :required t) ...)
;;  :steps (step1 step2 ...)
;;  :on-error :stop|:continue|:ask)
;;
;; Step types:
;; (:type :elisp :code "(do-something)")
;; (:type :shell :command "npm test" :var result)
;; (:type :prompt :message "Continue?" :var response)
;; (:type :confirm :message "Are you sure?")
;; (:type :condition :test (lambda (env) ...) :then step :else step)
;; (:type :memory-add :mem-type "note" :content "...")
;; (:type :notify :message "Done!" :level info|warning|error)

;;; Core Operations

(defun hive-mcp-workflow-register (name spec)
  "Register workflow NAME with SPEC."
  (puthash name spec hive-mcp-workflow-registry)
  (hive-mcp-workflow--save)
  (message "Workflow '%s' registered" name))

(defun hive-mcp-workflow-unregister (name)
  "Remove workflow NAME."
  (remhash name hive-mcp-workflow-registry)
  (hive-mcp-workflow--save))

(defun hive-mcp-workflow-get (name)
  "Get workflow spec by NAME."
  (gethash name hive-mcp-workflow-registry))

(defun hive-mcp-workflow-list ()
  "Return list of all workflows with metadata."
  (let (workflows)
    (maphash
     (lambda (name spec)
       (push (list :name name
                   :description (plist-get spec :description)
                   :params (plist-get spec :params))
             workflows))
     hive-mcp-workflow-registry)
    (nreverse workflows)))

;;; Execution

(defun hive-mcp-workflow-run (name &optional args)
  "Execute workflow NAME with optional ARGS plist."
  (if-let* ((spec (gethash name hive-mcp-workflow-registry)))
      (progn
        (run-hook-with-args 'hive-mcp-workflow-before-hook name args)
        (let ((result (hive-mcp-workflow--execute spec args)))
          (run-hook-with-args 'hive-mcp-workflow-after-hook name result)
          result))
    (error "Unknown workflow: %s" name)))

(defun hive-mcp-workflow--execute (spec args)
  "Execute workflow SPEC with ARGS.
Supports two modes:
  1. :handler - direct function invocation with args plist
  2. :steps - step-by-step execution with env passing"
  (let ((handler (plist-get spec :handler)))
    ;; If workflow has a :handler, call it directly with args
    (if handler
        (condition-case err
            (funcall handler args)
          (error
           (list :success nil
                 :error (error-message-string err))))
      ;; Otherwise execute step-by-step
      (let* ((steps (plist-get spec :steps))
             (on-error (or (plist-get spec :on-error) :stop))
             (env (hive-mcp-workflow--init-env spec args))
             (step-num 0))
        (catch 'workflow-abort
          (dolist (step steps)
            (setq step-num (1+ step-num))
            (condition-case err
                (setq env (hive-mcp-workflow--run-step step env))
              (error
               (pcase on-error
                 (:stop
                  (throw 'workflow-abort
                         (list :success nil
                               :error (error-message-string err)
                               :failed-step step-num
                               :env env)))
                 (:continue nil)
                 (:ask
                  (unless (yes-or-no-p
                           (format "Step %d failed: %s. Continue? "
                                   step-num (error-message-string err)))
                    (throw 'workflow-abort
                           (list :success nil
                                 :error "Aborted by user"
                                 :failed-step step-num
                                 :env env))))))))
          (list :success t :env env))))))

(defun hive-mcp-workflow--init-env (spec args)
  "Initialize environment for workflow SPEC with ARGS."
  (let ((env (copy-sequence args))
        (params (plist-get spec :params)))
    ;; Apply defaults for missing params
    (dolist (param params)
      (let ((name (plist-get param :name))
            (default (plist-get param :default)))
        (unless (plist-get env (intern (concat ":" name)))
          (when default
            (setq env (plist-put env (intern (concat ":" name)) default))))))
    ;; Check required params
    (dolist (param params)
      (when (plist-get param :required)
        (let ((name (plist-get param :name)))
          (unless (plist-get env (intern (concat ":" name)))
            (error "Required parameter missing: %s" name)))))
    env))

(defun hive-mcp-workflow--run-step (step env)
  "Run a single workflow STEP with ENV.  Return updated ENV."
  (let ((type (plist-get step :type)))
    ;; Check for custom handler first
    (if-let* ((handler (gethash type hive-mcp-workflow-step-handlers)))
        (funcall handler step env)
      ;; Built-in step types
      (pcase type
        (:elisp
         (hive-mcp-workflow--step-elisp step env))
        (:shell
         (hive-mcp-workflow--step-shell step env))
        (:prompt
         (hive-mcp-workflow--step-prompt step env))
        (:confirm
         (hive-mcp-workflow--step-confirm step env))
        (:condition
         (hive-mcp-workflow--step-condition step env))
        (:memory-add
         (hive-mcp-workflow--step-memory-add step env))
        (:notify
         (hive-mcp-workflow--step-notify step env))
        (_
         (error "Unknown step type: %s" type))))))

;;; Built-in Step Implementations

(defun hive-mcp-workflow--step-elisp (step env)
  "Execute elisp code from STEP with ENV bindings."
  (let* ((code (plist-get step :code))
         ;; Make env available to code
         (result (eval (read code) t)))
    (if-let* ((var (plist-get step :var)))
        (plist-put env var result)
      env)))

(defun hive-mcp-workflow--step-shell (step env)
  "Execute shell command from STEP with ENV variable substitution."
  (let* ((cmd-template (plist-get step :command))
         ;; Simple variable substitution ${var}
         (cmd (hive-mcp-workflow--substitute-vars cmd-template env))
         (result (string-trim (shell-command-to-string cmd))))
    (if-let* ((var (plist-get step :var)))
        (plist-put env var result)
      env)))

(defun hive-mcp-workflow--step-prompt (step env)
  "Prompt user for input using message from STEP, store result in ENV."
  (let* ((message (plist-get step :message))
         (default (plist-get step :default))
         (result (read-string (concat message ": ") default)))
    (plist-put env (plist-get step :var) result)))

(defun hive-mcp-workflow--step-confirm (step env)
  "Ask user for confirmation using message from STEP.
Returns ENV if confirmed, otherwise signals error."
  (unless (yes-or-no-p (plist-get step :message))
    (error "User cancelled"))
  env)

(defun hive-mcp-workflow--step-condition (step env)
  "Execute conditional branch from STEP based on test result in ENV."
  (let ((test-fn (plist-get step :test)))
    (if (funcall test-fn env)
        (when-let* ((then-step (plist-get step :then)))
          (hive-mcp-workflow--run-step then-step env))
      (when-let* ((else-step (plist-get step :else)))
        (hive-mcp-workflow--run-step else-step env)))))

(defun hive-mcp-workflow--step-memory-add (step env)
  "Add memory entry from STEP, substituting variables from ENV."
  (let ((type (plist-get step :mem-type))
        (content (hive-mcp-workflow--substitute-vars
                  (plist-get step :content) env))
        (tags (plist-get step :tags)))
    (hive-mcp-memory-add (intern type) content tags))
  env)

(defun hive-mcp-workflow--step-notify (step env)
  "Show notification from STEP message, substituting variables from ENV."
  (let ((message (hive-mcp-workflow--substitute-vars
                  (plist-get step :message) env))
        (level (or (plist-get step :level) 'info)))
    (pcase level
      ('error (user-error "%s" message))
      ('warning (display-warning 'hive-mcp message :warning))
      (_ (message "[MCP] %s" message))))
  env)

;;; Utility Functions

(defun hive-mcp-workflow--substitute-vars (template env)
  "Substitute ${var} in TEMPLATE with values from ENV."
  (if (stringp template)
      (replace-regexp-in-string
       "\\${\\([^}]+\\)}"
       (lambda (match)
         (let* ((var-name (match-string 1 match))
                (var-key (intern (concat ":" var-name)))
                (value (plist-get env var-key)))
           (if value (format "%s" value) match)))
       template)
    template))

;;; Custom Step Type Registration

(defun hive-mcp-workflow-register-step-type (type handler)
  "Register workflow step TYPE with HANDLER function.
HANDLER receives (step env) and should return updated env."
  (puthash type handler hive-mcp-workflow-step-handlers))

;;; Persistence

(defun hive-mcp-workflow--save ()
  "Save workflows to file."
  (make-directory (file-name-directory hive-mcp-workflows-file) t)
  (with-temp-file hive-mcp-workflows-file
    (let ((workflows nil))
      (maphash (lambda (k v) (push (cons k v) workflows))
               hive-mcp-workflow-registry)
      (insert ";;; hive-mcp workflows - auto-generated\n")
      (insert ";;; Do not edit manually\n\n")
      (insert "(setq hive-mcp-workflow--saved-data\n  '")
      (pp workflows (current-buffer))
      (insert ")\n"))))

(defun hive-mcp-workflow--load ()
  "Load workflows from file."
  (when (file-exists-p hive-mcp-workflows-file)
    (load hive-mcp-workflows-file t t)
    (when (boundp 'hive-mcp-workflow--saved-data)
      (dolist (wf hive-mcp-workflow--saved-data)
        (puthash (car wf) (cdr wf) hive-mcp-workflow-registry)))))

;;; Interactive Commands

(defun hive-mcp-workflow-run-interactive ()
  "Interactively run a workflow."
  (interactive)
  (let* ((workflows (hive-mcp-workflow-list))
         (names (mapcar (lambda (wf) (plist-get wf :name)) workflows))
         (name (completing-read "Workflow: " names nil t)))
    (let ((result (hive-mcp-workflow-run name)))
      (if (plist-get result :success)
          (message "Workflow completed successfully")
        (message "Workflow failed: %s" (plist-get result :error))))))

;;; Memory-Integrated Workflows
;;
;; These workflows use the memory system with project scope for persistence.

(defun hive-mcp-workflow-wrap--get-date ()
  "Return current date in YYYY-MM-DD format."
  (format-time-string "%Y-%m-%d"))

(defun hive-mcp-workflow-wrap--make-tags (base-tags)
  "Create tags list with BASE-TAGS plus auto-injected project scope."
  (hive-mcp-memory--inject-project-scope base-tags))

(defun hive-mcp-workflow-wrap--git-status ()
  "Get git status summary for wrap workflow."
  (let ((branch (string-trim
                 (shell-command-to-string
                  "git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'")))
        (status (shell-command-to-string "git status --porcelain 2>/dev/null"))
        (unmerged (string-trim
                   (shell-command-to-string
                    "git branch --no-merged main 2>/dev/null | grep -E '^\\s*(feature|fix|feat)/' | head -10 || true"))))
    (list :branch branch
          :has-uncommitted (not (string-empty-p status))
          :unmerged-feature-branches
          (when (not (string-empty-p unmerged))
            (split-string unmerged "\n" t "\\s-*")))))

(defun hive-mcp-workflow-wrap--store-accomplishments (accomplishments)
  "Store ACCOMPLISHMENTS as session-summary note with short-term duration.
Returns the stored entry."
  (when accomplishments
    (let* ((tags (hive-mcp-workflow-wrap--make-tags
                  '("session-summary" "wrap")))
           (date (hive-mcp-workflow-wrap--get-date))
           (content (format "## Session Summary: %s\n\n### Completed\n%s"
                            date
                            (mapconcat (lambda (a) (format "- [x] %s" a))
                                       accomplishments "\n"))))
      (hive-mcp-memory-add 'note content tags nil 'short-term))))

(defun hive-mcp-workflow-wrap--store-decisions (decisions)
  "Store DECISIONS as decision entries with long-term duration.
Returns list of stored entries."
  (when decisions
    (mapcar
     (lambda (decision)
       (let* ((tags (hive-mcp-workflow-wrap--make-tags '("wrap" "session-decision")))
              (date (hive-mcp-workflow-wrap--get-date))
              (content (list :title decision
                             :rationale "Session decision"
                             :date date)))
         (hive-mcp-memory-add 'decision content tags nil 'long-term)))
     decisions)))

(defun hive-mcp-workflow-wrap--store-conventions (conventions)
  "Store CONVENTIONS as convention entries with permanent duration.
Returns list of stored entries."
  (when conventions
    (mapcar
     (lambda (convention)
       (let* ((tags (hive-mcp-workflow-wrap--make-tags '("wrap")))
              (date (hive-mcp-workflow-wrap--get-date))
              (content (list :description convention
                             :date date)))
         (hive-mcp-memory-add 'convention content tags nil 'permanent)))
     conventions)))

(defun hive-mcp-workflow-wrap--sync-kanban (completed-task-ids)
  "Move COMPLETED-TASK-IDS to done status in kanban.
Returns count of successfully moved tasks."
  (when (and completed-task-ids (fboundp 'hive-mcp-kanban-task-move))
    (let ((moved 0))
      (dolist (task-id completed-task-ids)
        (condition-case nil
            (progn
              (hive-mcp-kanban-task-move task-id "done")
              (setq moved (1+ moved)))
          (error nil)))
      moved)))

(defun hive-mcp-workflow-wrap--get-kanban-status ()
  "Get kanban stats if available."
  (when (fboundp 'hive-mcp-kanban-stats)
    (condition-case nil
        (hive-mcp-kanban-stats)
      (error nil))))

(defun hive-mcp-workflow-wrap--gather-recent-notes ()
  "Get memory entries (notes/snippets) created today."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (notes (ignore-errors (hive-mcp-memory-query 'note nil 20)))
         (snippets (ignore-errors (hive-mcp-memory-query 'snippet nil 20))))
    (seq-filter
     (lambda (entry)
       (when-let* ((created (plist-get entry :created)))
         (string-prefix-p today created)))
     (append (when (listp notes) notes)
             (when (listp snippets) snippets)))))

(defun hive-mcp-workflow-wrap--gather-git-commits ()
  "Get commits on current branch from today."
  (let ((output (shell-command-to-string
                 "git log --since='midnight' --oneline 2>/dev/null")))
    (when (and output (not (string-empty-p output)))
      (split-string output "\n" t))))

(defun hive-mcp-workflow-wrap--gather-kanban-activity ()
  "Get in-progress and review kanban tasks."
  (when (fboundp 'hive-mcp-kanban-list-tasks)
    (condition-case nil
        (list :in-progress (hive-mcp-kanban-list-tasks "inprogress")
              :review (hive-mcp-kanban-list-tasks "inreview"))
      (error nil))))

(defun hive-mcp-workflow-wrap--gather-channel-events ()
  "Get recent channel events if channel is active."
  (when (and (fboundp 'hive-mcp-channel-get-recent-events)
             (fboundp 'hive-mcp-channel-connected-p)
             (hive-mcp-channel-connected-p))
    (condition-case nil
        (hive-mcp-channel-get-recent-events 10)
      (error nil))))

(defun hive-mcp-workflow-wrap--gather-session-data ()
  "Auto-gather session data from all available sources.
Returns plist with :recent-notes, :recent-commits,
:kanban-activity, :ai-interactions."
  (list :recent-notes (hive-mcp-workflow-wrap--gather-recent-notes)
        :recent-commits (hive-mcp-workflow-wrap--gather-git-commits)
        :kanban-activity (hive-mcp-workflow-wrap--gather-kanban-activity)
        :ai-interactions (hive-mcp-workflow-wrap--gather-channel-events)))

(defun hive-mcp-workflow-wrap--merge-args (gathered provided)
  "Merge GATHERED data with user-PROVIDED args.
PROVIDED takes precedence. Converts gathered data to wrap args format."
  (let ((result (copy-sequence provided)))
    ;; If no accomplishments provided, derive from gathered notes
    (unless (plist-get result :accomplishments)
      (when-let* ((notes (plist-get gathered :recent-notes)))
        (plist-put result :accomplishments
                   (mapcar (lambda (n)
                             (let ((content (plist-get n :content)))
                               (if (> (length content) 100)
                                   (concat (substring content 0 97) "...")
                                 content)))
                           (seq-take notes 5)))))
    ;; Add git commits as context
    (when-let* ((commits (plist-get gathered :recent-commits)))
      (unless (plist-get result :git-commits)
        (plist-put result :git-commits commits)))
    result))

(defun hive-mcp-workflow-wrap (&optional args)
  "Execute wrap workflow with ARGS plist.

ARGS can contain:
  :accomplishments - list of completed tasks (stored as note, short-term)
  :decisions - list of decisions made (stored as decision, long-term)
  :conventions - list of conventions (stored as convention, permanent)
  :in-progress - list of in-progress items (for summary)
  :next-actions - list of next session priorities (for summary)
  :completed-tasks - list of kanban task IDs to mark done

All stored entries auto-inject project scope via
`hive-mcp-memory--inject-project-scope'.

Returns structured result with:
  :success, :project, :date, :stored, :expired-cleaned, :git, :kanban"
  (interactive)
  (let* ((accomplishments (plist-get args :accomplishments))
         (decisions (plist-get args :decisions))
         (conventions (plist-get args :conventions))
         (in-progress (plist-get args :in-progress))
         (next-actions (plist-get args :next-actions))
         (completed-tasks (plist-get args :completed-tasks))
         (project-name (hive-mcp-memory--get-project-name))
         (date (hive-mcp-workflow-wrap--get-date))
         (git-info (hive-mcp-workflow-wrap--git-status))
         (kanban-before (hive-mcp-workflow-wrap--get-kanban-status))
         (stored '())
         (expired-count 0))

    ;; 1. Cleanup expired memory entries first
    (setq expired-count (hive-mcp-memory-cleanup-expired))

    ;; 2. Store accomplishments as session summary
    (when accomplishments
      (hive-mcp-workflow-wrap--store-accomplishments accomplishments)
      (push 'accomplishments stored))

    ;; 3. Store decisions
    (when decisions
      (hive-mcp-workflow-wrap--store-decisions decisions)
      (push 'decisions stored))

    ;; 4. Store conventions
    (when conventions
      (hive-mcp-workflow-wrap--store-conventions conventions)
      (push 'conventions stored))

    ;; 5. Create full session summary note if any content provided
    (when (or accomplishments decisions in-progress next-actions)
      (let* ((tags (hive-mcp-workflow-wrap--make-tags
                    '("session-summary" "wrap" "full-summary")))
             (content (format "## Session Summary: %s\n\n### Completed\n%s\n\n### Decisions Made\n%s\n\n### In Progress\n%s\n\n### Next Actions\n%s"
                              date
                              (if accomplishments
                                  (mapconcat (lambda (a) (format "- [x] %s" a))
                                             accomplishments "\n")
                                "- (none)")
                              (if decisions
                                  (mapconcat (lambda (d) (format "- %s" d))
                                             decisions "\n")
                                "- (none)")
                              (if in-progress
                                  (mapconcat (lambda (ip) (format "- [ ] %s" ip))
                                             in-progress "\n")
                                "- (none)")
                              (if next-actions
                                  (mapconcat (lambda (na) (format "- %s" na))
                                             next-actions "\n")
                                "- (none)"))))
        (hive-mcp-memory-add 'note content tags nil 'short-term)
        (push 'session-summary stored)))

    ;; 6. Sync kanban - move completed tasks to done
    (when completed-tasks
      (let ((moved-count (hive-mcp-workflow-wrap--sync-kanban completed-tasks)))
        (when (> moved-count 0)
          (push 'kanban-synced stored))))

    ;; 7. Get final kanban status
    (let ((kanban-after (hive-mcp-workflow-wrap--get-kanban-status)))

      ;; Return result
      (list :success t
            :date date
            :project (or project-name "global")
            :stored (nreverse stored)
            :counts (list :accomplishments (length accomplishments)
                          :decisions (length decisions)
                          :conventions (length conventions)
                          :tasks-completed (length completed-tasks))
            :expired-cleaned expired-count
            :git git-info
            :kanban (list :before kanban-before
                          :after kanban-after)
            :summary (format "Session wrapped for %s. Stored: %s. Cleaned %d expired entries."
                             (or project-name "global")
                             (mapconcat #'symbol-name stored ", ")
                             expired-count)))))

(defun hive-mcp-workflow-catchup (&optional _args)
  "Execute the catchup workflow - restore context from memory.
Queries session notes, decisions, and conventions with project scope.
Returns structured context for display.
ARGS is unused but accepted for workflow handler compatibility."
  (interactive)
  (let* ((project-name (hive-mcp-memory--get-project-name))
         (applicable-scopes (hive-mcp-memory--applicable-scope-tags)))
    
    ;; Query each type with scope filtering
    (let ((session-notes (hive-mcp-memory-query 'note '("session-summary") nil 3 nil nil))
          (decisions (hive-mcp-memory-query 'decision nil nil 10 nil nil))
          (conventions (hive-mcp-memory-query 'convention nil nil 10 nil nil))
          (snippets-meta (seq-take 
                          (mapcar (lambda (e) 
                                    (list :id (plist-get e :id)
                                          :preview (truncate-string-to-width 
                                                    (or (plist-get e :content) "") 60)))
                                  (hive-mcp-memory-query 'snippet nil nil 5 nil nil))
                          5))
          (expiring (hive-mcp-memory-query-expiring 7))
          (git-branch (string-trim 
                       (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'none'")))
          (uncommitted (not (string-empty-p 
                             (shell-command-to-string "git status --porcelain 2>/dev/null"))))
          (last-commit (string-trim
                        (shell-command-to-string "git log -1 --format='%h - %s' 2>/dev/null || echo 'none'"))))
      
      (list :success t
            :project project-name
            :scopes applicable-scopes
            :git (list :branch git-branch
                       :uncommitted uncommitted  
                       :last-commit last-commit)
            :memory (list :session-notes (length session-notes)
                          :decisions (length decisions)
                          :conventions (length conventions)
                          :snippets-available (length snippets-meta)
                          :expiring-soon (length expiring))
            :context (list :recent-sessions session-notes
                           :active-decisions decisions
                           :conventions conventions
                           :snippet-previews snippets-meta
                           :expiring-entries expiring)
            :session-guidance "## Session Guidance (Progressive Crystallization)

As you work, proactively store insights using `mcp_memory_add` with:
- type: 'note' or 'decision'
- duration: 'ephemeral'
- tags: include session tag and 'session-progress'

These ephemeral entries will be reviewed and crystallized at wrap time.

**Capture proactively:**
- Key decisions and their rationale
- Discoveries about the codebase  
- Problems encountered and solutions
- Ideas for future improvements

When completing kanban tasks, move them to 'done' - the system will automatically create progress notes."))))

;;; Built-in Example Workflows

(defun hive-mcp-workflow--register-builtins ()
  "Register built-in example workflows."
  ;; Quick note workflow
  (unless (gethash "quick-note" hive-mcp-workflow-registry)
    (puthash "quick-note"
             '(:name "quick-note"
               :description "Add a quick note with current context"
               :steps ((:type :prompt
                        :message "Note"
                        :var :note-text)
                       (:type :memory-add
                        :mem-type "note"
                        :content "${note-text}")
                       (:type :notify
                        :message "Note saved!")))
             hive-mcp-workflow-registry))

  ;; Git commit workflow
  (unless (gethash "commit" hive-mcp-workflow-registry)
    (puthash "commit"
             '(:name "commit"
               :description "Stage all and commit with message"
               :params ((:name "message" :required t))
               :steps ((:type :shell
                        :command "git add -A")
                       (:type :shell
                        :command "git commit -m \"${message}\"")
                       (:type :notify
                        :message "Committed: ${message}")))
             hive-mcp-workflow-registry))
  
  ;; Wrap workflow (memory-integrated)
  ;; Uses :handler instead of :steps for direct function invocation with args
  (unless (gethash "wrap" hive-mcp-workflow-registry)
    (puthash "wrap"
             '(:name "wrap"
               :description "End-of-session wrap-up (Memory-Integrated)"
               :params ((:name "accomplishments" :type list :required nil
                         :description "List of completed tasks")
                        (:name "decisions" :type list :required nil
                         :description "List of decisions made")
                        (:name "conventions" :type list :required nil
                         :description "List of conventions to store permanently")
                        (:name "in-progress" :type list :required nil
                         :description "List of in-progress items")
                        (:name "next-actions" :type list :required nil
                         :description "List of next session priorities")
                        (:name "completed-tasks" :type list :required nil
                         :description "Kanban task IDs to mark done"))
               :handler hive-mcp-workflow-wrap)
             hive-mcp-workflow-registry))

  ;; Catchup workflow (memory-integrated)
  (unless (gethash "catchup" hive-mcp-workflow-registry)
    (puthash "catchup"
             '(:name "catchup"
               :description "Catch Up (Memory-Integrated)"
               :handler hive-mcp-workflow-catchup)
             hive-mcp-workflow-registry)))

;; Auto-register builtins on load
(hive-mcp-workflow--register-builtins)

(provide 'hive-mcp-workflows)
;;; hive-mcp-workflows.el ends here
