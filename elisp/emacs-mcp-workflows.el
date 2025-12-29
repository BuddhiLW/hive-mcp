;;; emacs-mcp-workflows.el --- Workflow system for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of emacs-mcp.

;;; Commentary:
;;
;; User-defined workflow system for emacs-mcp.  Allows creating
;; multi-step automations with elisp, shell commands, prompts,
;; and conditions.
;;

;;; Code:

(require 'emacs-mcp-memory)

;;; Customization

(defgroup emacs-mcp-workflows nil
  "Workflow settings for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-workflow-")

(defcustom emacs-mcp-workflows-file
  (expand-file-name "workflows.el"
                    (expand-file-name "emacs-mcp" user-emacs-directory))
  "File storing user workflow definitions."
  :type 'file
  :group 'emacs-mcp-workflows)

;;; Registry

(defvar emacs-mcp-workflow-registry (make-hash-table :test 'equal)
  "Registry of user-defined workflows.
Each entry is NAME -> plist with :description :params :steps :on-error.")

(defvar emacs-mcp-workflow-step-handlers (make-hash-table :test 'eq)
  "Handlers for custom workflow step types.
Each entry is TYPE -> function taking (step env) and returning env.")

(defvar emacs-mcp-workflow-before-hook nil
  "Hook run before workflow execution.  Args: WORKFLOW-NAME ARGS.")

(defvar emacs-mcp-workflow-after-hook nil
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

(defun emacs-mcp-workflow-register (name spec)
  "Register workflow NAME with SPEC."
  (puthash name spec emacs-mcp-workflow-registry)
  (emacs-mcp-workflow--save)
  (message "Workflow '%s' registered" name))

(defun emacs-mcp-workflow-unregister (name)
  "Remove workflow NAME."
  (remhash name emacs-mcp-workflow-registry)
  (emacs-mcp-workflow--save))

(defun emacs-mcp-workflow-get (name)
  "Get workflow spec by NAME."
  (gethash name emacs-mcp-workflow-registry))

(defun emacs-mcp-workflow-list ()
  "Return list of all workflows with metadata."
  (let (workflows)
    (maphash
     (lambda (name spec)
       (push (list :name name
                   :description (plist-get spec :description)
                   :params (plist-get spec :params))
             workflows))
     emacs-mcp-workflow-registry)
    (nreverse workflows)))

;;; Execution

(defun emacs-mcp-workflow-run (name &optional args)
  "Execute workflow NAME with optional ARGS plist."
  (if-let* ((spec (gethash name emacs-mcp-workflow-registry)))
      (progn
        (run-hook-with-args 'emacs-mcp-workflow-before-hook name args)
        (let ((result (emacs-mcp-workflow--execute spec args)))
          (run-hook-with-args 'emacs-mcp-workflow-after-hook name result)
          result))
    (error "Unknown workflow: %s" name)))

(defun emacs-mcp-workflow--execute (spec args)
  "Execute workflow SPEC with ARGS."
  (let* ((steps (plist-get spec :steps))
         (on-error (or (plist-get spec :on-error) :stop))
         (env (emacs-mcp-workflow--init-env spec args))
         (step-num 0))
    (catch 'workflow-abort
      (dolist (step steps)
        (setq step-num (1+ step-num))
        (condition-case err
            (setq env (emacs-mcp-workflow--run-step step env))
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
      (list :success t :env env))))

(defun emacs-mcp-workflow--init-env (spec args)
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

(defun emacs-mcp-workflow--run-step (step env)
  "Run a single workflow STEP with ENV.  Return updated ENV."
  (let ((type (plist-get step :type)))
    ;; Check for custom handler first
    (if-let* ((handler (gethash type emacs-mcp-workflow-step-handlers)))
        (funcall handler step env)
      ;; Built-in step types
      (pcase type
        (:elisp
         (emacs-mcp-workflow--step-elisp step env))
        (:shell
         (emacs-mcp-workflow--step-shell step env))
        (:prompt
         (emacs-mcp-workflow--step-prompt step env))
        (:confirm
         (emacs-mcp-workflow--step-confirm step env))
        (:condition
         (emacs-mcp-workflow--step-condition step env))
        (:memory-add
         (emacs-mcp-workflow--step-memory-add step env))
        (:notify
         (emacs-mcp-workflow--step-notify step env))
        (_
         (error "Unknown step type: %s" type))))))

;;; Built-in Step Implementations

(defun emacs-mcp-workflow--step-elisp (step env)
  "Execute elisp code from STEP with ENV bindings."
  (let* ((code (plist-get step :code))
         ;; Make env available to code
         (result (eval (read code) t)))
    (if-let* ((var (plist-get step :var)))
        (plist-put env var result)
      env)))

(defun emacs-mcp-workflow--step-shell (step env)
  "Execute shell command from STEP with ENV variable substitution."
  (let* ((cmd-template (plist-get step :command))
         ;; Simple variable substitution ${var}
         (cmd (emacs-mcp-workflow--substitute-vars cmd-template env))
         (result (string-trim (shell-command-to-string cmd))))
    (if-let* ((var (plist-get step :var)))
        (plist-put env var result)
      env)))

(defun emacs-mcp-workflow--step-prompt (step env)
  "Prompt user for input using message from STEP, store result in ENV."
  (let* ((message (plist-get step :message))
         (default (plist-get step :default))
         (result (read-string (concat message ": ") default)))
    (plist-put env (plist-get step :var) result)))

(defun emacs-mcp-workflow--step-confirm (step env)
  "Ask user for confirmation using message from STEP.
Returns ENV if confirmed, otherwise signals error."
  (unless (yes-or-no-p (plist-get step :message))
    (error "User cancelled"))
  env)

(defun emacs-mcp-workflow--step-condition (step env)
  "Execute conditional branch from STEP based on test result in ENV."
  (let ((test-fn (plist-get step :test)))
    (if (funcall test-fn env)
        (when-let* ((then-step (plist-get step :then)))
          (emacs-mcp-workflow--run-step then-step env))
      (when-let* ((else-step (plist-get step :else)))
        (emacs-mcp-workflow--run-step else-step env)))))

(defun emacs-mcp-workflow--step-memory-add (step env)
  "Add memory entry from STEP, substituting variables from ENV."
  (let ((type (plist-get step :mem-type))
        (content (emacs-mcp-workflow--substitute-vars
                  (plist-get step :content) env))
        (tags (plist-get step :tags)))
    (emacs-mcp-memory-add (intern type) content tags))
  env)

(defun emacs-mcp-workflow--step-notify (step env)
  "Show notification from STEP message, substituting variables from ENV."
  (let ((message (emacs-mcp-workflow--substitute-vars
                  (plist-get step :message) env))
        (level (or (plist-get step :level) 'info)))
    (pcase level
      ('error (user-error "%s" message))
      ('warning (display-warning 'emacs-mcp message :warning))
      (_ (message "[MCP] %s" message))))
  env)

;;; Utility Functions

(defun emacs-mcp-workflow--substitute-vars (template env)
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

(defun emacs-mcp-workflow-register-step-type (type handler)
  "Register workflow step TYPE with HANDLER function.
HANDLER receives (step env) and should return updated env."
  (puthash type handler emacs-mcp-workflow-step-handlers))

;;; Persistence

(defun emacs-mcp-workflow--save ()
  "Save workflows to file."
  (make-directory (file-name-directory emacs-mcp-workflows-file) t)
  (with-temp-file emacs-mcp-workflows-file
    (let ((workflows nil))
      (maphash (lambda (k v) (push (cons k v) workflows))
               emacs-mcp-workflow-registry)
      (insert ";;; emacs-mcp workflows - auto-generated\n")
      (insert ";;; Do not edit manually\n\n")
      (insert "(setq emacs-mcp-workflow--saved-data\n  '")
      (pp workflows (current-buffer))
      (insert ")\n"))))

(defun emacs-mcp-workflow--load ()
  "Load workflows from file."
  (when (file-exists-p emacs-mcp-workflows-file)
    (load emacs-mcp-workflows-file t t)
    (when (boundp 'emacs-mcp-workflow--saved-data)
      (dolist (wf emacs-mcp-workflow--saved-data)
        (puthash (car wf) (cdr wf) emacs-mcp-workflow-registry)))))

;;; Interactive Commands

(defun emacs-mcp-workflow-run-interactive ()
  "Interactively run a workflow."
  (interactive)
  (let* ((workflows (emacs-mcp-workflow-list))
         (names (mapcar (lambda (wf) (plist-get wf :name)) workflows))
         (name (completing-read "Workflow: " names nil t)))
    (let ((result (emacs-mcp-workflow-run name)))
      (if (plist-get result :success)
          (message "Workflow completed successfully")
        (message "Workflow failed: %s" (plist-get result :error))))))

;;; Built-in Example Workflows

(defun emacs-mcp-workflow--register-builtins ()
  "Register built-in example workflows."
  ;; Quick note workflow
  (unless (gethash "quick-note" emacs-mcp-workflow-registry)
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
             emacs-mcp-workflow-registry))

  ;; Git commit workflow
  (unless (gethash "commit" emacs-mcp-workflow-registry)
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
             emacs-mcp-workflow-registry)))

(provide 'emacs-mcp-workflows)
;;; emacs-mcp-workflows.el ends here
