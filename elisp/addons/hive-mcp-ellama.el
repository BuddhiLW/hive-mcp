;;; hive-mcp-ellama.el --- ellama/Ollama integration for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, mcp, ellama, ollama
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Integration between hive-mcp and ellama for local Ollama LLM inference.
;;
;; OPTIONAL DEPENDENCIES:
;; - ellama (https://github.com/s-kostyaev/ellama)
;; - Ollama running locally with models: devstral, deepseek-r1:7b
;;
;; Features:
;; - Inject memory context into ellama prompts before sending
;; - Store notable ellama responses to memory
;; - Swarm backend for Ollama workers (two-tier: Claude coordinator + Ollama workers)
;; - Memory-aware system directives
;; - Model selection for different task types
;;
;; Usage:
;;   (hive-mcp-ellama-mode 1)  ; Enable integration globally
;;
;; Or add to your config:
;;   (with-eval-after-load 'ellama
;;     (hive-mcp-ellama-mode 1))
;;
;; Two-Tier Swarm Architecture:
;;   - Claude (premium): Coordinates, breaks down complex tasks, reviews
;;   - Ollama (free): Implements low-level coding tasks with MCP tool access
;;
;; Key Integration Points:
;; - ellama-chat-done-callback: Response storage and logging
;; - hive-mcp-swarm: Backend for spawning Ollama workers

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Soft dependencies
(declare-function ellama-chat "ellama")
(declare-function ellama-stream "ellama")
(declare-function ellama-complete "ellama")
(declare-function ellama-provider-ollama "llm-ollama")
(declare-function make-llm-ollama "llm-ollama")
(declare-function hive-mcp-ai-build-context "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-inject-context "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-store-response "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-log-interaction "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-dispatch-to-swarm "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-run-post-response-hooks "hive-mcp-ai-bridge")
(declare-function hive-mcp-ai-format-context-for "hive-mcp-ai-bridge")
(declare-function hive-mcp-api-call "hive-mcp-api")

(defvar ellama-provider)
(defvar ellama-chat-done-callback)

;;;; Customization

(defgroup hive-mcp-ellama nil
  "Integration between hive-mcp and ellama/Ollama."
  :group 'hive-mcp
  :prefix "hive-mcp-ellama-")

(defcustom hive-mcp-ellama-default-model "devstral"
  "Default Ollama model for inference and swarm workers."
  :type 'string
  :group 'hive-mcp-ellama)

(defcustom hive-mcp-ellama-models
  '(("devstral" . "Best quality coding - Mistral's dev model")
    ("devstral-small" . "Fast, good enough for simple tasks")
    ("deepseek-r1:7b" . "Reasoning tasks with chain-of-thought"))
  "Available Ollama models with descriptions.
Each entry is (MODEL-NAME . DESCRIPTION)."
  :type '(alist :key-type string :value-type string)
  :group 'hive-mcp-ellama)

(defcustom hive-mcp-ellama-inject-context t
  "When non-nil, inject memory context into ellama prompts."
  :type 'boolean
  :group 'hive-mcp-ellama)

(defcustom hive-mcp-ellama-include-semantic t
  "When non-nil, include semantic search in context injection."
  :type 'boolean
  :group 'hive-mcp-ellama)

(defcustom hive-mcp-ellama-store-responses t
  "When non-nil, store notable ellama responses to memory."
  :type 'boolean
  :group 'hive-mcp-ellama)

(defcustom hive-mcp-ellama-log-interactions t
  "When non-nil, log ellama interactions for audit trail."
  :type 'boolean
  :group 'hive-mcp-ellama)

(defcustom hive-mcp-ellama-context-prefix "\n\n---\n## Project Context\n\n"
  "Prefix added before injected context."
  :type 'string
  :group 'hive-mcp-ellama)

(defcustom hive-mcp-ellama-ollama-host "localhost"
  "Ollama server host."
  :type 'string
  :group 'hive-mcp-ellama)

(defcustom hive-mcp-ellama-ollama-port 11434
  "Ollama server port."
  :type 'integer
  :group 'hive-mcp-ellama)

;;;; Internal State

(defvar hive-mcp-ellama--initialized nil
  "Whether the addon has been initialized.")

(defvar hive-mcp-ellama--last-prompt nil
  "The last prompt sent, for response context.")

(defvar hive-mcp-ellama--workers (make-hash-table :test 'equal)
  "Hash table of active Ollama swarm workers.
Key: worker-id, Value: plist with :model :buffer :status :task")

;;;; Ensure Dependencies

(defun hive-mcp-ellama--ensure-bridge ()
  "Ensure hive-mcp-ai-bridge is available."
  (or (featurep 'hive-mcp-ai-bridge)
      (require 'hive-mcp-ai-bridge nil t)))

(defun hive-mcp-ellama--ensure-ellama ()
  "Ensure ellama is available."
  (or (featurep 'ellama)
      (require 'ellama nil t)))

(defun hive-mcp-ellama--ensure-llm-ollama ()
  "Ensure llm-ollama is available for provider creation."
  (or (featurep 'llm-ollama)
      (require 'llm-ollama nil t)))

;;;; Provider Management

(defun hive-mcp-ellama-make-provider (&optional model)
  "Create an Ollama provider for MODEL.
MODEL defaults to `hive-mcp-ellama-default-model'."
  (when (hive-mcp-ellama--ensure-llm-ollama)
    (make-llm-ollama
     :chat-model (or model hive-mcp-ellama-default-model)
     :host hive-mcp-ellama-ollama-host
     :port hive-mcp-ellama-ollama-port)))

;;;; Context Injection

(defun hive-mcp-ellama--build-context-prompt (prompt)
  "Build a prompt with context injected.
PROMPT is the original user prompt."
  (if (and hive-mcp-ellama-inject-context
           (hive-mcp-ellama--ensure-bridge))
      (let ((context (hive-mcp-ai-build-context
                      (when hive-mcp-ellama-include-semantic
                        prompt))))
        (if context
            (concat (hive-mcp-ai-format-context-for 'ellama context)
                    hive-mcp-ellama-context-prefix
                    prompt)
          prompt))
    prompt))

;;;; Response Handling

(defun hive-mcp-ellama--handle-response (response)
  "Handle ellama RESPONSE after completion."
  (when (hive-mcp-ellama--ensure-bridge)
    ;; Log interaction
    (when hive-mcp-ellama-log-interactions
      (hive-mcp-ai-log-interaction
       "ellama" "response"
       (list :prompt-length (length (or hive-mcp-ellama--last-prompt ""))
             :response-length (length response)
             :model hive-mcp-ellama-default-model)))

    ;; Store notable responses
    (when hive-mcp-ellama-store-responses
      (hive-mcp-ai-store-response response "ellama"))

    ;; Run post-response hooks
    (hive-mcp-ai-run-post-response-hooks "ellama" response)))

;;;; Core Functions

;;;###autoload
(defun hive-mcp-ellama-dispatch (prompt &optional model callback)
  "Send PROMPT to Ollama via ellama.
MODEL overrides `hive-mcp-ellama-default-model'.
CALLBACK is called with the response when complete."
  (interactive "sPrompt: ")
  (when (hive-mcp-ellama--ensure-ellama)
    (setq hive-mcp-ellama--last-prompt prompt)
    (let ((ellama-provider (hive-mcp-ellama-make-provider model))
          (ellama-chat-done-callback
           (lambda (response)
             (hive-mcp-ellama--handle-response response)
             (when callback
               (funcall callback response)))))
      (ellama-chat prompt))))

;;;###autoload
(defun hive-mcp-ellama-dispatch-with-context (prompt &optional model callback)
  "Send PROMPT to Ollama with memory context injected.
MODEL overrides `hive-mcp-ellama-default-model'.
CALLBACK is called with the response when complete."
  (interactive "sPrompt: ")
  (let ((augmented-prompt (hive-mcp-ellama--build-context-prompt prompt)))
    (hive-mcp-ellama-dispatch augmented-prompt model callback)))

;;;###autoload
(defun hive-mcp-ellama-select-model ()
  "Interactively select an Ollama model."
  (interactive)
  (let* ((choices (mapcar (lambda (m)
                            (format "%s - %s" (car m) (cdr m)))
                          hive-mcp-ellama-models))
         (choice (completing-read "Model: " choices nil t))
         (model (car (split-string choice " - "))))
    (setq hive-mcp-ellama-default-model model)
    (message "Switched to model: %s" model)))

;;;; Swarm Backend Functions

(defun hive-mcp-ellama-swarm-spawn (worker-id name &optional model)
  "Spawn an Ollama swarm worker with WORKER-ID and NAME.
MODEL overrides `hive-mcp-ellama-default-model'.
Returns the worker-id on success."
  (let ((actual-model (or model hive-mcp-ellama-default-model)))
    (puthash worker-id
             (list :worker-id worker-id
                   :name name
                   :model actual-model
                   :status 'idle
                   :task nil
                   :result nil
                   :spawned-at (format-time-string "%FT%T%z"))
             hive-mcp-ellama--workers)
    (message "[ellama-swarm] Spawned worker %s with model %s" worker-id actual-model)
    worker-id))

(defun hive-mcp-ellama-swarm-dispatch (worker-id prompt &optional callback)
  "Dispatch PROMPT to Ollama swarm WORKER-ID.
CALLBACK is called with the result when complete.
Returns a task-id."
  (let* ((worker (gethash worker-id hive-mcp-ellama--workers))
         (model (plist-get worker :model))
         (task-id (format "ollama-task-%s-%d" worker-id (floor (float-time)))))
    (unless worker
      (error "Worker not found: %s" worker-id))
    ;; Update worker status
    (plist-put worker :status 'working)
    (plist-put worker :task task-id)
    ;; Dispatch to ellama
    (hive-mcp-ellama-dispatch-with-context
     prompt
     model
     (lambda (response)
       ;; Update worker with result
       (plist-put worker :status 'idle)
       (plist-put worker :result response)
       (plist-put worker :task nil)
       (when callback
         (funcall callback response))))
    task-id))

(defun hive-mcp-ellama-swarm-status ()
  "Get status of all Ollama swarm workers."
  (let ((workers '()))
    (maphash (lambda (id worker)
               (push (list :worker-id id
                           :name (plist-get worker :name)
                           :model (plist-get worker :model)
                           :status (plist-get worker :status)
                           :task (plist-get worker :task))
                     workers))
             hive-mcp-ellama--workers)
    (list :total (hash-table-count hive-mcp-ellama--workers)
          :workers (nreverse workers))))

(defun hive-mcp-ellama-swarm-kill (worker-id)
  "Kill Ollama swarm worker WORKER-ID."
  (remhash worker-id hive-mcp-ellama--workers)
  (message "[ellama-swarm] Killed worker %s" worker-id))

(defun hive-mcp-ellama-swarm-kill-all ()
  "Kill all Ollama swarm workers."
  (let ((count (hash-table-count hive-mcp-ellama--workers)))
    (clrhash hive-mcp-ellama--workers)
    (message "[ellama-swarm] Killed %d workers" count)))

;;;; MCP Tool Wrappers (for Ollama agents)

(defun hive-mcp-ellama--tool-read (path)
  "Read file at PATH via MCP.
Used by Ollama workers for file operations."
  (when (fboundp 'hive-mcp-api-call)
    (hive-mcp-api-call "read_file" `(:path ,path))))

(defun hive-mcp-ellama--tool-edit (path old-string new-string)
  "Edit file at PATH, replacing OLD-STRING with NEW-STRING via MCP."
  (when (fboundp 'hive-mcp-api-call)
    (hive-mcp-api-call "file_edit"
                       `(:file_path ,path
                         :old_string ,old-string
                         :new_string ,new-string))))

(defun hive-mcp-ellama--tool-bash (command)
  "Run shell COMMAND via MCP."
  (when (fboundp 'hive-mcp-api-call)
    (hive-mcp-api-call "bash" `(:command ,command))))

;;;; Interactive Commands

;;;###autoload
(defun hive-mcp-ellama-chat-with-context ()
  "Start ellama chat with memory context injection."
  (interactive)
  (let ((prompt (read-string "Chat with context: ")))
    (hive-mcp-ellama-dispatch-with-context prompt)))

;;;###autoload
(defun hive-mcp-ellama-query-memory (query)
  "Query memory with QUERY and insert results at point."
  (interactive "sQuery memory: ")
  (when (hive-mcp-ellama--ensure-bridge)
    (let ((results (hive-mcp-ai-build-context query)))
      (if results
          (insert results)
        (message "No relevant memory found")))))

;;;###autoload
(defun hive-mcp-ellama-store-region (beg end)
  "Store region between BEG and END as memory snippet."
  (interactive "r")
  (when (hive-mcp-ellama--ensure-bridge)
    (let ((content (buffer-substring-no-properties beg end)))
      (hive-mcp-ai-store-response content "ellama-manual"
                                  '("user-selected"))
      (message "Stored selection to memory"))))

;;;; Minor Mode

(defun hive-mcp-ellama--setup ()
  "Set up ellama integration."
  (when (hive-mcp-ellama--ensure-ellama)
    ;; Set default provider if not already set
    (unless (bound-and-true-p ellama-provider)
      (setq ellama-provider (hive-mcp-ellama-make-provider)))
    ;; Hook into ellama completion
    (advice-add 'ellama-chat :around #'hive-mcp-ellama--chat-advice)))

(defun hive-mcp-ellama--teardown ()
  "Tear down ellama integration."
  (advice-remove 'ellama-chat #'hive-mcp-ellama--chat-advice))

(defun hive-mcp-ellama--chat-advice (orig-fun prompt &rest args)
  "Advice for ellama-chat to inject context.
ORIG-FUN is the original function, PROMPT is the user prompt, ARGS are extra args."
  (setq hive-mcp-ellama--last-prompt prompt)
  (let ((augmented (if hive-mcp-ellama-inject-context
                       (hive-mcp-ellama--build-context-prompt prompt)
                     prompt)))
    (apply orig-fun augmented args)))

;;;###autoload
(define-minor-mode hive-mcp-ellama-mode
  "Minor mode for hive-mcp integration with ellama/Ollama.

When enabled:
- Memory context is injected into prompts before sending
- Notable responses are stored to memory
- Interactions are logged for audit
- Swarm backend available for two-tier orchestration"
  :init-value nil
  :lighter " MCP-ELL"
  :global t
  :group 'hive-mcp-ellama
  (if hive-mcp-ellama-mode
      (progn
        (hive-mcp-ellama--setup)
        (setq hive-mcp-ellama--initialized t)
        (message "hive-mcp-ellama: enabled"))
    (hive-mcp-ellama--teardown)
    (message "hive-mcp-ellama: disabled")))

;;;; Transient Menu (optional)

(with-eval-after-load 'transient
  (transient-define-prefix hive-mcp-ellama-menu ()
    "MCP integration menu for ellama/Ollama."
    ["hive-mcp + ellama"
     ["Chat"
      ("c" "Chat with context" hive-mcp-ellama-chat-with-context)
      ("q" "Query memory" hive-mcp-ellama-query-memory)
      ("m" "Select model" hive-mcp-ellama-select-model)]
     ["Store"
      ("s" "Store region" hive-mcp-ellama-store-region)]
     ["Swarm"
      ("S" "Swarm status" (lambda () (interactive) (message "%s" (hive-mcp-ellama-swarm-status))))
      ("K" "Kill all workers" hive-mcp-ellama-swarm-kill-all)]
     ["Toggle"
      ("t" "Toggle mode" hive-mcp-ellama-mode)]]))

;;;; Addon Lifecycle

(defun hive-mcp-ellama--addon-init ()
  "Initialize ellama addon."
  (require 'hive-mcp-ai-bridge nil t)
  (message "hive-mcp-ellama: initialized"))

(defun hive-mcp-ellama--addon-shutdown ()
  "Shutdown ellama addon."
  (when hive-mcp-ellama-mode
    (hive-mcp-ellama-mode -1))
  (hive-mcp-ellama-swarm-kill-all)
  (message "hive-mcp-ellama: shutdown complete"))

;;;; Registration

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'ellama
   :version "0.1.0"
   :description "Ollama integration via ellama for two-tier swarm"
   :requires '(hive-mcp-ai-bridge)
   :provides '(hive-mcp-ellama-mode)
   :init #'hive-mcp-ellama--addon-init
   :shutdown #'hive-mcp-ellama--addon-shutdown))

(provide 'hive-mcp-ellama)
;;; hive-mcp-ellama.el ends here
