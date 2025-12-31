;;; emacs-mcp-chroma.el --- Chroma vector database integration for emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, memory, semantic-search, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates Chroma vector database with emacs-mcp for
;; semantic memory search capabilities.
;;
;; Features:
;; - Automatic docker-compose startup for Chroma DB
;; - Semantic search across project memory entries
;; - Fallback to local notes when Chroma is unavailable
;; - Ollama integration for local embeddings (no API keys needed)
;;
;; Requirements:
;; - Docker and docker-compose installed
;; - Ollama installed with nomic-embed-text model (optional but recommended)
;;   Run: ollama pull nomic-embed-text
;;
;; Usage:
;;   (emacs-mcp-addon-load 'chroma)
;;   M-x emacs-mcp-chroma-transient
;;
;; The addon will:
;; 1. Start the Chroma container via docker-compose
;; 2. Configure the MCP server to use Chroma for memory
;; 3. Fall back to local memory if Chroma is unavailable

;;; Code:

(require 'emacs-mcp-api)

;; Forward declarations
(declare-function emacs-mcp-addon-register "emacs-mcp-addons")
(declare-function transient-define-prefix "transient")
(declare-function emacs-mcp--send-request "emacs-mcp")

;;;; Customization:

(defgroup emacs-mcp-chroma nil
  "Chroma vector database integration for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-chroma-")

(defcustom emacs-mcp-chroma-docker-compose-file nil
  "Path to docker-compose.yml for Chroma.
If nil, uses the default location in the emacs-mcp project."
  :type '(choice (const nil) file)
  :group 'emacs-mcp-chroma)

(defcustom emacs-mcp-chroma-host "localhost"
  "Chroma server host."
  :type 'string
  :group 'emacs-mcp-chroma)

(defcustom emacs-mcp-chroma-port 8000
  "Chroma server port."
  :type 'integer
  :group 'emacs-mcp-chroma)

(defcustom emacs-mcp-chroma-auto-start t
  "When non-nil, automatically start Chroma on addon initialization."
  :type 'boolean
  :group 'emacs-mcp-chroma)

(defcustom emacs-mcp-chroma-embedding-provider 'ollama
  "Embedding provider to use for vector search.
Options are `ollama' (local), `mock' (testing), or `none' (disabled)."
  :type '(choice (const :tag "Ollama (local)" ollama)
                 (const :tag "Mock (testing)" mock)
                 (const :tag "Disabled" none))
  :group 'emacs-mcp-chroma)

(defcustom emacs-mcp-chroma-ollama-model "nomic-embed-text"
  "Ollama model to use for embeddings."
  :type 'string
  :group 'emacs-mcp-chroma)

(defcustom emacs-mcp-chroma-startup-timeout 30
  "Timeout in seconds to wait for Chroma to become healthy."
  :type 'integer
  :group 'emacs-mcp-chroma)

;;;; Internal Variables:

(defvar emacs-mcp-chroma--process nil
  "Process handle for docker-compose.")

(defvar emacs-mcp-chroma--status 'unknown
  "Current Chroma status: `running', `stopped', `starting', or `unknown'.")

(defvar emacs-mcp-chroma--fallback-active nil
  "When non-nil, using fallback local memory instead of Chroma.")

(defvar emacs-mcp-chroma--project-root nil
  "Cached path to the emacs-mcp project root.")

;;;; Helper Functions:

(defun emacs-mcp-chroma--project-root ()
  "Find the emacs-mcp project root directory."
  (or emacs-mcp-chroma--project-root
      (setq emacs-mcp-chroma--project-root
            (let ((load-path-dir (file-name-directory (or load-file-name
                                                           buffer-file-name
                                                           default-directory))))
              ;; Go up from elisp/addons/ to project root
              (expand-file-name "../.." load-path-dir)))))

(defun emacs-mcp-chroma--docker-compose-file ()
  "Return the path to docker-compose.yml."
  (or emacs-mcp-chroma-docker-compose-file
      (expand-file-name "docker-compose.yml"
                        (emacs-mcp-chroma--project-root))))

(defun emacs-mcp-chroma--docker-available-p ()
  "Check if docker is available."
  (zerop (call-process "docker" nil nil nil "info")))

(defun emacs-mcp-chroma--compose-available-p ()
  "Check if docker-compose is available."
  (or (zerop (call-process "docker" nil nil nil "compose" "version"))
      (zerop (call-process "docker-compose" nil nil nil "--version"))))

(defun emacs-mcp-chroma--ollama-available-p ()
  "Check if Ollama is available and has the embedding model."
  (and (zerop (call-process "ollama" nil nil nil "list"))
       (let ((output (shell-command-to-string "ollama list")))
         (string-match-p emacs-mcp-chroma-ollama-model output))))

(defun emacs-mcp-chroma--health-check ()
  "Check if Chroma is responding to health checks."
  (condition-case nil
      (let ((url (format "http://%s:%d/api/v2/heartbeat"
                         emacs-mcp-chroma-host
                         emacs-mcp-chroma-port)))
        (with-current-buffer (url-retrieve-synchronously url t nil 5)
          (goto-char (point-min))
          (search-forward "\n\n" nil t)
          (let ((json-object-type 'plist))
            (ignore-errors (json-read)))))
    (error nil)))

;;;; Docker-Compose Management:

(defun emacs-mcp-chroma--compose-command ()
  "Return the docker-compose command to use."
  (if (zerop (call-process "docker" nil nil nil "compose" "version"))
      '("docker" "compose")
    '("docker-compose")))

(defun emacs-mcp-chroma-start ()
  "Start the Chroma container via docker-compose."
  (interactive)
  (let ((compose-file (emacs-mcp-chroma--docker-compose-file)))
    (unless (file-exists-p compose-file)
      (error "docker-compose.yml not found at %s" compose-file))
    (unless (emacs-mcp-chroma--docker-available-p)
      (error "Docker is not available"))
    (unless (emacs-mcp-chroma--compose-available-p)
      (error "docker-compose is not available"))

    (setq emacs-mcp-chroma--status 'starting)
    (message "Starting Chroma container...")

    (let* ((default-directory (file-name-directory compose-file))
           (cmd (emacs-mcp-chroma--compose-command))
           (args (append (cdr cmd) (list "-f" compose-file "up" "-d"))))
      (setq emacs-mcp-chroma--process
            (apply #'start-process "emacs-mcp-chroma"
                   "*emacs-mcp-chroma*"
                   (car cmd) args))

      (set-process-sentinel
       emacs-mcp-chroma--process
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (if (zerop (process-exit-status proc))
               (emacs-mcp-chroma--wait-for-healthy)
             (setq emacs-mcp-chroma--status 'stopped)
             (message "Failed to start Chroma container"))))))))

(defun emacs-mcp-chroma--wait-for-healthy ()
  "Wait for Chroma to become healthy, then configure."
  (let ((attempts 0)
        (max-attempts (/ emacs-mcp-chroma-startup-timeout 2)))
    (run-with-timer
     2 2
     (lambda ()
       (setq attempts (1+ attempts))
       (cond
        ((emacs-mcp-chroma--health-check)
         (setq emacs-mcp-chroma--status 'running)
         (setq emacs-mcp-chroma--fallback-active nil)
         (emacs-mcp-chroma--configure-mcp)
         (message "Chroma is running and connected"))
        ((>= attempts max-attempts)
         (setq emacs-mcp-chroma--status 'stopped)
         (setq emacs-mcp-chroma--fallback-active t)
         (message "Chroma failed to start, using local memory fallback")))))))

(defun emacs-mcp-chroma-stop ()
  "Stop the Chroma container."
  (interactive)
  (let* ((compose-file (emacs-mcp-chroma--docker-compose-file))
         (default-directory (file-name-directory compose-file))
         (cmd (emacs-mcp-chroma--compose-command))
         (args (append (cdr cmd) (list "-f" compose-file "down"))))
    (apply #'call-process (car cmd) nil nil nil args)
    (setq emacs-mcp-chroma--status 'stopped)
    (message "Chroma container stopped")))

(defun emacs-mcp-chroma-restart ()
  "Restart the Chroma container."
  (interactive)
  (emacs-mcp-chroma-stop)
  (sleep-for 1)
  (emacs-mcp-chroma-start))

;;;; MCP Configuration:

(defun emacs-mcp-chroma--configure-mcp ()
  "Configure the MCP server to use Chroma."
  (when (fboundp 'emacs-mcp--send-request)
    (emacs-mcp--send-request
     "tools/call"
     `(:name "cider_eval_silent"
       :arguments (:code ,(format "(do
         (require '[emacs-mcp.chroma :as chroma])
         (chroma/configure! {:host \"%s\" :port %d})
         %s
         :configured)"
                                  emacs-mcp-chroma-host
                                  emacs-mcp-chroma-port
                                  (pcase emacs-mcp-chroma-embedding-provider
                                    ('ollama (format "(require '[emacs-mcp.embeddings.ollama :as ollama])
                                                      (chroma/set-embedding-provider!
                                                        (ollama/->OllamaEmbedder \"%s\"))"
                                                     emacs-mcp-chroma-ollama-model))
                                    ('mock "(chroma/set-embedding-provider! (chroma/->MockEmbedder))")
                                    (_ "")))))
     (lambda (response)
       (if (plist-get response :error)
           (message "Failed to configure Chroma: %s" (plist-get response :error))
         (message "Chroma configured with %s embeddings"
                  emacs-mcp-chroma-embedding-provider))))))

;;;; Semantic Search API:

(defun emacs-mcp-chroma-search (query &optional limit type)
  "Search memory entries semantically for QUERY.
LIMIT is max results (default 10).
TYPE filters by memory type (note, snippet, convention, decision)."
  (interactive "sSearch query: ")
  (let ((limit (or limit 10)))
    (if (and (not emacs-mcp-chroma--fallback-active)
             (eq emacs-mcp-chroma--status 'running))
        ;; Use Chroma semantic search
        (emacs-mcp-chroma--semantic-search query limit type)
      ;; Fallback to local text search
      (emacs-mcp-chroma--local-search query limit type))))

(defun emacs-mcp-chroma--semantic-search (query limit type)
  "Perform semantic search via Chroma."
  (when (fboundp 'emacs-mcp--send-request)
    (emacs-mcp--send-request
     "tools/call"
     `(:name "cider_eval_silent"
       :arguments (:code ,(format "(do
         (require '[emacs-mcp.chroma :as chroma])
         (chroma/search-similar %S :limit %d %s))"
                                  query limit
                                  (if type (format ":type %S" type) ""))))
     (lambda (response)
       (if (plist-get response :error)
           (progn
             (message "Semantic search failed, falling back to local: %s"
                      (plist-get response :error))
             (emacs-mcp-chroma--local-search query limit type))
         (emacs-mcp-chroma--display-results
          (plist-get response :result)
          "Semantic Search"))))))

(defun emacs-mcp-chroma--local-search (query limit type)
  "Fallback local search using mcp_memory_query."
  (when (fboundp 'emacs-mcp--send-request)
    (emacs-mcp--send-request
     "tools/call"
     `(:name "mcp_memory_query"
       :arguments (:type ,(or type "note")
                   :limit ,(or limit 20)))
     (lambda (response)
       (let* ((entries (plist-get response :result))
              (filtered (seq-filter
                         (lambda (entry)
                           (string-match-p (regexp-quote query)
                                           (or (plist-get entry :content) "")))
                         entries)))
         (emacs-mcp-chroma--display-results
          (seq-take filtered (or limit 10))
          "Local Search (fallback)"))))))

(defun emacs-mcp-chroma--display-results (results source)
  "Display search RESULTS from SOURCE in a buffer."
  (let ((buf (get-buffer-create "*MCP Memory Search*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "=== Memory Search Results (%s) ===\n\n" source))
        (if (null results)
            (insert "No results found.\n")
          (dolist (result results)
            (let ((id (or (plist-get result :id) "unknown"))
                  (doc (or (plist-get result :document)
                           (plist-get result :content) ""))
                  (distance (plist-get result :distance))
                  (metadata (plist-get result :metadata)))
              (insert (format "--- %s ---\n" id))
              (when distance
                (insert (format "  Distance: %.2f\n" distance)))
              (when metadata
                (insert (format "  Type: %s\n" (plist-get metadata :type))))
              (insert (format "  %s\n\n"
                              (if (> (length doc) 200)
                                  (concat (substring doc 0 200) "...")
                                doc))))))
        (goto-char (point-min))))
    (display-buffer buf)))

;;;; Index Management:

(defun emacs-mcp-chroma-index-all ()
  "Index all existing memory entries in Chroma."
  (interactive)
  (unless (eq emacs-mcp-chroma--status 'running)
    (error "Chroma is not running"))
  (when (fboundp 'emacs-mcp--send-request)
    (message "Indexing all memory entries...")
    (emacs-mcp--send-request
     "tools/call"
     `(:name "cider_eval_silent"
       :arguments (:code "(do
         (require '[emacs-mcp.chroma :as chroma])
         (require '[emacs-mcp.memory :as memory])
         (let [entries (memory/get-all-entries)]
           (chroma/index-memory-entries! entries)
           (count entries)))"))
     (lambda (response)
       (if (plist-get response :error)
           (message "Failed to index: %s" (plist-get response :error))
         (message "Indexed %s memory entries" (plist-get response :result)))))))

;;;; Status and Info:

(defun emacs-mcp-chroma-status ()
  "Display Chroma integration status."
  (interactive)
  (let ((buf (get-buffer-create "*MCP Chroma Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Chroma Integration Status ===\n\n")
        (insert (format "Status: %s\n" emacs-mcp-chroma--status))
        (insert (format "Fallback Active: %s\n"
                        (if emacs-mcp-chroma--fallback-active "Yes" "No")))
        (insert (format "Host: %s:%d\n"
                        emacs-mcp-chroma-host emacs-mcp-chroma-port))
        (insert (format "Embedding Provider: %s\n"
                        emacs-mcp-chroma-embedding-provider))
        (insert "\n--- Checks ---\n")
        (insert (format "Docker: %s\n"
                        (if (emacs-mcp-chroma--docker-available-p) "OK" "Not found")))
        (insert (format "docker-compose: %s\n"
                        (if (emacs-mcp-chroma--compose-available-p) "OK" "Not found")))
        (insert (format "Ollama: %s\n"
                        (if (emacs-mcp-chroma--ollama-available-p) "OK" "Not found")))
        (insert (format "Chroma health: %s\n"
                        (if (emacs-mcp-chroma--health-check) "OK" "Not responding")))
        (insert (format "docker-compose.yml: %s\n"
                        (if (file-exists-p (emacs-mcp-chroma--docker-compose-file))
                            "Found" "Not found")))
        (goto-char (point-min))))
    (display-buffer buf)))

;;;; Interactive Commands:

;;;###autoload
(defun emacs-mcp-chroma-transient ()
  "MCP Chroma menu."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (transient-define-prefix emacs-mcp-chroma--menu ()
          "MCP Chroma menu."
          ["emacs-mcp + Chroma"
           ["Container"
            ("s" "Start" emacs-mcp-chroma-start)
            ("S" "Stop" emacs-mcp-chroma-stop)
            ("r" "Restart" emacs-mcp-chroma-restart)
            ("i" "Status" emacs-mcp-chroma-status)]
           ["Search"
            ("/" "Semantic search" emacs-mcp-chroma-search)
            ("I" "Index all entries" emacs-mcp-chroma-index-all)]])
        (emacs-mcp-chroma--menu))
    (message "Transient not available")))

;;;; Minor Mode:

(defvar emacs-mcp-chroma-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m v /") #'emacs-mcp-chroma-search)
    (define-key map (kbd "C-c m v s") #'emacs-mcp-chroma-status)
    (define-key map (kbd "C-c m v v") #'emacs-mcp-chroma-transient)
    map)
  "Keymap for `emacs-mcp-chroma-mode'.")

;;;###autoload
(define-minor-mode emacs-mcp-chroma-mode
  "Minor mode for Chroma vector database integration."
  :init-value nil
  :lighter " MCP-Chroma"
  :global t
  :keymap emacs-mcp-chroma-mode-map
  :group 'emacs-mcp-chroma
  (if emacs-mcp-chroma-mode
      (message "emacs-mcp-chroma enabled")
    (message "emacs-mcp-chroma disabled")))

;;;; Addon Lifecycle:

(defun emacs-mcp-chroma--addon-init ()
  "Initialize Chroma addon using bb environment check script."
  (require 'emacs-mcp-api nil t)
  (require 'json)
  (require 'url)

  ;; Use bb script to check and optionally start environment
  (let* ((script-path (expand-file-name "scripts/chroma-check.bb"
                                         (emacs-mcp-chroma--project-root)))
         (status-json (when (file-exists-p script-path)
                        (shell-command-to-string
                         (format "bb %s status 2>/dev/null" script-path))))
         (status (when (and status-json (not (string-empty-p status-json)))
                   (ignore-errors (json-read-from-string status-json)))))

    (if status
        (let ((docker (alist-get 'docker status))
              (compose (alist-get 'docker-compose status))
              (ollama (alist-get 'ollama status))
              (model (alist-get 'ollama-model status))
              (chroma (alist-get 'chroma-running status)))

          ;; Set provider based on availability
          (setq emacs-mcp-chroma-embedding-provider
                (cond
                 ((and ollama model) 'ollama)
                 (t 'mock)))

          ;; Update status
          (setq emacs-mcp-chroma--status
                (if chroma 'running 'stopped))
          (setq emacs-mcp-chroma--fallback-active (not chroma))

          ;; Auto-start if configured and able
          (when (and emacs-mcp-chroma-auto-start
                     docker compose
                     (not chroma))
            (message "emacs-mcp-chroma: Starting Chroma via bb script...")
            (let ((result (shell-command-to-string
                           (format "bb %s start 2>&1" script-path))))
              (when (string-match-p "healthy" result)
                (setq emacs-mcp-chroma--status 'running)
                (setq emacs-mcp-chroma--fallback-active nil))))

          ;; Configure MCP if running
          (when (eq emacs-mcp-chroma--status 'running)
            (emacs-mcp-chroma--configure-mcp))

          ;; Inform user about status
          (cond
           ((eq emacs-mcp-chroma--status 'running)
            (message "emacs-mcp-chroma: Semantic search enabled (Chroma + %s)"
                     emacs-mcp-chroma-embedding-provider))
           ((not docker)
            (message "emacs-mcp-chroma: Docker not available, using local memory fallback")
            (message "  See: https://github.com/BuddhiLW/emacs-mcp#semantic-memory-search"))
           ((not ollama)
            (message "emacs-mcp-chroma: Ollama not installed, using mock embeddings")
            (message "  Install: curl -fsSL https://ollama.com/install.sh | sh"))
           (t
            (message "emacs-mcp-chroma: Using local memory fallback"))))

      ;; No bb script found
      (message "emacs-mcp-chroma: bb script not found, manual setup required"))))

(defun emacs-mcp-chroma--addon-shutdown ()
  "Shutdown Chroma addon."
  (when emacs-mcp-chroma-mode
    (emacs-mcp-chroma-mode -1))
  ;; Don't stop the container on shutdown (it's persistent)
  (message "emacs-mcp-chroma: shutdown"))

;;;; Addon Registration:

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'chroma
   :version "0.1.0"
   :description "Chroma vector database for semantic memory search"
   :requires '(emacs-mcp-api)
   :provides '(emacs-mcp-chroma-mode emacs-mcp-chroma-transient)
   :init #'emacs-mcp-chroma--addon-init
   :shutdown #'emacs-mcp-chroma--addon-shutdown))

(provide 'emacs-mcp-chroma)
;;; emacs-mcp-chroma.el ends here
