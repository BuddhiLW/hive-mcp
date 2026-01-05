;;; hive-mcp-chroma.el --- Chroma vector database integration for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, memory, semantic-search, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates Chroma vector database with hive-mcp for
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
;;   (hive-mcp-addon-load 'chroma)
;;   M-x hive-mcp-chroma-transient
;;
;; The addon will:
;; 1. Start the Chroma container via docker-compose
;; 2. Configure the MCP server to use Chroma for memory
;; 3. Fall back to local memory if Chroma is unavailable

;;; Code:

(require 'hive-mcp-api)

;; Forward declarations
(declare-function hive-mcp-addon-register "hive-mcp-addons")
(declare-function transient-define-prefix "transient")
(declare-function hive-mcp--send-request "hive-mcp")

;;;; Customization:

(defgroup hive-mcp-chroma nil
  "Chroma vector database integration for hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-chroma-")

(defcustom hive-mcp-chroma-docker-compose-file nil
  "Path to docker-compose.yml for Chroma.
If nil, uses the default location in the hive-mcp project."
  :type '(choice (const nil) file)
  :group 'hive-mcp-chroma)

(defcustom hive-mcp-chroma-host "localhost"
  "Chroma server host."
  :type 'string
  :group 'hive-mcp-chroma)

(defcustom hive-mcp-chroma-port 8000
  "Chroma server port."
  :type 'integer
  :group 'hive-mcp-chroma)

(defcustom hive-mcp-chroma-auto-start t
  "When non-nil, automatically start Chroma on addon initialization."
  :type 'boolean
  :group 'hive-mcp-chroma)

(defcustom hive-mcp-chroma-embedding-provider 'ollama
  "Embedding provider to use for vector search.
Options are `ollama' (local), `mock' (testing), or `none' (disabled)."
  :type '(choice (const :tag "Ollama (local)" ollama)
                 (const :tag "Mock (testing)" mock)
                 (const :tag "Disabled" none))
  :group 'hive-mcp-chroma)

(defcustom hive-mcp-chroma-ollama-model "nomic-embed-text"
  "Ollama model to use for embeddings."
  :type 'string
  :group 'hive-mcp-chroma)

(defcustom hive-mcp-chroma-startup-timeout 30
  "Timeout in seconds to wait for Chroma to become healthy."
  :type 'integer
  :group 'hive-mcp-chroma)

;;;; Internal Variables:

(defvar hive-mcp-chroma--process nil
  "Process handle for docker-compose.")

(defvar hive-mcp-chroma--status 'unknown
  "Current Chroma status: `running', `stopped', `starting', or `unknown'.")

(defvar hive-mcp-chroma--fallback-active nil
  "When non-nil, using fallback local memory instead of Chroma.")

(defvar hive-mcp-chroma--project-root nil
  "Cached path to the hive-mcp project root.")

;;;; Helper Functions:

(defun hive-mcp-chroma--project-root ()
  "Find the hive-mcp project root directory."
  (or hive-mcp-chroma--project-root
      (setq hive-mcp-chroma--project-root
            (let ((load-path-dir (file-name-directory (or load-file-name
                                                           buffer-file-name
                                                           default-directory))))
              ;; Go up from elisp/addons/ to project root
              (expand-file-name "../.." load-path-dir)))))

(defun hive-mcp-chroma--docker-compose-file ()
  "Return the path to docker-compose.yml."
  (or hive-mcp-chroma-docker-compose-file
      (expand-file-name "docker-compose.yml"
                        (hive-mcp-chroma--project-root))))

(defun hive-mcp-chroma--docker-available-p ()
  "Check if docker is available."
  (zerop (call-process "docker" nil nil nil "info")))

(defun hive-mcp-chroma--compose-available-p ()
  "Check if docker-compose is available."
  (or (zerop (call-process "docker" nil nil nil "compose" "version"))
      (zerop (call-process "docker-compose" nil nil nil "--version"))))

(defun hive-mcp-chroma--ollama-available-p ()
  "Check if Ollama is available and has the embedding model."
  (and (zerop (call-process "ollama" nil nil nil "list"))
       (let ((output (shell-command-to-string "ollama list")))
         (string-match-p hive-mcp-chroma-ollama-model output))))

(defun hive-mcp-chroma--health-check ()
  "Check if Chroma is responding to health checks."
  (condition-case nil
      (let ((url (format "http://%s:%d/api/v2/heartbeat"
                         hive-mcp-chroma-host
                         hive-mcp-chroma-port)))
        (with-current-buffer (url-retrieve-synchronously url t nil 5)
          (goto-char (point-min))
          (search-forward "\n\n" nil t)
          (let ((json-object-type 'plist))
            (ignore-errors (json-read)))))
    (error nil)))

;;;; Docker-Compose Management:

(defun hive-mcp-chroma--compose-command ()
  "Return the docker-compose command to use."
  (if (zerop (call-process "docker" nil nil nil "compose" "version"))
      '("docker" "compose")
    '("docker-compose")))

(defun hive-mcp-chroma-start ()
  "Start the Chroma container via docker-compose."
  (interactive)
  (let ((compose-file (hive-mcp-chroma--docker-compose-file)))
    (unless (file-exists-p compose-file)
      (error "docker-compose.yml not found at %s" compose-file))
    (unless (hive-mcp-chroma--docker-available-p)
      (error "Docker is not available"))
    (unless (hive-mcp-chroma--compose-available-p)
      (error "docker-compose is not available"))

    (setq hive-mcp-chroma--status 'starting)
    (message "Starting Chroma container...")

    (let* ((default-directory (file-name-directory compose-file))
           (cmd (hive-mcp-chroma--compose-command))
           (args (append (cdr cmd) (list "-f" compose-file "up" "-d"))))
      (setq hive-mcp-chroma--process
            (apply #'start-process "hive-mcp-chroma"
                   "*hive-mcp-chroma*"
                   (car cmd) args))

      (set-process-sentinel
       hive-mcp-chroma--process
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (if (zerop (process-exit-status proc))
               (hive-mcp-chroma--wait-for-healthy)
             (setq hive-mcp-chroma--status 'stopped)
             (message "Failed to start Chroma container"))))))))

(defun hive-mcp-chroma--wait-for-healthy ()
  "Wait for Chroma to become healthy, then configure."
  (let ((attempts 0)
        (max-attempts (/ hive-mcp-chroma-startup-timeout 2)))
    (run-with-timer
     2 2
     (lambda ()
       (setq attempts (1+ attempts))
       (cond
        ((hive-mcp-chroma--health-check)
         (setq hive-mcp-chroma--status 'running)
         (setq hive-mcp-chroma--fallback-active nil)
         (hive-mcp-chroma--configure-mcp)
         (message "Chroma is running and connected"))
        ((>= attempts max-attempts)
         (setq hive-mcp-chroma--status 'stopped)
         (setq hive-mcp-chroma--fallback-active t)
         (message "Chroma failed to start, using local memory fallback")))))))

(defun hive-mcp-chroma-stop ()
  "Stop the Chroma container."
  (interactive)
  (let* ((compose-file (hive-mcp-chroma--docker-compose-file))
         (default-directory (file-name-directory compose-file))
         (cmd (hive-mcp-chroma--compose-command))
         (args (append (cdr cmd) (list "-f" compose-file "down"))))
    (apply #'call-process (car cmd) nil nil nil args)
    (setq hive-mcp-chroma--status 'stopped)
    (message "Chroma container stopped")))

(defun hive-mcp-chroma-restart ()
  "Restart the Chroma container."
  (interactive)
  (hive-mcp-chroma-stop)
  (sleep-for 1)
  (hive-mcp-chroma-start))

;;;; MCP Configuration:

(defun hive-mcp-chroma--configure-mcp ()
  "Configure the MCP server to use Chroma."
  (when (fboundp 'hive-mcp--send-request)
    (hive-mcp--send-request
     "tools/call"
     `(:name "cider_eval_silent"
       :arguments (:code ,(format "(do
         (require '[hive-mcp.chroma :as chroma])
         (chroma/configure! {:host \"%s\" :port %d})
         %s
         :configured)"
                                  hive-mcp-chroma-host
                                  hive-mcp-chroma-port
                                  (pcase hive-mcp-chroma-embedding-provider
                                    ('ollama (format "(require '[hive-mcp.embeddings.ollama :as ollama])
                                                      (chroma/set-embedding-provider!
                                                        (ollama/->OllamaEmbedder \"%s\"))"
                                                     hive-mcp-chroma-ollama-model))
                                    ('mock "(chroma/set-embedding-provider! (chroma/->MockEmbedder))")
                                    (_ "")))))
     (lambda (response)
       (if (plist-get response :error)
           (message "Failed to configure Chroma: %s" (plist-get response :error))
         (message "Chroma configured with %s embeddings"
                  hive-mcp-chroma-embedding-provider))))))

;;;; Semantic Search API:

(defun hive-mcp-chroma-search (query &optional limit type)
  "Search memory entries semantically for QUERY.
LIMIT is max results (default 10).
TYPE filters by memory type (note, snippet, convention, decision)."
  (interactive "sSearch query: ")
  (let ((limit (or limit 10)))
    (if (and (not hive-mcp-chroma--fallback-active)
             (eq hive-mcp-chroma--status 'running))
        ;; Use Chroma semantic search
        (hive-mcp-chroma--semantic-search query limit type)
      ;; Fallback to local text search
      (hive-mcp-chroma--local-search query limit type))))

(defun hive-mcp-chroma--semantic-search (query limit type)
  "Perform semantic search via Chroma."
  (when (fboundp 'hive-mcp--send-request)
    (hive-mcp--send-request
     "tools/call"
     `(:name "cider_eval_silent"
       :arguments (:code ,(format "(do
         (require '[hive-mcp.chroma :as chroma])
         (chroma/search-similar %S :limit %d %s))"
                                  query limit
                                  (if type (format ":type %S" type) ""))))
     (lambda (response)
       (if (plist-get response :error)
           (progn
             (message "Semantic search failed, falling back to local: %s"
                      (plist-get response :error))
             (hive-mcp-chroma--local-search query limit type))
         (hive-mcp-chroma--display-results
          (plist-get response :result)
          "Semantic Search"))))))

(defun hive-mcp-chroma--local-search (query limit type)
  "Fallback local search using mcp_memory_query."
  (when (fboundp 'hive-mcp--send-request)
    (hive-mcp--send-request
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
         (hive-mcp-chroma--display-results
          (seq-take filtered (or limit 10))
          "Local Search (fallback)"))))))

(defun hive-mcp-chroma--display-results (results source)
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

(defun hive-mcp-chroma-index-all ()
  "Index all existing memory entries in Chroma."
  (interactive)
  (unless (eq hive-mcp-chroma--status 'running)
    (error "Chroma is not running"))
  (when (fboundp 'hive-mcp--send-request)
    (message "Indexing all memory entries...")
    (hive-mcp--send-request
     "tools/call"
     `(:name "cider_eval_silent"
       :arguments (:code "(do
         (require '[hive-mcp.chroma :as chroma])
         (require '[hive-mcp.memory :as memory])
         (let [entries (memory/get-all-entries)]
           (chroma/index-memory-entries! entries)
           (count entries)))"))
     (lambda (response)
       (if (plist-get response :error)
           (message "Failed to index: %s" (plist-get response :error))
         (message "Indexed %s memory entries" (plist-get response :result)))))))

;;;; Status and Info:

(defun hive-mcp-chroma-status ()
  "Display Chroma integration status."
  (interactive)
  (let ((buf (get-buffer-create "*MCP Chroma Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Chroma Integration Status ===\n\n")
        (insert (format "Status: %s\n" hive-mcp-chroma--status))
        (insert (format "Fallback Active: %s\n"
                        (if hive-mcp-chroma--fallback-active "Yes" "No")))
        (insert (format "Host: %s:%d\n"
                        hive-mcp-chroma-host hive-mcp-chroma-port))
        (insert (format "Embedding Provider: %s\n"
                        hive-mcp-chroma-embedding-provider))
        (insert "\n--- Checks ---\n")
        (insert (format "Docker: %s\n"
                        (if (hive-mcp-chroma--docker-available-p) "OK" "Not found")))
        (insert (format "docker-compose: %s\n"
                        (if (hive-mcp-chroma--compose-available-p) "OK" "Not found")))
        (insert (format "Ollama: %s\n"
                        (if (hive-mcp-chroma--ollama-available-p) "OK" "Not found")))
        (insert (format "Chroma health: %s\n"
                        (if (hive-mcp-chroma--health-check) "OK" "Not responding")))
        (insert (format "docker-compose.yml: %s\n"
                        (if (file-exists-p (hive-mcp-chroma--docker-compose-file))
                            "Found" "Not found")))
        (goto-char (point-min))))
    (display-buffer buf)))

;;;; Interactive Commands:

;;;###autoload
(defun hive-mcp-chroma-transient ()
  "MCP Chroma menu."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (transient-define-prefix hive-mcp-chroma--menu ()
          "MCP Chroma menu."
          ["hive-mcp + Chroma"
           ["Container"
            ("s" "Start" hive-mcp-chroma-start)
            ("S" "Stop" hive-mcp-chroma-stop)
            ("r" "Restart" hive-mcp-chroma-restart)
            ("i" "Status" hive-mcp-chroma-status)]
           ["Search"
            ("/" "Semantic search" hive-mcp-chroma-search)
            ("I" "Index all entries" hive-mcp-chroma-index-all)]])
        (hive-mcp-chroma--menu))
    (message "Transient not available")))

;;;; Minor Mode:

(defvar hive-mcp-chroma-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m v /") #'hive-mcp-chroma-search)
    (define-key map (kbd "C-c m v s") #'hive-mcp-chroma-status)
    (define-key map (kbd "C-c m v v") #'hive-mcp-chroma-transient)
    map)
  "Keymap for `hive-mcp-chroma-mode'.")

;;;###autoload
(define-minor-mode hive-mcp-chroma-mode
  "Minor mode for Chroma vector database integration."
  :init-value nil
  :lighter " MCP-Chroma"
  :global t
  :keymap hive-mcp-chroma-mode-map
  :group 'hive-mcp-chroma
  (if hive-mcp-chroma-mode
      (message "hive-mcp-chroma enabled")
    (message "hive-mcp-chroma disabled")))

;;;; Addon Lifecycle:

(defun hive-mcp-chroma--addon-init ()
  "Initialize Chroma addon using bb environment check script."
  (require 'hive-mcp-api nil t)
  (require 'json)
  (require 'url)

  ;; Use bb script to check and optionally start environment
  (let* ((script-path (expand-file-name "scripts/chroma-check.bb"
                                         (hive-mcp-chroma--project-root)))
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
          (setq hive-mcp-chroma-embedding-provider
                (cond
                 ((and ollama model) 'ollama)
                 (t 'mock)))

          ;; Update status
          (setq hive-mcp-chroma--status
                (if chroma 'running 'stopped))
          (setq hive-mcp-chroma--fallback-active (not chroma))

          ;; Auto-start if configured and able
          (when (and hive-mcp-chroma-auto-start
                     docker compose
                     (not chroma))
            (message "hive-mcp-chroma: Starting Chroma via bb script...")
            (let ((result (shell-command-to-string
                           (format "bb %s start 2>&1" script-path))))
              (when (string-match-p "healthy" result)
                (setq hive-mcp-chroma--status 'running)
                (setq hive-mcp-chroma--fallback-active nil))))

          ;; Configure MCP if running
          (when (eq hive-mcp-chroma--status 'running)
            (hive-mcp-chroma--configure-mcp))

          ;; Inform user about status
          (cond
           ((eq hive-mcp-chroma--status 'running)
            (message "hive-mcp-chroma: Semantic search enabled (Chroma + %s)"
                     hive-mcp-chroma-embedding-provider))
           ((not docker)
            (message "hive-mcp-chroma: Docker not available, using local memory fallback")
            (message "  See: https://github.com/BuddhiLW/hive-mcp#semantic-memory-search"))
           ((not ollama)
            (message "hive-mcp-chroma: Ollama not installed, using mock embeddings")
            (message "  Install: curl -fsSL https://ollama.com/install.sh | sh"))
           (t
            (message "hive-mcp-chroma: Using local memory fallback"))))

      ;; No bb script found
      (message "hive-mcp-chroma: bb script not found, manual setup required"))))

(defun hive-mcp-chroma--addon-shutdown ()
  "Shutdown Chroma addon."
  (when hive-mcp-chroma-mode
    (hive-mcp-chroma-mode -1))
  ;; Don't stop the container on shutdown (it's persistent)
  (message "hive-mcp-chroma: shutdown"))

;;;; Addon Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'chroma
   :version "0.1.0"
   :description "Chroma vector database for semantic memory search"
   :requires '(hive-mcp-api)
   :provides '(hive-mcp-chroma-mode hive-mcp-chroma-transient)
   :init #'hive-mcp-chroma--addon-init
   :shutdown #'hive-mcp-chroma--addon-shutdown))

(provide 'hive-mcp-chroma)
;;; hive-mcp-chroma.el ends here
