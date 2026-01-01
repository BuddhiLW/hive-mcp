(ns emacs-mcp.tools
  "MCP tool definitions for Emacs interaction."
  (:require [emacs-mcp.emacsclient :as ec]
            [emacs-mcp.elisp :as el]
            [emacs-mcp.telemetry :as telemetry]
            [emacs-mcp.validation :as v]
            [emacs-mcp.org-clj.parser :as org-parser]
            [emacs-mcp.org-clj.writer :as org-writer]
            [emacs-mcp.org-clj.query :as org-query]
            [emacs-mcp.org-clj.transform :as org-transform]
            [emacs-mcp.org-clj.render :as org-render]
            [emacs-mcp.prompt-capture :as prompt-capture]
            [emacs-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; =============================================================================
;; MCP Response Format Helpers
;; =============================================================================
;; All MCP tool handlers MUST return responses in this format:
;;   Success: {:type "text" :text "result string"}
;;   Error:   {:type "text" :text "error message" :isError true}
;;
;; DO NOT use: {:content [{:type "text" :text ...}]} - this causes timeouts!

(defn mcp-success
  "Create a successful MCP response. Text can be string or will be pr-str'd."
  [text]
  {:type "text" :text (if (string? text) text (pr-str text))})

(defn mcp-error
  "Create an error MCP response."
  [message]
  {:type "text" :text message :isError true})

(defn mcp-json
  "Create a successful MCP response with JSON-encoded data."
  [data]
  {:type "text" :text (json/write-str data)})

;; Tool handlers

(defn handle-eval-elisp
  "Execute arbitrary elisp code with telemetry."
  [params]
  (try
    (v/validate-code (:code params))
    (let [{:keys [code]} params]
      (telemetry/with-eval-telemetry :elisp code nil
        (let [{:keys [success result error]} (ec/eval-elisp code)]
          (if success
            {:type "text" :text result}
            {:type "text" :text (str "Error: " error) :isError true}))))
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

(defn handle-list-buffers
  "List all open buffers."
  [_]
  (log/info "list-buffers")
  {:type "text" :text (ec/buffer-list)})

(defn handle-get-buffer-content
  "Get content of a specific buffer."
  [params]
  (try
    (v/validate-buffer-request params)
    (let [{:keys [buffer_name]} params]
      (log/info "get-buffer-content:" buffer_name)
      (try
        {:type "text" :text (ec/buffer-content buffer_name)}
        (catch Exception e
          {:type "text" :text (str "Error: " (.getMessage e)) :isError true})))
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

(defn handle-current-buffer
  "Get current buffer name and file."
  [_]
  (log/info "current-buffer")
  {:type "text"
   :text (str "Buffer: " (ec/current-buffer) "\n"
              "File: " (or (ec/current-file) "(not visiting file)"))})

(defn handle-switch-to-buffer
  "Switch to a buffer."
  [{:keys [buffer_name]}]
  (log/info "switch-to-buffer:" buffer_name)
  (ec/switch-to-buffer buffer_name)
  {:type "text" :text (str "Switched to buffer: " buffer_name)})

(defn handle-find-file
  "Open a file in Emacs."
  [{:keys [file_path]}]
  (log/info "find-file:" file_path)
  (ec/find-file file_path)
  {:type "text" :text (str "Opened file: " file_path)})

(defn handle-save-buffer
  "Save the current buffer."
  [_]
  (log/info "save-buffer")
  (ec/save-buffer)
  {:type "text" :text "Buffer saved"})

(defn handle-goto-line
  "Go to a specific line."
  [params]
  (try
    (v/validate-goto-line-request params)
    (let [{:keys [line]} params]
      (log/info "goto-line:" line)
      (ec/goto-line line)
      {:type "text" :text (str "Moved to line " line)})
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

(defn handle-insert-text
  "Insert text at cursor position."
  [{:keys [text]}]
  (log/info "insert-text:" (subs text 0 (min 50 (count text))) "...")
  (ec/insert-text text)
  {:type "text" :text "Text inserted"})

(defn handle-project-root
  "Get the current project root directory."
  [_]
  (log/info "project-root")
  {:type "text" :text (or (ec/project-root) "No project detected")})

(defn handle-recent-files
  "Get list of recently opened files."
  [_]
  (log/info "recent-files")
  {:type "text" :text (ec/recent-files)})

(defn handle-emacs-status
  "Check if Emacs is running and get basic info."
  [_]
  (log/info "emacs-status")
  (if (ec/emacs-running?)
    {:type "text"
     :text (str "Emacs is running\n"
                "Current buffer: " (ec/current-buffer) "\n"
                "Current file: " (or (ec/current-file) "none"))}
    {:type "text" :text "Emacs server is not running" :isError true}))

;; ============================================================================
;; emacs-mcp.el Integration Tools
;; These tools require emacs-mcp.el to be loaded in Emacs
;; ============================================================================

(defn emacs-mcp-el-available?
  "Check if emacs-mcp.el is loaded in Emacs."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'emacs-mcp-api)")]
    (and success (= result "t"))))

(defn handle-mcp-get-context
  "Get full context from Emacs including buffer, project, git, and memory."
  [_]
  (log/info "mcp-get-context")
  (if (emacs-mcp-el-available?)
    (let [{:keys [success result error]} (ec/eval-elisp "(json-encode (emacs-mcp-api-get-context))")]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded. Run (require 'emacs-mcp) and (emacs-mcp-mode 1) in Emacs." :isError true}))

(defn handle-mcp-memory-add
  "Add an entry to project memory.
   After successful elisp memory add, auto-indexes in Chroma if configured."
  [{:keys [type content tags duration]}]
  (log/info "mcp-memory-add:" type)
  (if (emacs-mcp-el-available?)
    (let [tags-str (if (seq tags) (str "'" (pr-str tags)) "nil")
          duration-str (if duration (pr-str duration) "nil")
          elisp (format "(json-encode (emacs-mcp-api-memory-add %s %s %s %s))"
                        (pr-str type)
                        (pr-str content)
                        tags-str
                        duration-str)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (do
          ;; Try to auto-index in Chroma if configured
          (when (chroma/embedding-configured?)
            (try
              (let [parsed-result (json/read-str result :key-fn keyword)
                    entry-id (:id parsed-result)]
                (when entry-id
                  (chroma/index-memory-entry! {:id entry-id
                                               :content content
                                               :type type
                                               :tags tags})
                  (log/info "Auto-indexed memory entry in Chroma:" entry-id)))
              (catch Exception e
                (log/warn "Failed to auto-index memory entry in Chroma:"
                          (ex-message e)))))
          {:type "text" :text result})
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-query
  "Query project memory by type."
  [{:keys [type tags limit duration]}]
  (log/info "mcp-memory-query:" type)
  (if (emacs-mcp-el-available?)
    (let [tags-str (if (seq tags) (str "'" (pr-str tags)) "nil")
          limit-val (or limit 20)
          duration-str (if duration (pr-str duration) "nil")
          elisp (format "(json-encode (emacs-mcp-api-memory-query %s %s %d %s))"
                        (pr-str type)
                        tags-str
                        limit-val
                        duration-str)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-query-metadata
  "Query project memory by type, returning only metadata (id, type, preview, tags, created).
  Use this for efficient browsing - returns ~10x fewer tokens than full query.
  Follow up with mcp_memory_get_full to fetch specific entries."
  [{:keys [type tags limit]}]
  (log/info "mcp-memory-query-metadata:" type)
  (if (emacs-mcp-el-available?)
    (let [tags-str (if (seq tags) (str "'" (pr-str tags)) "nil")
          limit-val (or limit 20)
          elisp (format "(json-encode (emacs-mcp-api-memory-query-metadata %s %s %d))"
                        (pr-str type)
                        tags-str
                        limit-val)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-get-full
  "Get full content of a memory entry by ID.
  Use after mcp_memory_query_metadata to fetch specific entries."
  [{:keys [id]}]
  (log/info "mcp-memory-get-full:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (format "(json-encode (emacs-mcp-api-memory-get-full %s))"
                        (pr-str id))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-search-semantic
  "Search project memory using semantic similarity (vector search).
   Requires Chroma to be configured with an embedding provider."
  [{:keys [query limit type]}]
  (log/info "mcp-memory-search-semantic:" query)
  (let [status (chroma/status)]
    (if-not (:configured? status)
      {:type "text"
       :text (json/write-str
              {:error "Chroma semantic search not configured"
               :message "To enable semantic search, configure Chroma with an embedding provider. See emacs-mcp.chroma namespace."
               :status status})
       :isError true}
      (try
        (let [results (chroma/search-similar query
                                             :limit (or limit 10)
                                             :type type)
              ;; Format results for user-friendly output
              formatted (mapv (fn [{:keys [id document metadata distance]}]
                                {:id id
                                 :type (get metadata :type)
                                 :tags (when-let [t (get metadata :tags)]
                                         (when (not= t "")
                                           (clojure.string/split t #",")))
                                 :distance distance
                                 :preview (when document
                                            (subs document 0 (min 200 (count document))))})
                              results)]
          {:type "text"
           :text (json/write-str {:results formatted
                                  :count (count formatted)
                                  :query query})})
        (catch Exception e
          {:type "text"
           :text (json/write-str {:error (str "Semantic search failed: " (.getMessage e))
                                  :status status})
           :isError true})))))

(defn handle-mcp-memory-set-duration
  "Set duration category for a memory entry."
  [{:keys [id duration]}]
  (log/info "mcp-memory-set-duration:" id duration)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-set-duration"
                                          id duration)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-promote
  "Promote memory entry to longer duration."
  [{:keys [id]}]
  (log/info "mcp-memory-promote:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-promote" id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-demote
  "Demote memory entry to shorter duration."
  [{:keys [id]}]
  (log/info "mcp-memory-demote:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-demote" id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-cleanup-expired
  "Remove all expired memory entries."
  [_]
  (log/info "mcp-memory-cleanup-expired")
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-cleanup-expired")
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-expiring-soon
  "List memory entries expiring within N days."
  [{:keys [days]}]
  (log/info "mcp-memory-expiring-soon:" (or days 7))
  (if (emacs-mcp-el-available?)
    (let [days-val (or days 7)
          elisp (el/require-and-call-json "emacs-mcp-api" "emacs-mcp-api-memory-expiring-soon" days-val)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-run-workflow
  "Run a user-defined workflow."
  [{:keys [name args]}]
  (log/info "mcp-run-workflow:" name)
  (if (emacs-mcp-el-available?)
    (let [elisp (if args
                  (format "(json-encode (emacs-mcp-api-run-workflow %s '%s))"
                          (pr-str name)
                          (pr-str args))
                  (format "(json-encode (emacs-mcp-api-run-workflow %s))"
                          (pr-str name)))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-notify
  "Show notification to user in Emacs."
  [{:keys [message type]}]
  (log/info "mcp-notify:" message)
  (let [type-str (or type "info")
        elisp (format "(emacs-mcp-api-notify %s %s)"
                      (pr-str message)
                      (pr-str type-str))
        {:keys [success error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text "Notification sent"}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-mcp-list-workflows
  "List available workflows."
  [_]
  (log/info "mcp-list-workflows")
  (if (emacs-mcp-el-available?)
    (let [{:keys [success result error]} (ec/eval-elisp "(json-encode (emacs-mcp-api-list-workflows))")]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-capabilities
  "Check emacs-mcp.el availability and capabilities."
  [_]
  (log/info "mcp-capabilities")
  (if (emacs-mcp-el-available?)
    (let [{:keys [success result error]} (ec/eval-elisp "(json-encode (emacs-mcp-api-capabilities))")]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text"
     :text (json/write-str {:available false
                            :message "emacs-mcp.el is not loaded. Run (require 'emacs-mcp) and (emacs-mcp-mode 1) in Emacs."})}))

(defn handle-mcp-watch-buffer
  "Get recent content from a buffer for monitoring (e.g., *Messages*)."
  [{:keys [buffer_name lines]}]
  (log/info "mcp-watch-buffer:" buffer_name)
  (let [num-lines (or lines 50)
        elisp (format "(with-current-buffer %s
                         (save-excursion
                           (goto-char (point-max))
                           (forward-line (- %d))
                           (buffer-substring-no-properties (point) (point-max))))"
                      (pr-str buffer_name)
                      num-lines)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-mcp-list-special-buffers
  "List special buffers useful for monitoring (*Messages*, *Warnings*, etc.)."
  [_]
  (log/info "mcp-list-special-buffers")
  (let [elisp "(mapcar #'buffer-name
                 (seq-filter
                   (lambda (buf)
                     (string-match-p \"^\\\\*\" (buffer-name buf)))
                   (buffer-list)))"
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-mcp-buffer-info
  "Get detailed info about a buffer including size, modified time, mode."
  [{:keys [buffer_name]}]
  (log/info "mcp-buffer-info:" buffer_name)
  (let [elisp (format "(with-current-buffer %s
                         (json-encode
                           (list :name (buffer-name)
                                 :size (buffer-size)
                                 :lines (count-lines (point-min) (point-max))
                                 :mode (symbol-name major-mode)
                                 :modified (buffer-modified-p)
                                 :file (buffer-file-name)
                                 :point (point)
                                 :point-max (point-max))))"
                      (pr-str buffer_name))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

;; ============================================================================
;; CIDER Integration Tools
;; ============================================================================

(defn handle-cider-status
  "Get CIDER connection status."
  [_]
  (log/info "cider-status")
  (let [elisp (el/require-and-call-json 'emacs-mcp-cider 'emacs-mcp-cider-status)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-cider-eval-silent
  "Evaluate Clojure code via CIDER silently with telemetry."
  [params]
  (try
    (v/validate-cider-eval-request params)
    (let [{:keys [code]} params]
      (telemetry/with-eval-telemetry :cider-silent code nil
        (let [elisp (el/require-and-call-text 'emacs-mcp-cider 'emacs-mcp-cider-eval-silent code)
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            {:type "text" :text result}
            {:type "text" :text (str "Error: " error) :isError true}))))
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

(defn handle-cider-eval-explicit
  "Evaluate Clojure code via CIDER interactively (shows in REPL) with telemetry."
  [params]
  (try
    (v/validate-cider-eval-request params)
    (let [{:keys [code]} params]
      (telemetry/with-eval-telemetry :cider-explicit code nil
        (let [elisp (el/require-and-call-text 'emacs-mcp-cider 'emacs-mcp-cider-eval-explicit code)
              {:keys [success result error]} (ec/eval-elisp elisp)]
          (if success
            {:type "text" :text result}
            {:type "text" :text (str "Error: " error) :isError true}))))
    (catch clojure.lang.ExceptionInfo e
      (if (= :validation (:type (ex-data e)))
        (v/wrap-validation-error e)
        (throw e)))))

;; =============================================================================
;; Multi-Session CIDER Tools
;; =============================================================================

(defn handle-cider-spawn-session
  "Spawn a new named CIDER session with its own nREPL server.
   Useful for parallel agent work where each agent needs isolated REPL."
  [{:keys [name project_dir agent_id]}]
  (log/info "cider-spawn-session" {:name name :agent_id agent_id})
  (let [elisp (el/require-and-call-json 'emacs-mcp-cider 'emacs-mcp-cider-spawn-session
                                        name project_dir agent_id)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-list-sessions
  "List all active CIDER sessions with their status and ports."
  [_]
  (log/info "cider-list-sessions")
  (let [elisp (el/require-and-call-json 'emacs-mcp-cider 'emacs-mcp-cider-list-sessions)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-eval-session
  "Evaluate Clojure code in a specific named CIDER session."
  [{:keys [session_name code]}]
  (log/info "cider-eval-session" {:session session_name :code-length (count code)})
  (let [elisp (el/require-and-call-text 'emacs-mcp-cider 'emacs-mcp-cider-eval-in-session
                                        session_name code)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success result)
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-kill-session
  "Kill a specific named CIDER session."
  [{:keys [session_name]}]
  (log/info "cider-kill-session" {:session session_name})
  (let [elisp (el/require-and-call 'emacs-mcp-cider 'emacs-mcp-cider-kill-session session_name)
        {:keys [success error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success (format "Session '%s' killed" session_name))
      (mcp-error (format "Error: %s" error)))))

(defn handle-cider-kill-all-sessions
  "Kill all CIDER sessions."
  [_]
  (log/info "cider-kill-all-sessions")
  (let [elisp (el/require-and-call 'emacs-mcp-cider 'emacs-mcp-cider-kill-all-sessions)
        {:keys [success error]} (ec/eval-elisp elisp)]
    (if success
      (mcp-success "All CIDER sessions killed")
      (mcp-error (format "Error: %s" error)))))

;; ============================================================
;; Magit Integration Tools (requires emacs-mcp-magit addon)
;; ============================================================

(defn magit-addon-available?
  "Check if the magit addon is loaded in Emacs."
  []
  (let [elisp "(progn
                (require 'emacs-mcp-magit nil t)
                (if (featurep 'emacs-mcp-magit) t nil))"
        {:keys [success result]} (ec/eval-elisp elisp)]
    (and success (= result "t"))))

(defn handle-magit-status
  "Get comprehensive git repository status via magit addon."
  [_]
  (log/info "magit-status")
  (let [elisp (el/require-and-call-json 'emacs-mcp-magit 'emacs-mcp-magit-api-status)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-branches
  "Get branch information including current, upstream, local and remote branches."
  [_]
  (log/info "magit-branches")
  (let [elisp (el/require-and-call-json 'emacs-mcp-magit 'emacs-mcp-magit-api-branches)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-log
  "Get recent commit log."
  [{:keys [count]}]
  (log/info "magit-log" {:count count})
  (let [n (or count 10)
        elisp (el/require-and-call-json 'emacs-mcp-magit 'emacs-mcp-magit-api-log n)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-diff
  "Get diff for staged, unstaged, or all changes."
  [{:keys [target]}]
  (log/info "magit-diff" {:target target})
  (let [target-sym (case target
                     "staged" 'staged
                     "unstaged" 'unstaged
                     "all" 'all
                     'staged)
        elisp (el/require-and-call-text 'emacs-mcp-magit 'emacs-mcp-magit-api-diff target-sym)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-stage
  "Stage files for commit. Use 'all' to stage all modified files."
  [{:keys [files]}]
  (log/info "magit-stage" {:files files})
  (let [file-arg (if (= files "all") 'all files)
        elisp (el/require-and-call 'emacs-mcp-magit 'emacs-mcp-magit-api-stage file-arg)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text (or result "Staged files")}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-commit
  "Create a commit with the given message."
  [{:keys [message all]}]
  (log/info "magit-commit" {:message-len (count message) :all all})
  (let [options (if all "'(:all t)" "nil")
        elisp (el/format-elisp
               "(progn
                  (require 'emacs-mcp-magit nil t)
                  (if (fboundp 'emacs-mcp-magit-api-commit)
                      (emacs-mcp-magit-api-commit %s %s)
                    \"emacs-mcp-magit not loaded\"))"
               (pr-str message) options)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-push
  "Push to remote. Optionally set upstream tracking."
  [{:keys [set_upstream]}]
  (log/info "magit-push" {:set_upstream set_upstream})
  (let [options (if set_upstream "'(:set-upstream t)" "nil")
        elisp (el/format-elisp
               "(progn
                  (require 'emacs-mcp-magit nil t)
                  (if (fboundp 'emacs-mcp-magit-api-push)
                      (emacs-mcp-magit-api-push %s)
                    \"emacs-mcp-magit not loaded\"))"
               options)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-pull
  "Pull from upstream."
  [_]
  (log/info "magit-pull")
  (let [elisp (el/require-and-call-text 'emacs-mcp-magit 'emacs-mcp-magit-api-pull)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-fetch
  "Fetch from remote(s)."
  [{:keys [remote]}]
  (log/info "magit-fetch" {:remote remote})
  (let [elisp (if remote
                (el/require-and-call-text 'emacs-mcp-magit 'emacs-mcp-magit-api-fetch remote)
                (el/require-and-call-text 'emacs-mcp-magit 'emacs-mcp-magit-api-fetch))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-magit-feature-branches
  "Get list of feature/fix/feat branches (for /ship and /ship-pr skills)."
  [_]
  (log/info "magit-feature-branches")
  ;; Complex elisp with client-side filtering - use format-elisp
  (let [elisp (el/format-elisp
               "(progn
                  (require 'emacs-mcp-magit nil t)
                  (if (fboundp 'emacs-mcp-magit-api-branches)
                      (let* ((branches (emacs-mcp-magit-api-branches))
                             (local (plist-get branches :local))
                             (feature-branches 
                               (seq-filter 
                                 (lambda (b) 
                                   (string-match-p \"^\\\\(feature\\\\|fix\\\\|feat\\\\)/\" b))
                                 local)))
                        (json-encode (list :current (plist-get branches :current)
                                           :feature_branches feature-branches)))
                    (json-encode (list :error \"emacs-mcp-magit not loaded\"))))")
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

;; Projectile Integration Handlers (requires emacs-mcp-projectile addon)

(defn projectile-addon-available?
  "Check if emacs-mcp-projectile addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'emacs-mcp-projectile)")]
    (and success (= result "t"))))

(defn handle-projectile-info
  "Get current project info including name, root, type, and file count."
  [_]
  (log/info "projectile-info")
  (let [elisp (el/require-and-call-json 'emacs-mcp-projectile 'emacs-mcp-projectile-api-project-info)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-files
  "List files in current project, optionally filtered by pattern."
  [{:keys [pattern]}]
  (log/info "projectile-files" {:pattern pattern})
  (let [elisp (if pattern
                (el/require-and-call-json 'emacs-mcp-projectile 'emacs-mcp-projectile-api-project-files pattern)
                (el/require-and-call-json 'emacs-mcp-projectile 'emacs-mcp-projectile-api-project-files))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-find-file
  "Find files matching a filename in current project."
  [{:keys [filename]}]
  (log/info "projectile-find-file" {:filename filename})
  (let [elisp (el/require-and-call-json 'emacs-mcp-projectile 'emacs-mcp-projectile-api-find-file filename)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-search
  "Search project for a pattern using ripgrep or grep."
  [{:keys [pattern]}]
  (log/info "projectile-search" {:pattern pattern})
  (let [elisp (el/require-and-call-json 'emacs-mcp-projectile 'emacs-mcp-projectile-api-search pattern)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-recent
  "Get recently visited files in current project."
  [_]
  (log/info "projectile-recent")
  (let [elisp (el/require-and-call-json 'emacs-mcp-projectile 'emacs-mcp-projectile-api-recent-files)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-list-projects
  "List all known projectile projects."
  [_]
  (log/info "projectile-list-projects")
  (let [elisp (el/require-and-call-json 'emacs-mcp-projectile 'emacs-mcp-projectile-api-list-projects)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

;;; ============================================================================
;;; Kanban Tools (org-kanban integration)
;;; ============================================================================

(defn kanban-addon-available?
  "Check if emacs-mcp-org-kanban addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'emacs-mcp-org-kanban)")]
    (and success (= result "t"))))

(defn handle-mcp-kanban-status
  "Get kanban status including tasks by status, progress, and backend info."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (emacs-mcp-kanban-api-status))")]
      (mcp-success (str result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded. Run (emacs-mcp-addon-load 'org-kanban)")))

(defn handle-mcp-kanban-list-tasks
  "List kanban tasks, optionally filtered by status."
  [{:keys [status]}]
  (if (kanban-addon-available?)
    (let [elisp (if status
                  (format "(json-encode (emacs-mcp-kanban-list-tasks nil \"%s\"))" status)
                  "(json-encode (emacs-mcp-kanban-list-tasks))")
          result (ec/eval-elisp elisp)]
      (mcp-success (str result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-create-task
  "Create a new kanban task."
  [{:keys [title description]}]
  (if (kanban-addon-available?)
    (let [elisp (if description
                  (format "(json-encode (emacs-mcp-kanban-create-task \"%s\" \"%s\"))"
                          (v/escape-elisp-string title)
                          (v/escape-elisp-string description))
                  (format "(json-encode (emacs-mcp-kanban-create-task \"%s\"))"
                          (v/escape-elisp-string title)))
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Created task: " result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-update-task
  "Update a kanban task's status or title."
  [{:keys [task_id status title]}]
  (if (kanban-addon-available?)
    (let [props (cond-> ""
                  status (str (format ":status \"%s\" " status))
                  title (str (format ":title \"%s\" " (v/escape-elisp-string title))))
          elisp (format "(emacs-mcp-kanban-update-task \"%s\" %s)" task_id props)
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Updated task: " result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-move-task
  "Move a task to a new status column."
  [{:keys [task_id new_status]}]
  (if (kanban-addon-available?)
    (let [elisp (format "(emacs-mcp-kanban-move-task \"%s\" \"%s\")" task_id new_status)
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Moved task to " new_status)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-roadmap
  "Get roadmap view with milestones and progress."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (emacs-mcp-kanban-api-roadmap))")]
      (mcp-success (str result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-my-tasks
  "Get tasks assigned to or modified by the current agent."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (emacs-mcp-kanban-api-my-tasks))")]
      (mcp-success (str result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-sync
  "Sync tasks between vibe-kanban and standalone backends."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(emacs-mcp-kanban-sync-all)")]
      (mcp-success (str "Sync complete: " result)))
    (mcp-error "emacs-mcp-org-kanban addon not loaded.")))

;; =============================================================================
;; Swarm Orchestration Tools (requires emacs-mcp-swarm addon)
;; =============================================================================

(defn swarm-addon-available?
  "Check if emacs-mcp-swarm addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'emacs-mcp-swarm)")]
    (and success (= result "t"))))

(defn handle-swarm-spawn
  "Spawn a new Claude slave instance."
  [{:keys [name presets cwd role terminal]}]
  (if (swarm-addon-available?)
    (let [presets-str (when (seq presets)
                        (format "'(%s)" (clojure.string/join " " (map #(format "\"%s\"" %) presets))))
          elisp (format "(json-encode (emacs-mcp-swarm-api-spawn \"%s\" %s %s %s))"
                        (v/escape-elisp-string (or name "slave"))
                        (or presets-str "nil")
                        (if cwd (format "\"%s\"" (v/escape-elisp-string cwd)) "nil")
                        (if terminal (format "\"%s\"" terminal) "nil"))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded. Run (require 'emacs-mcp-swarm)" :isError true}))

(defn handle-swarm-dispatch
  "Dispatch a prompt to a slave."
  [{:keys [slave_id prompt timeout_ms]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (emacs-mcp-swarm-api-dispatch \"%s\" \"%s\" %s))"
                        (v/escape-elisp-string slave_id)
                        (v/escape-elisp-string prompt)
                        (or timeout_ms "nil"))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-status
  "Get swarm status including all slaves and their states."
  [{:keys [slave_id]}]
  (if (swarm-addon-available?)
    (let [elisp (if slave_id
                  (format "(json-encode (emacs-mcp-swarm-status \"%s\"))" slave_id)
                  "(json-encode (emacs-mcp-swarm-api-status))")
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-collect
  "Collect response from a task."
  [{:keys [task_id timeout_ms]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (emacs-mcp-swarm-api-collect \"%s\" %s))"
                        (v/escape-elisp-string task_id)
                        (or timeout_ms "nil"))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-list-presets
  "List available swarm presets."
  [_]
  (if (swarm-addon-available?)
    (let [{:keys [success result error]} (ec/eval-elisp "(json-encode (emacs-mcp-swarm-api-list-presets))")]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-kill
  "Kill a slave or all slaves."
  [{:keys [slave_id]}]
  (if (swarm-addon-available?)
    (let [elisp (if (= slave_id "all")
                  "(json-encode (emacs-mcp-swarm-api-kill-all))"
                  (format "(json-encode (emacs-mcp-swarm-api-kill \"%s\"))"
                          (v/escape-elisp-string slave_id)))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-broadcast
  "Broadcast a prompt to all slaves."
  [{:keys [prompt]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (emacs-mcp-swarm-broadcast \"%s\"))"
                        (v/escape-elisp-string prompt))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "emacs-mcp-swarm addon not loaded." :isError true}))

;; ============================================================
;; org-clj Native Org-Mode Tools
;; ============================================================

(defn handle-org-clj-parse
  "Parse an org file and return its structure as JSON."
  [{:keys [file_path]}]
  (try
    (let [content (slurp file_path)
          doc (org-parser/parse-document content)
          json-str (json/write-str doc)]
      (mcp-success json-str))
    (catch Exception e
      (mcp-error (str "Error parsing org file: " (.getMessage e))))))

(defn handle-org-clj-write
  "Write an org document structure back to a file."
  [{:keys [file_path document]}]
  (try
    (let [doc (if (string? document)
                (json/read-str document :key-fn keyword)
                document)
          org-text (org-writer/write-document doc)]
      (spit file_path org-text)
      (mcp-success (str "Successfully wrote " (count org-text) " characters to " file_path)))
    (catch Exception e
      (mcp-error (str "Error writing org file: " (.getMessage e))))))

(defn handle-org-clj-query
  "Query headlines in a parsed org document."
  [{:keys [file_path query_type query_value]}]
  (try
    (let [content (slurp file_path)
          doc (org-parser/parse-document content)
          results (case query_type
                    "by_id" [(org-query/find-by-id doc query_value)]
                    "by_vibe_id" [(org-query/find-by-vibe-id doc query_value)]
                    "by_status" (org-query/find-by-status doc query_value)
                    "todo" (org-query/find-todo doc)
                    "done" (org-query/find-done doc)
                    "in_progress" (org-query/find-in-progress doc)
                    "stats" [(org-query/task-stats doc)]
                    (throw (ex-info (str "Unknown query type: " query_type)
                                    {:query_type query_type})))
          ;; Filter out nils
          results (filterv some? results)]
      (mcp-json results))
    (catch Exception e
      (mcp-error (str "Error querying org file: " (.getMessage e))))))

(defn handle-org-kanban-native-status
  "Get kanban status using native Clojure parser (no elisp dependency)."
  [{:keys [file_path]}]
  (try
    (let [content (slurp file_path)
          doc (org-parser/parse-document content)
          stats (org-query/task-stats doc)
          todos (org-query/find-todo doc)
          in-progress (org-query/find-in-progress doc)
          done (org-query/find-done doc)
          result {:stats stats
                  :by_status {:todo (mapv #(select-keys % [:title :properties]) todos)
                              :in_progress (mapv #(select-keys % [:title :properties]) in-progress)
                              :done (mapv #(select-keys % [:title :properties]) done)}
                  :file file_path
                  :backend "org-clj-native"}]
      {:type "text" :text (json/write-str result)})
    (catch Exception e
      {:type "text" :text (str "Error getting kanban status: " (.getMessage e)) :isError true})))

(defn handle-org-kanban-native-move
  "Move a task to a new status using native Clojure parser."
  [{:keys [file_path task_id new_status]}]
  (try
    (let [content (slurp file_path)
          doc (org-parser/parse-document content)
          ;; Check if task exists
          task (org-query/find-by-id doc task_id)]
      (if task
        (let [updated-doc (org-transform/set-status doc task_id new_status)
              org-text (org-writer/write-document updated-doc)]
          (spit file_path org-text)
          (mcp-json {:success true
                     :task_id task_id
                     :old_status (:keyword task)
                     :new_status new_status}))
        (mcp-error (json/write-str {:success false
                                    :error (str "Task not found: " task_id)}))))
    (catch Exception e
      (mcp-error (str "Error moving task: " (.getMessage e))))))

(defn handle-org-kanban-render
  "Render a visual kanban board from an org file."
  [{:keys [file_path format column_width max_cards]}]
  (try
    (let [renderer (case (or format "terminal")
                     "terminal" (org-render/terminal-renderer
                                 {:column-width (or column_width 28)
                                  :max-cards (or max_cards 10)})
                     "emacs" (org-render/emacs-renderer)
                     (throw (ex-info (str "Unknown format: " format) {:format format})))
          output (org-render/render-file renderer file_path)]
      (mcp-success output))
    (catch Exception e
      (mcp-error (str "Error rendering kanban: " (.getMessage e))))))

;;; Prompt Capture Tools

(defn handle-prompt-capture
  "Capture a well-structured prompt with analysis for RAG."
  [{:keys [prompt accomplishes well_structured improvements
           category tags quality source model context]}]
  (prompt-capture/handle-prompt-capture
   {:prompt prompt
    :accomplishes accomplishes
    :well_structured well_structured
    :improvements improvements
    :category category
    :tags tags
    :quality quality
    :source source
    :model model
    :context context}))

(defn handle-prompt-list
  "List captured prompts with optional filtering."
  [{:keys [category quality limit]}]
  (prompt-capture/handle-prompt-list
   {:category category :quality quality :limit limit}))

(defn handle-prompt-search
  "Search captured prompts by keyword."
  [{:keys [query limit]}]
  (prompt-capture/handle-prompt-search
   {:query query :limit limit}))

(defn handle-prompt-analyze
  "Analyze a prompt's structure without saving."
  [{:keys [prompt]}]
  (prompt-capture/handle-prompt-analyze {:prompt prompt}))

(defn handle-prompt-stats
  "Get statistics about captured prompts."
  [_]
  (prompt-capture/handle-prompt-stats nil))

;; Tool definitions

(def tools
  [{:name "eval_elisp"
    :description "Execute arbitrary Emacs Lisp code in the running Emacs instance. Returns the result of the evaluation."
    :inputSchema {:type "object"
                  :properties {"code" {:type "string"
                                       :description "The Emacs Lisp code to evaluate"}}
                  :required ["code"]}
    :handler handle-eval-elisp}

   {:name "emacs_status"
    :description "Check if Emacs server is running and get basic status information."
    :inputSchema {:type "object" :properties {}}
    :handler handle-emacs-status}

   {:name "list_buffers"
    :description "List all open buffers in Emacs."
    :inputSchema {:type "object" :properties {}}
    :handler handle-list-buffers}

   {:name "get_buffer_content"
    :description "Get the full content of a specific Emacs buffer."
    :inputSchema {:type "object"
                  :properties {"buffer_name" {:type "string"
                                              :description "Name of the buffer to read"}}
                  :required ["buffer_name"]}
    :handler handle-get-buffer-content}

   {:name "current_buffer"
    :description "Get the name of the current buffer and its associated file path."
    :inputSchema {:type "object" :properties {}}
    :handler handle-current-buffer}

   {:name "switch_to_buffer"
    :description "Switch to a specific buffer in Emacs."
    :inputSchema {:type "object"
                  :properties {"buffer_name" {:type "string"
                                              :description "Name of the buffer to switch to"}}
                  :required ["buffer_name"]}
    :handler handle-switch-to-buffer}

   {:name "find_file"
    :description "Open a file in Emacs. Creates a new buffer visiting that file."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the file to open"}}
                  :required ["file_path"]}
    :handler handle-find-file}

   {:name "save_buffer"
    :description "Save the current buffer to its associated file."
    :inputSchema {:type "object" :properties {}}
    :handler handle-save-buffer}

   {:name "goto_line"
    :description "Move cursor to a specific line number in the current buffer."
    :inputSchema {:type "object"
                  :properties {"line" {:type "integer"
                                       :description "Line number to go to (1-indexed)"}}
                  :required ["line"]}
    :handler handle-goto-line}

   {:name "insert_text"
    :description "Insert text at the current cursor position in Emacs."
    :inputSchema {:type "object"
                  :properties {"text" {:type "string"
                                       :description "Text to insert"}}
                  :required ["text"]}
    :handler handle-insert-text}

   {:name "project_root"
    :description "Get the root directory of the current project (detected by project.el)."
    :inputSchema {:type "object" :properties {}}
    :handler handle-project-root}

   {:name "recent_files"
    :description "Get list of recently opened files from recentf."
    :inputSchema {:type "object" :properties {}}
    :handler handle-recent-files}

   ;; emacs-mcp.el Integration Tools
   {:name "mcp_capabilities"
    :description "Check if emacs-mcp.el is loaded and get available capabilities. Use this first to verify the enhanced features are available."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-capabilities}

   {:name "mcp_get_context"
    :description "Get full context from Emacs including current buffer, region, project info, git status, and project memory. Requires emacs-mcp.el."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-get-context}

   {:name "mcp_memory_add"
    :description "Add an entry to project memory. Types: note, snippet, convention, decision. Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision"]
                                       :description "Type of memory entry"}
                               "content" {:type "string"
                                          :description "Content of the memory entry"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags for categorization"}
                               "duration" {:type "string"
                                           :enum ["session" "short-term" "long-term" "permanent"]
                                           :description "Duration category: session=current only, short-term=7 days, long-term=90 days, permanent=never expires"}}
                  :required ["type" "content"]}
    :handler handle-mcp-memory-add}

   {:name "mcp_memory_query"
    :description "Query project memory by type. Returns stored notes, snippets, conventions, or decisions. Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision" "conversation"]
                                       :description "Type of memory entries to query"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags to filter by"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results (default: 20)"}
                               "duration" {:type "string"
                                           :enum ["session" "short-term" "long-term" "permanent"]
                                           :description "Filter by duration category"}}
                  :required ["type"]}
    :handler handle-mcp-memory-query}

   {:name "mcp_memory_query_metadata"
    :description "Query project memory by type, returning only metadata (id, type, preview, tags, created). Use this for efficient browsing - returns ~10x fewer tokens than full query. Follow up with mcp_memory_get_full to fetch specific entries. Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision" "conversation"]
                                       :description "Type of memory entries to query"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Optional tags to filter by"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results (default: 20)"}}
                  :required ["type"]}
    :handler handle-mcp-memory-query-metadata}

   {:name "mcp_memory_get_full"
    :description "Get full content of a memory entry by ID. Use after mcp_memory_query_metadata to fetch specific entries when you need the full content. Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "ID of the memory entry to retrieve"}}
                  :required ["id"]}
    :handler handle-mcp-memory-get-full}

   ;; Semantic Memory Search (requires Chroma integration)
   {:name "mcp_memory_search_semantic"
    :description "Search project memory using semantic similarity (vector search). Finds conceptually related entries even without exact keyword matches. Requires Chroma to be configured with an embedding provider."
    :inputSchema {:type "object"
                  :properties {"query" {:type "string"
                                        :description "Natural language query to search for semantically similar memory entries"}
                               "limit" {:type "integer"
                                        :description "Maximum number of results to return (default: 10)"}
                               "type" {:type "string"
                                       :enum ["note" "snippet" "convention" "decision"]
                                       :description "Optional filter by memory type"}}
                  :required ["query"]}
    :handler handle-mcp-memory-search-semantic}

   ;; Memory Duration Management Tools
   {:name "mcp_memory_set_duration"
    :description "Set duration category for a memory entry. session=current only, short-term=7 days, long-term=90 days, permanent=never expires."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "Memory entry ID"}
                               "duration" {:type "string"
                                           :enum ["session" "short-term" "long-term" "permanent"]
                                           :description "Duration category to set"}}
                  :required ["id" "duration"]}
    :handler handle-mcp-memory-set-duration}

   {:name "mcp_memory_promote"
    :description "Promote memory entry to longer duration (e.g., short-term -> long-term)."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "Memory entry ID to promote"}}
                  :required ["id"]}
    :handler handle-mcp-memory-promote}

   {:name "mcp_memory_demote"
    :description "Demote memory entry to shorter duration (e.g., long-term -> short-term)."
    :inputSchema {:type "object"
                  :properties {"id" {:type "string"
                                     :description "Memory entry ID to demote"}}
                  :required ["id"]}
    :handler handle-mcp-memory-demote}

   {:name "mcp_memory_cleanup_expired"
    :description "Remove all expired memory entries. Returns deletion count."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-memory-cleanup-expired}

   {:name "mcp_memory_expiring_soon"
    :description "List entries expiring within N days."
    :inputSchema {:type "object"
                  :properties {"days" {:type "integer"
                                       :default 7
                                       :description "Number of days to look ahead (default: 7)"}}
                  :required []}
    :handler handle-mcp-memory-expiring-soon}

   {:name "mcp_list_workflows"
    :description "List available user-defined workflows. Requires emacs-mcp.el."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-list-workflows}

   {:name "mcp_run_workflow"
    :description "Run a user-defined workflow by name. Workflows can automate multi-step tasks. Requires emacs-mcp.el."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Name of the workflow to run"}
                               "args" {:type "object"
                                       :description "Optional arguments to pass to the workflow"}}
                  :required ["name"]}
    :handler handle-mcp-run-workflow}

   {:name "mcp_notify"
    :description "Show a notification message to the user in Emacs."
    :inputSchema {:type "object"
                  :properties {"message" {:type "string"
                                          :description "Message to display"}
                               "type" {:type "string"
                                       :enum ["info" "warning" "error"]
                                       :description "Type of notification (default: info)"}}
                  :required ["message"]}
    :handler handle-mcp-notify}

   ;; Buffer Monitoring Tools
   {:name "mcp_watch_buffer"
    :description "Get recent content from a buffer for monitoring. Useful for watching *Messages*, *Warnings*, *Compile-Log*, etc. Returns the last N lines."
    :inputSchema {:type "object"
                  :properties {"buffer_name" {:type "string"
                                              :description "Name of the buffer to watch (e.g., \"*Messages*\")"}
                               "lines" {:type "integer"
                                        :description "Number of lines to retrieve from end (default: 50)"}}
                  :required ["buffer_name"]}
    :handler handle-mcp-watch-buffer}

   {:name "mcp_list_special_buffers"
    :description "List all special buffers (those starting with *) useful for monitoring. Returns buffer names like *Messages*, *scratch*, *Warnings*, etc."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-list-special-buffers}

   {:name "mcp_buffer_info"
    :description "Get detailed info about a buffer including size, line count, major mode, and modification status."
    :inputSchema {:type "object"
                  :properties {"buffer_name" {:type "string"
                                              :description "Name of the buffer to inspect"}}
                  :required ["buffer_name"]}
    :handler handle-mcp-buffer-info}

   ;; CIDER Integration Tools (requires emacs-mcp-cider addon)
   {:name "cider_status"
    :description "Get CIDER connection status including connected state, REPL buffer name, current namespace, and REPL type."
    :inputSchema {:type "object" :properties {}}
    :handler handle-cider-status}

   {:name "cider_eval_silent"
    :description "Evaluate Clojure code via CIDER silently. Fast evaluation without REPL buffer output. Use for routine/automated evals."
    :inputSchema {:type "object"
                  :properties {"code" {:type "string"
                                       :description "Clojure code to evaluate"}}
                  :required ["code"]}
    :handler handle-cider-eval-silent}

   {:name "cider_eval_explicit"
    :description "Evaluate Clojure code via CIDER interactively. Shows output in REPL buffer for collaborative debugging. Use when stuck or want user to see output."
    :inputSchema {:type "object"
                  :properties {"code" {:type "string"
                                       :description "Clojure code to evaluate"}}
                  :required ["code"]}
    :handler handle-cider-eval-explicit}

   ;; Multi-Session CIDER Tools (for parallel agent work)
   {:name "cider_spawn_session"
    :description "Spawn a new named CIDER session with its own nREPL server. Useful for parallel agent work where each agent needs an isolated REPL. Sessions auto-connect when nREPL starts."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Session identifier (e.g., 'agent-1', 'task-render')"}
                               "project_dir" {:type "string"
                                              :description "Directory to start nREPL in (optional, defaults to current project)"}
                               "agent_id" {:type "string"
                                           :description "Optional swarm agent ID to link this session to"}}
                  :required ["name"]}
    :handler handle-cider-spawn-session}

   {:name "cider_list_sessions"
    :description "List all active CIDER sessions with their status, ports, and linked agents."
    :inputSchema {:type "object" :properties {}}
    :handler handle-cider-list-sessions}

   {:name "cider_eval_session"
    :description "Evaluate Clojure code in a specific named CIDER session. Use for isolated evaluation in multi-agent scenarios."
    :inputSchema {:type "object"
                  :properties {"session_name" {:type "string"
                                               :description "Name of the session to evaluate in"}
                               "code" {:type "string"
                                       :description "Clojure code to evaluate"}}
                  :required ["session_name" "code"]}
    :handler handle-cider-eval-session}

   {:name "cider_kill_session"
    :description "Kill a specific named CIDER session and its nREPL server."
    :inputSchema {:type "object"
                  :properties {"session_name" {:type "string"
                                               :description "Name of the session to kill"}}
                  :required ["session_name"]}
    :handler handle-cider-kill-session}

   {:name "cider_kill_all_sessions"
    :description "Kill all CIDER sessions. Useful for cleanup after parallel agent work."
    :inputSchema {:type "object" :properties {}}
    :handler handle-cider-kill-all-sessions}

   ;; Magit Integration Tools (requires emacs-mcp-magit addon)
   {:name "magit_status"
    :description "Get comprehensive git repository status including branch, staged/unstaged/untracked files, ahead/behind counts, stashes, and recent commits. Requires emacs-mcp-magit addon."
    :inputSchema {:type "object" :properties {}}
    :handler handle-magit-status}

   {:name "magit_branches"
    :description "Get branch information including current branch, upstream, all local branches, and remote branches."
    :inputSchema {:type "object" :properties {}}
    :handler handle-magit-branches}

   {:name "magit_log"
    :description "Get recent commit log with hash, author, date, and subject."
    :inputSchema {:type "object"
                  :properties {"count" {:type "integer"
                                        :description "Number of commits to return (default: 10)"}}
                  :required []}
    :handler handle-magit-log}

   {:name "magit_diff"
    :description "Get diff for staged, unstaged, or all changes."
    :inputSchema {:type "object"
                  :properties {"target" {:type "string"
                                         :enum ["staged" "unstaged" "all"]
                                         :description "What to diff (default: staged)"}}
                  :required []}
    :handler handle-magit-diff}

   {:name "magit_stage"
    :description "Stage files for commit. Use 'all' to stage all modified files, or provide a file path."
    :inputSchema {:type "object"
                  :properties {"files" {:type "string"
                                        :description "File path to stage, or 'all' for all modified files"}}
                  :required ["files"]}
    :handler handle-magit-stage}

   {:name "magit_commit"
    :description "Create a git commit with the given message."
    :inputSchema {:type "object"
                  :properties {"message" {:type "string"
                                          :description "Commit message"}
                               "all" {:type "boolean"
                                      :description "If true, stage all changes before committing"}}
                  :required ["message"]}
    :handler handle-magit-commit}

   {:name "magit_push"
    :description "Push to remote. Optionally set upstream tracking for new branches."
    :inputSchema {:type "object"
                  :properties {"set_upstream" {:type "boolean"
                                               :description "Set upstream tracking if not already set"}}
                  :required []}
    :handler handle-magit-push}

   {:name "magit_pull"
    :description "Pull from upstream remote."
    :inputSchema {:type "object" :properties {}}
    :handler handle-magit-pull}

   {:name "magit_fetch"
    :description "Fetch from remote(s). Fetches all remotes if no specific remote is provided."
    :inputSchema {:type "object"
                  :properties {"remote" {:type "string"
                                         :description "Specific remote to fetch from (optional)"}}
                  :required []}
    :handler handle-magit-fetch}

   {:name "magit_feature_branches"
    :description "Get list of feature/fix/feat branches for shipping. Used by /ship and /ship-pr skills."
    :inputSchema {:type "object" :properties {}}
    :handler handle-magit-feature-branches}

   ;; Projectile Integration Tools (requires emacs-mcp-projectile addon)
   {:name "projectile_info"
    :description "Get current project info including name, root, type (with extended detection for npm, cargo, go-mod, etc.), and file count. Requires emacs-mcp-projectile addon."
    :inputSchema {:type "object" :properties {}}
    :handler handle-projectile-info}

   {:name "projectile_files"
    :description "List files in current project, optionally filtered by glob pattern."
    :inputSchema {:type "object"
                  :properties {"pattern" {:type "string"
                                          :description "Optional glob pattern to filter files (e.g., '*.clj', 'src/*.ts')"}}
                  :required []}
    :handler handle-projectile-files}

   {:name "projectile_find_file"
    :description "Find files matching a filename in current project. Returns relative and absolute paths."
    :inputSchema {:type "object"
                  :properties {"filename" {:type "string"
                                           :description "Filename or partial filename to search for"}}
                  :required ["filename"]}
    :handler handle-projectile-find-file}

   {:name "projectile_search"
    :description "Search current project for a pattern using ripgrep (preferred) or grep. Returns file, line number, and matching content."
    :inputSchema {:type "object"
                  :properties {"pattern" {:type "string"
                                          :description "Search pattern (regex supported)"}}
                  :required ["pattern"]}
    :handler handle-projectile-search}

   {:name "projectile_recent"
    :description "Get recently visited files in current project."
    :inputSchema {:type "object" :properties {}}
    :handler handle-projectile-recent}

   {:name "projectile_list_projects"
    :description "List all known projectile projects with their roots, names, existence status, and detected types."
    :inputSchema {:type "object" :properties {}}
    :handler handle-projectile-list-projects}

   ;; Kanban Integration Tools (requires emacs-mcp-org-kanban addon)
   {:name "mcp_kanban_status"
    :description "Get kanban status including tasks by status, progress percentage, backend info, and roadmap. Use at session start."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-kanban-status}

   {:name "mcp_kanban_list_tasks"
    :description "List kanban tasks, optionally filtered by status (todo, inprogress, inreview, done)."
    :inputSchema {:type "object"
                  :properties {"status" {:type "string"
                                         :enum ["todo" "inprogress" "inreview" "done"]
                                         :description "Filter by status (optional)"}}
                  :required []}
    :handler handle-mcp-kanban-list-tasks}

   {:name "mcp_kanban_create_task"
    :description "Create a new kanban task with title and optional description."
    :inputSchema {:type "object"
                  :properties {"title" {:type "string"
                                        :description "Task title"}
                               "description" {:type "string"
                                              :description "Task description (optional)"}}
                  :required ["title"]}
    :handler handle-mcp-kanban-create-task}

   {:name "mcp_kanban_update_task"
    :description "Update a kanban task's status or title."
    :inputSchema {:type "object"
                  :properties {"task_id" {:type "string"
                                          :description "Task ID to update"}
                               "status" {:type "string"
                                         :enum ["todo" "inprogress" "inreview" "done"]
                                         :description "New status (optional)"}
                               "title" {:type "string"
                                        :description "New title (optional)"}}
                  :required ["task_id"]}
    :handler handle-mcp-kanban-update-task}

   {:name "mcp_kanban_move_task"
    :description "Move a task to a new status column. Shorthand for update with status change."
    :inputSchema {:type "object"
                  :properties {"task_id" {:type "string"
                                          :description "Task ID to move"}
                               "new_status" {:type "string"
                                             :enum ["todo" "inprogress" "inreview" "done"]
                                             :description "Target status column"}}
                  :required ["task_id" "new_status"]}
    :handler handle-mcp-kanban-move-task}

   {:name "mcp_kanban_roadmap"
    :description "Get roadmap view with milestones, phases, and progress indicators."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-kanban-roadmap}

   {:name "mcp_kanban_my_tasks"
    :description "Get tasks assigned to or modified by the current agent session."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-kanban-my-tasks}

   {:name "mcp_kanban_sync"
    :description "Sync tasks between vibe-kanban cloud and standalone org-file backends."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-kanban-sync}

   ;; Swarm Orchestration Tools (requires emacs-mcp-swarm addon)
   {:name "swarm_spawn"
    :description "Spawn a new Claude slave instance for parallel task execution. Slaves run in terminal buffers (vterm or eat) with optional presets (system prompts)."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Name for the slave (used in buffer name)"}
                               "presets" {:type "array"
                                          :items {:type "string"}
                                          :description "List of preset names to apply (e.g., [\"tdd\", \"clarity\"])"}
                               "cwd" {:type "string"
                                      :description "Working directory for the slave (optional)"}
                               "role" {:type "string"
                                       :description "Predefined role (tester, reviewer, documenter, coordinator, ling, etc.)"}
                               "terminal" {:type "string"
                                           :enum ["vterm" "eat"]
                                           :description "Terminal backend to use (default: vterm). Use 'eat' for pure Emacs Lisp terminal."}}
                  :required ["name"]}
    :handler handle-swarm-spawn}

   {:name "swarm_dispatch"
    :description "Send a prompt to a slave Claude instance. Returns a task_id for tracking."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "ID of the slave to send prompt to"}
                               "prompt" {:type "string"
                                         :description "The prompt/task to send to the slave"}
                               "timeout_ms" {:type "integer"
                                             :description "Optional timeout in milliseconds"}}
                  :required ["slave_id" "prompt"]}
    :handler handle-swarm-dispatch}

   {:name "swarm_status"
    :description "Get swarm status including all active slaves, their states, and task counts."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "Optional: get status of specific slave only"}}
                  :required []}
    :handler handle-swarm-status}

   {:name "swarm_collect"
    :description "Collect the response from a dispatched task. Waits for completion up to timeout."
    :inputSchema {:type "object"
                  :properties {"task_id" {:type "string"
                                          :description "ID of the task to collect results from"}
                               "timeout_ms" {:type "integer"
                                             :description "How long to wait for completion (default: 5000)"}}
                  :required ["task_id"]}
    :handler handle-swarm-collect}

   {:name "swarm_list_presets"
    :description "List all available swarm presets (system prompts for slave specialization)."
    :inputSchema {:type "object" :properties {}}
    :handler handle-swarm-list-presets}

   {:name "swarm_kill"
    :description "Kill a slave instance or all slaves."
    :inputSchema {:type "object"
                  :properties {"slave_id" {:type "string"
                                           :description "ID of slave to kill, or \"all\" to kill all slaves"}}
                  :required ["slave_id"]}
    :handler handle-swarm-kill}

   {:name "swarm_broadcast"
    :description "Send the same prompt to all active slaves simultaneously."
    :inputSchema {:type "object"
                  :properties {"prompt" {:type "string"
                                         :description "The prompt to broadcast to all slaves"}}
                  :required ["prompt"]}
    :handler handle-swarm-broadcast}

   ;; org-clj Native Org-Mode Tools (no elisp dependency)
   {:name "org_clj_parse"
    :description "Parse an org-mode file and return its structure as JSON. Uses native Clojure parser without elisp dependency."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the org file to parse"}}
                  :required ["file_path"]}
    :handler handle-org-clj-parse}

   {:name "org_clj_write"
    :description "Write an org document structure back to a file. Takes a JSON document and writes org-mode formatted text."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to write the org file"}
                               "document" {:type "string"
                                           :description "JSON string representing the org document structure"}}
                  :required ["file_path" "document"]}
    :handler handle-org-clj-write}

   {:name "org_clj_query"
    :description "Query headlines in an org file. Supports queries by ID, status, or get statistics."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the org file"}
                               "query_type" {:type "string"
                                             :enum ["by_id" "by_vibe_id" "by_status" "todo" "done" "in_progress" "stats"]
                                             :description "Type of query to perform"}
                               "query_value" {:type "string"
                                              :description "Value for the query (ID for by_id, status string for by_status)"}}
                  :required ["file_path" "query_type"]}
    :handler handle-org-clj-query}

   {:name "org_kanban_native_status"
    :description "Get kanban status from an org file using native Clojure parser. Returns task counts and lists by status."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the kanban org file"}}
                  :required ["file_path"]}
    :handler handle-org-kanban-native-status}

   {:name "org_kanban_native_move"
    :description "Move a task to a new status in an org file using native Clojure parser. Updates the file directly."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the kanban org file"}
                               "task_id" {:type "string"
                                          :description "ID of the task to move (from :ID property)"}
                               "new_status" {:type "string"
                                             :description "New TODO status (e.g., TODO, IN-PROGRESS, DONE)"}}
                  :required ["file_path" "task_id" "new_status"]}
    :handler handle-org-kanban-native-move}

   {:name "org_kanban_render"
    :description "Render a visual kanban board from an org file. Supports terminal ASCII art or Emacs org-mode format."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the kanban org file"}
                               "format" {:type "string"
                                         :enum ["terminal" "emacs"]
                                         :description "Output format: 'terminal' for ASCII art, 'emacs' for org-mode (default: terminal)"}
                               "column_width" {:type "integer"
                                               :description "Width of each column in terminal mode (default: 28)"}
                               "max_cards" {:type "integer"
                                            :description "Maximum cards per column in terminal mode (default: 10)"}}
                  :required ["file_path"]}
    :handler handle-org-kanban-render}

   ;; Prompt Capture Tools (RAG Knowledge Base)
   {:name "prompt_capture"
    :description "Capture a well-structured LLM prompt with analysis for RAG knowledge base. Stores prompt text, what it accomplishes, why it's well-structured, and improvement suggestions. Auto-categorizes and validates quality."
    :inputSchema {:type "object"
                  :properties {"prompt" {:type "string"
                                         :description "The prompt text to capture (verbatim)"}
                               "accomplishes" {:type "string"
                                               :description "What this prompt accomplishes"}
                               "well_structured" {:type "string"
                                                  :description "Why this prompt is well-structured"}
                               "improvements" {:type "string"
                                               :description "How the prompt could be improved (optional, auto-generated if not provided)"}
                               "category" {:type "string"
                                           :enum ["coding" "debug" "planning" "meta" "research" "config" "workflow" "architecture"]
                                           :description "Category (optional, auto-inferred from content)"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Additional tags for categorization"}
                               "quality" {:type "string"
                                          :enum ["success" "partial" "failure" "untested"]
                                          :description "Quality/outcome rating (default: untested)"}
                               "source" {:type "string"
                                         :description "Source: 'user', 'observed', 'generated' (default: user)"}
                               "model" {:type "string"
                                        :description "Model used (e.g., claude-opus-4-5)"}
                               "context" {:type "string"
                                          :description "Additional context about the prompt"}}
                  :required ["prompt" "accomplishes" "well_structured"]}
    :handler handle-prompt-capture}

   {:name "prompt_list"
    :description "List captured prompts with optional filtering by category, quality, or tags."
    :inputSchema {:type "object"
                  :properties {"category" {:type "string"
                                           :enum ["coding" "debug" "planning" "meta" "research" "config" "workflow" "architecture"]
                                           :description "Filter by category"}
                               "quality" {:type "string"
                                          :enum ["success" "partial" "failure" "untested"]
                                          :description "Filter by quality rating"}
                               "limit" {:type "integer"
                                        :description "Maximum results to return (default: 20)"}}
                  :required []}
    :handler handle-prompt-list}

   {:name "prompt_search"
    :description "Search captured prompts by keyword in prompt text or accomplishes field."
    :inputSchema {:type "object"
                  :properties {"query" {:type "string"
                                        :description "Search keyword"}
                               "limit" {:type "integer"
                                        :description "Maximum results (default: 20)"}}
                  :required ["query"]}
    :handler handle-prompt-search}

   {:name "prompt_analyze"
    :description "Analyze a prompt's structure without saving. Returns quality score, category inference, and improvement suggestions."
    :inputSchema {:type "object"
                  :properties {"prompt" {:type "string"
                                         :description "The prompt text to analyze"}}
                  :required ["prompt"]}
    :handler handle-prompt-analyze}

   {:name "prompt_stats"
    :description "Get statistics about captured prompts including counts by category, quality, and recent entries."
    :inputSchema {:type "object" :properties {}}
    :handler handle-prompt-stats}])

(defn get-tool-by-name
  "Find a tool definition by name."
  [name]
  (first (filter #(= (:name %) name) tools)))
