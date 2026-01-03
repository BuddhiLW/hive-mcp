(ns emacs-mcp.tools
  "MCP tool definitions for Emacs interaction.
   
   This namespace aggregates tool definitions and their handlers from
   domain-specific modules under emacs-mcp.tools.*"
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
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            ;; Domain-specific tool modules (SOLID refactoring)
            [emacs-mcp.tools.core :as core]
            [emacs-mcp.tools.buffer :as buffer]
            [emacs-mcp.tools.memory :as memory]
            [emacs-mcp.tools.memory-kanban :as mem-kanban]
            [emacs-mcp.tools.cider :as cider]
            [emacs-mcp.tools.magit :as magit]
            [emacs-mcp.tools.projectile :as projectile]
            [emacs-mcp.tools.kanban :as kanban]
            [emacs-mcp.tools.swarm :as swarm]
            [emacs-mcp.tools.org :as org]
            [emacs-mcp.tools.prompt :as prompt]))

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
  "Query project memory by type with scope filtering.
  SCOPE controls which memories are returned:
    - nil/omitted: auto-filter by current project + global
    - \"all\": return all entries regardless of scope  
    - \"global\": return only scope:global entries
    - specific scope tag: filter by that scope"
  [{:keys [type tags limit duration scope]}]
  (log/info "mcp-memory-query:" type "scope:" scope)
  (if (emacs-mcp-el-available?)
    (let [tags-str (if (seq tags) (str "'" (pr-str tags)) "nil")
          limit-val (or limit 20)
          duration-str (if duration (pr-str duration) "nil")
          scope-str (if scope (pr-str scope) "nil")
          elisp (format "(json-encode (emacs-mcp-api-memory-query %s %s %d %s))"
                        (pr-str type)
                        tags-str
                        limit-val
                        scope-str)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-query-metadata
  "Query project memory by type, returning only metadata (id, type, preview, tags, created).
  Use this for efficient browsing - returns ~10x fewer tokens than full query.
  Follow up with mcp_memory_get_full to fetch specific entries.
  SCOPE controls filtering (see handle-mcp-memory-query for options)."
  [{:keys [type tags limit scope]}]
  (log/info "mcp-memory-query-metadata:" type "scope:" scope)
  (if (emacs-mcp-el-available?)
    (let [tags-str (if (seq tags) (str "'" (pr-str tags)) "nil")
          limit-val (or limit 20)
          scope-str (if scope (pr-str scope) "nil")
          elisp (format "(json-encode (emacs-mcp-api-memory-query-metadata %s %s %d %s))"
                        (pr-str type)
                        tags-str
                        limit-val
                        scope-str)
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

(defn handle-mcp-memory-log-access
  "Log access to a memory entry.
   Increments access-count and updates last-accessed timestamp."
  [{:keys [id]}]
  (log/info "mcp-memory-log-access:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-memory"
                                          "emacs-mcp-memory-log-access"
                                          id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-feedback
  "Submit helpfulness feedback for a memory entry.
   feedback should be 'helpful' or 'unhelpful'."
  [{:keys [id feedback]}]
  (log/info "mcp-memory-feedback:" id feedback)
  (if (emacs-mcp-el-available?)
    (let [fn-name (case feedback
                    "helpful" "emacs-mcp-memory-mark-helpful"
                    "unhelpful" "emacs-mcp-memory-mark-unhelpful"
                    (throw (ex-info "Invalid feedback type" {:feedback feedback})))
          elisp (el/require-and-call-json "emacs-mcp-memory" fn-name id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-helpfulness-ratio
  "Get helpfulness ratio for a memory entry.
   Returns helpful/(helpful+unhelpful) or null if no feedback."
  [{:keys [id]}]
  (log/info "mcp-memory-helpfulness-ratio:" id)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-memory"
                                          "emacs-mcp-memory-helpfulness-ratio"
                                          id)
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: emacs-mcp.el is not loaded." :isError true}))

(defn handle-mcp-memory-check-duplicate
  "Check if content already exists in memory (duplicate detection)."
  [{:keys [type content]}]
  (log/info "mcp-memory-check-duplicate:" type)
  (if (emacs-mcp-el-available?)
    (let [elisp (el/require-and-call-json "emacs-mcp-api"
                                          "emacs-mcp-api-memory-check-duplicate"
                                          type content)
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
;; JVM Process Cleanup (for swarm garbage collection)
;; ============================================================

(defn parse-jvm-process-line
  "Parse a ps output line into a process map."
  [line]
  (let [parts (str/split (str/trim line) #"\s+" 5)]
    (when (>= (count parts) 5)
      {:pid (first parts)
       :cpu (second parts)
       :mem (nth parts 2)
       :etime (nth parts 3)
       :cmd (nth parts 4)})))

(defn parse-etime-to-minutes
  "Parse elapsed time (format: [[DD-]HH:]MM:SS) to minutes."
  [etime]
  (try
    (let [parts (str/split etime #"[-:]")
          nums (map #(Integer/parseInt %) parts)]
      (case (count nums)
        2 (first nums) ; MM:SS -> minutes
        3 (+ (* 60 (first nums)) (second nums)) ; HH:MM:SS -> minutes
        4 (+ (* 24 60 (first nums)) (* 60 (second nums)) (nth nums 2)) ; DD-HH:MM:SS
        0))
    (catch Exception _ 0)))

(defn find-jvm-processes
  "Find all JVM processes with their details including parent info."
  []
  (try
    (let [result (shell/sh "ps" "-eo" "pid,ppid,pcpu,pmem,etime,args" "--no-headers")
          lines (str/split-lines (:out result))
          jvm-lines (filter #(re-find #"java" %) lines)]
      (keep (fn [line]
              (let [parts (str/split (str/trim line) #"\s+" 6)]
                (when (>= (count parts) 6)
                  {:pid (nth parts 0)
                   :ppid (nth parts 1)
                   :cpu (nth parts 2)
                   :mem (nth parts 3)
                   :etime (nth parts 4)
                   :cmd (nth parts 5)})))
            jvm-lines))
    (catch Exception e
      (log/error "Error finding JVM processes:" (.getMessage e))
      [])))

(defn get-all-process-parents
  "Get pid->{:ppid :comm} map for all processes in ONE ps call. Efficient!"
  []
  (try
    (let [result (shell/sh "ps" "-eo" "pid,ppid,comm" "--no-headers")
          lines (str/split-lines (:out result))]
      (into {}
            (keep (fn [line]
                    (let [parts (str/split (str/trim line) #"\s+" 3)]
                      (when (= 3 (count parts))
                        [(first parts) {:ppid (second parts) :comm (nth parts 2)}])))
                  lines)))
    (catch Exception _ {})))

(defn enrich-with-parent-info
  "Enrich process with parent info from pre-fetched map."
  [proc all-parents]
  (let [ppid (:ppid proc)
        parent (get all-parents ppid)
        parent-alive (boolean parent)
        parent-comm (:comm parent)
        is-init (= "1" ppid)
        is-claude (= "claude" parent-comm)
        truly-orphaned (or (not parent-alive) is-init)]
    (assoc proc
           :parent-alive parent-alive
           :parent-comm parent-comm
           :parent-is-claude is-claude
           :truly-orphaned truly-orphaned)))

(defn get-process-swarm-info
  "Get swarm environment variables from /proc/<pid>/environ.
   Returns nil if not a swarm-spawned process, or a map with swarm info."
  [pid]
  (try
    (let [environ-file (str "/proc/" pid "/environ")
          content (slurp environ-file)
          ;; environ file has null-separated entries
          entries (str/split content #"\x00")
          env-map (into {} (keep #(let [parts (str/split % #"=" 2)]
                                    (when (= 2 (count parts))
                                      [(first parts) (second parts)]))
                                 entries))
          slave-id (get env-map "CLAUDE_SWARM_SLAVE_ID")
          master-id (get env-map "CLAUDE_SWARM_MASTER")
          depth (get env-map "CLAUDE_SWARM_DEPTH")]
      (when (or slave-id master-id depth)
        {:swarm-slave-id slave-id
         :swarm-master-id master-id
         :swarm-depth (when depth (try (Integer/parseInt depth) (catch Exception _ nil)))}))
    (catch Exception _
      ;; Can't read environ (permission denied or process gone)
      nil)))

(defn classify-jvm-process
  "Classify a JVM process by type and swarm status (parent info added separately)."
  [{:keys [cmd pid] :as proc}]
  (let [swarm-info (get-process-swarm-info pid)
        proc-type (cond
                    (re-find #"shadow-cljs|shadow\.cljs" cmd) :shadow-cljs
                    (re-find #"emacs-mcp|emacs_mcp" cmd) :emacs-mcp
                    (re-find #"clojure-mcp|clj-mcp" cmd) :clojure-mcp
                    (re-find #"nrepl" cmd) :nrepl
                    (re-find #"leiningen" cmd) :leiningen
                    :else :other)]
    (-> proc
        (assoc :type proc-type)
        (assoc :swarm-spawned (boolean swarm-info))
        (merge swarm-info))))

(defn handle-jvm-cleanup
  "Find and optionally kill orphaned JVM processes.

   TRUE ORPHAN DETECTION:
   - Parent process is dead (not running)
   - Parent is PID 1 (reparented to init/systemd)
   
   EFFICIENT: Uses only 2 ps calls total (not O(n) per process).
   
   Keeps processes whose parent is a living Claude session."
  [{:keys [min_age_minutes dry_run keep_types swarm_only true_orphans_only]}]
  (try
    (let [min-age (or min_age_minutes 30)
          dry-run (if (nil? dry_run) true dry_run)
          keep-types-set (set (or keep_types ["shadow-cljs" "leiningen"]))
          swarm-only (boolean swarm_only)
          true-orphans-only (if (nil? true_orphans_only) true true_orphans_only)

          ;; EFFICIENT: Get all parent info in ONE call
          all-parents (get-all-process-parents)

          ;; Get JVM processes and classify
          all-procs (find-jvm-processes)
          classified (->> all-procs
                          (map classify-jvm-process)
                          (map #(enrich-with-parent-info % all-parents)))

          ;; Filter to swarm-only if requested
          working-procs (if swarm-only
                          (filter :swarm-spawned classified)
                          classified)

          ;; Swarm statistics
          swarm-procs (filter :swarm-spawned classified)
          by-slave (group-by :swarm-slave-id swarm-procs)

          ;; Group by type
          by-type (group-by :type working-procs)

          ;; Identify orphans based on detection mode
          identify-orphans (fn [procs]
                             (map (fn [p]
                                    (let [age (parse-etime-to-minutes (:etime p))
                                          protected-type (contains? keep-types-set (name (:type p)))
                                          truly-orphaned (:truly-orphaned p)
                                          age-orphaned (>= age min-age)
                                          is-orphan (cond
                                                      protected-type false
                                                      true-orphans-only truly-orphaned
                                                      :else (and truly-orphaned age-orphaned))
                                          reason (cond
                                                   protected-type "protected-type"
                                                   truly-orphaned (str "truly-orphaned (parent: "
                                                                       (or (:parent-comm p) "dead") ")")
                                                   (:parent-is-claude p) (str "managed-by-claude (ppid: "
                                                                              (:ppid p) ")")
                                                   :else (str "has-parent: " (:parent-comm p)))]
                                      (assoc p
                                             :orphan is-orphan
                                             :age-minutes age
                                             :reason reason)))
                                  procs))

          all-classified (identify-orphans working-procs)
          orphans (filter :orphan all-classified)
          managed (filter :parent-is-claude all-classified)

          ;; Kill orphans if not dry run
          killed-pids (when (and (not dry-run) (seq orphans))
                        (doseq [{:keys [pid]} orphans]
                          (try
                            (shell/sh "kill" pid)
                            (catch Exception e
                              (log/warn "Failed to kill PID" pid ":" (.getMessage e)))))
                        (map :pid orphans))

          summary {:total-jvm-processes (count all-procs)
                   :by-type (into {} (map (fn [[k v]] [(name k) (count v)]) by-type))
                   :swarm {:total-swarm-spawned (count swarm-procs)
                           :by-slave (into {} (map (fn [[k v]]
                                                     [(or k "unknown")
                                                      {:count (count v)
                                                       :pids (map :pid v)}])
                                                   by-slave))}
                   :orphan-detection {:mode (if true-orphans-only "true-orphans" "age-based")
                                      :truly-orphaned-count (count (filter :truly-orphaned all-classified))
                                      :managed-by-claude (count managed)}
                   :orphans-found (count orphans)
                   :orphan-pids (map :pid orphans)
                   :dry-run dry-run
                   :swarm-only-mode swarm-only
                   :killed (if dry-run [] (or killed-pids []))
                   :min-age-threshold min-age
                   :details (map #(select-keys % [:pid :ppid :type :etime :orphan :reason :age-minutes
                                                  :truly-orphaned :parent-alive :parent-comm :parent-is-claude
                                                  :swarm-spawned :swarm-slave-id :swarm-master-id :swarm-depth])
                                 all-classified)}]

      (mcp-json summary))
    (catch Exception e
      (mcp-error (str "Error during JVM cleanup: " (.getMessage e))))))

(defn get-memory-usage
  "Get current RAM usage from /proc/meminfo. Returns {:total :used :available :percent-used}."
  []
  (try
    (let [meminfo (slurp "/proc/meminfo")
          parse-kb (fn [pattern]
                     (when-let [m (re-find (re-pattern (str pattern ":\\s+(\\d+)")) meminfo)]
                       (Long/parseLong (second m))))
          total-kb (parse-kb "MemTotal")
          available-kb (parse-kb "MemAvailable")
          used-kb (- total-kb available-kb)
          percent-used (double (* 100 (/ used-kb total-kb)))]
      {:total-mb (quot total-kb 1024)
       :used-mb (quot used-kb 1024)
       :available-mb (quot available-kb 1024)
       :percent-used (Math/round percent-used)})
    (catch Exception e
      {:error (.getMessage e)})))

(defn handle-resource-guard
  "Check system resources and automatically clean up orphaned JVMs if memory is high.
   
   WORKFLOW:
   1. Check current RAM usage
   2. If above threshold (default 80%), run jvm_cleanup automatically
   3. Re-check memory after cleanup
   4. Return spawn permission based on final memory state
   
   Use this BEFORE spawning new Claude swarm slaves to prevent OOM.
   
   Parameters:
   - ram_threshold: Percentage threshold (default 80)
   - min_available_mb: Minimum available RAM in MB (default 2048)
   - auto_cleanup: Whether to auto-run jvm_cleanup when high (default true)
   - cleanup_dry_run: If auto_cleanup, whether to actually kill (default false)"
  [{:keys [ram_threshold min_available_mb auto_cleanup cleanup_dry_run]}]
  (try
    (let [threshold (or ram_threshold 80)
          min-available (or min_available_mb 2048)
          auto-clean (if (nil? auto_cleanup) true auto_cleanup)
          cleanup-dry (if (nil? cleanup_dry_run) false cleanup_dry_run)

          ;; Initial memory check
          initial-mem (get-memory-usage)

          _ (when (:error initial-mem)
              (throw (Exception. (str "Cannot read memory: " (:error initial-mem)))))

          initial-high? (or (>= (:percent-used initial-mem) threshold)
                            (< (:available-mb initial-mem) min-available))

          ;; Auto cleanup if needed
          cleanup-result (when (and initial-high? auto-clean)
                           (log/info "Memory high (" (:percent-used initial-mem) "%), running jvm_cleanup...")
                           (handle-jvm-cleanup {:dry_run cleanup-dry
                                                :true_orphans_only true}))

          ;; Parse cleanup result
          cleanup-data (when cleanup-result
                         (try
                           (json/read-str (:text cleanup-result) :key-fn keyword)
                           (catch Exception _ nil)))

          orphans-killed (when cleanup-data
                           (count (:killed cleanup-data)))

          ;; Re-check memory after cleanup
          final-mem (if (and cleanup-data (pos? (or orphans-killed 0)))
                      (do
                        (Thread/sleep 500) ;; Wait for processes to fully exit
                        (get-memory-usage))
                      initial-mem)

          final-high? (or (>= (:percent-used final-mem) threshold)
                          (< (:available-mb final-mem) min-available))

          ;; Determine spawn permission
          can-spawn (not final-high?)

          summary {:can-spawn can-spawn
                   :memory {:initial initial-mem
                            :final final-mem
                            :threshold-percent threshold
                            :min-available-mb min-available}
                   :status (cond
                             (not initial-high?) :healthy
                             (and initial-high? (not final-high?)) :recovered-after-cleanup
                             :else :capacity-reached)
                   :cleanup (when cleanup-data
                              {:ran true
                               :dry-run cleanup-dry
                               :orphans-found (:orphans-found cleanup-data)
                               :killed (count (:killed cleanup-data))})
                   :recommendation (cond
                                     can-spawn "Safe to spawn new processes"
                                     (not auto-clean) "Memory high - consider enabling auto_cleanup"
                                     cleanup-dry "Memory high - set cleanup_dry_run=false to actually kill orphans"
                                     :else "Capacity reached - wait for running tasks to complete")}]

      (mcp-json summary))
    (catch Exception e
      (mcp-error (str "Resource guard error: " (.getMessage e))))))

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
  "Delegate to org namespace for native kanban status."
  [params]
  (org/handle-org-kanban-native-status params))

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
  "Aggregated tool definitions from domain-specific modules.
   
   Each module exports its own tools vector following the tool registry pattern.
   This enables Open/Closed Principle - new tools are added to their respective
   modules without modifying this aggregation."
  (vec (concat buffer/tools
               memory/tools
               mem-kanban/tools
               cider/tools
               magit/tools
               projectile/tools
               kanban/tools
               swarm/tools
               org/tools
               prompt/tools)))

(defn get-tool-by-name
  "Find a tool definition by name."
  [name]
  (first (filter #(= (:name %) name) tools)))
