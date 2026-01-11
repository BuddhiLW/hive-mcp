(ns hive-mcp.tools
  "MCP tool definitions for Emacs interaction.
   
   This namespace aggregates tool definitions and their handlers from
   domain-specific modules under hive-mcp.tools.*"
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.elisp :as el]
            [hive-mcp.telemetry :as telemetry]
            [hive-mcp.validation :as v]
            [hive-mcp.org-clj.parser :as org-parser]
            [hive-mcp.org-clj.writer :as org-writer]
            [hive-mcp.org-clj.query :as org-query]
            [hive-mcp.org-clj.transform :as org-transform]
            [hive-mcp.org-clj.render :as org-render]
            [hive-mcp.prompt-capture :as prompt-capture]
            [hive-mcp.chroma :as chroma]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            ;; Domain-specific tool modules (SOLID refactoring)
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.buffer :as buffer]
            [hive-mcp.tools.memory :as memory]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.tools.cider :as cider]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.tools.projectile :as projectile]
            [hive-mcp.tools.kanban :as kanban]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.tools.org :as org]
            [hive-mcp.tools.prompt :as prompt]
            [hive-mcp.tools.presets :as presets-tools]
            [hive-mcp.tools.diff :as diff]
            [hive-mcp.tools.jvm :as jvm]
            [hive-mcp.tools.crystal :as crystal]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.channel :as channel]
            [hive-mcp.agent :as agent]))

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
;; hive-mcp.el Integration Tools
;; These tools require hive-mcp.el to be loaded in Emacs
;; ============================================================================

(defn hive-mcp-el-available?
  "Check if hive-mcp.el is loaded in Emacs."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp-api)")]
    (and success (= result "t"))))

(defn handle-mcp-get-context
  "Get full context from Emacs including buffer, project, git, and memory."
  [_]
  (log/info "mcp-get-context")
  (if (hive-mcp-el-available?)
    (let [{:keys [success result error]} (ec/eval-elisp "(json-encode (hive-mcp-api-get-context))")]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: hive-mcp.el is not loaded. Run (require 'hive-mcp) and (hive-mcp-mode 1) in Emacs." :isError true}))

(defn handle-mcp-memory-add
  "Add an entry to project memory. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-add params))

(defn handle-mcp-memory-query
  "Query project memory by type with scope filtering. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-query params))

(defn handle-mcp-memory-query-metadata
  "Query project memory by type, returning only metadata. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-query-metadata params))

(defn handle-mcp-memory-get-full
  "Get full content of a memory entry by ID. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-get-full params))

(defn handle-mcp-memory-search-semantic
  "Search project memory using semantic similarity. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-search-semantic params))

(defn handle-mcp-memory-set-duration
  "Set duration category for a memory entry. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-set-duration params))

(defn handle-mcp-memory-promote
  "Promote memory entry to longer duration. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-promote params))

(defn handle-mcp-memory-demote
  "Demote memory entry to shorter duration. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-demote params))

(defn handle-mcp-memory-cleanup-expired
  "Remove all expired memory entries. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-cleanup-expired params))

(defn handle-mcp-memory-expiring-soon
  "List memory entries expiring within N days. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-expiring-soon params))

(defn handle-mcp-memory-log-access
  "Log access to a memory entry. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-log-access params))

(defn handle-mcp-memory-feedback
  "Submit helpfulness feedback for a memory entry. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-feedback params))

(defn handle-mcp-memory-helpfulness-ratio
  "Get helpfulness ratio for a memory entry. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-helpfulness-ratio params))

(defn handle-mcp-memory-check-duplicate
  "Check if content already exists in memory. Delegates to memory module."
  [params]
  (memory/handle-mcp-memory-check-duplicate params))

(defn handle-mcp-run-workflow
  "Run a user-defined workflow."
  [{:keys [name args]}]
  (log/info "mcp-run-workflow:" name)
  (if (hive-mcp-el-available?)
    (let [elisp (if args
                  (format "(json-encode (hive-mcp-api-run-workflow %s '%s))"
                          (pr-str name)
                          (pr-str args))
                  (format "(json-encode (hive-mcp-api-run-workflow %s))"
                          (pr-str name)))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: hive-mcp.el is not loaded." :isError true}))

(defn handle-mcp-notify
  "Show notification to user in Emacs."
  [{:keys [message type]}]
  (log/info "mcp-notify:" message)
  (let [type-str (or type "info")
        elisp (format "(hive-mcp-api-notify %s %s)"
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
  (if (hive-mcp-el-available?)
    (let [{:keys [success result error]} (ec/eval-elisp "(json-encode (hive-mcp-api-list-workflows))")]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "Error: hive-mcp.el is not loaded." :isError true}))

(defn handle-mcp-capabilities
  "Check hive-mcp.el availability and capabilities."
  [_]
  (log/info "mcp-capabilities")
  (if (hive-mcp-el-available?)
    (let [{:keys [success result error]} (ec/eval-elisp "(json-encode (hive-mcp-api-capabilities))")]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text"
     :text (json/write-str {:available false
                            :message "hive-mcp.el is not loaded. Run (require 'hive-mcp) and (hive-mcp-mode 1) in Emacs."})}))

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
;; CIDER Integration Tools - Delegated to hive-mcp.tools.cider
;; ============================================================================

(defn handle-cider-status [params]
  (cider/handle-cider-status params))

(defn handle-cider-eval-silent [params]
  (cider/handle-cider-eval-silent params))

(defn handle-cider-eval-explicit [params]
  (cider/handle-cider-eval-explicit params))

(defn handle-cider-spawn-session [params]
  (cider/handle-cider-spawn-session params))

(defn handle-cider-list-sessions [params]
  (cider/handle-cider-list-sessions params))

(defn handle-cider-eval-session [params]
  (cider/handle-cider-eval-session params))

(defn handle-cider-kill-session [params]
  (cider/handle-cider-kill-session params))

(defn handle-cider-kill-all-sessions [params]
  (cider/handle-cider-kill-all-sessions params))

;; ============================================================
;; Magit Integration Tools - Delegations to hive-mcp.tools.magit
;; ============================================================

(defn handle-magit-status [params]
  (magit/handle-magit-status params))

(defn handle-magit-branches [params]
  (magit/handle-magit-branches params))

(defn handle-magit-log [params]
  (magit/handle-magit-log params))

(defn handle-magit-diff [params]
  (magit/handle-magit-diff params))

(defn handle-magit-stage [params]
  (magit/handle-magit-stage params))

(defn handle-magit-commit [params]
  (magit/handle-magit-commit params))

(defn handle-magit-push [params]
  (magit/handle-magit-push params))

(defn handle-magit-pull [params]
  (magit/handle-magit-pull params))

(defn handle-magit-fetch [params]
  (magit/handle-magit-fetch params))

(defn handle-magit-feature-branches [params]
  (magit/handle-magit-feature-branches params))

;; Projectile Integration Handlers (requires hive-mcp-projectile addon)

(defn projectile-addon-available?
  "Check if hive-mcp-projectile addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp-projectile)")]
    (and success (= result "t"))))

(defn handle-projectile-info
  "Get current project info including name, root, type, and file count."
  [_]
  (log/info "projectile-info")
  (let [elisp (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-project-info)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-files
  "List files in current project, optionally filtered by pattern."
  [{:keys [pattern]}]
  (log/info "projectile-files" {:pattern pattern})
  (let [elisp (if pattern
                (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-project-files pattern)
                (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-project-files))
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-find-file
  "Find files matching a filename in current project."
  [{:keys [filename]}]
  (log/info "projectile-find-file" {:filename filename})
  (let [elisp (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-find-file filename)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-search
  "Search project for a pattern using ripgrep or grep."
  [{:keys [pattern]}]
  (log/info "projectile-search" {:pattern pattern})
  (let [elisp (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-search pattern)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-recent
  "Get recently visited files in current project."
  [_]
  (log/info "projectile-recent")
  (let [elisp (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-recent-files)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

(defn handle-projectile-list-projects
  "List all known projectile projects."
  [_]
  (log/info "projectile-list-projects")
  (let [elisp (el/require-and-call-json 'hive-mcp-projectile 'hive-mcp-projectile-api-list-projects)
        {:keys [success result error]} (ec/eval-elisp elisp)]
    (if success
      {:type "text" :text result}
      {:type "text" :text (str "Error: " error) :isError true})))

;;; ============================================================================
;;; Kanban Tools (org-kanban integration)
;;; ============================================================================

(defn kanban-addon-available?
  "Check if hive-mcp-org-kanban addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp-org-kanban)")]
    (and success (= result "t"))))

(defn handle-mcp-kanban-status
  "Get kanban status including tasks by status, progress, and backend info."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (hive-mcp-kanban-api-status))")]
      (mcp-success (str result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded. Run (hive-mcp-addon-load 'org-kanban)")))

(defn handle-mcp-kanban-list-tasks
  "List kanban tasks, optionally filtered by status."
  [{:keys [status]}]
  (if (kanban-addon-available?)
    (let [elisp (if status
                  (format "(json-encode (hive-mcp-kanban-list-tasks nil \"%s\"))" status)
                  "(json-encode (hive-mcp-kanban-list-tasks))")
          result (ec/eval-elisp elisp)]
      (mcp-success (str result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-create-task
  "Create a new kanban task."
  [{:keys [title description]}]
  (if (kanban-addon-available?)
    (let [elisp (if description
                  (format "(json-encode (hive-mcp-kanban-create-task \"%s\" \"%s\"))"
                          (v/escape-elisp-string title)
                          (v/escape-elisp-string description))
                  (format "(json-encode (hive-mcp-kanban-create-task \"%s\"))"
                          (v/escape-elisp-string title)))
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Created task: " result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-update-task
  "Update a kanban task's status or title."
  [{:keys [task_id status title]}]
  (if (kanban-addon-available?)
    (let [props (cond-> ""
                  status (str (format ":status \"%s\" " status))
                  title (str (format ":title \"%s\" " (v/escape-elisp-string title))))
          elisp (format "(hive-mcp-kanban-update-task \"%s\" %s)" task_id props)
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Updated task: " result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-move-task
  "Move a task to a new status column."
  [{:keys [task_id new_status]}]
  (if (kanban-addon-available?)
    (let [elisp (format "(hive-mcp-kanban-move-task \"%s\" \"%s\")" task_id new_status)
          result (ec/eval-elisp elisp)]
      (mcp-success (str "Moved task to " new_status)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-roadmap
  "Get roadmap view with milestones and progress."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (hive-mcp-kanban-api-roadmap))")]
      (mcp-success (str result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-my-tasks
  "Get tasks assigned to or modified by the current agent."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(json-encode (hive-mcp-kanban-api-my-tasks))")]
      (mcp-success (str result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

(defn handle-mcp-kanban-sync
  "Sync tasks between vibe-kanban and standalone backends."
  [_]
  (if (kanban-addon-available?)
    (let [result (ec/eval-elisp "(hive-mcp-kanban-sync-all)")]
      (mcp-success (str "Sync complete: " result)))
    (mcp-error "hive-mcp-org-kanban addon not loaded.")))

;; =============================================================================
;; Swarm Orchestration Tools (requires hive-mcp-swarm addon)
;; =============================================================================

(defn swarm-addon-available?
  "Check if hive-mcp-swarm addon is loaded."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp-swarm)")]
    (and success (= result "t"))))

(defn handle-swarm-spawn
  "Spawn a new Claude slave instance."
  [{:keys [name presets cwd role terminal]}]
  (if (swarm-addon-available?)
    (let [presets-str (when (seq presets)
                        (format "'(%s)" (clojure.string/join " " (map #(format "\"%s\"" %) presets))))
          elisp (format "(json-encode (hive-mcp-swarm-api-spawn \"%s\" %s %s %s))"
                        (v/escape-elisp-string (or name "slave"))
                        (or presets-str "nil")
                        (if cwd (format "\"%s\"" (v/escape-elisp-string cwd)) "nil")
                        (if terminal (format "\"%s\"" terminal) "nil"))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded. Run (require 'hive-mcp-swarm)" :isError true}))

(defn handle-swarm-dispatch
  "Dispatch a prompt to a slave."
  [{:keys [slave_id prompt timeout_ms]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (hive-mcp-swarm-api-dispatch \"%s\" \"%s\" %s))"
                        (v/escape-elisp-string slave_id)
                        (v/escape-elisp-string prompt)
                        (or timeout_ms "nil"))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-status
  "Get swarm status including all slaves and their states."
  [{:keys [slave_id]}]
  (if (swarm-addon-available?)
    (let [elisp (if slave_id
                  (format "(json-encode (hive-mcp-swarm-status \"%s\"))" slave_id)
                  "(json-encode (hive-mcp-swarm-api-status))")
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-collect
  "Collect response from a task."
  [{:keys [task_id timeout_ms]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (hive-mcp-swarm-api-collect \"%s\" %s))"
                        (v/escape-elisp-string task_id)
                        (or timeout_ms "nil"))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-list-presets
  "List available swarm presets."
  [_]
  (if (swarm-addon-available?)
    (let [{:keys [success result error]} (ec/eval-elisp "(json-encode (hive-mcp-swarm-api-list-presets))")]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-kill
  "Kill a slave or all slaves."
  [{:keys [slave_id]}]
  (if (swarm-addon-available?)
    (let [elisp (if (= slave_id "all")
                  "(json-encode (hive-mcp-swarm-api-kill-all))"
                  (format "(json-encode (hive-mcp-swarm-api-kill \"%s\"))"
                          (v/escape-elisp-string slave_id)))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

(defn handle-swarm-broadcast
  "Broadcast a prompt to all slaves."
  [{:keys [prompt]}]
  (if (swarm-addon-available?)
    (let [elisp (format "(json-encode (hive-mcp-swarm-broadcast \"%s\"))"
                        (v/escape-elisp-string prompt))
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text result}
        {:type "text" :text (str "Error: " error) :isError true}))
    {:type "text" :text "hive-mcp-swarm addon not loaded." :isError true}))

;; ============================================================
;; JVM Process Cleanup - Delegated to hive-mcp.tools.jvm
;; ============================================================

(defn handle-jvm-cleanup
  "Find and optionally kill orphaned JVM processes.
   Delegates to hive-mcp.tools.jvm module."
  [params]
  (jvm/handle-jvm-cleanup params))

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
               crystal/tools
               memory/tools
               mem-kanban/tools
               cider/tools
               magit/tools
               projectile/tools
               kanban/tools
               swarm/tools
               org/tools
               prompt/tools
               presets-tools/tools
               diff/tools
               hivemind/tools
               channel/channel-tools
               agent/tools)))

(defn get-tool-by-name
  "Find a tool definition by name."
  [name]
  (first (filter #(= (:name %) name) tools)))
