(ns hive-mcp.tools.buffer
  "Buffer and Emacs interaction tools.

   Handles buffer operations, file operations, and hive-mcp.el integration."
  (:require [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.telemetry :as telemetry]
            [hive-mcp.validation :as v]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Basic Buffer Operations
;; =============================================================================

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

;; =============================================================================
;; hive-mcp.el Integration Tools
;; These tools require hive-mcp.el to be loaded in Emacs
;; =============================================================================

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

(defn handle-mcp-notify
  "Show notification to user via desktop notification AND Emacs echo-area.
   Desktop notification ensures visibility even when Emacs is not focused."
  [{:keys [message type]}]
  (log/info "mcp-notify:" message)
  (let [type-str (or type "info")]
    ;; Send desktop notification (primary - catches attention)
    (require 'hive-mcp.notify)
    ((resolve 'hive-mcp.notify/notify!) {:summary "Hive-MCP"
                                         :body message
                                         :type type-str})
    ;; Also send to Emacs echo-area (secondary - visible if Emacs focused)
    (let [elisp (format "(hive-mcp-api-notify %s %s)"
                        (pr-str message)
                        (pr-str type-str))
          {:keys [success error]} (ec/eval-elisp elisp)]
      (if success
        {:type "text" :text "Notification sent"}
        {:type "text" :text (str "Desktop sent, Emacs error: " error)}))))

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

(defn- clj->elisp
  "Convert Clojure value to elisp syntax string.
   Maps become plists, vectors become lists, keywords become :keyword."
  [v]
  (cond
    (nil? v) "nil"
    (keyword? v) (str ":" (name v))
    (string? v) (pr-str v)
    (number? v) (str v)
    (boolean? v) (if v "t" "nil")
    (vector? v) (str "'(" (str/join " " (map clj->elisp v)) ")")
    (sequential? v) (str "'(" (str/join " " (map clj->elisp v)) ")")
    (map? v) (str "(list "
                  (str/join " " (mapcat (fn [[k val]]
                                          [(clj->elisp k) (clj->elisp val)])
                                        v))
                  ")")
    :else (pr-str (str v))))

(defn handle-mcp-run-workflow
  "Run a user-defined workflow.
   Special-cases 'catchup' to use native Clojure implementation for Chroma access."
  [{:keys [name args]}]
  (log/info "mcp-run-workflow:" name)
  ;; Special handling for catchup - use native Clojure implementation
  (if (= name "catchup")
    (catchup/handle-native-catchup args)
    ;; All other workflows go through elisp
    (if (hive-mcp-el-available?)
      (let [elisp (if args
                    (format "(json-encode (hive-mcp-api-run-workflow %s %s))"
                            (pr-str name)
                            (clj->elisp args))
                    (format "(json-encode (hive-mcp-api-run-workflow %s))"
                            (pr-str name)))
            {:keys [success result error]} (ec/eval-elisp elisp)]
        (if success
          {:type "text" :text result}
          {:type "text" :text (str "Error: " error) :isError true}))
      {:type "text" :text "Error: hive-mcp.el is not loaded." :isError true})))

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

;; =============================================================================
;; Tool Definitions
;; =============================================================================

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

   {:name "mcp_capabilities"
    :description "Check if hive-mcp.el is loaded and get available capabilities. Use this first to verify the enhanced features are available."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-capabilities}

   {:name "mcp_get_context"
    :description "Get full context from Emacs including current buffer, region, project info, git status, and project memory. Requires hive-mcp.el."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-get-context}

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

   {:name "mcp_list_workflows"
    :description "List available user-defined workflows. Requires hive-mcp.el."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-list-workflows}

   {:name "mcp_run_workflow"
    :description "Run a user-defined workflow by name. Workflows can automate multi-step tasks. Requires hive-mcp.el. For 'catchup' workflow: pass directory in args to get correct project context."
    :inputSchema {:type "object"
                  :properties {"name" {:type "string"
                                       :description "Name of the workflow to run"}
                               "args" {:type "object"
                                       :description "Optional arguments. For catchup: {\"directory\": \"/path/to/project\"} to specify project context."}}
                  :required ["name"]}
    :handler handle-mcp-run-workflow}

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
    :handler handle-mcp-buffer-info}])
