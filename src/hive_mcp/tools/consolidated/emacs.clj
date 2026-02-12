(ns hive-mcp.tools.consolidated.emacs
  "Consolidated Emacs CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.emacs.elisp :as el]
            [taoensso.timbre :as log]))

(defn handle-eval
  "Evaluate Elisp code."
  [{:keys [code]}]
  (log/info "emacs-eval" {:code-length (count code)})
  (try
    (let [{:keys [success result error]} (ec/eval-elisp code)]
      (if success
        (mcp-success result)
        (mcp-error (str "Error: " error))))
    (catch Exception e
      (mcp-error (str "Failed: " (.getMessage e))))))

(defn handle-buffers
  "List Emacs buffers."
  [_]
  (log/info "emacs-buffers")
  (try
    (let [elisp "(json-encode (mapcar (lambda (b) (list :name (buffer-name b) :file (buffer-file-name b))) (buffer-list)))"
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-json {:buffers result})
        (mcp-error (str "Error: " error))))
    (catch Exception e
      (mcp-error (str "Failed: " (.getMessage e))))))

(defn handle-notify
  "Send notification to Emacs."
  [{:keys [message level]}]
  (log/info "emacs-notify" {:message message :level level})
  (try
    (let [level-kw (or level "info")
          elisp (el/format-elisp "(message \"[%s] %s\")" level-kw message)
          {:keys [success error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success "Notification sent")
        (mcp-error (str "Error: " error))))
    (catch Exception e
      (mcp-error (str "Failed: " (.getMessage e))))))

(defn handle-status
  "Get Emacs connection status."
  [_]
  (log/info "emacs-status")
  (try
    (mcp-json {:running (ec/emacs-running?)
               :current-buffer (ec/current-buffer)
               :current-file (ec/current-file)})
    (catch Exception e
      (mcp-error (str "Failed: " (.getMessage e))))))

(defn handle-switch-buffer
  "Switch to a buffer."
  [{:keys [buffer]}]
  (log/info "emacs-switch" {:buffer buffer})
  (try
    (let [elisp (el/format-elisp "(switch-to-buffer %s)" (pr-str buffer))
          {:keys [success error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success (str "Switched to " buffer))
        (mcp-error (str "Error: " error))))
    (catch Exception e
      (mcp-error (str "Failed: " (.getMessage e))))))

(defn handle-find-file
  "Open a file in Emacs."
  [{:keys [file]}]
  (log/info "emacs-find-file" {:file file})
  (try
    (let [elisp (el/format-elisp "(find-file %s)" (pr-str file))
          {:keys [success error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success (str "Opened " file))
        (mcp-error (str "Error: " error))))
    (catch Exception e
      (mcp-error (str "Failed: " (.getMessage e))))))

(defn handle-save
  "Save current buffer or all buffers."
  [{:keys [all]}]
  (log/info "emacs-save" {:all all})
  (try
    (let [elisp (if all "(save-some-buffers t)" "(save-buffer)")
          {:keys [success error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-success (if all "All buffers saved" "Buffer saved"))
        (mcp-error (str "Error: " error))))
    (catch Exception e
      (mcp-error (str "Failed: " (.getMessage e))))))

(defn handle-current-buffer
  "Get current buffer info."
  [_]
  (log/info "emacs-current-buffer")
  (try
    (let [elisp "(json-encode (list :name (buffer-name) :file (buffer-file-name) :modified (buffer-modified-p) :major-mode (symbol-name major-mode)))"
          {:keys [success result error]} (ec/eval-elisp elisp)]
      (if success
        (mcp-json {:buffer result})
        (mcp-error (str "Error: " error))))
    (catch Exception e
      (mcp-error (str "Failed: " (.getMessage e))))))

(def handlers
  {:eval    handle-eval
   :buffers handle-buffers
   :notify  handle-notify
   :status  handle-status
   :switch  handle-switch-buffer
   :find    handle-find-file
   :save    handle-save
   :current handle-current-buffer})

(def handle-emacs
  (make-cli-handler handlers))

(def tool-def
  {:name "emacs"
   :consolidated true
   :description "Emacs operations: eval (run elisp), buffers (list), notify (message), status (connection), switch (change buffer), find (open file), save (save buffers), current (buffer info). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["eval" "buffers" "notify" "status" "switch" "find" "save" "current" "help"]
                                         :description "Emacs operation to perform"}
                              "code" {:type "string"
                                      :description "Elisp code to evaluate"}
                              "message" {:type "string"
                                         :description "Notification message"}
                              "level" {:type "string"
                                       :enum ["info" "warn" "error"]
                                       :description "Notification level"}
                              "buffer" {:type "string"
                                        :description "Buffer name to switch to"}
                              "file" {:type "string"
                                      :description "File path to open"}
                              "all" {:type "boolean"
                                     :description "Save all buffers if true"}}
                 :required ["command"]}
   :handler handle-emacs})

(def tools [tool-def])
