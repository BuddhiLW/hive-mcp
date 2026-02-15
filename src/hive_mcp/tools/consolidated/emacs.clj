(ns hive-mcp.tools.consolidated.emacs
  "Consolidated Emacs CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.dns.result :as result]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.emacs.elisp :as el]
            [taoensso.timbre :as log]))

(defn- elisp->result
  "Convert emacs client response {:keys [success result error]} to Result."
  [resp]
  (if (:success resp)
    (result/ok (:result resp))
    (result/err :emacs/eval-error {:message (str (:error resp))})))

;; ── logic* fns (pure Result-returning) ───────────────────────────────────────

(defn- eval* [{:keys [code]}]
  (elisp->result (ec/eval-elisp code)))

(defn- buffers* [_]
  (let [elisp "(json-encode (mapcar (lambda (b) (list :name (buffer-name b) :file (buffer-file-name b))) (buffer-list)))"]
    (result/map-ok (elisp->result (ec/eval-elisp elisp))
                   (fn [r] {:buffers r}))))

(defn- notify* [{:keys [message level]}]
  (let [level-kw (or level "info")
        elisp (el/format-elisp "(message \"[%s] %s\")" level-kw message)]
    (result/map-ok (elisp->result (ec/eval-elisp elisp))
                   (constantly "Notification sent"))))

(defn- status* [_]
  (result/ok {:running (ec/emacs-running?)
              :current-buffer (ec/current-buffer)
              :current-file (ec/current-file)}))

(defn- switch-buffer* [{:keys [buffer]}]
  (let [elisp (el/format-elisp "(switch-to-buffer %s)" (pr-str buffer))]
    (result/map-ok (elisp->result (ec/eval-elisp elisp))
                   (constantly (str "Switched to " buffer)))))

(defn- find-file* [{:keys [file]}]
  (let [elisp (el/format-elisp "(find-file %s)" (pr-str file))]
    (result/map-ok (elisp->result (ec/eval-elisp elisp))
                   (constantly (str "Opened " file)))))

(defn- save* [{:keys [all]}]
  (let [elisp (if all "(save-some-buffers t)" "(save-buffer)")]
    (result/map-ok (elisp->result (ec/eval-elisp elisp))
                   (constantly (if all "All buffers saved" "Buffer saved")))))

(defn- current-buffer* [_]
  (let [elisp "(json-encode (list :name (buffer-name) :file (buffer-file-name) :modified (buffer-modified-p) :major-mode (symbol-name major-mode)))"]
    (result/map-ok (elisp->result (ec/eval-elisp elisp))
                   (fn [r] {:buffer r}))))

;; ── public handlers (MCP boundary) ──────────────────────────────────────────

(defn handle-eval
  "Evaluate Elisp code."
  [{:keys [code] :as params}]
  (log/info "emacs-eval" {:code-length (count code)})
  (rb/result->mcp-text (rb/try-result :emacs/eval-failed #(eval* params))))

(defn handle-buffers
  "List Emacs buffers."
  [params]
  (log/info "emacs-buffers")
  (rb/result->mcp (rb/try-result :emacs/buffers-failed #(buffers* params))))

(defn handle-notify
  "Send notification to Emacs."
  [{:keys [message level] :as params}]
  (log/info "emacs-notify" {:message message :level level})
  (rb/result->mcp-text (rb/try-result :emacs/notify-failed #(notify* params))))

(defn handle-status
  "Get Emacs connection status."
  [params]
  (log/info "emacs-status")
  (rb/result->mcp (rb/try-result :emacs/status-failed #(status* params))))

(defn handle-switch-buffer
  "Switch to a buffer."
  [{:keys [buffer] :as params}]
  (log/info "emacs-switch" {:buffer buffer})
  (rb/result->mcp-text (rb/try-result :emacs/switch-failed #(switch-buffer* params))))

(defn handle-find-file
  "Open a file in Emacs."
  [{:keys [file] :as params}]
  (log/info "emacs-find-file" {:file file})
  (rb/result->mcp-text (rb/try-result :emacs/find-file-failed #(find-file* params))))

(defn handle-save
  "Save current buffer or all buffers."
  [{:keys [all] :as params}]
  (log/info "emacs-save" {:all all})
  (rb/result->mcp-text (rb/try-result :emacs/save-failed #(save* params))))

(defn handle-current-buffer
  "Get current buffer info."
  [params]
  (log/info "emacs-current-buffer")
  (rb/result->mcp (rb/try-result :emacs/current-buffer-failed #(current-buffer* params))))

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
