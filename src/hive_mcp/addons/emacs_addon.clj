(ns hive-mcp.addons.emacs-addon
  "EmacsAddon — IAddon implementation for the Emacs editor integration.

   Wraps the EmacsclientEditor adapter and all Emacs-specific tool namespaces
   into a single composable addon unit. When initialized, sets the active
   IEditor backend to EmacsclientEditor. When shut down, clears it (falls
   back to NoopEditor).

   Extraction boundary: this addon + emacs/* + Emacs-specific tool files
   will move to a separate hive-emacs library in the future."
  (:require [hive-mcp.addons.protocol :as proto]
            [hive-mcp.dns.result :as r]
            [hive-mcp.protocols.editor :as ed]
            [hive-mcp.emacs.editor-adapter :as ema]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- collect-tools
  "Lazily collect tool definitions from Emacs-specific tool namespaces.
   Uses requiring-resolve to avoid compile-time coupling."
  []
  (let [tool-nses '[hive-mcp.tools.buffer
                    hive-mcp.tools.consolidated.emacs
                    hive-mcp.tools.cider
                    hive-mcp.tools.magit
                    hive-mcp.tools.kanban
                    hive-mcp.tools.projectile
                    hive-mcp.tools.docs]]
    (->> tool-nses
         (mapcat (fn [ns-sym]
                   (let [tools (r/guard Exception []
                                        (require ns-sym)
                                        (let [tools-var (ns-resolve ns-sym 'tools)
                                              docs-var (ns-resolve ns-sym 'docs-tools)]
                                          (cond
                                            tools-var @tools-var
                                            docs-var @docs-var
                                            :else [])))]
                     (when-let [err (:hive-dsl.result/error (meta tools))]
                       (log/warn "Failed to load tools from" ns-sym ":" (:message err)))
                     tools)))
         vec)))

(defrecord EmacsAddon [state-atom]
  proto/IAddon

  (addon-id [_] "hive.emacs")

  (addon-type [_] :native)

  (capabilities [_]
    #{:tools :health-reporting :editor})

  (initialize! [_ _config]
    (if @state-atom
      {:success? true :already-initialized? true}
      (let [result (r/try-effect* :addon/emacs-init-error
                                  (ed/set-editor! (ema/->emacsclient-editor))
                                  (reset! state-atom true)
                                  (log/info "EmacsAddon initialized — EmacsclientEditor wired as active IEditor")
                                  {:success? true
                                   :metadata {:editor-id (ed/editor-id (ed/get-editor))}})]
        (if (r/ok? result)
          (:ok result)
          (do (log/error "EmacsAddon initialization failed"
                         {:error (:message result) :class (:class result)})
              {:success? false
               :errors [(:message result)]})))))

  (shutdown! [_]
    (when @state-atom
      (ed/clear-editor!)
      (reset! state-atom false)
      (log/info "EmacsAddon shut down — IEditor cleared to NoopEditor"))
    {:success? true})

  (tools [_]
    (if @state-atom
      (collect-tools)
      []))

  (schema-extensions [_] {})

  (health [_]
    (if @state-atom
      (let [editor (ed/get-editor)
            available (ed/available? editor)]
        {:status (if available :ok :degraded)
         :details {:editor-id (ed/editor-id editor)
                   :emacs-running available}})
      {:status :down
       :details {:message "EmacsAddon not initialized"}})))

(defn ->emacs-addon
  "Create a new EmacsAddon instance."
  []
  (->EmacsAddon (atom false)))
