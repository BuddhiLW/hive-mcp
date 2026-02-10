(ns hive-mcp.agent.ling.vterm
  "Vterm spawn strategy — Emacs vterm buffer-based ling lifecycle.

   Delegates spawn/dispatch/status/kill to emacsclient elisp functions.
   Requires a running Emacs daemon with hive-mcp-swarm addon loaded.

   SOLID: Single Responsibility — only vterm/elisp interaction.
   CLARITY: L — Pure adapter between ILingStrategy and emacsclient."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-mcp.emacs.client :as ec]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Vterm Strategy Implementation
;;; =============================================================================

(defrecord VtermStrategy []
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (let [{:keys [id cwd presets project-id]} ling-ctx
          {:keys [task kanban-task-id terminal]
           :or {}} opts
          preset-list (or (:presets opts) presets)
          preset-str (or (swarm-core/format-elisp-list preset-list pr-str) "nil")
          elisp-code (format "(hive-mcp-swarm-api-spawn \"%s\" %s %s %s %s nil)"
                             id
                             preset-str
                             (if cwd (format "\"%s\"" cwd) "nil")
                             (if terminal (format "\"%s\"" terminal) "nil")
                             (if kanban-task-id (format "\"%s\"" kanban-task-id) "nil"))
          result (ec/eval-elisp-with-timeout elisp-code 10000)]
      (if (:success result)
        (let [elisp-slave-id (str/trim (or (:result result) id))]
          (log/info "Ling spawned via elisp (vterm)" {:requested-id id
                                                      :elisp-slave-id elisp-slave-id})
          elisp-slave-id)
        (do
          (log/error "Failed to spawn ling via elisp" {:id id :error (:error result)})
          (throw (ex-info "Failed to spawn ling"
                          {:id id :error (:error result)}))))))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (let [{:keys [id]} ling-ctx
          {:keys [task timeout-ms]
           :or {timeout-ms 60000}} task-opts
          escaped-prompt (-> task
                             (str/replace "\\" "\\\\")
                             (str/replace "\"" "\\\"")
                             (str/replace "\n" "\\n"))
          elisp-code (format "(hive-mcp-swarm-api-dispatch \"%s\" \"%s\" %d)"
                             id
                             escaped-prompt
                             timeout-ms)
          result (ec/eval-elisp-with-timeout elisp-code timeout-ms)]
      (if (:success result)
        (do
          (log/info "Task dispatched to ling via elisp (vterm)" {:ling-id id})
          true)
        (do
          (log/error "Failed to dispatch to ling via elisp" {:ling-id id :error (:error result)})
          (throw (ex-info "Vterm dispatch failed"
                          {:ling-id id :error (:error result)}))))))

  (strategy-status [_ ling-ctx ds-status]
    (let [{:keys [id]} ling-ctx
          elisp-result (when (or (nil? ds-status)
                                 (= :unknown (:slave/status ds-status)))
                         (ec/eval-elisp-with-timeout
                          (format "(hive-mcp-swarm-get-slave-status \"%s\")" id)
                          3000))]
      (if ds-status
        (cond-> ds-status
          (and (:success elisp-result)
               (not= (:result elisp-result) "nil"))
          (assoc :elisp-alive? true))
        (when (:success elisp-result)
          {:slave/id id
           :slave/status (if (= (:result elisp-result) "nil")
                           :dead
                           :unknown)
           :elisp-raw (:result elisp-result)}))))

  (strategy-kill! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx
          elisp-result (ec/eval-elisp-with-timeout
                        (format "(hive-mcp-swarm-slaves-kill \"%s\")" id)
                        5000)
          kill-succeeded? (and (:success elisp-result)
                               (not (nil? (:result elisp-result)))
                               (not= "nil" (:result elisp-result)))]
      (if kill-succeeded?
        (do
          (log/info "Ling killed via elisp (vterm)" {:id id})
          {:killed? true :id id})
        (do
          (log/warn "Elisp kill failed - NOT removing from DataScript"
                    {:id id :elisp-result elisp-result})
          {:killed? false :id id :reason :elisp-kill-failed}))))

  (strategy-interrupt! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      {:success? false
       :ling-id id
       :errors ["Interrupt not supported for vterm spawn mode"]})))

;;; =============================================================================
;;; Factory
;;; =============================================================================

(defn ->vterm-strategy
  "Create a VtermStrategy instance."
  []
  (->VtermStrategy))
