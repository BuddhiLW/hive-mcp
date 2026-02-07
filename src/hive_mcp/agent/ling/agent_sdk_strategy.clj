(ns hive-mcp.agent.ling.agent-sdk-strategy
  "Agent SDK spawn strategy — Claude Agent SDK (Python) ling lifecycle.

   Delegates to hive-mcp.agent.headless-sdk for SDK session management.
   Uses libpython-clj to bridge Clojure ↔ Python, running the Claude Agent SDK
   in the same JVM process with direct DataScript access.

   SAA (Silence-Abstract-Act) phases are managed by the SDK module.

   SOLID: Single Responsibility — only SDK strategy adaptation.
   CLARITY: L — Pure adapter between ILingStrategy and headless-sdk module."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.agent.headless-sdk :as sdk]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Agent SDK Strategy Implementation
;;; =============================================================================

(defrecord AgentSDKStrategy []
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (let [{:keys [id cwd presets model agents]} ling-ctx
          {:keys [task]} opts
          ;; Agents from opts take precedence over ling-ctx (spawn-time override)
          effective-agents (or (:agents opts) agents)]
      ;; Graceful degradation: check SDK availability before attempting spawn
      (when-not (sdk/available?)
        (throw (ex-info "Claude Agent SDK not available for spawn"
                        {:ling-id id
                         :sdk-status (sdk/sdk-status)
                         :spawn-mode :agent-sdk
                         :hint (case (sdk/sdk-status)
                                 :no-libpython "Add clj-python/libpython-clj to deps.edn"
                                 :no-sdk "Run: pip install claude-agent-sdk"
                                 :not-initialized "Python initialization failed"
                                 "Unknown SDK issue")})))
      (let [result (sdk/spawn-headless-sdk! id (cond-> {:cwd cwd
                                                        :system-prompt (str "Agent " id " in project")
                                                        :presets presets}
                                                 effective-agents (assoc :agents effective-agents)))]
        (log/info "Ling spawned via Agent SDK" {:id id :cwd cwd :model (or model "claude")
                                                :backend :agent-sdk :phase (:phase result)
                                                :agents-count (count effective-agents)})
        ;; If initial task provided, dispatch immediately
        (when task
          (sdk/dispatch-headless-sdk! id task))
        ;; Return the ling-id (consistent with other strategies)
        id)))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (let [{:keys [id]} ling-ctx
          {:keys [task]} task-opts
          dispatch-opts (select-keys task-opts [:skip-silence? :skip-abstract? :phase])]
      (when-not (sdk/get-session id)
        (throw (ex-info "Agent SDK session not found for dispatch"
                        {:ling-id id})))
      (let [result-ch (sdk/dispatch-headless-sdk! id task dispatch-opts)]
        (log/info "Task dispatched to Agent SDK ling" {:ling-id id
                                                       :has-result-ch? (some? result-ch)})
        ;; Return the result channel so callers can consume SAA messages
        result-ch)))

  (strategy-status [_ ling-ctx ds-status]
    (let [{:keys [id]} ling-ctx
          sdk-info (sdk/sdk-status-for id)]
      (if sdk-info
        (cond-> (or ds-status {})
          true (assoc :slave/id id
                      :ling/spawn-mode :agent-sdk
                      :sdk-alive? true
                      :sdk-phase (:phase sdk-info)
                      :sdk-session-id (:session-id sdk-info)
                      :sdk-observations-count (:observations-count sdk-info)
                      :sdk-started-at (:started-at sdk-info)
                      :sdk-uptime-ms (:uptime-ms sdk-info)
                      :sdk-backend (:backend sdk-info))
          (nil? ds-status) (assoc :slave/status :idle))
        ;; No SDK session found
        (if ds-status
          (assoc ds-status :sdk-alive? false)
          nil))))

  (strategy-kill! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      (try
        (let [result (sdk/kill-headless-sdk! id)]
          (log/info "Agent SDK ling killed" {:id id})
          {:killed? true :id id :backend :agent-sdk})
        (catch clojure.lang.ExceptionInfo e
          ;; Session might not exist (already dead or never spawned)
          (log/warn "Agent SDK kill exception" {:id id :error (ex-message e)})
          {:killed? true :id id :reason :session-not-found})))))

;;; =============================================================================
;;; Factory
;;; =============================================================================

(defn ->agent-sdk-strategy
  "Create an AgentSDKStrategy instance."
  []
  (->AgentSDKStrategy))

;;; =============================================================================
;;; Convenience Predicates
;;; =============================================================================

(defn sdk-available?
  "Check if the Agent SDK backend is available for use.
   Delegates to headless-sdk/available?."
  []
  (sdk/available?))

(comment
  ;; Usage examples

  ;; Create strategy
  ;; (def strategy (->agent-sdk-strategy))

  ;; Check availability
  ;; (sdk-available?)

  ;; Spawn via strategy
  ;; (strategy-spawn! strategy
  ;;   {:id "sdk-ling-1" :cwd "/tmp" :presets ["ling" "mcp-first"]}
  ;;   {:task "Explore the codebase"})

  ;; Status
  ;; (strategy-status strategy {:id "sdk-ling-1"} nil)

  ;; Kill
  ;; (strategy-kill! strategy {:id "sdk-ling-1"})
  )
