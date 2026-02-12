(ns hive-mcp.agent.ling.agent-sdk-strategy
  "Agent SDK spawn strategy for Claude Code SDK ling lifecycle."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.agent.headless-sdk :as sdk]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord AgentSDKStrategy []
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (let [{:keys [id cwd presets model agents]} ling-ctx
          {:keys [task]} opts
          effective-agents (or (:agents opts) agents)]
      (when-not (sdk/available?)
        (throw (ex-info "Claude Agent SDK not available for spawn"
                        {:ling-id id
                         :sdk-status (sdk/sdk-status)
                         :spawn-mode :agent-sdk
                         :hint (case (sdk/sdk-status)
                                 :no-libpython "Add clj-python/libpython-clj to deps.edn"
                                 :no-sdk "Run: pip install claude-code-sdk"
                                 :not-initialized "Python initialization failed"
                                 "Unknown SDK issue")})))
      (let [system-prompt (str "Agent " id " in project. Use hive-mcp tools for coordination.")
            mcp-servers {"hive" {"type" "stdio"
                                 "command" "bb"
                                 "args" ["-cp" (str cwd "/src") "-m" "hive-mcp.server.core"]
                                 "env" {"BB_MCP_PROJECT_DIR" cwd}}}
            spawn-env (cond-> {"CLAUDE_SWARM_SLAVE_ID" id
                               "BB_MCP_PROJECT_DIR" cwd}
                        (and model (not= model "claude"))
                        (assoc "OPENROUTER_MODEL" model))
            result (sdk/spawn-headless-sdk! id (cond-> {:cwd cwd
                                                        :system-prompt system-prompt
                                                        :mcp-servers mcp-servers
                                                        :presets presets
                                                        :env spawn-env}
                                                 effective-agents (assoc :agents effective-agents)))]
        (log/info "Ling spawned via Agent SDK" {:id id :cwd cwd :model (or model "claude")
                                                :backend :agent-sdk :phase (:phase result)
                                                :agents-count (count effective-agents)})
        (when task
          (sdk/dispatch-headless-sdk! id task))
        id)))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (let [{:keys [id]} ling-ctx
          {:keys [task]} task-opts
          dispatch-opts (cond-> (select-keys task-opts [:skip-silence? :skip-abstract? :phase :raw?])
                          (:dispatch-context task-opts)
                          (assoc :dispatch-context (:dispatch-context task-opts)))]
      (when-not (sdk/get-session id)
        (throw (ex-info "Agent SDK session not found for dispatch"
                        {:ling-id id})))
      (let [result-ch (sdk/dispatch-headless-sdk! id task dispatch-opts)]
        (log/info "Task dispatched to Agent SDK ling" {:ling-id id
                                                       :has-result-ch? (some? result-ch)
                                                       :raw? (:raw? dispatch-opts false)})
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
                      :sdk-backend (:backend sdk-info)
                      :sdk-turn-count (or (:turn-count sdk-info) 0)
                      :sdk-has-persistent-client? (boolean (:has-persistent-client? sdk-info))
                      :sdk-interruptable? (boolean (:interruptable? sdk-info)))
          (nil? ds-status) (assoc :slave/status :idle))
        (if ds-status
          (assoc ds-status :sdk-alive? false)
          nil))))

  (strategy-kill! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      (try
        (let [_ (sdk/kill-headless-sdk! id)]
          (log/info "Agent SDK ling killed" {:id id})
          {:killed? true :id id :backend :agent-sdk})
        (catch clojure.lang.ExceptionInfo e
          (log/warn "Agent SDK kill exception" {:id id :error (ex-message e)})
          {:killed? true :id id :reason :session-not-found}))))

  (strategy-interrupt! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      (log/info "Interrupting Agent SDK ling" {:id id})
      (sdk/interrupt-headless-sdk! id))))

(defn ->agent-sdk-strategy
  "Create an AgentSDKStrategy instance."
  []
  (->AgentSDKStrategy))

(defn sdk-available?
  "Check if the Agent SDK backend is available."
  []
  (sdk/available?))
