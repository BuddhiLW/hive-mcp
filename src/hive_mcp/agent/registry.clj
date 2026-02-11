(ns hive-mcp.agent.registry
  "Tool and agent registry for delegation."
  (:require [hive-mcp.agent.protocol :refer [IAgentRegistry agent-type]]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce registry (atom {}))

(declare register!)

(defonce ^:private initialized? (atom false))

(defn ensure-registered!
  "Lazily initialize tool registry on first use."
  []
  (when (and (empty? @registry)
             (not @initialized?))
    (reset! initialized? true)
    (try
      (log/info "Auto-registering tools for agent delegation...")
      (require 'hive-mcp.tools)
      (let [tools-var (resolve 'hive-mcp.tools/tools)]
        (when tools-var
          (register! @tools-var)))
      (catch Exception e
        (log/warn "Failed to auto-register tools:" (ex-message e))))))

(defn register!
  "Register tools for agent use."
  [tools]
  (doseq [{:keys [name handler] :as tool} tools]
    (swap! registry assoc name (assoc tool :handler handler)))
  (log/info "Registered" (count tools) "tools for agent delegation"))

(defn get-schemas
  "Get tool schemas for specified tool names (or all if nil)."
  [tool-names]
  (let [all-tools @registry
        selected (if tool-names
                   (select-keys all-tools tool-names)
                   all-tools)]
    (mapv #(dissoc % :handler) (vals selected))))

(defn get-tool
  "Get a tool by name from the registry."
  [tool-name]
  (get @registry tool-name))

(defn list-tools
  "List all registered tool names."
  []
  (keys @registry))

(defn refresh!
  "Clear and re-register all tools from hive-mcp.tools."
  []
  (reset! registry {})
  (reset! initialized? true)
  (try
    (log/info "[hot-reload] Refreshing tool registry...")
    (require 'hive-mcp.tools :reload)
    (let [tools-var (resolve 'hive-mcp.tools/tools)]
      (when tools-var
        (register! @tools-var))
      (log/info "[hot-reload] Tool registry refreshed:" (count @registry) "tools")
      (count @registry))
    (catch Exception e
      (log/error "[hot-reload] Failed to refresh tools:" (ex-message e))
      0)))

(defonce ^:private agents (atom {}))

(defrecord AgentRegistry []
  IAgentRegistry
  (register! [_ agent]
    (let [id (:id agent)]
      (swap! agents assoc id agent)
      (log/debug "Registered agent:" id)
      id))

  (unregister! [_ agent-id]
    (swap! agents dissoc agent-id)
    (log/debug "Unregistered agent:" agent-id)
    agent-id)

  (get-agent [_ agent-id]
    (get @agents agent-id))

  (list-agents [_]
    (vals @agents))

  (list-agents-by-type [_ t]
    (filter #(= t (agent-type %)) (vals @agents))))

(def agent-registry
  "Singleton agent registry instance."
  (->AgentRegistry))

(defn register-agent!
  "Register an agent in the registry."
  [agent]
  (.register! agent-registry agent))

(defn unregister-agent!
  "Remove an agent from the registry."
  [agent-id]
  (.unregister! agent-registry agent-id))

(defn get-agent-by-id
  "Get an agent by ID."
  [id]
  (.get-agent agent-registry id))

(defn list-all-agents
  "List all registered agents."
  []
  (.list-agents agent-registry))

(defn list-lings
  "List all ling agents."
  []
  (.list-agents-by-type agent-registry :ling))

(defn list-drones
  "List all drone agents."
  []
  (.list-agents-by-type agent-registry :drone))

(defn sync-from-datascript!
  "Sync agent registry with DataScript slave state."
  []
  (let [ling-ns (requiring-resolve 'hive-mcp.agent.ling/->ling)
        drone-ns (requiring-resolve 'hive-mcp.agent.drone/->drone)
        slaves (ds-queries/get-all-slaves)
        agents-created (for [slave slaves]
                         (let [id (:slave/id slave)
                               depth (:slave/depth slave)
                               opts {:cwd (:slave/cwd slave)
                                     :project-id (:slave/project-id slave)
                                     :presets (:slave/presets slave)}]
                           (if (= 1 depth)
                             (ling-ns id opts)
                             (drone-ns id opts))))]
    (doseq [agent agents-created]
      (register-agent! agent))
    (log/info "Synced agent registry from DataScript:" (count agents-created) "agents")
    {:synced-count (count agents-created)
     :agents (vec agents-created)}))

(defn clear-agents!
  "Clear all agents from the registry."
  []
  (reset! agents {})
  (log/info "Cleared agent registry"))
