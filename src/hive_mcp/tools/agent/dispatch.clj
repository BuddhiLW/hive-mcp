(ns hive-mcp.tools.agent.dispatch
  "Agent task dispatch handler with context abstraction support."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- get-delegate-fn
  "Lazily resolve delegate-agentic-drone! to avoid circular dep."
  []
  (require 'hive-mcp.agent.core)
  (resolve 'hive-mcp.agent.core/delegate-agentic-drone!))

(defn- build-dispatch-context
  "Build an IDispatchContext from dispatch parameters."
  [prompt ctx_refs kg_node_ids scope]
  (if (seq ctx_refs)
    (let [refs-map (reduce-kv (fn [m k v] (assoc m (keyword k) v)) {} ctx_refs)]
      (dispatch-ctx/->ref-context prompt
                                  {:ctx-refs    refs-map
                                   :kg-node-ids (vec (or kg_node_ids []))
                                   :scope       scope}))
    (dispatch-ctx/ensure-context prompt)))

(defn handle-dispatch
  "Dispatch a task to an agent."
  [{:keys [agent_id prompt files priority ctx_refs kg_node_ids scope]}]
  (cond
    (empty? agent_id)
    (mcp-error "agent_id is required")

    (empty? prompt)
    (mcp-error "prompt is required")

    :else
    (try
      (if-let [agent-data (queries/get-slave agent_id)]
        (let [agent-type (if (= 1 (:slave/depth agent-data)) :ling :drone)
              agent (case agent-type
                      :ling (ling/->ling agent_id {:cwd (:slave/cwd agent-data)
                                                   :presets (:slave/presets agent-data)
                                                   :project-id (:slave/project-id agent-data)
                                                   :spawn-mode (or (:ling/spawn-mode agent-data) :vterm)})
                      :drone (drone/->drone agent_id {:cwd (:slave/cwd agent-data)
                                                      :parent-id (:slave/parent agent-data)
                                                      :project-id (:slave/project-id agent-data)}))
              ctx (build-dispatch-context prompt ctx_refs kg_node_ids
                                          (or scope (:slave/project-id agent-data)))
              resolved-prompt (:prompt (dispatch-ctx/resolve-context ctx))
              task-opts (cond-> {:task resolved-prompt
                                 :dispatch-context ctx
                                 :files files
                                 :priority (keyword (or priority "normal"))}
                          (= agent-type :drone)
                          (assoc :delegate-fn (get-delegate-fn)))
              task-id (proto/dispatch! agent task-opts)]
          (log/info "Dispatched task to agent" {:agent_id agent_id
                                                :task-id task-id
                                                :context-type (dispatch-ctx/context-type ctx)})
          (mcp-json {:success true
                     :agent-id agent_id
                     :task-id task-id
                     :context-type (name (dispatch-ctx/context-type ctx))
                     :files files}))
        (mcp-error (str "Agent not found: " agent_id)))
      (catch Exception e
        (log/error "Failed to dispatch to agent" {:agent_id agent_id :error (ex-message e)})
        (mcp-error (str "Failed to dispatch: " (ex-message e)))))))
