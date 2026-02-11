(ns hive-mcp.tools.agent.status
  "Agent status query handler with DataScript and elisp fallback."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.swarm.datascript.queries :as queries]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-status
  "Get agent status, optionally filtered by agent_id, type, or project_id."
  [{:keys [agent_id type project_id]}]
  (let [eid (when (and agent_id (not= agent_id "coordinator")) agent_id)]
    (try
      (cond
        eid
        (if-let [agent-data (queries/get-slave eid)]
          (mcp-json {:agent (helpers/format-agent agent-data)})
          (mcp-error (str "Agent not found: " eid)))
        type
        (let [agent-type (keyword type)
              depth (case agent-type :ling 1 :drone 2 nil)
              all-agents (if project_id
                           (queries/get-slaves-by-project project_id)
                           (if (= agent-type :ling)
                             (helpers/merge-with-elisp-lings (queries/get-all-slaves))
                             (queries/get-all-slaves)))
              filtered (if depth
                         (filter #(= depth (:slave/depth %)) all-agents)
                         all-agents)]
          (mcp-json (helpers/format-agents filtered)))
        project_id
        (mcp-json (helpers/format-agents (queries/get-slaves-by-project project_id)))
        :else
        (mcp-json (helpers/format-agents (helpers/merge-with-elisp-lings (queries/get-all-slaves)))))
      (catch Exception e
        (log/error "Failed to get agent status" {:error (ex-message e)})
        (mcp-error (str "Failed to get status: " (ex-message e)))))))
