(ns hive-mcp.tools.agent.lifecycle
  "Agent lifecycle handlers: interrupt, cleanup, claims, collect, broadcast."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.registry :as registry]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.tools.swarm.collect :as swarm-collect]
            [hive-mcp.tools.swarm.status :as swarm-status]
            [taoensso.timbre :as log]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-interrupt
  "Interrupt the current query/task of a running agent-sdk ling."
  [{:keys [agent_id]}]
  (if (str/blank? agent_id)
    (mcp-error "agent_id is required for interrupt")
    (let [result (ling/interrupt-ling! agent_id)]
      (if (:success? result)
        (mcp-json result)
        (mcp-error (str "Interrupt failed: " (str/join ", " (:errors result))))))))

(defn handle-cleanup
  "Reconcile DataScript registry with actual Emacs state, removing orphan agents."
  [_params]
  (try
    (let [ds-agents (queries/get-all-slaves)
          elisp-lings (or (helpers/query-elisp-lings) [])
          elisp-ids (set (map :slave/id elisp-lings))
          ;; Only check lings (depth=1), drones are JVM-side
          orphan-lings (->> ds-agents
                            (filter #(= 1 (:slave/depth %)))
                            (filter #(not (elisp-ids (:slave/id %))))
                            (map :slave/id))
          removed (doall
                   (for [slave-id orphan-lings]
                     (do
                       (log/info "Removing orphan ling from DataScript" {:slave-id slave-id})
                       (registry/remove-slave! slave-id)
                       slave-id)))]
      (log/info "Cleanup completed" {:orphans-removed (count removed)
                                     :ds-total (count ds-agents)
                                     :elisp-lings (count elisp-lings)})
      (mcp-json {:success true
                 :orphans-removed (count removed)
                 :removed-ids (vec removed)
                 :ds-agents-before (count ds-agents)
                 :elisp-lings-found (count elisp-lings)}))
    (catch Exception e
      (log/error "Cleanup failed" {:error (ex-message e)})
      (mcp-error (str "Cleanup failed: " (ex-message e))))))

(defn handle-claims
  "Get file ownership claims for an agent or all agents."
  [{:keys [agent_id]}]
  (try
    (if agent_id
      (let [logic-claims (logic/get-all-claims)
            agent-claims (->> logic-claims
                              (filter #(= agent_id (:slave-id %)))
                              (mapv (fn [{:keys [file slave-id]}]
                                      {:file file :owner slave-id})))]
        (mcp-json {:agent-id agent_id
                   :claims agent-claims
                   :count (count agent-claims)}))
      (let [all-claims (logic/get-all-claims)
            formatted (->> all-claims
                           (mapv (fn [{:keys [file slave-id]}]
                                   {:file file :owner slave-id})))]
        (mcp-json {:claims formatted
                   :count (count formatted)
                   :by-owner (frequencies (map :owner formatted))})))
    (catch Exception e
      (log/error "Failed to get claims" {:error (ex-message e)})
      (mcp-error (str "Failed to get claims: " (ex-message e))))))

(defn handle-collect
  "Collect response from a dispatched task."
  [{:keys [task_id] :as params}]
  (if (empty? task_id)
    (mcp-error "task_id is required")
    (swarm-collect/handle-swarm-collect params)))

(defn handle-broadcast
  "Broadcast a prompt to all active lings."
  [{:keys [prompt] :as params}]
  (if (empty? prompt)
    (mcp-error "prompt is required")
    (swarm-status/handle-swarm-broadcast params)))
