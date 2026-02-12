(ns hive-mcp.tools.agent.dag
  "DAG scheduler subcommand handlers (start, stop, status)."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-dag-start
  "Start the DAG scheduler for a plan."
  [{:keys [plan_id cwd max_slots presets project_id]}]
  (cond
    (str/blank? plan_id)
    (mcp-error "plan_id is required for dag start")

    (str/blank? cwd)
    (mcp-error "cwd is required for dag start")

    :else
    (try
      (let [start! (requiring-resolve 'hive-mcp.scheduler.dag-waves/start-dag!)
            opts (cond-> {:cwd cwd}
                   max_slots  (assoc :max-slots (if (string? max_slots)
                                                  (parse-long max_slots)
                                                  max_slots))
                   presets    (assoc :presets (if (string? presets)
                                                [presets]
                                                (vec presets)))
                   project_id (assoc :project-id project_id))
            result (start! plan_id opts)]
        (mcp-json result))
      (catch Exception e
        (mcp-error (str "Failed to start DAG: " (ex-message e)))))))

(defn handle-dag-stop
  "Stop the DAG scheduler."
  [_params]
  (try
    (let [stop! (requiring-resolve 'hive-mcp.scheduler.dag-waves/stop-dag!)
          result (stop!)]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Failed to stop DAG: " (ex-message e))))))

(defn handle-dag-status
  "Get current DAG scheduler status."
  [_params]
  (try
    (let [status-fn (requiring-resolve 'hive-mcp.scheduler.dag-waves/dag-status)
          result (status-fn)]
      (mcp-json result))
    (catch Exception e
      (mcp-error (str "Failed to get DAG status: " (ex-message e))))))
