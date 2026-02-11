(ns hive-mcp.tools.catchup.permeation
  "Auto-permeation of ling wraps during catchup."
  (:require [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.swarm.datascript :as ds]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn auto-permeate-wraps
  "Automatically permeate pending ling wraps during catchup."
  [directory]
  (try
    (let [project-id (scope/get-current-project-id directory)
          ;; Include children for hierarchical projects
          queue-items (ds/get-unprocessed-wraps-for-hierarchy project-id)
          processed-count (count queue-items)
          agent-ids (mapv :wrap-queue/agent-id queue-items)]
      ;; Mark each as processed
      (doseq [item queue-items]
        (ds/mark-wrap-processed! (:wrap-queue/id item)))
      (when (pos? processed-count)
        (log/info "catchup auto-permeated" processed-count "ling wraps from agents:" agent-ids))
      {:permeated processed-count
       :agents agent-ids})
    (catch Exception e
      (log/warn "auto-permeate failed (non-fatal):" (.getMessage e))
      {:permeated 0 :agents [] :error (.getMessage e)})))
