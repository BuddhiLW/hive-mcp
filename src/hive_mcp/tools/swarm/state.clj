(ns hive-mcp.tools.swarm.state
  "Hivemind state integration for swarm status, mapping DataScript slave status to working status."
  (:require [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.swarm.datascript :as ds]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn get-slave-working-status
  "Get the working status of a slave from DataScript."
  [agent-id]
  (when-let [slave (ds/get-slave agent-id)]
    (let [status (:slave/status slave)]
      (case status
        :working "working"
        :idle "idle"
        :error "idle"
        :blocked "blocked"
        (:started :progress) "working"
        :completed "idle"
        "idle"))))

(defn get-unified-swarm-status
  "Get unified swarm status merging hivemind state with lings registry."
  []
  (let [lings (registry/get-available-lings)]
    (into {}
          (map (fn [[slave-id info]]
                 (let [working-status (get-slave-working-status slave-id)]
                   [slave-id (assoc info :working-status (or working-status "idle"))]))
               lings))))

(defn merge-hivemind-into-slaves
  "Merge hivemind working status into slaves-detail list."
  [slaves-detail]
  (when (seq slaves-detail)
    (mapv (fn [slave]
            (if-let [slave-id (:slave-id slave)]
              (if-let [hivemind-status (get-slave-working-status slave-id)]
                (assoc slave :status (keyword hivemind-status))
                slave)
              slave))
          slaves-detail)))
