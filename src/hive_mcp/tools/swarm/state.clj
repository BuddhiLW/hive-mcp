(ns hive-mcp.tools.swarm.state
  "Hivemind state integration for swarm status.

   Maps hivemind event types to swarm working status.
   Provides unified view merging hivemind state with lings registry.

   SOLID: SRP - Single responsibility for state mapping/merging.
   CLARITY: R - Represented intent with clear status mapping."
  (:require [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.hivemind :as hivemind]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; ============================================================
;; Hivemind Status Mapping
;; ============================================================

(defn get-slave-working-status
  "Get the working status of a slave based on hivemind events.

   Maps hivemind event types to swarm working status:
   - :started, :progress -> \"working\"
   - :completed, :error -> \"idle\"
   - :blocked -> \"blocked\"
   - nil (not registered) -> nil

   agent-id: The slave/agent ID to query

   CLARITY: R - Clear mapping from hivemind to swarm status"
  [agent-id]
  (when-let [agent (get @hivemind/agent-registry agent-id)]
    (let [status (:status agent)]
      (case status
        (:started :progress) "working"
        :completed "idle"
        :error "idle"
        :blocked "blocked"
        ;; Default to idle for unknown states
        "idle"))))

;; ============================================================
;; Unified Status
;; ============================================================

(defn get-unified-swarm-status
  "Get unified swarm status merging hivemind state with lings registry.

   Returns a map of slave-id -> {:name, :presets, :cwd, :working-status, ...}
   The :working-status field reflects the current state from hivemind events.

   SOLID: DIP - Depends on abstractions (registry/hivemind), not implementations."
  []
  (let [lings (registry/get-available-lings)]
    (into {}
          (map (fn [[slave-id info]]
                 (let [working-status (get-slave-working-status slave-id)]
                   [slave-id (assoc info :working-status (or working-status "idle"))]))
               lings))))

(defn merge-hivemind-into-slaves
  "Merge hivemind working status into slaves-detail list.

   Takes elisp slaves-detail vector and enriches with hivemind status.
   Returns updated vector preserving all entries.

   CLARITY: R - Clear transformation of slaves data"
  [slaves-detail]
  (when (seq slaves-detail)
    (mapv (fn [slave]
            (if-let [slave-id (:slave-id slave)]
              (if-let [hivemind-status (get-slave-working-status slave-id)]
                (assoc slave :status (keyword hivemind-status))
                slave)
              slave))
          slaves-detail)))
