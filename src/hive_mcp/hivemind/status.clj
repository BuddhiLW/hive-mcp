(ns hive-mcp.hivemind.status
  "Hivemind status queries and agent lifecycle management.

   Provides functions for querying the hivemind state (agent status,
   pending asks, swarm prompts) and managing agent registration/teardown.

   ADR-002 AMENDED: DataScript is the source of truth for slave data.
   Message history comes from agent-registry atom (hivemind-specific).

   SOLID: SRP — query and lifecycle logic only.
   CLARITY: L — Read-heavy module, writes limited to registration/teardown."
  (:require [hive-mcp.hivemind.state :as state]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.channel.websocket :as ws]
            [hive-mcp.swarm.protocol :as proto]
            [hive-mcp.swarm.datascript.registry :as registry]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.tools.memory.scope :as mem-scope]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Status Queries
;; =============================================================================

(defn- build-agents-map
  "Build agents map from DataScript (source of truth) merged with message history.

   ADR-002 AMENDED: DataScript is the source of truth for slave data.
   Message history comes from agent-registry atom (hivemind-specific).

   Arguments:
     project-id - Optional project ID to filter agents by. When nil, returns all agents.

   Returns map of agent-id -> {:status :name :presets :cwd :messages :project-id ...}"
  ([]
   (build-agents-map nil))
  ([project-id]
   (let [;; Get slaves from DataScript (source of truth)
         ;; Filter by project-id if provided
         ds-slaves (if project-id
                     (proto/get-slaves-by-project registry/default-registry project-id)
                     (proto/get-all-slaves registry/default-registry))
         ;; Get message history from local atom
         msg-history @state/agent-registry]
     ;; Build merged map: DataScript data + messages
     (reduce
      (fn [acc slave]
        (let [slave-id (:slave/id slave)
              messages (get-in msg-history [slave-id :messages] [])
              last-seen (get-in msg-history [slave-id :last-seen])]
          (assoc acc slave-id
                 {:status (:slave/status slave)
                  :name (:slave/name slave)
                  :presets (vec (:slave/presets slave))
                  :cwd (:slave/cwd slave)
                  :project-id (:slave/project-id slave)
                  :current-task (:slave/current-task slave)
                  :messages messages
                  :last-seen last-seen})))
      {}
      ds-slaves))))

(defn get-status
  "Get current hivemind status.

   ADR-002 AMENDED: :agents now comes from DataScript (source of truth)
   merged with message history from agent-registry atom.

   Arguments:
     project-id - Optional project ID to filter agents by. When nil, returns all agents.

   Returns map with:
   - :agents - map of agent-id -> status (from DataScript + message history)
   - :pending-asks - list of unanswered questions (agent-initiated)
   - :pending-swarm-prompts - list of permission prompts from slaves (push notifications)
   - :channel-connected - whether Emacs is connected (legacy bencode channel)
   - :ws-connected - whether WebSocket channel is connected (preferred)
   - :project-id - the project filter applied (nil if showing all)"
  ([]
   (get-status nil))
  ([project-id]
   {:agents (build-agents-map project-id)
    :project-id project-id
    :pending-asks (mapv (fn [[id {:keys [question options agent-id]}]]
                          {:ask-id id
                           :agent-id agent-id
                           :question question
                           :options options})
                        @state/pending-asks)
    :pending-swarm-prompts (mapv (fn [[slave-id {:keys [prompt timestamp session-id received-at]}]]
                                   {:slave-id slave-id
                                    :prompt prompt
                                    :timestamp timestamp
                                    :session-id session-id
                                    :received-at received-at})
                                 @state/pending-swarm-prompts)
    :channel-connected (channel/server-connected?)
    :ws-connected (ws/connected?)
    :ws-clients (ws/client-count)}))

(defn get-agent-messages
  "Get recent messages from a specific agent.

   ADR-002 AMENDED: Agent existence is determined by DataScript OR message history.
   Messages are stored in agent-registry atom (hivemind-specific).

   Returns vector of messages (up to 10), empty vector if agent exists but
   hasn't shouted, or nil if agent not found in either source."
  [agent-id]
  (let [msg-history-entry (get @state/agent-registry agent-id)
        ds-slave (proto/get-slave registry/default-registry agent-id)]
    (cond
      ;; Has messages in history
      msg-history-entry (:messages msg-history-entry)
      ;; Exists in DataScript but no messages yet
      ds-slave []
      ;; Not found anywhere
      :else nil)))

;; =============================================================================
;; Agent Lifecycle
;; =============================================================================

(defn register-agent!
  "Register an agent in the hivemind registry without requiring a shout.

   Used when agents are spawned - they should be immediately visible
   in available-agents for hivemind_messages even before they shout.

   ADR-002 AMENDED: Now also ensures agent exists in DataScript (source of truth).
   If agent already in DataScript, just initializes message history.
   If not in DataScript, adds it there too (backward compatibility).

   agent-id: Unique identifier (typically slave-id from swarm)
   metadata: Map with optional :name, :presets, :cwd

   SOLID: SRP - Single responsibility for registration
   CLARITY: I - Inputs are guarded (handles missing metadata gracefully)"
  [agent-id metadata]
  (let [now (System/currentTimeMillis)]
    ;; Initialize message history (local atom)
    (swap! state/agent-registry assoc agent-id
           {:messages []
            :last-seen now})
    ;; Ensure agent exists in DataScript (source of truth)
    ;; If not already there, add it for backward compatibility
    ;; Note: Use :idle status as :spawned is not in DataScript's valid statuses
    (when-not (proto/get-slave registry/default-registry agent-id)
      (let [cwd (:cwd metadata)
            project-id (when cwd (mem-scope/get-current-project-id cwd))]
        (proto/add-slave! registry/default-registry agent-id
                          {:name (or (:name metadata) agent-id)
                           :status :idle
                           :depth 1
                           :presets (:presets metadata)
                           :cwd cwd
                           :project-id project-id})))
    (log/info "Agent registered in hivemind:" agent-id)
    true))

(defn clear-agent!
  "Remove an agent from the registry (when it terminates).

   ADR-002 AMENDED: Clears from both:
   - agent-registry atom (message history)
   - DataScript (source of truth for slave data)
   - core.logic claims (file locks for conflict detection)

   GHOST CLAIMS FIX: Previously only DataScript claims were released,
   leaving 'ghost claims' in core.logic that blocked other drones.
   Now releases claims from BOTH storage systems."
  [agent-id]
  ;; Clear message history
  (swap! state/agent-registry dissoc agent-id)
  ;; Release core.logic claims FIRST (prevents ghost claims)
  ;; This is critical - these claims are used for conflict detection
  (logic/release-claims-for-slave! agent-id)
  ;; Clear from DataScript (also releases DataScript claims via lings/remove-slave!)
  (proto/remove-slave! registry/default-registry agent-id)
  (log/info "Agent cleared from hivemind:" agent-id))
