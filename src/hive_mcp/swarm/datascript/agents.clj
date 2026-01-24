(ns hive-mcp.swarm.datascript.agents
  "Agent context lookup functions for swarm coordination.

   Provides auto-context resolution from agent-id, enabling tools
   to derive project-id without explicit directory parameter.

   SOLID-S: Single Responsibility - agent context resolution only.
   CLARITY: R - Represented intent with clear lookup semantics."
  (:require [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.tools.memory.scope :as scope]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn get-project-for-agent
  "Look up project-id for an agent from DataScript.

   Resolution order:
     1. Return :slave/project-id if stored on the slave entity
     2. Derive project-id from :slave/cwd using Emacs project detection
     3. Return 'global' if agent not found or no cwd available

   Arguments:
     agent-id - Slave/agent identifier (e.g., 'ling-auth-123')

   Returns:
     Project-id string (e.g., 'hive-mcp', 'funeraria:sisf-sync', or 'global')

   Usage:
     ;; Auto-context in hivemind_shout when directory not passed
     (let [project-id (get-project-for-agent agent-id)]
       (dispatch-event project-id ...))

   Note: This function may call Emacs to derive project-id from directory,
   so it has I/O side effects. Cache results if called frequently."
  [agent-id]
  (when-not (or (nil? agent-id) (empty? agent-id))
    (if-let [slave (queries/get-slave agent-id)]
      (let [stored-project-id (:slave/project-id slave)
            cwd (:slave/cwd slave)]
        (cond
          ;; Use stored project-id if available
          (and stored-project-id (not (empty? stored-project-id)))
          (do
            (log/debug "get-project-for-agent:" agent-id "-> stored project-id:" stored-project-id)
            stored-project-id)

          ;; Derive from cwd if available
          cwd
          (let [derived-id (scope/get-current-project-id cwd)]
            (log/debug "get-project-for-agent:" agent-id "-> derived from cwd:" cwd "=>" derived-id)
            derived-id)

          ;; No project context available
          :else
          (do
            (log/debug "get-project-for-agent:" agent-id "-> no project context, using 'global'")
            "global")))
      ;; Agent not found in DataScript
      (do
        (log/debug "get-project-for-agent:" agent-id "-> agent not found in DataScript")
        nil))))

(defn get-cwd-for-agent
  "Look up working directory for an agent from DataScript.

   Arguments:
     agent-id - Slave/agent identifier

   Returns:
     Working directory string or nil if not found"
  [agent-id]
  (when-not (or (nil? agent-id) (empty? agent-id))
    (when-let [slave (queries/get-slave agent-id)]
      (:slave/cwd slave))))

(defn get-agent-context
  "Get full agent context from DataScript.

   Arguments:
     agent-id - Slave/agent identifier

   Returns:
     Map with:
       :agent-id   - The agent identifier
       :project-id - Project ID (stored or derived)
       :cwd        - Working directory
       :presets    - Applied presets
       :depth      - Hierarchy depth (1=ling, 2=drone)
       :status     - Current status
     Or nil if agent not found"
  [agent-id]
  (when-not (or (nil? agent-id) (empty? agent-id))
    (when-let [slave (queries/get-slave agent-id)]
      (let [project-id (get-project-for-agent agent-id)]
        {:agent-id   agent-id
         :project-id project-id
         :cwd        (:slave/cwd slave)
         :presets    (:slave/presets slave)
         :depth      (:slave/depth slave)
         :status     (:slave/status slave)}))))
