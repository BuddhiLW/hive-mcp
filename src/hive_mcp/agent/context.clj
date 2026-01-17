(ns hive-mcp.agent.context
  "Agent execution context - provides thread-local state for tool execution.

   CRITICAL: This namespace MUST have minimal dependencies to avoid cycles.
   It exists specifically to break the executor<->hivemind cycle.

   Usage:
     ;; In tool handlers that need agent context:
     (require '[hive-mcp.agent.context :as ctx])
     (let [agent-id (ctx/current-agent-id)]
       ...)")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *current-agent-id*
  "Dynamic var holding the current agent-id during tool execution.

   Used by tools like hivemind_shout to identify the calling agent
   when the agent doesn't explicitly pass agent_id.

   CRITICAL: This fixes the 'unknown-agent' attribution bug where
   drones calling hivemind_shout would show up as 'unknown-agent'
   because the CLAUDE_SWARM_SLAVE_ID env var isn't set in the
   MCP server JVM.

   Bound by executor/execute-tool-calls before executing any tools."
  nil)

(defn current-agent-id
  "Get the current agent-id from execution context.

   Returns the agent-id bound during tool execution, or nil if
   called outside of a tool execution context.

   Fallback chain for tool handlers:
     1. Explicit agent_id parameter from caller
     2. (ctx/current-agent-id) - executor context
     3. (System/getenv \"CLAUDE_SWARM_SLAVE_ID\") - env var
     4. \"unknown-agent\" - last resort"
  []
  *current-agent-id*)
