(ns hive-mcp.hivemind.tools
  "Hivemind MCP tool definitions.

   Defines the tool schemas and inline handlers for:
   - hivemind_shout: Broadcast status/progress
   - hivemind_ask: Request human decision
   - hivemind_status: Get coordinator state
   - hivemind_respond: Answer a pending ask
   - hivemind_messages: Get agent message history

   SOLID: SRP — tool definitions and handler wiring only.
   CLARITY: L — Thin adapter layer delegating to messaging/status modules."
  (:require [hive-mcp.hivemind.state :as state]
            [hive-mcp.hivemind.messaging :as messaging]
            [hive-mcp.hivemind.status :as status]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.swarm.protocol :as proto]
            [hive-mcp.swarm.datascript.registry :as registry]
            [hive-mcp.tools.memory.scope :as mem-scope]
            [clojure.data.json :as json]
            [clojure.set :as set]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; MCP Tool Definitions
;; =============================================================================

(def tools
  [{:name "hivemind_shout"
    :description "Broadcast status/progress to the hivemind coordinator.

USE THIS to report:
- Task progress: (hivemind_shout :progress {:task \"..\" :percent 50})
- Completion: (hivemind_shout :completed {:task \"..\" :result \"..\"})
- Errors: (hivemind_shout :error {:task \"..\" :error \"..\"})
- Blocked: (hivemind_shout :blocked {:task \"..\" :reason \"need input\"})

The coordinator sees all shouts in real-time.

IMPORTANT: Lings MUST pass agent_id explicitly (use your $CLAUDE_SWARM_SLAVE_ID).
The env var fallback reads from MCP server process, NOT your ling process!"
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "REQUIRED for lings: Pass your $CLAUDE_SWARM_SLAVE_ID. Without this, status sync will fail."}
                               "event_type" {:type "string"
                                             :enum ["progress" "completed" "error" "blocked" "started"]
                                             :description "Type of event"}
                               "task" {:type "string"
                                       :description "Current task description"}
                               "message" {:type "string"
                                          :description "Status message"}
                               "directory" {:type "string"
                                            :description "Working directory for project-id derivation. Pass your cwd to scope shouts to your project."}
                               "data" {:type "object"
                                       :description "Additional event data"}}
                  :required ["event_type"]}
    :handler (fn [{:keys [agent_id event_type task message directory data]}]
               ;; P1 FIX: Check ctx/current-agent-id for drone context
               ;; Fallback chain: explicit param -> context binding -> env var -> unknown
               ;; WARNING: System/getenv reads MCP SERVER's env, NOT the calling ling's env!
               ;; Lings MUST pass agent_id explicitly for status sync to work.
               (let [ctx-agent (ctx/current-agent-id)
                     env-agent (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                     effective-id (or agent_id
                                      ctx-agent
                                      env-agent
                                      "unknown-agent")
                     ;; Warn when falling back to env or unknown - helps diagnose status sync issues
                     _ (when (and (not agent_id) (not ctx-agent))
                         (log/warn "[hivemind_shout] No agent_id in args or context. Using fallback:"
                                   (if env-agent
                                     (str "env CLAUDE_SWARM_SLAVE_ID=" env-agent " (MCP server's env, NOT ling's!)")
                                     "unknown-agent")))
                     fallback-used? (and (not agent_id) (not ctx-agent))
                     effective-dir (or directory
                                       (ctx/current-directory))]
                 (messaging/shout! effective-id (keyword event_type)
                                   (merge {:task task :message message :directory effective-dir} data))
                 {:type "text" :text (json/write-str (cond-> {:success true
                                                              :agent_id effective-id}
                                                       fallback-used?
                                                       (assoc :warning (str "agent_id not provided - using fallback: " effective-id
                                                                            ". Lings should always pass agent_id explicitly for status sync."))))}))}

   {:name "hivemind_ask"
    :description "Request a decision from the human coordinator.

USE THIS when you need human approval or guidance:
- Before destructive operations
- When multiple valid approaches exist
- When requirements are ambiguous

BLOCKS until human responds (up to timeout).

Example: hivemind_ask('Should I delete these 50 files?', ['yes', 'no', 'show me first'])

IMPORTANT: Lings MUST pass agent_id explicitly (use your $CLAUDE_SWARM_SLAVE_ID).
The env var fallback reads from MCP server process, NOT your ling process!"
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "REQUIRED for lings: Pass your $CLAUDE_SWARM_SLAVE_ID. Without this, coordination may fail."}
                               "question" {:type "string"
                                           :description "What decision do you need?"}
                               "options" {:type "array"
                                          :items {:type "string"}
                                          :description "Available options (or omit for free-form)"}
                               "timeout_ms" {:type "integer"
                                             :description "Timeout in ms (default 300000 = 5 min)"}
                               "directory" {:type "string"
                                            :description "Working directory for project-id derivation. Pass your cwd for proper scoping."}}
                  :required ["question"]}
    :handler (fn [{:keys [agent_id question options timeout_ms directory]}]
               ;; P1 FIX: Check ctx/current-* for drone context
               ;; Fallback chain: explicit param -> context binding -> env var -> unknown
               (let [effective-id (or agent_id
                                      (ctx/current-agent-id)
                                      (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                                      "unknown-agent")
                     effective-dir (or directory
                                       (ctx/current-directory))
                     result (messaging/ask! effective-id question options
                                            :timeout-ms (or timeout_ms 300000))]
                 {:type "text"
                  :text (json/write-str
                         (if (:timeout result)
                           {:timeout true :message "No response within timeout"}
                           {:decision (:decision result)
                            :by (:by result)
                            :directory effective-dir}))}))}

   {:name "hivemind_status"
    :description "Get current hivemind coordinator status.

Returns:
- Active agents and their status
- Pending questions awaiting human decision
- Channel connection status

When directory is provided, filters to only show agents belonging to that project.
This prevents cross-project pollution in multi-project hivemind sessions."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description "Working directory to scope results to a specific project. Pass your cwd to see only agents from your project."}}
                  :required []}
    :handler (fn [{:keys [directory]}]
               (let [effective-dir (or directory (ctx/current-directory))
                     project-id (when effective-dir (mem-scope/get-current-project-id effective-dir))]
                 {:type "text"
                  :text (json/write-str (status/get-status project-id))}))}

   {:name "hivemind_respond"
    :description "Respond to a pending ask from an agent.

Used by the coordinator to answer agent questions."
    :inputSchema {:type "object"
                  :properties {"ask_id" {:type "string"
                                         :description "ID of the ask to respond to"}
                               "decision" {:type "string"
                                           :description "The decision/response"}}
                  :required ["ask_id" "decision"]}
    :handler (fn [{:keys [ask_id decision]}]
               {:type "text"
                :text (json/write-str
                       (if (messaging/respond-ask! ask_id decision)
                         {:success true}
                         {:error "No pending ask with that ID"}))})}

   {:name "hivemind_messages"
    :description "Get recent messages from a specific agent.

Returns up to 10 recent shout messages with their payloads.
Use this to retrieve message content that agents have broadcast.

When directory is provided, the available-agents list is filtered to that project.
The specific agent lookup is still allowed even if agent is from another project."
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "Agent identifier to get messages from"}
                               "directory" {:type "string"
                                            :description "Working directory to scope available-agents list to a specific project. Pass your cwd to see only agents from your project."}}
                  :required ["agent_id"]}
    :handler (fn [args]
               ;; Support both snake_case and kebab-case keys:
               ;; - :agent_id (snake_case) - if MCP passes keywords unchanged
               ;; - :agent-id (kebab-case) - jsonrpc4clj converts "agent_id" -> :agent-id
               ;; Also support string keys as fallback
               (let [agent_id (or (:agent_id args)
                                  (:agent-id args)
                                  (get args "agent_id")
                                  (get args "agent-id"))
                     ;; Use ctx/current-directory as fallback for directory
                     effective-dir (or (:directory args)
                                       (get args "directory")
                                       (ctx/current-directory))
                     ;; Derive project-id for filtering available-agents list
                     project-id (when effective-dir (mem-scope/get-current-project-id effective-dir))
                     ;; ADR-002 AMENDED: Available agents = union of DataScript + message history
                     ;; When project-id provided, filter DataScript agents to that project
                     ds-agents (if project-id
                                 (clojure.core/set (map :slave/id (proto/get-slaves-by-project registry/default-registry project-id)))
                                 (clojure.core/set (map :slave/id (proto/get-all-slaves registry/default-registry))))
                     ;; Message history orphans - filter by checking if their messages have matching project-id
                     msg-agents (if project-id
                                  (->> @state/agent-registry
                                       (filter (fn [[_id {:keys [messages]}]]
                                                 (some #(= project-id (:project-id %)) messages)))
                                       (map first)
                                       set)
                                  (clojure.core/set (keys @state/agent-registry)))
                     available-agents (vec (set/union ds-agents msg-agents))]
                 {:type "text"
                  :text (json/write-str
                         (if-let [messages (status/get-agent-messages agent_id)]
                           {:agent_id agent_id
                            :messages messages
                            :project-filter project-id}
                           {:error (str "Agent not found: " agent_id)
                            :available-agents available-agents
                            :project-filter project-id}))}))}])
;; REMOVED: hivemind_listen - polling anti-pattern, use event-driven get-status instead

(defn register-tools!
  "Register hivemind tools with the MCP server."
  [register-fn]
  (doseq [tool tools]
    (register-fn tool)))
