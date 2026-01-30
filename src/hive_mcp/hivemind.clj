(ns hive-mcp.hivemind
  "Hivemind coordination tools for swarm agents.

   Provides event-driven communication between agents and the coordinator,
   replacing file-based polling with instant push notifications.

   Tools:
   - hivemind_shout: Broadcast status/progress to coordinator
   - hivemind_ask: Request human decision (blocks until response)
   - hivemind_status: Get current hivemind state (includes pending prompts)
   - hivemind_messages: Get recent messages from specific agent

   Architecture: Pure push via channel.clj, accumulated state via atoms.
   Coordinator queries get-status when ready - no polling loops needed.

   Project Scoping:
   - hivemind_status and hivemind_messages accept optional directory param
   - When directory provided, results are filtered to that project's agents only
   - Prevents cross-project pollution in multi-project hivemind sessions
   - Uses hive-mcp.tools.memory.scope/get-current-project-id for resolution"
  (:require [hive-mcp.channel :as channel]
            [hive-mcp.channel.websocket :as ws]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.swarm.protocol :as proto]
            [hive-mcp.swarm.datascript.registry :as registry]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.guards :as guards]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.tools.memory.scope :as mem-scope]
            [clojure.core.async :as async :refer [>!! chan timeout alt!!]]
            [clojure.data.json :as json]
            [clojure.set :as set]
            [hive-mcp.events.core :as events]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State

(defonce ^{:doc "Map of ask-id -> {:question ... :response-chan ...}"}
  pending-asks
  (atom {}))

(defonce ^{:doc "LEGACY: Message history storage. DataScript is source of truth for slave data.

                 ADR-002 AMENDED: This atom stores ONLY message history (ring buffer).
                 Slave metadata (status, name, presets, cwd) comes from DataScript.

                 Map of agent-id -> {:messages [...] :last-seen timestamp}
                 Messages is a vector of recent shouts (max 10 per agent).

                 Migration note: This was previously the authoritative agent registry.
                 Now DataScript (swarm/datascript.clj) is the source of truth for slaves.
                 This atom persists messages which are hivemind-specific (not swarm state)."}
  agent-registry
  (atom {}))

;; =============================================================================
;; Piggyback Message Source Registration (DIP)
;; =============================================================================

(defn- all-hivemind-messages
  "Provide all hivemind messages to piggyback module.
   Returns flat seq of {:agent-id :event-type :message :timestamp :project-id}.

   Note: project-id is included for per-project cursor scoping.
   Messages without project-id default to 'global'.

   BUG FIX: Shouts may have :task but no :message (or vice versa).
   Fall back to :task when :message is nil to prevent {:m nil} in piggyback."
  []
  (mapcat (fn [[agent-id {:keys [messages]}]]
            (for [{:keys [event-type message task timestamp project-id]} messages]
              {:agent-id agent-id
               :event-type event-type
               :message (or message task)
               :timestamp timestamp
               :project-id (or project-id "global")}))
          @agent-registry))

;; Register this namespace as the message source for piggyback (DIP)
(piggyback/register-message-source! all-hivemind-messages)

;; =============================================================================

(defonce ^{:doc "Map of slave-id -> {:prompt :timestamp :session-id}
                 Permission prompts pushed from Emacs swarm slaves.
                 These are distinct from asks (agent-initiated questions)."}
  pending-swarm-prompts
  (atom {}))

;; Ling result tracking for coordinator review
(defonce ling-results (atom {}))

(defn record-ling-result!
  "Record ling completion for coordinator review."
  [agent-id result]
  (swap! ling-results assoc agent-id
         {:result result
          :timestamp (System/currentTimeMillis)
          :reviewed? false}))

(defn get-pending-ling-results
  "Get ling results awaiting coordinator review."
  []
  (->> @ling-results
       (filter (fn [[_ v]] (not (:reviewed? v))))
       (into {})))

(defn mark-ling-reviewed!
  "Mark a ling result as reviewed by coordinator."
  [agent-id]
  (swap! ling-results assoc-in [agent-id :reviewed?] true))

(defn clear-ling-results!
  "Clear all ling results (e.g., at session end)."
  []
  (reset! ling-results {}))

(defn add-swarm-prompt!
  "Add a permission prompt from a swarm slave.
   Called by sync.clj when :prompt-shown event received from Emacs."
  [slave-id prompt-text session-id timestamp]
  (let [prompt-data {:prompt prompt-text
                     :timestamp timestamp
                     :session-id session-id
                     :received-at (System/currentTimeMillis)}]
    (swap! pending-swarm-prompts assoc slave-id prompt-data)
    (log/info "Swarm prompt received from" slave-id ":"
              (subs prompt-text 0 (min 50 (count prompt-text))))
    prompt-data))

(defn remove-swarm-prompt!
  "Remove a swarm prompt (after it's been answered)."
  [slave-id]
  (swap! pending-swarm-prompts dissoc slave-id)
  (log/debug "Swarm prompt cleared:" slave-id))

(defn get-swarm-prompts
  "Get all pending swarm prompts."
  []
  @pending-swarm-prompts)

;; =============================================================================
;; Core Functions

(defn shout!
  "Broadcast a message to the hivemind coordinator.

   event-type: keyword like :progress, :completed, :error, :blocked
   data: map with event details, optionally including :project-id

   Stores up to 10 recent messages per agent for retrieval via hivemind_status.
   Broadcasts via WebSocket (Aleph) for reliable push delivery.

   ADR-002 AMENDED: Updates both:
   - agent-registry atom: message history (hivemind-specific)
   - DataScript: slave status (source of truth for slave data)

   Project-id resolution order:
   1. Explicit :project-id in data
   2. Derived from :directory in data
   3. From slave's :slave/cwd in DataScript
   4. Fallback to 'global'

   Returns true if broadcast succeeded."
  [agent-id event-type data]
  (let [now (System/currentTimeMillis)
        ;; Derive project-id with fallback chain
        explicit-project-id (:project-id data)
        directory (:directory data)
        slave-cwd (when-let [slave (proto/get-slave registry/default-registry agent-id)]
                    (:slave/cwd slave))
        project-id (or explicit-project-id
                       (when directory (mem-scope/get-current-project-id directory))
                       (when slave-cwd (mem-scope/get-current-project-id slave-cwd))
                       "global")
        message (cond-> {:event-type event-type
                         :timestamp now
                         :project-id project-id
                         :data (dissoc data :task :message :directory :project-id)}
                  (:task data) (assoc :task (:task data))
                  (:message data) (assoc :message (:message data)))
        event {:type (keyword (str "hivemind-" (name event-type)))
               :agent-id agent-id
               :timestamp now
               :project-id project-id
               :data data}]
    ;; Update agent-registry with message history ONLY (ring buffer, max 10)
    ;; Status/metadata now comes from DataScript
    (swap! agent-registry update agent-id
           (fn [agent]
             (let [messages (or (:messages agent) [])
                   new-messages (vec (take-last 10 (conj messages message)))]
               {:messages new-messages
                :last-seen now})))
    ;; Update DataScript status if slave exists there
    ;; This keeps DataScript in sync with hivemind events
    (when (proto/get-slave registry/default-registry agent-id)
      (proto/update-slave! registry/default-registry agent-id {:slave/status event-type}))
    ;; Broadcast to Emacs via WebSocket (primary - reliable)
    (when (ws/connected?)
      (ws/emit! (:type event) (dissoc event :type)))
    ;; Also broadcast via old channel (for backwards compat)
    (channel/broadcast! event)
    (log/info "Hivemind shout:" agent-id event-type "project:" project-id)
    ;; Auto-trigger crystallization on ling completion
    (when (= event-type :completed)
      (events/dispatch {:type :ling/completed
                        :agent-id agent-id
                        :project-id project-id
                        :data data}))
    true))

(defn ask!
  "Request a decision from the human coordinator.
   
   Blocks until response received or timeout.
   
   agent-id: identifier for this agent
   question: string describing what decision is needed
   options: vector of option strings, or nil for free-form
   timeout-ms: how long to wait (default 5 minutes)
   
   Returns {:decision ... :by ...} or {:timeout true}"
  [agent-id question options & {:keys [timeout-ms] :or {timeout-ms 300000}}]
  (let [ask-id (str (random-uuid))
        response-chan (chan 1)
        ask-event {:type :hivemind-ask
                   :ask-id ask-id
                   :agent-id agent-id
                   :question question
                   :options options
                   :timestamp (System/currentTimeMillis)}]
    ;; Register pending ask
    (swap! pending-asks assoc ask-id {:question question
                                      :options options
                                      :agent-id agent-id
                                      :response-chan response-chan})
    ;; Broadcast to coordinator
    (channel/broadcast! ask-event)
    (log/info "Hivemind ask:" agent-id question)
    ;; Wait for response
    (let [result (alt!!
                   response-chan ([v] v)
                   (timeout timeout-ms) {:timeout true :ask-id ask-id})]
      ;; Cleanup
      (swap! pending-asks dissoc ask-id)
      result)))

(defn respond-ask!
  "Respond to a pending ask (called from Emacs side).
   
   ask-id: the ask-id from the original ask event
   decision: the chosen option or free-form response
   by: who made the decision (default 'human')"
  [ask-id decision & {:keys [by] :or {by "human"}}]
  (if-let [{:keys [response-chan]} (get @pending-asks ask-id)]
    (do
      (>!! response-chan {:decision decision :by by :ask-id ask-id})
      (log/info "Hivemind response:" ask-id decision)
      true)
    (do
      (log/warn "No pending ask for id:" ask-id)
      false)))

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
         msg-history @agent-registry]
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
                        @pending-asks)
    :pending-swarm-prompts (mapv (fn [[slave-id {:keys [prompt timestamp session-id received-at]}]]
                                   {:slave-id slave-id
                                    :prompt prompt
                                    :timestamp timestamp
                                    :session-id session-id
                                    :received-at received-at})
                                 @pending-swarm-prompts)
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
  (let [msg-history-entry (get @agent-registry agent-id)
        ds-slave (proto/get-slave registry/default-registry agent-id)]
    (cond
      ;; Has messages in history
      msg-history-entry (:messages msg-history-entry)
      ;; Exists in DataScript but no messages yet
      ds-slave []
      ;; Not found anywhere
      :else nil)))

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
    (swap! agent-registry assoc agent-id
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
  (swap! agent-registry dissoc agent-id)
  ;; Release core.logic claims FIRST (prevents ghost claims)
  ;; This is critical - these claims are used for conflict detection
  (logic/release-claims-for-slave! agent-id)
  ;; Clear from DataScript (also releases DataScript claims via lings/remove-slave!)
  (proto/remove-slave! registry/default-registry agent-id)
  (log/info "Agent cleared from hivemind:" agent-id))

(defn clear-agent-registry!
  "Clear agent registry. GUARDED - no-op if coordinator running.

   CLARITY-Y: Yield safe failure - prevents test fixtures from
   corrupting production hivemind state."
  []
  (guards/when-not-coordinator
   "clear-agent-registry! called"
   (reset! agent-registry {})))

;; =============================================================================
;; Event Subscriptions (for agents to listen)

;; REMOVED: listen! was a polling anti-pattern
;; Event-driven architecture: shout! pushes to channel, get-status reads accumulated state
;; No blocking/polling needed - the coordinator queries when ready

;; =============================================================================
;; MCP Tool Definitions

(def tools
  [{:name "hivemind_shout"
    :description "Broadcast status/progress to the hivemind coordinator.
                  
USE THIS to report:
- Task progress: (hivemind_shout :progress {:task \"..\" :percent 50})
- Completion: (hivemind_shout :completed {:task \"..\" :result \"..\"})
- Errors: (hivemind_shout :error {:task \"..\" :error \"..\"})
- Blocked: (hivemind_shout :blocked {:task \"..\" :reason \"need input\"})

The coordinator sees all shouts in real-time.

NOTE: agent_id is auto-detected from CLAUDE_SWARM_SLAVE_ID env var if not provided."
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "Your agent identifier (auto-detected from CLAUDE_SWARM_SLAVE_ID if omitted)"}
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
               ;; Fallback chain: explicit param → context binding → env var → unknown
               (let [effective-id (or agent_id
                                      (ctx/current-agent-id)
                                      (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                                      "unknown-agent")
                     effective-dir (or directory
                                       (ctx/current-directory))]
                 (shout! effective-id (keyword event_type)
                         (merge {:task task :message message :directory effective-dir} data))
                 {:type "text" :text (json/write-str {:success true
                                                      :agent_id effective-id})}))}

   {:name "hivemind_ask"
    :description "Request a decision from the human coordinator.
                  
USE THIS when you need human approval or guidance:
- Before destructive operations
- When multiple valid approaches exist
- When requirements are ambiguous

BLOCKS until human responds (up to timeout).

Example: hivemind_ask('Should I delete these 50 files?', ['yes', 'no', 'show me first'])

NOTE: agent_id is auto-detected from CLAUDE_SWARM_SLAVE_ID env var if not provided."
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "Your agent identifier (auto-detected from CLAUDE_SWARM_SLAVE_ID if omitted)"}
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
               ;; Fallback chain: explicit param → context binding → env var → unknown
               (let [effective-id (or agent_id
                                      (ctx/current-agent-id)
                                      (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                                      "unknown-agent")
                     effective-dir (or directory
                                       (ctx/current-directory))
                     result (ask! effective-id question options
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
                  :text (json/write-str (get-status project-id))}))}

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
                       (if (respond-ask! ask_id decision)
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
                                  (->> @agent-registry
                                       (filter (fn [[_id {:keys [messages]}]]
                                                 (some #(= project-id (:project-id %)) messages)))
                                       (map first)
                                       set)
                                  (clojure.core/set (keys @agent-registry)))
                     available-agents (vec (set/union ds-agents msg-agents))]
                 {:type "text"
                  :text (json/write-str
                         (if-let [messages (get-agent-messages agent_id)]
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
