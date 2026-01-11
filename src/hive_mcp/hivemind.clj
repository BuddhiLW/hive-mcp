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
   Coordinator queries get-status when ready - no polling loops needed."
  (:require [hive-mcp.channel :as channel]
            [hive-mcp.channel.websocket :as ws]
            [hive-mcp.channel.piggyback :as piggyback]
            [clojure.core.async :as async :refer [<!! >!! chan timeout alt!!]]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; =============================================================================
;; State

(defonce ^{:doc "Map of ask-id -> {:question ... :response-chan ...}"}
  pending-asks
  (atom {}))

(defonce ^{:doc "Map of agent-id -> {:status :last-seen :current-task :messages}
                 Messages is a vector of recent shouts (max 10 per agent)."}
  agent-registry
  (atom {}))

;; =============================================================================
;; Piggyback Message Source Registration (DIP)
;; =============================================================================

(defn- all-hivemind-messages
  "Provide all hivemind messages to piggyback module.
   Returns flat seq of {:agent-id :event-type :message :timestamp}."
  []
  (mapcat (fn [[agent-id {:keys [messages]}]]
            (for [{:keys [event-type message timestamp]} messages]
              {:agent-id agent-id
               :event-type event-type
               :message message
               :timestamp timestamp}))
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
   data: map with event details
   
   Stores up to 10 recent messages per agent for retrieval via hivemind_status.
   Broadcasts via WebSocket (Aleph) for reliable push delivery.
   Returns true if broadcast succeeded."
  [agent-id event-type data]
  (let [now (System/currentTimeMillis)
        message {:event-type event-type
                 :timestamp now
                 :task (:task data)
                 :message (:message data)
                 :data (dissoc data :task :message)}
        event {:type (keyword (str "hivemind-" (name event-type)))
               :agent-id agent-id
               :timestamp now
               :data data}]
    ;; Update agent registry with message history (ring buffer, max 10)
    (swap! agent-registry update agent-id
           (fn [agent]
             (let [messages (or (:messages agent) [])
                   new-messages (vec (take-last 10 (conj messages message)))]
               {:status event-type
                :last-seen now
                :current-task (:task data)
                :messages new-messages})))
    ;; Broadcast to Emacs via WebSocket (primary - reliable)
    (when (ws/connected?)
      (ws/emit! (:type event) (dissoc event :type)))
    ;; Also broadcast via old channel (for backwards compat)
    (channel/broadcast! event)
    (log/info "Hivemind shout:" agent-id event-type)
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

(defn get-status
  "Get current hivemind status.
   
   Returns map with:
   - :agents - map of agent-id -> status
   - :pending-asks - list of unanswered questions (agent-initiated)
   - :pending-swarm-prompts - list of permission prompts from slaves (push notifications)
   - :channel-connected - whether Emacs is connected (legacy bencode channel)
   - :ws-connected - whether WebSocket channel is connected (preferred)"
  []
  {:agents @agent-registry
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
   :ws-clients (ws/client-count)})

(defn get-agent-messages
  "Get recent messages from a specific agent.
   
   Returns vector of messages (up to 10) or nil if agent not found."
  [agent-id]
  (get-in @agent-registry [agent-id :messages]))

(defn clear-agent!
  "Remove an agent from the registry (when it terminates)."
  [agent-id]
  (swap! agent-registry dissoc agent-id)
  (log/info "Agent cleared from hivemind:" agent-id))

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
                               "data" {:type "object"
                                       :description "Additional event data"}}
                  :required ["event_type"]}
    :handler (fn [{:keys [agent_id event_type task message data]}]
               (let [effective-id (or agent_id
                                      (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                                      "unknown-agent")]
                 (shout! effective-id (keyword event_type)
                         (merge {:task task :message message} data))
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
                                             :description "Timeout in ms (default 300000 = 5 min)"}}
                  :required ["question"]}
    :handler (fn [{:keys [agent_id question options timeout_ms]}]
               (let [effective-id (or agent_id
                                      (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                                      "unknown-agent")
                     result (ask! effective-id question options
                                  :timeout-ms (or timeout_ms 300000))]
                 {:type "text"
                  :text (json/write-str
                         (if (:timeout result)
                           {:timeout true :message "No response within timeout"}
                           {:decision (:decision result) :by (:by result)}))}))}

   {:name "hivemind_status"
    :description "Get current hivemind coordinator status.
                  
Returns:
- Active agents and their status
- Pending questions awaiting human decision
- Channel connection status"
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler (fn [_]
               {:type "text"
                :text (json/write-str (get-status))})}

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
Use this to retrieve message content that agents have broadcast."
    :inputSchema {:type "object"
                  :properties {"agent_id" {:type "string"
                                           :description "Agent identifier to get messages from"}}
                  :required ["agent_id"]}
    :handler (fn [{:keys [agent_id]}]
               {:type "text"
                :text (json/write-str
                       (if-let [messages (get-agent-messages agent_id)]
                         {:agent_id agent_id :messages messages}
                         {:error (str "Agent not found: " agent_id)
                          :available-agents (keys @agent-registry)}))})}])
;; REMOVED: hivemind_listen - polling anti-pattern, use event-driven get-status instead

(defn register-tools!
  "Register hivemind tools with the MCP server."
  [register-fn]
  (doseq [tool tools]
    (register-fn tool)))
