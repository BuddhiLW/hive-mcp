(ns emacs-mcp.hivemind
  "Hivemind coordination tools for swarm agents.
   
   Provides event-driven communication between agents and the coordinator,
   replacing file-based polling with instant push notifications.
   
   Tools:
   - hivemind_shout: Broadcast status/progress to coordinator
   - hivemind_ask: Request human decision (blocks until response)
   - hivemind_status: Get current hivemind state
   - hivemind_listen: Subscribe to specific event types
   
   All communication flows through the bidirectional channel to Emacs."
  (:require [emacs-mcp.channel :as channel]
            [clojure.core.async :as async :refer [<!! >!! chan timeout alt!!]]
            [taoensso.timbre :as log]))

;; =============================================================================
;; State

(defonce pending-asks
  "Map of ask-id -> {:question ... :response-chan ...}"
  (atom {}))

(defonce agent-registry
  "Map of agent-id -> {:status :last-seen :current-task}"
  (atom {}))

;; =============================================================================
;; Core Functions

(defn shout!
  "Broadcast a message to the hivemind coordinator.
   
   event-type: keyword like :progress, :completed, :error, :blocked
   data: map with event details
   
   Returns true if broadcast succeeded."
  [agent-id event-type data]
  (let [event {:type (keyword (str "hivemind-" (name event-type)))
               :agent-id agent-id
               :timestamp (System/currentTimeMillis)
               :data data}]
    ;; Update agent registry
    (swap! agent-registry assoc agent-id
           {:status event-type
            :last-seen (System/currentTimeMillis)
            :current-task (:task data)})
    ;; Broadcast to Emacs
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
   - :pending-asks - list of unanswered questions
   - :channel-connected - whether Emacs is connected"
  []
  {:agents @agent-registry
   :pending-asks (mapv (fn [[id {:keys [question options agent-id]}]]
                         {:ask-id id
                          :agent-id agent-id
                          :question question
                          :options options})
                       @pending-asks)
   :channel-connected (channel/connected?)})

(defn clear-agent!
  "Remove an agent from the registry (when it terminates)."
  [agent-id]
  (swap! agent-registry dissoc agent-id)
  (log/info "Agent cleared from hivemind:" agent-id))

;; =============================================================================
;; Event Subscriptions (for agents to listen)

(defn subscribe!
  "Subscribe to hivemind events of a specific type.
   
   event-type: keyword like :hivemind-task-assigned
   handler-fn: function that receives the event map
   
   Returns subscription id for unsubscribing."
  [event-type handler-fn]
  (channel/subscribe! event-type handler-fn))

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

The coordinator sees all shouts in real-time."
    :inputSchema {:type "object"
                  :properties {:agent_id {:type "string"
                                          :description "Your agent identifier"}
                               :event_type {:type "string"
                                            :enum ["progress" "completed" "error" "blocked" "started"]
                                            :description "Type of event"}
                               :task {:type "string"
                                      :description "Current task description"}
                               :message {:type "string"
                                         :description "Status message"}
                               :data {:type "object"
                                      :description "Additional event data"}}
                  :required ["agent_id" "event_type"]}
    :handler (fn [{:keys [agent_id event_type task message data]}]
               (shout! agent_id (keyword event_type)
                       (merge {:task task :message message} data))
               {:success true})}

   {:name "hivemind_ask"
    :description "Request a decision from the human coordinator.
                  
USE THIS when you need human approval or guidance:
- Before destructive operations
- When multiple valid approaches exist
- When requirements are ambiguous

BLOCKS until human responds (up to timeout).

Example: hivemind_ask('Should I delete these 50 files?', ['yes', 'no', 'show me first'])"
    :inputSchema {:type "object"
                  :properties {:agent_id {:type "string"
                                          :description "Your agent identifier"}
                               :question {:type "string"
                                          :description "What decision do you need?"}
                               :options {:type "array"
                                         :items {:type "string"}
                                         :description "Available options (or omit for free-form)"}
                               :timeout_ms {:type "integer"
                                            :description "Timeout in ms (default 300000 = 5 min)"}}
                  :required ["agent_id" "question"]}
    :handler (fn [{:keys [agent_id question options timeout_ms]}]
               (let [result (ask! agent_id question options
                                  :timeout-ms (or timeout_ms 300000))]
                 (if (:timeout result)
                   {:timeout true :message "No response within timeout"}
                   {:decision (:decision result)
                    :by (:by result)})))}

   {:name "hivemind_status"
    :description "Get current hivemind coordinator status.
                  
Returns:
- Active agents and their status
- Pending questions awaiting human decision
- Channel connection status"
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler (fn [_] (get-status))}

   {:name "hivemind_respond"
    :description "Respond to a pending ask from an agent.
                  
Used by the coordinator to answer agent questions."
    :inputSchema {:type "object"
                  :properties {:ask_id {:type "string"
                                        :description "ID of the ask to respond to"}
                               :decision {:type "string"
                                          :description "The decision/response"}}
                  :required ["ask_id" "decision"]}
    :handler (fn [{:keys [ask_id decision]}]
               (if (respond-ask! ask_id decision)
                 {:success true}
                 {:error "No pending ask with that ID"}))}])

(defn register-tools!
  "Register hivemind tools with the MCP server."
  [register-fn]
  (doseq [tool tools]
    (register-fn tool)))
