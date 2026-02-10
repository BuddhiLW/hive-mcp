(ns hive-mcp.hivemind.messaging
  "Hivemind messaging — shout, ask, respond, and piggyback registration.

   Core communication functions for the hivemind coordination system.
   Agents broadcast status via shout!, request decisions via ask!,
   and coordinators respond via respond-ask!.

   Also registers the piggyback message source for tool response embedding.

   SOLID: SRP — messaging logic only, state lives in hivemind.state.
   CLARITY: L — Side-effects clearly delineated (broadcasts, DataScript updates)."
  (:require [hive-mcp.hivemind.state :as state]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.channel.websocket :as ws]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.swarm.protocol :as proto]
            [hive-mcp.swarm.datascript.registry :as registry]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.tools.memory.scope :as mem-scope]
            [hive-mcp.events.core :as events]
            [clojure.core.async :as async :refer [>!! chan timeout alt!!]]
            [taoensso.timbre :as log])
  (:import [java.lang Exception]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Piggyback Message Source Registration (DIP)
;; =============================================================================

(defn- all-hivemind-messages
  "Provide all hivemind messages to piggyback module.
   Returns flat seq of {:agent-id :event-type :message :timestamp :project-id}.

   Note: project-id is included for per-project cursor scoping.
   Messages without project-id default to 'global'.

   BUG FIX: Shouts may have :task but no :message (or vice versa).
   Fall back to :task when :message is nil, then empty string to prevent {:m nil} in piggyback."
  []
  (mapcat (fn [[agent-id {:keys [messages]}]]
            (for [{:keys [event-type message task timestamp project-id]} messages]
              {:agent-id agent-id
               :event-type event-type
               :message (or message task "")
               :timestamp timestamp
               :project-id (or project-id "global")}))
          @state/agent-registry))

;; Register this namespace as the message source for piggyback (DIP)
(piggyback/register-message-source! all-hivemind-messages)

;; =============================================================================
;; Core Functions
;; =============================================================================

(defn- event-type->slave-status
  "Map hivemind event type to valid DataScript slave status.

   Event types from shouts (:started, :progress, :completed, :error, :blocked)
   must be mapped to valid slave-statuses from schema.clj:
   #{:idle :spawning :starting :initializing :working :blocked :error :terminated}

   BUG FIX: Previously event-type was passed directly to DataScript, but
   :completed/:progress/:started are not valid slave statuses, causing
   lings to appear stuck on 'working' when they were actually idle."
  [event-type]
  (case event-type
    :started   :working
    :progress  :working
    :completed :idle
    :error     :error
    :blocked   :blocked
    ;; Default to :idle for unknown event types
    :idle))

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
        ;; BUG FIX: Resolve agent-id mismatch between shout (short ID like "ling-NAME")
        ;; and DataScript (full spawn ID like "swarm-NAME-TIMESTAMP").
        ;; Uses get-slave-by-name-or-id which tries exact match first, then name fallback.
        resolved-slave (queries/get-slave-by-name-or-id agent-id)
        resolved-slave-id (or (:slave/id resolved-slave) agent-id)
        ;; Derive project-id with fallback chain
        explicit-project-id (:project-id data)
        directory (:directory data)
        slave-cwd (:slave/cwd resolved-slave)
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
    (swap! state/agent-registry update agent-id
           (fn [agent]
             (let [messages (or (:messages agent) [])
                   new-messages (vec (take-last 10 (conj messages message)))]
               {:messages new-messages
                :last-seen now})))
    ;; Update DataScript status if slave exists there (using resolved slave-id)
    ;; This keeps DataScript in sync with hivemind events
    ;; BUG FIX: Map event-type to valid slave status (not all event types are valid statuses)
    (when resolved-slave
      (proto/update-slave! registry/default-registry resolved-slave-id {:slave/status (event-type->slave-status event-type)}))
    ;; Broadcast to Emacs via WebSocket (primary - reliable)
    (when (ws/connected?)
      (ws/emit! (:type event) (dissoc event :type)))
    ;; Publish to internal pub/sub (for SlackConnector and other internal subscribers)
    (channel/publish! event)
    ;; Also broadcast via old channel (for Emacs clients)
    (channel/broadcast! event)
    ;; Broadcast to Olympus Web UI (port 7911)
    ;; Uses requiring-resolve to avoid circular dependency
    (try
      (when-let [emit-fn (requiring-resolve 'hive-mcp.transport.olympus/emit-hivemind-shout!)]
        (emit-fn {:agent-id agent-id
                  :event-type (name event-type)
                  :message (:message data)
                  :task (:task data)
                  :data (dissoc data :task :message)}))
      (catch Exception _
        ;; Olympus WS not started yet - ignore silently
        nil))
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
    (swap! state/pending-asks assoc ask-id {:question question
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
      (swap! state/pending-asks dissoc ask-id)
      result)))

(defn respond-ask!
  "Respond to a pending ask (called from Emacs side).

   ask-id: the ask-id from the original ask event
   decision: the chosen option or free-form response
   by: who made the decision (default 'human')"
  [ask-id decision & {:keys [by] :or {by "human"}}]
  (if-let [{:keys [response-chan]} (get @state/pending-asks ask-id)]
    (do
      (>!! response-chan {:decision decision :by by :ask-id ask-id})
      (log/info "Hivemind response:" ask-id decision)
      true)
    (do
      (log/warn "No pending ask for id:" ask-id)
      false)))
