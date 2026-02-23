(ns hive-mcp.hivemind.messaging
  "Hivemind messaging — shout, ask, respond, and piggyback registration.

   M1 (protocol-first): Shouts publish via IEventBackbone (NATS, Redis, etc.).
   Fallback fanout uses IDeliveryChannel registry — no hardcoded transports.
   Local state (atom, DataScript) still updated synchronously for consistency."

  (:require [hive-mcp.hivemind.state :as state]
            [hive-mcp.hivemind.event-registry :as event-registry]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.protocols.delivery-channel :as dc]
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

(defn- all-hivemind-messages
  "Return all hivemind messages for piggyback module."
  []
  (mapcat (fn [[agent-id {:keys [messages]}]]
            (for [{:keys [event-type message task timestamp project-id]} messages]
              {:agent-id agent-id
               :event-type event-type
               :message (or message task "")
               :timestamp timestamp
               :project-id (or project-id "global")}))
          @state/agent-registry))

(piggyback/register-message-source! all-hivemind-messages)

(defn- event-type->slave-status
  "Map hivemind event type to valid DataScript slave status.
   Derives from event-registry — no hardcoded case statement."
  [event-type]
  (event-registry/slave-status event-type))

(defn- publish-shout-to-backbone!
  "Publish shout via IEventBackbone. Subscribers handle fanout.
   Uses requiring-resolve for bridge to avoid circular dep."
  [payload]
  (try
    (when-let [publish-fn (requiring-resolve 'hive-mcp.nats.bridge/publish-shout!)]
      (publish-fn payload))
    (catch Exception e
      (log/debug "[Backbone] Shout publish failed (non-fatal):" (.getMessage e)))))

(defn- fanout-shout-direct!
  "Direct fanout via IDeliveryChannel registry when backbone is unavailable.
   Protocol-mediated — no hardcoded transport calls."
  [payload]
  (dc/fanout! payload))

(defn shout!
  "Broadcast a message to the hivemind coordinator.

   M1 Architecture (protocol-first):
   - Local state (atom + DataScript) updated synchronously
   - If backbone connected: single publish → backbone subscribers handle fanout
   - If backbone disconnected: direct fanout via IDeliveryChannel registry
   - hive-events dispatch for :ling/completed (domain event, always fires)"
  [agent-id event-type data]
  (let [now (System/currentTimeMillis)
        resolved-slave (queries/get-slave-by-name-or-id agent-id)
        resolved-slave-id (or (:slave/id resolved-slave) agent-id)
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
        ;; Backbone payload — flat, self-contained, no internal references
        backbone-payload {:agent-id agent-id
                          :event-type event-type
                          :timestamp now
                          :project-id project-id
                          :message (:message data)
                          :task (:task data)
                          :data (dissoc data :task :message :directory :project-id)}]
    ;; 1. Local state — always (atom for piggyback reads)
    (swap! state/agent-registry update agent-id
           (fn [agent]
             (let [messages (or (:messages agent) [])
                   new-messages (vec (take-last 10 (conj messages message)))]
               {:messages new-messages
                :last-seen now})))
    ;; 2. DataScript slave status — always
    (when resolved-slave
      (proto/update-slave! registry/default-registry resolved-slave-id
                           {:slave/status (event-type->slave-status event-type)}))
    ;; 3. Backbone publish OR direct fanout (protocol-mediated)
    (let [backbone (eb/get-backbone)]
      (if (eb/connected? backbone)
        (publish-shout-to-backbone! backbone-payload)
        (fanout-shout-direct! backbone-payload)))
    ;; 4. Log
    (log/info "Hivemind shout:" agent-id event-type "project:" project-id)
    ;; 5. Domain event dispatch (hive-events, not fanout)
    (when (= event-type :completed)
      (try
        (events/dispatch [:ling/completed {:agent-id agent-id
                                           :project-id project-id
                                           :data data}])
        (catch Exception e
          (log/debug "ling/completed dispatch failed (handler may not be registered):" (.getMessage e)))))
    true))

(defn ask!
  "Request a decision from the human coordinator, blocking until response or timeout."
  [agent-id question options & {:keys [timeout-ms] :or {timeout-ms 300000}}]
  (let [ask-id (str (random-uuid))
        response-chan (chan 1)
        ask-event {:type :hivemind-ask
                   :ask-id ask-id
                   :agent-id agent-id
                   :question question
                   :options options
                   :timestamp (System/currentTimeMillis)}]
    (swap! state/pending-asks assoc ask-id {:question question
                                            :options options
                                            :agent-id agent-id
                                            :response-chan response-chan})
    (channel/broadcast! ask-event)
    (log/info "Hivemind ask:" agent-id question)
    (let [result (alt!!
                   response-chan ([v] v)
                   (timeout timeout-ms) {:timeout true :ask-id ask-id})]
      (swap! state/pending-asks dissoc ask-id)
      result)))

(defn respond-ask!
  "Respond to a pending ask from an agent."
  [ask-id decision & {:keys [by] :or {by "human"}}]
  (if-let [{:keys [response-chan]} (get @state/pending-asks ask-id)]
    (do
      (>!! response-chan {:decision decision :by by :ask-id ask-id})
      (log/info "Hivemind response:" ask-id decision)
      true)
    (do
      (log/warn "No pending ask for id:" ask-id)
      false)))
