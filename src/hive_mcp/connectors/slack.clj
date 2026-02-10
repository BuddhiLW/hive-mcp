(ns hive-mcp.connectors.slack
  "Slack connector for hivemind integration via Slack Web API + Webhooks.

   Implements IConnector protocol for connection lifecycle management.
   Provides both webhook-based event forwarding AND Slack Web API operations:
   - send-message: Post messages to channels via chat.postMessage
   - list-channels: List available channels via conversations.list
   - read-history: Read channel history via conversations.history

   Configuration (connect! opts):
     :bot-token   - Slack Bot User OAuth Token (xoxb-...) for Web API (required for API ops)
     :webhook-url - Slack Incoming Webhook URL (optional, for event forwarding)
     :channel     - Default channel for messages (optional)
     :username    - Bot username (default: 'Hivemind')
     :icon-emoji  - Bot icon (default: ':bee:')
     :events      - Set of event types to forward via webhook (default: all hivemind events)
     :format      - Message format :compact or :detailed (default: :compact)

   Usage:
     (def slack (->slack-connector :hivemind-slack))
     (connect! slack {:bot-token \"xoxb-...\"
                      :webhook-url \"https://hooks.slack.com/...\"})

     ;; Web API operations
     (send-message slack {:channel \"#general\" :text \"Hello from hive!\"})
     (list-channels slack {})
     (read-history slack {:channel \"C1234567890\" :limit 10})

     ;; Disconnect
     (disconnect! slack)

   CLARITY-L: Layers stay pure - connector handles only Slack integration.
   CLARITY-Y: Yield safe failure - graceful degradation on Slack API errors.
   SOLID-S: Single responsibility - Slack communication only."
  (:require [hive-mcp.protocols.connector :as connector]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clj-http.client :as http]
            [taoensso.timbre :as log])
  (:import [java.time Instant]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:private slack-api-base "https://slack.com/api")

(def ^:private default-config
  {:username "Hivemind"
   :icon-emoji ":bee:"
   :format :compact
   :timeout-ms 10000
   :events #{:hivemind-started :hivemind-progress :hivemind-completed
             :hivemind-error :hivemind-blocked :hivemind-ask}})

(def ^:private event-emoji
  "Emoji mapping for event types."
  {:hivemind-started   ":rocket:"
   :hivemind-progress  ":gear:"
   :hivemind-completed ":white_check_mark:"
   :hivemind-error     ":x:"
   :hivemind-blocked   ":warning:"
   :hivemind-ask       ":question:"})

;; =============================================================================
;; HTTP Helpers (testable via with-redefs)
;; =============================================================================

(defn slack-api-request
  "Make an authenticated request to Slack Web API.
   Returns parsed JSON response body as Clojure map.

   Arguments:
     method    - :get or :post
     endpoint  - API endpoint (e.g., \"chat.postMessage\")
     bot-token - Slack bot token (xoxb-...)
     opts      - Request-specific options:
                 :body    - Request body map (for POST, auto-JSON-encoded)
                 :params  - Query parameters map (for GET)
                 :timeout - Timeout in ms (default: 10000)

   Returns map with:
     :ok      - Boolean (Slack API success flag)
     :error   - Error string if :ok is false
     + all other fields from Slack API response

   CLARITY-Y: Never throws. Returns {:ok false :error \"...\"} on failure."
  [method endpoint bot-token opts]
  (try
    (let [url (str slack-api-base "/" endpoint)
          timeout (or (:timeout opts) 10000)
          request-opts (cond-> {:headers {"Authorization" (str "Bearer " bot-token)
                                          "Content-Type" "application/json; charset=utf-8"}
                                :socket-timeout timeout
                                :connection-timeout timeout
                                :throw-exceptions false
                                :as :auto}
                         (= method :post)
                         (assoc :body (json/write-str (:body opts)))

                         (= method :get)
                         (assoc :query-params (:params opts)))
          response (case method
                     :get  (http/get url request-opts)
                     :post (http/post url request-opts))]
      (if (<= 200 (:status response) 299)
        (let [body (if (string? (:body response))
                     (json/read-str (:body response) :key-fn keyword)
                     (:body response))]
          (or body {:ok false :error "Empty response body"}))
        {:ok false
         :error (str "HTTP " (:status response) ": " (:body response))}))
    (catch Exception e
      {:ok false
       :error (.getMessage e)})))

;; =============================================================================
;; Message Formatting
;; =============================================================================

(defn- format-compact
  "Format event as compact single-line Slack message."
  [{:keys [type agent-id data] :as _event}]
  (let [emoji (get event-emoji type ":speech_balloon:")
        task (or (:task data) "")
        message (or (:message data) "")]
    (str emoji " *" (name type) "* | `" agent-id "`"
         (when (seq task) (str " | " task))
         (when (seq message) (str "\n>" message)))))

(defn- format-detailed
  "Format event as detailed Slack message with attachments."
  [{:keys [type agent-id timestamp project-id data] :as _event}]
  (let [emoji (get event-emoji type ":speech_balloon:")
        color (case type
                :hivemind-completed "#36a64f"
                :hivemind-error "#ff0000"
                :hivemind-blocked "#ffcc00"
                "#439FE0")
        task (:task data)
        message (:message data)]
    {:text (str emoji " *" (name type) "*")
     :attachments [{:color color
                    :fields (cond-> [{:title "Agent"
                                      :value (str "`" agent-id "`")
                                      :short true}
                                     {:title "Project"
                                      :value (or project-id "global")
                                      :short true}]
                              task (conj {:title "Task"
                                          :value task
                                          :short false})
                              message (conj {:title "Message"
                                             :value message
                                             :short false}))
                    :ts (when timestamp (/ timestamp 1000))}]}))

(defn- format-event
  "Format hivemind event for Slack based on format setting."
  [event format-type]
  (case format-type
    :detailed (format-detailed event)
    {:text (format-compact event)}))

;; =============================================================================
;; Webhook Posting
;; =============================================================================

(defn- post-to-webhook!
  "POST message to Slack webhook. Returns {:success? bool :error msg}."
  [webhook-url payload config]
  (try
    (let [body (merge payload
                      (select-keys config [:channel :username :icon-emoji]))
          response (http/post webhook-url
                              {:body (json/write-str body)
                               :content-type :json
                               :socket-timeout 5000
                               :connection-timeout 5000
                               :throw-exceptions false})]
      (if (= 200 (:status response))
        {:success? true}
        {:success? false
         :error (str "Slack webhook returned " (:status response) ": " (:body response))}))
    (catch Exception e
      {:success? false
       :error (.getMessage e)})))

;; =============================================================================
;; Event Subscription (optional - only if channel ns available)
;; =============================================================================

(defn- try-resolve-channel-fns
  "Attempt to resolve hive-mcp.channel.core subscribe!/unsubscribe! functions.
   Returns {:subscribe! fn :unsubscribe! fn} or nil if not available.
   CLARITY-Y: Graceful fallback if channel ns not loaded."
  []
  (try
    (require 'hive-mcp.channel.core)
    (let [sub-fn (ns-resolve (find-ns 'hive-mcp.channel.core) 'subscribe!)
          unsub-fn (ns-resolve (find-ns 'hive-mcp.channel.core) 'unsubscribe!)]
      (when (and sub-fn unsub-fn)
        {:subscribe! @sub-fn
         :unsubscribe! @unsub-fn}))
    (catch Exception _
      (log/debug "[slack] hive-mcp.channel.core not available - event forwarding disabled")
      nil)))

(defn- start-event-listener!
  "Start go-loop that listens for hivemind events and forwards to Slack webhook.
   Returns the subscription info for cleanup, or nil if channel ns unavailable."
  [webhook-url config state-atom]
  (when-let [{:keys [subscribe!]} (try-resolve-channel-fns)]
    (let [events (:events config)
          merged-ch (async/chan 256)
          sub-channels (mapv (fn [event-type]
                               (let [ch (subscribe! event-type)]
                                 (async/go-loop []
                                   (when-let [event (async/<! ch)]
                                     (async/>! merged-ch event)
                                     (recur)))
                                 {:type event-type :ch ch}))
                             events)]
      ;; Main processing loop
      (async/go-loop []
        (when-let [event (async/<! merged-ch)]
          (when (:connected? @state-atom)
            (let [payload (format-event event (:format config))
                  result (post-to-webhook! webhook-url payload config)]
              (if (:success? result)
                (do
                  (swap! state-atom update :message-count (fnil inc 0))
                  (log/debug "[slack] Event forwarded:" (:type event)))
                (do
                  (swap! state-atom update :error-count (fnil inc 0))
                  (swap! state-atom assoc :last-error (:error result))
                  (log/warn "[slack] Forward failed:" (:error result))))))
          (recur)))
      {:merged-ch merged-ch
       :sub-channels sub-channels})))

(defn- stop-event-listener!
  "Stop the event listener and clean up subscriptions."
  [{:keys [merged-ch sub-channels]}]
  (when-let [{:keys [unsubscribe!]} (try-resolve-channel-fns)]
    (when merged-ch
      (async/close! merged-ch))
    (doseq [{:keys [type ch]} sub-channels]
      (unsubscribe! type ch))))

;; =============================================================================
;; Slack Web API Operations
;; =============================================================================

(defn send-message
  "Send a message to a Slack channel via chat.postMessage.

   Arguments:
     connector - SlackConnector instance (must be connected with :bot-token)
     opts      - Message options:
                 :channel  - Channel ID or name (e.g., \"C1234567890\" or \"#general\")
                             Falls back to connector's default channel.
                 :text     - Message text (required if no :blocks)
                 :blocks   - Slack Block Kit blocks (optional)
                 :thread-ts - Thread timestamp to reply in thread (optional)
                 :unfurl-links - Boolean, enable link unfurling (default: true)

   Returns map with:
     :ok       - Boolean success
     :ts       - Message timestamp (for threading)
     :channel  - Channel ID message was posted to
     :error    - Error string if failed

   CLARITY-Y: Returns {:ok false :error \"...\"} on any failure."
  [connector opts]
  (let [state @(:state connector)
        bot-token (:bot-token state)]
    (if-not bot-token
      {:ok false :error "No bot-token configured. Connect with :bot-token to use Web API."}
      (let [channel (or (:channel opts) (:default-channel state))
            _ (when-not channel
                (throw (ex-info "No channel specified" {:opts opts})))
            body (cond-> {:channel channel}
                   (:text opts) (assoc :text (:text opts))
                   (:blocks opts) (assoc :blocks (:blocks opts))
                   (:thread-ts opts) (assoc :thread_ts (:thread-ts opts))
                   (contains? opts :unfurl-links) (assoc :unfurl_links (:unfurl-links opts)))
            result (slack-api-request :post "chat.postMessage" bot-token {:body body})]
        (if (:ok result)
          (do
            (swap! (:state connector) update :message-count (fnil inc 0))
            (log/debug "[slack] Message sent to" channel)
            (select-keys result [:ok :ts :channel :message]))
          (do
            (swap! (:state connector) update :error-count (fnil inc 0))
            (swap! (:state connector) assoc :last-error (:error result))
            (log/warn "[slack] send-message failed:" (:error result))
            {:ok false :error (:error result)}))))))

(defn list-channels
  "List Slack channels the bot has access to via conversations.list.

   Arguments:
     connector - SlackConnector instance (must be connected with :bot-token)
     opts      - Options:
                 :types          - Comma-separated channel types
                                   (default: \"public_channel,private_channel\")
                 :exclude-archived - Exclude archived channels (default: true)
                 :limit          - Max results per page (default: 200, max: 1000)
                 :cursor         - Pagination cursor (optional)

   Returns map with:
     :ok       - Boolean success
     :channels - Vector of channel maps with :id, :name, :topic, :purpose, :num-members
     :cursor   - Next page cursor (nil if no more pages)
     :error    - Error string if failed"
  [connector opts]
  (let [state @(:state connector)
        bot-token (:bot-token state)]
    (if-not bot-token
      {:ok false :error "No bot-token configured. Connect with :bot-token to use Web API."}
      (let [params (cond-> {:types (or (:types opts) "public_channel,private_channel")
                            :exclude_archived (if (contains? opts :exclude-archived)
                                                (:exclude-archived opts)
                                                true)
                            :limit (or (:limit opts) 200)}
                     (:cursor opts) (assoc :cursor (:cursor opts)))
            result (slack-api-request :get "conversations.list" bot-token {:params params})]
        (if (:ok result)
          {:ok true
           :channels (mapv (fn [ch]
                             {:id (:id ch)
                              :name (:name ch)
                              :topic (get-in ch [:topic :value])
                              :purpose (get-in ch [:purpose :value])
                              :num-members (:num_members ch)
                              :is-archived (:is_archived ch)})
                           (:channels result))
           :cursor (get-in result [:response_metadata :next_cursor])}
          {:ok false :error (:error result)})))))

(defn read-history
  "Read message history from a Slack channel via conversations.history.

   Arguments:
     connector - SlackConnector instance (must be connected with :bot-token)
     opts      - Options:
                 :channel  - Channel ID (e.g., \"C1234567890\") (required)
                 :limit    - Number of messages to return (default: 20, max: 1000)
                 :oldest   - Unix timestamp, only messages after this (optional)
                 :latest   - Unix timestamp, only messages before this (optional)
                 :cursor   - Pagination cursor (optional)
                 :inclusive - Include messages with oldest/latest timestamps (default: false)

   Returns map with:
     :ok       - Boolean success
     :messages - Vector of message maps with :text, :user, :ts, :type, :thread-ts
     :has-more - Boolean, whether more messages exist
     :cursor   - Next page cursor (nil if no more)
     :error    - Error string if failed"
  [connector opts]
  (let [state @(:state connector)
        bot-token (:bot-token state)]
    (if-not bot-token
      {:ok false :error "No bot-token configured. Connect with :bot-token to use Web API."}
      (if-not (:channel opts)
        {:ok false :error "Missing required :channel option"}
        (let [params (cond-> {:channel (:channel opts)
                              :limit (or (:limit opts) 20)}
                       (:oldest opts) (assoc :oldest (:oldest opts))
                       (:latest opts) (assoc :latest (:latest opts))
                       (:cursor opts) (assoc :cursor (:cursor opts))
                       (:inclusive opts) (assoc :inclusive (:inclusive opts)))
              result (slack-api-request :get "conversations.history" bot-token {:params params})]
          (if (:ok result)
            {:ok true
             :messages (mapv (fn [msg]
                               {:text (:text msg)
                                :user (:user msg)
                                :ts (:ts msg)
                                :type (:type msg)
                                :thread-ts (:thread_ts msg)
                                :reply-count (:reply_count msg)})
                             (:messages result))
             :has-more (:has_more result)
             :cursor (get-in result [:response_metadata :next_cursor])}
            {:ok false :error (:error result)}))))))

;; =============================================================================
;; Auth Test (used by connect! and health-check)
;; =============================================================================

(defn- auth-test
  "Test bot token validity via auth.test endpoint.
   Returns {:ok true :user-id ... :team ...} or {:ok false :error ...}."
  [bot-token]
  (let [result (slack-api-request :post "auth.test" bot-token {:body {}})]
    (if (:ok result)
      {:ok true
       :user-id (:user_id result)
       :team (:team result)
       :team-id (:team_id result)
       :bot-id (:bot_id result)
       :url (:url result)}
      {:ok false :error (or (:error result) "auth.test failed")})))

;; =============================================================================
;; SlackConnector Implementation
;; =============================================================================

(defrecord SlackConnector [id state]
  connector/IConnector

  (connector-id [_] id)

  (connector-info [_]
    {:id id
     :name "Slack Connector"
     :description "Slack integration via Web API and Webhooks for hivemind"
     :version "2.0.0"
     :system-type :api
     :capabilities #{:read :write :subscribe}})

  (connect! [this opts]
    (if (:connected? @state)
      {:success? false
       :connection this
       :errors ["Already connected"]
       :metadata {}}
      (let [bot-token (:bot-token opts)
            webhook-url (:webhook-url opts)]
        ;; Need at least one of bot-token or webhook-url
        (if (and (nil? bot-token) (nil? webhook-url))
          {:success? false
           :connection nil
           :errors ["At least one of :bot-token or :webhook-url is required"]
           :metadata {}}
          (let [;; Validate bot token if provided
                auth-result (when bot-token (auth-test bot-token))
                token-ok? (or (nil? bot-token) (:ok auth-result))]
            (if-not token-ok?
              {:success? false
               :connection nil
               :errors [(str "Bot token auth failed: " (:error auth-result))]
               :metadata {}}
              (let [config (merge default-config
                                  (select-keys opts [:channel :username :icon-emoji
                                                     :events :format :timeout-ms]))
                    ;; Start webhook event listener if webhook-url provided
                    subscriptions (when webhook-url
                                    (start-event-listener! webhook-url config state))]
                (reset! state
                        {:connected? true
                         :bot-token bot-token
                         :webhook-url webhook-url
                         :default-channel (:channel opts)
                         :config config
                         :subscriptions subscriptions
                         :auth-info auth-result
                         :connected-at (Instant/now)
                         :message-count 0
                         :error-count 0
                         :last-error nil})
                (log/info "[slack] Connected:" id
                          (cond-> {}
                            bot-token (assoc :api true :team (:team auth-result))
                            webhook-url (assoc :webhook true)))
                {:success? true
                 :connection this
                 :errors []
                 :metadata (cond-> {:capabilities (cond-> #{}
                                                    bot-token (conj :api :read :write)
                                                    webhook-url (conj :webhook :subscribe))}
                             auth-result (merge (select-keys auth-result
                                                             [:team :user-id :bot-id])))})))))))

  (disconnect! [_]
    (if-not (:connected? @state)
      {:success? true :errors []}
      (do
        ;; Stop event listener
        (when-let [subs (:subscriptions @state)]
          (stop-event-listener! subs))
        ;; Send goodbye via webhook if available
        (when-let [url (:webhook-url @state)]
          (post-to-webhook! url
                            {:text ":wave: *Hivemind Slack Connector* disconnected"}
                            (:config @state)))
        (reset! state {:connected? false})
        (log/info "[slack] Disconnected:" id)
        {:success? true :errors []})))

  (connected? [_]
    (boolean (:connected? @state)))

  (health-check [_]
    (if-not (:connected? @state)
      {:healthy? false
       :latency-ms nil
       :errors ["Not connected"]
       :checked-at (Instant/now)}
      (let [bot-token (:bot-token @state)
            start-ms (System/currentTimeMillis)]
        (if bot-token
          ;; Active health check via auth.test
          (let [result (auth-test bot-token)
                latency (- (System/currentTimeMillis) start-ms)]
            {:healthy? (:ok result)
             :latency-ms latency
             :errors (if (:ok result) [] [(:error result)])
             :checked-at (Instant/now)})
          ;; Passive check - just verify config is present
          (let [healthy? (boolean (:webhook-url @state))]
            {:healthy? healthy?
             :latency-ms (- (System/currentTimeMillis) start-ms)
             :errors (if healthy? [] ["No webhook-url or bot-token configured"])
             :checked-at (Instant/now)})))))

  (reconnect! [this opts]
    (connector/disconnect! this)
    (connector/connect! this opts))

  (get-status [_]
    (let [s @state]
      {:id id
       :connected? (boolean (:connected? s))
       :last-connected (:connected-at s)
       :last-error (:last-error s)
       :error-count (or (:error-count s) 0)
       :uptime-ms (when (and (:connected? s) (:connected-at s))
                    (- (System/currentTimeMillis)
                       (.toEpochMilli ^Instant (:connected-at s))))
       :metrics {:message-count (or (:message-count s) 0)
                 :events-subscribed (count (get-in s [:config :events]))
                 :has-bot-token? (boolean (:bot-token s))
                 :has-webhook? (boolean (:webhook-url s))
                 :team (get-in s [:auth-info :team])}}))

  ;; -- IConnector extended methods (send/receive/sync!) --

  (send [this data opts]
    (let [s @state
          bot-token (:bot-token s)]
      (if-not (:connected? s)
        {:success? false :response nil :errors ["Not connected"] :sent-at (Instant/now)}
        (if-not bot-token
          {:success? false :response nil
           :errors ["No bot-token configured for Web API send"]
           :sent-at (Instant/now)}
          (let [channel (or (:channel opts) (:default-channel s))
                text (cond
                       (string? data) data
                       (map? data) (or (:text data) (json/write-str data))
                       :else (str data))
                body (cond-> {:channel channel :text text}
                       (:thread-ts opts) (assoc :thread_ts (:thread-ts opts))
                       (:blocks data) (assoc :blocks (:blocks data)))
                result (slack-api-request :post "chat.postMessage" bot-token {:body body})]
            (if (:ok result)
              (do
                (swap! state update :message-count (fnil inc 0))
                {:success? true
                 :response {:ts (:ts result) :channel (:channel result)}
                 :errors []
                 :sent-at (Instant/now)})
              (do
                (swap! state update :error-count (fnil inc 0))
                (swap! state assoc :last-error (:error result))
                {:success? false
                 :response nil
                 :errors [(:error result)]
                 :sent-at (Instant/now)})))))))

  (receive [this opts]
    (let [s @state
          bot-token (:bot-token s)]
      (if-not (:connected? s)
        {:success? false :data [] :count 0 :errors ["Not connected"] :has-more? false}
        (if-not bot-token
          {:success? false :data [] :count 0
           :errors ["No bot-token configured for Web API receive"]
           :has-more? false}
          (let [channel (or (:channel opts) (:default-channel s))
                params (cond-> {:channel channel
                                :limit (or (:limit opts) 100)}
                         (:since opts) (assoc :oldest (:since opts)))
                result (slack-api-request :get "conversations.history" bot-token {:params params})]
            (if (:ok result)
              (let [messages (mapv (fn [msg]
                                     {:text (:text msg)
                                      :user (:user msg)
                                      :ts (:ts msg)
                                      :type (:type msg)
                                      :thread-ts (:thread_ts msg)})
                                   (:messages result))]
                {:success? true
                 :data messages
                 :count (count messages)
                 :errors []
                 :has-more? (boolean (:has_more result))})
              {:success? false :data [] :count 0
               :errors [(:error result)] :has-more? false}))))))

  (sync! [this opts]
    ;; Slack is not a bidirectional sync target. Provide a reasonable implementation
    ;; that does inbound (receive) or outbound (send) based on :direction.
    (let [direction (or (:direction opts) :inbound)]
      (case direction
        :inbound
        (let [recv-result (connector/receive this opts)]
          {:success? (:success? recv-result)
           :sent 0
           :received (:count recv-result)
           :conflicts []
           :resolved 0
           :errors (:errors recv-result)
           :synced-at (Instant/now)})

        :outbound
        (if-let [data (:data opts)]
          (let [send-result (connector/send this data opts)]
            {:success? (:success? send-result)
             :sent (if (:success? send-result) 1 0)
             :received 0
             :conflicts []
             :resolved 0
             :errors (:errors send-result)
             :synced-at (Instant/now)})
          {:success? false :sent 0 :received 0 :conflicts []
           :resolved 0 :errors ["No :data provided for outbound sync"]
           :synced-at (Instant/now)})

        ;; :bidirectional - do receive only (Slack doesn't have "pull then push" semantics)
        (let [recv-result (connector/receive this opts)]
          {:success? (:success? recv-result)
           :sent 0
           :received (:count recv-result)
           :conflicts []
           :resolved 0
           :errors (:errors recv-result)
           :synced-at (Instant/now)})))))

;; =============================================================================
;; Constructor
;; =============================================================================

(defn ->slack-connector
  "Create a new SlackConnector instance.

   Arguments:
     id - Keyword identifier for this connector (default: :hivemind-slack)

   Returns:
     SlackConnector instance (not yet connected).
     Call connect! with {:bot-token \"xoxb-...\"} and/or {:webhook-url \"...\"} to activate.

   Examples:
     (->slack-connector)
     (->slack-connector :my-slack)"
  ([] (->slack-connector :hivemind-slack))
  ([id] (->SlackConnector id (atom {:connected? false}))))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn start-slack-connector!
  "Convenience function to create, register, and connect a SlackConnector.

   Arguments:
     opts - Config map with at least one of:
            :bot-token   - Slack Bot User OAuth Token (xoxb-...)
            :webhook-url - Slack Incoming Webhook URL
            + any connect! options

   Returns:
     Connected SlackConnector instance, registered in connector registry."
  [opts]
  (let [connector (->slack-connector (or (:id opts) :hivemind-slack))
        connect-opts (dissoc opts :id)]
    (connector/register-connector! connector)
    (let [result (connector/connect! connector connect-opts)]
      (if (:success? result)
        connector
        (do
          (connector/unregister-connector! (connector/connector-id connector))
          (throw (ex-info "Failed to connect SlackConnector"
                          {:errors (:errors result)})))))))

(defn stop-slack-connector!
  "Stop and unregister SlackConnector.

   Arguments:
     id - Connector ID (default: :hivemind-slack)"
  ([] (stop-slack-connector! :hivemind-slack))
  ([id]
   (when-let [connector (connector/get-connector id)]
     (connector/disconnect! connector)
     (connector/unregister-connector! id))))

(comment
  ;; Development REPL examples

  ;; Create and connect with bot token (Web API)
  (def slack (start-slack-connector!
              {:bot-token "xoxb-YOUR-BOT-TOKEN"
               :channel "#hivemind"}))

  ;; Send a message
  (send-message slack {:channel "#general" :text "Hello from hivemind!"})

  ;; List channels
  (list-channels slack {:types "public_channel"})

  ;; Read history
  (read-history slack {:channel "C1234567890" :limit 5})

  ;; IConnector protocol send/receive
  (connector/send slack "Hello!" {:channel "#general"})
  (connector/receive slack {:channel "C123" :limit 10})

  ;; With webhook event forwarding too
  (def slack2 (start-slack-connector!
               {:bot-token "xoxb-YOUR-BOT-TOKEN"
                :webhook-url "https://hooks.slack.com/services/YOUR/WEBHOOK/URL"
                :channel "#hivemind"
                :events #{:hivemind-completed :hivemind-error}
                :format :detailed}))

  ;; Check status
  (connector/get-status slack)
  (connector/health-check slack)

  ;; Stop
  (stop-slack-connector!))
