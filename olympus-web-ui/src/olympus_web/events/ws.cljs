(ns olympus-web.events.ws
  "WebSocket connection events for hive-mcp channel.
   
   Handles:
   - Connection lifecycle (connect, disconnect, reconnect)
   - Message parsing and dispatch to appropriate handlers
   - Heartbeat/keepalive"
  (:require [re-frame.core :as rf]
            [olympus-web.config :as config]))

;; =============================================================================
;; WebSocket State
;; =============================================================================

(defonce ws-instance (atom nil))

;; =============================================================================
;; Connection Events
;; =============================================================================

(rf/reg-event-fx
 :ws/connect
 (fn [{:keys [db]} [_ url]]
   (let [ws-url (or url (:url (:connection db)) config/ws-url)]
     (when-let [old-ws @ws-instance]
       (.close old-ws))
     {:db (-> db
              (assoc-in [:connection :status] :connecting)
              (assoc-in [:connection :url] ws-url)
              (assoc-in [:connection :last-error] nil))
      :ws/open {:url ws-url}})))

(rf/reg-event-db
 :ws/connected
 (fn [db _]
   (-> db
       (assoc-in [:connection :status] :connected)
       (assoc-in [:connection :reconnect-attempts] 0))))

(rf/reg-event-fx
 :ws/disconnected
 (fn [{:keys [db]} [_ reason]]
   (let [attempts (get-in db [:connection :reconnect-attempts] 0)]
     (reset! ws-instance nil)
     (if (< attempts config/ws-max-reconnect-attempts)
       {:db (-> db
                (assoc-in [:connection :status] :disconnected)
                (update-in [:connection :reconnect-attempts] inc))
        :dispatch-later [{:ms config/ws-reconnect-delay-ms
                          :dispatch [:ws/connect]}]}
       {:db (-> db
                (assoc-in [:connection :status] :disconnected)
                (assoc-in [:connection :last-error] (str "Max reconnect attempts reached: " reason)))}))))

(rf/reg-event-db
 :ws/error
 (fn [db [_ error]]
   (assoc-in db [:connection :last-error] (str error))))

;; =============================================================================
;; Message Handling
;; =============================================================================

(rf/reg-event-fx
 :ws/message-received
 (fn [{:keys [db]} [_ raw-message]]
   (try
     (let [msg (js->clj (js/JSON.parse raw-message) :keywordize-keys true)
           msg-type (keyword (:type msg))]
       ;; Dispatch to specific handler based on message type
       (case msg-type
         ;; === Initial Snapshot (sent by Olympus WS on connect) ===
         :init-snapshot
         {:dispatch [:ws/init-snapshot-received msg]}

         ;; === Hivemind Shout Events (from Olympus protocol) ===
         :hivemind-shout
         {:dispatch [:hivemind/event-received
                     {:agent-id (:agent-id msg)
                      :event-type (keyword (:event-type msg))
                      :message (:message msg)
                      :task (:task msg)
                      :timestamp (:timestamp msg)}]}

         ;; Legacy hivemind event types (direct format)
         (:started :progress :completed :error :blocked)
         {:dispatch [:hivemind/event-received msg]}

         ;; === Agent Lifecycle Events ===
         :agent-spawned {:dispatch [:agents/spawned (:agent msg)]}
         :agent-killed {:dispatch [:agents/killed (:agent msg)]}
         :agent-status {:dispatch [:agents/status-updated (:agent msg)]}

         ;; === Snapshot Response Events (from request-snapshot) ===
         :agents {:dispatch [:agents/set-all (:data msg)]}
         :waves {:dispatch [:waves/set-all (vals (:data msg))]}
         :kg-snapshot {:dispatch [:kg/set-all (get-in msg [:data :entries])]}
         :project-tree {:dispatch [:projects/set-all (:data msg)]}

         ;; === Wave Events ===
         :wave-started {:dispatch [:waves/started msg]}
         :wave-dispatched {:dispatch [:waves/started msg]}
         :wave-task-update {:dispatch [:waves/task-updated msg]}
         :wave-task-updated {:dispatch [:waves/task-updated msg]}
         :wave-completed {:dispatch [:waves/completed msg]}

         ;; === KG/Memory Events ===
         :memory-added {:dispatch [:kg/entry-added msg]}
         :memory-updated {:dispatch [:kg/entry-updated msg]}
         :kg-entry-added {:dispatch [:kg/entry-added (:entry msg)]}
         :kg-edge-added {:dispatch [:kg/edge-added msg]}

         ;; === Agora Dialogue Events ===
         :agora/created {:dispatch [:agora/dialogue-created msg]}
         :agora/participant-joined {:dispatch [:agora/participant-joined msg]}

         ;; === Ling Output Streaming Events ===
         :ling-output-stream
         {:dispatch [:terminal/output-received msg]}

         ;; === Command Response Events (from olympus-server commands) ===
         :command-response
         (let [cmd (:command msg)]
           (cond
             ;; ling-output/get response -> history
             (and (= cmd "ling-output/get") (:success msg))
             {:dispatch [:terminal/history-received (:data msg)]}

             ;; ling-output/subscribe confirmation
             (and (= cmd "ling-output/subscribe") (:success msg))
             {:dispatch [:terminal/subscription-confirmed (:data msg)]}

             ;; ling-output/list response
             (and (= cmd "ling-output/list") (:success msg))
             {:dispatch [:terminal/available-lings-received
                         (get-in msg [:data :headless-lings])]}

             :else {}))

         ;; === System Events ===
         :mcp-health-restored {:dispatch [:system/health-restored msg]}
         :pong {} ;; Keepalive response - ignore silently

         ;; Default: Unknown message type
         {:dispatch [:ws/unknown-message msg]}))
     (catch :default e
       (js/console.warn "Failed to parse WS message:" e raw-message)
       {:db db}))))

;; =============================================================================
;; Init Snapshot Handler (processes full state on connect)
;; =============================================================================

(rf/reg-event-fx
 :ws/init-snapshot-received
 (fn [{:keys [db]} [_ {:keys [data timestamp]}]]
   (js/console.log "Received init snapshot at" timestamp
                   "- agents:" (count (:agents data))
                   "waves:" (count (:waves data))
                   "kg entries:" (get-in data [:kg :entry-count])
                   "projects:" (get-in data [:project-tree :total]))
   {:dispatch-n [;; Load all agents
                 [:agents/set-all (:agents data)]
                 ;; Load all waves (convert map to seq)
                 [:waves/set-all (vals (:waves data))]
                 ;; Load KG entries
                 [:kg/set-all (get-in data [:kg :entries])]
                 ;; Load project tree (HCR Wave 5)
                 [:projects/set-all (:project-tree data)]]
    :db (assoc-in db [:connection :last-snapshot] timestamp)}))

(rf/reg-event-db
 :ws/unknown-message
 (fn [db [_ msg]]
   (js/console.log "Unknown WS message type:" (:type msg) msg)
   db))

;; =============================================================================
;; WebSocket Effect Handler
;; =============================================================================

(rf/reg-fx
 :ws/open
 (fn [{:keys [url]}]
   (try
     (let [ws (js/WebSocket. url)]
       (reset! ws-instance ws)

       (set! (.-onopen ws)
             (fn [_]
               (js/console.log "WebSocket connected to" url)
               (rf/dispatch [:ws/connected])))

       (set! (.-onclose ws)
             (fn [event]
               (js/console.log "WebSocket closed:" (.-reason event))
               (rf/dispatch [:ws/disconnected (.-reason event)])))

       (set! (.-onerror ws)
             (fn [error]
               (js/console.error "WebSocket error:" error)
               (rf/dispatch [:ws/error error])))

       (set! (.-onmessage ws)
             (fn [event]
               (let [data (.-data event)]
                 ;; Handle pong responses silently
                 (when-not (= data "pong")
                   (rf/dispatch [:ws/message-received data]))))))
     (catch :default e
       (js/console.error "Failed to create WebSocket:" e)
       (rf/dispatch [:ws/error e])))))

;; =============================================================================
;; Keepalive (ping/pong)
;; =============================================================================

(rf/reg-event-fx
 :ws/send-ping
 (fn [_ _]
   (when-let [ws @ws-instance]
     (when (= (.-readyState ws) 1) ; OPEN
       (.send ws "ping")))
   {}))

;; Start keepalive interval
(defonce keepalive-interval
  (js/setInterval #(rf/dispatch [:ws/send-ping]) 30000))
