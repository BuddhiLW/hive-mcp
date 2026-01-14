(ns hive-mcp.channel.websocket
  "WebSocket-based channel for Emacs communication.
   
   Uses Aleph (Netty) for robust async networking.
   Follows CLARITY: Composition over modification - leverage battle-tested libraries.
   
   Usage:
     (start! {:port 9999})
     (broadcast! {:type :hivemind-progress :data {...}})
     (stop!)"
  (:require [aleph.http :as http]
            [aleph.netty :as netty]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private server-atom (atom nil))
(defonce ^:private clients (atom #{}))

;; =============================================================================
;; WebSocket Handler  
;; =============================================================================

(defn- handle-message [client-id msg]
  (log/debug "Received from" client-id ":" msg)
  ;; Route incoming messages to internal event system if needed
  (when-let [handler (:on-message @server-atom)]
    (handler msg client-id)))

(defn- ws-handler [req]
  ;; Note: Don't use server-side heartbeats - websocket.el doesn't handle them well
  ;; Instead rely on client-side keepalive or message-based heartbeat
  (d/let-flow [socket (http/websocket-connection req)]
              (let [client-id (str "ws-" (System/currentTimeMillis) "-" (rand-int 10000))]
                (log/info "WebSocket client connected:" client-id)
                (swap! clients conj socket)

      ;; Handle incoming messages (including ping/pong)
                (s/consume (fn [raw]
                             (cond
                     ;; Handle ping - respond with pong
                               (= raw "ping")
                               (s/put! socket "pong")

                     ;; Handle JSON messages
                               :else
                               (when-let [msg (try
                                                (json/read-str raw :key-fn keyword)
                                                (catch Exception e
                                                  (log/debug "Non-JSON message:" raw)
                                                  nil))]
                                 (handle-message client-id msg))))
                           socket)

      ;; Cleanup on disconnect  
                (s/on-closed socket
                             (fn []
                               (log/info "WebSocket client disconnected:" client-id)
                               (swap! clients disj socket)))

      ;; Return the socket for the response
                socket)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn start!
  "Start WebSocket channel server.
   
   Options:
     :port - Port number (default: 9999)
     :on-message - Handler fn for incoming messages (fn [msg client-id])
   
   Returns the actual port number."
  [{:keys [port on-message] :or {port 9999}}]
  (if @server-atom
    (do
      (log/warn "WebSocket channel already running on port" (:port @server-atom))
      (:port @server-atom))
    (let [server (http/start-server ws-handler {:port port})
          actual-port (netty/port server)]
      (reset! server-atom {:server server
                           :port actual-port
                           :on-message on-message})
      (log/info "WebSocket channel started on port" actual-port)
      actual-port)))

(defn stop!
  "Stop the WebSocket channel server."
  []
  (when-let [{:keys [server port]} @server-atom]
    (.close server)
    (reset! server-atom nil)
    (reset! clients #{})
    (log/info "WebSocket channel stopped (was on port" port ")")
    true))

(defn broadcast!
  "Broadcast message to all connected WebSocket clients.
   Message is JSON-encoded before sending."
  [msg]
  (let [json-msg (json/write-str msg)
        active-clients @clients]
    (when (seq active-clients)
      (log/debug "Broadcasting to" (count active-clients) "clients:" (:type msg))
      (doseq [client active-clients]
        (when-not (s/closed? client)
          (d/catch
           (s/put! client json-msg)
           (fn [e]
             (log/warn "Broadcast failed:" (.getMessage e))
             (swap! clients disj client))))))))

(defn connected?
  "Returns true if at least one client is connected."
  []
  (boolean (seq @clients)))

(defn client-count
  "Returns number of connected clients."
  []
  (count @clients))

(defn status
  "Returns channel status map."
  []
  {:running? (boolean @server-atom)
   :port (:port @server-atom)
   :clients (count @clients)
   :connected? (connected?)})

;; =============================================================================
;; Convenience - emit hivemind events
;; =============================================================================

(defn emit!
  "Emit a typed event to all connected clients.
   Adds timestamp automatically."
  [event-type data]
  (broadcast! (merge {:type (name event-type)
                      :timestamp (System/currentTimeMillis)}
                     data)))
