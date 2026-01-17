(ns hive-mcp.channel.websocket
  "WebSocket-based channel for Emacs communication.

   Uses Aleph (Netty) for robust async networking.
   Follows CLARITY: Composition over modification - leverage battle-tested libraries.

   Usage:
     (start! {:port 9999})
     (broadcast! {:type :hivemind-progress :data {...}})
     (stop!)

   Also serves /metrics endpoint for Prometheus scraping (CLARITY-T: Telemetry first)."
  (:require [aleph.http :as http]
            [aleph.netty :as netty]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private server-atom (atom nil))
(defonce ^:private clients (atom #{}))

;; =============================================================================
;; HTTP Routes (CLARITY-T: Telemetry first - /metrics endpoint)
;; =============================================================================

(defn- metrics-handler
  "Handle GET /metrics requests for Prometheus scraping.
   Dynamically loads prometheus namespace to avoid circular deps."
  [_req]
  (try
    (require 'hive-mcp.telemetry.prometheus)
    (let [handler (resolve 'hive-mcp.telemetry.prometheus/metrics-handler)]
      (handler nil))
    (catch Exception e
      (log/warn "Prometheus metrics unavailable:" (.getMessage e))
      {:status 503
       :headers {"Content-Type" "text/plain"}
       :body "Prometheus metrics not initialized"})))

(defn- health-handler
  "Handle GET /health requests for health checks."
  [_req]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/write-str {:status "healthy"
                          :websocket {:clients (count @clients)
                                      :running? (boolean @server-atom)}
                          :timestamp (System/currentTimeMillis)})})

;; =============================================================================
;; WebSocket Handler
;; =============================================================================

(defn- handle-message [client-id msg]
  (log/debug "Received from" client-id ":" msg)
  ;; Route incoming messages to internal event system if needed
  (when-let [handler (:on-message @server-atom)]
    (handler msg client-id)))

(defn- ws-connection-handler
  "Handle WebSocket connection upgrade.
   Note: Don't use server-side heartbeats - websocket.el doesn't handle them well.
   Instead rely on client-side keepalive or message-based heartbeat."
  [req]
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
                                                (catch Exception _e
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

(defn- hybrid-handler
  "Hybrid HTTP/WebSocket handler.
   Routes:
     GET /metrics -> Prometheus metrics
     GET /health  -> Health check
     *            -> WebSocket upgrade"
  [req]
  (let [uri (:uri req)
        method (:request-method req)]
    (cond
      ;; Prometheus metrics endpoint
      (and (= method :get) (= uri "/metrics"))
      (metrics-handler req)

      ;; Health check endpoint
      (and (= method :get) (= uri "/health"))
      (health-handler req)

      ;; WebSocket upgrade (default)
      :else
      (ws-connection-handler req))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn start!
  "Start WebSocket channel server with HTTP endpoints.

   Options:
     :port - Port number (default: 9999)
     :on-message - Handler fn for incoming messages (fn [msg client-id])

   HTTP Routes:
     GET /metrics - Prometheus metrics (text/plain)
     GET /health  - Health check (application/json)
     *            - WebSocket upgrade

   Returns the actual port number."
  [{:keys [port on-message] :or {port 9999}}]
  (if @server-atom
    (do
      (log/warn "WebSocket channel already running on port" (:port @server-atom))
      (:port @server-atom))
    (let [server (http/start-server hybrid-handler {:port port})
          actual-port (netty/port server)]
      (reset! server-atom {:server server
                           :port actual-port
                           :on-message on-message})
      (log/info "WebSocket channel started on port" actual-port
                "(endpoints: /metrics, /health, WebSocket)")
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
  (let [port (:port @server-atom)]
    {:running? (boolean @server-atom)
     :port port
     :clients (count @clients)
     :connected? (connected?)
     :endpoints (when port
                  {:metrics (str "http://localhost:" port "/metrics")
                   :health (str "http://localhost:" port "/health")})}))

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
