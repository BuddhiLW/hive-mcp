(ns hive-mcp.server.transport
  "Network server management: nREPL, WebSocket, Channel.

   Bounded context: MCP protocol handling and network transport.

   Manages:
   - Embedded nREPL server (for bb-mcp tool forwarding)
   - WebSocket MCP server (Claude Code IDE integration)
   - WebSocket channel with auto-healing (hivemind events)
   - Olympus WebSocket server (Olympus Web UI)
   - Legacy TCP channel (deprecated, backward compat)"
  (:require [nrepl.server :as nrepl-server]
            [hive-mcp.config.core :as config]
            [hive-mcp.transport.websocket :as ws]
            [hive-mcp.transport.olympus :as olympus-ws]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.channel.websocket :as ws-channel]
            [clojure.core.async :as async]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; nREPL Server
;; =============================================================================

(defn start-embedded-nrepl!
  "Start an embedded nREPL server for bb-mcp tool forwarding.

   CRITICAL: This runs in the SAME JVM as the MCP server and channel,
   allowing bb-mcp to forward tool calls that access the live channel.

   Without this, bb-mcp connects to a separate nREPL JVM that has no
   channel server running, so hivemind broadcasts go nowhere.

   Retries up to 5 times with 2s backoff to handle TIME_WAIT from kill -9.

   Parameters:
     nrepl-server-atom - atom to store nREPL server reference for shutdown"
  [nrepl-server-atom]
  (let [nrepl-port (config/get-service-value :nrepl :port
                                             :env "HIVE_MCP_NREPL_PORT"
                                             :parse parse-long
                                             :default 7910)
        middleware (try
                     (require 'cider.nrepl)
                     (let [mw-var (resolve 'cider.nrepl/cider-middleware)]
                       (when mw-var @mw-var))
                     (catch Exception _ nil))
        handler (if (seq middleware)
                  (apply nrepl-server/default-handler middleware)
                  (nrepl-server/default-handler))
        server-opts {:port nrepl-port :bind "127.0.0.1" :handler handler}
        max-retries 5
        retry-delay-ms 2000]
    (loop [attempt 1]
      (let [result (try
                     (let [server (nrepl-server/start-server server-opts)]
                       (reset! nrepl-server-atom server)
                       (log/info "Embedded nREPL started on port" nrepl-port
                                 (if middleware "(with CIDER middleware)" "(basic)")
                                 (when (> attempt 1) (str "(attempt " attempt ")")))
                       server)
                     (catch java.net.BindException e
                       (if (< attempt max-retries)
                         (do
                           (log/warn "nREPL port" nrepl-port "busy (attempt" (str attempt "/" max-retries "),")
                                     "retrying in" (str retry-delay-ms "ms..."))
                           ::retry)
                         (do
                           (log/error "nREPL failed to bind port" nrepl-port "after" max-retries "attempts:" (.getMessage e))
                           nil)))
                     (catch Exception e
                       (log/warn "Embedded nREPL failed to start (non-fatal):" (.getMessage e))
                       nil))]
        (if (= result ::retry)
          (do (Thread/sleep retry-delay-ms)
              (recur (inc attempt)))
          result)))))

;; =============================================================================
;; WebSocket MCP Server
;; =============================================================================

(defn start-websocket-server!
  "Start WebSocket MCP server if enabled via config or HIVE_MCP_WEBSOCKET=true."
  []
  (when (config/get-service-value :websocket :enabled
                                  :env "HIVE_MCP_WEBSOCKET"
                                  :parse #(= "true" %)
                                  :default false)
    (let [port (config/get-service-value :websocket :port
                                         :env "HIVE_MCP_WS_PORT"
                                         :parse parse-long)
          project-dir (or (:project-dir (config/get-service-config :websocket))
                          (System/getenv "HIVE_MCP_PROJECT_DIR"))]
      (log/info "Starting WebSocket MCP server" {:port port :project-dir project-dir})
      (ws/start-server! {:port port
                         :project-dir project-dir}))))

;; =============================================================================
;; WebSocket Channel with Auto-Healing
;; =============================================================================

(defn start-ws-channel-with-healing!
  "Start WebSocket channel server with auto-healing.

   Runs a background async loop that monitors and restarts if needed.

   Parameters:
     ws-channel-monitor - atom to store the monitoring go-loop channel"
  [ws-channel-monitor]
  (let [port (config/get-service-value :ws-channel :port
                                       :env "HIVE_MCP_WS_CHANNEL_PORT"
                                       :parse parse-long
                                       :default 9999)
        check-interval-ms 30000] ; Check every 30 seconds
    ;; Start initial server
    (try
      (ws-channel/start! {:port port})
      (log/info "WebSocket channel server started on port" port)
      (catch Exception e
        (log/warn "WebSocket channel initial start failed:" (.getMessage e))))
    ;; Start monitoring loop
    (when-not @ws-channel-monitor
      (reset! ws-channel-monitor
              (async/go-loop []
                (async/<! (async/timeout check-interval-ms))
                (when-not (ws-channel/connected?)
                  (log/info "WebSocket channel: no clients, server healthy"))
                  ;; Server running but no clients is fine

                (when-not (:running? (ws-channel/status))
                  (log/warn "WebSocket channel server died, attempting restart...")
                  (try
                    (ws-channel/start! {:port port})
                    (log/info "WebSocket channel server restarted on port" port)
                    (catch Exception e
                      (log/error "WebSocket channel restart failed:" (.getMessage e)))))
                (recur)))
      (log/info "WebSocket channel auto-heal monitor started"))))

;; =============================================================================
;; Olympus WebSocket Server
;; =============================================================================

(defn start-olympus-ws!
  "Start Olympus WebSocket server for Olympus Web UI (port 7911).
   Sends full snapshot on connect, supports typed event protocol."
  []
  (try
    (olympus-ws/start!)
    (olympus-ws/wire-hivemind-events!)
    (log/info "Olympus WebSocket server started on port 7911")
    (catch Exception e
      (log/warn "Olympus WebSocket server failed to start (non-fatal):" (.getMessage e)))))

;; =============================================================================
;; A2A Protocol Gateway
;; =============================================================================

(defn start-a2a-gateway!
  "Start A2A JSON-RPC gateway for external agent interoperability.
   Opt-in via config :a2a :enabled or HIVE_MCP_A2A_ENABLED=true."
  []
  (when (config/get-service-value :a2a :enabled
                                  :env "HIVE_MCP_A2A_ENABLED"
                                  :parse #(= "true" %)
                                  :default false)
    (try
      (require 'hive-mcp.transport.a2a)
      ((resolve 'hive-mcp.transport.a2a/start!)
       {:port (config/get-service-value :a2a :port
                                        :env "HIVE_MCP_A2A_PORT"
                                        :parse parse-long
                                        :default 7912)
        :api-key (config/get-service-value :a2a :api-key
                                           :env "HIVE_MCP_A2A_API_KEY")})
      (log/info "A2A gateway started")
      (catch Exception e
        (log/warn "A2A gateway failed to start (non-fatal):" (.getMessage e))))))

;; =============================================================================
;; Legacy Channel (deprecated)
;; =============================================================================

(defn start-legacy-channel!
  "Start legacy bidirectional channel server (deprecated - kept for backward compat).
   Marks coordinator as running to protect from test fixture cleanup."
  []
  (let [channel-port (config/get-service-value :channel :port
                                               :env "HIVE_MCP_CHANNEL_PORT"
                                               :parse parse-long
                                               :default 9998)]
    (try
      (channel/start-server! {:type :tcp :port channel-port})
      ;; Mark coordinator as running to protect from test fixture cleanup
      ;; the production server when tests run in the same JVM
      (channel/mark-coordinator-running!)
      (log/info "Legacy channel server started on TCP port" channel-port)
      (catch Exception e
        (log/warn "Legacy channel server failed to start (non-fatal):" (.getMessage e))))))
