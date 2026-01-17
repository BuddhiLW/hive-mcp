(ns hive-mcp.tools.health
  "MCP health check tools for connection verification.

   Provides a lightweight tool for lings to verify MCP connectivity
   after hot-reload or other events that might break connections.

   CLARITY: Yield safe failure - health checks are non-destructive."
  (:require [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.channel.websocket :as ws-channel]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Handler: mcp_health
;; =============================================================================

(defn handle-mcp-health
  "Health check for MCP connection.

   Returns status information about:
   - MCP server status (always 'healthy' if this runs)
   - WebSocket channel connectivity
   - Timestamp

   Lings should call this after receiving :mcp-health-restored event
   or when they suspect MCP connection issues.

   Arguments:
     args - Optional map (currently ignored)

   Returns:
     {:status \"healthy\"
      :mcp {:status \"connected\"}
      :websocket-channel {:connected bool :clients N}
      :timestamp N}"
  [_args]
  (try
    (let [ws-status (ws-channel/status)
          result {:status "healthy"
                  :mcp {:status "connected"
                        :message "MCP server is operational"}
                  :websocket-channel {:connected (:connected? ws-status)
                                      :clients (:clients ws-status)
                                      :running (:running? ws-status)}
                  :timestamp (System/currentTimeMillis)}]
      (log/debug "Health check performed:" result)
      (mcp-json result))
    (catch Exception e
      (log/error "Health check failed:" (.getMessage e))
      (mcp-json {:status "degraded"
                 :error (.getMessage e)
                 :timestamp (System/currentTimeMillis)}))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  "Health check MCP tool definitions."
  [{:name "mcp_health"
    :description "Health check for MCP connection. Returns server status, WebSocket channel info, and timestamp. Use after hot-reload or to verify connectivity."
    :inputSchema {:type "object" :properties {}}
    :handler handle-mcp-health}])
