(ns hive-mcp.server.sse
  "MCP SSE transport — standalone daemon mode (M2).

   Replaces stdio transport with HTTP-based Server-Sent Events:
   - GET  /sse     → SSE stream (server→client JSON-RPC messages)
   - POST /message → JSON-RPC requests (client→server)

   Architecture:
   The same jsonrpc4clj ChanServer used by stdio, but backed by
   HTTP channels instead of stdin/stdout. This decouples hive-mcp
   from Emacs process lifecycle — no parent process needed.

   Subject to MCP SSE transport spec:
   1. Client connects GET /sse
   2. Server sends 'endpoint' event with POST URL
   3. Client sends JSON-RPC via POST /message
   4. Server streams responses via SSE

   Usage:
     (start-sse-server! {:port 8808})  ; non-blocking
     (stop-sse-server!)                ; graceful shutdown"

  (:require [aleph.http :as http]
            [babashka.json :as json]
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            [clojure.core.async :as async]
            [io.modelcontext.clojure-sdk.server :as sdk-server]
            [jsonrpc4clj.server :as jsonrpc-server]
            [manifold.stream :as ms]
            [manifold.deferred :as md]
            [taoensso.timbre :as log])
  (:import [java.util UUID]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private server-atom (atom nil))
(defonce ^:private input-ch-atom (atom nil))
(defonce ^:private output-ch-atom (atom nil))
(defonce ^:private sse-clients (atom {}))  ;; session-id -> manifold sink stream
(defonce ^:private mcp-context-atom (atom nil))

;; =============================================================================
;; JSON Helpers (match SDK wire format)
;; =============================================================================

(defn- kw->camelCase [k]
  (cond-> k (keyword? k) csk/->camelCaseString))

(defn- serialize-msg
  "Serialize a Clojure map to JSON string in MCP wire format (camelCase keys)."
  [msg]
  (json/write-str (cske/transform-keys kw->camelCase msg)))

(defn- parse-msg
  "Parse JSON string to Clojure map."
  [body-str]
  (json/read-str body-str))

;; =============================================================================
;; SSE Output Pump
;; =============================================================================

(defn- sse-event
  "Format a Server-Sent Event string."
  ([event-name data-str]
   (str "event: " event-name "\ndata: " data-str "\n\n"))
  ([data-str]
   (str "data: " data-str "\n\n")))

(defn- start-output-pump!
  "Background loop: takes from output-ch, serializes, and pushes to all SSE clients.
   This is the server→client direction of the MCP protocol."
  [output-ch]
  (async/go-loop []
    (when-let [msg (async/<! output-ch)]
      (let [json-str (serialize-msg msg)
            event-str (sse-event "message" json-str)
            dead-clients (atom [])]
        ;; Fan out to all connected SSE clients
        (doseq [[session-id sink] @sse-clients]
          (let [result (md/timeout! (ms/try-put! sink event-str) 5000 ::timeout)]
            (when (or (= @result ::timeout) (false? @result))
              (swap! dead-clients conj session-id))))
        ;; Clean up dead clients
        (doseq [session-id @dead-clients]
          (log/info "[SSE] Removing dead client:" session-id)
          (swap! sse-clients dissoc session-id)))
      (recur))))

;; =============================================================================
;; Ring Handlers
;; =============================================================================

(defn- handle-sse-connect
  "Handle GET /sse — establish SSE stream for a client session.

   MCP SSE spec:
   1. Send 'endpoint' event with POST URL for client to send messages
   2. Keep connection alive with SSE events from output-ch"
  [req]
  (let [session-id (str (UUID/randomUUID))
        sink (ms/stream 64)]
    ;; Register this client
    (swap! sse-clients assoc session-id sink)
    (log/info "[SSE] Client connected:" session-id)

    ;; Send endpoint event first (MCP SSE spec)
    (let [port (get-in req [:server-port] 8808)
          endpoint-url (str "/message?sessionId=" session-id)]
      (ms/put! sink (sse-event "endpoint" endpoint-url)))

    ;; Return SSE response
    {:status 200
     :headers {"Content-Type" "text/event-stream"
               "Cache-Control" "no-cache"
               "Connection" "keep-alive"
               "X-Session-Id" session-id}
     :body (ms/->source sink)}))

(defn- handle-message
  "Handle POST /message — receive JSON-RPC message from client.

   Parses JSON body and puts on input-ch for ChanServer processing."
  [req]
  (try
    (let [body (slurp (:body req))
          msg (parse-msg body)
          input-ch @input-ch-atom]
      (if input-ch
        (do
          (async/>!! input-ch msg)
          {:status 202
           :headers {"Content-Type" "application/json"}
           :body (json/write-str {"status" "accepted"})})
        {:status 503
         :headers {"Content-Type" "application/json"}
         :body (json/write-str {"error" "Server not initialized"})}))
    (catch Exception e
      (log/warn "[SSE] Failed to process message:" (.getMessage e))
      {:status 400
       :headers {"Content-Type" "application/json"}
       :body (json/write-str {"error" (.getMessage e)})})))

(defn- handle-health
  "Handle GET /health — daemon health check."
  [_req]
  (let [client-count (count @sse-clients)]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/write-str {"status" "ok"
                            "transport" "sse"
                            "clients" client-count
                            "server" "hive-mcp"})}))

(defn- ring-handler
  "Ring handler routing requests to SSE/message/health endpoints."
  [req]
  (let [method (:request-method req)
        path (:uri req)]
    (cond
      (and (= method :get) (= path "/sse"))
      (handle-sse-connect req)

      (and (= method :post) (= path "/message"))
      (handle-message req)

      (and (= method :get) (= path "/health"))
      (handle-health req)

      :else
      {:status 404
       :headers {"Content-Type" "application/json"}
       :body (json/write-str {"error" "Not found"
                              "hint" "Use GET /sse for SSE stream, POST /message for JSON-RPC"})})))

;; =============================================================================
;; Server Lifecycle
;; =============================================================================

(defn start-sse-server!
  "Start the MCP SSE transport server.

   Creates:
   1. core.async input-ch + output-ch (same as stdio transport)
   2. jsonrpc4clj ChanServer backed by those channels
   3. Aleph HTTP server with SSE/message/health routes
   4. Output pump that forwards ChanServer output to SSE clients

   Options:
     :port     - HTTP port (default: 8808)
     :spec     - MCP server spec (from routes/build-server-spec)

   Returns the Aleph server instance."
  [{:keys [port spec]
    :or {port 8808}}]
  (when @server-atom
    (log/warn "[SSE] Server already running, stopping first...")
    (stop-sse-server!))

  (let [input-ch (async/chan 128)
        output-ch (async/chan 128)
        log-ch (async/chan (async/sliding-buffer 20))

        ;; Create ChanServer — same abstraction as stdio, different channels
        server (jsonrpc-server/chan-server {:input-ch input-ch
                                            :output-ch output-ch
                                            :log-ch log-ch})

        ;; Create MCP context and start JSON-RPC processing
        context (assoc (sdk-server/create-context! spec) :server server)
        join (jsonrpc-server/start server context)

        ;; Start HTTP server with Aleph
        http-server (http/start-server ring-handler {:port port})]

    ;; Store state
    (reset! input-ch-atom input-ch)
    (reset! output-ch-atom output-ch)
    (reset! mcp-context-atom context)
    (reset! server-atom http-server)

    ;; Start output pump (ChanServer → SSE clients)
    (start-output-pump! output-ch)

    (log/info "[SSE] MCP SSE transport started on port" port)
    (log/info "[SSE] Endpoints: GET /sse, POST /message, GET /health")

    {:server http-server
     :context context
     :join join
     :port port}))

(defn stop-sse-server!
  "Stop the MCP SSE transport server. Graceful shutdown."
  []
  (when-let [server @server-atom]
    ;; Close all SSE client streams
    (doseq [[session-id sink] @sse-clients]
      (ms/close! sink)
      (log/debug "[SSE] Closed client:" session-id))
    (reset! sse-clients {})

    ;; Close channels (triggers ChanServer pipeline cleanup)
    (when-let [input-ch @input-ch-atom]
      (async/close! input-ch))
    (when-let [output-ch @output-ch-atom]
      (async/close! output-ch))

    ;; Stop HTTP server
    (.close ^java.io.Closeable server)

    ;; Reset state
    (reset! server-atom nil)
    (reset! input-ch-atom nil)
    (reset! output-ch-atom nil)
    (reset! mcp-context-atom nil)

    (log/info "[SSE] MCP SSE transport stopped")))

(defn sse-running?
  "Check if the SSE transport is running."
  []
  (some? @server-atom))

(defn sse-status
  "Get SSE transport status."
  []
  {:running? (sse-running?)
   :clients (count @sse-clients)
   :client-sessions (vec (keys @sse-clients))})
