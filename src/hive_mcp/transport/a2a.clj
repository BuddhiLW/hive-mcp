(ns hive-mcp.transport.a2a
  "A2A (Agent-to-Agent) protocol gateway.

   Thin JSON-RPC facade over existing IAgent protocol. Enables
   external agent frameworks (LangChain, CrewAI, Google ADK)
   to communicate with hive-mcp agents via the A2A standard.

   Architecture: Aleph HTTP server (same pattern as transport/olympus.clj)
   with JSON-RPC 2.0 dispatch, AgentCard endpoint, and SSE streaming.

   Spec: https://google.github.io/A2A/

   from transport/olympus.clj, dispatch patterns from tools/agent/dispatch.clj."
  (:require [aleph.http :as http]
            [aleph.netty :as netty]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.config :as config]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.transport.a2a.schema :as schema]
            [hive-mcp.transport.a2a.handlers :as handlers]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private server-atom (atom nil))
(defonce ^:private sse-clients (atom #{}))

;; =============================================================================
;; AgentCard Builder
;; =============================================================================

(defn- build-agent-card
  "Build A2A AgentCard from live DataScript state.
   Queries active agents and maps to A2A skills."
  []
  (try
    (let [slaves (ds-queries/get-all-slaves)
          active-agents (->> slaves
                             (filter #(#{:idle :working :spawning :blocked}
                                       (:slave/status %)))
                             (map (fn [s]
                                    {:id (:slave/id s)
                                     :name (or (:slave/name s) (:slave/id s))
                                     :description (str (name (or (:slave/status s) :unknown))
                                                       " agent"
                                                       (when-let [p (:slave/project-id s)]
                                                         (str " in " p)))}))
                             vec)
          skills (mapv (fn [a]
                         {:id (:id a)
                          :name (:name a)
                          :description (:description a)})
                       active-agents)]
      {:name "hive-mcp"
       :description "Hive-MCP multi-agent coordinator with knowledge graph, memory, and orchestration"
       :url (str "http://localhost:" (or (:port @server-atom) 7912))
       :version "0.1.0"
       :capabilities {:streaming true
                      :pushNotifications false
                      :stateTransitionHistory false}
       :authentication {:schemes []}
       :defaultInputModes ["text"]
       :defaultOutputModes ["text"]
       :skills skills})
    (catch Exception e
      (log/warn "Failed to build AgentCard:" (.getMessage e))
      {:name "hive-mcp"
       :description "Hive-MCP (error building card)"
       :capabilities {:streaming false}
       :skills []})))

;; =============================================================================
;; API Key Verification
;; =============================================================================

(defn- verify-api-key
  "Check Authorization: Bearer <key> header.
   Returns true if api-key is nil (no auth) or key matches."
  [req api-key]
  (if (nil? api-key)
    true
    (let [auth-header (get-in req [:headers "authorization"] "")]
      (= auth-header (str "Bearer " api-key)))))

;; =============================================================================
;; JSON-RPC Dispatcher
;; =============================================================================

(defn- parse-jsonrpc-request
  "Parse JSON-RPC 2.0 request from body string.
   Returns parsed map or error response."
  [body-str]
  (try
    (let [parsed (json/read-str body-str :key-fn keyword)]
      (if (and (:method parsed) (:jsonrpc parsed))
        parsed
        {:error {:code (:invalid-request schema/error-codes)
                 :message "Missing required JSON-RPC fields"}}))
    (catch Exception _
      {:error {:code (:parse-error schema/error-codes)
               :message "Invalid JSON"}})))

(defn- dispatch-jsonrpc
  "Route JSON-RPC method to handler.
   Returns JSON-RPC response map."
  [request]
  (let [{:keys [id method params error]} request]
    (if error
      (schema/jsonrpc-error id (:code error) (:message error))
      (let [handler-result
            (case method
              "tasks/send"
              (handlers/handle-send-message params)

              "tasks/get"
              (handlers/handle-get-task params)

              "tasks/cancel"
              (handlers/handle-cancel-task params)

              "tasks/sendSubscribe"
              (handlers/handle-send-streaming-message params)

              ;; Legacy method names (some A2A clients use these)
              "SendMessage"
              (handlers/handle-send-message params)

              "GetTask"
              (handlers/handle-get-task params)

              "CancelTask"
              (handlers/handle-cancel-task params)

              "SendStreamingMessage"
              (handlers/handle-send-streaming-message params)

              ;; Unknown method
              {:error {:code (:method-not-found schema/error-codes)
                       :message (str "Unknown method: " method)}})
            {:keys [result error stream?]} handler-result]
        (if error
          (schema/jsonrpc-error id (:code error) (:message error))
          (schema/jsonrpc-success id result))))))

;; =============================================================================
;; SSE Stream Handler
;; =============================================================================

(defn- handle-sse-stream
  "Set up SSE stream for a task.
   Subscribes to channel events and pushes TaskStatusUpdateEvents."
  [task-id]
  (try
    (let [subscribe! (requiring-resolve 'hive-mcp.channel.core/subscribe!)
          unsubscribe! (requiring-resolve 'hive-mcp.channel.core/unsubscribe!)
          event-ch (subscribe! :task-completed)
          output (s/stream 32)]
      (swap! sse-clients conj output)
      ;; Send initial connected event
      (s/put! output (str "data: " (json/write-str
                                    (schema/make-status-update-event
                                     task-id "working" "Connected to SSE stream"))
                          "\n\n"))
      ;; Background loop: forward matching channel events to SSE
      (future
        (try
          (loop []
            (when-let [event (async/<!! event-ch)]
              (let [event-task-id (or (:task-id event) (:id event))]
                (when (= (str event-task-id) (str task-id))
                  (let [state (if (:error event) "failed" "completed")
                        msg (or (:result event) (:error event) "done")
                        sse-data (schema/make-status-update-event
                                  task-id state (str msg))]
                    (when-not (s/closed? output)
                      @(s/put! output (str "data: " (json/write-str sse-data) "\n\n")))
                    ;; Terminal state — close stream
                    (when (#{"completed" "failed" "canceled"} state)
                      (s/close! output)))))
              (when-not (s/closed? output)
                (recur))))
          (catch Exception e
            (log/debug "SSE stream error for task" task-id ":" (.getMessage e)))
          (finally
            (unsubscribe! :task-completed event-ch)
            (swap! sse-clients disj output)
            (when-not (s/closed? output)
              (s/close! output)))))
      ;; Return SSE response
      {:status 200
       :headers {"Content-Type" "text/event-stream"
                 "Cache-Control" "no-cache"
                 "Connection" "keep-alive"
                 "Access-Control-Allow-Origin" "*"}
       :body (s/->source output)})
    (catch Exception e
      (log/warn "Failed to set up SSE stream:" (.getMessage e))
      {:status 500
       :headers {"Content-Type" "application/json"
                 "Access-Control-Allow-Origin" "*"}
       :body (json/write-str {:error "SSE setup failed"})})))

;; =============================================================================
;; HTTP Handler
;; =============================================================================

(defn- json-response
  "Helper to create JSON HTTP response with CORS headers."
  [status data]
  {:status status
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
             "Access-Control-Allow-Headers" "Content-Type, Authorization"}
   :body (json/write-str data)})

(defn- read-body
  "Read request body to string."
  [req]
  (when-let [body (:body req)]
    (cond
      (string? body) body
      (instance? java.io.InputStream body) (slurp body)
      :else (str body))))

(defn- make-http-handler
  "Build Ring handler for A2A gateway.

   Routes:
     GET  /.well-known/agent.json  -> AgentCard
     POST /                        -> JSON-RPC dispatch
     GET  /sse/:task-id            -> SSE stream
     GET  /health                  -> Health check
     OPTIONS *                     -> CORS preflight"
  [api-key]
  (fn [req]
    (let [uri (:uri req)
          method (:request-method req)]
      (cond
        ;; CORS preflight
        (= method :options)
        {:status 204
         :headers {"Access-Control-Allow-Origin" "*"
                   "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
                   "Access-Control-Allow-Headers" "Content-Type, Authorization"
                   "Access-Control-Max-Age" "86400"}}

        ;; AgentCard
        (and (= method :get) (= uri "/.well-known/agent.json"))
        (json-response 200 (build-agent-card))

        ;; Health check
        (and (= method :get) (= uri "/health"))
        (json-response 200 {:status "healthy"
                            :service "a2a-gateway"
                            :sse-clients (count @sse-clients)
                            :timestamp (System/currentTimeMillis)})

        ;; SSE stream for task
        (and (= method :get) (str/starts-with? uri "/sse/"))
        (let [task-id (subs uri 5)]
          (if (empty? task-id)
            (json-response 400 {:error "task-id required in path"})
            (handle-sse-stream task-id)))

        ;; JSON-RPC endpoint
        (and (= method :post) (= uri "/"))
        (if (verify-api-key req api-key)
          (let [body-str (read-body req)]
            (if (empty? body-str)
              (json-response 400 (schema/jsonrpc-error nil
                                                       (:parse-error schema/error-codes)
                                                       "Empty request body"))
              (let [request (parse-jsonrpc-request body-str)]
                (if (:error request)
                  (json-response 400 (schema/jsonrpc-error nil
                                                           (get-in request [:error :code])
                                                           (get-in request [:error :message])))
                  (let [response (dispatch-jsonrpc request)]
                    (json-response 200 response))))))
          (json-response 401 (schema/jsonrpc-error nil
                                                   (:unauthorized schema/error-codes)
                                                   "Invalid or missing API key")))

        ;; Catch-all
        :else
        {:status 405
         :headers {"Content-Type" "text/plain"
                   "Access-Control-Allow-Origin" "*"}
         :body "Method not allowed"}))))

;; =============================================================================
;; Public API - Server Lifecycle
;; =============================================================================

(defn start!
  "Start A2A gateway HTTP server.

   Options:
     :port    - Port number (default: 7912)
     :api-key - API key for Bearer auth (nil = no auth)

   Returns the actual port number."
  ([] (start! {}))
  ([{:keys [port api-key]}]
   (let [port (or port 7912)]
     (if @server-atom
       (do
         (log/warn "A2A gateway already running on port" (:port @server-atom))
         (:port @server-atom))
       (try
         (let [handler (make-http-handler api-key)
               server (http/start-server handler {:port port})
               actual-port (netty/port server)]
           (reset! server-atom {:server server
                                :port actual-port
                                :api-key (some? api-key)})
           (log/info "A2A gateway started on port" actual-port
                     (if api-key "(auth enabled)" "(no auth)"))
           actual-port)
         (catch Exception e
           (log/error "Failed to start A2A gateway:" (.getMessage e))
           nil))))))

(defn stop!
  "Stop the A2A gateway and drain SSE streams."
  []
  (doseq [client @sse-clients]
    (when-not (s/closed? client)
      (s/close! client)))
  (reset! sse-clients #{})
  (when-let [{:keys [server port]} @server-atom]
    (.close server)
    (reset! server-atom nil)
    (log/info "A2A gateway stopped (was on port" port ")")
    true))

(defn status
  "Get A2A gateway status."
  []
  {:running? (boolean @server-atom)
   :port (:port @server-atom)
   :clients (count @sse-clients)
   :auth? (:api-key @server-atom false)})

(comment
  ;; REPL testing
  (start! {:port 7913})
  (status)
  (build-agent-card)
  (stop!)

  ;; Test JSON-RPC dispatch
  (dispatch-jsonrpc {:jsonrpc "2.0" :id 1 :method "tasks/get" :params {:id "test-123"}}))
