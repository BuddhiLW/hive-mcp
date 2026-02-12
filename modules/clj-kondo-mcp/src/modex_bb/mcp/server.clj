(ns modex-bb.mcp.server
  "Handles JSON RPC interface and dispatches to an MCP server.
   Babashka-compatible version using cheshire for JSON."
  (:require [modex-bb.mcp.log :as log]
            [cheshire.core :as json]
            [modex-bb.mcp.protocols :as mcp :refer [AServer]]
            [modex-bb.mcp.schema :as schema]
            [modex-bb.mcp.json-rpc :as json-rpc]
            [modex-bb.mcp.tools :as tools]))

(defn format-tool-results
  "Format a tool result into the expected JSON-RPC text response format."
  [results]
  (let [content-type "text"]
    {:content (vec (for [result results]
                     {:type content-type
                      :text (str result)}))
     :isError false}))

(defn format-tool-errors
  [errors]
  (-> (format-tool-results errors)
      (assoc :isError true)))

(defn ->server
  "Returns a reified instance of AServer (an MCP Server),
  given tools, resources and prompts. Only tools are supported at this time."
  [{:keys [protocol-version
           name version initialize
           tools resources prompts
           on-receive
           on-send
           enqueue-notification]
    :or   {protocol-version schema/latest-protocol-version
           initialize       (fn [_init-params])}}]
  (reify AServer
    (protocol-version [_this] protocol-version)

    (server-name [_this] name)
    (version [_this] version)

    (capabilities [_this]
      {:tools     {:listChanged (boolean (seq tools))}
       :resources {:listChanged (boolean (seq resources))}
       :prompts   {:listChanged (boolean (seq prompts))}})

    (on-receive [_this msg]
      (when on-receive (on-receive msg)))

    (on-send [_this msg]
      (when on-send (on-send msg)))

    (enqueue-notification [_this msg]
      (when enqueue-notification
        (enqueue-notification msg)))

    (initialize [_this init-params]
      (initialize init-params))

    (list-tools [_this]
      (->> (vals tools)
           (mapv tools/tool->json-schema)))

    (list-resources [_this] [])
    (list-prompts [_this] [])

    (call-tool
      [_this tool-name arg-map]
      (let [tool-key (keyword tool-name)
            tool     (if tool-key (get tools tool-key))]
        (if-not tool
          (throw (ex-info (str "Unknown tool: " (str tool-name))
                          {:code      schema/error-invalid-params
                           :cause     :tool.exception/missing-tool
                           :tool/name tool-name}))
          (tools/invoke-tool tool arg-map))))))

(defn read-json-rpc-message
  "Reads a JSON value from stdin and coerces map string keys to keywords.
  Returns parse error on JSON parse exception."
  []
  (try
    (when-let [line (read-line)]
      (log/debug "Received message:" line)
      (json/parse-string line true))  ; true = keyword keys
    (catch Exception e
      (log/debug "Error parsing message:" (ex-message e))
      {:error {:code    schema/error-parse
               :message "Parse error"}})))

(defn write-json-rpc!
  "Writes JSON to stdout with locking for thread safety.
  Returns string written on success, nil otherwise."
  [message]
  (try
    (let [json-str (json/generate-string message)]
      (log/debug "Server Sending message:" json-str)
      (locking *out*
        (println json-str)
        (flush))
      json-str)
    (catch Exception e
      (log/debug "Error writing message:" (ex-message e))
      nil)))

(defn handle-tool-call-request
  "Handles tools/call request and invokes tool. Returns JSON-RPC result or error."
  [server {:as _request :keys [id params]}]
  (log/debug "Handling tools/call request (ID " id ") with params:" (pr-str params))
  (let [{tool-name :name
         arg-map   :arguments} params]
    (try
      (let [{:keys [success results errors]} (mcp/call-tool server tool-name arg-map)]
        (if success
          (->> (format-tool-results results)
               (json-rpc/result id))
          (do
            (log/debug "Tool Error:" errors)
            (->> (format-tool-errors errors)
                 (json-rpc/result id)))))
      (catch Exception ex
        (log/debug "handle-tool-call-request exception:" ex)
        (let [err-data (ex-data ex)]
          (json-rpc/error id {:code    (:code err-data)
                              :message (ex-message ex)}))))))

(defn handle-request
  "Router for AServer - dispatches to appropriate handler based on method."
  [mcp-server, {:as request :keys [id method params]} & [send-notification]]
  (try
    (case method
      "ping" (do
               (log/debug "Handling ping request with id:" id)
               (json-rpc/result id {}))

      "tools/call" (handle-tool-call-request mcp-server request)

      "initialize" (let [init-response {:protocolVersion (mcp/protocol-version mcp-server)
                                        :capabilities    (mcp/capabilities mcp-server)
                                        :serverInfo      {:name    (mcp/server-name mcp-server)
                                                          :version (mcp/version mcp-server)}}]
                     (let [inited-notification (json-rpc/method "notifications/initialized")]
                       (mcp/enqueue-notification mcp-server inited-notification)
                       (future
                         (log/warn 'initialize)
                         (try
                           (mcp/initialize mcp-server params)
                           (send-notification inited-notification)
                           (catch Exception ex
                             (log/error "MCP Server initialize failed:" (ex-message ex))))))

                     (json-rpc/result id init-response))

      "tools/list" (json-rpc/result id {:tools (mcp/list-tools mcp-server)})
      "prompts/list" (do
                       (log/debug "Handling prompts/list request with id:" id)
                       (json-rpc/result id {:prompts (mcp/list-prompts mcp-server)}))
      "resources/list" (do
                         (log/debug "Handling resources/list request with id:" id)
                         (json-rpc/result id {:resources (mcp/list-resources mcp-server)}))
      (do
        (log/debug "Unknown method:" method)
        (json-rpc/error id {:code    schema/error-method-not-found
                            :message (str "Method not found: " method)})))
    (catch Exception e
      (log/error "Error handling request:" (ex-message e))
      (json-rpc/error id {:code    schema/error-internal
                          :message (str "Internal error: " (ex-message e))}))))

(defn notification?
  "Notifications have method, but no id."
  [{:as _message :keys [method id]}]
  (and method (not id)))

(defn handle-notification
  "We don't need to do anything special for notifications right now."
  [{:as _notification :keys [method]}]
  (log/debug "Received notification:" method)
  nil)

(defn handle-message
  "Returns a JSON-RPC message."
  [server message send-notification]
  (try
    (log/debug "Handling message:" message)
    (cond
      (:error message)
      (log/debug "Error message:" message)

      (and (:method message) (:id message))
      (handle-request server message send-notification)

      (notification? message)
      (do
        (log/debug "Handling notification:" (:method message))
        (handle-notification message))

      :else
      (do (log/warn "Unknown message type:" message)
          nil))
    (catch Exception e
      (log/error "Critical error handling message:" (ex-message e))
      (if-let [id (:id message)]
        (json-rpc/error id {:code    schema/error-internal
                            :message (str "Internal error: " (ex-message e))})
        nil))))

(defn start-server!
  "Main server loop - reads from stdin, writes to stdout."
  [server]
  (log/debug "Starting Modex MCP server (Babashka)...")
  (try
    (let [send-notification-handler (fn [message] (write-json-rpc! message))]
      (loop []
        (log/debug "Waiting for request...")
        (let [message (read-json-rpc-message)]
          (mcp/on-receive server message)

          (if-not message
            (log/debug "Reader returned nil, client probably disconnected")

            (do (future
                  (let [?response (handle-message server message send-notification-handler)]
                    (when ?response
                      (log/debug "Responding with messages:" (pr-str ?response))
                      (mcp/on-send server ?response)
                      (write-json-rpc! ?response))))
                (recur))))))

    (log/debug "Exiting.")
    (catch Exception e
      (log/error "Critical error in server:" (ex-message e)))))
