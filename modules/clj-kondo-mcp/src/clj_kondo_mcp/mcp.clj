(ns clj-kondo-mcp.mcp
  "Minimal MCP (Model Context Protocol) implementation for Babashka.
   Implements JSON-RPC 2.0 over stdio transport."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

;; Protocol version
(def protocol-version "2024-11-05")

;; --- JSON-RPC helpers ---

(defn ->response
  "Create a JSON-RPC response."
  [id result]
  {:jsonrpc "2.0"
   :id id
   :result result})

(defn ->error-response
  "Create a JSON-RPC error response."
  [id code message & [data]]
  {:jsonrpc "2.0"
   :id id
   :error (cond-> {:code code :message message}
            data (assoc :data data))})

(defn ->notification
  "Create a JSON-RPC notification (no id, no response expected)."
  [method params]
  {:jsonrpc "2.0"
   :method method
   :params params})

;; --- Tool schema helpers ---

(defn param->json-schema
  "Convert a parameter definition to JSON Schema."
  [{:keys [name type description required]}]
  [name (cond-> {:type (case type
                         :string "string"
                         :number "number"
                         :integer "integer"
                         :boolean "boolean"
                         :array "array"
                         :object "object"
                         "string")}
          description (assoc :description description))])

(defn tool->mcp-schema
  "Convert a tool definition to MCP tool schema."
  [{:keys [name description parameters]}]
  (let [required-params (->> parameters
                             (filter :required)
                             (map :name)
                             vec)
        properties (into {} (map param->json-schema parameters))]
    {:name name
     :description description
     :inputSchema {:type "object"
                   :properties properties
                   :required required-params}}))

;; --- MCP Server ---

(defrecord MCPServer [name version tools])

(defn make-server
  "Create an MCP server with tools."
  [{:keys [name version tools]}]
  (->MCPServer name version tools))

(defn handle-initialize
  "Handle initialize request."
  [server _params]
  {:protocolVersion protocol-version
   :capabilities {:tools {}}
   :serverInfo {:name (:name server)
                :version (:version server)}})

(defn handle-tools-list
  "Handle tools/list request."
  [server _params]
  {:tools (mapv tool->mcp-schema (vals (:tools server)))})

(defn handle-tools-call
  "Handle tools/call request."
  [server {:keys [name arguments]}]
  (if-let [tool (get (:tools server) (keyword name))]
    (try
      (let [result ((:handler tool) arguments)]
        {:content [{:type "text"
                    :text (if (string? result)
                            result
                            (json/generate-string result {:pretty true}))}]})
      (catch Exception e
        {:content [{:type "text"
                    :text (str "Error: " (.getMessage e))}]
         :isError true}))
    {:content [{:type "text"
                :text (str "Unknown tool: " name)}]
     :isError true}))

(defn handle-request
  "Route a request to the appropriate handler."
  [server {:keys [id method params]}]
  (let [result (case method
                 "initialize" (handle-initialize server params)
                 "notifications/initialized" nil ;; notification, no response
                 "tools/list" (handle-tools-list server params)
                 "tools/call" (handle-tools-call server params)
                 "ping" {} ;; pong
                 ;; Unknown method
                 {:error {:code -32601 :message (str "Unknown method: " method)}})]
    (when (and id result)
      (->response id result))))

;; --- Stdio Transport ---

(defn read-message
  "Read a JSON-RPC message from stdin."
  []
  (when-let [line (read-line)]
    (when-not (str/blank? line)
      (try
        (json/parse-string line true)
        (catch Exception e
          (binding [*out* *err*]
            (println "Parse error:" (.getMessage e) "Line:" line))
          nil)))))

(defn write-message
  "Write a JSON-RPC message to stdout."
  [msg]
  (println (json/generate-string msg))
  (flush))

(defn log-stderr
  "Log a message to stderr for debugging."
  [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(defn run-server!
  "Run the MCP server, reading from stdin and writing to stdout."
  [server]
  (log-stderr "Starting" (:name server) "v" (:version server))
  (loop []
    (when-let [msg (read-message)]
      (log-stderr "Received:" (:method msg))
      (when-let [response (handle-request server msg)]
        (log-stderr "Responding to:" (:method msg))
        (write-message response))
      (recur)))
  (log-stderr "Server shutting down"))

;; --- Tool definition DSL ---

(defn defparam
  "Define a tool parameter."
  [name type description & {:keys [required] :or {required true}}]
  {:name (clojure.core/name name)
   :type type
   :description description
   :required required})

(defn deftool
  "Define a tool with name, description, parameters, and handler."
  [tool-name description parameters handler]
  {:name (clojure.core/name tool-name)
   :description description
   :parameters parameters
   :handler handler})
