(ns hive-mcp.server
  "MCP server for Emacs interaction via emacsclient."
  (:require [io.modelcontext.clojure-sdk.stdio-server :as io-server]
            [hive-mcp.tools :as tools]
            [hive-mcp.docs :as docs]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.channel :as channel]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.embeddings.ollama :as ollama]
            [nrepl.server :as nrepl-server]
            [taoensso.timbre :as log])
  (:gen-class))

;; Store nREPL server reference for shutdown
(defonce ^:private nrepl-server-atom (atom nil))

;; Configure Timbre to write to stderr instead of stdout
;; This is CRITICAL for MCP servers - stdout is the JSON-RPC channel
(log/merge-config!
 {:appenders
  {:println {:enabled? true
             :async? false
             :fn (fn [data]
                   (let [{:keys [output_]} data]
                     (binding [*out* *err*]
                       (println (force output_)))))}}})

;; Convert our tool definitions to the SDK format
(defn make-tool
  "Convert a tool definition with :handler to SDK format."
  [{:keys [name description inputSchema handler]}]
  {:name name
   :description description
   :inputSchema inputSchema
   :handler handler})

(def emacs-server-spec
  {:name "hive-mcp"
   :version "0.1.0"
   ;; hivemind/tools already included in tools/tools aggregation
   :tools (mapv make-tool (concat tools/tools docs/docs-tools))})

(defn init-embedding-provider!
  "Initialize the embedding provider for semantic memory search.
  Attempts to configure Ollama embeddings for Chroma.
  Fails gracefully if Ollama or Chroma are not available.
  
  Configuration via environment variables:
    CHROMA_HOST - Chroma server host (default: localhost)
    CHROMA_PORT - Chroma server port (default: 8000)
    OLLAMA_HOST - Ollama server URL (default: http://localhost:11434)"
  []
  (try
    ;; Configure Chroma connection - read from env or use defaults
    (let [chroma-host (or (System/getenv "CHROMA_HOST") "localhost")
          chroma-port (or (some-> (System/getenv "CHROMA_PORT") Integer/parseInt) 8000)]
      (chroma/configure! {:host chroma-host :port chroma-port})
      (log/info "Chroma configured:" chroma-host ":" chroma-port))
    ;; Configure Ollama embedding provider
    (let [ollama-host (or (System/getenv "OLLAMA_HOST") "http://localhost:11434")
          provider (ollama/->provider {:host ollama-host})]
      (chroma/set-embedding-provider! provider)
      (log/info "Embedding provider initialized: Ollama at" ollama-host)
      true)
    (catch Exception e
      (log/warn "Could not initialize embedding provider:"
                (.getMessage e)
                "- Semantic search will be unavailable")
      false)))

(defn start-embedded-nrepl!
  "Start an embedded nREPL server for bb-mcp tool forwarding.

   CRITICAL: This runs in the SAME JVM as the MCP server and channel,
   allowing bb-mcp to forward tool calls that access the live channel.

   Without this, bb-mcp connects to a separate nREPL JVM that has no
   channel server running, so hivemind broadcasts go nowhere."
  []
  (let [nrepl-port (parse-long (or (System/getenv "HIVE_MCP_NREPL_PORT") "7910"))]
    (try
      ;; Try to load cider middleware if available
      (let [middleware (try
                         (require 'cider.nrepl)
                         (let [mw-var (resolve 'cider.nrepl/cider-middleware)]
                           (when mw-var @mw-var))
                         (catch Exception _
                           nil))
            ;; default-handler takes middleware as varargs, use apply
            handler (if (seq middleware)
                      (apply nrepl-server/default-handler middleware)
                      (nrepl-server/default-handler))
            server-opts {:port nrepl-port :bind "127.0.0.1" :handler handler}]
        (let [server (nrepl-server/start-server server-opts)]
          (reset! nrepl-server-atom server)
          (log/info "Embedded nREPL started on port" nrepl-port
                    (if middleware "(with CIDER middleware)" "(basic)"))
          server))
      (catch Exception e
        (log/warn "Embedded nREPL failed to start (non-fatal):" (.getMessage e))
        nil))))

(defn start!
  "Start the MCP server."
  [& _args]
  (let [server-id (random-uuid)]
    (log/info "Starting hive-mcp server:" server-id)
    ;; Start embedded nREPL FIRST - bb-mcp needs this to forward tool calls
    ;; This MUST run in the same JVM as channel server for hivemind to work
    (start-embedded-nrepl!)
    ;; Initialize embedding provider for semantic search (fails gracefully)
    (init-embedding-provider!)
    ;; Start bidirectional channel server for push-based events
    (let [channel-port (parse-long (or (System/getenv "HIVE_MCP_CHANNEL_PORT") "9998"))]
      (try
        (channel/start-server! {:type :tcp :port channel-port})
        (log/info "Channel server started on TCP port" channel-port)
        (catch Exception e
          (log/warn "Channel server failed to start (non-fatal):" (.getMessage e)))))
    ;; Start swarm sync - bridges channel events to logic database
    ;; This enables: task-completed → release claims → process queue
    (try
      (sync/start-sync!)
      (log/info "Swarm sync started - logic database will track swarm state")
      (catch Exception e
        (log/warn "Swarm sync failed to start (non-fatal):" (.getMessage e))))
    @(io-server/run! (assoc emacs-server-spec :server-id server-id))))

(defn -main
  "Entry point for the MCP server."
  [& args]
  (apply start! args))

(comment
  ;; For REPL development
  (start!))
