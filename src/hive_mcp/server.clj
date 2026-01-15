(ns hive-mcp.server
  "MCP server for Emacs interaction via emacsclient."
  (:require [io.modelcontext.clojure-sdk.stdio-server :as io-server]
            [io.modelcontext.clojure-sdk.server :as sdk-server]
            [jsonrpc4clj.server :as jsonrpc-server]
            [hive-mcp.tools :as tools]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.docs :as docs]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.channel :as channel]
            [hive-mcp.channel.websocket :as ws-channel]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.embeddings.ollama :as ollama]
            [hive-mcp.agent :as agent]
            [hive-mcp.transport.websocket :as ws]
            [hive-mcp.hooks :as hooks]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects :as effects]
            [hive-mcp.events.handlers :as ev-handlers]
            [hive-mcp.events.channel-bridge :as channel-bridge]
            [nrepl.server :as nrepl-server]
            [clojure.core.async :as async]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s])
  (:gen-class))

;; Define specs for tool definitions, responses, and hivemind messages
(s/def ::tool-def
  (s/keys :req-un [::name ::description ::inputSchema ::handler]))

(s/def ::name string?)
(s/def ::description string?)
(s/def ::inputSchema map?)
(s/def ::handler fn?)

(s/def ::tool-response
  (s/keys :req-un [::content]))

(s/def ::content (s/coll-of map?))

(s/def ::hivemind-message
  (s/keys :req-un [::agent-id ::event-type ::message]))

(s/def ::agent-id string?)
(s/def ::event-type keyword?)
(s/def ::message string?)

;; Store nREPL server reference for shutdown
(defonce ^:private nrepl-server-atom (atom nil))

;; Store MCP server context for hot-reload capability
;; CLARITY: Telemetry first - expose state for debugging and updates
(defonce ^:private server-context-atom (atom nil))

;; Global hooks registry for event-driven workflows
;; CLARITY: Open for extension - allows runtime hook registration
(defonce ^:private hooks-registry-atom (atom nil))

;; Track if shutdown hook is registered
(defonce ^:private shutdown-hook-registered? (atom false))

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

;; Spec definitions for tool and hivemind message validation
(s/def ::name string?)
(s/def ::description string?)
(s/def ::inputSchema map?)
(s/def ::handler fn?)
(s/def ::tool-def (s/keys :req-un [::name ::description ::inputSchema ::handler]))
(s/def ::tool-response (s/keys :req-un [::name ::description ::inputSchema ::handler]))
(s/def ::agent-id string?)
(s/def ::event-type string?)
(s/def ::message string?)
(s/def ::hivemind-message (s/keys :req-un [::agent-id ::event-type ::message]))

;; =============================================================================
;; SRP Helpers for make-tool
;; =============================================================================

(defn- normalize-content
  "Normalize handler result to content array.
   SRP: Single responsibility for content normalization.
   Handles: sequential (passthrough), map (wrap), other (text wrap)."
  [result]
  (cond
    (sequential? result) (vec result)
    (map? result) [result]
    :else [{:type "text" :text (str result)}]))

(defn- find-last-text-idx
  "Find index of last text-type item in content (searching from end).
   SRP: Single responsibility for text item location.
   Returns nil if no text item found."
  [content]
  (some (fn [[idx item]]
          (when (= "text" (:type item)) idx))
        (map-indexed vector (reverse content))))

(defn- wrap-piggyback
  "Append piggyback messages to content with HIVEMIND delimiters.
   SRP: Single responsibility for piggyback embedding.
   Appends to last text item if exists, otherwise adds new text item.

   Format:
   ---HIVEMIND---
   [{:a \"agent-id\" :e \"event-type\" :m \"message\"}]
   ---/HIVEMIND---"
  [content piggyback]
  (if (and piggyback (seq piggyback))
    (let [piggyback-text (str "\n\n---HIVEMIND---\n"
                              (pr-str piggyback)
                              "\n---/HIVEMIND---")]
      (if-let [last-text-idx (find-last-text-idx content)]
        (let [actual-idx (- (count content) 1 last-text-idx)
              last-item (nth content actual-idx)]
          (assoc content actual-idx
                 (update last-item :text str piggyback-text)))
        (conj content {:type "text" :text piggyback-text})))
    content))

;; =============================================================================
;; Tool Definition Conversion
;; =============================================================================

;; Convert our tool definitions to the SDK format
;; Wraps handlers to append hivemind piggyback messages to response text
(s/fdef make-tool
  :args (s/cat :tool-def ::tool-def)
  :ret ::tool-response)

(defn make-tool
  "Convert a tool definition with :handler to SDK format.
   Wraps handler to attach pending hivemind messages via content embedding.

   Uses SRP helper functions:
   - normalize-content: converts result to content array
   - find-last-text-idx: locates last text item
   - wrap-piggyback: embeds hivemind messages

   Agent ID for piggyback is extracted from args or defaults to 'coordinator'."
  [{:keys [name description inputSchema handler]}]
  (let [wrapped-handler (fn [args]
                          (let [result (handler args)
                                agent-id (or (:agent_id args)
                                             (:agent-id args)
                                             (get args "agent_id")
                                             (get args "agent-id")
                                             "coordinator")
                                _ (require 'hive-mcp.tools.core)
                                piggyback ((resolve 'hive-mcp.tools.core/get-hivemind-piggyback) agent-id)
                                content (normalize-content result)
                                content-with-piggyback (wrap-piggyback content piggyback)]
                            {:content content-with-piggyback}))]
    {:name name
     :description description
     :inputSchema inputSchema
     :handler wrapped-handler}))

(defn build-server-spec
  "Build MCP server spec with capability-based tool filtering.

   MUST be called AFTER init-embedding-provider! to get accurate Chroma status.

   Uses tools/get-filtered-tools for dynamic kanban tool switching:
   - Chroma available → mcp_mem_kanban_* tools
   - Chroma unavailable → org_kanban_native_* tools (fallback)"
  []
  (let [filtered-tools (tools/get-filtered-tools)]
    (log/info "Building server spec with" (count filtered-tools) "tools (capability-filtered)")
    {:name "hive-mcp"
     :version "0.1.0"
     :tools (mapv make-tool (concat filtered-tools docs/docs-tools))}))

;; DEPRECATED: Static spec kept for backward compatibility with tests
;; Prefer build-server-spec for capability-aware tool list
(def emacs-server-spec
  {:name "hive-mcp"
   :version "0.1.0"
   ;; hivemind/tools already included in tools/tools aggregation
   :tools (mapv make-tool (concat tools/tools docs/docs-tools))})

(defn get-server-context
  "Get the current MCP server context (for debugging/hot-reload)."
  []
  @server-context-atom)

(defn refresh-tools!
  "Hot-reload all tools in the running server.
   CLARITY: Open for extension - allows runtime tool updates without restart.

   Uses capability-based filtering - re-checks Chroma availability
   to dynamically switch between mem-kanban and org-kanban-native tools."
  []
  (when-let [context @server-context-atom]
    (let [tools-atom (:tools context)
          filtered-tools (tools/get-filtered-tools)
          new-tools (mapv make-tool (concat filtered-tools docs/docs-tools))]
      ;; Clear and re-register all tools
      (reset! tools-atom {})
      (doseq [tool new-tools]
        (swap! tools-atom assoc (:name tool) {:tool (dissoc tool :handler)
                                              :handler (:handler tool)}))
      (log/info "Hot-reloaded" (count new-tools) "tools (capability-filtered)")
      (count new-tools))))

(defn debug-tool-handler
  "Get info about a registered tool handler (for debugging)."
  [tool-name]
  (when-let [context @server-context-atom]
    (let [tools-atom (:tools context)
          tool-entry (get @tools-atom tool-name)]
      (when tool-entry
        {:name tool-name
         :handler-class (str (type (:handler tool-entry)))
         :tool-keys (keys (:tool tool-entry))}))))

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

(defn start-websocket-server!
  "Start WebSocket MCP server if HIVE_MCP_WEBSOCKET=true."
  []
  (when (= "true" (System/getenv "HIVE_MCP_WEBSOCKET"))
    (let [port (some-> (System/getenv "HIVE_MCP_WS_PORT") parse-long)
          project-dir (System/getenv "HIVE_MCP_PROJECT_DIR")]
      (log/info "Starting WebSocket MCP server" {:port port :project-dir project-dir})
      (ws/start-server! {:port port
                         :project-dir project-dir}))))

(defonce ws-channel-monitor (atom nil))

(defn start-ws-channel-with-healing!
  "Start WebSocket channel server with auto-healing.
   
   CLARITY: Yield safe failure - if server dies, restart it automatically.
   Runs a background async loop that monitors and restarts if needed."
  []
  (let [port (parse-long (or (System/getenv "HIVE_MCP_WS_CHANNEL_PORT") "9999"))
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
                  (log/info "WebSocket channel: no clients, server healthy")
                  ;; Server running but no clients is fine
                  )
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
;; Hooks System Initialization
;; =============================================================================

(defn get-hooks-registry
  "Get the global hooks registry for external registration."
  []
  @hooks-registry-atom)

(defn- trigger-session-end!
  "Trigger session-end hooks for auto-wrap.
   Called by JVM shutdown hook.

   CLARITY: Yield safe failure - errors logged but don't break shutdown."
  [reason]
  (log/info "Triggering session-end hooks:" reason)
  (when-let [registry @hooks-registry-atom]
    (try
      (let [ctx {:reason reason
                 :session (System/currentTimeMillis)
                 :triggered-by "jvm-shutdown"}
            results (hooks/trigger-hooks registry :session-end ctx)]
        (log/info "Session-end hooks completed:" (count results) "handlers executed")
        results)
      (catch Exception e
        (log/error e "Session-end hooks failed (non-fatal)")
        nil))))

(defn- register-shutdown-hook!
  "Register JVM shutdown hook to trigger session-end for auto-wrap.

   Only registers once. Safe to call multiple times.

   CLARITY: Yield safe failure - hook errors don't break JVM shutdown."
  []
  (when-not @shutdown-hook-registered?
    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      (fn []
        (log/info "JVM shutdown detected - running session-end hooks")
        (trigger-session-end! "jvm-shutdown"))))
    (reset! shutdown-hook-registered? true)
    (log/info "JVM shutdown hook registered for auto-wrap")))

(defn init-hooks!
  "Initialize the hooks system and register crystal hooks.

   Creates global registry, registers crystal hooks (auto-wrap),
   and sets up JVM shutdown hook.

   Should be called early in server startup."
  []
  (when-not @hooks-registry-atom
    (let [registry (hooks/create-registry)]
      (reset! hooks-registry-atom registry)
      (log/info "Global hooks registry created")
      ;; Inject registry into sync module for Layer 4 hook wiring
      ;; This enables architectural guarantee of synthetic shouts on task completion
      (sync/set-hooks-registry! registry)
      ;; Register crystal hooks (includes auto-wrap on session-end)
      (crystal-hooks/register-hooks! registry)
      ;; Register JVM shutdown hook to trigger session-end
      (register-shutdown-hook!)
      {:registry registry
       :hooks-registered true})))

(defn start!
  "Start the MCP server."
  [& _args]
  (let [server-id (random-uuid)]
    (log/info "Starting hive-mcp server:" server-id)
    ;; Initialize hooks system FIRST - needed for session-end auto-wrap
    (init-hooks!)
    ;; Initialize hive-events system (re-frame inspired event dispatch)
    ;; EVENTS-01: Event system must init after hooks but before channel
    (try
      (ev/init!)
      (effects/register-effects!)
      (ev-handlers/register-handlers!)
      (log/info "hive-events system initialized")
      (catch Exception e
        (log/warn "hive-events initialization failed (non-fatal):" (.getMessage e))))
    ;; Start embedded nREPL FIRST - bb-mcp needs this to forward tool calls
    ;; This MUST run in the same JVM as channel server for hivemind to work
    (start-embedded-nrepl!)
    ;; Start WebSocket server if enabled (for Claude Code IDE integration)
    (start-websocket-server!)
    ;; Initialize embedding provider for semantic search (fails gracefully)
    (init-embedding-provider!)
    ;; Register tools for agent delegation (allows local models to use MCP tools)
    ;; Uses filtered tools based on Chroma availability
    (let [filtered-tools (tools/get-filtered-tools)]
      (agent/register-tools! filtered-tools)
      (log/info "Registered" (count filtered-tools) "tools for agent delegation (capability-filtered)"))
    ;; Start WebSocket channel with auto-healing (primary - Aleph/Netty based)
    ;; This is the reliable push channel for hivemind events
    (start-ws-channel-with-healing!)
    ;; Start legacy bidirectional channel server (deprecated - kept for backwards compat)
    (let [channel-port (parse-long (or (System/getenv "HIVE_MCP_CHANNEL_PORT") "9998"))]
      (try
        (channel/start-server! {:type :tcp :port channel-port})
        (log/info "Legacy channel server started on TCP port" channel-port)
        (catch Exception e
          (log/warn "Legacy channel server failed to start (non-fatal):" (.getMessage e)))))
    ;; Initialize channel bridge - wires channel events to hive-events dispatch
    ;; EVENTS-01: Must init after both channel server and event system
    (try
      (channel-bridge/init!)
      (log/info "Channel bridge initialized - channel events will dispatch to hive-events")
      (catch Exception e
        (log/warn "Channel bridge initialization failed (non-fatal):" (.getMessage e))))
    ;; Start swarm sync - bridges channel events to logic database
    ;; This enables: task-completed → release claims → process queue
    (try
      (sync/start-sync!)
      (log/info "Swarm sync started - logic database will track swarm state")
      (catch Exception e
        (log/warn "Swarm sync failed to start (non-fatal):" (.getMessage e))))
    ;; Start lings registry sync - keeps Clojure registry in sync with elisp
    ;; ADR-001: Event-driven sync for lings_available to return accurate counts
    (try
      (swarm/start-registry-sync!)
      (log/info "Lings registry sync started - lings_available will track elisp lings")
      (catch Exception e
        (log/warn "Lings registry sync failed to start (non-fatal):" (.getMessage e))))
    ;; Start MCP server - create context ourselves to enable hot-reload
    ;; CLARITY: Telemetry first - expose state for debugging
    ;; NOTE: build-server-spec must be called AFTER init-embedding-provider!
    ;; to get accurate Chroma availability for capability-based tool switching
    (let [spec (assoc (build-server-spec) :server-id server-id)
          log-ch (async/chan (async/sliding-buffer 20))
          server (io-server/stdio-server {:log-ch log-ch})
          ;; Create context and store for hot-reload capability
          context (assoc (sdk-server/create-context! spec) :server server)]
      (reset! server-context-atom context)
      (log/info "Server context stored for hot-reload capability")
      ;; Start the JSON-RPC server with our context
      (jsonrpc-server/start server context))))

(defn -main
  "Entry point for the MCP server."
  [& args]
  (apply start! args))

(comment
  ;; For REPL development
  (start!))
