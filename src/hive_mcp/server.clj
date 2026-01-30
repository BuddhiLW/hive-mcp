(ns hive-mcp.server
  "MCP server for Emacs interaction via emacsclient."
  (:require [io.modelcontext.clojure-sdk.stdio-server :as io-server]
            [io.modelcontext.clojure-sdk.server :as sdk-server]
            [jsonrpc4clj.server :as jsonrpc-server]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.channel :as channel]
            [hive-mcp.channel.websocket :as ws-channel]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.embeddings.ollama :as ollama]
            [hive-mcp.embeddings.service :as embedding-service]
            [hive-mcp.embeddings.config :as embedding-config]
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
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [hive-hot.core :as hot]
            [hive-hot.events :as hot-events]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.guards :as guards])
  (:gen-class))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Specs moved to hive-mcp.server.routes
;; Hivemind message spec (kept here for validation in server lifecycle)
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

;; Store coordinator-id for graceful shutdown
;; CLARITY-Y: Yield safe failure - enables coordinator cleanup on JVM exit
(defonce ^:private coordinator-id-atom (atom nil))

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

;; =============================================================================
;; Route Delegation (CLARITY-L: Layers stay pure)
;;
;; Routing logic extracted to hive-mcp.server.routes for SRP.
;; This module focuses on server lifecycle only.
;; Re-exports below maintain backward compatibility.
;; =============================================================================

;; Re-export for backward compatibility (tests, external consumers)
(def make-tool
  "Convert tool definition to SDK format. Delegates to routes module."
  routes/make-tool)

(def extract-agent-id
  "Extract agent-id from args. Delegates to routes module."
  routes/extract-agent-id)

(def emacs-server-spec
  "DEPRECATED: Use routes/build-server-spec instead."
  routes/emacs-server-spec)

(defn get-server-context
  "Get the current MCP server context (for debugging/hot-reload)."
  []
  @server-context-atom)

(defn refresh-tools!
  "Hot-reload all tools in the running server.
   Delegates to routes/refresh-tools! with server context atom."
  []
  (routes/refresh-tools! server-context-atom))

(defn debug-tool-handler
  "Get info about a registered tool handler (for debugging).
   Delegates to routes/debug-tool-handler with server context atom."
  [tool-name]
  (routes/debug-tool-handler server-context-atom tool-name))

(defn init-embedding-provider!
  "Initialize embedding providers for semantic memory search.

  Sets up:
  1. Chroma connection (vector database)
  2. EmbeddingService (per-collection routing)
  3. Per-collection embedding configuration:
     - hive-mcp-memory: Ollama (768 dims, fast, local)
     - hive-mcp-presets: OpenRouter (4096 dims, accurate) if API key available
  4. Global fallback provider (Ollama)

  Configuration via environment variables:
    CHROMA_HOST - Chroma server host (default: localhost)
    CHROMA_PORT - Chroma server port (default: 8000)
    OLLAMA_HOST - Ollama server URL (default: http://localhost:11434)
    OPENROUTER_API_KEY - OpenRouter API key (optional, for presets collection)"
  []
  (try
    ;; Configure Chroma connection - read from env or use defaults
    (let [chroma-host (or (System/getenv "CHROMA_HOST") "localhost")
          chroma-port (or (some-> (System/getenv "CHROMA_PORT") Integer/parseInt) 8000)]
      (chroma/configure! {:host chroma-host :port chroma-port})
      (log/info "Chroma configured:" chroma-host ":" chroma-port))

    ;; Initialize EmbeddingService for per-collection routing
    (embedding-service/init!)

    ;; Configure per-collection embedding providers
    ;; Memory collection: Ollama (fast, local, 768 dims)
    (try
      (embedding-service/configure-collection!
       "hive-mcp-memory"
       (embedding-config/ollama-config))
      (catch Exception e
        (log/warn "Could not configure Ollama for memory:" (.getMessage e))))

    ;; Presets collection: OpenRouter (accurate, 4096 dims) if API key available
    (when (System/getenv "OPENROUTER_API_KEY")
      (try
        (embedding-service/configure-collection!
         "hive-mcp-presets"
         (embedding-config/openrouter-config))
        (log/info "Presets collection configured with OpenRouter (4096 dims)")
        (catch Exception e
          (log/warn "Could not configure OpenRouter for presets, using Ollama fallback:"
                    (.getMessage e))
          ;; Fallback: use Ollama for presets too
          (try
            (embedding-service/configure-collection!
             "hive-mcp-presets"
             (embedding-config/ollama-config))
            (catch Exception _ nil)))))

    ;; Set global fallback provider (Ollama) for backward compatibility
    (let [ollama-host (or (System/getenv "OLLAMA_HOST") "http://localhost:11434")
          provider (ollama/->provider {:host ollama-host})]
      (chroma/set-embedding-provider! provider)
      (log/info "Global fallback embedding provider: Ollama at" ollama-host))

    (log/info "EmbeddingService status:" (embedding-service/status))
    true
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
            server-opts {:port nrepl-port :bind "127.0.0.1" :handler handler}
            server (nrepl-server/start-server server-opts)]
        (reset! nrepl-server-atom server)
        (log/info "Embedded nREPL started on port" nrepl-port
                  (if middleware "(with CIDER middleware)" "(basic)"))
        server)
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

;; =============================================================================
;; Hot-Reload Auto-Healing (CLARITY-Y: Yield safe failure)
;; =============================================================================

(defonce ^:private hot-reload-listener-registered? (atom false))

(defn- emit-mcp-health-event!
  "Emit health event via WebSocket channel after hot-reload.
   Lings can listen for this to confirm MCP is operational."
  [loaded-ns unloaded-ns ms]
  (try
    (ws-channel/emit! :mcp-health-restored
                      {:loaded (count loaded-ns)
                       :unloaded (count unloaded-ns)
                       :reload-ms ms
                       :timestamp (System/currentTimeMillis)
                       :status "healthy"})
    (log/info "Emitted :mcp-health-restored event after hot-reload")
    (catch Exception e
      (log/warn "Failed to emit health event (non-fatal):" (.getMessage e)))))

(defn- handle-hot-reload-success!
  "Handler for successful hot-reload - refreshes tools and emits health event.

   CLARITY: Yield safe failure - errors logged but don't break the reload."
  [{:keys [loaded unloaded ms]}]
  (log/info "Hot-reload completed:" (count loaded) "loaded," (count unloaded) "unloaded in" ms "ms")
  ;; Refresh MCP tool handlers to point to new var values
  (try
    (when @server-context-atom
      (routes/refresh-tools! server-context-atom)
      (log/info "MCP tools refreshed after hot-reload"))
    (catch Exception e
      (log/error "Failed to refresh MCP tools after hot-reload:" (.getMessage e))))
  ;; Emit health event for lings
  (emit-mcp-health-event! loaded unloaded ms))

(defn- register-hot-reload-listener!
  "Register listener with hive-hot to auto-heal MCP after reload.

   Only registers once. Safe to call multiple times."
  []
  (when-not @hot-reload-listener-registered?
    (try
      (require 'hive-hot.core)
      (let [add-listener! (resolve 'hive-hot.core/add-listener!)]
        (add-listener! :mcp-auto-heal
                       (fn [event]
                         (when (= (:type event) :reload-success)
                           (handle-hot-reload-success! event))))
        (reset! hot-reload-listener-registered? true)
        (log/info "Registered hot-reload listener for MCP auto-healing"))
      (catch Exception e
        (log/warn "Could not register hot-reload listener (non-fatal):" (.getMessage e))))))

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
        ;; Mark coordinator as terminated in DataScript (Phase 4)
        (when-let [coord-id @coordinator-id-atom]
          (try
            (require 'hive-mcp.swarm.datascript)
            (let [mark-terminated! (resolve 'hive-mcp.swarm.datascript/mark-coordinator-terminated!)]
              (mark-terminated! coord-id)
              (log/info "Coordinator marked terminated:" coord-id))
            (catch Exception e
              (log/warn "Coordinator cleanup failed (non-fatal):" (.getMessage e)))))
        (trigger-session-end! "jvm-shutdown"))))
    (reset! shutdown-hook-registered? true)
    (log/info "JVM shutdown hook registered for auto-wrap")))

(defn- read-project-config
  "Read .hive-project.edn config.
   Returns {:watch-dirs [...] :hot-reload bool} or nil.
   :hot-reload defaults to true for backward compatibility."
  []
  (try
    (let [project-file (java.io.File. ".hive-project.edn")]
      (when (.exists project-file)
        (let [config (edn/read-string (slurp project-file))]
          {:watch-dirs (:watch-dirs config)
           :hot-reload (get config :hot-reload true)})))
    (catch Exception _
      nil)))

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
    (when-let [sock (System/getenv "EMACS_SOCKET_NAME")]
      (log/info "Targeting Emacs daemon:" sock))
    ;; CLARITY-Y: Mark coordinator running FIRST to protect production state
    ;; This prevents test fixtures from resetting ev/reset-all! or ds/reset-conn!
    (guards/mark-coordinator-running!)
    ;; Initialize hooks system - needed for session-end auto-wrap
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
    ;; Register coordinator in DataScript + hivemind (Phase 4)
    ;; CLARITY-T: Telemetry first - expose coordinator identity for hivemind operations
    (try
      (require 'hive-mcp.swarm.datascript)
      (require 'hive-mcp.swarm.datascript.lings)
      (let [register! (resolve 'hive-mcp.swarm.datascript/register-coordinator!)
            add-slave! (resolve 'hive-mcp.swarm.datascript.lings/add-slave!)
            project-id (or (System/getenv "HIVE_MCP_PROJECT_ID") "hive-mcp")
            cwd (System/getProperty "user.dir")]
        (register! project-id {:project project-id})
        ;; Also register "coordinator" as a slave (depth 0) for bb-mcp compatibility
        ;; bb-mcp injects agent_id: "coordinator" on all tool calls for piggyback tracking
        (add-slave! "coordinator" {:name "coordinator"
                                   :status :idle
                                   :depth 0  ;; depth 0 = coordinator (not a ling)
                                   :project-id project-id
                                   :cwd cwd})
        (reset! coordinator-id-atom project-id)
        (log/info "Coordinator registered:" project-id "(also as slave for bb-mcp compat)"))
      (catch Exception e
        (log/warn "Coordinator registration failed (non-fatal):" (.getMessage e))))
    ;; Start embedded nREPL FIRST - bb-mcp needs this to forward tool calls
    ;; This MUST run in the same JVM as channel server for hivemind to work
    (start-embedded-nrepl!)
    ;; Start WebSocket server if enabled (for Claude Code IDE integration)
    (start-websocket-server!)
    ;; Initialize embedding provider for semantic search (fails gracefully)
    (init-embedding-provider!)
    ;; Register tools for agent delegation (allows local models to use MCP tools)
    ;; Delegates to routes module for capability-filtered tools
    (routes/register-tools-for-delegation!)
    ;; Start WebSocket channel with auto-healing (primary - Aleph/Netty based)
    ;; This is the reliable push channel for hivemind events
    (start-ws-channel-with-healing!)
    ;; Start legacy bidirectional channel server (deprecated - kept for backwards compat)
    (let [channel-port (parse-long (or (System/getenv "HIVE_MCP_CHANNEL_PORT") "9998"))]
      (try
        (channel/start-server! {:type :tcp :port channel-port})
        ;; Mark coordinator as running to protect from test fixture cleanup
        ;; CLARITY-Y: This prevents ch/stop-server! in test fixtures from killing
        ;; the production server when tests run in the same JVM
        (channel/mark-coordinator-running!)
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
    ;; Initialize hot-reload watcher with claim-aware coordination
    ;; ADR: State-based debouncing - claimed files buffer until release
    ;; CLARITY-I: Check :hot-reload config before starting watcher
    (let [project-config (read-project-config)
          hot-reload-enabled? (get project-config :hot-reload true)]
      (if hot-reload-enabled?
        (try
          (let [src-dirs (or (some-> (System/getenv "HIVE_MCP_SRC_DIRS")
                                     (str/split #":"))
                             (:watch-dirs project-config)
                             ["src"])
                claim-checker (hot-events/make-claim-checker logic/get-all-claims)]
            (hot/init-with-watcher! {:dirs src-dirs
                                     :claim-checker claim-checker
                                     :debounce-ms 100})
            (log/info "Hot-reload watcher started:" {:dirs src-dirs})
            ;; Register MCP auto-heal listener to refresh tools after reload
            (register-hot-reload-listener!))
          (catch Exception e
            (log/warn "Hot-reload watcher failed to start (non-fatal):" (.getMessage e))))
        (log/info "Hot-reload disabled via .hive-project.edn")))
    ;; Start lings registry sync - keeps Clojure registry in sync with elisp
    ;; ADR-001: Event-driven sync for lings_available to return accurate counts
    (try
      (swarm/start-registry-sync!)
      (log/info "Lings registry sync started - lings_available will track elisp lings")
      (catch Exception e
        (log/warn "Lings registry sync failed to start (non-fatal):" (.getMessage e))))
    ;; Start MCP server - create context ourselves to enable hot-reload
    ;; CLARITY: Telemetry first - expose state for debugging
    ;; NOTE: routes/build-server-spec must be called AFTER init-embedding-provider!
    ;; to get accurate Chroma availability for capability-based tool switching
    (let [spec (assoc (routes/build-server-spec) :server-id server-id)
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
