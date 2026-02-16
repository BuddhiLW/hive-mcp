(ns hive-mcp.server.core
  "MCP server entry point.

   Thin orchestrator delegating to sub-modules:
   - lifecycle: hooks, shutdown, configuration
   - transport: nREPL, WebSocket, channel servers
   - init: service initialization (embedding, events, hot-reload)
   - routes: tool dispatch, handler wrappers, server spec"
  (:require [io.modelcontext.clojure-sdk.stdio-server :as io-server]
            [io.modelcontext.clojure-sdk.server :as sdk-server]
            [jsonrpc4clj.server :as jsonrpc-server]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.server.lifecycle :as lifecycle]
            [hive-mcp.server.transport :as transport]
            [hive-mcp.server.init :as init]
            [hive-mcp.server.guards :as guards]
            [clojure.core.async :as async]
            [taoensso.timbre :as log])
  (:gen-class))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Server State (defonce atoms for lifecycle management)
;; =============================================================================

;; Store nREPL server reference for shutdown
(defonce ^:private nrepl-server-atom (atom nil))

;; Store MCP server context for hot-reload capability
(defonce ^:private server-context-atom (atom nil))

;; Global hooks registry for event-driven workflows
(defonce ^:private hooks-registry-atom (atom nil))

;; Track if shutdown hook is registered
(defonce ^:private shutdown-hook-registered? (atom false))

;; Store coordinator-id for graceful shutdown
(defonce ^:private coordinator-id-atom (atom nil))

;; WebSocket channel monitor for auto-healing
(defonce ^:private ws-channel-monitor (atom nil))

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
;; Server Lifecycle - Thin orchestrator
;; =============================================================================

(defn start!
  "Start the MCP server.

   Orchestrates startup by delegating to sub-modules in correct order:
   1. Guards + Hooks (lifecycle)
   2. Events + Coordinator (init)
   3. Network servers (transport)
   4. Services: embedding + memory (init)
   5. Channels + Sync (transport + init)
   6. Hot-reload + Registry sync (init)
   7. MCP stdio server (must be last - blocks)"
  [& _args]
  (let [server-id (random-uuid)]
    (log/info "Starting hive-mcp server:" server-id)
    (when-let [sock (System/getenv "EMACS_SOCKET_NAME")]
      (log/info "Targeting Emacs daemon:" sock))

    ;; Phase 1: Guards + Hooks
    (guards/mark-coordinator-running!)
    (lifecycle/init-hooks! hooks-registry-atom shutdown-hook-registered? coordinator-id-atom)

    ;; Phase 2: Events + Coordinator registration
    (init/init-events!)
    (init/register-coordinator! coordinator-id-atom)

    ;; Phase 3: Transport (network servers)
    (transport/start-embedded-nrepl! nrepl-server-atom)
    (transport/start-websocket-server!)
    (init/init-nats!)

    ;; Phase 4: Services (embedding, memory store, tool delegation)
    (init/init-embedding-provider!)
    (init/wire-memory-store!)
    (routes/register-tools-for-delegation!)

    ;; Phase 4.4: Forge belt defaults (extensions can override)
    (init/register-forge-belt-defaults!)

    ;; Phase 4.5: Extension loading (classpath addon discovery)
    ;; Must run AFTER embedding/memory (extensions may use Chroma).
    ;; Must run BEFORE workflow engine (handlers may use extensions).
    ;; hive-claude auto-discovered here via META-INF manifest (registers :claude terminal)
    (init/load-extensions!)

    ;; Phase 5: Channels + Sync
    (transport/start-ws-channel-with-healing! ws-channel-monitor)
    (transport/start-olympus-ws!)
    (transport/start-a2a-gateway!)
    (transport/start-legacy-channel!)
    (init/init-channel-bridge!)
    (init/start-swarm-sync!)

    ;; Phase 5.5: Global config + auto-generate missing .hive-project.edn
    ;; Must run after services (Chroma) and before hot-reload (needs project configs)
    (try
      (require 'hive-mcp.config.core)
      (require 'hive-mcp.tools.hive-project)
      (let [load-config! (resolve 'hive-mcp.config.core/load-global-config!)
            scan! (resolve 'hive-mcp.tools.hive-project/scan-and-generate-missing!)]
        (load-config!)
        (let [result (scan!)]
          (log/info "Phase 5.5: Auto-gen .hive-project.edn:" result)))
      (catch Exception e
        (log/warn "Phase 5.5: Auto-gen scan failed (non-fatal):" (.getMessage e))))

    ;; Phase 5.7: FSM Workflow Engine (registry + IWorkflowEngine wiring)
    ;; Must run after services (handlers use memory/kanban at runtime).
    (init/init-workflow-engine!)

    ;; Phase 6: Hot-reload + Registry sync
    (init/init-hot-reload-watcher! server-context-atom (lifecycle/read-project-config))
    (init/start-registry-sync!)

    ;; Phase 6.5: Decay Scheduler (periodic memory/edge/disc decay)
    ;; Must run after config loaded (Phase 5.5) and embedding provider (Phase 4).
    ;; Daemon thread — dies with JVM, no explicit shutdown needed.
    (init/start-decay-scheduler!)

    ;; Phase 7: Start MCP server (must be last - blocks on stdio)
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
