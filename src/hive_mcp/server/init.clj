(ns hive-mcp.server.init
  "Service initialization: embedding, hot-reload, events, coordinator.

   Bounded context: Service bootstrap and dependency wiring.

   Manages:
   - Embedding provider initialization (Chroma, Ollama, OpenRouter)
   - Hot-reload auto-healing (MCP tool refresh after reload)
   - Event system initialization (re-frame inspired)
   - Coordinator registration in DataScript
   - Memory store wiring (IMemoryStore protocol)
   - Channel bridge + swarm sync + registry sync
   - L2 decay scheduler (periodic memory/edge/disc decay)"
  (:require [hive-mcp.chroma :as chroma]
            [hive-mcp.channel.websocket :as ws-channel]
            [hive-mcp.embeddings.ollama :as ollama]
            [hive-mcp.embeddings.service :as embedding-service]
            [hive-mcp.embeddings.config :as embedding-config]
            [hive-mcp.config :as global-config]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects :as effects]
            [hive-mcp.events.handlers :as ev-handlers]
            [hive-mcp.events.channel-bridge :as channel-bridge]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.memory.store.chroma :as chroma-store]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.swarm.logic :as logic]
            [hive-hot.core :as hot]
            [hive-hot.events :as hot-events]
            [taoensso.timbre :as log]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Hot-Reload State
;; =============================================================================

;; Track if hot-reload listener is registered (private, module-scoped)
(defonce ^:private hot-reload-listener-registered? (atom false))

;; =============================================================================
;; Embedding Provider Initialization
;; =============================================================================

(defn init-embedding-provider!
  "Initialize embedding providers for semantic memory search.

  Sets up:
  1. Chroma connection (vector database)
  2. EmbeddingService (per-collection routing)
  3. Per-collection embedding configuration:
     - hive-mcp-memory: Ollama (768 dims, fast, local)
     - hive-mcp-presets: OpenRouter (4096 dims, accurate) if API key available
  4. Global fallback provider (Ollama)

  Configuration priority (highest to lowest):
  1. ~/.config/hive-mcp/config.edn :embeddings section
  2. ~/.config/hive-mcp/config.edn :services / :secrets sections
  3. Environment variables (OLLAMA_HOST, OPENROUTER_API_KEY, etc.) as fallback
  4. Built-in defaults"
  []
  (try
    ;; Load global config to get :embeddings section
    (let [cfg (global-config/get-global-config)
          embed-cfg (get cfg :embeddings {})
          ollama-cfg (get embed-cfg :ollama {})
          openrouter-cfg (get embed-cfg :openrouter {})]

      ;; Configure Chroma connection - config.edn :services > env vars > defaults
      (let [chroma-host (global-config/get-service-value :chroma :host :env "CHROMA_HOST" :default "localhost")
            chroma-port (global-config/get-service-value :chroma :port :env "CHROMA_PORT" :parse parse-long :default 8000)]
        (chroma/configure! {:host chroma-host :port chroma-port})
        (log/info "Chroma configured:" chroma-host ":" chroma-port))

      ;; Initialize EmbeddingService for per-collection routing
      (embedding-service/init!)

      ;; Read Ollama host/model from :embeddings > :services > env vars > defaults
      (let [ollama-host (or (:host ollama-cfg)
                            (global-config/get-service-value :ollama :host
                                                             :env "OLLAMA_HOST"
                                                             :default "http://localhost:11434"))
            ollama-model (or (:model ollama-cfg) "nomic-embed-text")
            openrouter-model (or (:model openrouter-cfg) "qwen/qwen3-embedding-8b")]

        ;; Configure per-collection embedding providers
        ;; Memory collection: Ollama (fast, local, 768 dims)
        (try
          (embedding-service/configure-collection!
           "hive-mcp-memory"
           (embedding-config/ollama-config {:host ollama-host :model ollama-model}))
          (catch Exception e
            (log/warn "Could not configure Ollama for memory:" (.getMessage e))))

        ;; Presets collection: OpenRouter (accurate, 4096 dims) if API key available
        (when (global-config/get-secret :openrouter-api-key)
          (try
            (embedding-service/configure-collection!
             "hive-mcp-presets"
             (embedding-config/openrouter-config {:model openrouter-model}))
            (log/info "Presets collection configured with OpenRouter (4096 dims)")
            (catch Exception e
              (log/warn "Could not configure OpenRouter for presets, using Ollama fallback:"
                        (.getMessage e))
              ;; Fallback: use Ollama for presets too
              (try
                (embedding-service/configure-collection!
                 "hive-mcp-presets"
                 (embedding-config/ollama-config {:host ollama-host :model ollama-model}))
                (catch Exception _ nil)))))

        ;; Plans collection: OpenRouter (4096 dims) for large plan entries (1000-5000+ chars)
        ;; Plans exceed Ollama's ~1500 char embedding limit, so OpenRouter is preferred
        (if (global-config/get-secret :openrouter-api-key)
          (try
            (embedding-service/configure-collection!
             "hive-mcp-plans"
             (embedding-config/openrouter-config {:model openrouter-model}))
            (log/info "Plans collection configured with OpenRouter (4096 dims)")
            (catch Exception e
              (log/warn "Could not configure OpenRouter for plans, using Ollama fallback:"
                        (.getMessage e))
              ;; Fallback: use Ollama (truncation risk for large plans, but works)
              (try
                (embedding-service/configure-collection!
                 "hive-mcp-plans"
                 (embedding-config/ollama-config {:host ollama-host :model ollama-model}))
                (log/warn "Plans collection using Ollama - entries >1500 chars may be truncated")
                (catch Exception _ nil))))
          ;; No OpenRouter key - use Ollama with warning
          (try
            (embedding-service/configure-collection!
             "hive-mcp-plans"
             (embedding-config/ollama-config {:host ollama-host :model ollama-model}))
            (log/warn "Plans collection using Ollama (no OPENROUTER_API_KEY) - entries >1500 chars may be truncated")
            (catch Exception _ nil)))

        ;; Set global fallback provider (Ollama) for backward compatibility
        (let [provider (ollama/->provider {:host ollama-host})]
          (chroma/set-embedding-provider! provider)
          (log/info "Global fallback embedding provider: Ollama at" ollama-host))

        (log/info "Embedding config from config.edn:" {:ollama-host ollama-host
                                                       :ollama-model ollama-model
                                                       :openrouter-model openrouter-model})
        (log/info "EmbeddingService status:" (embedding-service/status))
        true))
    (catch Exception e
      (log/warn "Could not initialize embedding provider:"
                (.getMessage e)
                "- Semantic search will be unavailable")
      false)))

;; =============================================================================
;; Hot-Reload Auto-Healing (CLARITY-Y: Yield safe failure)
;; =============================================================================

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

   CLARITY: Yield safe failure - errors logged but don't break the reload.

   Parameters:
     server-context-atom - atom containing MCP server context"
  [server-context-atom {:keys [loaded unloaded ms]}]
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

   Only registers once. Safe to call multiple times.

   Parameters:
     server-context-atom - atom containing MCP server context"
  [server-context-atom]
  (when-not @hot-reload-listener-registered?
    (try
      (require 'hive-hot.core)
      (let [add-listener! (resolve 'hive-hot.core/add-listener!)]
        (add-listener! :mcp-auto-heal
                       (fn [event]
                         (when (= (:type event) :reload-success)
                           (handle-hot-reload-success! server-context-atom event))))
        (reset! hot-reload-listener-registered? true)
        (log/info "Registered hot-reload listener for MCP auto-healing"))
      (catch Exception e
        (log/warn "Could not register hot-reload listener (non-fatal):" (.getMessage e))))))

;; =============================================================================
;; Event System Initialization
;; =============================================================================

(defn init-events!
  "Initialize hive-events system (re-frame inspired event dispatch).
   EVENTS-01: Event system must init after hooks but before channel."
  []
  (try
    (ev/init!)
    (effects/register-effects!)
    (ev-handlers/register-handlers!)
    (log/info "hive-events system initialized")
    (catch Exception e
      (log/warn "hive-events initialization failed (non-fatal):" (.getMessage e)))))

;; =============================================================================
;; Coordinator Registration
;; =============================================================================

(defn register-coordinator!
  "Register coordinator in DataScript + hivemind (Phase 4).

   CLARITY-T: Telemetry first - expose coordinator identity for hivemind operations.

   Parameters:
     coordinator-id-atom - atom to store coordinator project-id"
  [coordinator-id-atom]
  (try
    (require 'hive-mcp.swarm.datascript)
    (require 'hive-mcp.swarm.datascript.lings)
    (let [register! (resolve 'hive-mcp.swarm.datascript/register-coordinator!)
          add-slave! (resolve 'hive-mcp.swarm.datascript.lings/add-slave!)
          project-id (global-config/get-service-value :project :id :env "HIVE_MCP_PROJECT_ID" :default "hive-mcp")
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
      (log/warn "Coordinator registration failed (non-fatal):" (.getMessage e)))))

;; =============================================================================
;; Memory Store Wiring
;; =============================================================================

(defn wire-memory-store!
  "Wire ChromaMemoryStore as active IMemoryStore backend (Phase 1 vectordb abstraction).
   SOLID-D: Consumers can depend on IMemoryStore protocol instead of chroma directly.
   Must run AFTER init-embedding-provider! since Chroma config is set there."
  []
  (try
    (let [store (chroma-store/create-store)]
      (mem-proto/set-store! store)
      (log/info "ChromaMemoryStore wired as active IMemoryStore backend"))
    (catch Exception e
      (log/warn "ChromaMemoryStore initialization failed (non-fatal):" (.getMessage e)))))

;; =============================================================================
;; Channel Bridge + Sync
;; =============================================================================

(defn init-channel-bridge!
  "Initialize channel bridge - wires channel events to hive-events dispatch.
   EVENTS-01: Must init after both channel server and event system."
  []
  (try
    (channel-bridge/init!)
    (log/info "Channel bridge initialized - channel events will dispatch to hive-events")
    (catch Exception e
      (log/warn "Channel bridge initialization failed (non-fatal):" (.getMessage e)))))

(defn start-swarm-sync!
  "Start swarm sync - bridges channel events to logic database.
   This enables: task-completed → release claims → process queue."
  []
  (try
    (sync/start-sync!)
    (log/info "Swarm sync started - logic database will track swarm state")
    (catch Exception e
      (log/warn "Swarm sync failed to start (non-fatal):" (.getMessage e)))))

;; =============================================================================
;; Hot-Reload Watcher
;; =============================================================================

(defn init-hot-reload-watcher!
  "Initialize hot-reload watcher with claim-aware coordination.

   ADR: State-based debouncing - claimed files buffer until release.
   CLARITY-I: Check :hot-reload config before starting watcher.

   Parameters:
     server-context-atom - atom containing MCP server context
     project-config      - map from read-project-config (or nil)"
  [server-context-atom project-config]
  (let [hot-reload-enabled? (get project-config :hot-reload true)]
    (if hot-reload-enabled?
      (try
        (let [src-dirs (or (global-config/get-service-value :project :src-dirs
                                                            :env "HIVE_MCP_SRC_DIRS"
                                                            :parse #(str/split % #":"))
                           (:watch-dirs project-config)
                           ["src"])
              claim-checker (hot-events/make-claim-checker logic/get-all-claims)]
          (hot/init-with-watcher! {:dirs src-dirs
                                   :claim-checker claim-checker
                                   :debounce-ms 100})
          (log/info "Hot-reload watcher started:" {:dirs src-dirs})
          ;; Register MCP auto-heal listener to refresh tools after reload
          (register-hot-reload-listener! server-context-atom)
          ;; Register state protection for DataScript state validation
          (try
            (require 'hive-mcp.hot.state)
            (let [register! (resolve 'hive-mcp.hot.state/register-with-hive-hot!)]
              (register!))
            (catch Exception e
              (log/debug "Hot-state protection not registered:" (.getMessage e))))
          ;; Register SAA Silence strategy for hot-reload aware exploration
          (try
            (require 'hive-mcp.hot.silence)
            (let [register! (resolve 'hive-mcp.hot.silence/register-with-hive-hot!)]
              (register!))
            (catch Exception e
              (log/debug "SAA Silence not registered:" (.getMessage e)))))
        (catch Exception e
          (log/warn "Hot-reload watcher failed to start (non-fatal):" (.getMessage e))))
      (log/info "Hot-reload disabled via .hive-project.edn"))))

;; =============================================================================
;; Registry Sync
;; =============================================================================

(defn start-registry-sync!
  "Start lings registry sync - keeps Clojure registry in sync with elisp.
   ADR-001: Event-driven sync for lings_available to return accurate counts."
  []
  (try
    (swarm/start-registry-sync!)
    (log/info "Lings registry sync started - lings_available will track elisp lings")
    (catch Exception e
      (log/warn "Lings registry sync failed to start (non-fatal):" (.getMessage e)))))

;; =============================================================================
;; L2 Decay Scheduler
;; =============================================================================

(defn start-decay-scheduler!
  "Start the L2 periodic decay scheduler.
   Runs memory staleness decay, edge confidence decay, and disc certainty
   decay on a configurable interval (default: 60 minutes).

   Configure via config.edn :services :scheduler:
     {:enabled true :interval-minutes 60 :memory-limit 50 :edge-limit 100}

   Non-fatal: if scheduler fails to start, system continues without it.
   Decay still runs on wrap/catchup hooks as before."
  []
  (try
    (require 'hive-mcp.scheduler.decay)
    (let [start-fn (resolve 'hive-mcp.scheduler.decay/start!)]
      (when start-fn
        (let [result (start-fn)]
          (if (:started result)
            (log/info "L2 decay scheduler started:" result)
            (log/info "L2 decay scheduler not started:" (:reason result))))))
    (catch Exception e
      (log/warn "L2 decay scheduler failed to start (non-fatal):" (.getMessage e)))))

(defn stop-decay-scheduler!
  "Stop the L2 periodic decay scheduler. Called during shutdown."
  []
  (try
    (require 'hive-mcp.scheduler.decay)
    (when-let [stop-fn (resolve 'hive-mcp.scheduler.decay/stop!)]
      (stop-fn))
    (catch Exception _)))

;; =============================================================================
;; Workflow Engine Initialization
;; =============================================================================

(defn init-workflow-engine!
  "Initialize FSM workflow registry and wire FSMWorkflowEngine as active engine.

   1. Calls registry/init! to scan EDN specs and register all built-in handlers
   2. Creates FSMWorkflowEngine and sets it as the active IWorkflowEngine

   Must run AFTER embedding/memory services (handlers may need them at runtime).
   Non-fatal: if initialization fails, NoopWorkflowEngine remains as fallback."
  []
  (try
    (require 'hive-mcp.workflows.registry)
    (require 'hive-mcp.workflows.fsm-engine)
    (require 'hive-mcp.protocols.workflow)
    (let [registry-init! (resolve 'hive-mcp.workflows.registry/init!)
          create-engine  (resolve 'hive-mcp.workflows.fsm-engine/create-engine)
          set-engine!    (resolve 'hive-mcp.protocols.workflow/set-workflow-engine!)]
      (registry-init!)
      (set-engine! (create-engine))
      (log/info "FSM workflow engine initialized and wired as active IWorkflowEngine"))
    (catch Exception e
      (log/warn "Workflow engine initialization failed (non-fatal):" (.getMessage e)))))
