(ns hive-mcp.server.lifecycle
  "Server lifecycle: hooks, shutdown, configuration.

   Bounded context: Server start/stop/reload orchestration.

   Manages:
   - Global hooks registry (event-driven workflows)
   - JVM shutdown hooks (auto-wrap, coordinator cleanup)
   - Project configuration (.hive-project.edn)"
  (:require [hive-mcp.hooks.core :as hooks]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.swarm.sync :as sync]
            [hive-mcp.transport.olympus :as olympus-ws]
            [taoensso.timbre :as log]
            [clojure.edn :as edn]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Hooks Registry Access
;; =============================================================================

(defn get-hooks-registry
  "Get the global hooks registry for external registration.
   Takes the hooks-registry-atom as parameter for decoupling."
  [hooks-registry-atom]
  @hooks-registry-atom)

;; =============================================================================
;; Session End / Shutdown
;; =============================================================================

(defn- trigger-session-end!
  "Trigger session-end hooks for auto-wrap.
   Called by JVM shutdown hook.

   CLARITY: Yield safe failure - errors logged but don't break shutdown."
  [hooks-registry-atom reason]
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

(defn register-shutdown-hook!
  "Register JVM shutdown hook to trigger session-end for auto-wrap.

   Only registers once. Safe to call multiple times.

   CLARITY: Yield safe failure - hook errors don't break JVM shutdown.

   Parameters:
     shutdown-hook-registered? - atom tracking registration state
     coordinator-id-atom       - atom with coordinator project-id
     hooks-registry-atom       - atom with hooks registry"
  [shutdown-hook-registered? coordinator-id-atom hooks-registry-atom]
  (when-not @shutdown-hook-registered?
    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      (fn []
        (log/info "JVM shutdown detected - running shutdown sequence")
        ;; Stop Olympus WebSocket server first (close client connections cleanly)
        ;; CLARITY-Y: Yield safe failure - errors don't break shutdown sequence
        (try
          (olympus-ws/stop!)
          (log/info "Olympus WebSocket server stopped")
          (catch Exception e
            (log/warn "Olympus WS shutdown failed (non-fatal):" (.getMessage e))))
        ;; Mark coordinator as terminated in DataScript (Phase 4)
        (when-let [coord-id @coordinator-id-atom]
          (try
            (require 'hive-mcp.swarm.datascript)
            (let [mark-terminated! (resolve 'hive-mcp.swarm.datascript/mark-coordinator-terminated!)]
              (mark-terminated! coord-id)
              (log/info "Coordinator marked terminated:" coord-id))
            (catch Exception e
              (log/warn "Coordinator cleanup failed (non-fatal):" (.getMessage e)))))
        (trigger-session-end! hooks-registry-atom "jvm-shutdown"))))
    (reset! shutdown-hook-registered? true)
    (log/info "JVM shutdown hook registered for auto-wrap")))

;; =============================================================================
;; Project Configuration
;; =============================================================================

(defn read-project-config
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

;; =============================================================================
;; Hooks Initialization
;; =============================================================================

(defn init-hooks!
  "Initialize the hooks system and register crystal hooks.

   Creates global registry, registers crystal hooks (auto-wrap),
   and sets up JVM shutdown hook.

   Should be called early in server startup.

   Parameters:
     hooks-registry-atom       - atom to store the registry
     shutdown-hook-registered? - atom tracking shutdown hook state
     coordinator-id-atom       - atom with coordinator project-id"
  [hooks-registry-atom shutdown-hook-registered? coordinator-id-atom]
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
      (register-shutdown-hook! shutdown-hook-registered? coordinator-id-atom hooks-registry-atom)
      {:registry registry
       :hooks-registered true})))
