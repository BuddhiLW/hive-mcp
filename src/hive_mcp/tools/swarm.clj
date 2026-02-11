(ns hive-mcp.tools.swarm
  "Swarm management facade.

   Thin re-export layer delegating to focused submodules:
   - swarm.core      - Shared utilities, addon check
   - swarm.registry  - Lings registry, event-driven sync
   - swarm.state     - Hivemind state integration
   - swarm.lifecycle - Spawn, kill handlers
   - swarm.dispatch  - Dispatch handler with coordinator
   - swarm.collect   - Collect handler with push/poll
   - swarm.status    - Status, lings-available, broadcast
   - swarm.channel   - Channel event management
   - swarm.wave      - Batch drone wave execution
   - swarm.validated-wave - Validated wave with self-healing

  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.tools.swarm.state :as state]
            [hive-mcp.tools.swarm.lifecycle :as lifecycle]
            [hive-mcp.tools.swarm.dispatch :as dispatch]
            [hive-mcp.tools.swarm.collect :as collect]
            [hive-mcp.tools.swarm.status :as status]
            [hive-mcp.tools.swarm.channel :as channel]
            [hive-mcp.tools.swarm.wave :as wave]
            [hive-mcp.tools.swarm.validated-wave :as validated-wave]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Backward Compatibility: Re-export Registry API
;; ============================================================
;; Tests access these directly from hive-mcp.tools.swarm

(def register-ling!
  "Register a spawned ling. Re-exported from registry module."
  registry/register-ling!)

(def unregister-ling!
  "Unregister a ling. Re-exported from registry module."
  registry/unregister-ling!)

(def get-available-lings
  "Get all registered lings. Re-exported from registry module."
  registry/get-available-lings)

(def start-registry-sync!
  "Start event-driven registry sync. Re-exported from registry module."
  registry/start-registry-sync!)

(def stop-registry-sync!
  "Stop registry sync. Re-exported from registry module."
  registry/stop-registry-sync!)

;; ============================================================
;; Backward Compatibility: Re-export State API
;; ============================================================

(def get-slave-working-status
  "Get slave working status from hivemind. Re-exported from state module."
  state/get-slave-working-status)

(def get-unified-swarm-status
  "Get unified swarm status. Re-exported from state module."
  state/get-unified-swarm-status)

;; ============================================================
;; Backward Compatibility: Re-export Channel API
;; ============================================================

(def start-channel-subscriptions!
  "Start channel subscriptions. Re-exported from channel module."
  channel/start-channel-subscriptions!)

(def stop-channel-subscriptions!
  "Stop channel subscriptions. Re-exported from channel module."
  channel/stop-channel-subscriptions!)

(def check-event-journal
  "Check event journal for task. Re-exported from channel module."
  channel/check-event-journal)

(def clear-event-journal!
  "Clear event journal. Re-exported from channel module."
  channel/clear-event-journal!)

;; ============================================================
;; Backward Compatibility: Re-export Core API
;; ============================================================

(def swarm-addon-available?
  "Check if swarm addon is available. Re-exported from core module."
  core/swarm-addon-available?)

;; ============================================================
;; Handler Delegation
;; ============================================================

(def handle-swarm-spawn
  "Spawn a new slave. Delegated to lifecycle module."
  lifecycle/handle-swarm-spawn)

(def handle-swarm-kill
  "Kill a slave. Delegated to lifecycle module."
  lifecycle/handle-swarm-kill)

(def handle-swarm-dispatch
  "Dispatch to a slave. Delegated to dispatch module."
  dispatch/handle-swarm-dispatch)

(def handle-swarm-collect
  "Collect task result. Delegated to collect module."
  collect/handle-swarm-collect)

(def handle-swarm-status
  "Get swarm status. Delegated to status module."
  status/handle-swarm-status)

(def handle-lings-available
  "List available lings. Delegated to status module."
  status/handle-lings-available)

(def handle-swarm-broadcast
  "Broadcast to all slaves. Delegated to status module."
  status/handle-swarm-broadcast)

(def handle-dispatch-drone-wave
  "Dispatch batch drone wave. Delegated to wave module."
  wave/handle-dispatch-drone-wave)

(def handle-get-wave-status
  "Get wave execution status. Delegated to wave module."
  wave/handle-get-wave-status)

(def handle-dispatch-validated-wave
  "Dispatch validated wave with self-healing. Delegated to validated-wave module."
  validated-wave/handle-dispatch-validated-wave)
