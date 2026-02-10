(ns hive-mcp.events.effects
  "Concrete effect implementations for the hive-mcp event system — facade module.

   This namespace re-exports key public vars from the effects sub-modules
   for backward compatibility. New code should require specific sub-modules:

   - hive-mcp.events.effects.coeffect         — coeffects (now, agent-context, db-snapshot, etc.)
   - hive-mcp.events.effects.notification     — shout, log, channel, olympus
   - hive-mcp.events.effects.memory           — memory-write, wrap-notify, wrap-crystallize
   - hive-mcp.events.effects.agent            — dispatch-task, swarm-send-prompt, agora, saa
   - hive-mcp.events.effects.dispatch         — event chaining (dispatch, dispatch-n)
   - hive-mcp.events.effects.infrastructure   — ds-transact, git, kanban, metrics
   - hive-mcp.events.effects.kg               — knowledge graph edges

   Usage:
   ```clojure
   (require '[hive-mcp.events.effects :as effects])
   (effects/register-effects!)
   ```

   SOLID: Single Responsibility - facade for effect registration
   CLARITY: Y - Yield safe failure (effects catch and log errors)"
  (:require [hive-mcp.events.effects.coeffect :as cofx-effects]
            [hive-mcp.events.effects.notification :as notif-effects]
            [hive-mcp.events.effects.memory :as mem-effects]
            [hive-mcp.events.effects.agent :as agent-effects]
            [hive-mcp.events.effects.dispatch :as dispatch-effects]
            [hive-mcp.events.effects.infrastructure :as infra-effects]
            [hive-mcp.events.effects.kg :as kg-effects]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports: Memory handler injection (public API)
;; =============================================================================

(def set-memory-write-handler!
  "Set the handler function for :memory-write effect.
   Called during server initialization to wire infrastructure layer."
  mem-effects/set-memory-write-handler!)

(def set-wrap-crystallize-handler!
  "Set the handler function for :wrap-crystallize effect.
   Called during server initialization to wire tools layer."
  mem-effects/set-wrap-crystallize-handler!)

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-effects!
  "Register all concrete effect handlers and coeffects.

   Safe to call multiple times - only registers once.

   Delegates to domain-specific submodules:
   - notification: :shout :targeted-shout :log :channel-publish :emit-system-error :olympus-broadcast
   - memory:       :memory-write :wrap-notify :wrap-crystallize
   - agent:        :dispatch-task :swarm-send-prompt :agora/continue :agora/execute-drone :saa/run-workflow
   - dispatch:     :dispatch :dispatch-n
   - infrastructure: :ds-transact :git-commit :kanban-sync :kanban-move-done :report-metrics :tool-registry-refresh
   - kg:           :kg-add-edge :kg-update-confidence :kg-increment-confidence :kg-remove-edge :kg-remove-edges-for-node

   Coeffects registered (POC-08/09/10/11):
   - :now             - Current timestamp in milliseconds
   - :agent-context   - Agent ID and current working directory
   - :db-snapshot     - DataScript database snapshot
   - :waiting-lings   - Query lings waiting on a specific file (File Claim Cascade)
   - :request-ctx     - Current request context from tool execution

   Returns true if effects were registered, false if already registered."
  []
  (when-not @*registered
    ;; ==========================================================================
    ;; Coeffects (delegated to coeffect submodule)
    ;; ==========================================================================

    (cofx-effects/register-coeffects!)

    ;; ==========================================================================
    ;; Effects (delegated to domain-specific submodules)
    ;; ==========================================================================

    (notif-effects/register-notification-effects!)
    (mem-effects/register-memory-effects!)
    (agent-effects/register-agent-effects!)
    (dispatch-effects/register-dispatch-effects!)
    (infra-effects/register-infrastructure-effects!)
    (kg-effects/register-kg-effects!)

    ;; NOTE: :crystal/wrap-notify event handler is registered in
    ;; hive-mcp.events.handlers.crystal/register-handlers! with proper
    ;; defensive stats handling. Do NOT duplicate here.

    (reset! *registered true)
    (log/info "[hive-events] All effect/coeffect submodules registered (coeffect, notification, memory, agent, dispatch, infrastructure, kg)")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
