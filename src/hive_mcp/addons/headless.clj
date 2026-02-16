(ns hive-mcp.addons.headless
  "IHeadlessBackend protocol for addon-contributed headless backends.

   Headless addons extend the IAddon lifecycle with headless-specific operations:
   - Spawn headless processes (SDK subprocesses, ProcessBuilder, API sessions)
   - Dispatch tasks to running headless sessions
   - Query session status/liveness
   - Kill/interrupt running sessions

   Architecture:
     IHeadlessBackend is a *companion* protocol to IAddon (same pattern as
     ITerminalAddon in hive-mcp.addons.terminal). Concrete headless backends
     implement BOTH IAddon and IHeadlessBackend on the same reify.

     Method signatures mirror ILingStrategy exactly -- same arities and argument
     semantics -- so the headless-registry can dispatch to addon-contributed
     backends transparently via the HeadlessAddonStrategy adapter.

   See also:
   - hive-mcp.addons.protocol             -- IAddon base protocol
   - hive-mcp.addons.terminal             -- ITerminalAddon (analogous pattern)
   - hive-mcp.addons.headless-caps        -- Optional capability protocols (ISP)
   - hive-mcp.agent.ling.strategy         -- ILingStrategy (method signatures mirrored)
   - hive-mcp.agent.ling.headless-addon-strategy -- Adapter (Layer 2 bridge)
   - hive-mcp.agent.ling.headless-registry       -- Registry (dispatch lookup)"
  (:require [hive-mcp.addons.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; IHeadlessBackend Protocol
;; =============================================================================

(defprotocol IHeadlessBackend
  "Protocol for addon-contributed headless backends.

   Implementors must also satisfy IAddon for addon lifecycle integration.
   The headless backend is initialized during IAddon/initialize! and torn
   down during IAddon/shutdown!.

   Method signatures mirror ILingStrategy exactly -- same arities and argument
   semantics -- enabling the headless-registry to dispatch to addon-contributed
   backends transparently."

  (headless-id [this]
    "Return the keyword identifier for this headless backend.
     Examples: :claude-sdk, :claude-process, :openrouter
     Used by the headless-registry for backend selection and by ling spawn
     logic for mode resolution. Must be stable -- used as dispatch key.")

  (headless-spawn! [this ctx opts]
    "Spawn a headless session/process using this backend's mechanism.
     Arguments:
       ctx  - Ling context map {:keys [id cwd presets project-id model]}
       opts - Spawn options map {:keys [task buffer-capacity env-extra agents]}
     Returns: String slave-id on success.
     Throws: ex-info on spawn failure with {:id str :error str}
     Mirrors: ILingStrategy/strategy-spawn! [this ling-ctx opts]")

  (headless-dispatch! [this ctx task-opts]
    "Dispatch a task to a running headless session.
     Arguments:
       ctx       - Ling context map {:keys [id]}
       task-opts - Task options {:keys [task timeout-ms] :or {timeout-ms 60000}}
     Returns: Result channel, true, or other backend-specific result.
     Throws: ex-info on dispatch failure with {:ling-id str :error str}
     Mirrors: ILingStrategy/strategy-dispatch! [this ling-ctx task-opts]")

  (headless-status [this ctx ds-status]
    "Get headless-specific liveness and status information.
     Arguments:
       ctx       - Ling context map {:keys [id]}
       ds-status - DataScript status map, may be nil or {:slave/status kw}
     Returns: Status map {:slave/id str :slave/status kw} or nil.
     May include backend-specific keys (e.g. :sdk-phase for SDK backend).
     Mirrors: ILingStrategy/strategy-status [this ling-ctx ds-status]")

  (headless-kill! [this ctx]
    "Terminate the headless session/process using this backend's mechanism.
     Arguments: ctx - Ling context map {:keys [id]}
     Returns: {:killed? true :id str} or {:killed? false :id str :reason kw}
     Mirrors: ILingStrategy/strategy-kill! [this ling-ctx]")

  (headless-interrupt! [this ctx]
    "Interrupt the current query/task of a running headless session.
     Arguments: ctx - Ling context map {:keys [id]}
     Returns: {:success? true :ling-id str} or {:success? false :ling-id str :reason kw}
     Backends that don't support interrupts return {:success? false :reason :not-supported}.
     Mirrors: ILingStrategy/strategy-interrupt! [this ling-ctx]"))

;; =============================================================================
;; Capability Declaration
;; =============================================================================

(defprotocol IHeadlessCapabilities
  "Optional protocol for headless backends to declare their capabilities.
   Used by the registry for capability queries and by core dispatch for
   feature gating. Backends that don't implement this protocol are assumed
   to have basic capabilities only (#{:cap/streaming :cap/multi-turn})."

  (declared-capabilities [this]
    "Return the set of HeadlessCapability keywords this backend supports.
     E.g. #{:cap/hooks :cap/interrupts :cap/subagents :cap/checkpointing}"))

;; =============================================================================
;; Predicates
;; =============================================================================

(defn headless-backend?
  "Check if object implements IHeadlessBackend.
   Does NOT require IAddon -- in-tree backends (OpenRouter) may implement
   IHeadlessBackend without being full addons."
  [x]
  (satisfies? IHeadlessBackend x))

(defn headless-addon?
  "Check if object implements both IAddon and IHeadlessBackend.
   Analogous to terminal-addon? in hive-mcp.addons.terminal."
  [x]
  (and (satisfies? proto/IAddon x)
       (satisfies? IHeadlessBackend x)))

(def ^:private default-capabilities
  "Default capabilities assumed for backends that don't implement IHeadlessCapabilities."
  #{:cap/streaming :cap/multi-turn})

(defn capabilities
  "Get declared capabilities for a headless backend.
   Falls back to default-capabilities if IHeadlessCapabilities not satisfied."
  [backend]
  (if (satisfies? IHeadlessCapabilities backend)
    (declared-capabilities backend)
    default-capabilities))
