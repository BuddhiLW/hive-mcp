(ns hive-mcp.events.handlers.system
  "Event handlers for system-level events (Telemetry Phase 1).

   Handles:
   - :system/error - Structured error telemetry

   CLARITY: Telemetry first - observable system behavior.
   SOLID: SRP - System-level event handling only."
  (:require [hive-mcp.events.core :as ev]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; :system/error Handler
;; =============================================================================

(defn- handle-system-error
  "Handle :system/error event - emit structured error for telemetry.

   Produces :emit-system-error effect to:
   1. Log with structured format
   2. Emit to WebSocket channel
   3. Store in DataScript

   Event shape:
   [:system/error {:error-type :harvest-failed
                   :source \"hooks/harvest-session-progress\"
                   :message \"Emacs unreachable\"
                   :context {:fn \"...\"}}]"
  [coeffects [_ error-data]]
  (log/debug "[SYSTEM] Error event received:" (:error-type error-data))
  {:emit-system-error error-data})

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-handlers!
  "Register system event handlers. Call at startup.

   Handlers registered:
   - :system/error - Structured error telemetry

   Safe to call multiple times."
  []
  (when-not @*registered
    (ev/reg-event :system/error [] handle-system-error)
    (reset! *registered true)
    (log/info "[hive-events] System handlers registered: :system/error")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
