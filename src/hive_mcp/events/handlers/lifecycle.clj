(ns hive-mcp.events.handlers.lifecycle
  "Lifecycle event handlers for GC sweep.

   Handles:
   - :lifecycle/sweep - Trigger bounded atom sweep across all registered atoms

   The sweep event produces a :lifecycle/sweep-fx effect that performs
   the actual eviction side-effect via the bounded atom registry.

   Part of gc-fix-4: prevents GC death spiral from unbounded atom growth."

  (:require [hive-mcp.events.core :as ev]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handler: :lifecycle/sweep
;; =============================================================================

(defn handle-lifecycle-sweep
  "Handler for :lifecycle/sweep events.

   Produces :lifecycle/sweep-fx effect to trigger bounded atom eviction.
   The handler is pure -- actual side-effects happen in the effect handler.

   Event shape:
   [:lifecycle/sweep {}]                     ; sweep all
   [:lifecycle/sweep {:atom-ids [:my-atom]}] ; sweep specific atoms (optional)

   Produces effects:
   - :lifecycle/sweep-fx - Execute the sweep
   - :log                - Log the sweep trigger"
  [_coeffects [_ data]]
  (let [atom-ids (:atom-ids data)]
    (log/debug "[lifecycle] Sweep event received, atom-ids:" atom-ids)
    {:lifecycle/sweep-fx {:atom-ids atom-ids}
     :log {:level :info
           :message (str "Lifecycle sweep triggered"
                         (when (seq atom-ids)
                           (str " for atoms: " (pr-str atom-ids))))}}))

;; =============================================================================
;; Convenience Function
;; =============================================================================

(defn trigger-sweep!
  "Convenience function to dispatch a :lifecycle/sweep event.

   Dispatches synchronously and returns the sweep stats from the effect.

   Usage:
   (trigger-sweep!)                        ; sweep all registered atoms
   (trigger-sweep! {:atom-ids [:my-atom]}) ; sweep specific atoms"
  ([]
   (trigger-sweep! {}))
  ([opts]
   (ev/dispatch [:lifecycle/sweep (or opts {})])))

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-handlers!
  "Register lifecycle event handlers. Call at startup.

   Handlers registered:
   - :lifecycle/sweep - Bounded atom GC sweep

   Safe to call multiple times."
  []
  (when-not @*registered
    (ev/reg-event :lifecycle/sweep [] handle-lifecycle-sweep)
    (reset! *registered true)
    (log/info "[hive-events] Lifecycle handlers registered: :lifecycle/sweep")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
