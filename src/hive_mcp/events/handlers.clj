(ns hive-mcp.events.handlers
  "Event handlers for hive-mcp events.

   This namespace is a FACADE that re-exports handlers from domain-specific modules:
   - handlers.task    - Task lifecycle (:task/complete, :task/shout-complete, :git/commit-modified)
   - handlers.ling    - Ling lifecycle (:ling/started, :ling/completed, :ling/ready-for-wrap)
   - handlers.session - Session lifecycle (:session/end, :session/wrap)
   - handlers.kanban  - Kanban state (:kanban/done, :kanban/sync)
   - handlers.crystal - Wrap/crystallize (:crystal/wrap-request, :crystal/wrap-notify)
   - handlers.wave    - Drone waves (:wave/start, :wave/item-done, :wave/complete)
   - handlers.drone   - Drone lifecycle (:drone/started, :drone/completed, :drone/failed)
   - handlers.claim   - File claims (:claim/file-released, :claim/notify-waiting)

   ## Usage
   ```clojure
   (require '[hive-mcp.events.handlers :as handlers])
   (handlers/register-handlers!)
   ```

   ## Available Events
   - :task/complete         - Signal task completion to hivemind
   - :task/shout-complete   - Broadcast completion with message (P5-1)
   - :git/commit-modified   - Git commit if files changed (P5-2)
   - :ling/started          - Ling spawned and initialized (EVENTS-03)
   - :ling/completed        - Ling finished all work (EVENTS-03)
   - :ling/ready-for-wrap   - Auto-wrap hook on ling completion
   - :session/end           - Session ending, trigger auto-wrap (EVENTS-06)
   - :session/wrap          - Trigger wrap workflow (P5-3)
   - :kanban/done           - Kanban task completed (EVENTS-09)
   - :kanban/sync           - Sync kanban at session end (P5-4)
   - :crystal/wrap-request  - Unified wrap path (Option A)
   - :crystal/wrap-notify   - Wrap notification for HIVEMIND piggyback
   - :wave/start            - Wave execution started
   - :wave/item-done        - Wave item completed/failed
   - :wave/complete         - Wave execution finished
   - :drone/started         - Drone spawned and began task (CLARITY-T)
   - :drone/completed       - Drone finished successfully (CLARITY-T)
   - :drone/failed          - Drone execution failed (CLARITY-T)
   - :claim/file-released   - File claim released, notify waiting lings
   - :claim/notify-waiting  - Send targeted shout to waiting ling
   - :system/error          - Structured error telemetry (Telemetry Phase 1)

   SOLID: SRP - Facade delegates to domain-specific modules
   CLARITY: R - Represented intent through clear module structure"
  (:require [hive-mcp.events.handlers.task :as task]
            [hive-mcp.events.handlers.ling :as ling]
            [hive-mcp.events.handlers.session :as session]
            [hive-mcp.events.handlers.kanban :as kanban]
            [hive-mcp.events.handlers.crystal :as crystal]
            [hive-mcp.events.handlers.wave :as wave]
            [hive-mcp.events.handlers.drone :as drone]
            [hive-mcp.events.handlers.claim :as claim]
            [hive-mcp.events.handlers.system :as system]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-handlers!
  "Register all event handlers. Call at startup.

   Safe to call multiple times - only registers once.

   Delegates to domain-specific modules:
   - task/register-handlers!    - Task lifecycle
   - ling/register-handlers!    - Ling lifecycle
   - session/register-handlers! - Session lifecycle
   - kanban/register-handlers!  - Kanban state
   - crystal/register-handlers! - Wrap/crystallize
   - wave/register-handlers!    - Drone waves
   - drone/register-handlers!   - Drone lifecycle (CLARITY-T)
   - claim/register-handlers!   - File claims
   - system/register-handlers!  - System telemetry (Phase 1)

   Returns true if handlers were registered, false if already registered."
  []
  (when-not @*registered
    ;; Register all domain handlers
    (task/register-handlers!)
    (ling/register-handlers!)
    (session/register-handlers!)
    (kanban/register-handlers!)
    (crystal/register-handlers!)
    (wave/register-handlers!)
    (drone/register-handlers!)
    (claim/register-handlers!)
    (system/register-handlers!)

    (reset! *registered true)
    (println "[hive-events] Handlers registered: :task/complete :task/shout-complete :git/commit-modified :ling/started :ling/completed :ling/ready-for-wrap :session/end :session/wrap :kanban/sync :kanban/done :crystal/wrap-request :crystal/wrap-notify :wave/start :wave/item-done :wave/complete :drone/started :drone/completed :drone/failed :claim/file-released :claim/notify-waiting :system/error")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
