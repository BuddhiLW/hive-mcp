(ns hive-mcp.tools.swarm.registry
  "Lings registry - Track spawned lings for easy lookup.

   ADR-002 Amendment: DataScript IS the unified registry.
   This module provides a compatibility layer over DataScript for
   ling registration/lookup operations.

   Events: slave-spawned, slave-killed (from hive-mcp-swarm-events.el)

   SOLID: SRP - Single responsibility for ling registration/lookup.
   CLARITY: Y - Yield safe failure with graceful channel fallback."
  (:require [hive-mcp.channel :as ch]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.memory.scope :as scope]
            [clojure.core.async :as async :refer [go-loop <!]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Lings Registry State (Migrated to DataScript - ADR-002)
;; ============================================================

;; State for event-driven registry synchronization.
(defonce ^:private registry-sync-state
  (atom {:running false
         :subscriptions []}))

;; ============================================================
;; Registry CRUD Operations (DataScript-backed - ADR-002)
;; ============================================================

(defn- ds-slave->legacy-format
  "Transform DataScript slave entity to legacy registry format.

   DataScript: {:slave/id :slave/name :slave/presets :slave/cwd :slave/project-id :slave/created-at}
   Legacy:     {slave-id {:name :presets :cwd :project-id :spawned-at}}"
  [slave]
  {(:slave/id slave)
   {:name (:slave/name slave)
    :presets (vec (or (:slave/presets slave) []))
    :cwd (:slave/cwd slave)
    :project-id (:slave/project-id slave)
    :spawned-at (when-let [ts (:slave/created-at slave)]
                  (.getTime ts))}})

(defn register-ling!
  "Register a spawned ling in the registry.

   ADR-002: Writes to DataScript as primary store.
   Derives project-id from cwd for project-scoped operations.

   SOLID: SRP - Only handles registration, not events."
  [slave-id {:keys [name presets cwd kanban-task-id]}]
  ;; Derive project-id from cwd for project-scoped operations (e.g., swarm_kill 'all')
  (let [project-id (when cwd (scope/get-current-project-id cwd))]
    ;; Primary: DataScript
    (ds/add-slave! slave-id {:name name
                             :presets (vec (or presets []))
                             :cwd cwd
                             :project-id project-id
                             :kanban-task-id kanban-task-id})))

(defn unregister-ling!
  "Remove a ling from the registry.

   ADR-002: Removes from DataScript as primary store.

   SOLID: SRP - Only handles unregistration, not events."
  [slave-id]
  ;; Primary: DataScript
  (ds/remove-slave! slave-id))

(defn get-available-lings
  "Get all registered lings.

   ADR-002: Reads from DataScript as primary source.

   Returns: {slave-id {:name, :presets, :cwd, :spawned-at}}"
  []
  ;; Primary: Read from DataScript
  (let [slaves (ds/get-all-slaves)]
    (if (seq slaves)
      (reduce merge {} (map ds-slave->legacy-format slaves))
      {})))

(defn clear-registry!
  "Clear all entries from the registry.

   ADR-002: Clears DataScript as primary store.

   Used when killing all slaves."
  []
  ;; Primary: DataScript
  (ds/reset-conn!))

;; ============================================================
;; Event-Driven Sync (ADR-001 Phase 2)
;; ============================================================

(defn- handle-ling-registered
  "Handle slave-spawned event from elisp (registry sync).
   Event: {:slave-id :name :presets :cwd :kanban-task-id}
   Registers the ling in BOTH registries:
   - lings-registry (for swarm operations)
   - hivemind agent-registry (for hivemind_messages)

   CLARITY: I - Inputs are guarded (handles both string and keyword keys)"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))
        name (or (get event "name") (:name event))
        presets (or (get event "presets") (:presets event) [])
        cwd (or (get event "cwd") (:cwd event))
        kanban-task-id (or (get event "kanban-task-id") (:kanban-task-id event))
        metadata {:name name :presets presets :cwd cwd :kanban-task-id kanban-task-id}]
    (when slave-id
      (log/info "Registry sync: registering ling" slave-id "via event"
                (when kanban-task-id (str "kanban-task:" kanban-task-id)))
      ;; Register in swarm lings registry
      (register-ling! slave-id metadata)
      ;; Also register in hivemind agent-registry (bug fix!)
      ;; This ensures hivemind_messages works for spawned agents
      (hivemind/register-agent! slave-id metadata))))

(defn- handle-ling-unregistered
  "Handle slave-killed event from elisp (registry sync).
   Event: {:slave-id}
   Unregisters the ling from BOTH registries.

   CLARITY: I - Inputs are guarded (handles both string and keyword keys)"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))]
    (when slave-id
      (log/info "Registry sync: unregistering ling" slave-id "via event")
      ;; Unregister from swarm registry
      (unregister-ling! slave-id)
      ;; Also clear from hivemind agent-registry
      (hivemind/clear-agent! slave-id))))

(defn- subscribe-to-registry-event!
  "Subscribe to a registry sync event type with handler.
   Returns the subscription channel.

   CLARITY: Y - Yield safe failure (logs errors, continues processing)"
  [event-type handler]
  (let [sub-ch (ch/subscribe! event-type)]
    (go-loop []
      (when-let [event (<! sub-ch)]
        (try
          (handler event)
          (catch Exception e
            (log/error "Registry sync handler error for" event-type ":" (.getMessage e))))
        (recur)))
    sub-ch))

(defn start-registry-sync!
  "Start event-driven synchronization of lings-registry.
   Subscribes to slave-spawned and slave-killed events
   from elisp channel to keep Clojure registry in sync.

   CLARITY: Y - Yield safe failure (handles channel not available gracefully)."
  []
  (if (:running @registry-sync-state)
    (do
      (log/warn "Registry sync already running")
      @registry-sync-state)
    (try
      (let [subs [(subscribe-to-registry-event! :slave-spawned handle-ling-registered)
                  (subscribe-to-registry-event! :slave-killed handle-ling-unregistered)]]
        (reset! registry-sync-state
                {:running true
                 :subscriptions subs})
        (log/info "Registry sync started - 2 event subscriptions active")
        @registry-sync-state)
      (catch Exception e
        (log/warn "Registry sync failed to start (channel may not be available):" (.getMessage e))
        @registry-sync-state))))

(defn stop-registry-sync!
  "Stop event-driven registry synchronization."
  []
  (when (:running @registry-sync-state)
    (doseq [sub (:subscriptions @registry-sync-state)]
      (async/close! sub))
    (reset! registry-sync-state {:running false :subscriptions []})
    (log/info "Registry sync stopped")))

(defn registry-sync-running?
  "Check if registry sync is currently running."
  []
  (:running @registry-sync-state))
