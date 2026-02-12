(ns hive-mcp.tools.swarm.registry
  "Lings registry - DataScript-backed compatibility layer for ling registration and lookup."
  (:require [hive-mcp.channel.core :as ch]
            [hive-mcp.hivemind.core :as hivemind]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.memory.scope :as scope]
            [clojure.core.async :as async :refer [go-loop <!]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private registry-sync-state
  (atom {:running false
         :subscriptions []}))

(defn- ds-slave->legacy-format
  "Transform DataScript slave entity to legacy registry format."
  [slave]
  {(:slave/id slave)
   {:name (:slave/name slave)
    :presets (vec (or (:slave/presets slave) []))
    :cwd (:slave/cwd slave)
    :project-id (:slave/project-id slave)
    :spawned-at (when-let [ts (:slave/created-at slave)]
                  (.getTime ts))}})

(defn register-ling!
  "Register a spawned ling in DataScript."
  [slave-id {:keys [name presets cwd kanban-task-id]}]
  (let [project-id (when cwd (scope/get-current-project-id cwd))]
    (ds/add-slave! slave-id {:name name
                             :presets (vec (or presets []))
                             :cwd cwd
                             :project-id project-id
                             :kanban-task-id kanban-task-id})))

(defn unregister-ling!
  "Remove a ling from the registry."
  [slave-id]
  (ds/remove-slave! slave-id))

(defn get-available-lings
  "Get all registered lings as {slave-id {:name, :presets, :cwd, :spawned-at}}."
  []
  (let [slaves (ds/get-all-slaves)]
    (if (seq slaves)
      (reduce merge {} (map ds-slave->legacy-format slaves))
      {})))

(defn clear-registry!
  "Clear all entries from the registry."
  []
  (ds/reset-conn!))

(defn- handle-ling-registered
  "Handle slave-spawned event from elisp for registry sync."
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
      ;; RACE GUARD: Only register if not already in DataScript.
      ;; The JVM spawn! method adds the slave with :working status when a task
      ;; is provided. Without this guard, register-ling! would overwrite that
      ;; status with the :idle default from ds/add-slave! destructuring.
      (when-not (ds/get-slave slave-id)
        (register-ling! slave-id metadata))
      (hivemind/register-agent! slave-id metadata))))

(defn- handle-ling-unregistered
  "Handle slave-killed event from elisp for registry sync."
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))]
    (when slave-id
      (log/info "Registry sync: unregistering ling" slave-id "via event")
      (unregister-ling! slave-id)
      (hivemind/clear-agent! slave-id))))

(defn- subscribe-to-registry-event!
  "Subscribe to a registry sync event type with handler."
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
  "Start event-driven synchronization of lings-registry."
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
