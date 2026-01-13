(ns hive-mcp.tools.swarm.registry
  "Lings registry - Track spawned lings for easy lookup.

   ADR-001 Phase 2: Event-Driven Registry Sync
   Subscribes to channel events from elisp to keep lings-registry in sync.
   Events: slave-spawned, slave-killed (from hive-mcp-swarm-events.el)

   SOLID: SRP - Single responsibility for ling registration/lookup.
   CLARITY: Y - Yield safe failure with graceful channel fallback."
  (:require [hive-mcp.channel :as ch]
            [clojure.core.async :as async :refer [go-loop <!]]
            [taoensso.timbre :as log]))

;; ============================================================
;; Lings Registry State
;; ============================================================

;; Atom tracking spawned lings: {slave-id {:name, :presets, :cwd, :spawned-at}}
(defonce lings-registry (atom {}))

;; State for event-driven registry synchronization.
(defonce ^:private registry-sync-state
  (atom {:running false
         :subscriptions []}))

;; ============================================================
;; Registry CRUD Operations
;; ============================================================

(defn register-ling!
  "Register a spawned ling in the registry.

   SOLID: SRP - Only handles registration, not events."
  [slave-id {:keys [name presets cwd]}]
  (swap! lings-registry assoc slave-id
         {:name name
          :presets presets
          :cwd cwd
          :spawned-at (System/currentTimeMillis)}))

(defn unregister-ling!
  "Remove a ling from the registry.

   SOLID: SRP - Only handles unregistration, not events."
  [slave-id]
  (swap! lings-registry dissoc slave-id))

(defn get-available-lings
  "Get all registered lings.

   Returns: {slave-id {:name, :presets, :cwd, :spawned-at}}"
  []
  @lings-registry)

(defn clear-registry!
  "Clear all entries from the registry.

   Used when killing all slaves."
  []
  (reset! lings-registry {}))

;; ============================================================
;; Event-Driven Sync (ADR-001 Phase 2)
;; ============================================================

(defn- handle-ling-registered
  "Handle slave-spawned event from elisp (registry sync).
   Event: {:slave-id :name :presets :cwd}
   Registers the ling in lings-registry.

   CLARITY: I - Inputs are guarded (handles both string and keyword keys)"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))
        name (or (get event "name") (:name event))
        presets (or (get event "presets") (:presets event) [])
        cwd (or (get event "cwd") (:cwd event))]
    (when slave-id
      (log/info "Registry sync: registering ling" slave-id "via event")
      (register-ling! slave-id {:name name :presets presets :cwd cwd}))))

(defn- handle-ling-unregistered
  "Handle slave-killed event from elisp (registry sync).
   Event: {:slave-id}
   Unregisters the ling from lings-registry.

   CLARITY: I - Inputs are guarded (handles both string and keyword keys)"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))]
    (when slave-id
      (log/info "Registry sync: unregistering ling" slave-id "via event")
      (unregister-ling! slave-id))))

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
