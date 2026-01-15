(ns hive-mcp.swarm.sync
  "Synchronize DataScript database with Emacs swarm state.

   Uses channel.clj event bus to receive state updates from Emacs
   and keep the DataScript database in sync for accurate conflict detection.

   Events handled:
   - :slave-spawned    - Register new slave in DataScript
   - :slave-status     - Update slave status
   - :slave-killed     - Remove slave and release claims
   - :task-dispatched  - Register task and file claims
   - :task-completed   - Mark task complete, release claims, process queue
   - :task-failed      - Mark task failed, release claims, process queue
   - :prompt-shown     - Forward permission prompts to hivemind coordinator
   - :prompt-stall     - Urgent alert when ling idle with pending prompt

   Migration Note (ADR-001):
   - Phase 1: Uses DataScript for swarm state (replacing core.logic pldb)
   - Phase 2: Elisp queries DataScript via MCP (future)
   - Phase 3: Single source of truth achieved (future)"
  (:require [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.logic :as logic] ;; Keep for coordinator compatibility
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.channel :as channel]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.hooks :as hooks]
            ;; MIGRATION NOTE: handlers require removed - using event dispatch instead (P5-5)
            [clojure.core.async :as async :refer [go-loop <!]]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private sync-state
  (atom {:running false
         :subscriptions []}))

;; Injected hooks registry - set via set-hooks-registry! to avoid cyclic deps
(defonce ^:private hooks-registry-atom (atom nil))

(defn set-hooks-registry!
  "Inject the hooks registry from server.clj to avoid cyclic dependency.
   Called during server initialization."
  [registry]
  (reset! hooks-registry-atom registry)
  (log/info "Sync: hooks registry injected"))

(defn get-hooks-registry
  "Get the injected hooks registry."
  []
  @hooks-registry-atom)

;; =============================================================================
;; Hook Action Execution (Layer 4 - Architectural Guarantee)
;; =============================================================================

(defn- trigger-task-complete-hooks!
  "Trigger :task-complete hooks via event dispatch.

   ARCHITECTURAL GUARANTEE: This function ensures a shout is emitted
   on task completion, regardless of whether the ling explicitly called
   hivemind_shout. Part of 4-layer convergence pattern.

   MIGRATION (P5-5): Uses event dispatch instead of direct handler calls.
   The :task/shout-complete event produces a :shout effect which is
   executed by the effects system.

   Returns: Vector of custom hook results"
  [task-id slave-id]
  ;; Dispatch event - the :task/shout-complete handler produces a :shout effect
  ;; which is executed by the registered effect handler (Layer 4 guarantee)
  (try
    (require '[hive-mcp.events.core :as ev])
    ((resolve 'hive-mcp.events.core/dispatch)
     [:task/shout-complete {:task-id task-id
                            :agent-id slave-id}])
    (log/debug "Layer4: Dispatched :task/shout-complete for" slave-id)
    (catch Exception e
      (log/warn "Layer4: Task shout-complete dispatch failed:" (.getMessage e))))

  ;; Also trigger any custom hooks registered via the registry
  ;; These receive merged event/context (single-arg handlers)
  (when-let [registry (get-hooks-registry)]
    (let [event {:type :task-complete
                 :task-id task-id}
          context {:agent-id slave-id
                   :task-id task-id}
          custom-results (hooks/trigger-hooks registry :task-complete
                                              (merge event context))]
      (log/debug "Layer4: Triggered" (count custom-results) "custom hooks")
      custom-results)))

;; =============================================================================
;; Event Handlers
;; =============================================================================

(defn- handle-slave-spawned
  "Handle slave spawn event.
   Event: {:slave-id :name :parent-id :depth}"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))
        name (or (get event "name") (:name event) slave-id)
        depth (or (get event "depth") (:depth event) 1)
        parent-id (or (get event "parent-id") (:parent-id event))]
    (when slave-id
      (ds/add-slave! slave-id {:status :idle
                               :name name
                               :depth depth
                               :parent parent-id})
      (log/debug "Sync: registered slave" slave-id "depth:" depth))))

(defn- handle-slave-status
  "Handle slave status change event.
   Event: {:slave-id :status}"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))
        status (keyword (or (get event "status") (:status event)))]
    (when (and slave-id status)
      (ds/update-slave! slave-id {:slave/status status})
      (log/debug "Sync: updated slave status" slave-id "->" status))))

(defn- handle-slave-killed
  "Handle slave killed event.
   Event: {:slave-id}
   
   EVENTS-07: Dispatches :ling/completed event BEFORE removing from DataScript
   to allow event handlers to access ling state before cleanup."
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))]
    (when slave-id
      ;; EVENTS-07: Dispatch BEFORE removal so handlers can access ling state
      (try
        (require '[hive-mcp.events.core :as ev])
        ((resolve 'hive-mcp.events.core/dispatch)
         [:ling/completed {:slave-id slave-id :reason "terminated"}])
        (catch Exception e
          (log/warn "Event dispatch for ling termination failed:" (.getMessage e))))
      ;; ds/remove-slave! handles claim release internally
      (ds/remove-slave! slave-id)
      (log/debug "Sync: removed slave" slave-id))))

(defn- handle-task-dispatched
  "Handle task dispatch event.
   Event: {:task-id :slave-id :files}"
  [event]
  (let [task-id (or (get event "task-id") (:task-id event))
        slave-id (or (get event "slave-id") (:slave-id event))
        files (or (get event "files") (:files event) [])]
    (when (and task-id slave-id)
      ;; Add task with files stored in :task/files
      (ds/add-task! task-id slave-id {:status :dispatched :files files})
      ;; Register file claims linked to task
      (doseq [f files]
        (ds/claim-file! f slave-id task-id))
      (log/debug "Sync: registered task" task-id "with" (count files) "files"))))

(defn- handle-task-completed
  "Handle task completion event.
   Event: {:task-id :slave-id}

   LAYER 4 INTEGRATION: Triggers :task-complete hooks which emit
   a synthetic shout. This is the architectural guarantee - completion
   is always visible to the coordinator regardless of ling behavior.
   
   EVENTS-02: Also dispatches to hive-events for unified event processing."
  [event]
  (let [task-id (or (get event "task-id") (:task-id event))
        slave-id (or (get event "slave-id") (:slave-id event))]
    (when task-id
      ;; ds/complete-task! handles status update, claim release, and slave stats
      (ds/complete-task! task-id)

      ;; EVENTS-02: Dispatch to hive-events for unified event processing
      (try
        (require '[hive-mcp.events.core :as ev])
        ((resolve 'hive-mcp.events.core/dispatch)
         [:task/complete {:task-id task-id :agent-id slave-id :result :completed}])
        (catch Exception e
          (log/warn "Event dispatch failed:" (.getMessage e))))

      ;; LAYER 4: Trigger hooks for synthetic shout (architectural guarantee)
      ;; Even if ling forgot to call hivemind_shout, this ensures visibility
      (trigger-task-complete-hooks! task-id slave-id)

      ;; Process queue - some waiting tasks might be ready now
      (let [ready (coord/process-queue!)]
        (when (seq ready)
          (log/info "Sync: " (count ready) "queued tasks now ready")))
      (log/debug "Sync: task completed" task-id))))

(defn- handle-task-failed
  "Handle task failure event.
   Event: {:task-id :error}"
  [event]
  (let [task-id (or (get event "task-id") (:task-id event))]
    (when task-id
      ;; ds/fail-task! handles status update and claim release
      (ds/fail-task! task-id :error)
      ;; Process queue - claims released
      (coord/process-queue!)
      (log/debug "Sync: task failed" task-id))))

(defn- handle-prompt-shown
  "Handle permission prompt event from Emacs swarm slave.
   Event: {:slave-id :prompt :timestamp :session-id}

   Forwards the prompt to hivemind so the coordinator can see it
   via hivemind_status without polling swarm_pending_prompts."
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))
        prompt (or (get event "prompt") (:prompt event))
        timestamp (or (get event "timestamp") (:timestamp event))
        session-id (or (get event "session-id") (:session-id event))]
    (when (and slave-id prompt)
      (hivemind/add-swarm-prompt! slave-id prompt session-id timestamp)
      (log/info "Sync: forwarded prompt from" slave-id "to hivemind"))))

(defn- handle-prompt-stall
  "Handle prompt-stall event from Emacs idle watcher.
   Event: {:slave-id :idle-duration-secs :prompt-preview :urgency}

   This is emitted when a ling has been idle AND has a pending prompt.
   Records as a shout so it appears in HIVEMIND piggyback messages,
   alerting the coordinator that they need to respond urgently."
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))
        idle-secs (or (get event "idle-duration-secs") (:idle-duration-secs event) 0)
        prompt-preview (or (get event "prompt-preview") (:prompt-preview event) "")
        urgency (or (get event "urgency") (:urgency event) "high")]
    (when slave-id
      ;; Record as a shout so it appears in HIVEMIND piggyback
      (hivemind/shout! slave-id :prompt-stall
                       {:task "awaiting-response"
                        :message (format "STALLED %.0fs waiting for prompt response: %s"
                                         idle-secs prompt-preview)
                        :idle-secs idle-secs
                        :urgency urgency})
      (log/warn "Sync: prompt-stall from" slave-id "- idle" idle-secs "secs"))))

;; =============================================================================
;; Event Subscription Management
;; =============================================================================

(def ^:private event-handlers
  "Map of event types to handler functions."
  {:slave-spawned handle-slave-spawned
   :slave-status handle-slave-status
   :slave-killed handle-slave-killed
   :task-dispatched handle-task-dispatched
   :task-completed handle-task-completed
   :task-failed handle-task-failed
   :prompt-shown handle-prompt-shown
   :prompt-stall handle-prompt-stall})

(defn- subscribe-to-event!
  "Subscribe to a single event type with handler."
  [event-type handler]
  (let [sub-ch (channel/subscribe! event-type)]
    (go-loop []
      (when-let [event (<! sub-ch)]
        (try
          (handler event)
          (catch Exception e
            (log/error "Sync handler error for" event-type ":" (.getMessage e))))
        (recur)))
    sub-ch))

;; =============================================================================
;; Full Sync from Emacs (Bootstrap)
;; =============================================================================

(defn full-sync-from-emacs!
  "One-time full sync from Emacs swarm state.
   Call this on startup to bootstrap the DataScript database."
  []
  (log/info "Starting full sync from Emacs...")
  (ds/reset-conn!)

  ;; Get current swarm status from Emacs
  (let [elisp "(json-encode (hive-mcp-swarm-api-status))"
        {:keys [success result error]} (ec/eval-elisp-with-timeout elisp 5000)]
    (if success
      (try
        (let [status (json/read-str result :key-fn keyword)]
          ;; Register all active slaves
          (when-let [slaves (:slaves-detail status)]
            (doseq [slave slaves]
              (let [slave-id (:slave-id slave)
                    slave-status (keyword (:status slave))
                    name (or (:name slave) slave-id)
                    depth (or (:depth slave) 1)]
                (ds/add-slave! slave-id {:status slave-status
                                         :name name
                                         :depth depth})
                (log/debug "Sync: bootstrapped slave" slave-id slave-status))))

          ;; Note: Task claims would need to be tracked by Emacs side
          ;; For now, we start fresh and track from dispatch forward

          (log/info "Full sync complete:"
                    (count (or (:slaves-detail status) [])) "slaves"))
        (catch Exception e
          (log/error "Failed to parse Emacs swarm status:" (.getMessage e))))
      (log/warn "Could not fetch Emacs swarm status:" error))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn start-sync!
  "Start event-driven synchronization with Emacs.
   Subscribes to channel events and updates logic database.

   Options:
   - :bootstrap? - If true, do full sync from Emacs first (default: true)"
  ([] (start-sync! {:bootstrap? true}))
  ([{:keys [bootstrap?] :or {bootstrap? true}}]
   (if (:running @sync-state)
     (do
       (log/warn "Sync already running")
       @sync-state)
     (do
       (log/info "Starting logic database sync...")

       ;; Bootstrap from current Emacs state
       (when bootstrap?
         (full-sync-from-emacs!))

       ;; Subscribe to all event types
       (let [subs (mapv (fn [[event-type handler]]
                          (subscribe-to-event! event-type handler))
                        event-handlers)]
         (reset! sync-state
                 {:running true
                  :subscriptions subs})
         (log/info "Logic database sync started -" (count subs) "event subscriptions")
         @sync-state)))))

(defn stop-sync!
  "Stop event synchronization."
  []
  (when (:running @sync-state)
    (doseq [sub (:subscriptions @sync-state)]
      (async/close! sub))
    (reset! sync-state {:running false :subscriptions []})
    (log/info "Logic database sync stopped")))

(defn sync-status
  "Get current sync status."
  []
  {:running (:running @sync-state)
   :subscription-count (count (:subscriptions @sync-state))
   :db-stats (ds/db-stats)})

(comment
  ;; Development REPL examples

  ;; Start sync
  (start-sync!)

  ;; Check status
  (sync-status)

  ;; Manual test: simulate events
  (handle-slave-spawned {:slave-id "test-slave-1" :name "test" :depth 1})
  (handle-task-dispatched {:task-id "task-1"
                           :slave-id "test-slave-1"
                           :files ["/src/core.clj"]})
  (ds/dump-db)

  ;; Stop sync
  (stop-sync!))
