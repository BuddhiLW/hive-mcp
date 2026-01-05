(ns hive-mcp.swarm.sync
  "Synchronize logic database with Emacs swarm state.

   Uses channel.clj event bus to receive state updates from Emacs
   and keep the logic database in sync for accurate conflict detection.

   Events handled:
   - :slave-spawned    - Register new slave in logic db
   - :slave-status     - Update slave status
   - :slave-killed     - Remove slave and release claims
   - :task-dispatched  - Register task and file claims
   - :task-completed   - Mark task complete, release claims, process queue
   - :task-failed      - Mark task failed, release claims, process queue
   - :prompt-shown     - Forward permission prompts to hivemind coordinator"
  (:require [hive-mcp.swarm.logic :as logic]
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.channel :as channel]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.hivemind :as hivemind]
            [clojure.core.async :as async :refer [go go-loop <!]]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private sync-state
  (atom {:running false
         :subscriptions []}))

;; =============================================================================
;; Event Handlers
;; =============================================================================

(defn- handle-slave-spawned
  "Handle slave spawn event.
   Event: {:slave-id :name :parent-id :depth}"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))
        status :idle]
    (when slave-id
      (logic/add-slave! slave-id status)
      (log/debug "Sync: registered slave" slave-id))))

(defn- handle-slave-status
  "Handle slave status change event.
   Event: {:slave-id :status}"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))
        status (keyword (or (get event "status") (:status event)))]
    (when (and slave-id status)
      (logic/update-slave-status! slave-id status)
      (log/debug "Sync: updated slave status" slave-id "->" status))))

(defn- handle-slave-killed
  "Handle slave killed event.
   Event: {:slave-id}"
  [event]
  (let [slave-id (or (get event "slave-id") (:slave-id event))]
    (when slave-id
      (logic/release-claims-for-slave! slave-id)
      (logic/remove-slave! slave-id)
      (log/debug "Sync: removed slave" slave-id))))

(defn- handle-task-dispatched
  "Handle task dispatch event.
   Event: {:task-id :slave-id :files}"
  [event]
  (let [task-id (or (get event "task-id") (:task-id event))
        slave-id (or (get event "slave-id") (:slave-id event))
        files (or (get event "files") (:files event) [])]
    (when (and task-id slave-id)
      (logic/add-task! task-id slave-id :dispatched)
      ;; Register file claims
      (doseq [f files]
        (logic/add-claim! f slave-id)
        (logic/add-task-file! task-id f))
      (log/debug "Sync: registered task" task-id "with" (count files) "files"))))

(defn- handle-task-completed
  "Handle task completion event.
   Event: {:task-id :slave-id}"
  [event]
  (let [task-id (or (get event "task-id") (:task-id event))]
    (when task-id
      (logic/update-task-status! task-id :completed)
      (logic/release-claims-for-task! task-id)
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
      (logic/update-task-status! task-id :error)
      (logic/release-claims-for-task! task-id)
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
   :prompt-shown handle-prompt-shown})

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
   Call this on startup to bootstrap the logic database."
  []
  (log/info "Starting full sync from Emacs...")
  (logic/reset-db!)

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
                    slave-status (keyword (:status slave))]
                (logic/add-slave! slave-id slave-status)
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
   :db-stats (logic/db-stats)})

(comment
  ;; Development REPL examples

  ;; Start sync
  (start-sync!)

  ;; Check status
  (sync-status)

  ;; Manual test: simulate events
  (handle-slave-spawned {:slave-id "test-slave-1"})
  (handle-task-dispatched {:task-id "task-1"
                           :slave-id "test-slave-1"
                           :files ["/src/core.clj"]})
  (logic/dump-db)

  ;; Stop sync
  (stop-sync!))
