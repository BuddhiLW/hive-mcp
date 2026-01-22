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
   - Phase 3: Single source of truth achieved (future)

   DRY Patterns (Sprint-2):
   - `get-field`: Unified event field extraction (string or keyword keys)
   - `dispatch-event!`: Safe event dispatch with error handling
   - Handler functions use consistent field extraction pattern"
  (:require [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.coordinator :as coord]
            [hive-mcp.channel :as channel]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.hooks :as hooks]
            [hive-mcp.tools.memory.scope :as scope]
            [clojure.core.async :as async :refer [go-loop <!]]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private sync-state
  (atom {:running false
         :subscriptions []}))

(defonce ^:private hooks-registry-atom (atom nil))

(defn set-hooks-registry!
  "Inject the hooks registry from server.clj to avoid cyclic dependency."
  [registry]
  (reset! hooks-registry-atom registry)
  (log/info "Sync: hooks registry injected"))

(defn get-hooks-registry
  "Get the injected hooks registry."
  []
  @hooks-registry-atom)

;; =============================================================================
;; DRY Helpers (Sprint-2 Refactoring)
;; =============================================================================

(defn- get-field
  "Extract field from event map, handling both string and keyword keys.
   DRY pattern: All handlers use this instead of (or (get event \"x\") (:x event))."
  ([event field] (get-field event field nil))
  ([event field default]
   (or (get event (name field))
       (get event field)
       default)))

(defn- dispatch-event!
  "Dispatch event to hive-events system with error handling.
   DRY pattern: Replaces repeated try/require/resolve/catch blocks."
  [event-vec]
  (try
    (require '[hive-mcp.events.core :as ev])
    ((resolve 'hive-mcp.events.core/dispatch) event-vec)
    (catch Exception e
      (log/warn "Event dispatch failed:" (.getMessage e)))))

;; =============================================================================
;; Hook Action Execution (Layer 4 - Architectural Guarantee)
;; =============================================================================

(defn- trigger-task-complete-hooks!
  "Trigger :task-complete hooks via event dispatch.

   ARCHITECTURAL GUARANTEE: Ensures a shout is emitted on task completion,
   regardless of whether the ling explicitly called hivemind_shout."
  [task-id slave-id]
  (dispatch-event! [:task/shout-complete {:task-id task-id :agent-id slave-id}])
  (log/debug "Layer4: Dispatched :task/shout-complete for" slave-id)
  (when-let [registry (get-hooks-registry)]
    (let [merged-ctx {:type :task-complete :task-id task-id :agent-id slave-id}
          results (hooks/trigger-hooks registry :task-complete merged-ctx)]
      (log/debug "Layer4: Triggered" (count results) "custom hooks")
      results)))

;; =============================================================================
;; Event Handlers
;; =============================================================================

(defn- handle-slave-spawned
  "Handle slave spawn event. Event: {:slave-id :name :parent-id :depth :cwd}
   Derives project-id from cwd for project-scoped swarm operations."
  [event]
  (let [slave-id (get-field event :slave-id)
        name (get-field event :name slave-id)
        depth (get-field event :depth 1)
        parent-id (get-field event :parent-id)
        cwd (get-field event :cwd)
        project-id (when cwd (scope/get-current-project-id cwd))]
    (when slave-id
      (ds/add-slave! slave-id {:status :idle :name name :depth depth
                               :parent parent-id :cwd cwd :project-id project-id})
      (log/debug "Sync: registered slave" slave-id "depth:" depth "project-id:" project-id))))

(defn- handle-slave-status
  "Handle slave status change event. Event: {:slave-id :status}"
  [event]
  (let [slave-id (get-field event :slave-id)
        status (some-> (get-field event :status) keyword)]
    (when (and slave-id status)
      (ds/update-slave! slave-id {:slave/status status})
      (log/debug "Sync: updated slave status" slave-id "->" status))))

(defn- handle-slave-killed
  "Handle slave killed event. Event: {:slave-id}
   EVENTS-07: Dispatches :ling/completed BEFORE removing from DataScript."
  [event]
  (let [slave-id (get-field event :slave-id)]
    (when slave-id
      (dispatch-event! [:ling/completed {:slave-id slave-id :reason "terminated"}])
      (ds/remove-slave! slave-id)
      (log/debug "Sync: removed slave" slave-id))))

(defn- handle-task-dispatched
  "Handle task dispatch event. Event: {:task-id :slave-id :files}

   ADR-002 FIX: Updates slave status to :working when task is dispatched.
   This ensures swarm_status accurately reflects ling activity."
  [event]
  (let [task-id (get-field event :task-id)
        slave-id (get-field event :slave-id)
        files (get-field event :files [])]
    (when (and task-id slave-id)
      (ds/add-task! task-id slave-id {:status :dispatched :files files})
      ;; ADR-002 FIX: Set slave status to :working on dispatch
      (ds/update-slave! slave-id {:slave/status :working})
      (doseq [f files]
        (ds/claim-file! f slave-id task-id))
      (log/debug "Sync: registered task" task-id "with" (count files) "files, slave now :working"))))

(defn- handle-task-completed
  "Handle task completion event. Event: {:task-id :slave-id}
   LAYER 4: Triggers hooks for synthetic shout (architectural guarantee)."
  [event]
  (let [task-id (get-field event :task-id)
        slave-id (get-field event :slave-id)]
    (when task-id
      (ds/complete-task! task-id)
      (dispatch-event! [:task/complete {:task-id task-id :agent-id slave-id :result :completed}])
      (trigger-task-complete-hooks! task-id slave-id)
      (when-let [ready (seq (coord/process-queue!))]
        (log/info "Sync:" (count ready) "queued tasks now ready"))
      (log/debug "Sync: task completed" task-id))))

(defn- handle-task-failed
  "Handle task failure event. Event: {:task-id :error}"
  [event]
  (let [task-id (get-field event :task-id)]
    (when task-id
      (ds/fail-task! task-id :error)
      (coord/process-queue!)
      (log/debug "Sync: task failed" task-id))))

(defn- handle-prompt-shown
  "Handle permission prompt event. Event: {:slave-id :prompt :timestamp :session-id}"
  [event]
  (let [slave-id (get-field event :slave-id)
        prompt (get-field event :prompt)
        timestamp (get-field event :timestamp)
        session-id (get-field event :session-id)]
    (when (and slave-id prompt)
      (hivemind/add-swarm-prompt! slave-id prompt session-id timestamp)
      (log/info "Sync: forwarded prompt from" slave-id "to hivemind"))))

(defn- handle-prompt-stall
  "Handle prompt-stall event. Event: {:slave-id :idle-duration-secs :prompt-preview :urgency}"
  [event]
  (let [slave-id (get-field event :slave-id)
        idle-secs (get-field event :idle-duration-secs 0)
        prompt-preview (get-field event :prompt-preview "")
        urgency (get-field event :urgency "high")]
    (when slave-id
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

(defn- register-slave-from-status!
  "Register a single slave from Emacs status data.
   Derives project-id from cwd for project-scoped swarm operations."
  [slave]
  (let [slave-id (:slave-id slave)
        status (keyword (:status slave))
        name (or (:name slave) slave-id)
        depth (or (:depth slave) 1)
        cwd (:cwd slave)
        project-id (when cwd (scope/get-current-project-id cwd))]
    (ds/add-slave! slave-id {:status status :name name :depth depth
                             :cwd cwd :project-id project-id})
    (log/debug "Sync: bootstrapped slave" slave-id status "project-id:" project-id)))

(defn full-sync-from-emacs!
  "One-time full sync from Emacs swarm state. Call on startup to bootstrap."
  []
  (log/info "Starting full sync from Emacs...")
  (ds/reset-conn!)
  (let [elisp "(json-encode (hive-mcp-swarm-api-status))"
        {:keys [success result error]} (ec/eval-elisp-with-timeout elisp 5000)]
    (if success
      (try
        (let [status (json/read-str result :key-fn keyword)
              slaves (or (:slaves-detail status) [])]
          (run! register-slave-from-status! slaves)
          (log/info "Full sync complete:" (count slaves) "slaves"))
        (catch Exception e
          (log/error "Failed to parse Emacs swarm status:" (.getMessage e))))
      (log/warn "Could not fetch Emacs swarm status:" error))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn start-sync!
  "Start event-driven synchronization with Emacs.
   Options: :bootstrap? - If true, do full sync from Emacs first (default: true)"
  ([] (start-sync! {:bootstrap? true}))
  ([{:keys [bootstrap?] :or {bootstrap? true}}]
   (if (:running @sync-state)
     (do (log/warn "Sync already running") @sync-state)
     (do
       (log/info "Starting logic database sync...")
       (when bootstrap? (full-sync-from-emacs!))
       (let [subs (mapv (fn [[et h]] (subscribe-to-event! et h)) event-handlers)]
         (reset! sync-state {:running true :subscriptions subs})
         (log/info "Logic database sync started -" (count subs) "event subscriptions")
         @sync-state)))))

(defn stop-sync!
  "Stop event synchronization."
  []
  (when (:running @sync-state)
    (run! async/close! (:subscriptions @sync-state))
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
  (start-sync!)
  (sync-status)
  (handle-slave-spawned {:slave-id "test-slave-1" :name "test" :depth 1})
  (handle-task-dispatched {:task-id "task-1" :slave-id "test-slave-1" :files ["/src/core.clj"]})
  (ds/dump-db)
  (stop-sync!))
