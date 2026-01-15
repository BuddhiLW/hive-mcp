(ns hive-mcp.events.effects
  "Concrete effect implementations for the hive-mcp event system.

   Registers handlers for side-effects described in event :effects maps.

   Effects implemented:
   - :shout          - Broadcast to hivemind coordinator
   - :log            - Log a message
   - :ds-transact    - Execute DataScript transaction
   - :wrap-notify    - Record ling wrap for coordinator permeation
   - :channel-publish - Emit event to WebSocket channel (POC-05)
   - :memory-write   - Add entry to Chroma memory (POC-06)
   - :dispatch-task  - Dispatch task to swarm slave (POC-07)
   - :report-metrics - Report event system metrics (CLARITY-T: Telemetry)
   - :dispatch       - Chain to another event (event composition)

   Usage:
   ```clojure
   (require '[hive-mcp.events.effects :as effects])
   (effects/register-effects!)
   ```

   SOLID: Single Responsibility - effect execution only
   CLARITY: Y - Yield safe failure (effects catch and log errors)"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.channel :as channel]
            [hive-mcp.tools.memory.crud :as memory-crud]
            [datascript.core :as d]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Effect: :shout
;; =============================================================================

(defn- handle-shout
  "Execute a :shout effect - broadcast to hivemind.

   Expected data shape:
   {:agent-id   \"swarm-worker-123\"
    :event-type :progress | :completed | :error | :blocked | :started
    :data       {:task \"...\" :message \"...\" ...}}"
  [{:keys [agent-id event-type data]}]
  (let [effective-id (or agent-id
                         (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                         "unknown-agent")]
    (hivemind/shout! effective-id event-type data)))

;; =============================================================================
;; Effect: :log
;; =============================================================================

(defn- handle-log
  "Execute a :log effect - log a message.

   Accepts either a string or a map:
   - String: logged at info level
   - Map: {:level :info/:warn/:error :message \"...\"}"
  [data]
  (cond
    (string? data)
    (log/info "[EVENT]" data)

    (map? data)
    (let [{:keys [level message]} data
          level (or level :info)]
      (case level
        :debug (log/debug "[EVENT]" message)
        :info (log/info "[EVENT]" message)
        :warn (log/warn "[EVENT]" message)
        :error (log/error "[EVENT]" message)
        (log/info "[EVENT]" message)))

    :else
    (log/info "[EVENT]" (str data))))

;; =============================================================================
;; Effect: :ds-transact
;; =============================================================================

(defn- handle-ds-transact
  "Execute a :ds-transact effect - DataScript transaction.

   Expected data: vector of transaction data (datoms or tx-maps).

   Example:
   {:ds-transact [{:slave/id \"worker-1\" :slave/status :working}]}"
  [tx-data]
  (when (seq tx-data)
    (let [conn (ds/get-conn)]
      (d/transact! conn tx-data))))

;; =============================================================================
;; Effect: :wrap-notify
;; =============================================================================

(defn- handle-wrap-notify
  "Execute a :wrap-notify effect - record ling wrap to DataScript.

   Expected data shape:
   {:wrap-id     \"wrap-uuid\"  ; auto-generated if not provided
    :agent-id    \"ling-123\"
    :session-id  \"session:2026-01-14:ling-123\"
    :created-ids [\"note-1\" \"note-2\"]
    :stats       {:notes 2 :decisions 0}}"
  [{:keys [wrap-id agent-id session-id created-ids stats]}]
  (let [wid (or wrap-id (str "wrap-" (java.util.UUID/randomUUID)))]
    (ds/add-wrap-notification! wid
                               {:agent-id agent-id
                                :session-id session-id
                                :created-ids created-ids
                                :stats stats})))

;; =============================================================================
;; Effect: :channel-publish (EVENTS-04)
;; =============================================================================

(defn- handle-channel-publish
  "Execute a :channel-publish effect - emit to WebSocket channel.

   Broadcasts event to all connected Emacs clients and local subscribers.

   Expected data shape:
   {:event-type :task-completed | :ling-spawned | :memory-added | ...
    :data       {:key \"value\" ...}}

   Example:
   {:channel-publish {:event-type :task-completed
                      :data {:task-id \"123\" :result \"done\"}}}"
  [{:keys [event-type data]}]
  (when event-type
    (channel/emit-event! event-type (or data {}))))

;; =============================================================================
;; Effect: :memory-write (EVENTS-04)
;; =============================================================================

(defn- handle-memory-write
  "Execute a :memory-write effect - add entry to Chroma memory.

   Creates a new memory entry via the memory CRUD system.
   Automatically handles duplicate detection and tag merging.

   Expected data shape:
   {:type      \"note\" | \"snippet\" | \"convention\" | \"decision\"
    :content   \"The content to store\"
    :tags      [\"tag1\" \"tag2\"]     ; optional
    :duration  \"ephemeral\" | \"short\" | \"medium\" | \"long\" | \"permanent\" ; optional, default: long
    :directory \"/path/to/project\"}   ; optional, for scoping

   Example:
   {:memory-write {:type \"decision\"
                   :content \"Use re-frame pattern for events\"
                   :tags [\"architecture\" \"events\"]
                   :duration \"permanent\"}}"
  [{:keys [type content] :as data}]
  (when (and type content)
    (try
      (memory-crud/handle-add data)
      (log/debug "[EVENT] Memory entry created:" type)
      (catch Exception e
        (log/error "[EVENT] Memory write failed:" (.getMessage e))))))

;; =============================================================================
;; Effect: :report-metrics (CLARITY-T: Telemetry)
;; =============================================================================

(defn- handle-report-metrics
  "Execute a :report-metrics effect - report metrics to external system.

   Reports current event system metrics for observability.
   Currently supports :log destination. Future: :prometheus, :statsd.

   Expected data shape:
   {:destination :log | :prometheus | :statsd}  ; default: :log

   Example:
   {:report-metrics {:destination :log}}
   
   CLARITY Principle: Telemetry first - observable system behavior."
  [{:keys [destination] :or {destination :log}}]
  (let [metrics (ev/get-metrics)]
    (case destination
      :log (log/info "[METRICS] Event system metrics:" metrics)
      ;; Future: :prometheus, :statsd
      (log/warn "[METRICS] Unknown metrics destination:" destination))))

;; =============================================================================
;; Effect: :dispatch (Event chaining)
;; =============================================================================

(defn- handle-dispatch
  "Execute a :dispatch effect - dispatch another event.

   Enables event chaining where one handler can trigger another.
   Used for composition (e.g., :ling/completed dispatches :session/end).

   Expected data: An event vector like [:event-id data]

   Example:
   {:dispatch [:session/end {:slave-id \"ling-123\"}]}
   
   Note: Uses async dispatch via future to prevent stack overflow on deep chains."
  [event]
  (when (and (vector? event) (keyword? (first event)))
    ;; Use async dispatch via future to prevent stack overflow
    (future
      (try
        (ev/dispatch event)
        (catch Exception e
          (log/error "[EVENT] Dispatch chain failed:" (.getMessage e)))))))

;; =============================================================================
;; Effect: :git-commit (P5-2)
;; =============================================================================

(defn- handle-git-commit
  "Execute a :git-commit effect - stage files and create commit.

   Stages specified files and creates a git commit with the given message.
   Uses shell commands via clojure.java.shell.

   Expected data shape:
   {:files   [\"src/a.clj\" \"src/b.clj\"]
    :message \"feat: Add new feature\"
    :task-id \"task-123\"}  ; optional, for logging

   Example:
   {:git-commit {:files [\"src/core.clj\"]
                 :message \"fix: Fix the bug\"}}"
  [{:keys [files message task-id]}]
  (when (and (seq files) message)
    (try
      (let [add-result (apply clojure.java.shell/sh
                              "git" "add" "--" (vec files))
            commit-result (clojure.java.shell/sh
                           "git" "commit" "-m" message)]
        (if (zero? (:exit commit-result))
          (log/info "[EVENT] Git commit created:" message
                    (when task-id (str " (task: " task-id ")")))
          (log/warn "[EVENT] Git commit failed:" (:err commit-result))))
      (catch Exception e
        (log/error "[EVENT] Git commit error:" (.getMessage e))))))

;; =============================================================================
;; Effect: :kanban-sync (P5-4)
;; =============================================================================

(defn- handle-kanban-sync
  "Execute a :kanban-sync effect - synchronize kanban state.

   Syncs kanban tasks between memory and external sources.
   Currently logs the intent; actual sync uses existing kanban infrastructure.

   Expected data shape:
   {:project   \"hive-mcp\"
    :direction :bidirectional | :push | :pull}

   Example:
   {:kanban-sync {:project \"hive-mcp\"
                  :direction :bidirectional}}"
  [{:keys [project direction]}]
  (when project
    (try
      ;; TODO: Wire to actual kanban sync infrastructure
      ;; For now, emit channel event for Emacs-side sync
      (channel/emit-event! :kanban-sync {:project project
                                         :direction (or direction :bidirectional)})
      (log/info "[EVENT] Kanban sync triggered:" project
                (str "direction=" (or direction :bidirectional)))
      (catch Exception e
        (log/error "[EVENT] Kanban sync error:" (.getMessage e))))))

;; =============================================================================
;; Effect: :dispatch-task (POC-07)
;; =============================================================================

(defn- handle-dispatch-task
  "Execute a :dispatch-task effect - dispatch task to swarm slave.

   Routes task through coordinator for conflict checking and queue management.
   If approved, task proceeds immediately. If conflicts exist, task is queued.

   Expected data shape:
   {:slave-id \"swarm-worker-123\"
    :prompt   \"Implement the feature\"
    :files    [\"src/core.clj\"]}  ; optional, for file claim tracking

   Example:
   {:dispatch-task {:slave-id \"swarm-ling-1\"
                    :prompt \"Fix authentication bug\"
                    :files [\"src/auth.clj\"]}}"
  [{:keys [slave-id prompt files]}]
  (when (and slave-id prompt)
    (try
      (let [result (coordinator/dispatch-or-queue!
                    {:slave-id slave-id
                     :prompt prompt
                     :files files})]
        (case (:action result)
          :dispatch (log/info "[EVENT] Task dispatched to" slave-id)
          :queued (log/info "[EVENT] Task queued for" slave-id
                            "- position:" (:position result))
          :blocked (log/warn "[EVENT] Task blocked for" slave-id
                             "- reason:" (:reason result)))
        result)
      (catch Exception e
        (log/error "[EVENT] Task dispatch failed:" (.getMessage e))))))

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-effects!
  "Register all concrete effect handlers and POC coeffects.

   Safe to call multiple times - only registers once.

   Effects registered:
   - :shout          - Broadcast to hivemind coordinator
   - :log            - Simple logging
   - :ds-transact    - DataScript transactions
   - :wrap-notify    - Record ling wrap to DataScript
   - :channel-publish - Emit to WebSocket channel
   - :memory-write   - Add entry to Chroma memory
   - :report-metrics - Report metrics
   - :dispatch       - Chain to another event
   - :git-commit     - Stage files and create git commit (P5-2)
   - :kanban-sync    - Synchronize kanban state (P5-4)
   - :dispatch-task  - Dispatch task to swarm slave (POC-07)

   Coeffects registered (POC-08/09/10):
   - :now            - Current timestamp in milliseconds
   - :agent-context  - Agent ID and current working directory
   - :db             - DataScript database snapshot

   Returns true if effects were registered, false if already registered."
  []
  (when-not @*registered
    ;; ==========================================================================
    ;; Coeffects (POC-08/09/10)
    ;; ==========================================================================

    ;; POC-08: :now coeffect - Current timestamp in milliseconds
    (ev/reg-cofx :now
                 (fn [coeffects]
                   (assoc coeffects :now (System/currentTimeMillis))))

    ;; POC-09: :agent-context coeffect - Agent environment info
    (ev/reg-cofx :agent-context
                 (fn [coeffects]
                   (assoc coeffects :agent-context
                          {:agent-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                           :cwd (System/getProperty "user.dir")})))

    ;; POC-10: :db-snapshot coeffect - DataScript database snapshot
    ;; Injects as :db-snapshot key (consistent with core.clj)
    (ev/reg-cofx :db-snapshot
                 (fn [coeffects]
                   (assoc coeffects :db-snapshot @(ds/get-conn))))

    ;; ==========================================================================
    ;; Effects
    ;; ==========================================================================

    ;; :shout - Broadcast to hivemind coordinator
    (ev/reg-fx :shout handle-shout)

    ;; :log - Simple logging
    (ev/reg-fx :log handle-log)

    ;; :ds-transact - DataScript transactions
    (ev/reg-fx :ds-transact handle-ds-transact)

    ;; :wrap-notify - Record ling wrap to DataScript
    (ev/reg-fx :wrap-notify handle-wrap-notify)

    ;; :channel-publish - Emit to WebSocket channel (EVENTS-04)
    (ev/reg-fx :channel-publish handle-channel-publish)

    ;; :memory-write - Add entry to Chroma memory (EVENTS-04)
    (ev/reg-fx :memory-write handle-memory-write)

    ;; :report-metrics - Report metrics to external system (CLARITY-T: Telemetry)
    (ev/reg-fx :report-metrics handle-report-metrics)

    ;; :dispatch - Chain events (for :ling/completed -> :session/end)
    (ev/reg-fx :dispatch handle-dispatch)

    ;; :git-commit - Stage files and create git commit (P5-2)
    (ev/reg-fx :git-commit handle-git-commit)

    ;; :kanban-sync - Synchronize kanban state (P5-4)
    (ev/reg-fx :kanban-sync handle-kanban-sync)

    ;; :dispatch-task - Dispatch task to swarm slave (POC-07)
    (ev/reg-fx :dispatch-task handle-dispatch-task)

    ;; Register event handlers that produce these effects
    (ev/reg-event :crystal/wrap-notify []
                  (fn [_coeffects event]
                    (let [{:keys [agent-id session-id created-ids stats]} (second event)]
                      {:wrap-notify {:agent-id agent-id
                                     :session-id session-id
                                     :created-ids created-ids
                                     :stats stats}
                       :log {:level :info
                             :message (str "Wrap notification from " agent-id)}})))

    (reset! *registered true)
    (log/info "[hive-events] Coeffects registered: :now :agent-context :db-snapshot")
    (log/info "[hive-events] Effects registered: :shout :log :ds-transact :wrap-notify :channel-publish :memory-write :report-metrics :dispatch :git-commit :kanban-sync :dispatch-task")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
