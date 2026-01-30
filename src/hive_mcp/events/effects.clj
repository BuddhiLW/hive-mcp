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
            [hive-mcp.events.effects.kg :as kg-effects]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.channel :as channel]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.validation :as v]
            [hive-mcp.emacsclient :as ec]
            [clojure.java.shell :as shell]
            [datascript.core :as d]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Injected Handler State
;; =============================================================================

(defonce ^:private memory-write-handler (atom nil))
(defonce ^:private wrap-crystallize-handler (atom nil))

(defn set-memory-write-handler!
  "Set the handler function for :memory-write effect.
   Called during server initialization to wire infrastructure layer.

   Example:
     (set-memory-write-handler! memory-crud/handle-add)"
  [f]
  (reset! memory-write-handler f))

(defn set-wrap-crystallize-handler!
  "Set the handler function for :wrap-crystallize effect.
   Called during server initialization to wire tools layer.

   DIP: Events layer uses this injection point instead of importing tools/crystal.

   Example:
     (set-wrap-crystallize-handler! crystal/handle-wrap-crystallize)"
  [f]
  (reset! wrap-crystallize-handler f))

;; =============================================================================
;; Effect: :shout
;; =============================================================================

(defn- handle-shout
  "Execute a :shout effect - broadcast to hivemind.

   Expected data shape:
   {:agent-id   \"swarm-worker-123\"
    :event-type :progress | :completed | :error | :blocked | :started
    :data       {:task \"...\" :message \"...\" ...}}

   P1 FIX: Fallback chain includes ctx/current-agent-id for drone context."
  [{:keys [agent-id event-type data]}]
  ;; P1 FIX: Check context for drone attribution
  ;; Uses hive-mcp.agent.context (no circular dep) for thread-local agent-id
  (let [get-ctx-agent-id (try
                           (requiring-resolve 'hive-mcp.agent.context/current-agent-id)
                           (catch Exception _ nil))
        effective-id (or agent-id
                         (when get-ctx-agent-id (get-ctx-agent-id))
                         (System/getenv "CLAUDE_SWARM_SLAVE_ID")
                         "unknown-agent")]
    (hivemind/shout! effective-id event-type data)))

;; =============================================================================
;; Effect: :targeted-shout (File Claim Cascade)
;; =============================================================================

(defn- handle-targeted-shout
  "Execute a :targeted-shout effect - send shout to specific agent.

   Like :shout but explicitly targets a specific agent-id rather than
   using the current agent from environment. Used for notifying lings
   about events that affect them (e.g., file availability).

   Expected data shape:
   {:target-agent-id \"ling-worker-123\"  ; Agent to receive the shout
    :event-type      :file-available     ; Type of event
    :data            {:file \"...\" ...}}  ; Event payload

   Example:
   {:targeted-shout {:target-agent-id \"swarm-ling-task-1\"
                     :event-type :file-available
                     :data {:file \"src/core.clj\"}}}"
  [{:keys [target-agent-id event-type data]}]
  (when target-agent-id
    (hivemind/shout! target-agent-id event-type data)))

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
    :project-id  \"hive-mcp\"   ; project ID for scoping (from ling's directory)
    :created-ids [\"note-1\" \"note-2\"]
    :stats       {:notes 2 :decisions 0}}"
  [{:keys [wrap-id agent-id session-id project-id created-ids stats]}]
  (let [wid (or wrap-id (str "wrap-" (java.util.UUID/randomUUID)))]
    (ds/add-wrap-notification! wid
                               {:agent-id agent-id
                                :session-id session-id
                                :project-id project-id
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
    (if-let [handler @memory-write-handler]
      (try
        (handler data)
        (log/debug "[EVENT] Memory entry created:" type)
        (catch Exception e
          (log/error "[EVENT] Memory write failed:" (.getMessage e))))
      (log/warn "[EVENT] Memory write handler not configured - call set-memory-write-handler! during initialization"))))

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
;; Effect: :emit-system-error (Telemetry Phase 1)
;; =============================================================================

(defn- handle-emit-system-error
  "Execute an :emit-system-error effect - emit structured error for telemetry.

   Provides structured error handling for catastrophic failures:
   1. Logs with structured format for searchability
   2. Emits to WebSocket channel for Emacs visibility
   3. Stores in DataScript for post-mortem analysis

   Expected data shape:
   {:error-type :harvest-failed | :component-failed | :restart-collision | :emacs-unreachable
    :source     \"hooks/harvest-session-progress\"
    :message    \"Emacs unreachable\"
    :context    {:fn \"harvest-session-progress\" :attempt 1}}

   Example:
   {:emit-system-error {:error-type :harvest-failed
                        :source \"hooks/harvest-session-progress\"
                        :message \"Connection refused\"
                        :context {:fn \"harvest-session-progress\"}}}

   CLARITY Principle: Telemetry first - observable system behavior."
  [{:keys [error-type source message context] :as data}]
  (let [timestamp (System/currentTimeMillis)
        error-data (assoc data :timestamp timestamp)]
    ;; 1. Log with structured format
    (log/error "[SYSTEM-ERROR]"
               {:error-type error-type
                :source source
                :message message
                :context context
                :timestamp timestamp})

    ;; 2. Emit to WebSocket channel for Emacs visibility
    (try
      (when (channel/server-connected?)
        (channel/emit-event! :system-error error-data))
      (catch Exception e
        (log/warn "[SYSTEM-ERROR] Failed to emit to channel:" (.getMessage e))))

    ;; 3. Store in DataScript for post-mortem analysis
    (try
      (let [conn (ds/get-conn)]
        (d/transact! conn [{:error/type :system-error
                            :error/error-type error-type
                            :error/source source
                            :error/message message
                            :error/context (pr-str context)
                            :error/timestamp timestamp}]))
      (catch Exception e
        (log/warn "[SYSTEM-ERROR] Failed to store in DataScript:" (.getMessage e))))))

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
;; Effect: :dispatch-n (File Claim Cascade)
;; =============================================================================

(defn- handle-dispatch-n
  "Execute a :dispatch-n effect - dispatch multiple events in sequence.

   Like :dispatch but accepts a vector of events to dispatch.
   Events are dispatched sequentially using async futures.
   Useful when one event needs to trigger multiple follow-up events.

   Expected data: Vector of event vectors.

   Example:
   {:dispatch-n [[:claim/notify-waiting {:target \"ling-1\" :file \"a.clj\"}]
                 [:claim/notify-waiting {:target \"ling-2\" :file \"a.clj\"}]]}

   Note: Uses async dispatch via futures to prevent stack overflow."
  [events]
  (when (and (sequential? events) (seq events))
    (doseq [event events]
      (when (and (vector? event) (keyword? (first event)))
        ;; Use async dispatch via future to prevent stack overflow
        (future
          (try
            (ev/dispatch event)
            (catch Exception e
              (log/error "[EVENT] Dispatch-n chain failed for" (first event) ":" (.getMessage e)))))))))

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
      (let [_add-result (apply shell/sh
                               "git" "add" "--" (vec files))
            commit-result (shell/sh
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
;; Effect: :kanban-move-done (Session Complete)
;; =============================================================================

(defn- handle-kanban-move-done
  "Execute a :kanban-move-done effect - move multiple tasks to done.

   Moves each specified kanban task to done status via the memory kanban system.
   Part of the session_complete workflow.

   Expected data shape:
   {:task-ids  [\"task-1\" \"task-2\"]
    :directory \"/project/path\"}  ; optional, for scoping

   Example:
   {:kanban-move-done {:task-ids [\"kanban-uuid-1\" \"kanban-uuid-2\"]}}"
  [{:keys [task-ids directory]}]
  (when (seq task-ids)
    (doseq [task-id task-ids]
      (try
        ;; Use emacsclient to call the kanban move function
        (let [dir-arg (if directory
                        (str "\"" directory "\"")
                        "nil")
              elisp (format "(json-encode (hive-mcp-api-kanban-move %s \"done\" %s))"
                            (str "\"" task-id "\"")
                            dir-arg)
              {:keys [success error timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
          (cond
            timed-out (log/warn "[EVENT] Kanban move for" task-id "timed out (10s) - continuing")
            success   (log/info "[EVENT] Kanban task moved to done:" task-id)
            :else     (log/warn "[EVENT] Kanban move failed for" task-id ":" error)))
        (catch Exception e
          (log/error "[EVENT] Kanban move error for" task-id ":" (.getMessage e)))))))

;; =============================================================================
;; Effect: :wrap-crystallize (Session Complete)
;; =============================================================================

(defn- handle-wrap-crystallize
  "Execute a :wrap-crystallize effect - run wrap crystallization.

   Triggers the wrap crystallize workflow to persist session learnings
   to long-term memory. Part of the session_complete workflow.

   DIP: Uses injected handler (set via set-wrap-crystallize-handler!)
   to avoid layer inversion (events importing tools).

   Expected data shape:
   {:agent-id \"ling-123\"}

   Example:
   {:wrap-crystallize {:agent-id \"swarm-ling-worker-456\"}}"
  [{:keys [agent-id]}]
  (if-let [handler @wrap-crystallize-handler]
    (try
      (handler {:agent_id agent-id})
      (log/info "[EVENT] Wrap crystallize completed for:" agent-id)
      (catch Exception e
        (log/error "[EVENT] Wrap crystallize failed:" (.getMessage e))))
    (log/warn "[EVENT] Wrap crystallize handler not configured - call set-wrap-crystallize-handler! during initialization")))

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
;; Effect: :swarm-send-prompt (Agora Turn Relay)
;; =============================================================================

(defn- handle-swarm-send-prompt
  "Execute a :swarm-send-prompt effect - send prompt to ling terminal.

   Sends a prompt directly to a ling's terminal for Agora dialogue relay.
   Uses elisp hive-mcp-swarm-send-to-terminal function.

   Expected data shape:
   {:slave-id \"swarm-ling-123\"
    :prompt   \"The prompt to send\"}

   Example:
   {:swarm-send-prompt {:slave-id \"swarm-ling-critic\"
                        :prompt \"[AGORA] Please review...\"}}"
  [{:keys [slave-id prompt]}]
  (when (and slave-id prompt)
    (try
      (let [elisp (format "(hive-mcp-swarm-send-to-terminal \"%s\" \"%s\")"
                          (v/escape-elisp-string slave-id)
                          (v/escape-elisp-string prompt))
            {:keys [success error timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
        (cond
          timed-out (log/warn "[EVENT] Agora prompt to" slave-id "timed out (10s) - continuing")
          success   (log/info "[EVENT] Sent Agora prompt to ling:" slave-id)
          :else     (log/warn "[EVENT] Failed to send Agora prompt to" slave-id ":" error)))
      (catch Exception e
        (log/error "[EVENT] Swarm send-prompt error:" (.getMessage e))))))

;; =============================================================================
;; Effect: :agora/continue (P0: Async Debate Continuation)
;; =============================================================================

(defn- handle-agora-continue
  "Execute an :agora/continue effect - continue debate asynchronously.

   Calls debate/continue-debate! in a future to avoid blocking the event loop.
   The continue-debate! function will emit another :agora/turn-completed event
   upon completion, creating the event-driven automation loop.

   Expected data shape:
   {:dialogue-id \"dialogue-uuid\"}

   Example:
   {:agora/continue {:dialogue-id \"abc-123\"}}

   Axiom: [ax: Drone Medium Limitations] - drones are single-shot,
   this effect orchestrates the sequence between turns."
  [{:keys [dialogue-id]}]
  (when dialogue-id
    (future
      (try
        (require 'hive-mcp.agora.debate)
        (let [continue-fn (resolve 'hive-mcp.agora.debate/continue-debate!)]
          (when continue-fn
            (log/debug "[EVENT] Continuing debate:" dialogue-id)
            (continue-fn dialogue-id)))
        (catch Exception e
          (log/error "[EVENT] Agora continue failed for" dialogue-id ":"
                     (.getMessage e)))))))

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-effects!
  "Register all concrete effect handlers and POC coeffects.

   Safe to call multiple times - only registers once.

   Effects registered:
   - :shout           - Broadcast to hivemind coordinator
   - :targeted-shout  - Send shout to specific agent (File Claim Cascade)
   - :log             - Simple logging
   - :ds-transact     - DataScript transactions
   - :wrap-notify     - Record ling wrap to DataScript
   - :channel-publish - Emit to WebSocket channel
   - :memory-write    - Add entry to Chroma memory
   - :report-metrics  - Report metrics
   - :dispatch        - Chain to another event
   - :dispatch-n      - Dispatch multiple events in sequence (File Claim Cascade)
   - :git-commit      - Stage files and create git commit (P5-2)
   - :kanban-sync     - Synchronize kanban state (P5-4)
   - :dispatch-task   - Dispatch task to swarm slave (POC-07)
   - :swarm-send-prompt - Send prompt to ling terminal (Agora Turn Relay)
   - :emit-system-error - Structured error telemetry (Telemetry Phase 1)
   - KG effects       - See hive-mcp.events.effects.kg

   Coeffects registered (POC-08/09/10/11):
   - :now             - Current timestamp in milliseconds
   - :agent-context   - Agent ID and current working directory
   - :db-snapshot     - DataScript database snapshot
   - :waiting-lings   - Query lings waiting on a specific file (File Claim Cascade)
   - :request-ctx     - Current request context from tool execution (agent-id, project-id, directory)

   Returns true if effects were registered, false if already registered."
  []
  (when-not @*registered
    ;; ==========================================================================
    ;; Coeffects (POC-08/09/10/11)
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

    ;; POC-11: :waiting-lings coeffect - Query lings waiting on a file
    ;; Used by :claim/file-released to find lings to notify
    ;; Note: This is a parameterized coeffect - takes file-path argument
    (ev/reg-cofx :waiting-lings
                 (fn [coeffects file-path]
                   (let [db @(ds/get-conn)
                         waiting (when (and db file-path)
                                   (d/q '[:find ?slave-id ?task-id
                                          :in $ ?file
                                          :where
                                          [?t :task/files ?file]
                                          [?t :task/status :queued]
                                          [?t :task/id ?task-id]
                                          [?t :task/slave ?s]
                                          [?s :slave/id ?slave-id]]
                                        db file-path))]
                     (assoc coeffects :waiting-lings
                            (mapv (fn [[slave-id task-id]]
                                    {:slave-id slave-id
                                     :task-id task-id})
                                  (or waiting []))))))

    ;; :request-ctx coeffect - Current request context from tool execution
    ;; Provides access to agent-id, project-id, directory bound during MCP tool execution
    ;; Uses hive-mcp.agent.context/request-ctx to get the thread-local context
    (ev/reg-cofx :request-ctx
                 (fn [coeffects]
                   (assoc coeffects :request-ctx (ctx/request-ctx))))

    ;; ==========================================================================
    ;; Effects
    ;; ==========================================================================

    ;; :shout - Broadcast to hivemind coordinator
    (ev/reg-fx :shout handle-shout)

    ;; :targeted-shout - Send shout to specific agent (File Claim Cascade)
    (ev/reg-fx :targeted-shout handle-targeted-shout)

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

    ;; :emit-system-error - Structured error telemetry (Telemetry Phase 1)
    (ev/reg-fx :emit-system-error handle-emit-system-error)

    ;; :dispatch - Chain events (for :ling/completed -> :session/end)
    (ev/reg-fx :dispatch handle-dispatch)

    ;; :dispatch-n - Dispatch multiple events (File Claim Cascade)
    (ev/reg-fx :dispatch-n handle-dispatch-n)

    ;; :git-commit - Stage files and create git commit (P5-2)
    (ev/reg-fx :git-commit handle-git-commit)

    ;; :kanban-sync - Synchronize kanban state (P5-4)
    (ev/reg-fx :kanban-sync handle-kanban-sync)

    ;; :dispatch-task - Dispatch task to swarm slave (POC-07)
    (ev/reg-fx :dispatch-task handle-dispatch-task)

    ;; :swarm-send-prompt - Send prompt to ling terminal (Agora Turn Relay)
    (ev/reg-fx :swarm-send-prompt handle-swarm-send-prompt)

    ;; :agora/continue - Async debate continuation (P0: Event-Driven Agora)
    (ev/reg-fx :agora/continue handle-agora-continue)

    ;; :agora/execute-drone - Execute drone turn (unified event chain)
    ;; Delegates to same handler as :agora/continue
    (ev/reg-fx :agora/execute-drone handle-agora-continue)

    ;; :kanban-move-done - Move kanban tasks to done (Session Complete)
    (ev/reg-fx :kanban-move-done handle-kanban-move-done)

    ;; :wrap-crystallize - Run wrap crystallization (Session Complete)
    (ev/reg-fx :wrap-crystallize handle-wrap-crystallize)

    ;; :tool-registry-refresh - Refresh tool handlers after hot-reload
    (ev/reg-fx :tool-registry-refresh
               (fn [_]
                 (try
                   (let [refresh-fn (requiring-resolve 'hive-mcp.agent.registry/refresh!)]
                     (refresh-fn))
                   (catch Exception e
                     (log/error "[hot-reload] Failed to refresh tool registry:" (ex-message e))))))

    ;; ==========================================================================
    ;; Knowledge Graph Effects (delegated to kg.clj for SRP compliance)
    ;; ==========================================================================
    (kg-effects/register-kg-effects!)

    ;; NOTE: :crystal/wrap-notify event handler is registered in
    ;; hive-mcp.events.handlers.crystal/register-handlers! with proper
    ;; defensive stats handling. Do NOT duplicate here.

    (reset! *registered true)
    (log/info "[hive-events] Coeffects registered: :now :agent-context :db-snapshot :waiting-lings :request-ctx")
    (log/info "[hive-events] Effects registered: :shout :targeted-shout :log :ds-transact :wrap-notify :channel-publish :memory-write :report-metrics :emit-system-error :dispatch :dispatch-n :git-commit :kanban-sync :dispatch-task :swarm-send-prompt :agora/continue :kanban-move-done :wrap-crystallize :tool-registry-refresh")
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing."
  []
  (reset! *registered false))
