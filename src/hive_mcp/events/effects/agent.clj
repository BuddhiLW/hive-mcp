(ns hive-mcp.events.effects.agent
  "Agent/swarm effect handlers for the hive-mcp event system.

   Effects implemented:
   - :dispatch-task      - Dispatch task to swarm slave (POC-07)
   - :swarm-send-prompt  - Send prompt to ling terminal (Agora Turn Relay)
   - :agora/continue     - Continue debate asynchronously (P0)
   - :agora/execute-drone - Execute drone turn (alias for :agora/continue)
   - :saa/run-workflow   - Run SAA workflow via FSM (async)
   - :saa/tool-gate      - Restrict tools per SAA phase (via saa-fx)
   - :saa/context-inject  - Inject context at phase transitions (via saa-fx)
   - :saa/shout           - Hivemind progress for SAA phases (via saa-fx)

   Usage:
   ```clojure
   (require '[hive-mcp.events.effects.agent :as agent-effects])
   (agent-effects/register-agent-effects!)
   ```

   SOLID: Single Responsibility - agent/swarm effect execution only
   CLARITY: Y - Yield safe failure (effects catch and log errors)"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.saa-fx :as saa-fx]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.validation :as v]
            [hive-mcp.emacsclient :as ec]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
    :files    [\"src/core.clj\"]}  ; optional, for file claim tracking"
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
    :prompt   \"The prompt to send\"}"
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
;; Effect: :saa/run-workflow (SAA FSM Integration)
;; =============================================================================

(defn- handle-saa-run-workflow
  "Execute a :saa/run-workflow effect - run SAA workflow via FSM.

   Triggers the SAA (Silence-Abstract-Act) workflow asynchronously.
   Uses the compiled FSM from the workflow registry, falling back to
   the inline spec if the registry isn't available.

   Expected data shape:
   {:task       \"Fix auth bug in login flow\"  ; required
    :agent-id   \"swarm-ling-123\"              ; required
    :directory  \"/path/to/project\"            ; required
    :plan-only? false                           ; optional, default false
    :resources  {...}}                          ; optional, override resources map

   Note: Runs in a future to avoid blocking the event loop.
   Dispatches :saa/completed or :saa/failed events on completion."
  [{:keys [task agent-id directory plan-only? resources] :as _data}]
  (when (and task agent-id)
    (future
      (try
        (require 'hive-mcp.workflows.saa-workflow)
        (let [run-fn (if plan-only?
                       (resolve 'hive-mcp.workflows.saa-workflow/run-plan-only)
                       (resolve 'hive-mcp.workflows.saa-workflow/run-full-saa))
              ;; Build minimal resources if not provided
              default-resources {:scope-fn (fn [dir]
                                             (try
                                               (let [scope-fn (requiring-resolve
                                                               'hive-mcp.swarm.scope/get-current-project-id)]
                                                 (scope-fn dir))
                                               (catch Exception _ "unknown")))
                                 :shout-fn (fn [aid phase msg]
                                             (hivemind/shout! aid :progress
                                                              {:workflow :saa
                                                               :phase phase
                                                               :message msg}))
                                 :clock-fn #(java.time.Instant/now)}
              effective-resources (merge default-resources resources)
              opts {:task task
                    :agent-id agent-id
                    :directory directory}
              result (run-fn effective-resources opts)]
          ;; Dispatch completion event
          (when-let [dispatch-fn (requiring-resolve 'hive-mcp.events.core/dispatch)]
            (dispatch-fn [:saa/completed (merge (select-keys result
                                                             [:agent-id :task :plan-memory-id
                                                              :kanban-task-ids :plan-only?
                                                              :tests-passed? :grounding-score])
                                                {:agent-id agent-id})])))
        (catch Exception e
          (log/error "[EVENT] SAA workflow failed:" (.getMessage e))
          ;; Dispatch failure event
          (try
            (when-let [dispatch-fn (requiring-resolve 'hive-mcp.events.core/dispatch)]
              (dispatch-fn [:saa/failed {:agent-id agent-id
                                         :task task
                                         :phase :unknown
                                         :error (.getMessage e)}]))
            (catch Exception _ nil)))))))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-agent-effects!
  "Register all agent/swarm effect handlers.

   Effects registered:
   - :dispatch-task       - Dispatch task to swarm slave (POC-07)
   - :swarm-send-prompt   - Send prompt to ling terminal (Agora Turn Relay)
   - :agora/continue      - Async debate continuation (P0)
   - :agora/execute-drone - Execute drone turn (alias for :agora/continue)
   - :saa/run-workflow    - Run SAA workflow via FSM (async)
   - :saa/tool-gate       - Restrict tools per SAA phase
   - :saa/context-inject   - Inject context at phase transitions
   - :saa/shout            - Hivemind progress for SAA phases

   Called from hive-mcp.events.effects/register-effects!"
  []
  (ev/reg-fx :dispatch-task handle-dispatch-task)
  (ev/reg-fx :swarm-send-prompt handle-swarm-send-prompt)
  (ev/reg-fx :agora/continue handle-agora-continue)
  (ev/reg-fx :agora/execute-drone handle-agora-continue)
  (ev/reg-fx :saa/run-workflow handle-saa-run-workflow)
  ;; SAA FX (delegated to saa_fx.clj for SRP compliance)
  (saa-fx/register-saa-fx!)
  (log/info "[hive-events.agent] Agent effects registered: :dispatch-task :swarm-send-prompt :agora/continue :agora/execute-drone :saa/run-workflow :saa/tool-gate :saa/context-inject :saa/shout"))
