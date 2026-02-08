(ns hive-mcp.agent.saa.fsm-orchestrator
  "FSM-backed ISAAOrchestrator implementation.

   Bridges the SAA workflow FSM engine to the ISAAOrchestrator protocol,
   enabling the three-phase SAA cycle to be driven by the deterministic
   state machine in hive-mcp.workflows.saa-workflow.

   Architecture:
   - Implements ISAAOrchestrator by delegating to FSM handler functions
   - Individual phases (run-silence!, run-abstract!, run-act!) call handlers directly
   - Full cycle (run-full-saa!) uses saa/run-full-saa for complete FSM execution
   - Resources map built from config, with shout-fn wired to output channel
   - Returns core.async channels per ISAAOrchestrator protocol contract
   - Uses async/thread (not go) since FSM handlers are blocking operations

   Comparison with SAAOrchestrator (agent/saa/orchestrator.clj):
   - SAAOrchestrator: Delegates to IAgentSession.query! (LLM-powered phases)
   - FSMSAAOrchestrator: Delegates to FSM handlers (deterministic, resource-injected)

   SOLID-S: Bridge layer only — no business logic, no state management.
   SOLID-D: Depends on ISAAOrchestrator abstraction and FSM handler fns.
   CLARITY-L: Pure delegation from protocol to FSM engine.
   CLARITY-Y: Returns error maps via channel, never throws."
  (:require [clojure.core.async :as async]
            [hive-mcp.protocols.agent-bridge :as bridge]
            [hive-mcp.workflows.saa-workflow :as saa]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Resource Building
;; =============================================================================

(defn- build-resources
  "Build FSM resources map from config, wiring shout-fn to output channel.

   The config map may contain any resource function key from
   saa-workflow (scope-fn, catchup-fn, explore-fn, etc.).
   The shout-fn is augmented to also put! progress messages to out-ch."
  [config out-ch]
  (let [user-shout (:shout-fn config)]
    (merge
     {:clock-fn (or (:clock-fn config) #(java.time.Instant/now))
      :error-response-fn (or (:error-response-fn config)
                              (fn [err-ctx] err-ctx))}
     (dissoc config :shout-fn :clock-fn :error-response-fn)
     {:shout-fn (fn [agent-id phase message]
                  (when out-ch
                    (async/put! out-ch {:type      :progress
                                        :saa-phase phase
                                        :agent-id  agent-id
                                        :message   message}))
                  (when user-shout
                    (try (user-shout agent-id phase message)
                         (catch Exception _ nil))))})))

(defn- resolve-agent-id
  "Resolve agent-id from session, falling back to opts or default."
  [session opts]
  (or (:agent-id opts)
      (try (bridge/session-id session) (catch Exception _ nil))
      "unknown"))

;; =============================================================================
;; FSMSAAOrchestrator
;; =============================================================================

(defrecord FSMSAAOrchestrator [config]
  bridge/ISAAOrchestrator

  (run-silence! [_ session task opts]
    (let [out-ch   (async/chan 64)
          agent-id (resolve-agent-id session opts)
          dir      (or (:directory opts) (:directory config))]
      (async/thread
        (try
          (let [resources (build-resources config out-ch)
                data      {:agent-id  agent-id
                           :task      task
                           :directory dir
                           :grounding-threshold (get config :grounding-threshold 0.6)}
                started   (saa/handle-start resources data)]
            (if (:error started)
              (async/>!! out-ch {:type :error :saa-phase :silence :error (:error started)})
              (let [caught (saa/handle-catchup resources started)]
                (if (:error caught)
                  (async/>!! out-ch {:type :error :saa-phase :silence :error (:error caught)})
                  (loop [d caught]
                    (let [sil (saa/handle-silence resources d)]
                      (if (:error sil)
                        (async/>!! out-ch {:type :error :saa-phase :silence :error (:error sil)})
                        (let [rev (saa/handle-silence-review resources sil)]
                          (if (or (saa/grounding-sufficient? rev)
                                  (saa/grounding-max-iterations? rev))
                            (async/>!! out-ch {:type               :phase-complete
                                               :saa-phase          :silence
                                               :observations       (:observations rev)
                                               :grounding-score    (:grounding-score rev)
                                               :silence-iterations (:silence-iterations rev)})
                            (recur rev))))))))))
          (catch Exception e
            (log/error "[fsm-saa] Silence failed" {:error (ex-message e)})
            (async/>!! out-ch {:type :error :saa-phase :silence :error (ex-message e)}))
          (finally (async/close! out-ch))))
      out-ch))

  (run-abstract! [_ session observations opts]
    (let [out-ch   (async/chan 64)
          agent-id (resolve-agent-id session opts)]
      (async/thread
        (try
          (let [resources (build-resources config out-ch)
                data      {:agent-id        agent-id
                           :task            (or (:task opts) "")
                           :observations    observations
                           :axioms          (get opts :axioms [])
                           :conventions     (get opts :conventions [])
                           :decisions       (get opts :decisions [])
                           :project-id      (or (:project-id opts) "unknown")
                           :abstract-retries 0}]
            (loop [d data]
              (let [abs (saa/handle-abstract resources d)]
                (cond
                  (or (:error abs) (nil? (:plan abs)))
                  (async/>!! out-ch {:type :error :saa-phase :abstract
                                     :error (or (:error abs) "No plan produced")})

                  :else
                  (let [val (saa/handle-validate-plan resources abs)]
                    (cond
                      (saa/plan-valid? val)
                      (let [stored (saa/handle-store-plan resources val)]
                        (async/>!! out-ch {:type            :phase-complete
                                           :saa-phase       :abstract
                                           :plan            (:plan stored)
                                           :plan-valid?     true
                                           :plan-memory-id  (:plan-memory-id stored)
                                           :kanban-task-ids (:kanban-task-ids stored)}))

                      (saa/plan-invalid-retryable? val)
                      (recur val)

                      :else
                      (async/>!! out-ch {:type              :error
                                         :saa-phase         :abstract
                                         :error             "Plan validation failed after max retries"
                                         :validation-errors (:validation-errors val)})))))))
          (catch Exception e
            (log/error "[fsm-saa] Abstract failed" {:error (ex-message e)})
            (async/>!! out-ch {:type :error :saa-phase :abstract :error (ex-message e)}))
          (finally (async/close! out-ch))))
      out-ch))

  (run-act! [_ session plan opts]
    (let [out-ch   (async/chan 64)
          agent-id (resolve-agent-id session opts)]
      (async/thread
        (try
          (let [resources (build-resources config out-ch)
                data      {:agent-id       agent-id
                           :task           (or (:task opts) "")
                           :plan           plan
                           :execution-mode (or (:execution-mode opts) :direct)}
                dispatched (saa/handle-act-dispatch resources data)]
            (if (:error dispatched)
              (async/>!! out-ch {:type :error :saa-phase :act :error (:error dispatched)})
              (let [verified (saa/handle-act-verify resources dispatched)]
                (async/>!! out-ch {:type           :phase-complete
                                   :saa-phase      :act
                                   :result         (:execution-result verified)
                                   :tests-passed?  (:tests-passed? verified)
                                   :verification   (:verification verified)}))))
          (catch Exception e
            (log/error "[fsm-saa] Act failed" {:error (ex-message e)})
            (async/>!! out-ch {:type :error :saa-phase :act :error (ex-message e)}))
          (finally (async/close! out-ch))))
      out-ch))

  (run-full-saa! [_ session task opts]
    (let [out-ch   (async/chan 128)
          agent-id (resolve-agent-id session opts)
          dir      (or (:directory opts) (:directory config))]
      (async/thread
        (try
          (let [resources    (build-resources config out-ch)
                initial-data {:task                task
                              :agent-id            agent-id
                              :directory           dir
                              :plan-only?          (boolean (:plan-only? opts))
                              :grounding-threshold (get config :grounding-threshold 0.6)}
                result       (saa/run-full-saa resources initial-data)]
            (async/>!! out-ch {:type      :saa-complete
                               :saa-phase :complete
                               :agent-id  agent-id
                               :result    result}))
          (catch Exception e
            (log/error "[fsm-saa] Full SAA failed" {:error (ex-message e)})
            (async/>!! out-ch {:type :error :error (ex-message e)}))
          (finally (async/close! out-ch))))
      out-ch)))

;; =============================================================================
;; Factory
;; =============================================================================

(defn ->fsm-saa-orchestrator
  "Create an FSMSAAOrchestrator instance.

   Arguments:
     config - Map of FSM resource functions and configuration:
              ;; Resource functions (see saa-workflow.clj for signatures)
              :scope-fn            - (directory) -> project-id
              :catchup-fn          - (agent-id, directory) -> context-map
              :explore-fn          - (task, agent-id, observations) -> result
              :score-grounding-fn  - (observations, files-read) -> float
              :synthesize-fn       - (task, observations, context) -> plan
              :validate-plan-fn    - (plan) -> {:valid? :errors}
              :store-plan-fn       - (plan, agent-id, directory) -> {:memory-id ...}
              :dispatch-fn         - (plan, mode, agent-id) -> {:wave-id :result}
              :verify-fn           - (result, plan) -> {:passed? :details}
              :shout-fn            - (agent-id, phase, message) -> nil
              ;; Configuration
              :grounding-threshold - Min grounding score (default: 0.6)
              :directory           - Default working directory

   Returns:
     FSMSAAOrchestrator implementing ISAAOrchestrator protocol."
  ([] (->fsm-saa-orchestrator {}))
  ([config] (->FSMSAAOrchestrator config)))
