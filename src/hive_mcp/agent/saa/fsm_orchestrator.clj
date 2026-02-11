(ns hive-mcp.agent.saa.fsm-orchestrator
  "FSM-backed ISAAOrchestrator implementation."
  (:require [clojure.core.async :as async]
            [hive-mcp.protocols.agent-bridge :as bridge]
            [hive-mcp.workflows.saa-workflow :as saa]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- build-resources
  "Build FSM resources map from config, wiring shout-fn to output channel."
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

(defn ->fsm-saa-orchestrator
  "Create an FSMSAAOrchestrator instance from a config map."
  ([] (->fsm-saa-orchestrator {}))
  ([config] (->FSMSAAOrchestrator config)))
