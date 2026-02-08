(ns hive-mcp.agent.drone.backend.hive-agent
  "HiveAgentBackend — IDroneExecutionBackend implementation wrapping hive-agent bridge.

   Delegates drone execution to the hive-agent multi-turn agentic loop
   via the hive-agent-bridge. The bridge uses requiring-resolve for IP
   boundary compliance — hive-agent may or may not be on classpath.

   SOLID-O: Registered via defmethod, no modification to execution.clj.
   SOLID-D: Depends on IDroneExecutionBackend abstraction.
   CLARITY-Y: Graceful degradation when hive-agent unavailable."
  (:require [hive-mcp.agent.drone.backend :as backend]
            [hive-mcp.agent.hive-agent-bridge :as bridge]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Result Adaptation
;; =============================================================================

(defn- adapt-bridge-result
  "Adapt hive-agent-bridge result to IDroneExecutionBackend contract.

   Bridge returns:
     {:status :completed/:error, :result str, :steps [], :tool_calls_made N,
      :tokens {:input N :output N :total N}, :model str, :hive-agent-metadata map}

   Protocol expects:
     {:status :completed/:failed/:timeout, :result str,
      :tokens {:input-tokens N :output-tokens N}, :model str}"
  [bridge-result]
  (let [status (case (:status bridge-result)
                 :completed :completed
                 :error     :failed
                 :failed)]
    {:status   status
     :result   (or (:result bridge-result) "")
     :tokens   {:input-tokens  (get-in bridge-result [:tokens :input] 0)
                :output-tokens (get-in bridge-result [:tokens :output] 0)}
     :model    (or (:model bridge-result) "unknown")
     :steps    (or (:tool_calls_made bridge-result) 0)
     :metadata (merge
                (:hive-agent-metadata bridge-result)
                {:backend :hive-agent})}))

;; =============================================================================
;; HiveAgentBackend Record
;; =============================================================================

(defrecord HiveAgentBackend []
  backend/IDroneExecutionBackend

  (execute-drone [_this task-context]
    (let [{:keys [task model max-steps preset cwd]} task-context
          bridge-opts {:task           task
                       :model          (or model "deepseek/deepseek-chat")
                       :max-turns      (or max-steps 20)
                       :preset-content preset
                       :project-id     nil}]
      (log/info {:event  :hive-agent-backend/executing
                 :model  (:model bridge-opts)
                 :cwd    cwd})
      (if-let [result (bridge/run-agent-via-bridge bridge-opts)]
        (do
          (log/info {:event  :hive-agent-backend/completed
                     :status (:status result)})
          (adapt-bridge-result result))
        ;; Bridge returned nil — hive-agent not on classpath
        (do
          (log/warn {:event :hive-agent-backend/unavailable})
          {:status :failed
           :result "hive-agent is not available on classpath"
           :tokens {:input-tokens 0 :output-tokens 0}
           :model  (or model "unknown")
           :steps  0
           :metadata {:backend :hive-agent
                      :reason  :unavailable}}))))

  (supports-validation? [_this]
    true)

  (backend-type [_this]
    :hive-agent))

;; =============================================================================
;; Constructor
;; =============================================================================

(defn make-hive-agent-backend
  "Create a HiveAgentBackend instance."
  []
  (->HiveAgentBackend))

;; =============================================================================
;; resolve-backend Registration
;; =============================================================================

(defmethod backend/resolve-backend :hive-agent [_context]
  (make-hive-agent-backend))
