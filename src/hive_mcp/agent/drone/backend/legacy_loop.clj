(ns hive-mcp.agent.drone.backend.legacy-loop
  "LegacyLoopBackend — IDroneExecutionBackend wrapping the existing OpenRouter drone loop.

   Adapts the existing run-agentic-loop (agent/drone/loop.clj) to the new
   IDroneExecutionBackend protocol, providing a standardized interface for
   the legacy multi-turn drone execution pipeline.

   This backend:
   - Creates an OpenRouter LLM backend from task-context model
   - Runs the existing agentic loop (think-act-observe)
   - Returns standardized result maps
   - Supports post-execution validation (true)
   - backend-type returns :legacy-loop

   SOLID-O: New backend via defmethod, no modification to execution.clj.
   SOLID-D: Depends on IDroneExecutionBackend abstraction.
   CLARITY-Y: Graceful error handling — returns :failed status, never throws."
  (:require [hive-mcp.agent.drone.backend :as backend]
            [hive-mcp.agent.drone.loop :as loop]
            [hive-mcp.agent.drone.tool-allowlist :as allowlist]
            [hive-mcp.agent.registry :as registry]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Helpers
;;; ============================================================

(defn- create-llm-backend
  "Create an OpenRouter LLM backend for the given model and preset.

   Uses requiring-resolve to avoid hard dependency on agent.config.

   Arguments:
     model  - Model identifier string
     preset - Preset content string (optional)

   Returns:
     LLMBackend instance

   Throws:
     ExceptionInfo on creation failure."
  [model preset]
  (let [factory-fn (requiring-resolve 'hive-mcp.agent.config/openrouter-backend)]
    (factory-fn {:model model :preset preset})))

(defn- resolve-effective-tools
  "Resolve the tool set for the drone execution.

   Arguments:
     tools - Explicit tool list from task-context (may be nil)

   Returns:
     Vector of tool name strings."
  [tools]
  (or (when (seq tools) (vec tools))
      (vec (allowlist/resolve-allowlist {:task-type :general}))))

(defn- normalize-status
  "Normalize agentic loop status to IDroneExecutionBackend contract.

   Mapping:
     :completed  -> :completed
     :max_steps  -> :completed  (ran out of steps but produced output)
     :noop       -> :failed     (no backend available)
     :error      -> :failed

   Arguments:
     status - Keyword from run-agentic-loop result

   Returns:
     :completed or :failed"
  [status]
  (case status
    :completed :completed
    :max_steps :completed
    (:noop :error) :failed
    ;; Unknown status — treat as failed
    :failed))

(defn- normalize-tokens
  "Extract token counts from loop result into standardized format.

   Arguments:
     tokens - Map with :input, :output keys (from agentic loop)

   Returns:
     Map with :input-tokens, :output-tokens"
  [tokens]
  {:input-tokens  (or (:input tokens) 0)
   :output-tokens (or (:output tokens) 0)})

(defn- build-result
  "Transform agentic loop result to IDroneExecutionBackend result contract.

   Arguments:
     loop-result     - Result map from run-agentic-loop
     effective-model - Model string used for execution

   Returns:
     Standardized result map per execute-drone contract."
  [loop-result effective-model]
  {:status     (normalize-status (:status loop-result))
   :result     (or (:result loop-result) "No result")
   :tokens     (normalize-tokens (:tokens loop-result))
   :model      (or (:model loop-result) effective-model)
   :steps      (or (:turns loop-result) 0)
   :tool-calls (or (:tool_calls_made loop-result) 0)})

(defn- error-result
  "Build a failed result map from an exception.

   Arguments:
     e     - Exception
     model - Model string for attribution

   Returns:
     Failed result map."
  [e model]
  {:status     :failed
   :result     (or (ex-message e) "Unknown error")
   :tokens     {:input-tokens 0 :output-tokens 0}
   :model      (or model "unknown")
   :steps      0
   :tool-calls 0})

;;; ============================================================
;;; LegacyLoopBackend Record
;;; ============================================================

(defrecord LegacyLoopBackend [model-override]
  backend/IDroneExecutionBackend

  (execute-drone [_this task-context]
    (let [{:keys [task model preset tools max-steps trace drone-id cwd]} task-context
          effective-model (or model-override model "devstral-small:24b")
          effective-id    (or drone-id "legacy-drone")]
      (try
        (log/info "LegacyLoopBackend executing"
                  {:drone-id effective-id :model effective-model :max-steps max-steps})

        (let [llm-backend     (create-llm-backend effective-model preset)
              _               (registry/ensure-registered!)
              effective-tools (resolve-effective-tools tools)
              loop-result     (loop/run-agentic-loop
                               {:task task :files [] :cwd cwd}
                               {:drone-id effective-id :kg-store nil}
                               {:max-turns    (or max-steps 10)
                                :backend      llm-backend
                                :tools        effective-tools
                                :permissions  #{}
                                :trace?       (boolean trace)
                                :agent-id     effective-id})]

          (log/info "LegacyLoopBackend completed"
                    {:drone-id effective-id
                     :status   (:status loop-result)
                     :turns    (:turns loop-result)})

          (build-result loop-result effective-model))

        (catch Exception e
          (log/error {:event      :legacy-loop/execution-failed
                      :drone-id   effective-id
                      :model      effective-model
                      :error      (ex-message e)})
          (error-result e effective-model)))))

  (supports-validation? [_this]
    true)

  (backend-type [_this]
    :legacy-loop))

;;; ============================================================
;;; Constructor
;;; ============================================================

(defn ->legacy-loop-backend
  "Create a LegacyLoopBackend instance.

   Options:
     :model - Override model (default: use model from task-context)

   Returns:
     LegacyLoopBackend record implementing IDroneExecutionBackend."
  [& [{:keys [model]}]]
  (->LegacyLoopBackend model))

;;; ============================================================
;;; Multimethod Registration
;;; ============================================================

(defmethod backend/resolve-backend :legacy-loop [context]
  (->legacy-loop-backend {:model (:model context)}))
