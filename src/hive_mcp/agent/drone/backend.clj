(ns hive-mcp.agent.drone.backend
  "IDroneExecutionBackend protocol — backend-specific drone execution operations.

   Abstracts the execution mechanism for drone tasks, enabling multiple backends
   (OpenRouter, hive-agent, local LLM, etc.) behind a uniform interface.

   Implementations (planned):
   - OpenRouterBackend  (drone/backend/openrouter.clj)  — OpenRouter API delegation
   - AgenticBackend     (drone/backend/agentic.clj)     — In-process multi-turn loop
   - HiveAgentBackend   (drone/backend/hive_agent.clj)  — hive-agent bridge

   The resolve-backend multimethod dispatches on the :backend key of the
   task context, returning the appropriate IDroneExecutionBackend implementation.

   SOLID-O: Open-Closed — new backends add a new defmethod, don't modify execution.clj.
   SOLID-D: Depend on IDroneExecutionBackend abstraction, not concrete backend calls.
   SOLID-I: Small protocol (3 methods) — ISP compliant.
   CLARITY-C: Composition over case dispatch in execution phases.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; IDroneExecutionBackend Protocol
;;; ============================================================

(defprotocol IDroneExecutionBackend
  "Strategy protocol for backend-specific drone execution.

   Each method receives a task context map and returns backend-specific results.
   The execution orchestrator (execution.clj) delegates to the backend
   via resolve-backend, keeping phase logic backend-agnostic.

   Implementations must be stateless — all configuration comes via task-context."

  (execute-drone [this task-context]
    "Execute a single drone task using this backend's mechanism.

     Arguments:
       task-context - Map with:
         :task          - Augmented task description string (required)
         :model         - Model identifier string (required)
         :preset        - Preset content string
         :tools         - Vector of tool specs for the drone
         :max-steps     - Integer step/turn budget
         :trace         - Boolean, emit progress events
         :sandbox       - Sandbox configuration map:
                          {:allowed-files [...] :allowed-dirs [...] :blocked-tools [...]}
         :drone-id      - Drone identifier (for logging/attribution)
         :cwd           - Working directory path

     Returns:
       Result map with:
         :status        - :completed | :failed | :timeout
         :result        - Response text or structured result
         :tokens        - {:input-tokens N :output-tokens N} (when available)
         :model         - Model actually used (may differ from requested)
         :steps         - Number of turns/steps taken (for agentic backends)
         :tool-calls    - Number of tool calls made (when tracked)

     Throws:
       ExceptionInfo on unrecoverable backend errors (timeout, auth, etc.)")

  (supports-validation? [this]
    "Whether this backend supports post-execution validation (lint, tests).

     Backends that run code in-process (agentic) can validate inline.
     Backends that delegate externally (OpenRouter) rely on the orchestrator
     for post-execution validation.

     Returns:
       Boolean — true if backend handles its own validation.")

  (backend-type [this]
    "Return keyword identifying this backend type.

     Used for:
       - Logging and metrics attribution
       - Model routing decisions
       - Prometheus label values

     Returns:
       Keyword — e.g. :openrouter, :agentic, :hive-agent"))

;;; ============================================================
;;; resolve-backend Multimethod
;;; ============================================================

(defmulti resolve-backend
  "Resolve an IDroneExecutionBackend implementation from a task context.

   Dispatches on the :backend key of the context map.

   Arguments:
     context - Map containing at minimum :backend keyword.
               May also contain :model, :cwd, and other backend-specific config.

   Returns:
     An IDroneExecutionBackend implementation.

   Throws:
     IllegalArgumentException if no implementation registered for the backend key.

   Usage:
     (def backend (resolve-backend {:backend :openrouter :model \"devstral-small:24b\"}))
     (execute-drone backend task-context)

   Registering new backends:
     (defmethod resolve-backend :my-backend [context]
       (->MyBackend (:model context)))"
  (fn [context] (:backend context)))

(defmethod resolve-backend :default [context]
  (throw (IllegalArgumentException.
          (str "No IDroneExecutionBackend registered for backend: "
               (pr-str (:backend context))
               ". Available backends are registered via (defmethod resolve-backend :key ...)"))))
