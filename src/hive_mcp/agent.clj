(ns hive-mcp.agent
  "Agent delegation with tool-use loop for two-tier LLM architecture.
   
   Allows Claude (coordinator) to delegate tasks to local models (Ollama)
   or cloud models (OpenRouter) while giving them access to hive-mcp tools.
   
   Architecture:
   - LLMBackend protocol for pluggable model backends (see agent.protocol)
   - Tool-use loop: send → execute tool calls → append results → repeat
   - Permission gates via hivemind.ask! for dangerous operations
   - Max steps guardrail to prevent runaway loops
   - Task-based model selection for OpenRouter free tier
   
   Usage:
     ;; Ollama (local)
     (delegate! {:backend :ollama
                 :model \"devstral-small:24b\"
                 :task \"Implement the foo function in src/bar.clj\"
                 :tools [:read_file :file_edit :grep :glob_files]
                 :max_steps 10})
     
     ;; OpenRouter with task type
     (delegate! {:backend :openrouter
                 :task-type :coding
                 :task \"Write a palindrome function\"})
     
     ;; OpenRouter with explicit model
     (delegate! {:backend :openrouter
                 :model \"mistralai/devstral-2512:free\"
                 :task \"Fix the bug in auth.clj\"})"
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ollama :as ollama]
            [hive-mcp.agent.config :as config]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.loop :as loop]
            [hive-mcp.agent.mcp :as mcp]
            [hive-mcp.agent.drone :as drone]
            [hive-mcp.channel :as channel]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Public API
;;; ============================================================
;; Protocol/Config/Registry APIs moved to:
;;   hive-mcp.agent.{protocol,config,registry}

(defn delegate!
  "Delegate a task to a local or cloud model with tool access.
   
   Options:
     :backend   - Backend type: :openrouter (default) or :ollama
     :model     - Model name (backend-specific)
     :preset    - Swarm preset for auto model selection (OpenRouter)
     :task-type - For OpenRouter: :coding, :coding-alt, :arch, :docs
     :host      - Ollama host (default: http://localhost:11434)
     :api-key   - OpenRouter API key (or set OPENROUTER_API_KEY env)
     :task      - Task description (required)
     :tools     - List of tool names to allow (nil = all registered)
     :permissions - Set of permissions (:auto-approve skips human checks)
     :max-steps - Maximum tool-use iterations (default: 50)
     :trace     - If true, emit progress events via channel for monitoring
   
   Returns result map with :status, :result, :steps, :tool_calls_made
   
   Examples:
     ;; OpenRouter with preset (auto model selection) - DEFAULT
     (delegate! {:preset \"tdd\" :task \"Write tests\"})
     
     ;; OpenRouter with task type
     (delegate! {:task-type :arch :task \"Review design\"})
     
     ;; OpenRouter with explicit model
     (delegate! {:model \"mistralai/devstral-2512:free\" :task \"...\"})
     
     ;; Ollama (local)
     (delegate! {:backend :ollama :task \"Fix the bug\" :model \"devstral-small:24b\"})"
  [{:keys [backend model host task preset task-type api-key tools permissions max-steps trace]
    :or {backend :openrouter
         host "http://localhost:11434"
         max-steps 50
         permissions #{}
         trace false}
    :as opts}]
  ;; Ensure tools are registered (lazy init for REPL usage)
  (registry/ensure-registered!)

  (when-not task
    (throw (ex-info "Task is required" {:opts opts})))

  (let [;; Only use default Ollama model if backend is :ollama and no model provided
        effective-model (or model
                            (when (= backend :ollama) "devstral-small:24b"))
        backend-instance (case backend
                           :ollama (ollama/ollama-backend {:host host :model (or effective-model "devstral-small:24b")})
                           :openrouter (config/openrouter-backend {:model effective-model
                                                                   :preset preset
                                                                   :task-type (or task-type :coding)
                                                                   :api-key api-key})
                           ;; Default to openrouter
                           (config/openrouter-backend {:model effective-model
                                                       :preset preset
                                                       :task-type (or task-type :coding)
                                                       :api-key api-key}))
        agent-id (str "delegate-" (System/currentTimeMillis))]

    (when trace
      (channel/emit-event! :agent-started {:agent-id agent-id
                                           :backend backend
                                           :preset preset
                                           :model (proto/model-name backend-instance)
                                           :task task}))

    (try
      (let [result (loop/run-loop {:backend backend-instance
                                   :task task
                                   :tools tools
                                   :permissions permissions
                                   :max-steps max-steps
                                   :agent-id agent-id
                                   :trace? trace})]
        result)

      (catch Exception e
        (log/error e "Agent delegation failed")
        (when trace
          (channel/emit-event! :agent-failed {:agent-id agent-id :error (ex-message e)}))
        {:status :error
         :result (ex-message e)
         :steps []
         :tool_calls_made 0}))))

;;; ============================================================
;;; Drone Delegation (delegated to agent/drone.clj)
;;; ============================================================

(defn delegate-drone!
  "Delegate a task to a drone (token-optimized leaf agent).
   See hive-mcp.agent.drone/delegate! for full documentation."
  [opts]
  (drone/delegate! opts delegate!))

;;; ============================================================
;;; MCP Tool Definitions (delegated to agent/mcp.clj)
;;; ============================================================

(def tools
  "MCP tools for agent delegation. See agent/mcp.clj for definitions."
  (mcp/make-tools delegate! delegate-drone!))

;;; ============================================================
;;; Re-exports from agent.registry (for backwards compatibility)
;;; ============================================================

(def register-tools!
  "Register tools for agent delegation. Delegates to registry/register!"
  registry/register!)
