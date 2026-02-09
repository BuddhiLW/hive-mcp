(ns hive-mcp.agent.hive-agent-bridge
  "Bridge to hive-agent's multi-turn agentic loop.

   Uses requiring-resolve pattern for IP boundary compliance.
   hive-agent is a separate repo that may or may not be on classpath.
   When unavailable, returns nil and callers fall back to old path.

   Integration points:
   - hive-agent.loop.core/run-agent     — multi-turn agentic loop
   - hive-agent.context.core/build-context — system prompt builder
   - hive-agent.tools.definitions/tool-definitions — tool schemas

   SOLID-O: Open for extension (new bridge fns), closed for modification.
   CLARITY-Y: Graceful degradation when hive-agent absent."
  (:require [hive-mcp.config :as config]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Config-Driven Defaults
;; =============================================================================

(defn- default-model
  "Resolve the default model from config.edn :models.default-model.
   CLARITY-Y: Falls back to hardcoded default when config unavailable."
  []
  (or (config/get-config-value "models.default-model")
      "x-ai/grok-code-fast-1"))

;; =============================================================================
;; requiring-resolve Stubs (IP Boundary Pattern)
;; =============================================================================

(defn resolve-run-agent
  "Resolve hive-agent.loop.core/run-agent.
   Returns the function or nil if hive-agent is not on classpath."
  []
  (try
    (requiring-resolve 'hive-agent.loop.core/run-agent)
    (catch Exception e
      (log/debug "hive-agent not on classpath:" (.getMessage e))
      nil)))

(defn resolve-build-context
  "Resolve hive-agent.context.core/build-context.
   Returns the function or nil if hive-agent is not on classpath."
  []
  (try
    (requiring-resolve 'hive-agent.context.core/build-context)
    (catch Exception e
      (log/debug "hive-agent context not available:" (.getMessage e))
      nil)))

(defn resolve-tool-definitions
  "Resolve hive-agent.tools.definitions/tool-definitions.
   Returns the function or nil if hive-agent is not on classpath."
  []
  (try
    (requiring-resolve 'hive-agent.tools.definitions/tool-definitions)
    (catch Exception e
      (log/debug "hive-agent tool definitions not available:" (.getMessage e))
      nil)))

;; =============================================================================
;; Availability Check
;; =============================================================================

(defn hive-agent-available?
  "Check if hive-agent is on classpath and run-agent is resolvable."
  []
  (boolean (resolve-run-agent)))

;; =============================================================================
;; Result Adaptation
;; =============================================================================

(defn adapt-hive-agent-result
  "Adapt hive-agent run-agent result to hive-mcp's expected format.

   hive-agent returns:
     {:result \"text\" :turns N :tool-calls-made N :kg-path \"/tmp/drone-...\"}

   hive-mcp expects (from agent loop):
     {:status :completed|:error :result \"text\" :steps [...] :tool_calls_made N
      :tokens {:input N :output N :total N} :model \"model-name\"}

   Arguments:
     ha-result - Result map from hive-agent.loop.core/run-agent
     model     - Model name used

   Returns:
     Map in hive-mcp format."
  [ha-result model]
  (let [error? (:error ha-result)
        status (if error? :error :completed)]
    {:status status
     :result (or (:result ha-result) "")
     :steps []  ;; hive-agent doesn't track steps in same format
     :tool_calls_made (or (:tool-calls-made ha-result) 0)
     :tokens {:input 0 :output 0 :total 0}  ;; hive-agent V1 doesn't track tokens
     :model (or model "unknown")
     :hive-agent-metadata {:turns (or (:turns ha-result) 0)
                           :kg-path (:kg-path ha-result)
                           :source :hive-agent}}))

;; =============================================================================
;; High-Level Bridge Function
;; =============================================================================

(defn run-agent-via-bridge
  "Run a task through hive-agent's agentic loop if available.

   Arguments:
     opts - Map with:
       :task           - Task description (required)
       :model          - OpenRouter model ID
       :max-turns      - Maximum loop iterations
       :preset-content - System prompt preset string
       :project-id     - Project ID for memory scoping
       :files          - Vector of file paths relevant to task (optional)
       :cwd            - Working directory (optional)
       :compress?      - Enable context compression (default: true in run-agent)

   Returns:
     Adapted result map in hive-mcp format, or nil if hive-agent unavailable.

   Usage:
     (if-let [result (run-agent-via-bridge {...})]
       result  ;; hive-agent handled it
       (fallback-to-old-path ...))  ;; hive-agent not available"
  [{:keys [task model max-turns preset-content project-id files cwd compress?] :as opts}]
  (if-let [run-agent-fn (resolve-run-agent)]
    (do
      (log/info {:event :hive-agent-bridge/dispatching
                 :model model
                 :max-turns max-turns
                 :task-preview (subs task 0 (min 100 (count task)))})
      (try
        (let [result (run-agent-fn (cond-> {:task task
                                            :model (or model (default-model))
                                            :max-turns (or max-turns 250)
                                            :preset-content preset-content
                                            :project-id project-id}
                                     files        (assoc :files files)
                                     cwd          (assoc :cwd cwd)
                                     (some? compress?) (assoc :kg-compress? compress?)))]
          (log/info {:event :hive-agent-bridge/completed
                     :turns (:turns result)
                     :tool-calls (:tool-calls-made result)
                     :error? (:error result)})
          (adapt-hive-agent-result result model))
        (catch Exception e
          (log/error {:event :hive-agent-bridge/error
                      :error (.getMessage e)})
          ;; Return error result rather than nil — caller knows we tried
          {:status :error
           :result (str "hive-agent error: " (.getMessage e))
           :steps []
           :tool_calls_made 0
           :tokens {:input 0 :output 0 :total 0}
           :model (or model "unknown")})))
    ;; hive-agent not available — return nil for fallback
    (do
      (log/debug "hive-agent not available, caller should use fallback path")
      nil)))
