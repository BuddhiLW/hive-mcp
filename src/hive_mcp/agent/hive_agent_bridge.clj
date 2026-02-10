(ns hive-mcp.agent.hive-agent-bridge
  "Bridge to external agent loop extension.

   Uses extension registry for optional agent capabilities.
   When the agent extension is not registered, returns nil and
   callers fall back to the built-in path.

   SOLID-O: Open for extension (new bridge fns), closed for modification.
   CLARITY-Y: Graceful degradation when extension absent."
  (:require [hive-mcp.config :as config]
            [hive-mcp.extensions.registry :as ext]
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
;; Extension Lookup
;; =============================================================================

(defn resolve-run-agent
  "Look up the agent loop extension.
   Returns the function or nil if not registered."
  []
  (ext/get-extension :ag/run))

(defn resolve-build-context
  "Look up the context builder extension.
   Returns the function or nil if not registered."
  []
  (ext/get-extension :ag/context))

(defn resolve-tool-definitions
  "Look up the tool definitions extension.
   Returns the function or nil if not registered."
  []
  (ext/get-extension :ag/tools))

;; =============================================================================
;; Availability Check
;; =============================================================================

(defn hive-agent-available?
  "Check if the agent loop extension is registered."
  []
  (ext/extension-available? :ag/run))

;; =============================================================================
;; Result Adaptation
;; =============================================================================

(defn adapt-hive-agent-result
  "Adapt agent extension result to hive-mcp's expected format.

   Arguments:
     ha-result - Result map from agent extension
     model     - Model name used

   Returns:
     Map in hive-mcp format."
  [ha-result model]
  (let [error? (:error ha-result)
        status (if error? :error :completed)]
    {:status status
     :result (or (:result ha-result) "")
     :steps []
     :tool_calls_made (or (:tool-calls-made ha-result) 0)
     :tokens {:input 0 :output 0 :total 0}
     :model (or model "unknown")
     :hive-agent-metadata {:turns (or (:turns ha-result) 0)
                           :kg-path (:kg-path ha-result)
                           :source :hive-agent}}))

;; =============================================================================
;; High-Level Bridge Function
;; =============================================================================

(defn run-agent-via-bridge
  "Run a task through the agent loop extension if available.

   Arguments:
     opts - Map with:
       :task           - Task description (required)
       :model          - OpenRouter model ID
       :max-turns      - Maximum loop iterations
       :preset-content - System prompt preset string
       :project-id     - Project ID for memory scoping
       :files          - Vector of file paths relevant to task (optional)
       :cwd            - Working directory (optional)
       :compress?      - Enable context compression (default: true)

   Returns:
     Adapted result map in hive-mcp format, or nil if extension unavailable.

   Usage:
     (if-let [result (run-agent-via-bridge {...})]
       result
       (fallback-to-built-in-path ...))"
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
          {:status :error
           :result (str "Agent extension error: " (.getMessage e))
           :steps []
           :tool_calls_made 0
           :tokens {:input 0 :output 0 :total 0}
           :model (or model "unknown")})))
    ;; Extension not available — return nil for fallback
    (do
      (log/debug "Agent extension not available, caller should use fallback path")
      nil)))
