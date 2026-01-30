(ns hive-mcp.agent.routing
  "Smart model routing for drone tasks.

   Routes tasks to optimal free-tier models based on:
   - Task classification (testing, refactoring, implementation, bugfix)
   - File path patterns
   - Historical success rates per model/task-type (from feedback module)
   - Fallback chains when primary models fail

   Integrates with:
   - hive-mcp.agent.drone.feedback for persistent pattern storage
   - hive-mcp.agent.drone.preset for task classification
   - hive-mcp.agent.config for model resolution

   CLARITY-T: Full telemetry for success rate tracking"
  (:require [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.agent.drone.feedback :as feedback]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Task Classification (delegates to preset module)
;;; ============================================================

(defn classify-task
  "Classify a task based on description and file paths.

   Delegates to hive-mcp.agent.drone.preset for consistent classification.

   Returns one of:
   - :testing        - Test files or test-related tasks
   - :refactoring    - Refactoring, simplification, cleanup
   - :implementation - Adding new features, creating new code
   - :bugfix         - Fixing bugs, errors, issues
   - :documentation  - Docs, comments, explanations
   - :general        - Catch-all for unclassified tasks"
  [task files]
  (preset/get-task-type task files))

;;; ============================================================
;;; Model Routing Table
;;; ============================================================

(def model-routes
  "Task type to primary/secondary model mapping.

   Strategy (paid tier):
   - grok-code-fast-1: Best for code generation, tests, implementations
   - deepseek-v3.2: Best at code understanding, refactoring, bugfix
   - Fallback chains use deepseek as secondary"
  (atom
   {:testing        {:primary "x-ai/grok-code-fast-1"
                     :secondary "deepseek/deepseek-v3.2"
                     :reason "Grok excels at test patterns and assertions"}

    :refactoring    {:primary "deepseek/deepseek-v3.2"
                     :secondary "x-ai/grok-code-fast-1"
                     :reason "DeepSeek strong at understanding code structure"}

    :implementation {:primary "x-ai/grok-code-fast-1"
                     :secondary "deepseek/deepseek-v3.2"
                     :reason "Grok optimized for code generation"}

    :bugfix         {:primary "deepseek/deepseek-v3.2"
                     :secondary "x-ai/grok-code-fast-1"
                     :reason "DeepSeek good at root cause analysis"}

    :documentation  {:primary "deepseek/deepseek-v3.2"
                     :secondary "x-ai/grok-code-fast-1"
                     :reason "DeepSeek good at natural language"}

    :general        {:primary "x-ai/grok-code-fast-1"
                     :secondary "deepseek/deepseek-v3.2"
                     :reason "Grok as robust default"}}))

(defn get-route
  "Get the model route for a task type."
  [task-type]
  (get @model-routes task-type (get @model-routes :general)))

(defn set-route!
  "Update the route for a task type.

   Example: (set-route! :testing {:primary \"model-a\" :secondary \"model-b\"})"
  [task-type route-config]
  (swap! model-routes assoc task-type route-config)
  @model-routes)

;;; ============================================================
;;; Success Rate Tracking (CLARITY-T)
;;; Hybrid: in-memory for fast routing + Chroma for persistence
;;; ============================================================

;; In-memory session tracking for fast routing decisions.
;; Format: {[model task-type] {:successes N :failures N :last-failure-time T}}
;; Resets each session; persisted patterns live in Chroma via feedback module.
(defonce ^:private session-rates (atom {}))

(defn record-success!
  "Record a successful task completion for a model/task-type pair.
   Updates both session cache and persistent storage via feedback module."
  [model task-type & [{:keys [duration-ms directory agent-id]}]]
  ;; Update session cache
  (swap! session-rates update [model task-type]
         (fnil (fn [stats]
                 (update stats :successes (fnil inc 0)))
               {:successes 0 :failures 0}))
  ;; Persist to Chroma via feedback module
  (feedback/record-pattern! task-type model :success
                            {:duration-ms duration-ms
                             :directory directory
                             :agent-id agent-id})
  (log/debug "Recorded success" {:model model :task-type task-type}))

(defn record-failure!
  "Record a task failure for a model/task-type pair.
   Updates both session cache and persistent storage via feedback module."
  [model task-type result-class & [{:keys [duration-ms directory agent-id]}]]
  ;; Update session cache
  (swap! session-rates update [model task-type]
         (fnil (fn [stats]
                 (-> stats
                     (update :failures (fnil inc 0))
                     (assoc :last-failure-time (System/currentTimeMillis))))
               {:successes 0 :failures 0}))
  ;; Persist to Chroma with specific failure type
  (feedback/record-pattern! task-type model (or result-class :unknown-failure)
                            {:duration-ms duration-ms
                             :directory directory
                             :agent-id agent-id})
  (log/debug "Recorded failure" {:model model :task-type task-type :class result-class}))

(defn get-success-rate
  "Get success rate for a model/task-type pair.

   Combines session cache with persisted patterns from feedback module.
   Returns ratio 0.0-1.0, or 1.0 if no data (give benefit of doubt)."
  [model task-type & [{:keys [directory]}]]
  (let [;; Session data (fast)
        session-stats (get @session-rates [model task-type] {:successes 0 :failures 0})
        session-total (+ (:successes session-stats 0) (:failures session-stats 0))
        ;; Persisted data (slower, but comprehensive)
        persisted-stats (feedback/get-success-rate task-type model {:directory directory})
        ;; Combine: weight session data higher for recency
        session-rate (if (pos? session-total)
                       (/ (:successes session-stats 0) session-total)
                       nil)
        persisted-rate (:success-rate persisted-stats)]
    (cond
      ;; Both available: weighted average (session: 60%, persisted: 40%)
      (and session-rate persisted-rate)
      (+ (* 0.6 session-rate) (* 0.4 persisted-rate))

      ;; Only session data
      session-rate session-rate

      ;; Only persisted data
      persisted-rate persisted-rate

      ;; No data - assume success
      :else 1.0)))

(defn get-all-success-rates
  "Get all success rate data for analysis (session + persisted)."
  []
  (let [session-data (into {}
                           (map (fn [[[model task-type] stats]]
                                  {[model task-type]
                                   (assoc stats
                                          :rate (get-success-rate model task-type)
                                          :source :session)}))
                           @session-rates)
        persisted-summary (feedback/aggregate-weekly-stats)]
    {:session session-data
     :persisted persisted-summary}))

(defn reset-session-rates!
  "Reset session success rate tracking (persisted data remains)."
  []
  (reset! session-rates {}))

;;; ============================================================
;;; Smart Model Selection
;;; ============================================================

(def ^:private cooldown-ms
  "Cooldown period after failure before retrying a model (5 minutes)."
  (* 5 60 1000))

(defn- model-on-cooldown?
  "Check if model is on cooldown for this task type (session-based)."
  [model task-type]
  (let [stats (get @session-rates [model task-type])
        last-failure (:last-failure-time stats 0)]
    (< (- (System/currentTimeMillis) last-failure) cooldown-ms)))

(defn- model-historically-bad?
  "Check if model has historically bad performance for this task type.
   Uses feedback module's pattern analysis."
  [model task-type & [{:keys [directory]}]]
  (feedback/should-avoid-combo? task-type model {:directory directory}))

(defn select-model
  "Select optimal model for a task.

   Uses multi-signal routing:
   1. Check if primary is on session cooldown (recent failure)
   2. Check if primary has historically poor performance (Chroma patterns)
   3. Consider feedback module's model recommendation

   Arguments:
     task      - Task description
     files     - List of file paths (optional)
     opts      - Options map:
                 :force-model - Override automatic selection
                 :directory   - Project directory for pattern scoping

   Returns map with:
     :model     - Selected model string
     :task-type - Classified task type
     :reason    - Why this model was selected
     :fallback  - Secondary model if primary fails
     :signals   - Map of routing signals used"
  [task files & [{:keys [force-model directory]}]]
  (if force-model
    {:model force-model
     :task-type :forced
     :reason "Explicit model override"
     :fallback nil
     :signals {:override true}}
    (let [task-type (classify-task task files)
          route (get-route task-type)
          primary (:primary route)
          secondary (:secondary route)
          ;; Multi-signal analysis
          on-cooldown? (model-on-cooldown? primary task-type)
          historically-bad? (model-historically-bad? primary task-type {:directory directory})
          ;; Check if feedback module recommends a different model
          recommended (feedback/recommend-model task-type [primary secondary] {:directory directory})
          ;; Decision logic
          use-secondary? (or on-cooldown? historically-bad?)
          use-recommended? (and recommended (not= recommended primary) (not use-secondary?))
          selected (cond
                     use-secondary? secondary
                     use-recommended? recommended
                     :else primary)
          signals {:on-cooldown on-cooldown?
                   :historically-bad historically-bad?
                   :feedback-recommended (when use-recommended? recommended)}]
      (log/debug "Model selection"
                 {:task-type task-type
                  :primary primary
                  :secondary secondary
                  :selected selected
                  :signals signals})
      {:model selected
       :task-type task-type
       :reason (cond
                 on-cooldown? (str "Primary on cooldown. " (:reason route))
                 historically-bad? (str "Primary has poor history. Fallback: " (:reason route))
                 use-recommended? (str "Feedback recommends " recommended)
                 :else (:reason route))
       :fallback (if (= selected secondary) primary secondary)
       :signals signals})))

(defn with-fallback
  "Execute a function with automatic fallback to secondary model on failure.

   Arguments:
     selection - Map from select-model
     f         - Function taking model string, returns result

   Returns result from f, trying fallback if primary fails."
  [{:keys [model task-type fallback]} f]
  (try
    (let [result (f model)]
      (record-success! model task-type)
      result)
    (catch Exception e
      (record-failure! model task-type :unknown-failure)
      (if fallback
        (do
          (log/warn "Primary model failed, trying fallback"
                    {:primary model :fallback fallback :error (ex-message e)})
          (try
            (let [result (f fallback)]
              (record-success! fallback task-type)
              result)
            (catch Exception e2
              (record-failure! fallback task-type :unknown-failure)
              (log/error "Fallback model also failed"
                         {:fallback fallback :error (ex-message e2)})
              (throw e2))))
        (throw e)))))

;;; ============================================================
;;; Public API
;;; ============================================================

(defn route-and-select
  "High-level API: classify task and select optimal model.

   Arguments:
     task  - Task description (required)
     files - List of file paths (optional)
     opts  - Options:
             :force-model - Override automatic selection
             :directory   - Project directory for pattern scoping

   Returns selection map suitable for drone delegation."
  [task & [files opts]]
  (let [selection (select-model task files opts)]
    (log/info "Routed task"
              {:task (subs task 0 (min 60 (count task)))
               :task-type (:task-type selection)
               :model (:model selection)
               :signals (:signals selection)})
    selection))

(defn list-routes
  "List all configured model routes."
  []
  @model-routes)

(defn get-routing-stats
  "Get comprehensive routing statistics for monitoring.

   Returns:
     :routes         - Current model routes by task type
     :session        - In-memory session statistics
     :feedback       - Persisted pattern analysis from Chroma
     :recommendations - Model/task-type recommendations from feedback analysis
     :config         - Routing configuration (cooldown, etc.)"
  [& [{:keys [directory]}]]
  (let [feedback-stats (feedback/aggregate-weekly-stats {:directory directory})
        recommendations (feedback/generate-recommendations {:directory directory})]
    {:routes @model-routes
     :session @session-rates
     :feedback feedback-stats
     :recommendations recommendations
     :config {:cooldown-ms cooldown-ms}}))

(defn get-model-for-task
  "Convenience function: get the best model for a task.

   This is the main entry point for drone model selection.

   Arguments:
     task  - Task description
     files - List of file paths (optional)
     opts  - Options map (see select-model)

   Returns model string."
  [task & [files opts]]
  (:model (select-model task files opts)))

(defn report-execution!
  "Report a drone execution result for routing optimization.

   Arguments:
     task-type    - Task type keyword
     model        - Model used
     result       - Drone result map with :status, :error, etc.
     opts         - Options with :duration-ms, :directory, :agent-id

   This updates both session cache and persistent patterns."
  [task-type model result & [opts]]
  (let [result-class (feedback/classify-result result)]
    (if (= result-class :success)
      (record-success! model task-type opts)
      (record-failure! model task-type result-class opts))))

;;; ============================================================
;;; Tool Proxy Support
;;; ============================================================

(def tool-proxy-models
  "Models that require tool proxy (no native tool_call support).

   These free-tier models return 404 for tool_use but are excellent
   for reasoning. We use a two-tier architecture:
   - Tier 1: These models for thinking/code generation
   - Tier 2: gpt-oss-120b for actual tool execution

   Note: Paid models like x-ai/grok-code-fast-1 support native tool calls."
  #{"mistralai/devstral-2512:free"
    "mistralai/devstral-small:free"
    "google/gemma-3-4b-it:free"
    "google/gemma-2-9b-it:free"})

(def tool-proxy-model
  "Model to use for actual tool execution (Tier 2).
   Must support native tool_call via OpenRouter."
  "openai/gpt-oss-120b:free")

;;; ============================================================
;;; Tool Proxy Configuration
;;; ============================================================

(def ^:private tool-proxy-config
  "Configuration for the tool proxy architecture.

   Options:
     :enabled        - Enable/disable tool proxy (default: true)
     :model          - Tier 2 model for tool execution
     :intent-pattern - Regex to match tool intent markers
     :max-iterations - Max proxy loops per task (default: 10)
     :fallback-on-error - Use mock dispatch if real fails (default: true)"
  (atom {:enabled true
         :model tool-proxy-model
         :intent-pattern #"\[TOOL:([a-zA-Z_][a-zA-Z0-9_]*)((?:\s+[a-zA-Z_][a-zA-Z0-9_]*=(?:\"[^\"]*\"|[^\s\]]+))*)\]"
         :max-iterations 10
         :fallback-on-error true}))

(defn get-tool-proxy-config
  "Get current tool proxy configuration."
  []
  @tool-proxy-config)

(defn set-tool-proxy-config!
  "Update tool proxy configuration.

   Example:
     (set-tool-proxy-config! {:model \"openai/gpt-4o-mini\" :max-iterations 15})"
  [config]
  (swap! tool-proxy-config merge config)
  @tool-proxy-config)

(defn tool-proxy-enabled?
  "Check if tool proxy is enabled."
  []
  (:enabled @tool-proxy-config))

(defn needs-tool-proxy?
  "Check if a model requires the tool proxy layer.

   Returns true if the model is in tool-proxy-models set,
   meaning it cannot make native tool_call and needs Tier 2 proxy."
  [model]
  (contains? tool-proxy-models model))

(defn get-tool-proxy-model
  "Get the Tier 2 model for tool execution."
  []
  tool-proxy-model)
