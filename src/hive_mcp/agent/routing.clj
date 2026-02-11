(ns hive-mcp.agent.routing
  "Smart model routing for drone tasks based on task classification and success rates."
  (:require [hive-mcp.agent.drone.preset :as preset]
            [hive-mcp.agent.drone.feedback :as feedback]
            [hive-mcp.config :as config]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn classify-task
  "Classify a task based on description and file paths."
  [task files]
  (preset/get-task-type task files))

(def ^:private hardcoded-routes
  "Fallback routing table when config.edn has no :models.routing key."
  {:testing        {:primary "x-ai/grok-code-fast-1"
                    :secondary "deepseek/deepseek-v3.2"
                    :reason "Grok completes file-writing tasks reliably"}
   :refactoring    {:primary "x-ai/grok-code-fast-1"
                    :secondary "deepseek/deepseek-v3.2"
                    :reason "Grok handles code structure changes"}
   :implementation {:primary "x-ai/grok-code-fast-1"
                    :secondary "deepseek/deepseek-v3.2"
                    :reason "Grok completes file-writing tasks reliably"}
   :bugfix         {:primary "x-ai/grok-code-fast-1"
                    :secondary "deepseek/deepseek-v3.2"
                    :reason "Grok strong at root cause analysis and file writes"}
   :documentation  {:primary "deepseek/deepseek-v3.2"
                    :secondary "x-ai/grok-code-fast-1"
                    :reason "DeepSeek good at natural language"}
   :general        {:primary "x-ai/grok-code-fast-1"
                    :secondary "deepseek/deepseek-v3.2"
                    :reason "Grok as robust default"}})

(def ^:private reason-strings
  "Reason strings per task type."
  {:testing        "Grok completes file-writing tasks reliably"
   :refactoring    "Grok handles code structure changes"
   :implementation "Grok completes file-writing tasks reliably"
   :bugfix         "Grok strong at root cause analysis and file writes"
   :documentation  "DeepSeek good at natural language"
   :general        "Grok as robust default"})

(defn- load-model-routes
  "Load model routes from config.edn :models.routing, with hardcoded fallback."
  []
  (if-let [config-routes (config/get-config-value "models.routing")]
    (reduce-kv (fn [m task-type route]
                 (assoc m task-type
                        (assoc route :reason
                               (get reason-strings task-type
                                    (get hardcoded-routes task-type :reason)))))
               {} config-routes)
    hardcoded-routes))

(def model-routes
  "Task type to primary/secondary model mapping atom."
  (atom (load-model-routes)))

(defn get-route
  "Get the model route for a task type."
  [task-type]
  (get @model-routes task-type (get @model-routes :general)))

(defn set-route!
  "Update the route for a task type."
  [task-type route-config]
  (swap! model-routes assoc task-type route-config)
  @model-routes)

;; In-memory session tracking for fast routing decisions.
;; Format: {[model task-type] {:successes N :failures N :last-failure-time T}}
(defonce ^:private session-rates (atom {}))

(defn record-success!
  "Record a successful task completion for a model/task-type pair."
  [model task-type & [{:keys [duration-ms directory agent-id]}]]
  (swap! session-rates update [model task-type]
         (fnil (fn [stats]
                 (update stats :successes (fnil inc 0)))
               {:successes 0 :failures 0}))
  (feedback/record-pattern! task-type model :success
                            {:duration-ms duration-ms
                             :directory directory
                             :agent-id agent-id})
  (log/debug "Recorded success" {:model model :task-type task-type}))

(defn record-failure!
  "Record a task failure for a model/task-type pair."
  [model task-type result-class & [{:keys [duration-ms directory agent-id]}]]
  (swap! session-rates update [model task-type]
         (fnil (fn [stats]
                 (-> stats
                     (update :failures (fnil inc 0))
                     (assoc :last-failure-time (System/currentTimeMillis))))
               {:successes 0 :failures 0}))
  (feedback/record-pattern! task-type model (or result-class :unknown-failure)
                            {:duration-ms duration-ms
                             :directory directory
                             :agent-id agent-id})
  (log/debug "Recorded failure" {:model model :task-type task-type :class result-class}))

(defn get-success-rate
  "Get combined session and persisted success rate for a model/task-type pair."
  [model task-type & [{:keys [directory]}]]
  (let [session-stats (get @session-rates [model task-type] {:successes 0 :failures 0})
        session-total (+ (:successes session-stats 0) (:failures session-stats 0))
        persisted-stats (feedback/get-success-rate task-type model {:directory directory})
        session-rate (if (pos? session-total)
                       (/ (:successes session-stats 0) session-total)
                       nil)
        persisted-rate (:success-rate persisted-stats)]
    (cond
      (and session-rate persisted-rate)
      (+ (* 0.6 session-rate) (* 0.4 persisted-rate))

      session-rate session-rate
      persisted-rate persisted-rate
      :else 1.0)))

(defn get-all-success-rates
  "Get all success rate data for analysis."
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
  "Reset session success rate tracking."
  []
  (reset! session-rates {}))

(def ^:private cooldown-ms
  "Cooldown period after failure before retrying a model (5 minutes)."
  (* 5 60 1000))

(defn- model-on-cooldown?
  "Check if model is on cooldown for this task type."
  [model task-type]
  (let [stats (get @session-rates [model task-type])
        last-failure (:last-failure-time stats 0)]
    (< (- (System/currentTimeMillis) last-failure) cooldown-ms)))

(defn- model-historically-bad?
  "Check if model has historically bad performance for this task type."
  [model task-type & [{:keys [directory]}]]
  (feedback/should-avoid-combo? task-type model {:directory directory}))

(defn select-model
  "Select optimal model for a task using multi-signal routing."
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
          on-cooldown? (model-on-cooldown? primary task-type)
          historically-bad? (model-historically-bad? primary task-type {:directory directory})
          recommended (feedback/recommend-model task-type [primary secondary] {:directory directory})
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
  "Execute f with automatic fallback to secondary model on failure."
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

(defn route-and-select
  "Classify task and select optimal model for drone delegation."
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
  "Get comprehensive routing statistics for monitoring."
  [& [{:keys [directory]}]]
  (let [feedback-stats (feedback/aggregate-weekly-stats {:directory directory})
        recommendations (feedback/generate-recommendations {:directory directory})]
    {:routes @model-routes
     :session @session-rates
     :feedback feedback-stats
     :recommendations recommendations
     :config {:cooldown-ms cooldown-ms}}))

(defn get-model-for-task
  "Get the best model string for a task."
  [task & [files opts]]
  (:model (select-model task files opts)))

(defn report-execution!
  "Report a drone execution result for routing optimization."
  [task-type model result & [opts]]
  (let [result-class (feedback/classify-result result)]
    (if (= result-class :success)
      (record-success! model task-type opts)
      (record-failure! model task-type result-class opts))))

(def tool-proxy-models
  "Models that require tool proxy (no native tool_call support)."
  #{"mistralai/devstral-2512:free"
    "mistralai/devstral-small:free"
    "google/gemma-3-4b-it:free"
    "google/gemma-2-9b-it:free"})

(def tool-proxy-model
  "Model used for actual tool execution (Tier 2)."
  "openai/gpt-oss-120b:free")

(def ^:private tool-proxy-config
  "Configuration for the tool proxy architecture."
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
  "Update tool proxy configuration."
  [config]
  (swap! tool-proxy-config merge config)
  @tool-proxy-config)

(defn tool-proxy-enabled?
  "Check if tool proxy is enabled."
  []
  (:enabled @tool-proxy-config))

(defn needs-tool-proxy?
  "Check if a model requires the tool proxy layer."
  [model]
  (contains? tool-proxy-models model))

(defn get-tool-proxy-model
  "Get the Tier 2 model for tool execution."
  []
  tool-proxy-model)
