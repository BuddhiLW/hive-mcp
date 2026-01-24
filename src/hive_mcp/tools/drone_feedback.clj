(ns hive-mcp.tools.drone-feedback
  "MCP tools for drone feedback and execution pattern learning.

   CLARITY Framework:
   - C: Composition - builds on existing memory infrastructure
   - L: Layers pure - validation separate from storage
   - I: Inputs guarded - validates category and required fields
   - T: Telemetry first - logs feedback for coordinator review
   - Y: Yield safe - routes away from failing model/task combos

   Two types of feedback:
   1. Subjective feedback (tool-missing, workflow-friction, etc.)
   2. Execution patterns (model/task success/failure rates)

   Drones use this to report:
   - tool-missing: Needed functionality not available
   - tool-broken: Tool exists but doesn't work as expected
   - workflow-friction: Process is inefficient or painful
   - suggestion: Ideas for improvement
   - success: Patterns that worked well

   Storage: Entries are stored as notes with tags:
   - drone-feedback (always)
   - feedback-<category>
   - continuous-improvement
   - agent:<drone-id> (when available)"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.agent.drone.feedback :as fb]
            [hive-mcp.chroma :as chroma]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:private valid-categories
  "Valid feedback category values."
  #{"tool-missing" "tool-broken" "workflow-friction" "suggestion" "success"})

;; =============================================================================
;; Validation (CLARITY: Inputs are guarded)
;; =============================================================================

(defn- validate-feedback
  "Validate feedback input. Returns nil if valid, error map if invalid."
  [{:keys [category message]}]
  (cond
    (nil? message)
    {:error "Missing required field: message"}

    (nil? category)
    {:error "Missing required field: category"}

    (not (valid-categories category))
    {:error (str "Invalid category: " category
                 ". Must be one of: " (pr-str valid-categories))}

    :else nil))

;; =============================================================================
;; Content Formatting
;; =============================================================================

(defn- format-feedback-content
  "Format feedback into structured readable content."
  [{:keys [category tool message context]}]
  (str "## Drone Feedback\n\n"
       "**Category:** " category "\n"
       (when tool (str "**Tool:** " tool "\n"))
       "**Message:** " message "\n"
       (when context (str "**Context:** " context "\n"))))

;; =============================================================================
;; Handler
;; =============================================================================

(defn handle-drone-feedback
  "Submit feedback about drone task execution.

   Required:
   - category: One of tool-missing, tool-broken, workflow-friction, suggestion, success
   - message: Description of what happened or what is needed

   Optional:
   - tool: Name of the tool if applicable
   - context: Task being attempted when feedback was generated
   - agent_id: Drone identifier (auto-detected from CLAUDE_SWARM_SLAVE_ID)"
  [{:keys [category tool _message _context agent_id directory] :as params}]
  (log/info "drone-feedback:" category "tool:" tool)

  ;; Validate input
  (if-let [validation-error (validate-feedback params)]
    (mcp-json validation-error)

    ;; Store feedback in memory
    (with-chroma
      (let [project-id (scope/get-current-project-id directory)
            ;; Agent tagging
            agent-id (or agent_id (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
            agent-tag (when agent-id (str "agent:" agent-id))

            ;; Build tags per ADR spec
            base-tags ["drone-feedback"
                       (str "feedback-" category)
                       "continuous-improvement"]
            tags-with-agent (if agent-tag
                              (conj base-tags agent-tag)
                              base-tags)
            tags-with-scope (scope/inject-project-scope tags-with-agent project-id)

            ;; Format content for readability
            content (format-feedback-content params)
            content-hash (chroma/content-hash content)

            ;; Use medium duration (30 days) - long enough for coordinator review
            duration-str "medium"
            expires (dur/calculate-expires duration-str)

            ;; Check for duplicate feedback
            existing (chroma/find-duplicate "note" content-hash :project-id project-id)]

        (if existing
          ;; Duplicate found - just return existing
          (do
            (log/info "Duplicate feedback found:" (:id existing))
            (mcp-json (fmt/entry->json-alist existing)))

          ;; Create new entry
          (let [entry-id (chroma/index-memory-entry!
                          {:type "note"
                           :content content
                           :tags tags-with-scope
                           :content-hash content-hash
                           :duration duration-str
                           :expires (or expires "")
                           :project-id project-id})
                created (chroma/get-entry-by-id entry-id)]
            (log/info "Created drone feedback:" entry-id "category:" category)
            (mcp-json (fmt/entry->json-alist created))))))))

;; =============================================================================
;; Tool Definition
;; =============================================================================

;; =============================================================================
;; Pattern Analysis Handlers
;; =============================================================================

(defn handle-drone-pattern-stats
  "Get aggregated statistics about drone execution patterns.

   Returns model performance, task success rates, problematic combos,
   and top performers. Use for weekly analysis and model selection."
  [{:keys [directory]}]
  (log/info "drone-pattern-stats for:" (or directory "current project"))
  (let [stats (fb/aggregate-weekly-stats {:directory directory})]
    (mcp-json stats)))

(defn handle-drone-pattern-recommendations
  "Get recommendations for improving drone success rates.

   Analyzes problematic model/task combinations and suggests
   prompt improvements or model switches."
  [{:keys [directory]}]
  (log/info "drone-pattern-recommendations for:" (or directory "current project"))
  (let [recommendations (fb/generate-recommendations {:directory directory})]
    (mcp-json {:recommendations recommendations})))

(defn handle-drone-model-recommend
  "Recommend a model for a specific task type based on historical patterns.

   Uses success rates to pick the best performing model from
   available options."
  [{:keys [task_type models directory]}]
  (log/info "drone-model-recommend for:" task_type "models:" models)
  (if (or (nil? task_type) (nil? models) (empty? models))
    (mcp-json {:error "Required: task_type and models (array of model names)"})
    (let [task-kw (keyword task_type)
          recommended (fb/recommend-model task-kw models {:directory directory})
          stats (fb/get-success-rate task-kw recommended {:directory directory})]
      (mcp-json {:recommended-model recommended
                 :task-type task_type
                 :stats stats}))))

(defn handle-drone-should-avoid
  "Check if a model/task combination should be avoided.

   Returns true if the combo has consistently failed (>=3 samples,
   <30% success rate)."
  [{:keys [task_type model directory]}]
  (log/info "drone-should-avoid:" task_type "/" model)
  (if (or (nil? task_type) (nil? model))
    (mcp-json {:error "Required: task_type and model"})
    (let [avoid? (fb/should-avoid-combo? (keyword task_type) model {:directory directory})
          stats (fb/get-success-rate (keyword task_type) model {:directory directory})]
      (mcp-json {:should-avoid avoid?
                 :task-type task_type
                 :model model
                 :stats stats}))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  "Tool definitions for drone feedback and pattern handlers."
  [{:name "drone_feedback"
    :description "Submit feedback about drone task execution. Report what worked, what didn't, and suggestions for improvement. Feedback is stored for coordinator review during /catchup or /wrap."
    :inputSchema {:type "object"
                  :properties {"category" {:type "string"
                                           :enum ["tool-missing" "tool-broken"
                                                  "workflow-friction" "suggestion" "success"]
                                           :description "Type of feedback"}
                               "tool" {:type "string"
                                       :description "Tool name if applicable (e.g., 'bash', 'cider_eval_silent')"}
                               "message" {:type "string"
                                          :description "What happened or what you need"}
                               "context" {:type "string"
                                          :description "Task you were trying to accomplish"}}
                  :required ["category" "message"]}
    :handler handle-drone-feedback}

   {:name "drone_pattern_stats"
    :description "Get aggregated statistics about drone execution patterns. Returns success rates by model, by task type, problematic combos, and top performers. Use for weekly analysis."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description "Working directory for project scoping"}}}
    :handler handle-drone-pattern-stats}

   {:name "drone_pattern_recommendations"
    :description "Get recommendations for improving drone success rates. Analyzes problematic model/task combinations and suggests prompt improvements."
    :inputSchema {:type "object"
                  :properties {"directory" {:type "string"
                                            :description "Working directory for project scoping"}}}
    :handler handle-drone-pattern-recommendations}

   {:name "drone_model_recommend"
    :description "Recommend a model for a specific task type based on historical success patterns. Pass available models and get the best performer."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :description "Task type keyword (e.g., 'coding', 'refactor', 'fix')"}
                               "models" {:type "array"
                                         :items {:type "string"}
                                         :description "Available model identifiers to choose from"}
                               "directory" {:type "string"
                                            :description "Working directory for project scoping"}}
                  :required ["task_type" "models"]}
    :handler handle-drone-model-recommend}

   {:name "drone_should_avoid"
    :description "Check if a model/task combination should be avoided based on past failures. Returns true if combo has >=3 samples with <30% success rate."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :description "Task type keyword"}
                               "model" {:type "string"
                                        :description "Model identifier to check"}
                               "directory" {:type "string"
                                            :description "Working directory for project scoping"}}
                  :required ["task_type" "model"]}
    :handler handle-drone-should-avoid}])
