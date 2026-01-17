(ns hive-mcp.agent.config
  "OpenRouter configuration for agent delegation.
   
   Manages task-type to model mappings and preset configurations.
   All state is held in atoms for runtime configurability via MCP."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Task-Type Model Configuration
;;; ============================================================

(defonce task-models
  ;; Task-type to model mapping for OpenRouter free tier.
  ;; Configurable via MCP tools from Elisp.
  ;;
  ;; Default task types:
  ;;   :coding      - Code generation, implementation, bug fixes
  ;;   :coding-alt  - Fallback for coding tasks  
  ;;   :arch        - Architecture, design decisions, planning
  ;;   :docs        - Documentation, explanations, comments
  (atom {:coding "mistralai/devstral-2512:free"
         :coding-alt "google/gemma-3-4b-it:free"
         :arch "xiaomi/mimo-v2-flash:free"
         :docs "openai/gpt-oss-120b:free"}))

(defn list-models
  "List all configured OpenRouter task models."
  []
  @task-models)

(defn set-model!
  "Set the model for a task type.
   
   Example: (set-model! :coding \"anthropic/claude-3-haiku\")"
  [task-type model]
  (swap! task-models assoc task-type model)
  @task-models)

(defn remove-model!
  "Remove a task type from the model mapping."
  [task-type]
  (swap! task-models dissoc task-type)
  @task-models)

(defn get-model
  "Get the model for a task type. Falls back to :coding if not found."
  [task-type]
  (or (get @task-models task-type)
      (get @task-models :coding)))

;;; ============================================================
;;; Preset Configuration
;;; ============================================================

(defonce preset-task-types
  ;; Mapping from swarm presets/roles to OpenRouter task types.
  ;; Configurable via MCP tools from Elisp.
  ;;
  ;; Preset categories:
  ;;   :coding     - Implementation, testing, bug fixing
  ;;   :coding-alt - Alternative coding model (refactoring, exploration)
  ;;   :arch       - Architecture, design, review, planning
  ;;   :docs       - Documentation, explanations
  (atom {;; Implementation-focused
         "tdd" :coding
         "tester" :coding
         "fixer" :coding
         "refactorer" :coding
         "ling" :coding
         "ling-worker" :coding
         "ling-tdd" :coding
         "ling-fix" :coding
         "minimal" :coding
         ;; Alternative coding (refactoring, exploration)
         "ling-refactor" :coding-alt
         ;; Architecture/design-focused
         "reviewer" :arch
         "clarity" :arch
         "solid" :arch
         "ddd" :arch
         "researcher" :arch
         "task-coordinator" :arch
         "hivemind" :arch
         "hivemind-master" :arch
         "hive-master" :arch
         "mcp-first" :arch
         "ling-pattern" :arch
         ;; Documentation-focused
         "documenter" :docs
         "ling-docs" :docs}))

(defn preset->task-type
  "Get the task type for a preset name. Returns :coding as default."
  [preset]
  (get @preset-task-types (name preset) :coding))

(defn list-preset-mappings
  "List all preset to task-type mappings."
  []
  @preset-task-types)

(defn set-preset-task-type!
  "Set the task type for a preset.
   
   Example: (set-preset-task-type! \"my-preset\" :arch)"
  [preset task-type]
  (swap! preset-task-types assoc (name preset) (keyword task-type))
  @preset-task-types)

;;; ============================================================
;;; Backend Factory
;;; ============================================================

(defn resolve-model
  "Resolve the model to use based on priority:
   explicit model > preset-derived > task-type > :coding default

   Returns the selected model string.

   CLARITY-T: Debug logs the resolution path for traceability."
  [{:keys [model preset task-type]}]
  (let [resolved-task-type (or (when preset (preset->task-type preset))
                               (keyword task-type)
                               :coding)
        ;; Determine which source provided the model
        [resolved-model source] (cond
                                  model [model :explicit]
                                  (get-model resolved-task-type)
                                  [(get-model resolved-task-type) (keyword (str "task-type-" (name resolved-task-type)))]
                                  :else [(get-model :coding) :default-coding])]
    ;; CLARITY-T: Trace the resolution path for debugging
    (log/debug "Model resolution:" {:preset preset
                                    :task-type task-type
                                    :resolved-task-type resolved-task-type
                                    :source source
                                    :model resolved-model})
    resolved-model))

(defn resolve-model-with-trace
  "Like resolve-model but returns the resolution path for debugging.

   Returns map with:
   - :model       - The resolved model string
   - :source      - Where the model came from (:explicit :task-type-* :default-coding)
   - :preset      - Input preset
   - :task-type   - Resolved task type

   Useful for debugging model selection issues."
  [{:keys [model preset task-type] :as opts}]
  (let [resolved-task-type (or (when preset (preset->task-type preset))
                               (keyword task-type)
                               :coding)
        [resolved-model source] (cond
                                  model [model :explicit]
                                  (get-model resolved-task-type)
                                  [(get-model resolved-task-type) (keyword (str "task-type-" (name resolved-task-type)))]
                                  :else [(get-model :coding) :default-coding])]
    {:model resolved-model
     :source source
     :preset preset
     :task-type task-type
     :resolved-task-type resolved-task-type}))

(defn openrouter-backend
  "Create an OpenRouter backend for agent delegation.
   
   Options:
     :model     - Explicit model name (highest priority)
     :preset    - Swarm preset name for auto task-type selection
     :task-type - Task type for model selection (:coding :arch :docs)
     :api-key   - OpenRouter API key (or set OPENROUTER_API_KEY env)
   
   Priority: model > preset > task-type > :coding (default)"
  [{:keys [model preset task-type api-key]
    :or {task-type :coding}}]
  (let [resolved-model (resolve-model {:model model :preset preset :task-type task-type})
        key (or api-key (System/getenv "OPENROUTER_API_KEY"))]
    (when-not key
      (throw (ex-info "OpenRouter API key required" {:env "OPENROUTER_API_KEY"})))
    (log/debug "OpenRouter backend" {:preset preset :task-type task-type :model resolved-model})
    (require 'hive-mcp.agent.openrouter)
    ((resolve 'hive-mcp.agent.openrouter/->OpenRouterBackend) key resolved-model)))
