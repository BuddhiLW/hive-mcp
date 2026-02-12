(ns hive-mcp.agent.mcp
  "MCP tool definitions for agent delegation."
  (:require [hive-mcp.agent.config :as config]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.telemetry.prometheus :as prom]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-delegate-drone
  "Handle delegate_drone MCP tool call, delegating to the provided function."
  [delegate-drone-fn {:keys [task files preset trace parent_id cwd]}]
  ;; DEPRECATION WARNING: Prefer unified 'delegate' tool
  (log/warn {:event :deprecation-warning
             :tool "delegate_drone"
             :message "DEPRECATED: Use 'delegate' tool instead. delegate({task: \"...\", files: [...]})"})

  (let [trace-id (str "mcp-" (java.util.UUID/randomUUID))
        start-time-ns (System/nanoTime)
        file-count (count (or files []))
        task-preview (when task (subs task 0 (min 100 (count task))))]

    (prom/inc-mcp-requests! "delegate_drone")

    (log/info {:event :delegate-drone/request-start
               :trace-id trace-id
               :file-count file-count
               :preset (or preset "drone-worker")
               :parent-id parent_id
               :has-cwd (boolean cwd)
               :task-preview task-preview})

    (if (str/blank? task)
      (do
        (log/warn {:event :delegate-drone/validation-failed
                   :trace-id trace-id
                   :error-type :validation
                   :error "Task is required"})
        (prom/inc-errors-total! :validation true)
        (mcp-error "Task is required"))

      (try
        (let [result (delegate-drone-fn {:task task
                                         :files files
                                         :preset (or preset "drone-worker")
                                         :trace (if (nil? trace) true trace)
                                         :parent-id parent_id
                                         :cwd cwd})
              duration-sec (/ (- (System/nanoTime) start-time-ns) 1e9)]

          (prom/observe-request-duration! "delegate_drone" duration-sec)

          ;; Ensure we always return a structured response
          (if (nil? result)
            (do
              (log/warn {:event :delegate-drone/nil-result
                         :trace-id trace-id
                         :duration-sec duration-sec
                         :error-type :execution})
              (prom/inc-errors-total! :execution true)
              (mcp-error "Drone returned nil - execution may have failed silently"))

            (let [status (or (:status result) :unknown)
                  success? (= :completed status)]
              (log/info {:event :delegate-drone/request-end
                         :trace-id trace-id
                         :status status
                         :success success?
                         :duration-sec duration-sec
                         :drone-id (:agent-id result)
                         :files-modified (count (or (:files-modified result) []))
                         :files-failed (count (or (:files-failed result) []))})

              (mcp-json (assoc result
                               :status status
                               :trace-id trace-id
                               :message (or (:message result)
                                            (if success?
                                              "Drone completed successfully"
                                              "Drone execution finished")))))))

        (catch clojure.lang.ExceptionInfo e
          ;; Structured error from delegate! (conflicts, validation, etc.)
          (let [data (ex-data e)
                error-type (or (:error-type data) :delegation-error)
                duration-sec (/ (- (System/nanoTime) start-time-ns) 1e9)
                error-category (case error-type
                                 :conflict :conflict
                                 :validation :validation
                                 :timeout :timeout
                                 :rate-limit :rate-limit
                                 :execution)]

            (prom/observe-request-duration! "delegate_drone" duration-sec)
            (prom/inc-errors-total! error-category (not= error-category :execution))

            (log/error {:event :delegate-drone/failed
                        :trace-id trace-id
                        :error-type error-type
                        :error-category error-category
                        :error (ex-message e)
                        :drone-id (:drone-id data)
                        :conflicts (:conflicts data)
                        :files files
                        :duration-sec duration-sec
                        :task-preview task-preview})

            (mcp-json {:status :failed
                       :error (ex-message e)
                       :error-type error-type
                       :trace-id trace-id
                       :conflicts (:conflicts data)
                       :drone-id (:drone-id data)
                       :files (:files data)})))

        (catch Exception e
          ;; Unexpected error - log and return structured response
          (let [duration-sec (/ (- (System/nanoTime) start-time-ns) 1e9)
                stacktrace-str (pr-str (.getStackTrace e))]

            (prom/observe-request-duration! "delegate_drone" duration-sec)
            (prom/inc-errors-total! :unexpected false)

            (log/error {:event :delegate-drone/unexpected-error
                        :trace-id trace-id
                        :error (ex-message e)
                        :exception-class (.getName (class e))
                        :files files
                        :duration-sec duration-sec
                        :task-preview task-preview
                        :stacktrace (subs stacktrace-str 0 (min 500 (count stacktrace-str)))})

            (mcp-json {:status :failed
                       :error (str "Drone delegation failed: " (ex-message e))
                       :error-type :unexpected
                       :trace-id trace-id
                       :exception-class (.getName (class e))})))))))

(defn make-tools
  "Create MCP tool definitions with injected handler functions."
  [delegate-drone-fn]
  [{:name "delegate_drone"
    :description "Delegate a task to a drone (token-optimized leaf agent). Drones use OpenRouter free-tier models and receive catchup context automatically. Use for file mutations to save coordinator tokens."
    :inputSchema {:type "object"
                  :properties {:task {:type "string"
                                      :description "Task description for the drone"}
                               :files {:type "array"
                                       :items {:type "string"}
                                       :description "List of files the drone will modify"}
                               :preset {:type "string"
                                        :description "Preset to use (default: drone-worker)"}
                               :trace {:type "boolean"
                                       :description "Enable progress events (default: true)"}
                               :parent_id {:type "string"
                                           :description "Parent ling's slave-id for swarm status sync. Pass your CLAUDE_SWARM_SLAVE_ID env var value."}}
                  :required ["task"]}
    :handler (partial handle-delegate-drone delegate-drone-fn)}

   ;; OpenRouter model configuration
   {:name "openrouter_list_models"
    :description "List all configured OpenRouter task-type to model mappings. Shows which models are used for :coding, :arch, :docs tasks."
    :inputSchema {:type "object" :properties {}}
    :handler (fn [_]
               (mcp-json {:models (config/list-models)
                          :task-types (keys (config/list-models))}))}

   {:name "openrouter_set_model"
    :description "Set the OpenRouter model for a specific task type. Task types: coding, coding-alt, arch, docs (or custom)."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :description "Task type (e.g., 'coding', 'arch', 'docs')"}
                               "model" {:type "string"
                                        :description "OpenRouter model ID (e.g., 'mistralai/devstral-2512:free')"}}
                  :required ["task_type" "model"]}
    :handler (fn [{:keys [task_type model]}]
               (let [task-key (keyword task_type)
                     updated (config/set-model! task-key model)]
                 (mcp-json {:success true
                            :message (format "Set %s → %s" task_type model)
                            :models updated})))}

   {:name "openrouter_remove_model"
    :description "Remove an OpenRouter task-type mapping."
    :inputSchema {:type "object"
                  :properties {"task_type" {:type "string"
                                            :description "Task type to remove"}}
                  :required ["task_type"]}
    :handler (fn [{:keys [task_type]}]
               (let [task-key (keyword task_type)
                     updated (config/remove-model! task-key)]
                 (mcp-json {:success true
                            :message (format "Removed %s" task_type)
                            :models updated})))}

   ;; Preset to task-type mappings
   {:name "preset_list_mappings"
    :description "List all swarm preset to task-type mappings. Shows which presets map to :coding, :arch, :docs."
    :inputSchema {:type "object" :properties {}}
    :handler (fn [_]
               (let [mappings (config/list-preset-mappings)
                     by-type (group-by val mappings)]
                 (mcp-json {:mappings mappings
                            :by-task-type {:coding (keys (get by-type :coding))
                                           :arch (keys (get by-type :arch))
                                           :docs (keys (get by-type :docs))}})))}

   {:name "preset_set_task_type"
    :description "Set the task type for a swarm preset. This determines which OpenRouter model is used when delegating with that preset."
    :inputSchema {:type "object"
                  :properties {"preset" {:type "string"
                                         :description "Preset name (e.g., 'tdd', 'reviewer', 'documenter')"}
                               "task_type" {:type "string"
                                            :enum ["coding" "coding-alt" "arch" "docs"]
                                            :description "Task type to map to"}}
                  :required ["preset" "task_type"]}
    :handler (fn [{:keys [preset task_type]}]
               (let [updated (config/set-preset-task-type! preset task_type)]
                 (mcp-json {:success true
                            :message (format "Set preset %s → %s" preset task_type)
                            :mappings updated})))}])
