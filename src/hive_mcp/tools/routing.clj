(ns hive-mcp.tools.routing
  "MCP tools for drone model routing configuration.

   Provides tools to:
   - View current model routes
   - Update routes for task types
   - Get routing statistics and recommendations
   - Reset session data"
  (:require [hive-mcp.agent.routing :as routing]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Tool Handlers
;;; ============================================================

(defn handle-list-routes
  "List all configured model routes by task type."
  [_params]
  (try
    (let [routes (routing/list-routes)
          formatted (into {}
                          (map (fn [[task-type config]]
                                 [(name task-type)
                                  {:primary (:primary config)
                                   :secondary (:secondary config)
                                   :reason (:reason config)}]))
                          routes)]
      {:text (json/write-str {:routes formatted
                              :task-types (mapv name (keys routes))})})
    (catch Exception e
      (log/error e "Failed to list routes")
      {:text (json/write-str {:error (str "Failed to list routes: " (.getMessage e))})
       :isError true})))

(defn handle-set-route
  "Set the model route for a task type.

   Parameters:
     :task_type - Task type (testing, refactoring, implementation, bugfix, documentation, general)
     :primary   - Primary model identifier
     :secondary - Secondary/fallback model identifier (optional)
     :reason    - Reason for this routing choice (optional)"
  [{:keys [task_type primary secondary reason]}]
  (try
    (when-not task_type
      (throw (ex-info "task_type is required" {:param :task_type})))
    (when-not primary
      (throw (ex-info "primary model is required" {:param :primary})))

    (let [task-key (keyword task_type)
          route-config {:primary primary
                        :secondary (or secondary primary)
                        :reason (or reason "User configured")}]
      (routing/set-route! task-key route-config)
      (log/info "Updated route" {:task-type task_type :primary primary :secondary secondary})
      {:text (json/write-str {:status "ok"
                              :task-type task_type
                              :route route-config})})
    (catch Exception e
      (log/error e "Failed to set route")
      {:text (json/write-str {:error (str "Failed to set route: " (.getMessage e))})
       :isError true})))

(defn handle-get-stats
  "Get routing statistics including success rates and recommendations.

   Parameters:
     :directory - Optional project directory for scoping"
  [{:keys [directory]}]
  (try
    (let [stats (routing/get-routing-stats {:directory directory})
          ;; Simplify session data for JSON serialization
          session-simplified (into {}
                                   (map (fn [[[model task-type] data]]
                                          [(str model ":" (name task-type))
                                           data]))
                                   (:session stats))]
      {:text (json/write-str (assoc stats :session session-simplified))})
    (catch Exception e
      (log/error e "Failed to get routing stats")
      {:text (json/write-str {:error (str "Failed to get stats: " (.getMessage e))})
       :isError true})))

(defn handle-select-model
  "Select the optimal model for a task (preview routing decision).

   Parameters:
     :task      - Task description (required)
     :files     - List of file paths (optional)
     :directory - Project directory for scoping"
  [{:keys [task files directory]}]
  (try
    (when-not task
      (throw (ex-info "task is required" {:param :task})))

    (let [files-vec (when files
                      (if (string? files)
                        [files]
                        (vec files)))
          selection (routing/route-and-select task files-vec {:directory directory})]
      {:text (json/write-str selection)})
    (catch Exception e
      (log/error e "Failed to select model")
      {:text (json/write-str {:error (str "Failed to select model: " (.getMessage e))})
       :isError true})))

(defn handle-reset-session
  "Reset session-based routing data (persisted patterns remain)."
  [_params]
  (try
    (routing/reset-session-rates!)
    (log/info "Reset session routing data")
    {:text (json/write-str {:status "ok" :message "Session routing data reset"})}
    (catch Exception e
      (log/error e "Failed to reset session")
      {:text (json/write-str {:error (str "Failed to reset: " (.getMessage e))})
       :isError true})))

;;; ============================================================
;;; Tool Definitions
;;; ============================================================

(def tools
  "MCP tool definitions for routing configuration."
  [{:name "routing_list_routes"
    :description "List all configured model routes by task type. Shows primary and secondary models for testing, refactoring, implementation, bugfix, documentation, and general tasks."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-list-routes}

   {:name "routing_set_route"
    :description "Set the model route for a task type. Configure which models handle specific task categories."
    :inputSchema {:type "object"
                  :properties {:task_type {:type "string"
                                           :description "Task type (testing, refactoring, implementation, bugfix, documentation, general)"
                                           :enum ["testing" "refactoring" "implementation" "bugfix" "documentation" "general"]}
                               :primary {:type "string"
                                         :description "Primary model identifier (e.g., mistralai/devstral-2512:free)"}
                               :secondary {:type "string"
                                           :description "Secondary/fallback model identifier"}
                               :reason {:type "string"
                                        :description "Reason for this routing choice"}}
                  :required ["task_type" "primary"]}
    :handler handle-set-route}

   {:name "routing_get_stats"
    :description "Get routing statistics including success rates, recommendations, and problematic model/task combinations. Use for monitoring and optimization."
    :inputSchema {:type "object"
                  :properties {:directory {:type "string"
                                           :description "Project directory for scoping pattern analysis"}}
                  :required []}
    :handler handle-get-stats}

   {:name "routing_select_model"
    :description "Preview model selection for a task without executing. Shows which model would be selected and why, including routing signals (cooldown, history, feedback)."
    :inputSchema {:type "object"
                  :properties {:task {:type "string"
                                      :description "Task description to route"}
                               :files {:type "array"
                                       :items {:type "string"}
                                       :description "List of file paths that will be modified"}
                               :directory {:type "string"
                                           :description "Project directory for pattern scoping"}}
                  :required ["task"]}
    :handler handle-select-model}

   {:name "routing_reset_session"
    :description "Reset session-based routing data (cooldowns, recent failures). Persisted patterns in Chroma are not affected."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handle-reset-session}])
