(ns hive-mcp.tools.consolidated.kanban
  "Consolidated Kanban CLI tool.

   Canonical commands: list, create, update, status, sync, plan-to-kanban, batch-update
   Deprecated aliases: move→update, roadmap→status, my-tasks→list

   Usage via MCP: kanban {\"command\": \"list\", \"status\": \"inprogress\"}

   SOLID: Facade pattern - single tool entry point for Kanban operations.
   CLARITY: L - Thin adapter delegating to memory-kanban (Chroma backend)."
  (:require [clojure.tools.logging :as log]
            [hive-mcp.tools.cli :refer [make-cli-handler make-batch-handler]]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.plan.tool :as plan-tool]))

;; =============================================================================
;; Canonical Handlers - the real commands
;; =============================================================================

(def ^:private canonical-handlers
  "Core command → handler map. These are the canonical commands."
  {:list           mem-kanban/handle-mem-kanban-list-slim
   :create         mem-kanban/handle-mem-kanban-create
   :update         mem-kanban/handle-mem-kanban-move
   :status         mem-kanban/handle-mem-kanban-stats
   :sync           (fn [_] {:success true :message "Memory kanban is single-backend, no sync needed"})
   :plan-to-kanban plan-tool/handle-plan-to-kanban
   :batch-update   (let [inner (make-batch-handler {:update mem-kanban/handle-mem-kanban-move})]
                     (fn [{:keys [operations] :as params}]
                       (inner (assoc params :operations
                                     (mapv #(assoc % :command "update") operations)))))})

;; =============================================================================
;; Deprecated Aliases → route to canonical handlers
;; =============================================================================

(def ^:private deprecated-aliases
  "DEPRECATED alias → canonical command mapping.
   These exist for backward compatibility only.
     move     → update  (move is just a status change, subset of update)
     roadmap  → status  (roadmap is status with milestone view)
     my-tasks → list    (my-tasks is list with agent filter)"
  {:move     :update
   :roadmap  :status
   :my-tasks :list})

(defn- wrap-deprecated
  "Wrap a canonical handler with deprecation logging."
  [alias-key canonical-key handler-fn]
  (fn [params]
    (log/warn {:event :deprecation-warning
               :command (name alias-key)
               :canonical (name canonical-key)
               :message (str "DEPRECATED: '" (name alias-key)
                             "' is deprecated. Use '" (name canonical-key)
                             "' instead.")})
    (handler-fn params)))

(def handlers
  "Map of command keywords to handler functions.
   Merges canonical handlers with deprecated aliases (with deprecation logging)."
  (merge canonical-handlers
         (reduce-kv (fn [m alias-key canonical-key]
                      (assoc m alias-key
                             (wrap-deprecated alias-key canonical-key
                                              (get canonical-handlers canonical-key))))
                    {}
                    deprecated-aliases)))

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-kanban
  "Unified CLI handler for Kanban operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated kanban command."
  {:name "kanban"
   :consolidated true
   :description "Kanban task management: list (all/filtered tasks), create (new task), update (change status/modify task), status (board overview + milestones), sync (backends), plan-to-kanban (convert plan to tasks, supports plan_id or plan_path), batch-update (bulk status changes). Aliases (deprecated): move→update, roadmap→status, my-tasks→list. Use command='help' to list all. HCR: list and status automatically aggregate descendant project tasks when project has children in the hierarchy tree."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["list" "create" "move" "status" "update" "roadmap" "my-tasks" "sync" "plan-to-kanban" "batch-update" "help"]
                                         :description "Kanban operation to perform"}
                              ;; list params
                              "status" {:type "string"
                                        :enum ["todo" "inprogress" "inreview" "done"]
                                        :description "Filter by task status"}
                              ;; create params
                              "title" {:type "string"
                                       :description "Task title for create"}
                              "description" {:type "string"
                                             :description "Task description"}
                              ;; move/update params
                              "task_id" {:type "string"
                                         :description "Task ID to move/update"}
                              "new_status" {:type "string"
                                            :enum ["todo" "inprogress" "inreview" "done"]
                                            :description "Target status for move"}
                              ;; plan-to-kanban params
                              "plan_id" {:type "string"
                                         :description "Memory entry ID containing the plan (for plan-to-kanban)"}
                              "plan_path" {:type "string"
                                           :description "File path to a plan file (alternative to plan_id for plan-to-kanban). Slurps file content directly — zero-token plan loading for large plans."}
                              ;; batch-update params
                              "operations" {:type "array"
                                            :items {:type "object"
                                                    :properties {"task_id" {:type "string"}
                                                                 "new_status" {:type "string"
                                                                               :enum ["todo" "inprogress" "inreview" "done"]}
                                                                 "description" {:type "string"}}
                                                    :required ["task_id"]}
                                            :description "Array of update operations for batch-update command"}
                              ;; scope params
                              "directory" {:type "string"
                                           :description "Working directory for project scope (auto-detected if not provided)"}
                              ;; batch common params
                              "parallel" {:type "boolean"
                                          :description "Run batch operations in parallel (default: false)"}}
                 :required ["command"]}
   :handler handle-kanban})

(def tools
  "Tool definitions for registration."
  [tool-def])
