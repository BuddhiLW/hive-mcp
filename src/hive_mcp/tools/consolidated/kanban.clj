(ns hive-mcp.tools.consolidated.kanban
  "Consolidated Kanban CLI tool.

   Subcommands: list, create, move, status

   Usage via MCP: kanban {\"command\": \"list\", \"status\": \"inprogress\"}

   SOLID: Facade pattern - single tool entry point for Kanban operations.
   CLARITY: L - Thin adapter delegating to memory-kanban (Chroma backend)."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.memory-kanban :as mem-kanban]))

;; =============================================================================
;; Handlers Map - Wire commands to memory-kanban handlers (Chroma backend)
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:list     mem-kanban/handle-mem-kanban-list-slim
   :create   mem-kanban/handle-mem-kanban-create
   :move     mem-kanban/handle-mem-kanban-move
   :status   mem-kanban/handle-mem-kanban-stats
   :update   mem-kanban/handle-mem-kanban-move  ; move handles status updates
   :roadmap  mem-kanban/handle-mem-kanban-stats ; stats includes overview
   :my-tasks mem-kanban/handle-mem-kanban-list-slim
   :sync     (fn [_] {:success true :message "Memory kanban is single-backend, no sync needed"})})

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
   :description "Kanban task management: list (all/filtered tasks), create (new task), move (change status), status (board overview), update (modify task), roadmap (milestones), my-tasks (agent's tasks), sync (backends). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["list" "create" "move" "status" "update" "roadmap" "my-tasks" "sync" "help"]
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
                                            :description "Target status for move"}}
                 :required ["command"]}
   :handler handle-kanban})

(def tools
  "Tool definitions for registration."
  [tool-def])
