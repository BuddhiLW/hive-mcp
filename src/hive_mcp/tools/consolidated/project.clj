(ns hive-mcp.tools.consolidated.project
  "Consolidated Project (Projectile) CLI tool.

   Subcommands: info, files, search, find, recent, list

   Usage via MCP: project {\"command\": \"info\"}

   SOLID: Facade pattern - single tool entry point for projectile operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.projectile :as projectile-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:info   projectile-handlers/handle-projectile-info
   :files  projectile-handlers/handle-projectile-files
   :search projectile-handlers/handle-projectile-search
   :find   projectile-handlers/handle-projectile-find-file
   :recent projectile-handlers/handle-projectile-recent
   :list   projectile-handlers/handle-projectile-list-projects})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-project
  "Unified CLI handler for projectile operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated project command."
  {:name "project"
   :description "Projectile project operations: info (project details), files (list files), search (content search), find (find by filename), recent (recently visited), list (all projects). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["info" "files" "search" "find" "recent" "list" "help"]
                                         :description "Project operation to perform"}
                              ;; files params
                              "pattern" {:type "string"
                                         :description "Glob pattern to filter files or search pattern"}
                              ;; find params
                              "filename" {:type "string"
                                          :description "Filename to search for"}}
                 :required ["command"]}
   :handler handle-project})

(def tools
  "Tool definitions for registration."
  [tool-def])
