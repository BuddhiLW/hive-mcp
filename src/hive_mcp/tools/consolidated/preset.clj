(ns hive-mcp.tools.consolidated.preset
  "Consolidated Preset CLI tool.

   Subcommands: list, get, search, add, delete, status, migrate

   Usage via MCP: preset {\"command\": \"search\", \"query\": \"testing focused\"}

   SOLID: Facade pattern - single tool entry point for preset operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.presets :as preset-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:list    preset-handlers/handle-preset-list
   :get     preset-handlers/handle-preset-get
   :search  preset-handlers/handle-preset-search
   :add     preset-handlers/handle-preset-add
   :delete  preset-handlers/handle-preset-delete
   :status  preset-handlers/handle-preset-status
   :migrate preset-handlers/handle-preset-migrate})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-preset
  "Unified CLI handler for preset operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated preset command."
  {:name "preset"
   :description "Swarm preset management: list (all presets), get (by name), search (semantic query), add (custom preset), delete (remove), status (integration info), migrate (from files to Chroma). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["list" "get" "search" "add" "delete" "status" "migrate" "help"]
                                         :description "Preset operation to perform"}
                              ;; get/delete params
                              "name" {:type "string"
                                      :description "Preset name"}
                              ;; search params
                              "query" {:type "string"
                                       :description "Natural language search query"}
                              "limit" {:type "integer"
                                       :description "Maximum results to return"}
                              "category" {:type "string"
                                          :enum ["testing" "coding" "architecture" "coordination" "workflow" "general"]
                                          :description "Filter by category"}
                              ;; add params
                              "content" {:type "string"
                                         :description "Full markdown content of preset"}
                              "tags" {:type "array"
                                      :items {:type "string"}
                                      :description "Tags for searchability"}
                              ;; migrate params
                              "directory" {:type "string"
                                           :description "Path to directory containing .md preset files"}}
                 :required ["command"]}
   :handler handle-preset})

(def tools
  "Tool definitions for registration."
  [tool-def])
