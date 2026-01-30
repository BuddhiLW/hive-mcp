(ns hive-mcp.tools.consolidated.olympus
  "Consolidated Olympus grid control CLI tool.

   Subcommands: focus, arrange, tab, status

   Usage via MCP: olympus {\"command\": \"focus\", \"ling-id\": \"ling-123\"}

   SOLID: Facade pattern - single tool entry point for Olympus operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.olympus :as olympus-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:focus   olympus-handlers/handle-olympus-focus
   :arrange olympus-handlers/handle-olympus-arrange
   :tab     olympus-handlers/handle-olympus-tab
   :status  olympus-handlers/handle-olympus-status})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-olympus
  "Unified CLI handler for Olympus grid control."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated olympus command."
  {:name "olympus"
   :description "Olympus grid control: focus (maximize ling), arrange (trigger layout), tab (navigate tabs), status (current layout). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["focus" "arrange" "tab" "status" "help"]
                                         :description "Olympus operation to perform"}
                              ;; focus params
                              "ling-id" {:type "string"
                                         :description "Specific ling ID to focus"}
                              "position" {:type "integer"
                                          :description "Position number (1-4) to focus"}
                              "restore" {:type "boolean"
                                         :description "If true, restore grid view (unfocus)"}
                              ;; arrange params
                              "mode" {:type "string"
                                      :enum ["auto" "manual" "stacked"]
                                      :description "Layout mode for arrange"}
                              ;; tab params
                              "direction" {:type "string"
                                           :enum ["next" "prev"]
                                           :description "Navigate to next or previous tab"}
                              "tab" {:type "integer"
                                     :description "Specific tab number to jump to (0-indexed)"}}
                 :required ["command"]}
   :handler handle-olympus})

(def tools
  "Tool definitions for registration."
  [tool-def])
