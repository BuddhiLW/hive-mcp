(ns hive-mcp.tools.consolidated.hivemind
  "Consolidated Hivemind CLI tool.

   Subcommands: shout, ask, status, respond, messages

   Usage via MCP: hivemind {\"command\": \"shout\", \"event_type\": \"progress\", \"message\": \"50% done\"}

   SOLID: Facade pattern - single tool entry point for hivemind coordination.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.hivemind :as hm]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

;; Extract handlers from hivemind tools definitions
(def ^:private tools-by-name
  (into {} (map (fn [t] [(keyword (clojure.string/replace (:name t) "hivemind_" "")) (:handler t)])
                hm/tools)))

(def handlers
  "Map of command keywords to handler functions."
  {:shout    (:shout tools-by-name)
   :ask      (:ask tools-by-name)
   :status   (:status tools-by-name)
   :respond  (:respond tools-by-name)
   :messages (:messages tools-by-name)})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-hivemind
  "Unified CLI handler for hivemind coordination."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated hivemind command."
  {:name "hivemind"
   :description "Hivemind coordination: shout (broadcast status), ask (request decision), status (coordinator state), respond (answer ask), messages (agent history). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["shout" "ask" "status" "respond" "messages" "help"]
                                         :description "Hivemind operation to perform"}
                              ;; shout params
                              "event_type" {:type "string"
                                            :enum ["progress" "completed" "error" "blocked" "started"]
                                            :description "Type of event for shout"}
                              "task" {:type "string"
                                      :description "Current task description"}
                              "message" {:type "string"
                                         :description "Status message"}
                              "data" {:type "object"
                                      :description "Additional event data"}
                              "directory" {:type "string"
                                           :description "Working directory for project-id derivation"}
                              ;; ask params
                              "question" {:type "string"
                                          :description "Question for human coordinator"}
                              "options" {:type "array"
                                         :items {:type "string"}
                                         :description "Available options for ask"}
                              "timeout_ms" {:type "integer"
                                            :description "Timeout in ms (default 300000)"}
                              ;; respond params
                              "ask_id" {:type "string"
                                        :description "ID of the ask to respond to"}
                              "decision" {:type "string"
                                          :description "The decision/response"}
                              ;; messages/common params
                              "agent_id" {:type "string"
                                          :description "Agent identifier"}}
                 :required ["command"]}
   :handler handle-hivemind})

(def tools
  "Tool definitions for registration."
  [tool-def])
