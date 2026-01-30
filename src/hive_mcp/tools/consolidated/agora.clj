(ns hive-mcp.tools.consolidated.agora
  "Consolidated Agora CLI tool.

   Subcommands: dialogue, dispatch, consensus, list, join, history, debate, debate-status, continue

   Usage via MCP: agora {\"command\": \"dialogue\", \"participants\": [...], \"topic\": \"...\"}

   SOLID: Facade pattern - single tool entry point for Agora dialogue operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.agora :as agora-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:dialogue      agora-handlers/handle-agora-create-dialogue
   :dispatch      agora-handlers/handle-agora-dispatch
   :consensus     agora-handlers/handle-agora-check-consensus
   :list          agora-handlers/handle-agora-list-dialogues
   :join          agora-handlers/handle-agora-join-dialogue
   :history       agora-handlers/handle-agora-get-history
   :debate        agora-handlers/handle-agora-create-debate
   :debate-status agora-handlers/handle-agora-debate-status
   :continue      agora-handlers/handle-agora-continue-debate
   :list-debates  agora-handlers/handle-agora-list-debates
   :staged        agora-handlers/handle-agora-create-staged-debate
   :stage-status  agora-handlers/handle-agora-stage-status})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-agora
  "Unified CLI handler for Agora dialogue operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated agora command."
  {:name "agora"
   :description "Agora dialogue system: dialogue (create), dispatch (send message), consensus (check Nash equilibrium), list (all dialogues), join (add participant), history (transcript), debate/debate-status/continue (drone debates), staged/stage-status (two-stage). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["dialogue" "dispatch" "consensus" "list" "join" "history" "debate" "debate-status" "continue" "list-debates" "staged" "stage-status" "help"]
                                         :description "Agora operation to perform"}
                              ;; dialogue params
                              "participants" {:type "array"
                                              :items {:type "string"}
                                              :description "Vector of ling slave-ids (min 2)"}
                              "topic" {:type "string"
                                       :description "Dialogue topic"}
                              "config" {:type "object"
                                        :description "Optional: {threshold, timeout-ms}"}
                              ;; dispatch params
                              "dialogue_id" {:type "string"
                                             :description "Dialogue ID"}
                              "to" {:type "string"
                                    :description "Target ling slave-id"}
                              "message" {:type "string"
                                         :description "Message content"}
                              "signal" {:type "string"
                                        :enum ["propose" "counter" "approve" "no-change" "defer"]
                                        :description "Explicit signal"}
                              "from" {:type "string"
                                      :description "Sender slave-id"}
                              "timeout_ms" {:type "number"
                                            :description "Dispatch timeout"}
                              "files" {:type "array"
                                       :items {:type "string"}
                                       :description "Related files"}
                              ;; list params
                              "status" {:type "string"
                                        :enum ["active" "consensus" "timeout" "aborted"]
                                        :description "Filter by status"}
                              ;; join params
                              "slave_id" {:type "string"
                                          :description "Slave-id to join"}
                              ;; history params
                              "limit" {:type "integer"
                                       :description "Limit to last N turns"}
                              ;; debate params
                              "roles" {:type "array"
                                       :items {:type "object"
                                               :properties {"role" {:type "string"}
                                                            "position" {:type "string"}}}
                                       :description "Debate roles"}
                              "methodology" {:type "string"
                                             :enum ["opinion" "fact-based" "mixed"]
                                             :description "Debate methodology"}
                              "blocking" {:type "boolean"
                                          :description "Run to completion sync"}
                              ;; staged debate params
                              "research_roles" {:type "array"
                                                :items {:type "object"}
                                                :description "Research roles for stage 1"}
                              "debate_roles" {:type "array"
                                              :items {:type "object"}
                                              :description "Debate roles for stage 2"}}
                 :required ["command"]}
   :handler handle-agora})

(def tools
  "Tool definitions for registration."
  [tool-def])
