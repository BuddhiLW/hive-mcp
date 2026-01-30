(ns hive-mcp.tools.consolidated.cider
  "Consolidated CIDER CLI tool.

   Subcommands: eval, doc, info, session, complete, apropos

   Usage via MCP: cider {\"command\": \"eval\", \"code\": \"(+ 1 2)\"}

   SOLID: Facade pattern - single tool entry point for CIDER operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.cider :as cider-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:eval     cider-handlers/handle-cider-eval-silent
   :eval-explicit cider-handlers/handle-cider-eval-explicit
   :doc      cider-handlers/handle-cider-doc
   :info     cider-handlers/handle-cider-info
   :complete cider-handlers/handle-cider-complete
   :apropos  cider-handlers/handle-cider-apropos
   :status   cider-handlers/handle-cider-status
   :spawn    cider-handlers/handle-cider-spawn-session
   :sessions cider-handlers/handle-cider-list-sessions
   :eval-session cider-handlers/handle-cider-eval-session
   :kill-session cider-handlers/handle-cider-kill-session})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-cider
  "Unified CLI handler for CIDER operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated cider command."
  {:name "cider"
   :description "CIDER REPL operations: eval (silent), eval-explicit (show in REPL), doc (docstring), info (full metadata), complete (completions), apropos (search symbols), status (connection), spawn/sessions/kill-session (multi-REPL). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["eval" "eval-explicit" "doc" "info" "complete" "apropos" "status" "spawn" "sessions" "eval-session" "kill-session" "help"]
                                         :description "CIDER operation to perform"}
                              ;; eval params
                              "code" {:type "string"
                                      :description "Clojure code to evaluate"}
                              ;; doc/info/complete params
                              "symbol" {:type "string"
                                        :description "Symbol name for doc/info lookup"}
                              "prefix" {:type "string"
                                        :description "Prefix for completion"}
                              ;; apropos params
                              "pattern" {:type "string"
                                         :description "Regex pattern for apropos search"}
                              "search_docs" {:type "boolean"
                                             :description "Also search docstrings"}
                              ;; session params
                              "name" {:type "string"
                                      :description "Session name for spawn"}
                              "session_name" {:type "string"
                                              :description "Session name for eval-session/kill-session"}
                              "project_dir" {:type "string"
                                             :description "Project directory for spawn"}
                              "agent_id" {:type "string"
                                          :description "Agent ID to link session"}}
                 :required ["command"]}
   :handler handle-cider})

(def tools
  "Tool definitions for registration."
  [tool-def])
