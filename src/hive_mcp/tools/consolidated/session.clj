(ns hive-mcp.tools.consolidated.session
  "Consolidated Session CLI tool.

   Subcommands: complete, wrap

   Usage via MCP: session {\"command\": \"complete\", \"commit_msg\": \"feat: done\"}

   SOLID: Facade pattern - single tool entry point for session lifecycle.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.session-complete :as session-handlers]
            [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.tools.crystal :as crystal]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Wrap Handler - Delegates to crystal wrap
;; =============================================================================

(defn handle-wrap
  "Wrap session - crystallize learnings without commit.
   Use session_complete for full lifecycle with git commit."
  [{:keys [agent_id directory]}]
  (log/info "session-wrap" {:agent agent_id})
  (try
    ;; Delegate to crystal wrap_crystallize
    (let [result (crystal/handle-wrap-crystallize {:directory directory})]
      result)
    (catch Exception e
      (mcp-error (str "Wrap failed: " (.getMessage e))))))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:complete session-handlers/handle-session-complete
   :wrap     handle-wrap})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-session
  "Unified CLI handler for session lifecycle."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated session command."
  {:name "session"
   :description "Session lifecycle: complete (commit + kanban + wrap + shout), wrap (crystallize only without commit). Use 'complete' at end of work session. Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["complete" "wrap" "help"]
                                         :description "Session operation to perform"}
                              ;; complete params
                              "commit_msg" {:type "string"
                                            :description "Git commit message (required for complete)"}
                              "task_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Kanban task IDs to mark done"}
                              "agent_id" {:type "string"
                                          :description "Ling's slave-id (CLAUDE_SWARM_SLAVE_ID)"}
                              "directory" {:type "string"
                                           :description "Working directory for git/kanban scoping"}}
                 :required ["command"]}
   :handler handle-session})

(def tools
  "Tool definitions for registration."
  [tool-def])
