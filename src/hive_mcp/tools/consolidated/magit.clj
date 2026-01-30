(ns hive-mcp.tools.consolidated.magit
  "Consolidated Magit CLI tool.

   Subcommands: status, stage, commit, push, branches, log, diff, pull, fetch

   Usage via MCP: magit {\"command\": \"status\", \"directory\": \"/path/to/repo\"}

   SOLID: Facade pattern - single tool entry point for Git/Magit operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.magit :as magit-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:status   magit-handlers/handle-magit-status
   :stage    magit-handlers/handle-magit-stage
   :commit   magit-handlers/handle-magit-commit
   :push     magit-handlers/handle-magit-push
   :branches magit-handlers/handle-magit-branches
   :log      magit-handlers/handle-magit-log
   :diff     magit-handlers/handle-magit-diff
   :pull     magit-handlers/handle-magit-pull
   :fetch    magit-handlers/handle-magit-fetch
   :feature-branches magit-handlers/handle-magit-feature-branches})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-magit
  "Unified CLI handler for Magit/Git operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated magit command."
  {:name "magit"
   :description "Git operations via Magit: status (repo state), stage (add files), commit (create commit), push (to remote), branches (list all), log (recent commits), diff (view changes), pull/fetch (from remote), feature-branches (for /ship). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["status" "stage" "commit" "push" "branches" "log" "diff" "pull" "fetch" "feature-branches" "help"]
                                         :description "Git operation to perform"}
                              ;; Common param
                              "directory" {:type "string"
                                           :description "IMPORTANT: Pass your working directory to target YOUR project"}
                              ;; stage params
                              "files" {:type "string"
                                       :description "File path to stage, or 'all' for all modified"}
                              ;; commit params
                              "message" {:type "string"
                                         :description "Commit message"}
                              "all" {:type "boolean"
                                     :description "Stage all changes before committing"}
                              ;; push params
                              "set_upstream" {:type "boolean"
                                              :description "Set upstream tracking for new branch"}
                              ;; log params
                              "count" {:type "integer"
                                       :description "Number of commits to return (default: 10)"}
                              ;; diff params
                              "target" {:type "string"
                                        :enum ["staged" "unstaged" "all"]
                                        :description "What to diff (default: staged)"}
                              ;; fetch params
                              "remote" {:type "string"
                                        :description "Specific remote to fetch from"}}
                 :required ["command"]}
   :handler handle-magit})

(def tools
  "Tool definitions for registration."
  [tool-def])
