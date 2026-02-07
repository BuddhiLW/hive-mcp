(ns hive-mcp.tools.consolidated.magit
  "Consolidated Magit CLI tool.

   Subcommands: status, stage, commit, push, branches, log, diff, pull, fetch,
                feature-branches, batch-commit

   Usage via MCP: magit {\"command\": \"status\", \"directory\": \"/path/to/repo\"}
   Batch usage:   magit {\"command\": \"batch-commit\", \"operations\": [{\"message\": \"...\", \"files\": \"...\"}, ...]}

   SOLID: Facade pattern - single tool entry point for Git/Magit operations.
   CLARITY: L - Thin adapter delegating to domain handlers.
   CLARITY: C - Batch handler composed via make-batch-handler HOF."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler make-batch-handler]]
            [hive-mcp.tools.magit :as magit-handlers]))

;; =============================================================================
;; Batch-Commit Handler (via make-batch-handler HOF)
;; =============================================================================

(def ^:private batch-commit-handler
  "Batch commit: accepts {:operations [{:message \"...\", :files \"...\", :all true}, ...], :parallel bool}.
   Each operation is a commit with its own message+files.
   Shared params (directory) from outer call merge with per-op params.

   Uses make-batch-handler HOF from cli.clj — same pattern as kanban batch-update,
   agent batch-spawn, kg batch-edge.

   Parameters:
     operations - Array of commit parameter objects:
                  [{:message \"feat: X\" :files \"src/a.clj\"}, {:message \"fix: Y\" :all true}]
     parallel   - Run commits in parallel (default: false). NOTE: sequential is
                  usually safer for git commits to avoid conflicts.
     directory  - Shared directory for all commits (merged into each op)

   Returns: {:results [...] :summary {:total N :success M :failed F}}"
  (let [commit-handlers {:commit magit-handlers/handle-magit-commit}
        batch-fn (make-batch-handler commit-handlers)]
    (fn [{:keys [operations] :as params}]
      (if (or (nil? operations) (empty? operations))
        {:isError true :text "operations is required (array of commit parameter objects, each with :message)"}
        (let [;; Auto-inject :command "commit" into each operation
              ops-with-command (mapv #(assoc % :command "commit") operations)]
          (batch-fn (assoc params :operations ops-with-command)))))))

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
   :feature-branches magit-handlers/handle-magit-feature-branches
   :batch-commit batch-commit-handler})

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
   :consolidated true
   :description "Git operations via Magit: status (repo state), stage (add files), commit (create commit), push (to remote), branches (list all), log (recent commits), diff (view changes), pull/fetch (from remote), feature-branches (for /ship). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["status" "stage" "commit" "push" "branches" "log" "diff" "pull" "fetch" "feature-branches" "batch-commit" "help"]
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
                                        :description "Specific remote to fetch from"}
                              ;; batch-commit params
                              "operations" {:type "array"
                                            :items {:type "object"
                                                    :properties {"message" {:type "string"}
                                                                 "files" {:type "string"}
                                                                 "all" {:type "boolean"}}
                                                    :required ["message"]}
                                            :description "Array of commit operations for batch-commit. Each: {message, files?, all?}"}
                              "parallel" {:type "boolean"
                                          :description "Run batch operations in parallel (default: false)"}}
                 :required ["command"]}
   :handler handle-magit})

(def tools
  "Tool definitions for registration."
  [tool-def])
