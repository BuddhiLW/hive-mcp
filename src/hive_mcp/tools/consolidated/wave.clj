(ns hive-mcp.tools.consolidated.wave
  "Consolidated Wave CLI tool.

   Subcommands: dispatch, status, approve, reject, review, auto-approve

   Usage via MCP: wave {\"command\": \"dispatch\", \"tasks\": [...]}

   SOLID: Facade pattern - single tool entry point for drone wave operations.
   CLARITY: L - Thin adapter delegating to domain handlers."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler format-help]]
            [hive-mcp.tools.swarm :as swarm-handlers]
            [hive-mcp.tools.diff :as diff-handlers]))

;; =============================================================================
;; Handlers Map - Wire commands to existing handlers
;; =============================================================================

(def handlers
  "Map of command keywords to handler functions."
  {:dispatch      swarm-handlers/handle-dispatch-drone-wave
   :dispatch-validated swarm-handlers/handle-dispatch-validated-wave
   :status        swarm-handlers/handle-get-wave-status
   :review        diff-handlers/handle-review-wave-diffs
   :approve       diff-handlers/handle-approve-wave-diffs
   :reject        diff-handlers/handle-reject-wave-diffs
   :auto-approve  diff-handlers/handle-auto-approve-wave-diffs})

;; =============================================================================
;; CLI Handler
;; =============================================================================

(def handle-wave
  "Unified CLI handler for drone wave operations."
  (make-cli-handler handlers))

;; =============================================================================
;; Tool Definition
;; =============================================================================

(def tool-def
  "MCP tool definition for consolidated wave command."
  {:name "wave"
   :description "Drone wave operations: dispatch (parallel drones), dispatch-validated (with lint), status (execution progress), review (see proposed diffs), approve (apply diffs), reject (discard diffs), auto-approve (safe diffs only). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["dispatch" "dispatch-validated" "status" "review" "approve" "reject" "auto-approve" "help"]
                                         :description "Wave operation to perform"}
                              ;; dispatch params
                              "tasks" {:type "array"
                                       :items {:type "object"
                                               :properties {"file" {:type "string"}
                                                            "task" {:type "string"}}
                                               :required ["file" "task"]}
                                       :description "Array of {file, task} objects"}
                              "preset" {:type "string"
                                        :description "Drone preset (default: drone-worker)"}
                              "trace" {:type "boolean"
                                       :description "Emit progress events"}
                              "cwd" {:type "string"
                                     :description "Working directory for path resolution"}
                              ;; dispatch-validated params
                              "validate" {:type "boolean"
                                          :description "Run clj-kondo lint after execution"}
                              "max_retries" {:type "integer"
                                             :description "Max retry iterations"}
                              "lint_level" {:type "string"
                                            :enum ["error" "warning" "info"]
                                            :description "Lint severity threshold"}
                              ;; status/review/approve/reject params
                              "wave_id" {:type "string"
                                         :description "Wave ID to operate on"}
                              ;; approve params
                              "diff_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Specific diff IDs to approve"}
                              ;; reject params
                              "reason" {:type "string"
                                        :description "Reason for rejection"}}
                 :required ["command"]}
   :handler handle-wave})

(def tools
  "Tool definitions for registration."
  [tool-def])
