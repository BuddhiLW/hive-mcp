(ns hive-mcp.tools.diff
  "Diff-based workflow tools for drone agents — facade module.

   This namespace re-exports key public vars from the diff sub-modules
   for backward compatibility. New code should require specific sub-modules:

   - hive-mcp.tools.diff.state       — atoms, TDD status
   - hive-mcp.tools.diff.compute     — pure diff computation
   - hive-mcp.tools.diff.validation  — path validation
   - hive-mcp.tools.diff.auto-approve — approval rules
   - hive-mcp.tools.diff.handlers    — core MCP handlers
   - hive-mcp.tools.diff.wave        — wave batch operations
   - hive-mcp.tools.diff.batch       — multi-drone batch operations

   Workflow:
   1. Drone calls propose_diff with old/new content and description
   2. Hivemind reviews with list_proposed_diffs (sees metadata only)
   3. If needed, get_diff_details shows hunks for inspection
   4. Hivemind calls apply_diff (applies change) or reject_diff (discards)

   Token-Efficient Three-Tier API (ADR 20260125002853):
   - Tier 1: list_proposed_diffs -> metadata + metrics only (~200 tokens/diff)
   - Tier 2: get_diff_details -> formatted hunks (~500 tokens/diff)
   - Tier 3: apply_diff -> uses stored full content internally"
  (:require [hive-mcp.tools.diff.state :as state]
            [hive-mcp.tools.diff.validation :as validation]
            [hive-mcp.tools.diff.handlers :as handlers]
            [hive-mcp.tools.diff.wave :as wave]
            [hive-mcp.tools.diff.batch :as batch]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports: State
;; =============================================================================

(def pending-diffs
  "Atom storing pending diff proposals. Map of diff-id -> diff-data."
  state/pending-diffs)

;; =============================================================================
;; Re-exports: Validation
;; =============================================================================

(def get-project-root
  "Get project root from Emacs or fall back to cwd."
  validation/get-project-root)

(def translate-sandbox-path
  "Translate clojure-mcp sandbox paths back to real project paths."
  validation/translate-sandbox-path)

(def validate-diff-path
  "Validate a file path for propose_diff."
  validation/validate-diff-path)

;; =============================================================================
;; Re-exports: Core Handlers
;; =============================================================================

(def handle-propose-diff
  "Handle propose_diff tool call."
  handlers/handle-propose-diff)

(def handle-list-proposed-diffs
  "Handle list_proposed_diffs tool call."
  handlers/handle-list-proposed-diffs)

(def handle-apply-diff
  "Handle apply_diff tool call."
  handlers/handle-apply-diff)

(def handle-reject-diff
  "Handle reject_diff tool call."
  handlers/handle-reject-diff)

(def handle-get-diff-details
  "Handle get_diff_details tool call."
  handlers/handle-get-diff-details)

;; =============================================================================
;; Re-exports: Wave Operations
;; =============================================================================

(def handle-review-wave-diffs
  "Handle review_wave_diffs tool call."
  wave/handle-review-wave-diffs)

(def handle-approve-wave-diffs
  "Handle approve_wave_diffs tool call."
  wave/handle-approve-wave-diffs)

(def handle-reject-wave-diffs
  "Handle reject_wave_diffs tool call."
  wave/handle-reject-wave-diffs)

(def handle-auto-approve-wave-diffs
  "Handle auto_approve_wave_diffs tool call."
  wave/handle-auto-approve-wave-diffs)

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  [{:name "propose_diff"
    :description "Propose a file change for review by the hivemind. Drones should use this instead of file_write/file_edit. The change will be queued for review and the hivemind will apply or reject it."
    :inputSchema {:type "object"
                  :properties {"file_path" {:type "string"
                                            :description "Absolute path to the file to modify"}
                               "old_content" {:type "string"
                                              :description "The current/expected content of the file (for safety verification)"}
                               "new_content" {:type "string"
                                              :description "The proposed new content for the file"}
                               "description" {:type "string"
                                              :description "Description of what this change does and why"}
                               "drone_id" {:type "string"
                                           :description "ID of the drone proposing this change"}
                               "directory" {:type "string"
                                            :description "Working directory for path validation. Pass your cwd to ensure paths are validated against YOUR project, not the MCP server's directory."}}
                  :required ["file_path" "old_content" "new_content"]}
    :handler handle-propose-diff}

   {:name "list_proposed_diffs"
    :description "List all pending diff proposals awaiting review. Use this to see what changes drones have proposed. Returns unified diff format for easy review."
    :inputSchema {:type "object"
                  :properties {"drone_id" {:type "string"
                                           :description "Optional: filter by drone ID"}}
                  :required []}
    :handler handle-list-proposed-diffs}

   {:name "apply_diff"
    :description "Apply a proposed diff to the file. Only call this after reviewing the diff. Will fail if file content has changed since the diff was proposed (safety check)."
    :inputSchema {:type "object"
                  :properties {"diff_id" {:type "string"
                                          :description "ID of the diff to apply"}}
                  :required ["diff_id"]}
    :handler handle-apply-diff}

   {:name "reject_diff"
    :description "Reject a proposed diff without applying. Use when the proposed change is incorrect, unnecessary, or conflicts with other changes."
    :inputSchema {:type "object"
                  :properties {"diff_id" {:type "string"
                                          :description "ID of the diff to reject"}
                               "reason" {:type "string"
                                         :description "Optional reason for rejection (helpful for drone learning)"}}
                  :required ["diff_id"]}
    :handler handle-reject-diff}

   {:name "get_diff_details"
    :description "Get full details of a specific diff including old and new content. Use when you need to see the complete content, not just the unified diff."
    :inputSchema {:type "object"
                  :properties {"diff_id" {:type "string"
                                          :description "ID of the diff to inspect"}}
                  :required ["diff_id"]}
    :handler handle-get-diff-details}

   {:name "review_wave_diffs"
    :description "Review all diffs proposed by a wave. Returns summary with auto-approve analysis. Use this after dispatch_validated_wave to see what changes drones proposed."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID to review diffs for"}}
                  :required ["wave_id"]}
    :handler handle-review-wave-diffs}

   {:name "approve_wave_diffs"
    :description "Approve and apply all or selected diffs from a wave. Call after reviewing with review_wave_diffs."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID to approve diffs for"}
                               "diff_ids" {:type "array"
                                           :items {:type "string"}
                                           :description "Optional: specific diff IDs to approve (omit for all)"}}
                  :required ["wave_id"]}
    :handler handle-approve-wave-diffs}

   {:name "reject_wave_diffs"
    :description "Reject all diffs from a wave without applying. Use when the wave produced incorrect changes."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID to reject diffs for"}
                               "reason" {:type "string"
                                         :description "Reason for rejection (helpful for learning)"}}
                  :required ["wave_id"]}
    :handler handle-reject-wave-diffs}

   {:name "auto_approve_wave_diffs"
    :description "Auto-approve diffs meeting criteria, flag others for manual review. Uses configurable rules (max lines, no deletions-only, etc.)."
    :inputSchema {:type "object"
                  :properties {"wave_id" {:type "string"
                                          :description "Wave ID to process"}}
                  :required ["wave_id"]}
    :handler handle-auto-approve-wave-diffs}

   {:name "batch_review_diffs"
    :description "Get all pending diffs from multiple drones for batch review. Returns diffs sorted by timestamp with auto-approve analysis."
    :inputSchema {:type "object"
                  :properties {"drone_ids" {:type "array"
                                            :items {:type "string"}
                                            :description "Optional: drone IDs to filter (omit for all pending diffs)"}}
                  :required []}
    :handler batch/handle-batch-review-diffs}

   {:name "approve_safe_diffs"
    :description "Auto-approve diffs from multiple drones that meet safety criteria. Diffs not meeting criteria are flagged for manual review."
    :inputSchema {:type "object"
                  :properties {"drone_ids" {:type "array"
                                            :items {:type "string"}
                                            :description "Optional: drone IDs to filter (omit for all pending diffs)"}
                               "dry_run" {:type "boolean"
                                          :description "If true, only report what would be approved without actually applying"}}
                  :required []}
    :handler batch/handle-approve-safe-diffs}

   {:name "get_auto_approve_rules"
    :description "Get current auto-approve rules configuration. Shows max-lines-changed, no-deletions-only, require-description, and allowed-path-patterns."
    :inputSchema {:type "object"
                  :properties {}
                  :required []}
    :handler handlers/handle-get-auto-approve-rules}])
