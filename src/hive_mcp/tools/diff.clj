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
  "REMOVED: Flat diff tools no longer exposed. Use consolidated `wave` tool with review/approve/reject commands."
  [])
