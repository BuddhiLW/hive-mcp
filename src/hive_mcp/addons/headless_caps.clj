(ns hive-mcp.addons.headless-caps
  "Optional capability protocols for headless backends (ISP pattern).

   Small protocols that backends opt-in to via `satisfies?`. Core dispatch
   checks protocol satisfaction before calling -- unsatisfied protocols
   simply don't execute (NoOp by omission).

   Usage in hive-mcp core (e.g. forge-strike hook injection):
     (when (satisfies? IHookable backend)
       (headless-caps/register-hooks! backend ling-id {...}))

   Protocols:
   - IHookable         — Pre/post tool-use hook injection
   - ICheckpointable   — Session checkpoint/rewind
   - ISubagentHost     — Native subagent definitions
   - IBudgetGuardable  — Per-session cost budgeting

   See also:
   - hive-mcp.addons.headless  -- IHeadlessBackend (required protocol)
   - hive-mcp.agent.headless-capability -- HeadlessCapability ADT")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; IHookable — Pre/post tool-use hook injection
;; =============================================================================

(defprotocol IHookable
  "Optional protocol for backends that support hook injection.
   Enables SAA gating hooks, pre-tool-use validation, and post-tool-use logging."

  (register-hooks! [this ling-id hooks-map]
    "Register hooks for a ling session.
     hooks-map keys: :pre-tool-use, :post-tool-use, :on-error, :on-complete
     Each value is a function (fn [hook-context] ...).
     Returns: {:registered? bool :hook-count int}")

  (active-hooks [this ling-id]
    "Return the currently active hooks map for a ling session, or nil."))

;; =============================================================================
;; ICheckpointable — Session checkpoint/rewind
;; =============================================================================

(defprotocol ICheckpointable
  "Optional protocol for backends that support session checkpointing.
   Enables saving session state and rewinding to a previous checkpoint."

  (checkpoint! [this ling-id]
    "Create a checkpoint of the current session state.
     Returns: {:checkpoint-id str :created-at long}")

  (rewind! [this ling-id checkpoint-id]
    "Rewind session to a previous checkpoint.
     Returns: {:rewound? bool :checkpoint-id str}"))

;; =============================================================================
;; ISubagentHost — Native subagent definitions
;; =============================================================================

(defprotocol ISubagentHost
  "Optional protocol for backends that support native subagent definitions.
   Enables Claude Agent SDK-style nested agent hierarchies."

  (register-subagents! [this ling-id agent-defs]
    "Register subagent definitions for a ling session.
     agent-defs: map of agent-name -> {:description str :prompt str :tools [str] :model str}
     Returns: {:registered? bool :agent-count int}")

  (list-subagents [this ling-id]
    "Return the registered subagent definitions for a ling, or nil."))

;; =============================================================================
;; IBudgetGuardable — Per-session cost budgeting
;; =============================================================================

(defprotocol IBudgetGuardable
  "Optional protocol for backends that support per-session cost budgeting.
   Enables hard spending limits that interrupt sessions when exceeded."

  (set-budget! [this ling-id max-usd]
    "Set maximum USD budget for a ling session.
     Returns: {:budget-set? bool :max-usd number}")

  (budget-status [this ling-id]
    "Get current budget status for a ling session.
     Returns: {:max-usd number :spent-usd number :remaining-usd number :exceeded? bool}
     or nil if no budget set."))
