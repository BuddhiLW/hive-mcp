(ns hive-mcp.agent.sdk.options
  "ClaudeAgentOptions construction for SDK sessions.

   Thin orchestrator — delegates all Python interop to phase-hooks.
   Builds options objects by composing domain primitives:
   - Agent definitions via phase-hooks/agents-dict
   - Hook routing via phase-hooks/hooks-for-phase
   - Options construction via phase-hooks/agent-options"
  (:require [hive-mcp.agent.sdk.phase-hooks :as phase-hooks]
            [hive-mcp.agent.sdk.saa :as saa]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn build-options-obj
  "Build a ClaudeAgentOptions Python object with SAA phase gating.

   Creates the options object with:
   - SAA gating hooks (PreToolUse) for phase enforcement
   - Auto-observation hooks (PostToolUse) for zero-cost observability
   - Phase-specific capture hooks (PostToolUse/UserPromptSubmit/PreCompact)

   Arguments:
     phase - SAA phase keyword (:silence :abstract :act)
     opts  - Map with :cwd :system-prompt :session-id :mcp-servers :agents
             :agent-id :compressed-context"
  [phase {:keys [cwd system-prompt session-id _mcp-servers agents] :as opts}]
  (let [phase-config  (get saa/saa-phases phase)
        full-prompt   (str (or system-prompt "")
                           "\n\n"
                           (:system-prompt-suffix phase-config))
        agents-dict   (phase-hooks/agents-dict agents)
        merged-hooks  (phase-hooks/hooks-for-phase phase opts phase-config)]
    (phase-hooks/agent-options
     (cond-> {:allowed_tools  (:allowed-tools phase-config)
              :permission_mode (:permission-mode phase-config)
              :system_prompt  full-prompt}
       cwd          (assoc :cwd cwd)
       session-id   (assoc :resume session-id)
       agents-dict  (assoc :agents agents-dict)
       merged-hooks (assoc :hooks merged-hooks)))))

(defn build-base-options-obj
  "Build base ClaudeAgentOptions for a persistent client session (P3-T2).

   Uses Act-phase permissions (most permissive) since SAA phase gating
   is handled via prompt instructions in multi-turn mode.

   Arguments:
     opts - Map with :cwd :system-prompt :session-id :agents

   Returns a Python ClaudeAgentOptions object."
  [{:keys [cwd system-prompt session-id agents]}]
  (let [act-config    (get saa/saa-phases :act)
        agents-dict   (phase-hooks/agents-dict agents)
        auto-obs-hooks (phase-hooks/hooks-for-base cwd)]
    (phase-hooks/agent-options
     (cond-> {:allowed_tools  (:allowed-tools act-config)
              :permission_mode (:permission-mode act-config)
              :system_prompt  (or system-prompt "")}
       cwd            (assoc :cwd cwd)
       session-id     (assoc :resume session-id)
       agents-dict    (assoc :agents agents-dict)
       auto-obs-hooks (assoc :hooks auto-obs-hooks)))))
