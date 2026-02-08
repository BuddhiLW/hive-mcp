(ns hive-mcp.agent.sdk.options
  "ClaudeAgentOptions construction for SDK sessions.

   Builds Python ClaudeAgentOptions objects using proper libpython-clj interop:
   - AgentDefinition construction for subagents
   - SAA gating hooks (PreToolUse) for phase enforcement
   - Auto-observation hooks (PostToolUse) for zero-cost observability
   - Hook merging for combining multiple hook sources
   - Base and phase-specific options building"
  (:require [hive-mcp.agent.sdk.python :as py]
            [hive-mcp.agent.sdk.saa :as saa]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn build-agents-dict
  "Build a Python dict of AgentDefinition objects from Clojure agent specs.

   Converts Clojure agent definitions map into Python AgentDefinition instances
   suitable for ClaudeAgentOptions.agents parameter.

   Each agent spec is a map with:
     :description - Agent description string (required)
     :prompt      - Agent system prompt string (required)
     :tools       - List of tool name strings (optional)
     :model       - Model name: 'sonnet', 'opus', 'haiku', 'inherit' (optional)

   Arguments:
     agents - Map of agent-name -> agent-spec

   Returns:
     Python dict of {name: AgentDefinition(...)} or nil if agents is empty."
  [agents]
  (when (seq agents)
    (let [sdk-mod (py/py-import "claude_code_sdk")
          agent-def-class (py/py-attr sdk-mod "AgentDefinition")]
      (reduce-kv
       (fn [m agent-name agent-spec]
         (let [{:keys [description prompt tools model]} agent-spec
               kw-args (cond-> {:description (or description "")
                                :prompt (or prompt "")}
                         tools (assoc :tools (vec tools))
                         model (assoc :model model))]
           (assoc m agent-name (py/py-call-kw agent-def-class [] kw-args))))
       {} agents))))

(defn build-saa-hooks
  "Build SAA gating hooks for a phase using the Python saa_hooks module.

   Creates a PreToolUse hook configuration that enforces SAA phase boundaries
   by checking tool names against the phase's allowed-tools list.

   Arguments:
     phase-config - SAA phase config map with :allowed-tools and :name

   Returns:
     Python dict suitable for ClaudeAgentOptions(hooks=...), or nil on error."
  [phase-config]
  (try
    (let [saa-hooks-mod (py/py-import "claude_agent_sdk.saa_hooks")
          make-config-fn (py/py-attr saa-hooks-mod "make_saa_hooks_config")
          allowed-tools (vec (:allowed-tools phase-config))
          phase-name (name (:name phase-config))]
      (py/py-call-kw make-config-fn [allowed-tools]
                     {:phase_name phase-name
                      :timeout 30}))
    (catch Exception e
      (log/warn "[sdk.options] Failed to build SAA hooks, proceeding without"
                {:phase (:name phase-config) :error (ex-message e)})
      nil)))

(defn build-auto-observation-hooks
  "Build PostToolUse auto-observation hooks via the hive_hooks Python module.

   Creates the auto-observation hook that captures significant tool executions
   and stores them to hive-mcp memory via nREPL. Zero LLM cost.

   Arguments:
     cwd - Working directory for project scoping

   Returns:
     Python dict {'PostToolUse': [HookMatcher(hooks=[auto_obs_hook])]} or nil on error."
  [cwd]
  (try
    (py/py-run (str "import sys\n"
                    "sys.path.insert(0, '" (or cwd ".") "/python')\n"
                    "sys.path.insert(0, '" (or cwd ".") "/../hive-mcp/python')\n"))
    (let [hive-hooks-mod (py/py-import "hive_hooks")
          create-hook-fn (py/py-attr hive-hooks-mod "create_auto_observation_hook")
          config-class (py/py-attr hive-hooks-mod "AutoObservationConfig")
          config-obj (py/py-call-kw config-class []
                                    {:nrepl_host "localhost"
                                     :nrepl_port 7910
                                     :project_dir (or cwd "")
                                     :agent_id ""
                                     :batch_interval_s 2.0})
          hook-fn (py/py-call-kw create-hook-fn [] {:config config-obj})
          sdk-mod (py/py-import "claude_code_sdk")
          matcher-class (py/py-attr sdk-mod "HookMatcher")
          matcher-obj (py/py-call-kw matcher-class []
                                     {:hooks [hook-fn]})]
      {"PostToolUse" [matcher-obj]})
    (catch Exception e
      (log/warn "[sdk.options] Failed to build auto-observation hooks, proceeding without"
                {:error (ex-message e)})
      nil)))

(defn merge-hooks-dicts
  "Merge two Python hooks dictionaries.

   Each dict maps HookEvent -> [HookMatcher]. This function merges by
   concatenating matcher lists for each event type.

   Arguments:
     dict-a - First hooks dict (e.g., SAA PreToolUse hooks)
     dict-b - Second hooks dict (e.g., PostToolUse auto-observation hooks)

   Returns:
     Merged dict, or whichever is non-nil, or nil if both nil."
  [dict-a dict-b]
  (cond
    (and (nil? dict-a) (nil? dict-b)) nil
    (nil? dict-a) dict-b
    (nil? dict-b) dict-a
    :else
    (try
      (let [merged (merge-with (fn [a b]
                                 (if (and (sequential? a) (sequential? b))
                                   (vec (concat a b))
                                   (vec (concat (if (sequential? a) a [a])
                                                (if (sequential? b) b [b])))))
                               (py/py->clj dict-a)
                               (py/py->clj dict-b))]
        merged)
      (catch Exception e
        (log/warn "[sdk.options] Failed to merge hooks dicts, using dict-a"
                  {:error (ex-message e)})
        dict-a))))

(defn build-options-obj
  "Build a ClaudeAgentOptions Python object with SAA phase gating.

   Creates the options object as a real Python dataclass instance via call-kw.
   Includes SAA gating hooks via PreToolUse that enforce phase boundaries.

   Arguments:
     phase - SAA phase keyword (:silence :abstract :act)
     opts  - Map with :cwd :system-prompt :session-id :mcp-servers :agents"
  [phase {:keys [cwd system-prompt session-id _mcp-servers agents]}]
  (let [phase-config (get saa/saa-phases phase)
        full-prompt (str (or system-prompt "")
                         "\n\n"
                         (:system-prompt-suffix phase-config))
        sdk-mod (py/py-import "claude_code_sdk")
        opts-class (py/py-attr sdk-mod "ClaudeAgentOptions")
        agents-dict (build-agents-dict agents)
        saa-hooks (build-saa-hooks phase-config)
        auto-obs-hooks (build-auto-observation-hooks cwd)
        merged-hooks (merge-hooks-dicts saa-hooks auto-obs-hooks)]
    (py/py-call-kw opts-class []
                   (cond-> {:allowed_tools (:allowed-tools phase-config)
                            :permission_mode (:permission-mode phase-config)
                            :system_prompt full-prompt}
                     cwd           (assoc :cwd cwd)
                     session-id    (assoc :resume session-id)
                     agents-dict   (assoc :agents agents-dict)
                     merged-hooks  (assoc :hooks merged-hooks)))))

(defn build-base-options-obj
  "Build base ClaudeAgentOptions for a persistent client session (P3-T2).

   Uses Act-phase permissions (most permissive) since SAA phase gating
   is handled via prompt instructions in multi-turn mode.

   Arguments:
     opts - Map with :cwd :system-prompt :session-id :agents

   Returns a Python ClaudeAgentOptions object."
  [{:keys [cwd system-prompt session-id agents]}]
  (let [act-config (get saa/saa-phases :act)
        sdk-mod (py/py-import "claude_code_sdk")
        opts-class (py/py-attr sdk-mod "ClaudeAgentOptions")
        agents-dict (build-agents-dict agents)
        auto-obs-hooks (build-auto-observation-hooks cwd)]
    (py/py-call-kw opts-class []
                   (cond-> {:allowed_tools (:allowed-tools act-config)
                            :permission_mode (:permission-mode act-config)
                            :system_prompt (or system-prompt "")}
                     cwd            (assoc :cwd cwd)
                     session-id     (assoc :resume session-id)
                     agents-dict    (assoc :agents agents-dict)
                     auto-obs-hooks (assoc :hooks auto-obs-hooks)))))
