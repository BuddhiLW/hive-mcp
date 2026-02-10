(ns hive-mcp.agent.sdk.phase-hooks
  "DDD domain namespace for SDK phase hooks.

   Wraps all Python hook interop as idiomatic Clojure functions.
   Three SLAP layers:
     Layer 1 (Infrastructure): Python path setup, memoized module imports
     Layer 2 (Domain):         Hook factories returning Python hook callbacks
     Layer 3 (Composition):    Phase-aware hook routing and merging

   Domain language: hooks, matchers, phases — not py-import, py-attr, py-call-kw."
  (:require [hive-mcp.agent.sdk.python :as py]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Layer 1 — Infrastructure (private)
;;; =============================================================================

(defn- ensure-python-path!
  "One-shot sys.path setup for hive Python modules. Idempotent."
  [cwd]
  (py/py-run (str "import sys\n"
                  "sys.path.insert(0, '" (or cwd ".") "/python')\n"
                  "sys.path.insert(0, '" (or cwd ".") "/../hive-mcp/python')\n")))

(defn- sdk-module
  "Import claude_code_sdk module."
  []
  (py/py-import "claude_code_sdk"))

(defn- saa-hooks-module
  "Import claude_agent_sdk.saa_hooks module."
  []
  (py/py-import "claude_agent_sdk.saa_hooks"))

(defn- phase-hooks-module
  "Import hive_tools.phase_hooks module (requires python path setup)."
  [cwd]
  (ensure-python-path! cwd)
  (py/py-import "hive_tools.phase_hooks"))

(defn- hive-hooks-module
  "Import hive_hooks module for auto-observation (requires python path setup)."
  [cwd]
  (ensure-python-path! cwd)
  (py/py-import "hive_hooks"))

;;; =============================================================================
;;; Layer 2 — Domain Primitives (public)
;;; =============================================================================

;; --- Hook factories: each returns a Python hook callback object ---

(defn capture-exploration-hook
  "Create a PostToolUse hook that captures Read/Grep/Glob results to memory.
   Used in silence + act phases."
  [cwd agent-id]
  (let [mod (phase-hooks-module cwd)]
    (py/py-call-kw (py/py-attr mod "create_capture_exploration_hook") []
                   {:cwd (or cwd "") :agent_id (or agent-id "")})))

(defn store-plan-hook
  "Create a PostToolUse hook that detects and stores plans to memory.
   Used in abstract phase only."
  [cwd agent-id]
  (let [mod (phase-hooks-module cwd)]
    (py/py-call-kw (py/py-attr mod "create_store_plan_hook") []
                   {:cwd (or cwd "") :agent_id (or agent-id "")})))

(defn inject-context-hook
  "Create a UserPromptSubmit hook that injects compressed context from prior phase.
   Used in abstract + act phases when compressed-context is available."
  [cwd compressed-context]
  (let [mod (phase-hooks-module cwd)]
    (py/py-call-kw (py/py-attr mod "create_inject_context_hook") []
                   {:compressed_context (or compressed-context "")})))

(defn pre-compact-save-hook
  "Create a PreCompact hook that saves observations before context compaction.
   Used in all phases."
  [cwd agent-id]
  (let [mod (phase-hooks-module cwd)]
    (py/py-call-kw (py/py-attr mod "create_pre_compact_save_hook") []
                   {:cwd (or cwd "") :agent_id (or agent-id "")})))

(defn auto-observation-hook
  "Create a PostToolUse hook for zero-cost auto-observation via nREPL.
   Captures significant tool executions and stores to hive-mcp memory."
  [cwd agent-id]
  (let [mod (hive-hooks-module cwd)
        config-class (py/py-attr mod "AutoObservationConfig")
        config-obj (py/py-call-kw config-class []
                                  {:nrepl_host "localhost"
                                   :nrepl_port 7910
                                   :project_dir (or cwd "")
                                   :agent_id (or agent-id "")
                                   :batch_interval_s 2.0})]
    (py/py-call-kw (py/py-attr mod "create_auto_observation_hook") []
                   {:config config-obj})))

(defn saa-gating-hooks
  "Create PreToolUse SAA gating hooks that enforce phase boundaries.

   Arguments:
     phase-config - SAA phase config map with :allowed-tools and :name

   Returns:
     Python dict suitable for ClaudeAgentOptions(hooks=...), or nil on error."
  [phase-config]
  (let [mod (saa-hooks-module)
        make-config-fn (py/py-attr mod "make_saa_hooks_config")
        allowed-tools (vec (:allowed-tools phase-config))
        phase-name (name (:name phase-config))]
    (py/py-call-kw make-config-fn [allowed-tools]
                   {:phase_name phase-name :timeout 30})))

;; --- SDK wiring ---

(defn hook-matcher
  "Wrap hook callback functions in a HookMatcher for SDK registration.

   Arguments:
     hook-fns - Vector of Python hook callback objects

   Returns:
     HookMatcher instance."
  [hook-fns]
  (let [matcher-class (py/py-attr (sdk-module) "HookMatcher")]
    (py/py-call-kw matcher-class [] {:hooks (vec hook-fns)})))

;; --- Agent definitions ---

(defn agent-definition
  "Create a Python AgentDefinition from a Clojure agent spec.

   Arguments:
     agent-spec - Map with :description :prompt :tools :model

   Returns:
     Python AgentDefinition instance."
  [agent-spec]
  (let [{:keys [description prompt tools model]} agent-spec
        agent-def-class (py/py-attr (sdk-module) "AgentDefinition")
        kw-args (cond-> {:description (or description "")
                         :prompt (or prompt "")}
                  tools (assoc :tools (vec tools))
                  model (assoc :model model))]
    (py/py-call-kw agent-def-class [] kw-args)))

(defn agents-dict
  "Build a Python dict of AgentDefinition objects from Clojure agent specs.

   Arguments:
     agents - Map of agent-name -> agent-spec

   Returns:
     Map of {name: AgentDefinition(...)} or nil if agents is empty."
  [agents]
  (when (seq agents)
    (reduce-kv
     (fn [m agent-name agent-spec]
       (assoc m agent-name (agent-definition agent-spec)))
     {} agents)))

;; --- Options construction ---

(defn agent-options
  "Create a ClaudeAgentOptions Python object from a Clojure options map.

   Arguments:
     opts-map - Map of option keys (keyword or string) to values.
                Keys: :allowed_tools :permission_mode :system_prompt
                      :cwd :resume :agents :hooks

   Returns:
     Python ClaudeAgentOptions dataclass instance."
  [opts-map]
  (let [opts-class (py/py-attr (sdk-module) "ClaudeAgentOptions")]
    (py/py-call-kw opts-class [] opts-map)))

;;; =============================================================================
;;; Layer 3 — Composition (public)
;;; =============================================================================

(defn- merge-hooks-dicts
  "Merge two hooks dictionaries by concatenating matcher lists per event type.

   Each dict maps HookEvent string -> [HookMatcher].

   Returns:
     Merged dict, or whichever is non-nil, or nil if both nil."
  [dict-a dict-b]
  (cond
    (and (nil? dict-a) (nil? dict-b)) nil
    (nil? dict-a) dict-b
    (nil? dict-b) dict-a
    :else
    (merge-with (fn [a b]
                  (if (and (sequential? a) (sequential? b))
                    (vec (concat a b))
                    (vec (concat (if (sequential? a) a [a])
                                 (if (sequential? b) b [b])))))
                (py/py->clj dict-a)
                (py/py->clj dict-b))))

(defn- build-phase-capture-hooks
  "Build PostToolUse/UserPromptSubmit/PreCompact hooks for a SAA phase.

   Returns:
     Map {EventName: [HookMatcher]} or nil on error."
  [phase {:keys [cwd agent-id compressed-context]}]
  (try
    (let [;; PostToolUse hooks — phase-dependent
          post-tool-fns
          (cond-> []
            (#{:silence :act} phase)
            (conj (capture-exploration-hook cwd agent-id))
            (= :abstract phase)
            (conj (store-plan-hook cwd agent-id)))

          hooks (cond-> {}
                  (seq post-tool-fns)
                  (assoc "PostToolUse" [(hook-matcher post-tool-fns)])

                  ;; UserPromptSubmit: inject compressed context (abstract + act)
                  (and (#{:abstract :act} phase) compressed-context)
                  (assoc "UserPromptSubmit" [(hook-matcher [(inject-context-hook cwd compressed-context)])])

                  ;; PreCompact: save observations before compaction (all phases)
                  true
                  (assoc "PreCompact" [(hook-matcher [(pre-compact-save-hook cwd agent-id)])]))]
      hooks)
    (catch Exception e
      (log/warn "[phase-hooks] Failed to build phase capture hooks, proceeding without"
                {:phase phase :error (ex-message e)})
      nil)))

(defn- build-auto-observation-hooks
  "Build PostToolUse auto-observation hooks.

   Returns:
     Map {\"PostToolUse\": [HookMatcher]} or nil on error."
  [cwd]
  (try
    {"PostToolUse" [(hook-matcher [(auto-observation-hook cwd "")])]}
    (catch Exception e
      (log/warn "[phase-hooks] Failed to build auto-observation hooks, proceeding without"
                {:error (ex-message e)})
      nil)))

(defn- build-saa-gating-hooks
  "Build SAA gating hooks with error handling.

   Returns:
     Python hooks dict or nil on error."
  [phase-config]
  (try
    (saa-gating-hooks phase-config)
    (catch Exception e
      (log/warn "[phase-hooks] Failed to build SAA hooks, proceeding without"
                {:phase (:name phase-config) :error (ex-message e)})
      nil)))

(defn hooks-for-phase
  "Build merged hooks dict for a given SAA phase.

   Combines:
   1. SAA gating hooks (PreToolUse)
   2. Auto-observation hooks (PostToolUse)
   3. Phase-specific capture hooks (PostToolUse/UserPromptSubmit/PreCompact)

   Arguments:
     phase - SAA phase keyword (:silence :abstract :act)
     opts  - Map with :cwd :agent-id :compressed-context
     phase-config - SAA phase config map (from saa/saa-phases)

   Returns:
     Merged hooks dict {EventName → [HookMatcher]} or nil."
  [phase opts phase-config]
  (let [saa-hooks (build-saa-gating-hooks phase-config)
        auto-obs  (build-auto-observation-hooks (:cwd opts))
        capture   (build-phase-capture-hooks phase opts)]
    (-> saa-hooks
        (merge-hooks-dicts auto-obs)
        (merge-hooks-dicts capture))))

(defn hooks-for-base
  "Build hooks dict for base/persistent session (Act-phase, auto-obs only).

   Arguments:
     cwd - Working directory

   Returns:
     Hooks dict with auto-observation PostToolUse hooks, or nil."
  [cwd]
  (build-auto-observation-hooks cwd))
