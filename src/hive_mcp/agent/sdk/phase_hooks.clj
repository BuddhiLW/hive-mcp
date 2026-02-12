(ns hive-mcp.agent.sdk.phase-hooks
  "SDK phase hooks for SAA lifecycle gating and observation capture."
  (:require [hive-mcp.agent.sdk.python :as py]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- ensure-python-path!
  "One-shot sys.path setup for hive Python modules."
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
  "Import hive_tools.phase_hooks module."
  [cwd]
  (ensure-python-path! cwd)
  (py/py-import "hive_tools.phase_hooks"))

(defn- hive-hooks-module
  "Import hive_hooks module for auto-observation."
  [cwd]
  (ensure-python-path! cwd)
  (py/py-import "hive_hooks"))

(defn capture-exploration-hook
  "Create a PostToolUse hook that captures Read/Grep/Glob results to memory."
  [cwd agent-id]
  (let [mod (phase-hooks-module cwd)]
    (py/py-call-kw (py/py-attr mod "create_capture_exploration_hook") []
                   {:cwd (or cwd "") :agent_id (or agent-id "")})))

(defn store-plan-hook
  "Create a PostToolUse hook that detects and stores plans to memory."
  [cwd agent-id]
  (let [mod (phase-hooks-module cwd)]
    (py/py-call-kw (py/py-attr mod "create_store_plan_hook") []
                   {:cwd (or cwd "") :agent_id (or agent-id "")})))

(defn inject-context-hook
  "Create a UserPromptSubmit hook that injects compressed context from prior phase."
  [cwd compressed-context]
  (let [mod (phase-hooks-module cwd)]
    (py/py-call-kw (py/py-attr mod "create_inject_context_hook") []
                   {:compressed_context (or compressed-context "")})))

(defn pre-compact-save-hook
  "Create a PreCompact hook that saves observations before context compaction."
  [cwd agent-id]
  (let [mod (phase-hooks-module cwd)]
    (py/py-call-kw (py/py-attr mod "create_pre_compact_save_hook") []
                   {:cwd (or cwd "") :agent_id (or agent-id "")})))

(defn auto-observation-hook
  "Create a PostToolUse hook for zero-cost auto-observation via nREPL."
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
  "Create PreToolUse SAA gating hooks that enforce phase boundaries."
  [phase-config]
  (let [mod (saa-hooks-module)
        make-config-fn (py/py-attr mod "make_saa_hooks_config")
        allowed-tools (vec (:allowed-tools phase-config))
        phase-name (name (:name phase-config))]
    (py/py-call-kw make-config-fn [allowed-tools]
                   {:phase_name phase-name :timeout 30})))

(defn hook-matcher
  "Wrap hook callback functions in a HookMatcher for SDK registration."
  [hook-fns]
  (let [matcher-class (py/py-attr (sdk-module) "HookMatcher")]
    (py/py-call-kw matcher-class [] {:hooks (vec hook-fns)})))

(defn agent-definition
  "Create a Python AgentDefinition from a Clojure agent spec (currently no-op)."
  [_agent-spec]
  (log/debug "[phase-hooks] AgentDefinition no longer supported in claude_code_sdk, ignoring")
  nil)

(defn agents-dict
  "Build a map of AgentDefinition objects from Clojure agent specs."
  [agents]
  (when (seq agents)
    (reduce-kv
     (fn [m agent-name agent-spec]
       (assoc m agent-name (agent-definition agent-spec)))
     {} agents)))

(defn agent-options
  "Create a ClaudeCodeOptions Python object from a Clojure options map."
  [opts-map]
  (let [opts-class (py/py-attr (sdk-module) "ClaudeCodeOptions")
        clean-opts (dissoc opts-map :agents)]
    (py/py-call-kw opts-class [] clean-opts)))

(defn- merge-hooks-dicts
  "Merge two hooks dictionaries by concatenating matcher lists per event type."
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
  "Build PostToolUse/UserPromptSubmit/PreCompact hooks for a SAA phase."
  [phase {:keys [cwd agent-id compressed-context]}]
  (try
    (let [post-tool-fns
          (cond-> []
            (#{:silence :act} phase)
            (conj (capture-exploration-hook cwd agent-id))
            (= :abstract phase)
            (conj (store-plan-hook cwd agent-id)))

          hooks (cond-> {}
                  (seq post-tool-fns)
                  (assoc "PostToolUse" [(hook-matcher post-tool-fns)])

                  (and (#{:abstract :act} phase) compressed-context)
                  (assoc "UserPromptSubmit" [(hook-matcher [(inject-context-hook cwd compressed-context)])])

                  true
                  (assoc "PreCompact" [(hook-matcher [(pre-compact-save-hook cwd agent-id)])]))]
      hooks)
    (catch Exception e
      (log/warn "[phase-hooks] Failed to build phase capture hooks, proceeding without"
                {:phase phase :error (ex-message e)})
      nil)))

(defn- build-auto-observation-hooks
  "Build PostToolUse auto-observation hooks."
  [cwd]
  (try
    {"PostToolUse" [(hook-matcher [(auto-observation-hook cwd "")])]}
    (catch Exception e
      (log/warn "[phase-hooks] Failed to build auto-observation hooks, proceeding without"
                {:error (ex-message e)})
      nil)))

(defn- build-saa-gating-hooks
  "Build SAA gating hooks with error handling."
  [phase-config]
  (try
    (saa-gating-hooks phase-config)
    (catch Exception e
      (log/warn "[phase-hooks] Failed to build SAA hooks, proceeding without"
                {:phase (:name phase-config) :error (ex-message e)})
      nil)))

(defn hooks-for-phase
  "Build merged hooks dict for a given SAA phase."
  [phase opts phase-config]
  (let [saa-hooks (build-saa-gating-hooks phase-config)
        auto-obs  (build-auto-observation-hooks (:cwd opts))
        capture   (build-phase-capture-hooks phase opts)]
    (-> saa-hooks
        (merge-hooks-dicts auto-obs)
        (merge-hooks-dicts capture))))

(defn hooks-for-base
  "Build hooks dict for base/persistent session with auto-observation only."
  [cwd]
  (build-auto-observation-hooks cwd))
