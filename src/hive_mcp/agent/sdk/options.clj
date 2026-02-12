(ns hive-mcp.agent.sdk.options
  "ClaudeAgentOptions construction for SDK sessions."
  (:require [hive-mcp.agent.sdk.phase-hooks :as phase-hooks]
            [hive-mcp.agent.sdk.saa :as saa]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn build-options-obj
  "Build a ClaudeAgentOptions Python object with SAA phase gating."
  [phase {:keys [cwd system-prompt session-id mcp-servers agents env] :as opts}]
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
       mcp-servers  (assoc :mcp_servers mcp-servers)
       agents-dict  (assoc :agents agents-dict)
       merged-hooks (assoc :hooks merged-hooks)
       (seq env)    (assoc :env env)))))

(defn build-base-options-obj
  "Build base ClaudeAgentOptions for a persistent client session."
  [{:keys [cwd system-prompt session-id mcp-servers agents env]}]
  (let [act-config    (get saa/saa-phases :act)
        agents-dict   (phase-hooks/agents-dict agents)
        auto-obs-hooks (phase-hooks/hooks-for-base cwd)]
    (phase-hooks/agent-options
     (cond-> {:allowed_tools  (:allowed-tools act-config)
              :permission_mode (:permission-mode act-config)
              :system_prompt  (or system-prompt "")}
       cwd            (assoc :cwd cwd)
       session-id     (assoc :resume session-id)
       mcp-servers    (assoc :mcp_servers mcp-servers)
       agents-dict    (assoc :agents agents-dict)
       auto-obs-hooks (assoc :hooks auto-obs-hooks)
       (seq env)      (assoc :env env)))))
