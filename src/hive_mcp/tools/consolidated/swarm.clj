(ns hive-mcp.tools.consolidated.swarm
  "Consolidated swarm coordination tool — merges agent, wave, hivemind, agora, olympus.

   Uses nested command namespacing to avoid collisions:
     swarm agent spawn
     swarm wave dispatch
     swarm hivemind shout
     swarm agora dialogue
     swarm olympus focus

   Addons can extend via contribute-commands! \"swarm\"."
  (:require [hive-mcp.tools.composite :as composite]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Canonical Handlers — nested by subdomain
;; =============================================================================

(def canonical-handlers
  "Nested handler tree. Dispatch via 'agent spawn', 'wave dispatch', etc.
   Subdomain handler trees resolved lazily via composite/lazy-resolve-handlers —
   drops the static c-agent/c-wave/c-hivemind/c-agora/c-olympus :require
   coupling (DIP). Same nested handler-tree shape; same dispatch behaviour."
  {:agent    (composite/lazy-resolve-handlers 'hive-mcp.tools.consolidated.agent/handlers)
   :wave     (composite/lazy-resolve-handlers 'hive-mcp.tools.consolidated.wave/handlers)
   :hivemind (composite/lazy-resolve-handlers 'hive-mcp.tools.consolidated.hivemind/handlers)
   :agora    (composite/lazy-resolve-handlers 'hive-mcp.tools.consolidated.agora/handlers)
   :olympus  (composite/lazy-resolve-handlers 'hive-mcp.tools.consolidated.olympus/handlers)
   ;; Folded standalone root (visibility-gated) re-exposed as an ergonomic
   ;; subdomain: `swarm preset list`. preset/handlers is a flat leaf map, so
   ;; lazy-resolve merges it directly — no prefix-strip adapter needed.
   :preset   (composite/lazy-resolve-handlers 'hive-mcp.tools.consolidated.preset/handlers)})

(def handlers canonical-handlers)

;; =============================================================================
;; Tool Definition
;; =============================================================================

;; Collect all params from sub-tools for schema union
(def tool-def
  ;; Every subdomain's advertised params fold into the root through the one
  ;; shared resolver (composite/lazy-resolve-schema-props) — the same seam the
  ;; memory root uses — so a param a subdomain declares survives the MCP
  ;; boundary instead of being dropped and silently defaulted.
  (let [all-props (apply merge-with merge
                         (map composite/lazy-resolve-schema-props
                              '[hive-mcp.tools.consolidated.agent/tools
                                hive-mcp.tools.consolidated.wave/tools
                                hive-mcp.tools.consolidated.hivemind/tools
                                hive-mcp.tools.consolidated.agora/tools
                                hive-mcp.tools.consolidated.olympus/tools
                                hive-mcp.tools.consolidated.preset/tools]))]
    {:name "swarm"
     :consolidated true
     :description "Unified agent operations: spawn (create ling/drone), status (query agents), kill (terminate), kill-batch (terminate multiple agents in one call), batch-spawn (spawn multiple agents at once via operations array), dispatch (send task), interrupt (interrupt current query of agent-sdk ling), claims (file ownership), list (deprecated alias for status), collect (get task result), broadcast (prompt all), cleanup (remove orphan agents after Emacs restart). Type: 'ling' (Claude Code instance) or 'drone' (OpenRouter leaf worker). Nested: dag (start/stop/status DAGWave scheduler). Use command='help' to list all."
     :inputSchema {:type "object"
                   :properties (merge
                                {"command" {:type "string"
                                            :description "Swarm operation. Prefix with subdomain: 'agent spawn', 'wave dispatch', 'hivemind shout', 'agora dialogue', 'olympus focus', 'preset list'. Use command='help' to list all."}}
                                ;; Include all params from sub-tools
                                (dissoc all-props "command"))
                   :required ["command"]}
     :handler (composite/build-merged-handler "swarm" canonical-handlers)}))

(def tools [tool-def])
