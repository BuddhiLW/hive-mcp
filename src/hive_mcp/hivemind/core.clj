(ns hive-mcp.hivemind.core
  "Hivemind coordination — thin facade over sub-modules.

   Re-exports the public API for backward compatibility. All consumers
   can continue using `[hive-mcp.hivemind :as hivemind]` unchanged.

   Sub-modules:
   - hivemind.state     — atoms and accessors (ling-results, swarm-prompts)
   - hivemind.messaging — shout!, ask!, respond-ask!, piggyback registration
   - hivemind.status    — get-status, agent lifecycle (register/clear)
   - hivemind.tools     — MCP tool definitions and register-tools!"
  (:require [hive-mcp.hivemind.state :as state]
            [hive-mcp.hivemind.messaging :as messaging]
            [hive-mcp.hivemind.status :as status]
            [hive-mcp.hivemind.tools :as hm-tools]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; State atoms (for test fixtures that resolve these vars)
(def pending-asks state/pending-asks)
(def agent-registry state/agent-registry)
(def pending-swarm-prompts state/pending-swarm-prompts)
(def ling-results state/ling-results)

;; State accessors
(def record-ling-result! state/record-ling-result!)
(def get-pending-ling-results state/get-pending-ling-results)
(def mark-ling-reviewed! state/mark-ling-reviewed!)
(def clear-ling-results! state/clear-ling-results!)
(def add-swarm-prompt! state/add-swarm-prompt!)
(def remove-swarm-prompt! state/remove-swarm-prompt!)
(def get-swarm-prompts state/get-swarm-prompts)
(def clear-agent-registry! state/clear-agent-registry!)

;; Messaging
(def shout! messaging/shout!)
(def ask! messaging/ask!)
(def respond-ask! messaging/respond-ask!)

;; Status & lifecycle
(def get-status status/get-status)
(def get-agent-messages status/get-agent-messages)
(def register-agent! status/register-agent!)
(def clear-agent! status/clear-agent!)

;; Tools
(def tools hm-tools/tools)
(def register-tools! hm-tools/register-tools!)
