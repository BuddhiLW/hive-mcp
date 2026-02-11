(ns hive-mcp.agent.ling.strategy
  "ILingStrategy protocol for mode-specific spawn/dispatch/status/kill operations.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol ILingStrategy
  "Strategy protocol for mode-specific ling operations."

  (strategy-spawn! [this ling-ctx opts]
    "Spawn a ling using this strategy's mechanism.")

  (strategy-dispatch! [this ling-ctx task-opts]
    "Dispatch a task to a running ling.")

  (strategy-status [this ling-ctx ds-status]
    "Get mode-specific liveness and status information.")

  (strategy-kill! [this ling-ctx]
    "Terminate the ling using this strategy's mechanism.")

  (strategy-interrupt! [this ling-ctx]
    "Interrupt the current query/task of a running ling."))
