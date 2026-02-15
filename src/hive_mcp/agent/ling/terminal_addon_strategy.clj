(ns hive-mcp.agent.ling.terminal-addon-strategy
  "ILingStrategy adapter for ITerminalAddon instances.

   This is the Layer 2 bridge: hive-mcp core interacts with
   addon-contributed terminals exclusively through ILingStrategy.
   It never calls ITerminalAddon methods directly.

   spawn!/dispatch! let exceptions propagate (critical path).
   status/kill!/interrupt! use rescue with safe fallbacks (non-critical)."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.addons.terminal :as terminal]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord TerminalAddonStrategy [terminal-addon]
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (log/info "Terminal addon strategy-spawn!" {:terminal-id (terminal/terminal-id terminal-addon)
                                                :ling-id (:id ling-ctx)})
    (let [result (terminal/terminal-spawn! terminal-addon ling-ctx opts)]
      (log/info "Terminal addon spawn succeeded" {:ling-id (:id ling-ctx)
                                                  :slave-id result})
      result))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (log/info "Terminal addon strategy-dispatch!" {:terminal-id (terminal/terminal-id terminal-addon)
                                                   :ling-id (:id ling-ctx)})
    (terminal/terminal-dispatch! terminal-addon ling-ctx task-opts))

  (strategy-status [_ ling-ctx ds-status]
    (let [addon-status (rescue nil
                         (terminal/terminal-status terminal-addon ling-ctx ds-status))]
      (or addon-status
          (when ds-status
            (assoc ds-status :terminal-addon-error? true)))))

  (strategy-kill! [_ ling-ctx]
    (log/info "Terminal addon strategy-kill!" {:terminal-id (terminal/terminal-id terminal-addon)
                                               :ling-id (:id ling-ctx)})
    (let [result (rescue {:killed? false :id (:id ling-ctx) :reason :addon-exception}
                   (terminal/terminal-kill! terminal-addon ling-ctx))]
      (log/info "Terminal addon kill result" {:ling-id (:id ling-ctx) :result result})
      result))

  (strategy-interrupt! [_ ling-ctx]
    (rescue {:success? false
             :ling-id (:id ling-ctx)
             :errors ["Terminal addon interrupt threw exception"]}
      (terminal/terminal-interrupt! terminal-addon ling-ctx))))

(defn ->terminal-addon-strategy
  "Create a TerminalAddonStrategy wrapping an ITerminalAddon instance.

   The addon must satisfy the ITerminalAddon protocol.
   Returns an ILingStrategy-compatible record that delegates all
   operations to the addon."
  [terminal-addon]
  {:pre [(satisfies? terminal/ITerminalAddon terminal-addon)]}
  (->TerminalAddonStrategy terminal-addon))
