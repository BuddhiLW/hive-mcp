(ns hive-mcp.agent.ling.headless-addon-strategy
  "ILingStrategy adapter for IHeadlessBackend instances.

   This is the Layer 2 bridge: hive-mcp core interacts with
   addon-contributed headless backends exclusively through ILingStrategy.
   It never calls IHeadlessBackend methods directly.

   spawn!/dispatch! let exceptions propagate (critical path).
   status/kill!/interrupt! use rescue with safe fallbacks (non-critical)."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.addons.headless :as headless]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord HeadlessAddonStrategy [headless-backend]
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (log/info "Headless addon strategy-spawn!" {:headless-id (headless/headless-id headless-backend)
                                                :ling-id (:id ling-ctx)})
    (let [result (headless/headless-spawn! headless-backend ling-ctx opts)]
      (log/info "Headless addon spawn succeeded" {:ling-id (:id ling-ctx)
                                                  :slave-id result})
      result))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (log/info "Headless addon strategy-dispatch!" {:headless-id (headless/headless-id headless-backend)
                                                   :ling-id (:id ling-ctx)})
    (headless/headless-dispatch! headless-backend ling-ctx task-opts))

  (strategy-status [_ ling-ctx ds-status]
    (let [addon-status (rescue nil
                               (headless/headless-status headless-backend ling-ctx ds-status))]
      (or addon-status
          (when ds-status
            (assoc ds-status :headless-addon-error? true)))))

  (strategy-kill! [_ ling-ctx]
    (log/info "Headless addon strategy-kill!" {:headless-id (headless/headless-id headless-backend)
                                               :ling-id (:id ling-ctx)})
    (let [result (rescue {:killed? false :id (:id ling-ctx) :reason :addon-exception}
                         (headless/headless-kill! headless-backend ling-ctx))]
      (log/info "Headless addon kill result" {:ling-id (:id ling-ctx) :result result})
      result))

  (strategy-interrupt! [_ ling-ctx]
    (rescue {:success? false
             :ling-id (:id ling-ctx)
             :errors ["Headless addon interrupt threw exception"]}
            (headless/headless-interrupt! headless-backend ling-ctx))))

(defn ->headless-addon-strategy
  "Create a HeadlessAddonStrategy wrapping an IHeadlessBackend instance.

   The backend must satisfy the IHeadlessBackend protocol.
   Returns an ILingStrategy-compatible record that delegates all
   operations to the backend."
  [headless-backend]
  {:pre [(satisfies? headless/IHeadlessBackend headless-backend)]}
  (->HeadlessAddonStrategy headless-backend))
