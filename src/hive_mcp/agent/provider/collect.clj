(ns hive-mcp.agent.provider.collect
  "Provider COLLECT stratum: the effectful leaves, one per thing read.

   Each collector reads the host config and returns plain data. No collector
   calls another, and none decides anything: deciding is the promote stratum
   (`hive-mcp.agent.provider.policy`).

   This is the ONLY provider namespace allowed to touch `hive-mcp.config.core`,
   which is why a test can redefine the config seam here and drive every
   provider decision from it. `hive-mcp.agent.provider.strata-test` gates that."
  (:require [hive-mcp.config.core :as global-config]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn config-providers
  "The host config's :llm-providers map, or nil when it holds no map."
  []
  (let [m (global-config/get-config-value "llm-providers")]
    (when (map? m) m)))

(defn config-priority
  "The host config's :llm-provider-priority, or nil when it is not a sequence."
  []
  (let [v (global-config/get-config-value "llm-provider-priority")]
    (when (sequential? v) v)))

(defn agent-type-defaults
  "The host config's :agent-defaults entry for `agent-type`, or nil."
  [agent-type]
  (get (global-config/get-config-value "agent-defaults") (keyword agent-type)))

(defn present-secret-keys
  "Of `secret-keys`, the set that actually resolves to a configured secret."
  [secret-keys]
  (into #{} (filter #(some? (global-config/get-secret %))) secret-keys))
