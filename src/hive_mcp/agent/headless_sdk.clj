(ns hive-mcp.agent.headless-sdk
  "Facade re-exporting agent/sdk/ module public API for backward compatibility."
  (:require [hive-mcp.agent.sdk.availability :as avail]
            [hive-mcp.agent.sdk.lifecycle :as lifecycle]
            [hive-mcp.agent.sdk.saa :as saa]
            [hive-mcp.agent.sdk.session :as session]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def sdk-status avail/sdk-status)
(def available? avail/available?)
(def reset-availability! avail/reset-availability!)

(def saa-phases saa/saa-phases)
(def score-observations saa/score-observations)
(def with-silence-tracking saa/with-silence-tracking)

(def get-session session/get-session)

(def spawn-headless-sdk! lifecycle/spawn-headless-sdk!)
(def dispatch-headless-sdk! lifecycle/dispatch-headless-sdk!)
(def kill-headless-sdk! lifecycle/kill-headless-sdk!)
(def interrupt-headless-sdk! lifecycle/interrupt-headless-sdk!)
(def sdk-status-for lifecycle/sdk-status-for)
(def list-sdk-sessions lifecycle/list-sdk-sessions)
(def sdk-session? lifecycle/sdk-session?)
(def kill-all-sdk! lifecycle/kill-all-sdk!)

