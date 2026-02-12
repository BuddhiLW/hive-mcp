(ns hive-mcp.server.registration
  "Tool registration specs and discovery filtering.

   Bounded context: Tool/resource registration and discovery.

   Handles:
   - Hivemind message validation specs
   - Phase 2 strangle: tools/list override to hide deprecated tools"
  (:require [jsonrpc4clj.server :as jsonrpc-server]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Hivemind Message Specs (validation for server lifecycle events)
;; =============================================================================

(s/def ::hivemind-message
  (s/keys :req-un [::agent-id ::event-type ::message]))

(s/def ::agent-id string?)
(s/def ::event-type keyword?)
(s/def ::message string?)

;; =============================================================================
;; PHASE 2 STRANGLE: Override tools/list to hide deprecated tools
;;
;; Override the SDK's tools/list handler to filter out deprecated tools.
;; Deprecated tools remain callable (tools/call works) but are hidden from
;; discovery (tools/list excludes them).
;;
;; This enables graceful strangling of deprecated tools:
;; - New code uses consolidated tools (visible in tools/list)
;; - Old code continues to work (deprecated tools still callable)
;; - After sunset date, deprecated tools can be fully removed
;; =============================================================================

(defmethod jsonrpc-server/receive-request "tools/list"
  [_ context _params]
  (log/trace "tools/list request - filtering deprecated tools")
  (let [all-tools (vals @(:tools context))
        visible-tools (remove #(:deprecated (:tool %)) all-tools)
        deprecated-count (- (count all-tools) (count visible-tools))]
    (when (pos? deprecated-count)
      (log/debug "Hiding" deprecated-count "deprecated tools from tools/list"))
    {:tools (mapv :tool visible-tools)}))
