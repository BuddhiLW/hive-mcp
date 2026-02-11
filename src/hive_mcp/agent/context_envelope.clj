(ns hive-mcp.agent.context-envelope
  "Context preparation for agent spawn and dispatch. Delegates to extension layer when available."
  (:require [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const max-context-chars
  "Hard cap for context output (~2.5K tokens)."
  10000)

(defn- try-ext
  "Resolve and call an extension function. Returns nil on failure."
  [sym & args]
  (try
    (when-let [f (requiring-resolve sym)]
      (apply f args))
    (catch Exception e
      (log/debug "Extension not available:" sym (.getMessage e))
      nil)))

(defn- cap-output
  "Enforce hard character cap on context output."
  [s]
  (when (and s (string? s) (pos? (count s)))
    (if (> (count s) max-context-chars)
      (subs s 0 max-context-chars)
      s)))

(defn enrich-context
  "Prepare enriched context for agent dispatch via extension layer."
  ([ctx-refs kg-node-ids scope]
   (enrich-context ctx-refs kg-node-ids scope {}))
  ([ctx-refs kg-node-ids scope opts]
   (cap-output
    (try-ext 'hive-mcp.extensions.context/enrich
             ctx-refs kg-node-ids scope opts))))

(defn prepare-spawn-context
  "Prepare context for agent spawn via extension layer."
  ([directory]
   (prepare-spawn-context directory {}))
  ([directory opts]
   (cap-output
    (try-ext 'hive-mcp.extensions.context/prepare-spawn
             directory opts))))

(defn from-dispatch-context
  "Extract context from an IDispatchContext instance."
  ([dispatch-context]
   (from-dispatch-context dispatch-context {}))
  ([dispatch-context opts]
   (when (and dispatch-context
              (= :ref (dispatch-ctx/context-type dispatch-context)))
     (let [{:keys [ctx-refs kg-node-ids scope]} dispatch-context]
       (enrich-context ctx-refs kg-node-ids scope opts)))))

;; Backwards-compatible aliases
(def build-l2-envelope enrich-context)
(def build-spawn-envelope prepare-spawn-context)
(def envelope-from-dispatch-context from-dispatch-context)
