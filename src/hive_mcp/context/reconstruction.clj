(ns hive-mcp.context.reconstruction
  "Compressed context reconstruction from refs and KG traversal.

   Delegates to extension if available. Returns noop defaults otherwise.

   Extension points are resolved via the extensions registry at startup.
   When no extensions are registered, all functions gracefully degrade
   to empty/noop results."
  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/debug "Extension not available, returning default for" ext-key)
      default-val)))

;; =============================================================================
;; Public API — delegates to extension or returns noop
;; =============================================================================

(defn fetch-ref-data
  "Resolve context entries by reference map.
   Delegates to extension if available."
  [ctx-refs]
  (delegate-or-noop :cr/e {} [ctx-refs]))

(defn gather-kg-context
  "Gather structural context from seed nodes.
   Delegates to extension if available."
  [kg-node-ids scope]
  (delegate-or-noop :cr/f nil [kg-node-ids scope]))

(defn compress-kg-subgraph
  "Render a subgraph as compact representation.
   Delegates to extension if available."
  [kg-context]
  (delegate-or-noop :cr/g nil [kg-context]))

(defn render-compressed-context
  "Combine ref summaries and subgraph into bounded output.
   Delegates to extension if available."
  [ref-data kg-context]
  (delegate-or-noop :cr/h "" [ref-data kg-context]))

(defn reconstruct-context
  "Top-level context reconstruction.
   Delegates to extension if available."
  [ctx-refs kg-node-ids scope]
  (delegate-or-noop :cr/i "" [ctx-refs kg-node-ids scope]))

(defn catchup-refs->ref-context-opts
  "Convert catchup output to context options.
   Delegates to extension if available."
  [catchup-response scope]
  (delegate-or-noop :cr/j
                    {:ctx-refs       {}
                     :kg-node-ids    []
                     :scope          scope
                     :reconstruct-fn reconstruct-context}
                    [catchup-response scope]))
