(ns hive-mcp.knowledge-graph.similarity
  "GS module — node comparison based on relationship patterns.

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
;; Empty Signature (noop fallback)
;; =============================================================================

(def ^:private empty-signature
  {:outgoing-types {}
   :incoming-types {}
   :outgoing-neighbors #{}
   :incoming-neighbors #{}
   :degree 0})

(def ^:private empty-similarity
  {:score 0.0
   :details {:edge-type 0.0
             :neighbor 0.0
             :degree 0.0}})

;; =============================================================================
;; Public API — delegates to extension or returns noop
;; =============================================================================

(defn relation-signature
  "Extract the relation signature of a node.
   Delegates to extension if available."
  [node-id]
  (delegate-or-noop :gs/sig empty-signature [node-id]))

(defn neighbor-overlap
  "Compute neighbor overlap between two nodes using Jaccard index.
   Delegates to extension if available."
  [sig-a sig-b]
  (delegate-or-noop :gs/overlap
                    {:outgoing 0.0 :incoming 0.0 :combined 0.0}
                    [sig-a sig-b]))

(defn edge-type-similarity
  "Compute similarity based on edge type distributions.
   Delegates to extension if available."
  [sig-a sig-b]
  (delegate-or-noop :gs/edge-cmp
                    {:outgoing 0.0 :incoming 0.0 :combined 0.0}
                    [sig-a sig-b]))

(defn entry-cmp
  "Compare two nodes via extension delegate.
   Delegates to extension if available."
  [sig-a sig-b & [opts]]
  (delegate-or-noop :gs/struct-cmp empty-similarity
                    (if opts [sig-a sig-b opts] [sig-a sig-b])))

(defn build-signature-index
  "Pre-compute relation signatures for all nodes in scope.
   Delegates to extension if available."
  [& [opts]]
  (delegate-or-noop :gs/build-idx {} (if opts [opts] [])))

(defn find-gs-related
  "Find entries related to a given entry via extension delegate.
   Delegates to extension if available."
  [node-id & [opts]]
  (delegate-or-noop :gs/find-similar []
                    (if opts [node-id opts] [node-id])))

(defn pairwise-similarity
  "Compute pairwise node comparison score.
   Delegates to extension if available."
  [node-a node-b & [opts]]
  (delegate-or-noop :gs/pairwise empty-similarity
                    (if opts [node-a node-b opts] [node-a node-b])))

(defn find-node-roles
  "Group nodes by their graph role.
   Delegates to extension if available."
  [& [opts]]
  (delegate-or-noop :gs/find-roles [] (if opts [opts] [])))

(defn similarity-report
  "Generate a similarity report for a node.
   Delegates to extension if available."
  [node-id & [opts]]
  (delegate-or-noop :gs/report
                    {:node-id node-id :signature empty-signature :similar [] :roles []}
                    (if opts [node-id opts] [node-id])))
