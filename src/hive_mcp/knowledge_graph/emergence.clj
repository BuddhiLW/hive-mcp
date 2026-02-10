(ns hive-mcp.knowledge-graph.emergence
  "Emergence Detection — discovers clusters of structurally similar
   entries and creates synthetic nodes representing emergent concepts.

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

(defn detect-emergent-clusters
  "Detect clusters of structurally similar entries that may represent
   emergent concepts.
   Delegates to extension if available."
  [& [opts]]
  (delegate-or-noop :ge/detect [] (if opts [opts] [])))

(defn create-synthetic-node!
  "Create a synthetic node from a detected emergent cluster.
   Delegates to extension if available."
  [cluster & [opts]]
  (delegate-or-noop :ge/create-node! nil
                    (if opts [cluster opts] [cluster])))

(defn detect-and-create!
  "End-to-end emergence detection and synthetic node creation.
   Delegates to extension if available."
  [& [opts]]
  (delegate-or-noop :ge/detect-create!
                    {:detected 0 :created 0 :skipped 0 :clusters [] :synth-ids []}
                    (if opts [opts] [])))

(defn emergence-report
  "Generate a human-readable emergence detection report.
   Delegates to extension if available."
  [& [opts]]
  (delegate-or-noop :ge/report
                    {:scope nil :total-nodes 0 :clusters-detected 0
                     :clusters [] :existing-synthetics 0}
                    (if opts [opts] [])))
