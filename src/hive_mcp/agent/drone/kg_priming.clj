(ns hive-mcp.agent.drone.kg-priming
  "Domain context priming for drone task augmentation.

   Resolves seed topics into relevant domain knowledge and injects
   it into drone prompts. Delegates to extension if available.
   Noop fallback: returns empty string (no priming).

   CLARITY-Y: Auto-initializes KG store if needed. Never throws
   on missing store — graceful degradation to noop."
  (:require [hive-mcp.extensions.registry :as ext]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.protocols.kg :as kg-proto]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; KG Store Auto-Initialization
;; =============================================================================

(defn- ensure-kg-store!
  "Ensure KG store is initialized. Auto-initializes via connection if needed.
   CLARITY-Y: Returns true if store available, false on failure (never throws)."
  []
  (try
    (if (kg-proto/store-set?)
      true
      (do
        (log/info "KG store not initialized, auto-initializing for priming")
        (kg-conn/get-conn)
        (kg-proto/store-set?)))
    (catch Exception e
      (log/debug "KG store auto-initialization failed (non-fatal)"
                 {:error (.getMessage e)})
      false)))

(defn kg-store-available?
  "Check if the KG store is currently initialized (without auto-init).
   Lightweight check for callers that want to know store status."
  []
  (kg-proto/store-set?))

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value.
   Guards against missing KG store with auto-initialization.
   CLARITY-Y: Never throws — returns default-val on any failure."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (if (ensure-kg-store!)
      (try
        (apply f args)
        (catch Exception e
          (log/debug "Extension delegation failed, returning noop"
                     {:ext-key ext-key :error (.getMessage e)})
          default-val))
      (do
        (log/debug "KG store unavailable after auto-init attempt, noop for" ext-key)
        default-val))
    (do
      (log/debug "Extension not available, noop for" ext-key)
      default-val)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn prime-context
  "Returns a markdown block with domain knowledge relevant to the task, or empty string if none found."
  [{:keys [seeds] :as opts}]
  (if (seq seeds)
    (delegate-or-noop :pm/prime "" [opts])
    ""))

(defn resolve-seeds
  "Resolve seed topic strings into concrete references.
   Delegates to extension if available. Returns empty vector otherwise.

   Arguments (map):
     :tags       - Vector of topic tag strings
     :ids        - Explicit reference IDs to pass through
     :task       - Task string for relevance-based resolution
     :project-id - Project ID for scoping

   Returns:
     Vector of reference ID strings, or [] if noop."
  [opts]
  (delegate-or-noop :pm/seeds [] [opts]))

(defn priming-available?
  "Check if the priming extension is available and KG store is accessible.
   Returns true only when both extension AND store are ready."
  []
  (and (ext/extension-available? :pm/prime)
       (kg-store-available?)))
