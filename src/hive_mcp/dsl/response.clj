(ns hive-mcp.dsl.response
  "Response processing for token efficiency.

   Delegates to extension if available. Returns noop defaults otherwise.

   Extension points are resolved via the extensions registry at startup.
   When no extensions are registered, all functions gracefully degrade
   to pass-through (input returned unchanged)."
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
;; Auto-Detection for Ling Callers
;; =============================================================================

(defn ling-caller?
  "Check if the caller is a ling/swarm agent based on _caller_id prefix.
   Lings have _caller_id starting with 'swarm-' or 'ling-'.
   Returns truthy for ling callers, nil otherwise."
  [caller-id]
  (when (string? caller-id)
    (or (.startsWith ^String caller-id "swarm-")
        (.startsWith ^String caller-id "ling-")
        nil)))

;; =============================================================================
;; Param Resolution
;; =============================================================================

(defn resolve-compress-mode
  "Resolve the compression mode from MCP tool args.

   Priority (highest first):
   1. compact: false          → nil (explicit opt-out, overrides auto-detect)
   2. compact: true           → :compact
   3. compact: \"compact\"    → :compact
   4. compact: \"minimal\"    → :minimal
   5. Auto-detect: _caller_id starts with 'swarm-' or 'ling-' → :compact
   6. Otherwise              → nil (no compression)

   Auto-detection enables token-efficient responses for headless lings
   without requiring explicit compact param on every tool call.
   Coordinator ('coordinator' or nil _caller_id) keeps full responses.
   Lings can override with compact: false to get full responses."
  [args]
  (let [v (:compact args)]
    (cond
      (false? v)            nil
      (true? v)             :compact
      (string? v)           (let [kw (keyword v)]
                              (#{:compact :minimal :full} kw))
      (keyword? v)          (#{:compact :minimal :full} v)
      (nil? v)              (when (ling-caller? (:_caller_id args))
                              :compact)
      :else                 nil)))

;; =============================================================================
;; Public API — delegates to extension or returns noop (pass-through)
;; =============================================================================

(defn omit-defaults
  "Process response map via extension delegate.
   Delegates to extension if available."
  [m]
  (delegate-or-noop :dr/omit m [m]))

(defn strip-verbose
  "Process response map via extension delegate.
   Delegates to extension if available."
  [m]
  (delegate-or-noop :dr/strip m [m]))

(defn abbreviate-keys
  "Process response map via extension delegate.
   Delegates to extension if available."
  [m]
  (delegate-or-noop :dr/abbrev m [m]))

(defn minimal-entry
  "Reduce an entry-like map via extension delegate.
   Delegates to extension if available."
  [m]
  (delegate-or-noop :dr/entry m [m]))

(defn compact-response
  "Apply compact processing via extension delegate.
   Delegates to extension if available."
  [data]
  (delegate-or-noop :dr/compact data [data]))

(defn minimal-response
  "Apply minimal processing via extension delegate.
   Delegates to extension if available."
  [data]
  (delegate-or-noop :dr/minimal data [data]))

(defn compress
  "Apply response processing by mode.
   Delegates to extension if available."
  [data mode]
  (delegate-or-noop :dr/compress data [data mode]))

(defn compress-mcp-text
  "Process an MCP content item via extension delegate.
   Delegates to extension if available."
  [item mode]
  (delegate-or-noop :dr/text item [item mode]))

(defn compress-content
  "Process MCP content vector via extension delegate.
   Delegates to extension if available."
  [content mode]
  (delegate-or-noop :dr/content content [content mode]))

;; =============================================================================
;; Batch Envelope Processing — delegates to extension or returns noop
;; =============================================================================

(defn compress-batch-op
  "Process a single batch operation result via extension delegate.
   Delegates to extension if available."
  [op]
  (delegate-or-noop :dr/batch-op op [op]))

(defn compress-batch-envelope
  "Process a batch execution response envelope via extension delegate.
   Delegates to extension if available."
  [envelope]
  (delegate-or-noop :dr/batch-env envelope [envelope]))
