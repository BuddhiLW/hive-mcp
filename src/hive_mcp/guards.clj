(ns hive-mcp.guards
  "Guard functions to protect production state from test interference.
   
   CLARITY-Y: Yield safe failure - tests should not corrupt production.
   
   This module provides centralized coordinator detection that other modules
   can check before executing destructive operations like reset-all! or
   reset-conn!. When the MCP server is running in production, these guards
   prevent test fixtures from corrupting the live state.
   
   Usage:
     (guards/mark-coordinator-running!)   ; Called at server startup
     (guards/coordinator-running?)        ; Check before destructive ops
     (guards/when-not-coordinator         ; Macro wrapper
       \"Warning message\"
       (dangerous-operation!))"
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private coordinator-mode-atom (atom false))

;; =============================================================================
;; Public API
;; =============================================================================

(defn coordinator-running?
  "Check if the MCP coordinator is currently running.
   
   Returns true if mark-coordinator-running! has been called and
   clear-coordinator-running! has not been called since."
  []
  @coordinator-mode-atom)

(defn mark-coordinator-running!
  "Mark that the MCP coordinator is running.
   
   Called from server.clj during startup. Once marked, guard functions
   will protect production state from test interference.
   
   Idempotent - safe to call multiple times."
  []
  (reset! coordinator-mode-atom true)
  (log/info "Coordinator mode enabled - production state protected"))

(defn clear-coordinator-running!
  "Clear the coordinator running flag.
   
   Called during graceful shutdown or for testing purposes.
   
   Idempotent - safe to call multiple times."
  []
  (reset! coordinator-mode-atom false)
  (log/debug "Coordinator mode cleared"))

(defmacro when-not-coordinator
  "Execute body only if coordinator is not running.
   
   Returns nil and logs warning if coordinator is active.
   Returns the result of body evaluation if coordinator is not running.
   
   Example:
     (when-not-coordinator
       \"reset-all! blocked\"
       (reset! *state* initial-value))
   
   CLARITY-Y: Yield safe failure - graceful degradation when coordinator active."
  [warning-msg & body]
  `(if (coordinator-running?)
     (do
       (log/warn ~warning-msg " - skipped (coordinator running)")
       nil)
     (do ~@body)))
