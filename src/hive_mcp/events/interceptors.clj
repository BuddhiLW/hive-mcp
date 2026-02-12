(ns hive-mcp.events.interceptors
  "Interceptor facade - re-exports debug interceptor from core.
   
   DEPRECATED: Import from hive-mcp.events.core directly.
   This namespace exists only for backwards compatibility.
   
   Only `debug` remains as a re-export; all other interceptor
   primitives (interceptor?, ->interceptor, get-coeffect,
   assoc-coeffect, get-effect, assoc-effect) were removed as
   dead code — no callers existed outside this facade."
  (:require [hive-mcp.events.core :as core]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Re-exports from core (backwards compatibility)
;; =============================================================================

(def debug
  "Debug interceptor that logs event handling.
   
   DEPRECATED: Use hive-mcp.events.core/debug instead."
  core/debug)
