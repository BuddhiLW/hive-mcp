(ns hive-mcp.events.interceptors
  "Interceptor primitives - re-exports from core for backwards compatibility.
   
   DEPRECATED: Import from hive-mcp.events.core directly.
   This namespace exists only for backwards compatibility.
   
   All interceptor functionality is canonical in hive-mcp.events.core.
   This facade ensures existing code that imports from this namespace
   continues to work without modification.
   
   SOLID: Single Responsibility - facade for backwards compatibility only
   CLARITY: L - Layers stay pure (no duplicate logic, just re-exports)"
  (:require [hive-mcp.events.core :as core]))

;; =============================================================================
;; Re-exports from core (backwards compatibility)
;; =============================================================================

(def interceptor?
  "Returns true if m is a valid interceptor map.
   
   DEPRECATED: Use hive-mcp.events.core/interceptor? instead."
  core/interceptor?)

(def ->interceptor
  "Create an interceptor from keyword arguments.
   
   DEPRECATED: Use hive-mcp.events.core/->interceptor instead."
  core/->interceptor)

(def get-coeffect
  "Get a coeffect value from context.
   
   DEPRECATED: Use hive-mcp.events.core/get-coeffect instead."
  core/get-coeffect)

(def assoc-coeffect
  "Associate a coeffect value into context.
   
   DEPRECATED: Use hive-mcp.events.core/assoc-coeffect instead."
  core/assoc-coeffect)

(def get-effect
  "Get an effect value from context.
   
   DEPRECATED: Use hive-mcp.events.core/get-effect instead."
  core/get-effect)

(def assoc-effect
  "Associate an effect value into context.
   
   DEPRECATED: Use hive-mcp.events.core/assoc-effect instead."
  core/assoc-effect)

(def debug
  "Debug interceptor that logs event handling.
   
   DEPRECATED: Use hive-mcp.events.core/debug instead."
  core/debug)
