(ns hive-mcp.introspect.basic
  "BasicIntrospect - No-op fallback implementation of IIntrospect.

   Provides stub responses when no real introspection engine is available.
   Used as the default in hive-mcp (open source) without hive-logic.

   CLARITY-Y: Yield safe failure - graceful degradation when
   introspection capabilities are not available.

   SOLID-L: Liskov Substitution - this implementation can be used
   anywhere IIntrospect is expected, returning 'not available' responses."
  (:require [hive-mcp.introspect.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord BasicIntrospect []
  proto/IIntrospect

  (explain [_this fact]
    {:fact fact
     :derivation []
     :confidence 0.0
     :grounded false
     :status :not-available
     :message "Introspection not available. Install hive-logic for full provenance tracking."})

  (trace [_this query]
    {:query query
     :steps []
     :result nil
     :stats {}
     :status :not-available
     :message "Query tracing not available. Install hive-logic for execution tracing."})

  (diff [_this v1 v2]
    {:v1 v1
     :v2 v2
     :added []
     :removed []
     :changed []
     :stats {:added-count 0 :removed-count 0 :changed-count 0}
     :status :not-available
     :message "Knowledge diff not available. Install hive-logic for version comparison."}))

;; =============================================================================
;; Constructor
;; =============================================================================

(defn create-basic-introspect
  "Create a BasicIntrospect instance (no-op fallback).

   Use this when:
   - hive-logic is not installed
   - Testing without introspection dependencies
   - Graceful degradation in production

   Example:
     (def introspector (create-basic-introspect))
     (proto/set-introspector! introspector)"
  []
  (->BasicIntrospect))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn introspection-available?
  "Check if real introspection is available.
   Returns false for BasicIntrospect (no-op fallback)."
  [introspector]
  (not (instance? BasicIntrospect introspector)))

(defn ensure-introspector!
  "Ensure an introspector is set. Uses BasicIntrospect if none configured.

   Call this during system startup to guarantee introspection calls
   won't fail, even if they return 'not available' responses."
  []
  (when-not (proto/introspector-set?)
    (proto/set-introspector! (create-basic-introspect))))
