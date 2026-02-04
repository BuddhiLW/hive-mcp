(ns hive-mcp.autopoiesis.basic
  "BasicAutopoiesis - No-op fallback implementation.
   
   This implementation is used when the proprietary hive-knowledge
   library is not available. It provides safe, neutral responses
   for all IAutopoiesis protocol methods.
   
   CLARITY-L: Layers stay pure - this is the default adapter.
   CLARITY-Y: Yield safe failure - graceful degradation when
   advanced autopoiesis is not configured.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(require '[hive-mcp.autopoiesis.protocol :as proto])

;; =============================================================================
;; BasicAutopoiesis Record
;; =============================================================================

(defrecord BasicAutopoiesis []
  proto/IAutopoiesis

  (observe [_this _event]
    ;; No-op: observations are not recorded without an autopoiesis engine
    {:observed false
     :reason "No autopoiesis engine configured. Install hive-knowledge for observation tracking."})

  (trust? [_this _entry]
    ;; Neutral trust: without an engine, we can't compute trust signals
    ;; Return 0.5 (neutral) to avoid biasing decisions
    0.5)

  (emerge [_this _scope]
    ;; No emergence detection: return empty pattern list
    ;; Callers should handle empty results gracefully
    [])

  (learn [_this _feedback]
    ;; No learning: feedback is acknowledged but not processed
    {:learned false
     :model-updates 0
     :new-patterns 0
     :reason "No autopoiesis engine configured. Install hive-knowledge for learning capabilities."})

  (decay [_this]
    ;; No decay: without an engine, we don't modify knowledge
    ;; Memory's built-in TTL still applies via duration categories
    {:decayed-count 0
     :promoted-count 0
     :expired-count 0
     :timestamp (java.time.Instant/now)}))

;; =============================================================================
;; Constructor
;; =============================================================================

(defn basic-autopoiesis
  "Create a new BasicAutopoiesis instance.
   
   This is a no-op implementation that provides safe defaults
   when the full autopoiesis engine is not available.
   
   Returns a BasicAutopoiesis record."
  []
  (->BasicAutopoiesis))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-basic!
  "Initialize the autopoiesis system with the basic no-op implementation.
   
   Call this during system startup if hive-knowledge is not available.
   This ensures the system can operate (with degraded capabilities)
   without the advanced autopoiesis features.
   
   Returns the BasicAutopoiesis instance."
  []
  (let [instance (basic-autopoiesis)]
    (proto/set-autopoiesis! instance)
    instance))

;; =============================================================================
;; Introspection
;; =============================================================================

(defn basic-autopoiesis?
  "Check if the given autopoiesis instance is a BasicAutopoiesis.
   
   Useful for conditional logic based on capabilities:
   
   (if (basic-autopoiesis? (proto/get-autopoiesis))
     (log/warn \"Running with degraded autopoiesis\")
     (perform-advanced-emergence))"
  [instance]
  (instance? BasicAutopoiesis instance))

(defn capabilities
  "Return the capabilities of BasicAutopoiesis.
   
   Returns a map describing what this implementation can do:
   - :observe - false (no observation recording)
   - :trust   - :neutral (always returns 0.5)
   - :emerge  - false (no pattern detection)
   - :learn   - false (no model updates)
   - :decay   - false (no staleness management)"
  []
  {:observe false
   :trust :neutral
   :emerge false
   :learn false
   :decay false
   :implementation "BasicAutopoiesis"
   :version "1.0.0"})
