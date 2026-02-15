(ns hive-mcp.memory.types
  "MemoryType ADT — closed sum type for memory entry classification.

   Defines the canonical set of memory types used across hive-mcp.
   Wraps the existing type-registry SST with compile-time type safety
   via `defadt` from hive-dsl.

   ## Relationship to type-registry
   `hive-mcp.memory.type-registry` remains the SST for metadata (abstraction
   levels, duration defaults, catchup configs). This namespace provides:
   - Closed variant set (defadt — no new types without modifying this ns)
   - Type-safe dispatch via `adt-case` (compile-time exhaustiveness)
   - Coercion functions for string/keyword boundaries

   ## Coercion at Boundaries
   Chroma stores types as strings (\"axiom\"). MCP params arrive as strings.
   Internal Clojure code uses keywords. The ADT bridges all three:

     (from-string \"axiom\")     => {:adt/type :MemoryType :adt/variant :axiom}
     (->memory-type :axiom)     => {:adt/type :MemoryType :adt/variant :axiom}
     (variant->string mt)       => \"axiom\"
     (variant->keyword mt)      => :axiom"
  (:require [hive-dsl.adt :refer [defadt]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defadt MemoryType
  "Memory entry type — closed sum type for classification.
   Core types (7): visible in MCP tool enums.
   Extended types (12): Malli-valid, stored in Chroma, not first-class in MCP."
  :axiom :principle :decision :convention :snippet :note :plan
  :doc :todo :question :answer :warning :error
  :pattern :lesson :rule :guideline :workflow :recipe)

;; =============================================================================
;; String/Keyword Coercion (system boundary helpers)
;; =============================================================================

(defn from-string
  "Coerce a string to a MemoryType ADT value. Returns nil if invalid."
  [s]
  (when (string? s)
    (->memory-type (keyword s))))

(defn valid?
  "Check if a keyword or string represents a valid MemoryType."
  [x]
  (boolean
   (cond
     (keyword? x) (->memory-type x)
     (string? x)  (from-string x)
     :else        nil)))

(defn variant->keyword
  "Extract the variant keyword from a MemoryType ADT value."
  [mt]
  (:adt/variant mt))

(defn variant->string
  "Extract the variant name as a string from a MemoryType ADT value."
  [mt]
  (some-> (:adt/variant mt) name))

;; =============================================================================
;; Type Subsets (for common dispatch patterns)
;; =============================================================================

(def core-types #{:axiom :principle :decision :convention :snippet :note :plan})
(def intent-types #{:axiom :principle :decision :plan})
(def pattern-types #{:convention :pattern :lesson :rule :guideline :workflow :recipe})
(def semantic-types #{:snippet :note :doc :todo :question :answer :warning :error})
(def scope-piercing-types #{:axiom})
(def high-abstraction-routing-types #{:plan})
(def promotion-worthy-types #{:axiom :decision})

(def all-type-keywords (:variants MemoryType))
(def all-type-strings (set (map name all-type-keywords)))
