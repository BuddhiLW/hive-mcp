(ns hive-mcp.memory.types
  "MemoryType ADT — closed sum type for memory entry classification."
  (:require [hive-dsl.adt :refer [defadt]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defadt MemoryType
  "Memory entry type — closed sum type for classification."
  :axiom :principle :decision :convention :snippet :note :plan
  :doc :todo :question :answer :warning :error
  :pattern :lesson :rule :guideline :workflow :recipe)

(defn from-string [s]
  (when (string? s) (->memory-type (keyword s))))

(defn valid? [x]
  (boolean (cond (keyword? x) (->memory-type x) (string? x) (from-string x) :else nil)))

(defn variant->keyword [mt] (:adt/variant mt))
(defn variant->string [mt] (some-> (:adt/variant mt) name))

(def core-types #{:axiom :principle :decision :convention :snippet :note :plan})
(def intent-types #{:axiom :principle :decision :plan})
(def pattern-types #{:convention :pattern :lesson :rule :guideline :workflow :recipe})
(def semantic-types #{:snippet :note :doc :todo :question :answer :warning :error})
(def scope-piercing-types #{:axiom})
(def high-abstraction-routing-types #{:plan})
(def promotion-worthy-types #{:axiom :decision})
(def all-type-keywords (:variants MemoryType))
(def all-type-strings (set (map name all-type-keywords)))
