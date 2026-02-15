(ns hive-mcp.hivemind.event-type-property-test
  "Property-based tests for EventType ADT.

   Verifies algebraic properties that must hold for ALL valid inputs:

   - P1: keyword→EventType→keyword round-trip identity
   - P2: string→EventType→string round-trip identity
   - P3: All 6 keywords coerce without throwing (totality)
   - P4: Unknown keywords are rejected (exhaustiveness)
   - P5: All 6 variants are pairwise distinct
   - P6: Every variant has metadata in event registry (completeness)
   - P7: terminal? consistency with event registry
   - P8: slave-status consistency with event registry
   - P9: Serialize/deserialize round-trip via hive-dsl.adt
   - P10: event-type? predicate is true for all constructed values"
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hive-mcp.hivemind.event-type :as et]
            [hive-mcp.hivemind.event-registry :as reg]
            [hive-dsl.adt :as adt]))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-event-type-keyword
  "Generator for valid EventType variant keywords."
  (gen/elements [:started :progress :completed :error :blocked :wrap_notify]))

(def gen-event-type-string
  "Generator for valid EventType variant strings."
  (gen/elements ["started" "progress" "completed" "error" "blocked" "wrap_notify"]))

(def gen-event-type
  "Generator for valid EventType ADT values."
  (gen/fmap et/keyword->event-type gen-event-type-keyword))

(def gen-unknown-keyword
  "Generator for keywords that are NOT valid EventType variants."
  (gen/such-that #(not (contains? et/all-variants %))
                 gen/keyword
                 100))

(def gen-two-different-keywords
  "Generator for two distinct EventType keywords."
  (gen/such-that (fn [[a b]] (not= a b))
                 (gen/tuple gen-event-type-keyword gen-event-type-keyword)))

;; =============================================================================
;; P1: keyword→EventType→keyword round-trip identity
;; =============================================================================

(defspec p1-keyword-roundtrip 200
  (prop/for-all [kw gen-event-type-keyword]
                (= kw (et/event-type->keyword (et/keyword->event-type kw)))))

;; =============================================================================
;; P2: string→EventType→string round-trip identity
;; =============================================================================

(defspec p2-string-roundtrip 200
  (prop/for-all [s gen-event-type-string]
                (= s (et/event-type->string (et/string->event-type s)))))

;; =============================================================================
;; P3: All 6 keywords coerce without throwing (totality)
;; =============================================================================

(defspec p3-keyword-coercion-totality 200
  (prop/for-all [kw gen-event-type-keyword]
                (try
                  (et/keyword->event-type kw)
                  true
                  (catch Throwable _ false))))

;; =============================================================================
;; P4: Unknown keywords are rejected (exhaustiveness)
;; =============================================================================

(defspec p4-unknown-keywords-rejected 200
  (prop/for-all [kw gen-unknown-keyword]
                (try
                  (et/keyword->event-type kw)
                  false  ;; should have thrown
                  (catch clojure.lang.ExceptionInfo _ true)
                  (catch Throwable _ false))))

;; =============================================================================
;; P5: All 6 variants are pairwise distinct
;; =============================================================================

(defspec p5-variants-pairwise-distinct 100
  (prop/for-all [[kw1 kw2] gen-two-different-keywords]
                (not= (et/keyword->event-type kw1)
                      (et/keyword->event-type kw2))))

;; =============================================================================
;; P6: Every variant has metadata in event registry (completeness)
;; =============================================================================

(defspec p6-registry-completeness 100
  (prop/for-all [kw gen-event-type-keyword]
                (some? (get reg/registry kw))))

;; =============================================================================
;; P7: terminal? consistency with event registry
;; =============================================================================

(defspec p7-terminal-consistency 100
  (prop/for-all [kw gen-event-type-keyword]
                (= (et/terminal? (et/keyword->event-type kw))
                   (:terminal? (get reg/registry kw)))))

;; =============================================================================
;; P8: slave-status consistency with event registry
;; =============================================================================

(defspec p8-slave-status-consistency 100
  (prop/for-all [kw gen-event-type-keyword]
                (= (et/slave-status (et/keyword->event-type kw))
                   (:slave-status (get reg/registry kw)))))

;; =============================================================================
;; P9: Serialize/deserialize round-trip via hive-dsl.adt
;; =============================================================================

(defspec p9-serialize-deserialize-roundtrip 200
  (prop/for-all [kw gen-event-type-keyword]
                (let [et (et/keyword->event-type kw)
                      serialized (adt/serialize et)
                      deserialized (adt/deserialize serialized)]
                  (and (some? deserialized)
                       (= et deserialized)))))

;; =============================================================================
;; P10: event-type? predicate is true for all constructed values
;; =============================================================================

(defspec p10-predicate-true-for-constructed 200
  (prop/for-all [kw gen-event-type-keyword]
                (et/event-type? (et/keyword->event-type kw))))
