(ns hive-mcp.agent.headless-capability-property-test
  "Property-based tests for HeadlessCapability ADT.

   Tests algebraic properties:
   - Totality: coercion never throws for valid inputs
   - Round-trip: keyword -> ADT -> keyword = identity
   - Serialization round-trip: ADT -> serialize -> deserialize preserves variant/type
   - Exhaustiveness: adt-case covers all 9 variants
   - Invalid keywords return nil from from-keyword
   - ADT type consistency: adt-type is always :HeadlessCapability
   - adt-valid? for all variants: always true
   - Subset properties: basic <= sdk <= all

   Convention: 200 iterations per property (per hive-mcp testing convention)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [hive-dsl.adt :as adt :refer [adt-case]]
            [hive-mcp.agent.headless-capability :as hc]))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-capability-keyword
  "Generator for valid HeadlessCapability variant keywords."
  (gen/elements (vec hc/all-capabilities)))

(def gen-capability-string
  "Generator for valid HeadlessCapability strings."
  (gen/fmap #(str (namespace %) "/" (name %)) gen-capability-keyword))

(def gen-capability
  "Generator for HeadlessCapability ADT values."
  (gen/fmap hc/headless-capability gen-capability-keyword))

(def gen-invalid-keyword
  "Generator for keywords that are NOT valid capabilities."
  (gen/such-that #(not (contains? hc/all-capabilities %))
                 (gen/elements [:bogus :cap/bogus :invalid :cap/invalid
                                :hooks :interrupts :streaming :multi
                                :cap/fly :cap/teleport :cap/magic])
                 100))

;; =============================================================================
;; Property: Totality - all valid keywords coerce to HeadlessCapability
;; =============================================================================

(defspec all-keywords-coerce-to-capability 200
  (prop/for-all [kw gen-capability-keyword]
                (let [cap (hc/from-keyword kw)]
                  (and (some? cap)
                       (hc/headless-capability? cap)))))

;; =============================================================================
;; Property: Round-trip - keyword -> ADT -> keyword = identity
;; =============================================================================

(defspec keyword-round-trip 200
  (prop/for-all [kw gen-capability-keyword]
                (= kw (hc/to-keyword (hc/headless-capability kw)))))

(defspec from-to-keyword-round-trip 200
  (prop/for-all [kw gen-capability-keyword]
                (= kw (hc/to-keyword (hc/from-keyword kw)))))

;; =============================================================================
;; Property: Serialization round-trip - ADT -> serialize -> deserialize
;; =============================================================================

(defspec serialize-deserialize-round-trip 200
  (prop/for-all [cap gen-capability]
                (let [serialized (adt/serialize cap)
                      deserialized (adt/deserialize serialized)]
                  (and (some? deserialized)
                       (= (:adt/variant cap) (:adt/variant deserialized))
                       (= (:adt/type cap) (:adt/type deserialized))))))

;; =============================================================================
;; Property: Exhaustiveness - adt-case covers all 9 variants
;; =============================================================================

(defspec exhaustive-dispatch-returns-value 200
  (prop/for-all [cap gen-capability]
                (let [result (adt-case hc/HeadlessCapability cap
                                       :cap/hooks         :hookable
                                       :cap/interrupts    :interruptable
                                       :cap/subagents     :hosts-subagents
                                       :cap/checkpointing :checkpointable
                                       :cap/mcp-tools     :has-mcp
                                       :cap/streaming     :streams
                                       :cap/multi-turn    :multi-turn
                                       :cap/budget-guard  :budgeted
                                       :cap/saa           :saa-enabled)]
                  (contains? #{:hookable :interruptable :hosts-subagents
                               :checkpointable :has-mcp :streams
                               :multi-turn :budgeted :saa-enabled}
                             result))))

;; =============================================================================
;; Property: Invalid keywords return nil from from-keyword
;; =============================================================================

(defspec invalid-keywords-return-nil 200
  (prop/for-all [kw gen-invalid-keyword]
                (nil? (hc/from-keyword kw))))

;; =============================================================================
;; Property: ADT type consistency - adt-type is always :HeadlessCapability
;; =============================================================================

(defspec adt-type-is-always-headless-capability 200
  (prop/for-all [cap gen-capability]
                (= :HeadlessCapability (adt/adt-type cap))))

;; =============================================================================
;; Property: adt-valid? for all variants - always true
;; =============================================================================

(defspec adt-valid-for-all-variants 200
  (prop/for-all [cap gen-capability]
                (adt/adt-valid? cap)))

;; =============================================================================
;; Property: Subset properties - basic <= sdk <= all
;; =============================================================================

(defspec basic-capabilities-subset-of-sdk 200
  (prop/for-all [kw (gen/elements (vec hc/basic-capabilities))]
                (contains? hc/sdk-capabilities kw)))

(defspec sdk-capabilities-subset-of-all 200
  (prop/for-all [kw (gen/elements (vec hc/sdk-capabilities))]
                (contains? hc/all-capabilities kw)))

;; =============================================================================
;; Deterministic unit tests (edge cases)
;; =============================================================================

(deftest exactly-nine-variants
  (testing "HeadlessCapability has exactly 9 variants"
    (is (= 9 (count hc/all-capabilities)))))

(deftest variant-keywords-correct
  (testing "Variant set matches expected"
    (is (= #{:cap/hooks :cap/interrupts :cap/subagents :cap/checkpointing
             :cap/mcp-tools :cap/streaming :cap/multi-turn :cap/budget-guard :cap/saa}
           hc/all-capabilities))))

(deftest capability-to-protocol-maps-exactly-four
  (testing "capability->protocol maps exactly 4 capabilities"
    (is (= 4 (count hc/capability->protocol)))
    (is (= #{:cap/hooks :cap/checkpointing :cap/subagents :cap/budget-guard}
           (set (keys hc/capability->protocol))))))

(deftest sdk-capabilities-has-nine-entries
  (testing "sdk-capabilities has 9 entries"
    (is (= 9 (count hc/sdk-capabilities)))))

(deftest basic-capabilities-has-two-entries
  (testing "basic-capabilities has 2 entries"
    (is (= 2 (count hc/basic-capabilities)))
    (is (= #{:cap/streaming :cap/multi-turn} hc/basic-capabilities))))
