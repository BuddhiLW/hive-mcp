(ns hive-mcp.dns.result-spec-generative-test
  "Spec-based generative tests for the Result DSL.

   Exercises the fdef specs from result/spec.clj using:
   - clojure.spec.test.alpha/check for constructors and predicates
   - Manual property tests with spec-aware generators for combinators
     (bind/map-ok/map-err use fn? which lacks default generators)

   Complements result_property_test.clj (hand-written monad laws) and
   result_test.clj (example-based unit tests)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hive-mcp.dns.result :as r]
            [hive-mcp.dns.result.spec :as rspec]
            [hive-mcp.dns.result.taxonomy :as tax]))

;; =============================================================================
;; Custom Generators for Result Types
;;
;; The specs ::ok-result and ::err-result use (s/and map? ...) which cannot
;; generate valid samples (random maps rarely contain :ok or :error keys).
;; These generators produce valid Results directly.
;; =============================================================================

(def gen-ok-result
  "Generator for ok Results conforming to ::ok-result spec."
  (gen/fmap r/ok gen/any-printable))

(def gen-err-category
  "Generator for error category keywords (qualified)."
  (gen/elements [:io/timeout :sdk/invalid-request :kg/node-not-found
                 :chroma/connection-failed :parse/invalid-json
                 :effect/exception :transport/timeout :drone/spawn-failed
                 :emacs/not-connected :io/read-failure]))

(def gen-err-result
  "Generator for err Results conforming to ::err-result spec."
  (gen/let [cat gen-err-category
            extras (gen/map gen/keyword-ns gen/any-printable {:max-elements 3})]
    (-> extras
        (dissoc :ok)
        (assoc :error cat))))

(def gen-result
  "Generator for any Result (ok or err) conforming to ::result spec."
  (gen/one-of [gen-ok-result gen-err-result]))

;; --- Function Generators (for combinator tests) ---

(def gen-result-fn
  "Generator for functions (a -> Result), used for bind tests."
  (gen/elements [(fn [x] (r/ok x))
                 (fn [x] (r/ok (str x)))
                 (fn [_] (r/err :test/generated))
                 (fn [x] (r/ok [x]))
                 (fn [x] (r/ok {:wrapped x}))]))

(def gen-plain-fn
  "Generator for functions (a -> b), used for map-ok tests."
  (gen/elements [identity
                 str
                 (fn [x] [x])
                 (fn [x] {:v x})
                 (fn [_] 42)]))

(def gen-err-transform-fn
  "Generator for functions (ErrMap -> ErrMap), used for map-err tests."
  (gen/elements [identity
                 (fn [m] (assoc m :decorated true))
                 (fn [m] (assoc m :severity :high))
                 (fn [m] (dissoc m :message))]))

;; =============================================================================
;; Section 1: stest/check for Constructors and Predicates
;;
;; These functions have fully generatable arg specs (no fn? args).
;; stest/check exercises the full fdef contract: :args, :ret, and :fn.
;; =============================================================================

(defn- check-passed?
  "Run stest/check on a symbol and return true if all tests pass."
  [sym num-tests]
  (let [results (stest/check sym {:clojure.spec.test.check/opts
                                   {:num-tests num-tests}})
        result (first results)]
    (when (:failure result)
      (println "SPEC CHECK FAILURE for" (:sym result))
      (println "  Failure:" (str (:failure result))))
    (nil? (:failure result))))

(deftest stest-check-ok
  (testing "r/ok passes stest/check: any? -> ::ok-result, :fn preserves value"
    (is (check-passed? `r/ok 200))))

(deftest stest-check-err
  (testing "r/err passes stest/check: keyword? [map?] -> ::err-result, :fn preserves category"
    (is (check-passed? `r/err 200))))

(deftest stest-check-ok?
  (testing "r/ok? passes stest/check: any? -> boolean?"
    (is (check-passed? `r/ok? 200))))

(deftest stest-check-err?
  (testing "r/err? passes stest/check: any? -> boolean?"
    (is (check-passed? `r/err? 200))))

(deftest stest-check-known-error?
  (testing "tax/known-error? passes stest/check: any? -> boolean?"
    (is (check-passed? `tax/known-error? 200))))

;; =============================================================================
;; Section 2: Spec-Aware Generative Tests for Combinators
;;
;; bind/map-ok/map-err have fn? args which stest/check cannot generate.
;; These tests manually exercise the fdef contracts:
;;   - :args conformance (inputs match spec)
;;   - :ret conformance (outputs match spec)
;;   - :fn constraints (input/output relationships)
;; =============================================================================

;; --- bind: Result -> (a -> Result) -> Result ---

(defspec spec-bind-ret-is-result 200
  (prop/for-all [result gen-result
                 f gen-result-fn]
    (s/valid? ::rspec/result (r/bind result f))))

(defspec spec-bind-err-short-circuit 200
  (prop/for-all [e gen-err-result
                 f gen-result-fn]
    (let [bound (r/bind e f)]
      (and (s/valid? ::rspec/err-result bound)
           (= bound e)))))

(defspec spec-bind-ok-applies-f 200
  (prop/for-all [v gen/any-printable
                 f gen-result-fn]
    (let [ok-val (r/ok v)
          bound (r/bind ok-val f)]
      (and (s/valid? ::rspec/result bound)
           (= bound (f v))))))

;; --- map-ok: Result -> (a -> b) -> Result ---

(defspec spec-map-ok-ret-is-result 200
  (prop/for-all [result gen-result
                 f gen-plain-fn]
    (s/valid? ::rspec/result (r/map-ok result f))))

(defspec spec-map-ok-err-passthrough 200
  (prop/for-all [e gen-err-result
                 f gen-plain-fn]
    (= (r/map-ok e f) e)))

(defspec spec-map-ok-wraps-in-ok 200
  (prop/for-all [v gen/any-printable
                 f gen-plain-fn]
    (let [mapped (r/map-ok (r/ok v) f)]
      (and (s/valid? ::rspec/ok-result mapped)
           (= (:ok mapped) (f v))))))

;; --- map-err: Result -> (ErrMap -> ErrMap) -> Result ---

(defspec spec-map-err-ret-is-result 200
  (prop/for-all [result gen-result
                 f gen-err-transform-fn]
    (let [mapped (r/map-err result f)]
      (or (r/ok? mapped) (r/err? mapped)))))

(defspec spec-map-err-ok-passthrough 200
  (prop/for-all [v gen/any-printable
                 f gen-err-transform-fn]
    (= (r/map-err (r/ok v) f) (r/ok v))))

(defspec spec-map-err-transforms-err 200
  (prop/for-all [e gen-err-result
                 f gen-err-transform-fn]
    (= (r/map-err e f) (f e))))

;; =============================================================================
;; Section 3: Spec Validity Tests
;;
;; Verify the specs themselves correctly classify Results.
;; Tests the discriminated union property: Result = Ok | Err, never both.
;; =============================================================================

(deftest spec-ok-result-validity
  (testing "::ok-result accepts ok, rejects err and invalid"
    (is (s/valid? ::rspec/ok-result {:ok 42}))
    (is (s/valid? ::rspec/ok-result {:ok nil}))
    (is (s/valid? ::rspec/ok-result {:ok [1 2 3]}))
    (is (not (s/valid? ::rspec/ok-result {:error :x})))
    (is (not (s/valid? ::rspec/ok-result {:ok 1 :error :x})) "both keys = invalid")
    (is (not (s/valid? ::rspec/ok-result {})))
    (is (not (s/valid? ::rspec/ok-result nil)))
    (is (not (s/valid? ::rspec/ok-result "string")))))

(deftest spec-err-result-validity
  (testing "::err-result accepts err, rejects ok and invalid"
    (is (s/valid? ::rspec/err-result {:error :io/timeout}))
    (is (s/valid? ::rspec/err-result {:error :x :message "boom"}))
    (is (not (s/valid? ::rspec/err-result {:ok 42})))
    (is (not (s/valid? ::rspec/err-result {:ok 1 :error :x})) "both keys = invalid")
    (is (not (s/valid? ::rspec/err-result {})))
    (is (not (s/valid? ::rspec/err-result {:error "not-keyword"})))))

(deftest spec-result-sum-type
  (testing "::result is exactly one of ok or err, never both, never neither"
    (is (s/valid? ::rspec/result {:ok 42}))
    (is (s/valid? ::rspec/result {:error :x}))
    (is (not (s/valid? ::rspec/result {:ok 1 :error :x})))
    (is (not (s/valid? ::rspec/result {})))
    (is (not (s/valid? ::rspec/result nil)))
    (is (not (s/valid? ::rspec/result "not a map")))))

;; =============================================================================
;; Section 4: Generator Conformance
;;
;; Verify custom generators produce spec-valid samples.
;; This ensures Section 2's tests use valid inputs.
;; =============================================================================

(defspec gen-ok-result-conforms 100
  (prop/for-all [r gen-ok-result]
    (s/valid? ::rspec/ok-result r)))

(defspec gen-err-result-conforms 100
  (prop/for-all [r gen-err-result]
    (s/valid? ::rspec/err-result r)))

(defspec gen-result-conforms 100
  (prop/for-all [r gen-result]
    (s/valid? ::rspec/result r)))

(deftest generators-produce-both-variants
  (testing "gen-result produces both ok and err in 100 samples"
    (let [samples (gen/sample gen-result 100)]
      (is (some r/ok? samples) "Should produce at least one ok result")
      (is (some r/err? samples) "Should produce at least one err result"))))

;; =============================================================================
;; Section 5: Instrumentation Round-Trip
;;
;; Verify instrument!/unstrument! lifecycle works without breaking functions.
;; =============================================================================

(deftest instrumentation-roundtrip
  (testing "instrument! enables spec checking, unstrument! disables"
    (rspec/instrument!)
    ;; Valid calls should succeed
    (is (= {:ok 42} (r/ok 42)))
    (is (= {:error :io/timeout} (r/err :io/timeout)))
    (is (true? (r/ok? (r/ok 1))))
    (is (false? (r/err? (r/ok 1))))
    (is (= (r/ok 10) (r/bind (r/ok 5) #(r/ok (* 2 %)))))
    (is (= (r/ok 10) (r/map-ok (r/ok 5) #(* 2 %))))
    (is (boolean? (tax/known-error? :io/timeout)))
    ;; Clean up
    (rspec/unstrument!)))

;; =============================================================================
;; Section 6: Cross-Spec Coherence
;;
;; Verify that constructors produce outputs matching their respective specs,
;; and that predicates agree with spec validation.
;; =============================================================================

(defspec ok-constructor-matches-spec 200
  (prop/for-all [v gen/any-printable]
    (let [result (r/ok v)]
      (and (s/valid? ::rspec/ok-result result)
           (s/valid? ::rspec/result result)
           (not (s/valid? ::rspec/err-result result))))))

(defspec err-constructor-matches-spec 200
  (prop/for-all [cat gen-err-category]
    (let [result (r/err cat)]
      (and (s/valid? ::rspec/err-result result)
           (s/valid? ::rspec/result result)
           (not (s/valid? ::rspec/ok-result result))))))

(defspec predicates-agree-with-specs 200
  (prop/for-all [result gen-result]
    (and (= (r/ok? result) (s/valid? ::rspec/ok-result result))
         (= (r/err? result) (s/valid? ::rspec/err-result result))
         (s/valid? ::rspec/result result))))

(defspec err-with-data-matches-spec 200
  (prop/for-all [cat gen-err-category
                 data (gen/map gen/keyword-ns gen/any-printable {:max-elements 3})]
    (let [safe-data (dissoc data :ok)
          result (r/err cat safe-data)]
      (and (s/valid? ::rspec/err-result result)
           (= cat (:error result))
           (every? (fn [[k v]] (= v (get result k))) safe-data)))))
