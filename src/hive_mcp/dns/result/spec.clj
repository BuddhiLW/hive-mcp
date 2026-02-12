(ns hive-mcp.dns.result.spec
  "clojure.spec contracts for the Result monad.

   Applies type-theoretic best practices via spec:
   - Sum types: Result = Ok | Err (discriminated union via :ok/:error keys)
   - Smart constructors: ok/err are the only valid constructors
   - Making illegal states unrepresentable: a Result is EITHER ok? OR err?, never both
   - Totality: all functions are total (never throw)
   - Functor/Monad laws enforced by property tests

   References:
   - Wlaschin: Domain Modeling Made Functional (algebraic types, smart constructors)
   - Brady: Type-Driven Development (type-define-refine, totality)
   - Uncle Bob: Functional Design Ch10 (spec = best of static + dynamic)
   - clojure.org/guides/spec"
  (:require [clojure.spec.alpha :as s]
            [hive-mcp.dns.result :as r]
            [hive-mcp.dns.result.taxonomy :as tax]))

;; =============================================================================
;; Primitive Specs
;; =============================================================================

;; Error category: namespace-qualified keyword (e.g. :io/timeout, :sdk/missing-param)
(s/def ::error-category qualified-keyword?)

;; Known error category: subset of error-category in our taxonomy
(s/def ::known-error-category (s/and ::error-category tax/known-error?))

;; =============================================================================
;; Sum Type: Result = Ok | Err
;; =============================================================================

;; Ok result: map with :ok key (value can be anything, including nil)
(s/def ::ok-result (s/and map?
                          #(contains? % :ok)
                          #(not (contains? % :error))))

;; Err result: map with :error key (value is a keyword category)
(s/def ::err-result (s/and map?
                           #(contains? % :error)
                           #(not (contains? % :ok))
                           #(keyword? (:error %))))

;; The Result sum type: exactly one of Ok or Err
;; This is the Clojure equivalent of: type Result a e = Ok a | Err e
(s/def ::result (s/or :ok ::ok-result
                      :err ::err-result))

;; =============================================================================
;; Constructor Contracts (Smart Constructors)
;; =============================================================================

;; ok: any? -> ::ok-result
(s/fdef r/ok
  :args (s/cat :value any?)
  :ret ::ok-result
  :fn #(= (-> % :ret :ok) (-> % :args :value)))

;; err: keyword? -> ::err-result  (1-arity)
;; err: keyword? map? -> ::err-result  (2-arity)
(s/fdef r/err
  :args (s/or :unary (s/cat :category keyword?)
              :binary (s/cat :category keyword? :data map?))
  :ret ::err-result
  :fn #(= (-> % :ret :error) (-> % :args second :category)))

;; =============================================================================
;; Predicate Contracts
;; =============================================================================

;; ok?: any? -> boolean?
;; Totality guarantee: never throws for any input
(s/fdef r/ok?
  :args (s/cat :r any?)
  :ret boolean?)

;; err?: any? -> boolean?
(s/fdef r/err?
  :args (s/cat :r any?)
  :ret boolean?)

;; =============================================================================
;; Combinator Contracts
;; =============================================================================

;; bind: Result -> (a -> Result) -> Result
;; Monad law: if input is Err, output is same Err (short-circuit)
(s/fdef r/bind
  :args (s/cat :result (s/or :ok ::ok-result :err ::err-result)
               :f fn?)
  :ret (s/or :ok ::ok-result :err ::err-result))

;; map-ok: Result -> (a -> b) -> Result
;; Functor: preserves structure, only transforms the value inside Ok
(s/fdef r/map-ok
  :args (s/cat :result (s/or :ok ::ok-result :err ::err-result)
               :f fn?)
  :ret (s/or :ok ::ok-result :err ::err-result))

;; map-err: Result -> (ErrMap -> ErrMap) -> Result
;; Bifunctor over err side: transforms error data, preserves ok
(s/fdef r/map-err
  :args (s/cat :result (s/or :ok ::ok-result :err ::err-result)
               :f fn?)
  :ret (s/or :ok ::ok-result :err ::err-result))

;; =============================================================================
;; Macro Syntax Specs
;; =============================================================================

;; let-ok bindings vector: [sym1 expr1 sym2 expr2 ...]
;; Must be even-length vector of alternating symbols and expressions.
(s/def ::let-ok-bindings
  (s/and vector? #(even? (count %))))

;; =============================================================================
;; Macro Contracts
;; NOTE: Macros cannot be instrumented via stest/instrument (compile-time
;; expansion, not runtime calls). These fdef specs document contracts and
;; enable clj-kondo / spec-based tooling integration.
;; =============================================================================

;; let-ok: monadic let — binds :ok values, short-circuits on first error.
;; (let-ok [x (may-fail) y (use x)] (ok (+ x y)))
(s/fdef r/let-ok
  :args (s/cat :bindings ::let-ok-bindings
               :body (s/+ any?)))

;; try-effect: wraps body in try/catch, returning (ok result) or
;; (err :effect/exception {:message "..." :class "..."})
;; (try-effect (do-side-effect!))
(s/fdef r/try-effect
  :args (s/cat :body (s/+ any?)))

;; try-effect*: like try-effect but with a custom error category keyword.
;; (try-effect* :io/read-failure (slurp path))
(s/fdef r/try-effect*
  :args (s/cat :category keyword?
               :body (s/+ any?)))

;; =============================================================================
;; Taxonomy Contracts
;; =============================================================================

(s/fdef tax/known-error?
  :args (s/cat :category any?)
  :ret boolean?)

;; =============================================================================
;; Instrumentation Helpers
;; =============================================================================

;; Note: macros (let-ok, try-effect, try-effect*) have fdef specs above
;; but are excluded from instrument!/unstrument! — macros expand at
;; compile time and cannot be wrapped at runtime by stest/instrument.

(defn instrument!
  "Enable spec checking on all Result DSL functions.
   Use in dev/test, not production (performance cost).
   Note: macros (let-ok, try-effect, try-effect*) have fdef specs
   but cannot be instrumented (compile-time expansion)."
  []
  (require '[clojure.spec.test.alpha :as stest])
  ((resolve 'clojure.spec.test.alpha/instrument)
   [`r/ok `r/err `r/ok? `r/err? `r/bind `r/map-ok `r/map-err
    `tax/known-error?]))

(defn unstrument!
  "Disable spec checking (restore production performance)."
  []
  (require '[clojure.spec.test.alpha :as stest])
  ((resolve 'clojure.spec.test.alpha/unstrument)
   [`r/ok `r/err `r/ok? `r/err? `r/bind `r/map-ok `r/map-err
    `tax/known-error?]))
