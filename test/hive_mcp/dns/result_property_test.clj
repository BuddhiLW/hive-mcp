(ns hive-mcp.dns.result-property-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hive-mcp.dns.result :as r]
            [hive-mcp.dns.result.taxonomy :as tax]))

;; --- Generators ---

(def gen-ok
  "Generator for ok Results with any scalar value."
  (gen/fmap r/ok gen/any-printable))

(def gen-err-category
  "Generator for error category keywords."
  (gen/elements [:io/timeout :sdk/invalid-request :kg/node-not-found
                 :chroma/connection-failed :parse/invalid-json
                 :effect/exception :transport/timeout :drone/spawn-failed]))

(def gen-err
  "Generator for err Results."
  (gen/fmap r/err gen-err-category))

(def gen-result
  "Generator for any Result (ok or err)."
  (gen/one-of [gen-ok gen-err]))

(defn gen-result-fn
  "Generator for functions that return Results.
   Produces functions (any-value -> Result)."
  []
  (gen/elements [(fn [x] (r/ok x))
                 (fn [x] (r/ok (str x)))
                 (fn [_] (r/err :test/generated))
                 (fn [x] (r/ok [x]))]))

;; --- P1: Totality — ok/err never throw for any input ---

(defspec p1-ok-totality 200
  (prop/for-all [v gen/any-printable]
                (let [result (r/ok v)]
                  (r/ok? result))))

(defspec p1-err-totality 200
  (prop/for-all [cat gen-err-category]
                (let [result (r/err cat)]
                  (r/err? result))))

;; --- P2: Left identity — (bind (ok x) f) = (f x) ---

(defspec p2-left-identity 200
  (prop/for-all [v gen/any-printable
                 f (gen-result-fn)]
                (= (r/bind (r/ok v) f)
                   (f v))))

;; --- P3: Right identity — (bind m ok) = m ---

(defspec p3-right-identity 200
  (prop/for-all [m gen-result]
                (= (r/bind m r/ok)
                   m)))

;; --- P4: Associativity — (bind (bind m f) g) = (bind m #(bind (f %) g)) ---

(defspec p4-associativity 200
  (prop/for-all [m gen-result
                 f (gen-result-fn)
                 g (gen-result-fn)]
                (= (r/bind (r/bind m f) g)
                   (r/bind m (fn [x] (r/bind (f x) g))))))

;; --- P5: Functor law — (map-ok (err e) f) = (err e) ---

(defspec p5-functor-err-passthrough 200
  (prop/for-all [e gen-err]
                (= (r/map-ok e identity)
                   e)))

;; --- P6: Complementarity — ok? and err? are exact complements on Results ---

(defspec p6-complementarity 200
  (prop/for-all [m gen-result]
                (not= (r/ok? m) (r/err? m))))

;; --- P7: try-effect totality — always returns Result, never throws ---

(defspec p7-try-effect-totality 200
  (prop/for-all [v gen/any-printable]
                (let [result (r/try-effect v)]
                  (r/ok? result))))

(deftest p7-try-effect-exception-totality
  (testing "try-effect catches all exceptions and returns err"
    (dotimes [_ 200]
      (let [result (r/try-effect (throw (ex-info "boom" {:i (rand-int 1000)})))]
        (is (r/err? result))
        (is (= :effect/exception (:error result)))))))

;; --- Additional: map-ok functor identity law ---

(defspec map-ok-identity-law 200
  (prop/for-all [m gen-result]
                (= (r/map-ok m identity)
                   (if (r/ok? m) m m))))

;; --- Additional: bind with err always returns same err ---

(defspec bind-err-absorption 200
  (prop/for-all [e gen-err
                 f (gen-result-fn)]
                (= (r/bind e f) e)))

;; --- Additional: known-error? is total ---

(defspec taxonomy-known-error-total 200
  (prop/for-all [kw gen/keyword]
                (boolean? (tax/known-error? kw))))
