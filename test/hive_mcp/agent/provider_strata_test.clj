(ns hive-mcp.agent.provider-strata-test
  "The CPPB stratification of the provider subsystem, gated by READING THE
   SOURCE, not by a comment that says so.

   Strata, bottom up:
     provider.model    values and seeds. May require malli and clojure.*
     provider.policy   pure decisions. May require model and clojure.*
     provider.collect  config reads. The ONLY provider ns allowed hive-mcp.config
     provider          pipeline. Composes collect and policy, no I/O of its own
     agent.openrouter  boundary. Speaks HTTP, delegates the concept to provider

   Each rule is proven in BOTH states: the checker is also run against a
   synthetic ns form that violates it, so a gate that silently stopped checking
   fails this suite instead of passing it."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Reading a namespace's declared requires out of its source
;; =============================================================================

(defn- ns-form
  "The `ns` form of `ns-sym`, read from its source file on the classpath."
  [ns-sym]
  (let [path (-> (name ns-sym)
                 (str/replace "-" "_")
                 (str/replace "." "/")
                 (str ".clj"))]
    (with-open [r (java.io.PushbackReader. (io/reader (io/resource path)))]
      (read {:read-cond :allow} r))))

(defn- required-nses
  "The namespaces an `ns` form requires, as a set of symbols."
  [form]
  (->> form
       (drop 2)
       (filter #(and (sequential? %) (= :require (first %))))
       (mapcat rest)
       (map #(if (sequential? %) (first %) %))
       (filter symbol?)
       set))

(defn- forbidden-requires
  "Those required namespaces of `form` whose name matches any of `patterns`."
  [form patterns]
  (->> (required-nses form)
       (filter (fn [r] (some #(str/includes? (name r) %) patterns)))
       set))

(def ^:private violating-ns-form
  '(ns some.pure.ns
     "A stratum that reaches for the world."
     (:require [hive-mcp.config.core :as config]
               [clj-http.client :as http])))

;; =============================================================================
;; The gates
;; =============================================================================

(deftest model-stratum-is-pure-values-test
  (testing "the domain namespace requires no config, no HTTP, no sibling stratum"
    (let [form (ns-form 'hive-mcp.agent.provider.model)]
      (is (empty? (forbidden-requires form ["config" "http" "provider.policy"
                                            "provider.collect"])))
      (is (= '#{clojure.string malli.core} (required-nses form))
          "the domain layer's whole world is malli and clojure.string")))

  (testing "the gate itself still bites"
    (is (seq (forbidden-requires violating-ns-form ["config" "http"]))
        "a stratum reaching for config/http must be caught, or this gate is dead")))

(deftest policy-stratum-is-pure-decisions-test
  (testing "the promote namespace reads nothing: no config, no HTTP, no collect"
    (let [form (ns-form 'hive-mcp.agent.provider.policy)]
      (is (empty? (forbidden-requires form ["config" "http" "provider.collect"])))
      (is (= '#{clojure.string hive-mcp.agent.provider.model} (required-nses form))
          "policy may lean on the domain layer and nothing else")))

  (testing "the gate itself still bites"
    (is (seq (forbidden-requires violating-ns-form ["config"])))))

(deftest collect-stratum-is-the-only-config-reader-test
  (testing "collect owns the config seam"
    (is (contains? (required-nses (ns-form 'hive-mcp.agent.provider.collect))
                   'hive-mcp.config.core)))

  (testing "and no other provider stratum shares it"
    (doseq [n '[hive-mcp.agent.provider.model
                hive-mcp.agent.provider.policy
                hive-mcp.agent.provider]]
      (is (not (contains? (required-nses (ns-form n)) 'hive-mcp.config.core))
          (str n " must reach config through provider.collect, not directly"))))

  (testing "collect decides nothing: it does not require policy"
    (is (not (contains? (required-nses (ns-form 'hive-mcp.agent.provider.collect))
                        'hive-mcp.agent.provider.policy)))))

(deftest pipeline-stratum-composes-collect-and-policy-test
  (let [form (ns-form 'hive-mcp.agent.provider)]
    (testing "the pipeline requires both halves it composes"
      (is (set/subset? '#{hive-mcp.agent.provider.collect
                          hive-mcp.agent.provider.policy
                          hive-mcp.agent.provider.model}
                       (required-nses form))))
    (testing "and speaks no HTTP of its own"
      (is (empty? (forbidden-requires form ["http" "clj-http"]))))))

(deftest boundary-delegates-the-concept-test
  (let [form (ns-form 'hive-mcp.agent.openrouter)]
    (testing "the HTTP boundary owns the wire"
      (is (contains? (required-nses form) 'clj-http.client)))
    (testing "and asks the pipeline which providers exist"
      (is (contains? (required-nses form) 'hive-mcp.agent.provider)))
    (testing "it never reaches past the pipeline into policy or collect"
      (is (empty? (forbidden-requires form ["provider.policy" "provider.collect"]))))))

(comment
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent.provider-strata-test))
