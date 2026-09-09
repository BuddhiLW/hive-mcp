(ns hive-mcp.agent.provider-discovery-test
  "Tests for config-driven provider discovery: adding, removing and reordering
   an OpenAI-compatible provider from config alone, with no source edit.

   The invariant under test: `effective-provider-registry` and
   `effective-provider-priority` are the single source for WHICH providers
   exist and in WHAT order they are auto-selected. Nothing downstream may
   read the static seed instead."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.openrouter :as openrouter]
            [hive-mcp.config.core :as config]))

(defn- stub-config
  "Config stub: one map keyed by the config key `get-config-value` is asked for."
  [m]
  (fn [k] (get m k)))

(def ^:private acme-entry
  {:api-url       "https://acme.test/v1/chat/completions"
   :secret-key    :acme-api-key
   :default-model "acme-1"})

;; =============================================================================
;; Adding a provider from config
;; =============================================================================

(deftest config-only-provider-joins-discovery-test
  (testing "a provider that exists only in config is auto-discoverable"
    (with-redefs [config/get-config-value (stub-config {"llm-providers" {:acme acme-entry}})
                  config/get-secret       (fn [k] (when (= k :acme-api-key) "sk-acme"))]
      (is (contains? (set (openrouter/effective-provider-priority)) :acme)
          "the seed order never names :acme; the effective order must")
      (is (contains? (set (openrouter/available-providers)) :acme)
          "its configured key makes it available")
      (is (not (contains? (set (openrouter/available-providers)) :venice))
          "a seeded provider with no key stays unavailable")))

  (testing "config-only providers are appended in a deterministic order"
    (with-redefs [config/get-config-value
                  (stub-config {"llm-providers"
                                {:zeta acme-entry :acme acme-entry}})]
      (is (= [:acme :zeta]
             (vec (drop (count openrouter/provider-priority)
                        (openrouter/effective-provider-priority))))))))

;; =============================================================================
;; Removing a provider from config
;; =============================================================================

(deftest config-false-removes-a-seeded-provider-test
  (testing "a config value of false takes a seeded provider out of circulation"
    (with-redefs [config/get-config-value (stub-config {"llm-providers" {:venice false}})
                  config/get-secret       (fn [k] (when (= k :venice-api-key) "sk-venice"))]
      (is (not (contains? (openrouter/effective-provider-registry) :venice)))
      (is (not (contains? (set (openrouter/effective-provider-priority)) :venice)))
      (is (not (contains? (set (openrouter/available-providers)) :venice))
          "a removed provider is not available even with its key configured")
      (is (some? (openrouter/validate-provider :venice))
          "and it no longer validates as a known provider")))

  (testing "nil removes it too"
    (with-redefs [config/get-config-value (stub-config {"llm-providers" {:groq nil}})]
      (is (not (contains? (openrouter/effective-provider-registry) :groq)))))

  (testing "a non-map, non-removal value is ignored rather than corrupting the entry"
    (with-redefs [config/get-config-value (stub-config {"llm-providers" {:groq "nonsense"}})]
      (is (= (get openrouter/provider-registry :groq)
             (get (openrouter/effective-provider-registry) :groq)))))

  (testing "removal does not disturb the other seeded providers"
    (with-redefs [config/get-config-value (stub-config {"llm-providers" {:venice false}})]
      (is (= (vec (remove #{:venice} openrouter/provider-priority))
             (vec (openrouter/effective-provider-priority)))))))

;; =============================================================================
;; Dispatch-routed providers are never auto-selected
;; =============================================================================

(deftest dispatch-routed-provider-stays-out-of-discovery-test
  (testing ":anthropic is in the registry but never in the discovery order"
    (with-redefs [config/get-config-value (stub-config {})
                  config/get-secret       (fn [k] (when (= k :anthropic-api-key) "sk-ant"))]
      (is (contains? (openrouter/effective-provider-registry) :anthropic))
      (is (not (contains? (set (openrouter/effective-provider-priority)) :anthropic))
          "auto-discovery must only ever hand out OpenAI-compat providers")
      (is (not (contains? (set (openrouter/available-providers)) :anthropic))))))

;; =============================================================================
;; Reordering from config
;; =============================================================================

(deftest explicit-priority-config-replaces-the-seed-order-test
  (testing ":llm-provider-priority replaces the order outright"
    (with-redefs [config/get-config-value
                  (stub-config {"llm-provider-priority" ["groq" "venice"]})]
      (is (= [:groq :venice] (vec (openrouter/effective-provider-priority)))
          "nothing is appended to an explicit list")))

  (testing "unknown and dispatch-routed keys are dropped from an explicit list"
    (with-redefs [config/get-config-value
                  (stub-config {"llm-provider-priority" [:anthropic :nope :openai]})]
      (is (= [:openai] (vec (openrouter/effective-provider-priority))))))

  (testing "an explicit list can promote a config-only provider to first choice"
    (with-redefs [config/get-config-value
                  (stub-config {"llm-providers"          {:acme acme-entry}
                                "llm-provider-priority"  [:acme :ollama-compat]})
                  config/get-secret (fn [k] (when (= k :acme-api-key) "sk-acme"))]
      (is (= :acme (openrouter/best-available-provider)))
      (is (= "acme" (:provider-name (openrouter/auto-backend {:model "acme-1"})))))))

;; =============================================================================
;; Availability reads the effective entry, not the seed
;; =============================================================================

(deftest config-secret-key-override-decides-availability-test
  (testing "availability reads the config-overridden :secret-key, not the seeded one"
    (with-redefs [config/get-config-value
                  (stub-config {"llm-providers" {:venice {:secret-key :relay-api-key}}})
                  config/get-secret (fn [k] (when (= k :relay-api-key) "sk-relay"))]
      (is (contains? (set (openrouter/available-providers)) :venice)))))

(deftest auto-backend-diagnostic-reflects-the-effective-order-test
  (testing "the no-provider error lists what was actually checked"
    (with-redefs [config/get-config-value
                  (stub-config {"llm-providers"         {:venice false}
                                "llm-provider-priority" [:venice :groq]})
                  config/get-secret (fn [_] nil)]
      (try
        (openrouter/auto-backend {:model "m"})
        (is false "expected a throw: no provider has a key")
        (catch clojure.lang.ExceptionInfo e
          (is (= [:groq] (mapv :provider (:checked (ex-data e))))
              "a config-removed provider must not appear in the diagnostic"))))))

(comment
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent.provider-discovery-test))
