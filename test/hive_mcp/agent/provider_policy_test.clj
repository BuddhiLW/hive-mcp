(ns hive-mcp.agent.provider-policy-test
  "The promote stratum, tested as what it is: pure functions over data.

   Not one `with-redefs` appears in this namespace. Every decision the provider
   subsystem makes is reachable by passing a registry and a config value as
   arguments, which is the whole point of splitting policy from collect."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.provider.model :as model]
            [hive-mcp.agent.provider.policy :as policy]))

(def ^:private seed
  {:alpha {:api-url "https://alpha.test/v1/chat/completions"
           :secret-key :alpha-api-key
           :default-model "alpha-1"}
   :beta  {:api-url "https://beta.test/v1/chat/completions"
           :secret-key :beta-api-key
           :default-model "beta-1"}
   :local {:api-url "http://localhost:11434/v1/chat/completions"
           :secret-key nil
           :default-model "local-1"}
   :native {:dispatch :anthropic-oauth
            :secret-key :native-api-key
            :default-model "claude-sonnet-4-6"}})

(def ^:private seed-order [:alpha :beta :local])

;; =============================================================================
;; overlay
;; =============================================================================

(deftest overlay-test
  (testing "a map merges field by field, config winning"
    (is (= "alpha-2" (get-in (policy/overlay seed {:alpha {:default-model "alpha-2"}})
                             [:alpha :default-model])))
    (is (= "https://alpha.test/v1/chat/completions"
           (get-in (policy/overlay seed {:alpha {:default-model "alpha-2"}})
                   [:alpha :api-url]))
        "untouched fields survive"))

  (testing "an unknown key adds a provider"
    (is (contains? (policy/overlay seed {:gamma {:api-url "https://gamma.test/v1/chat/completions"
                                                 :secret-key :gamma-api-key
                                                 :default-model "gamma-1"}})
                   :gamma)))

  (testing "false and nil remove"
    (is (not (contains? (policy/overlay seed {:alpha false}) :alpha)))
    (is (not (contains? (policy/overlay seed {:alpha nil}) :alpha))))

  (testing "anything else is ignored, and a missing config leaves the seed alone"
    (is (= seed (policy/overlay seed {:alpha "nonsense"})))
    (is (= seed (policy/overlay seed nil)))
    (is (= seed (policy/overlay seed {"alpha" {:default-model "x"}}))
        "a string key is not a provider keyword")))

;; =============================================================================
;; discovery-order
;; =============================================================================

(deftest discovery-order-test
  (testing "seed order first, then config-only providers sorted by name"
    (let [reg (policy/overlay seed {:zeta (:alpha seed) :gamma (:alpha seed)})]
      (is (= [:alpha :beta :local :gamma :zeta]
             (policy/discovery-order reg seed-order nil)))))

  (testing "a removed provider leaves the order"
    (is (= [:beta :local]
           (policy/discovery-order (policy/overlay seed {:alpha false}) seed-order nil))))

  (testing "dispatch-routed entries are never offered"
    (is (not (contains? (set (policy/discovery-order seed (conj seed-order :native) nil))
                        :native))))

  (testing "an explicit order replaces the seed outright"
    (is (= [:beta :alpha] (policy/discovery-order seed seed-order ["beta" "alpha"])))
    (is (= [:alpha] (policy/discovery-order seed seed-order [:alpha :native :nope]))
        "unknown and dispatch-routed keys drop out")
    (is (= [:beta] (policy/discovery-order seed seed-order [:beta]))
        "nothing is appended to an explicit list")))

;; =============================================================================
;; available / secret-keys-of
;; =============================================================================

(deftest availability-test
  (testing "a nil secret-key needs no secret; others need theirs present"
    (is (= [:beta :local] (policy/available seed seed-order #{:beta-api-key})))
    (is (= [:local] (policy/available seed seed-order #{})))
    (is (= [:alpha :beta :local]
           (policy/available seed seed-order #{:alpha-api-key :beta-api-key}))))

  (testing "secret-keys-of names exactly what a collector must resolve"
    (is (= [:alpha-api-key :beta-api-key] (policy/secret-keys-of seed seed-order))
        "the keyless provider contributes no lookup")))

;; =============================================================================
;; validation
;; =============================================================================

(deftest validation-test
  (testing "an unknown provider reports what IS available"
    (let [err (policy/validate-provider seed :nope)]
      (is (= :unknown-provider (:error err)))
      (is (= (set (keys seed)) (set (:available err))))))

  (testing "a known provider validates"
    (is (nil? (policy/validate-provider seed :alpha))))

  (testing "a model is only constrained when the entry declares :available-models"
    (is (nil? (policy/validate-model seed :alpha "anything")))
    (let [reg (policy/overlay seed {:alpha {:available-models ["alpha-1"]}})]
      (is (nil? (policy/validate-model reg :alpha "alpha-1")))
      (is (= :unknown-model-for-provider (:error (policy/validate-model reg :alpha "alpha-9")))))))

;; =============================================================================
;; routing
;; =============================================================================

(deftest resolve-routing-test
  (testing "explicit provider wins, and supplies its default model"
    (is (= {:provider :beta :model "beta-1"}
           (policy/resolve-routing seed {:provider :beta}))))

  (testing "a '<provider>:<model>' prefix routes and strips"
    (is (= {:provider :alpha :model "some-model"}
           (policy/resolve-routing seed {:model "alpha:some-model"})))
    (is (= {:provider nil :model "unknown:some-model"}
           (policy/resolve-routing seed {:model "unknown:some-model"}))
        "a prefix naming no provider is part of the model name"))

  (testing "a Claude model name routes to :anthropic and loses the vendor prefix"
    (is (= {:provider :anthropic :model "claude-sonnet-4-6"}
           (policy/resolve-routing seed {:model "anthropic/claude-sonnet-4-6"}))))

  (testing "explicit routing beats Claude auto-detection (the privacy escape hatch)"
    (is (= {:provider :alpha :model "claude-sonnet-4-6"}
           (policy/resolve-routing seed {:provider :alpha :model "claude-sonnet-4-6"})))
    (is (= {:provider :alpha :model "claude-sonnet-4-6"}
           (policy/resolve-routing seed {:model "alpha:claude-sonnet-4-6"}))))

  (testing "then the agent-type default, then the collected fallback"
    (is (= {:provider :beta :model "beta-9"}
           (policy/resolve-routing seed {:type-defaults {:provider :beta :model "beta-9"}})))
    (is (= {:provider :local :model "local-1"}
           (policy/resolve-routing seed {:fallback-provider :local}))))

  (testing "with nothing to go on the provider stays open for the caller to fill"
    (is (nil? (:provider (policy/resolve-routing seed {}))))))

;; =============================================================================
;; domain predicates
;; =============================================================================

(deftest model-predicates-test
  (testing "dispatch-routed and openai-compat partition the registry"
    (is (model/dispatch-routed? (:native seed)))
    (is (not (model/openai-compat? (:native seed))))
    (is (model/openai-compat? (:alpha seed)))
    (is (not (model/dispatch-routed? nil)))
    (is (not (model/openai-compat? nil))))

  (testing "the shipped seed conforms to its own schema"
    (is (every? model/valid-provider-entry? (vals model/seed-registry)))
    (is (every? #(contains? model/seed-registry %) model/seed-priority)
        "every seeded discovery key resolves to an entry")))

(comment
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent.provider-policy-test))
