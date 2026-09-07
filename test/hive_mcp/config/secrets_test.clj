(ns hive-mcp.config.secrets-test
  "Startup secret resolution: the registered set plus whatever the config
   declares on its own."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.config.secrets :as secrets]))

(deftest a-config-declared-secret-outside-the-registry-survives-resolution
  (testing "registered keys resolve, and so does a key only the config knows"
    (let [{:keys [secrets sources]}
          (with-redefs [secrets/pass-show (constantly nil)]
            (secrets/resolve-all-secrets {:venice-api-key "pass:Venice/key"
                                          :axon-api-key   "pass:ai/k"}))]
      (is (= "pass:ai/k" (:axon-api-key secrets))
          "kept verbatim: resolve.clj expands the pass: prefix at read time")
      (is (= :config (:axon-api-key sources)))
      (is (= "pass:Venice/key" (:venice-api-key secrets)))
      (is (every? #(contains? secrets %) (keys secrets/secret-registry))
          "every registered key is still reported, missing ones included"))))

(deftest a-declared-secret-is-named-in-the-environment-by-its-keyword
  (is (= "AXON_API_KEY" (secrets/env-var-for :axon-api-key)))
  (is (= "OPENROUTER_API_KEY" (secrets/env-var-for :openrouter-api-key))))
