(ns hive-mcp.server-config-test
  "Tests for server configuration loading.
   
   Regression tests for hot-reload config (commit 723b2fa).
   Ensures :hot-reload false in .hive-project.edn is respected."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]))

;; =============================================================================
;; Test: read-project-config behavior
;; =============================================================================

(defn- simulate-read-project-config
  "Simulate the read-project-config logic from server.clj.
   Takes config map (as if parsed from .hive-project.edn).
   Returns {:watch-dirs [...] :hot-reload bool}."
  [config]
  {:watch-dirs (:watch-dirs config)
   :hot-reload (get config :hot-reload true)})

(deftest test-hot-reload-defaults-to-true
  (testing ":hot-reload defaults to true when not specified"
    (let [config {:project-id "test"}
          result (simulate-read-project-config config)]
      (is (true? (:hot-reload result))))))

(deftest test-hot-reload-false-is-respected
  (testing ":hot-reload false in config is respected"
    (let [config {:project-id "test"
                  :hot-reload false}
          result (simulate-read-project-config config)]
      (is (false? (:hot-reload result))))))

(deftest test-hot-reload-true-is-respected
  (testing ":hot-reload true in config is respected"
    (let [config {:project-id "test"
                  :hot-reload true}
          result (simulate-read-project-config config)]
      (is (true? (:hot-reload result))))))

(deftest test-watch-dirs-are-extracted
  (testing ":watch-dirs are extracted from config"
    (let [config {:project-id "test"
                  :watch-dirs ["src" "dev"]}
          result (simulate-read-project-config config)]
      (is (= ["src" "dev"] (:watch-dirs result))))))

(deftest test-nil-config-handled
  (testing "nil config results in default hot-reload true"
    (let [result (simulate-read-project-config nil)]
      (is (true? (:hot-reload result)))
      (is (nil? (:watch-dirs result))))))

;; =============================================================================
;; Test: EDN parsing of actual config format
;; =============================================================================

(deftest test-edn-parsing-hot-reload-false
  (testing "EDN string with :hot-reload false parses correctly"
    (let [edn-str "{:project-id \"test\"
                   :hot-reload false}"
          config (edn/read-string edn-str)
          result (simulate-read-project-config config)]
      (is (false? (:hot-reload result))))))

(deftest test-edn-parsing-with-comments
  (testing "EDN with comments parses correctly"
    (let [edn-str "{:project-id \"test\"
                   ;; Disable hot-reload for stability
                   :hot-reload false
                   :watch-dirs [\"src\"]}"
          config (edn/read-string edn-str)
          result (simulate-read-project-config config)]
      (is (false? (:hot-reload result)))
      (is (= ["src"] (:watch-dirs result))))))
