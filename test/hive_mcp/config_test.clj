(ns hive-mcp.config-test
  "Unit tests for global config loader (hive-mcp.config).

   Tests cover:
   - Loading config from file with defaults merge
   - Missing file handling (returns defaults)
   - Malformed file handling (returns defaults)
   - Accessor functions (project-roots, defaults, overrides)
   - Parent rule resolution via path prefix matching
   - Reset/reload behavior"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [hive-mcp.config :as config]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-config-fixture
  "Reset config state before each test."
  [f]
  (config/reset-config!)
  (f)
  (config/reset-config!))

(use-fixtures :each reset-config-fixture)

;; =============================================================================
;; Helper: Create temp config files
;; =============================================================================

(defn- write-temp-config!
  "Write an EDN map to a temp file, return the file path."
  [config-map]
  (let [f (java.io.File/createTempFile "hive-config-test" ".edn")]
    (.deleteOnExit f)
    (spit f (pr-str config-map))
    (.getAbsolutePath f)))

;; =============================================================================
;; Test: load-global-config!
;; =============================================================================

(deftest test-load-missing-file
  (testing "Loading from non-existent file returns defaults"
    (let [result (config/load-global-config! "/tmp/nonexistent-hive-config-xyz.edn")]
      (is (map? result))
      (is (= [] (:project-roots result)))
      (is (= :datahike (get-in result [:defaults :kg-backend])))
      (is (= false (get-in result [:defaults :hot-reload])))
      (is (= {} (:project-overrides result)))
      (is (= [] (:parent-rules result))))))

(deftest test-load-valid-config
  (testing "Loading valid config merges with defaults"
    (let [user-config {:project-roots ["/home/user/PP" "/home/user/PP/hive"]
                       :defaults {:kg-backend :datascript :hot-reload true}
                       :project-overrides {"hive-mcp" {:hot-reload true
                                                       :watch-dirs ["src" "hive-hot/src"]}}
                       :parent-rules [{:path-prefix "/home/user/PP/hive/"
                                       :parent-id "hive-mcp"}]}
          path (write-temp-config! user-config)
          result (config/load-global-config! path)]
      (is (= ["/home/user/PP" "/home/user/PP/hive"] (:project-roots result)))
      (is (= :datascript (get-in result [:defaults :kg-backend])))
      (is (= true (get-in result [:defaults :hot-reload])))
      (is (= {"hive-mcp" {:hot-reload true :watch-dirs ["src" "hive-hot/src"]}}
             (:project-overrides result)))
      (is (= [{:path-prefix "/home/user/PP/hive/" :parent-id "hive-mcp"}]
             (:parent-rules result))))))

(deftest test-load-partial-config-merges-defaults
  (testing "Partial config gets missing keys from defaults"
    (let [user-config {:project-roots ["/home/user/projects"]}
          path (write-temp-config! user-config)
          result (config/load-global-config! path)]
      ;; User value preserved
      (is (= ["/home/user/projects"] (:project-roots result)))
      ;; Defaults filled in for missing keys
      (is (= :datahike (get-in result [:defaults :kg-backend])))
      (is (= {} (:project-overrides result)))
      (is (= [] (:parent-rules result))))))

(deftest test-load-nested-defaults-merge
  (testing "Nested :defaults map is merged, not replaced"
    (let [user-config {:defaults {:hot-reload true}}
          path (write-temp-config! user-config)
          result (config/load-global-config! path)]
      ;; User override
      (is (= true (get-in result [:defaults :hot-reload])))
      ;; Default preserved (not wiped by user :defaults)
      (is (= :datahike (get-in result [:defaults :kg-backend]))))))

(deftest test-load-malformed-file
  (testing "Malformed EDN file returns defaults"
    (let [f (java.io.File/createTempFile "hive-config-bad" ".edn")]
      (.deleteOnExit f)
      (spit f "this is not valid edn {{{")
      (let [result (config/load-global-config! (.getAbsolutePath f))]
        (is (map? result))
        (is (= [] (:project-roots result)))))))

(deftest test-load-non-map-file
  (testing "EDN file that parses to non-map returns defaults"
    (let [f (java.io.File/createTempFile "hive-config-vec" ".edn")]
      (.deleteOnExit f)
      (spit f "[1 2 3]")
      (let [result (config/load-global-config! (.getAbsolutePath f))]
        (is (map? result))
        (is (= [] (:project-roots result)))))))

;; =============================================================================
;; Test: get-global-config (before and after load)
;; =============================================================================

(deftest test-get-global-config-before-load
  (testing "get-global-config returns defaults when not loaded"
    (config/reset-config!)
    (let [result (config/get-global-config)]
      (is (map? result))
      (is (= [] (:project-roots result)))
      (is (= :datahike (get-in result [:defaults :kg-backend]))))))

(deftest test-get-global-config-after-load
  (testing "get-global-config returns loaded config"
    (let [user-config {:project-roots ["/test/path"]}
          path (write-temp-config! user-config)]
      (config/load-global-config! path)
      (is (= ["/test/path"] (:project-roots (config/get-global-config)))))))

;; =============================================================================
;; Test: get-project-roots
;; =============================================================================

(deftest test-get-project-roots-default
  (testing "Project roots default to empty vector"
    (is (= [] (config/get-project-roots)))))

(deftest test-get-project-roots-loaded
  (testing "Project roots reflect loaded config"
    (let [path (write-temp-config! {:project-roots ["/a" "/b"]})]
      (config/load-global-config! path)
      (is (= ["/a" "/b"] (config/get-project-roots))))))

;; =============================================================================
;; Test: get-defaults
;; =============================================================================

(deftest test-get-defaults
  (testing "get-defaults returns defaults map"
    (let [path (write-temp-config! {:defaults {:kg-backend :datascript}})]
      (config/load-global-config! path)
      (let [defaults (config/get-defaults)]
        (is (= :datascript (:kg-backend defaults)))
        ;; Other defaults preserved
        (is (= false (:hot-reload defaults)))))))

;; =============================================================================
;; Test: get-project-overrides
;; =============================================================================

(deftest test-get-project-overrides-existing
  (testing "Returns overrides for known project"
    (let [path (write-temp-config!
                {:project-overrides {"my-proj" {:hot-reload true}}})]
      (config/load-global-config! path)
      (is (= {:hot-reload true} (config/get-project-overrides "my-proj"))))))

(deftest test-get-project-overrides-missing
  (testing "Returns nil for unknown project"
    (let [path (write-temp-config!
                {:project-overrides {"my-proj" {:hot-reload true}}})]
      (config/load-global-config! path)
      (is (nil? (config/get-project-overrides "unknown-proj"))))))

;; =============================================================================
;; Test: get-project-config (merged defaults + overrides)
;; =============================================================================

(deftest test-get-project-config-with-overrides
  (testing "Project config merges defaults with overrides"
    (let [path (write-temp-config!
                {:defaults {:kg-backend :datahike :hot-reload false}
                 :project-overrides {"hive-mcp" {:hot-reload true
                                                 :watch-dirs ["src"]}}})]
      (config/load-global-config! path)
      (let [cfg (config/get-project-config "hive-mcp")]
        (is (= true (:hot-reload cfg)))
        (is (= :datahike (:kg-backend cfg)))
        (is (= ["src"] (:watch-dirs cfg)))))))

(deftest test-get-project-config-without-overrides
  (testing "Project config returns just defaults when no overrides"
    (let [path (write-temp-config!
                {:defaults {:kg-backend :datascript}})]
      (config/load-global-config! path)
      (let [cfg (config/get-project-config "unknown")]
        (is (= :datascript (:kg-backend cfg)))
        (is (= false (:hot-reload cfg)))))))

;; =============================================================================
;; Test: get-parent-for-path
;; =============================================================================

(deftest test-get-parent-for-path-matching
  (testing "Path matching first matching parent-rule"
    (let [path (write-temp-config!
                {:parent-rules [{:path-prefix "/home/user/PP/hive/"
                                 :parent-id "hive-mcp"}
                                {:path-prefix "/home/user/PP/"
                                 :parent-id "root-project"}]})]
      (config/load-global-config! path)
      ;; Should match first rule (more specific)
      (is (= "hive-mcp"
             (config/get-parent-for-path "/home/user/PP/hive/hive-hot"))))))

(deftest test-get-parent-for-path-fallback
  (testing "Path matching falls through to less specific rule"
    (let [path (write-temp-config!
                {:parent-rules [{:path-prefix "/home/user/PP/hive/"
                                 :parent-id "hive-mcp"}
                                {:path-prefix "/home/user/PP/"
                                 :parent-id "root-project"}]})]
      (config/load-global-config! path)
      ;; Not in /hive/ subtree, matches second rule
      (is (= "root-project"
             (config/get-parent-for-path "/home/user/PP/funeraria"))))))

(deftest test-get-parent-for-path-no-match
  (testing "No matching rule returns nil"
    (let [path (write-temp-config!
                {:parent-rules [{:path-prefix "/home/user/PP/"
                                 :parent-id "root"}]})]
      (config/load-global-config! path)
      (is (nil? (config/get-parent-for-path "/completely/different/path"))))))

(deftest test-get-parent-for-path-nil
  (testing "nil directory returns nil"
    (is (nil? (config/get-parent-for-path nil)))))

;; =============================================================================
;; Test: reset-config!
;; =============================================================================

(deftest test-reset-config
  (testing "reset-config! clears cached config"
    (let [path (write-temp-config! {:project-roots ["/test"]})]
      (config/load-global-config! path)
      (is (= ["/test"] (config/get-project-roots)))
      (config/reset-config!)
      ;; After reset, returns defaults
      (is (= [] (config/get-project-roots))))))

(deftest test-reload-after-reset
  (testing "Can reload config after reset"
    (let [path1 (write-temp-config! {:project-roots ["/v1"]})
          path2 (write-temp-config! {:project-roots ["/v2"]})]
      (config/load-global-config! path1)
      (is (= ["/v1"] (config/get-project-roots)))
      (config/reset-config!)
      (config/load-global-config! path2)
      (is (= ["/v2"] (config/get-project-roots))))))

;; =============================================================================
;; Test: default-drone-model / default-drone-backend
;; =============================================================================

(deftest test-default-drone-model-from-config
  (testing "default-drone-model reads from :services :drone :default-model"
    (let [path (write-temp-config!
                {:services {:drone {:mode :local
                                    :default-model "custom/test-model"
                                    :default-backend :openrouter}}})]
      (config/load-global-config! path)
      (is (= "custom/test-model" (config/default-drone-model))))))

(deftest test-default-drone-model-fallback
  (testing "default-drone-model falls back to hardcoded default when :drone missing"
    (let [path (write-temp-config! {:services {}})]
      (config/load-global-config! path)
      ;; deep-merge fills :drone from defaults
      (is (= "devstral-small:24b" (config/default-drone-model))))))

(deftest test-default-drone-model-returns-string
  (testing "default-drone-model always returns a string"
    (is (string? (config/default-drone-model)))))

(deftest test-default-drone-backend-from-config
  (testing "default-drone-backend reads from :services :drone :default-backend"
    (let [path (write-temp-config!
                {:services {:drone {:mode :local
                                    :default-model "model"
                                    :default-backend :custom-backend}}})]
      (config/load-global-config! path)
      (is (= :custom-backend (config/default-drone-backend))))))

(deftest test-default-drone-backend-fallback
  (testing "default-drone-backend falls back to :openrouter when :drone missing"
    (let [path (write-temp-config! {:services {}})]
      (config/load-global-config! path)
      (is (= :openrouter (config/default-drone-backend))))))

(deftest test-default-drone-model-config-override
  (testing "User config.edn :drone :default-model overrides defaults"
    (let [path (write-temp-config!
                {:services {:drone {:default-model "my-org/my-model:latest"}}})]
      (config/load-global-config! path)
      (is (= "my-org/my-model:latest" (config/default-drone-model))))))
