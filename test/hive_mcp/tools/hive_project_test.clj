(ns hive-mcp.tools.hive-project-test
  "TDD tests for hive-project.clj auto-generation tool.
   
   Tests cover:
   - Watch directory inference by project type
   - Hot-reload enablement detection
   - Project ID generation
   - EDN content generation with new schema"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.tools.hive-project]))

;; =============================================================================
;; Test Private Functions via Eval (pure function testing)
;; =============================================================================

(deftest infer-watch-dirs-test
  (testing "Clojure project types include src, test, dev"
    (let [infer-watch-dirs #'hive-mcp.tools.hive-project/infer-watch-dirs]
      (doseq [clj-type ["clojure" "clojure-cli" "lein" "deps.edn" "shadow-cljs"]]
        (is (= ["src" "test" "dev"] (infer-watch-dirs clj-type))
            (str clj-type " should return [src test dev]")))))

  (testing "Node project types return src and lib"
    (let [infer-watch-dirs #'hive-mcp.tools.hive-project/infer-watch-dirs]
      (doseq [node-type ["npm" "yarn" "pnpm"]]
        (is (= ["src" "lib"] (infer-watch-dirs node-type))
            (str node-type " should return [src lib]")))))

  (testing "Go projects return cmd, pkg, internal"
    (let [infer-watch-dirs #'hive-mcp.tools.hive-project/infer-watch-dirs]
      (is (= ["cmd" "pkg" "internal"] (infer-watch-dirs "go-mod")))))

  (testing "Python project types return src and lib"
    (let [infer-watch-dirs #'hive-mcp.tools.hive-project/infer-watch-dirs]
      (doseq [py-type ["python" "poetry" "pipenv"]]
        (is (= ["src" "lib"] (infer-watch-dirs py-type))
            (str py-type " should return [src lib]")))))

  (testing "Generic/unknown types return [src]"
    (let [infer-watch-dirs #'hive-mcp.tools.hive-project/infer-watch-dirs]
      (is (= ["src"] (infer-watch-dirs "generic")))
      (is (= ["src"] (infer-watch-dirs "unknown-type"))))))

(deftest infer-hot-reload-test
  (testing "Clojure types enable hot-reload"
    (let [infer-hot-reload? #'hive-mcp.tools.hive-project/infer-hot-reload?]
      (doseq [clj-type ["clojure" "clojure-cli" "lein" "deps.edn" "shadow-cljs"]]
        (is (true? (infer-hot-reload? clj-type))
            (str clj-type " should enable hot-reload")))))

  (testing "Non-Clojure types disable hot-reload"
    (let [infer-hot-reload? #'hive-mcp.tools.hive-project/infer-hot-reload?]
      (doseq [other-type ["npm" "yarn" "go-mod" "python" "cargo" "generic"]]
        (is (false? (infer-hot-reload? other-type))
            (str other-type " should disable hot-reload"))))))

(deftest generate-project-id-test
  (testing "Project ID contains sanitized name prefix"
    (let [generate-project-id #'hive-mcp.tools.hive-project/generate-project-id]
      (let [id (generate-project-id "my-project")]
        (is (str/starts-with? id "my-project-"))
        (is (= 19 (count id)) "Format: my-project (10) + - (1) + hash (8) = 19"))))

  (testing "Project ID sanitizes special characters"
    (let [generate-project-id #'hive-mcp.tools.hive-project/generate-project-id]
      (let [id (generate-project-id "My@Project#Name")]
        (is (str/starts-with? id "my-project-name-"))
        (is (not (str/includes? id "@")))
        (is (not (str/includes? id "#"))))))

  (testing "Project ID hash is 8 hex characters"
    (let [generate-project-id #'hive-mcp.tools.hive-project/generate-project-id]
      (let [id (generate-project-id "test")
            hash-part (last (str/split id #"-"))]
        (is (= 8 (count hash-part)))
        (is (re-matches #"[0-9a-f]{8}" hash-part))))))

(deftest generate-edn-content-test
  (testing "EDN output includes all required fields"
    (let [generate-edn-content #'hive-mcp.tools.hive-project/generate-edn-content
          config {:project-id "test-12345678"
                  :project-type :clojure
                  :watch-dirs ["src" "test" "dev"]
                  :hot-reload true
                  :presets-path ".hive/presets"}
          edn (generate-edn-content config)]
      (is (str/includes? edn ":project-id \"test-12345678\""))
      (is (str/includes? edn ":project-type :clojure"))
      (is (str/includes? edn ":watch-dirs"))
      (is (str/includes? edn "[\"src\" \"test\" \"dev\"]"))
      (is (str/includes? edn ":hot-reload true"))
      (is (str/includes? edn ":presets-path"))))

  (testing "EDN output includes generation timestamp comment"
    (let [generate-edn-content #'hive-mcp.tools.hive-project/generate-edn-content
          config {:project-id "test-12345678"
                  :project-type :npm
                  :watch-dirs ["src" "lib"]
                  :hot-reload false}
          edn (generate-edn-content config)]
      (is (str/includes? edn ";; Generated:"))
      (is (str/includes? edn ";; hive-mcp project configuration"))))

  (testing "EDN output handles aliases when present"
    (let [generate-edn-content #'hive-mcp.tools.hive-project/generate-edn-content
          config {:project-id "new-id-12345678"
                  :project-type :generic
                  :watch-dirs ["src"]
                  :hot-reload false
                  :aliases ["old-id-1" "old-id-2"]}
          edn (generate-edn-content config)]
      (is (str/includes? edn ":aliases"))
      (is (str/includes? edn "old-id-1"))
      (is (str/includes? edn "old-id-2")))))

(deftest format-edn-value-test
  (testing "Keywords formatted correctly"
    (let [format-edn-value #'hive-mcp.tools.hive-project/format-edn-value]
      (is (= ":clojure" (format-edn-value :clojure)))
      (is (= ":npm" (format-edn-value :npm)))))

  (testing "Vectors formatted with proper quoting"
    (let [format-edn-value #'hive-mcp.tools.hive-project/format-edn-value]
      (is (= "[\"src\" \"test\"]" (format-edn-value ["src" "test"])))))

  (testing "Booleans formatted correctly"
    (let [format-edn-value #'hive-mcp.tools.hive-project/format-edn-value]
      (is (= "true" (format-edn-value true)))
      (is (= "false" (format-edn-value false))))))

;; =============================================================================
;; Integration Test (requires mocking projectile)
;; =============================================================================

(deftest type->watch-dirs-coverage-test
  (testing "All documented project types have watch-dirs mappings"
    (let [type->watch-dirs @#'hive-mcp.tools.hive-project/type->watch-dirs
          expected-types #{"clojure" "clojure-cli" "lein" "deps.edn" "shadow-cljs"
                           "npm" "yarn" "pnpm" "cargo" "go-mod"
                           "maven" "gradle" "python" "poetry" "pipenv"
                           "mix" "rebar3" "cmake" "meson" "make" "generic"}]
      (doseq [type expected-types]
        (is (contains? type->watch-dirs type)
            (str "Missing mapping for " type)))))

  (testing "All watch-dirs are non-empty vectors"
    (let [type->watch-dirs @#'hive-mcp.tools.hive-project/type->watch-dirs]
      (doseq [[type dirs] type->watch-dirs]
        (is (vector? dirs) (str type " should have vector of dirs"))
        (is (seq dirs) (str type " should have non-empty dirs"))))))
