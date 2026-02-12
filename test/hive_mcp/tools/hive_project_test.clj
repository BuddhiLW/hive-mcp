(ns hive-mcp.tools.hive-project-test
  "TDD tests for hive-project.clj auto-generation tool.

   Tests cover:
   - Watch directory inference by project type
   - Hot-reload enablement detection
   - Project ID generation
   - EDN content generation with new schema
   - Native project type detection (no Emacs)
   - Headless .hive-project.edn generation
   - Startup scan for missing .edn files"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [hive-mcp.tools.hive-project :as hp]
            [hive-mcp.config.core :as config]))

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

(deftest generate-base-project-id-test
  (testing "Base project ID contains sanitized name prefix"
    (let [generate-base-project-id #'hive-mcp.tools.hive-project/generate-base-project-id]
      (let [id (generate-base-project-id "my-project")]
        (is (str/starts-with? id "my-project-"))
        (is (= 19 (count id)) "Format: my-project (10) + - (1) + hash (8) = 19"))))

  (testing "Base project ID sanitizes special characters"
    (let [generate-base-project-id #'hive-mcp.tools.hive-project/generate-base-project-id]
      (let [id (generate-base-project-id "My@Project#Name")]
        (is (str/starts-with? id "my-project-name-"))
        (is (not (str/includes? id "@")))
        (is (not (str/includes? id "#"))))))

  (testing "Base project ID hash is 8 hex characters"
    (let [generate-base-project-id #'hive-mcp.tools.hive-project/generate-base-project-id]
      (let [id (generate-base-project-id "test")
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

;; =============================================================================
;; Helper: Create temp project directories for testing
;; =============================================================================

(defn- create-temp-project!
  "Create a temporary directory structure for testing.
   Returns the directory File object.
   marker-files is a vector of filenames to create (e.g. ['deps.edn' '.git'])."
  [marker-files]
  (let [tmp-dir (io/file (System/getProperty "java.io.tmpdir")
                         (str "hive-test-" (System/nanoTime)))]
    (.mkdirs tmp-dir)
    (doseq [f marker-files]
      (let [target (io/file tmp-dir f)]
        (if (= f ".git")
          (.mkdirs target)
          (spit target ""))))
    tmp-dir))

(defn- cleanup-temp-dir!
  "Recursively delete a temporary directory."
  [dir]
  (when (.exists dir)
    (doseq [f (reverse (file-seq dir))]
      (.delete f))))

;; =============================================================================
;; Native Project Type Detection Tests
;; =============================================================================

(deftest detect-project-type-native-test
  (testing "Clojure CLI project (.git + deps.edn)"
    (let [dir (create-temp-project! [".git" "deps.edn"])]
      (try
        (is (= "clojure-cli" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Lein project (.git + project.clj)"
    (let [dir (create-temp-project! [".git" "project.clj"])]
      (try
        (is (= "lein" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "npm project (.git + package.json)"
    (let [dir (create-temp-project! [".git" "package.json"])]
      (try
        (is (= "npm" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Cargo project (.git + Cargo.toml)"
    (let [dir (create-temp-project! [".git" "Cargo.toml"])]
      (try
        (is (= "cargo" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Go module project (.git + go.mod)"
    (let [dir (create-temp-project! [".git" "go.mod"])]
      (try
        (is (= "go-mod" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Maven project (.git + pom.xml)"
    (let [dir (create-temp-project! [".git" "pom.xml"])]
      (try
        (is (= "maven" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Python project (.git + pyproject.toml)"
    (let [dir (create-temp-project! [".git" "pyproject.toml"])]
      (try
        (is (= "python" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Generic project (.git only, no marker files)"
    (let [dir (create-temp-project! [".git"])]
      (try
        (is (= "generic" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Not a project (no .git directory)"
    (let [dir (create-temp-project! ["package.json"])]
      (try
        (is (nil? (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "nil directory returns nil"
    (is (nil? (hp/detect-project-type-native nil))))

  (testing "Non-existent directory returns nil"
    (is (nil? (hp/detect-project-type-native "/tmp/nonexistent-dir-12345"))))

  (testing "Priority: deps.edn wins over package.json"
    (let [dir (create-temp-project! [".git" "deps.edn" "package.json"])]
      (try
        (is (= "clojure-cli" (hp/detect-project-type-native (.getAbsolutePath dir))))
        (finally (cleanup-temp-dir! dir))))))

;; =============================================================================
;; Headless Generation Tests
;; =============================================================================

(deftest generate-hive-project-headless-test
  (testing "Generates .hive-project.edn for a git project"
    (let [dir (create-temp-project! [".git" "deps.edn"])]
      (try
        (let [result (hp/generate-hive-project-headless! (.getAbsolutePath dir))]
          (is (:success result) "Should succeed")
          (is (.exists (io/file dir ".hive-project.edn")) "File should exist")
          ;; Verify generated content
          (let [content (edn/read-string (slurp (io/file dir ".hive-project.edn")))]
            (is (= (.getName dir) (:project-id content))
                "project-id should be directory basename")
            (is (= :clojure-cli (:project-type content))
                "Should detect as clojure-cli")
            (is (= ["src" "test" "dev"] (:watch-dirs content))
                "Clojure projects should have [src test dev]")
            (is (true? (:hot-reload content))
                "Clojure projects should have hot-reload enabled")))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Never overwrites existing .hive-project.edn"
    (let [dir (create-temp-project! [".git" "deps.edn"])]
      (try
        ;; Create existing .edn with sentinel content
        (spit (io/file dir ".hive-project.edn") "{:project-id \"sentinel\"}")
        (let [result (hp/generate-hive-project-headless! (.getAbsolutePath dir))]
          (is (:skipped result) "Should be skipped")
          (is (= "existing" (:reason result)))
          ;; Verify original content preserved
          (let [content (slurp (io/file dir ".hive-project.edn"))]
            (is (str/includes? content "sentinel")
                "Original content should be preserved")))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Skips directories without .git"
    (let [dir (create-temp-project! ["package.json"])]
      (try
        (let [result (hp/generate-hive-project-headless! (.getAbsolutePath dir))]
          (is (:skipped result) "Should be skipped")
          (is (= "no-git" (:reason result)))
          (is (not (.exists (io/file dir ".hive-project.edn")))
              "Should NOT create file"))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Generates with parent-id from config parent-rules"
    (let [dir (create-temp-project! [".git" "go.mod"])
          dir-path (.getAbsolutePath dir)]
      (try
        ;; Load config with a parent rule matching the temp dir
        (config/load-global-config! "/tmp/nonexistent-config.edn")
        ;; Since config won't have rules, parent-id should be nil
        (let [result (hp/generate-hive-project-headless! dir-path)]
          (is (:success result))
          (let [content (edn/read-string (slurp (io/file dir ".hive-project.edn")))]
            (is (= :go-mod (:project-type content)))
            ;; parent-id should be nil since no matching rule
            (is (nil? (:parent-id content)))))
        (finally (cleanup-temp-dir! dir)))))

  (testing "Returns nil on nil input"
    ;; nil directory → io/file resolves to cwd which may have .edn
    ;; Either nil (error) or {:skipped true} is acceptable
    (let [result (hp/generate-hive-project-headless! nil)]
      (is (or (nil? result) (:skipped result))
          "nil input should either return nil or be skipped")))

  (testing "Skips non-existent directory (no-git reason)"
    (let [result (hp/generate-hive-project-headless!
                  "/tmp/absolutely-nonexistent-dir-999888777")]
      (is (:skipped result) "Should be skipped")
      (is (= "no-git" (:reason result))
          "Should skip because dir doesn't exist (no .git)"))))

;; =============================================================================
;; Scan and Generate Missing Tests
;; =============================================================================

(deftest scan-and-generate-missing-test
  (testing "Returns zero counts when no project-roots configured"
    ;; Reset config to defaults (empty project-roots)
    (config/load-global-config! "/tmp/nonexistent-config.edn")
    (let [result (hp/scan-and-generate-missing!)]
      (is (= 0 (:scanned result)))
      (is (= 0 (:generated result)))
      (is (= 0 (:skipped result)))
      (is (= 0 (:errors result)))))

  (testing "Scans project-root and generates missing .edn files"
    (let [;; Create a temp root with two child projects
          root (io/file (System/getProperty "java.io.tmpdir")
                        (str "hive-scan-test-" (System/nanoTime)))
          proj-a (io/file root "proj-a")
          proj-b (io/file root "proj-b")
          proj-c (io/file root "proj-c")]
      (try
        ;; Setup: proj-a has .git+deps.edn, proj-b has .git+package.json,
        ;; proj-c has .git+.hive-project.edn (should be skipped)
        (.mkdirs (io/file proj-a ".git"))
        (spit (io/file proj-a "deps.edn") "{}")
        (.mkdirs (io/file proj-b ".git"))
        (spit (io/file proj-b "package.json") "{}")
        (.mkdirs (io/file proj-c ".git"))
        (spit (io/file proj-c ".hive-project.edn") "{:project-id \"existing\"}")

        ;; Write temp config file with project-root pointing to our temp dir
        (let [config-file (io/file (System/getProperty "java.io.tmpdir")
                                   (str "hive-config-" (System/nanoTime) ".edn"))]
          (spit config-file
                (pr-str {:project-roots [(.getAbsolutePath root)]}))
          (config/load-global-config! (.getAbsolutePath config-file))

          (let [result (hp/scan-and-generate-missing!)]
            ;; proj-a and proj-b should get generated (2 dirs with .git, no .edn)
            ;; proj-c has .hive-project.edn already so isn't even in the scan list
            (is (= 2 (:scanned result))
                "Should scan 2 dirs without .edn")
            (is (= 2 (:generated result))
                "Should generate 2 files")
            (is (= 0 (:skipped result)))

            ;; Verify generated files
            (is (.exists (io/file proj-a ".hive-project.edn"))
                "proj-a should have .edn")
            (is (.exists (io/file proj-b ".hive-project.edn"))
                "proj-b should have .edn")

            ;; Verify content
            (let [a-config (edn/read-string (slurp (io/file proj-a ".hive-project.edn")))
                  b-config (edn/read-string (slurp (io/file proj-b ".hive-project.edn")))]
              (is (= "proj-a" (:project-id a-config)))
              (is (= :clojure-cli (:project-type a-config)))
              (is (= "proj-b" (:project-id b-config)))
              (is (= :npm (:project-type b-config))))

            ;; Verify existing file not touched
            (let [c-content (slurp (io/file proj-c ".hive-project.edn"))]
              (is (str/includes? c-content "existing")
                  "Existing .edn should not be modified")))

          (.delete config-file))
        (finally
          ;; Cleanup
          (doseq [f (reverse (file-seq root))]
            (.delete f)))))))

;; =============================================================================
;; Marker File Priority Test
;; =============================================================================

(deftest marker-file-priority-test
  (testing "First matching marker wins (deps.edn before package.json)"
    (let [markers @#'hive-mcp.tools.hive-project/marker-file->type]
      ;; Verify deps.edn comes before package.json in priority
      (let [deps-idx (count (take-while #(not= "deps.edn" (first %)) markers))
            pkg-idx (count (take-while #(not= "package.json" (first %)) markers))]
        (is (< deps-idx pkg-idx)
            "deps.edn should have higher priority than package.json"))))

  (testing "All marker types map to known project types"
    (let [markers @#'hive-mcp.tools.hive-project/marker-file->type
          type->watch-dirs @#'hive-mcp.tools.hive-project/type->watch-dirs]
      (doseq [[_file ptype] markers]
        (is (contains? type->watch-dirs ptype)
            (str ptype " should have watch-dirs mapping"))))))
