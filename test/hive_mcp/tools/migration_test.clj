(ns hive-mcp.tools.migration-test
  "Unit tests for migration tool handlers.

   Tests cover (mocked backends):
   - Command dispatch routing
   - Parameter validation
   - Help command
   - Adapter listing
   - Error handling for missing parameters

   CRITICAL: Uses with-redefs to mock KG operations - never touches real data."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.java.io :as io]
            [hive-mcp.tools.migration :as migration]
            [hive-mcp.migration.core :as core]
            [hive-mcp.migration.adapter :as adapter]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:dynamic *test-dir* nil)

(defn temp-dir-fixture
  "Create temp directory for file-based tests."
  [f]
  (let [tmp-dir (io/file (System/getProperty "java.io.tmpdir")
                         (str "hive-migration-tool-test-" (System/nanoTime)))]
    (.mkdirs tmp-dir)
    (binding [*test-dir* (.getAbsolutePath tmp-dir)]
      (try
        (f)
        (finally
          (when (.exists tmp-dir)
            (doseq [file (reverse (file-seq tmp-dir))]
              (.delete file))))))))

(use-fixtures :each temp-dir-fixture)

;; =============================================================================
;; Help Command Tests
;; =============================================================================

(deftest cmd-help-returns-commands-test
  (testing "cmd-help returns list of commands"
    (let [result (migration/cmd-help {})]
      (is (map? result))
      (is (contains? result :commands))
      (is (sequential? (:commands result)))
      (is (pos? (count (:commands result)))))))

(deftest cmd-help-includes-all-commands-test
  (testing "cmd-help includes all expected commands"
    (let [result (migration/cmd-help {})
          commands (set (map :command (:commands result)))]
      (is (contains? commands "status"))
      (is (contains? commands "backup"))
      (is (contains? commands "restore"))
      (is (contains? commands "list"))
      (is (contains? commands "switch"))
      (is (contains? commands "sync"))
      (is (contains? commands "export"))
      (is (contains? commands "import"))
      (is (contains? commands "validate"))
      (is (contains? commands "adapters")))))

(deftest cmd-help-has-descriptions-test
  (testing "cmd-help commands have descriptions"
    (let [result (migration/cmd-help {})]
      (doseq [cmd (:commands result)]
        (is (string? (:description cmd))
            (str "Missing description for: " (:command cmd)))))))

;; =============================================================================
;; Adapters Command Tests
;; =============================================================================

(deftest cmd-adapters-returns-list-test
  (testing "cmd-adapters returns adapter list"
    (let [result (migration/cmd-adapters {})]
      (is (map? result))
      (is (contains? result :adapters))
      (is (sequential? (:adapters result))))))

(deftest cmd-adapters-includes-builtin-test
  (testing "cmd-adapters includes built-in adapters"
    (let [result (migration/cmd-adapters {})
          ids (set (map :id (:adapters result)))]
      (is (contains? ids :edn))
      (is (contains? ids :json)))))

;; =============================================================================
;; List Command Tests
;; =============================================================================

(deftest cmd-list-returns-backups-test
  (testing "cmd-list returns backups structure"
    (let [result (migration/cmd-list {:dir *test-dir*})]
      (is (map? result))
      (is (contains? result :backups))
      (is (contains? result :count))
      (is (vector? (:backups result)))
      (is (number? (:count result))))))

(deftest cmd-list-finds-backups-test
  (testing "cmd-list finds backup files"
    ;; Create test backup files
    (spit (str *test-dir* "/kg-datascript-20260201T100000.edn")
          (pr-str {:backup/version 1}))
    (spit (str *test-dir* "/kg-datalevin-20260202T100000.edn")
          (pr-str {:backup/version 1}))
    
    (let [result (migration/cmd-list {:dir *test-dir*})]
      (is (= 2 (:count result)))
      (is (= 2 (count (:backups result)))))))

(deftest cmd-list-filters-by-scope-test
  (testing "cmd-list filters by scope parameter"
    (spit (str *test-dir* "/kg-datascript-20260201T100000.edn")
          (pr-str {:backup/version 1}))
    (spit (str *test-dir* "/memory-datascript-20260202T100000.edn")
          (pr-str {:backup/version 1}))
    
    (let [result (migration/cmd-list {:dir *test-dir* :scope :kg})]
      (is (= 1 (:count result)))
      (is (= :kg (:scope (first (:backups result))))))))

(deftest cmd-list-respects-limit-test
  (testing "cmd-list respects limit parameter"
    (dotimes [i 10]
      (spit (str *test-dir* "/kg-datascript-2026020" i "T100000.edn")
            (pr-str {:backup/version 1})))
    
    (let [result (migration/cmd-list {:dir *test-dir* :limit 3})]
      (is (= 3 (:count result))))))

;; =============================================================================
;; Validate Command Tests
;; =============================================================================

(deftest cmd-validate-valid-file-test
  (testing "cmd-validate accepts valid backup"
    (let [path (str *test-dir* "/valid-backup.edn")]
      (spit path (pr-str {:backup/version 1
                          :backup/scope :kg
                          :backup/counts {:edges 0}}))
      (let [result (migration/cmd-validate {:path path})]
        (is (:valid? result))
        (is (empty? (:errors result)))))))

(deftest cmd-validate-invalid-file-test
  (testing "cmd-validate rejects invalid backup"
    (let [path (str *test-dir* "/invalid-backup.edn")]
      (spit path (pr-str {:backup/version 1}))  ;; Missing scope and counts
      (let [result (migration/cmd-validate {:path path})]
        (is (not (:valid? result)))
        (is (seq (:errors result)))))))

(deftest cmd-validate-requires-path-test
  (testing "cmd-validate throws when path missing"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"path required"
                          (migration/cmd-validate {})))))

;; =============================================================================
;; Handle Migration Dispatcher Tests
;; =============================================================================

(deftest handle-migration-dispatches-help-test
  (testing "handle-migration dispatches to help"
    (let [result (migration/handle-migration {:command :help})]
      (is (contains? result :commands)))))

(deftest handle-migration-dispatches-adapters-test
  (testing "handle-migration dispatches to adapters"
    (let [result (migration/handle-migration {:command :adapters})]
      (is (contains? result :adapters)))))

(deftest handle-migration-dispatches-list-test
  (testing "handle-migration dispatches to list"
    (let [result (migration/handle-migration {:command :list :dir *test-dir*})]
      (is (contains? result :backups)))))

(deftest handle-migration-dispatches-validate-test
  (testing "handle-migration dispatches to validate"
    (let [path (str *test-dir* "/test.edn")]
      (spit path (pr-str {:backup/version 1 :backup/scope :kg :backup/counts {}}))
      (let [result (migration/handle-migration {:command :validate :path path})]
        (is (contains? result :valid?))))))

(deftest handle-migration-default-to-help-test
  (testing "handle-migration defaults to help for unknown command"
    (let [result (migration/handle-migration {:command :unknown})]
      (is (contains? result :commands)))))

(deftest handle-migration-string-command-test
  (testing "handle-migration accepts string command"
    (let [result (migration/handle-migration {:command "help"})]
      (is (contains? result :commands)))))

;; =============================================================================
;; Export Command Tests (Mocked)
;; =============================================================================

(deftest cmd-export-requires-path-test
  (testing "cmd-export throws when path missing"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"path required"
                          (migration/cmd-export {:scope :kg})))))

(deftest cmd-export-with-mock-test
  (testing "cmd-export exports data to file (mocked)"
    (let [path (str *test-dir* "/export-test.edn")]
      ;; Mock the KG export function
      (with-redefs [hive-mcp.knowledge-graph.migration/export-to-edn
                    (fn [] {:edges [{:id "e1"}]
                            :disc []
                            :synthetic []
                            :counts {:edges 1 :disc 0 :synthetic 0}})]
        (let [result (migration/cmd-export {:path path :scope :kg})]
          (is (:success result))
          (is (.exists (io/file path))))))))

;; =============================================================================
;; Import Command Tests (Mocked)
;; =============================================================================

(deftest cmd-import-requires-path-test
  (testing "cmd-import throws when path missing"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"path required"
                          (migration/cmd-import {})))))

(deftest cmd-import-dry-run-test
  (testing "cmd-import dry-run returns preview"
    (let [path (str *test-dir* "/import-test.edn")]
      (spit path (pr-str {:edges [{:id "e1"} {:id "e2"}]
                          :disc [{:path "/test"}]
                          :synthetic []}))
      (let [result (migration/cmd-import {:path path :dry-run true})]
        (is (:dry-run result))
        (is (= 2 (get-in result [:would-import :edges])))
        (is (= 1 (get-in result [:would-import :disc])))))))

;; =============================================================================
;; Backup Command Tests (Mocked)
;; =============================================================================

(deftest cmd-backup-with-mock-test
  (testing "cmd-backup creates backup file (mocked)"
    (with-redefs [hive-mcp.knowledge-graph.migration/export-to-edn
                  (fn [] {:edges [] :disc [] :synthetic []
                          :counts {:edges 0 :disc 0 :synthetic 0}})
                  hive-mcp.knowledge-graph.migration/detect-current-backend
                  (fn [] :datascript)]
      (let [result (migration/cmd-backup {:dir *test-dir* :scope :kg})]
        (is (:success result))
        (is (string? (:path result)))
        (is (.exists (io/file (:path result))))))))

;; =============================================================================
;; Restore Command Tests
;; =============================================================================

(deftest cmd-restore-requires-path-or-latest-test
  (testing "cmd-restore throws when no path or latest specified"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"No backup path"
                          (migration/cmd-restore {})))))

(deftest cmd-restore-dry-run-test
  (testing "cmd-restore dry-run returns metadata"
    (let [path (str *test-dir* "/restore-test.edn")]
      (spit path (pr-str {:backup/version 1
                          :backup/scope :kg
                          :backup/counts {:edges 5 :disc 2 :synthetic 1}
                          :data {:edges []}}))
      (let [result (migration/cmd-restore {:path path :dry-run true})]
        (is (:dry-run result))
        (is (= path (:path result)))
        (is (some? (:metadata result)))))))

(deftest cmd-restore-fails-invalid-backup-test
  (testing "cmd-restore fails on invalid backup"
    (let [path (str *test-dir* "/invalid-restore.edn")]
      (spit path (pr-str {:invalid "data"}))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"validation failed"
                            (migration/cmd-restore {:path path}))))))

;; =============================================================================
;; Switch Command Tests
;; =============================================================================

(deftest cmd-switch-requires-target-test
  (testing "cmd-switch throws when target missing"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Target backend required"
                          (migration/cmd-switch {})))))

(deftest cmd-switch-dry-run-test
  (testing "cmd-switch dry-run returns preview (mocked)"
    (with-redefs [hive-mcp.knowledge-graph.migration/detect-current-backend
                  (fn [] :datascript)
                  hive-mcp.knowledge-graph.migration/export-to-edn
                  (fn [] {:edges [] :disc [] :synthetic []
                          :counts {:edges 0 :disc 0 :synthetic 0}})]
      (let [result (migration/cmd-switch {:target :datalevin :dry-run true})]
        (is (:dry-run result))
        (is (= :datascript (:from result)))
        (is (= :datalevin (:to result)))))))

(deftest cmd-switch-same-backend-throws-test
  (testing "cmd-switch throws when target equals current"
    (with-redefs [hive-mcp.knowledge-graph.migration/detect-current-backend
                  (fn [] :datalevin)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Already using target backend"
                            (migration/cmd-switch {:target :datalevin}))))))

;; =============================================================================
;; Sync Command Tests
;; =============================================================================

(deftest cmd-sync-requires-target-test
  (testing "cmd-sync throws when target missing"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Target backend required"
                          (migration/cmd-sync {})))))
