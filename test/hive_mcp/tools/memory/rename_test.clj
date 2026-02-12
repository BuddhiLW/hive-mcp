(ns hive-mcp.tools.memory.rename-test
  "Tests for handle-rename-project unified rename command.

   Coverage:
   - Validation: missing params, same old/new
   - Dry-run: returns preview without modifying
   - Full rename: orchestrates Chroma + KG + EDN + config
   - EDN update: appends old-project-id to :aliases, idempotent
   - Config re-registration: kg-scope gets updated
   - Error handling: graceful on Chroma/KG failures"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [hive-mcp.tools.memory.migration :as migration]
            [hive-mcp.knowledge-graph.scope :as kg-scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Test Helpers
;; ============================================================

(defn- parse-mcp-result
  "Parse the JSON from an MCP result.
   mcp-json returns {:type 'text' :text '{...json...}'}."
  [result]
  (try
    (let [text (or (get-in result [:result 0 :text])
                   (:text result))]
      (when text
        (json/read-str text :key-fn keyword)))
    (catch Exception _
      result)))

(defn- create-temp-dir!
  "Create a temporary directory for testing. Returns absolute path string."
  []
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "hive-rename-test-" (System/nanoTime)))]
    (.mkdirs dir)
    (.getAbsolutePath dir)))

(defn- write-edn!
  "Write EDN data to a .hive-project.edn file in the given directory."
  [directory config]
  (let [edn-file (io/file directory ".hive-project.edn")]
    (spit (.getAbsolutePath edn-file) (pr-str config))))

(defn- read-edn
  "Read .hive-project.edn from a directory."
  [directory]
  (let [edn-file (io/file directory ".hive-project.edn")]
    (when (.exists edn-file)
      (edn/read-string (slurp edn-file)))))

(defn- cleanup-dir!
  "Remove a temp directory and its contents."
  [dir-path]
  (let [dir (io/file dir-path)]
    (doseq [f (reverse (file-seq dir))]
      (.delete f))))

;; Clean up kg-scope state between tests
(use-fixtures :each
  (fn [test-fn]
    (kg-scope/clear-config-cache!)
    (try
      (test-fn)
      (finally
        (kg-scope/clear-config-cache!)))))

;; ============================================================
;; Validation Tests
;; ============================================================

(deftest rename-missing-old-project-id-test
  (testing "rename fails when old-project-id is missing"
    (let [result (parse-mcp-result
                  (migration/handle-rename-project
                   {:new-project-id "new-project"}))]
      (is (some? result))
      ;; mcp-error returns isError true
      (is (true? (get-in (migration/handle-rename-project
                          {:new-project-id "new-project"})
                         [:isError]))))))

(deftest rename-missing-new-project-id-test
  (testing "rename fails when new-project-id is missing"
    (let [result (migration/handle-rename-project
                  {:old-project-id "old-project"})]
      (is (true? (:isError result))))))

(deftest rename-same-project-id-test
  (testing "rename fails when old and new are the same"
    (let [result (migration/handle-rename-project
                  {:old-project-id "same-id"
                   :new-project-id "same-id"})]
      (is (true? (:isError result))))))

;; ============================================================
;; Dry-Run Tests
;; ============================================================

(deftest rename-dry-run-test
  (testing "dry-run returns preview without modifying anything"
    (let [dir (create-temp-dir!)
          _ (write-edn! dir {:project-id "old-project"
                             :aliases []})
          ;; Mock Chroma and KG for dry-run
          call-log (atom [])]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries
                      (fn [& _args]
                        (swap! call-log conj :chroma-query)
                        [{:id "e1"} {:id "e2"} {:id "e3"}])

                      hive-mcp.chroma.core/embedding-configured?
                      (fn [] true)

                      hive-mcp.knowledge-graph.edges/get-edges-by-scope
                      (fn [scope]
                        (swap! call-log conj [:kg-query scope])
                        [{:id "edge1"} {:id "edge2"}])]

          (let [result (parse-mcp-result
                        (migration/handle-rename-project
                         {:old-project-id "old-project"
                          :new-project-id "new-project"
                          :directory dir
                          :dry-run true}))]
            ;; Verify dry-run result structure
            (is (= "dry-run" (:status result)))
            (is (= 3 (get-in result [:chroma :would-migrate])))
            (is (= 2 (get-in result [:kg-edges :would-migrate])))
            (is (true? (get-in result [:edn :exists])))
            (is (= [] (get-in result [:edn :current-aliases])))
            (is (true? (get-in result [:edn :would-add-alias])))
            (is (= "old-project" (:old-project-id result)))
            (is (= "new-project" (:new-project-id result)))

            ;; Verify .hive-project.edn was NOT modified
            (let [edn-after (read-edn dir)]
              (is (= "old-project" (:project-id edn-after)))
              (is (= [] (:aliases edn-after))))))
        (finally
          (cleanup-dir! dir))))))

(deftest rename-dry-run-no-directory-test
  (testing "dry-run works without directory (edn section shows nil)"
    (with-redefs [hive-mcp.chroma.core/query-entries
                  (fn [& _args] [])

                  hive-mcp.chroma.core/embedding-configured?
                  (fn [] true)

                  hive-mcp.knowledge-graph.edges/get-edges-by-scope
                  (fn [_] [])]

      (let [result (parse-mcp-result
                    (migration/handle-rename-project
                     {:old-project-id "old"
                      :new-project-id "new"
                      :dry-run true}))]
        (is (= "dry-run" (:status result)))
        (is (= 0 (get-in result [:chroma :would-migrate])))
        (is (false? (get-in result [:edn :exists])))))))

;; ============================================================
;; Full Rename Tests
;; ============================================================

(deftest rename-full-flow-test
  (testing "full rename orchestrates Chroma + KG + EDN + config"
    (let [dir (create-temp-dir!)
          _ (write-edn! dir {:project-id "old-project"
                             :aliases []
                             :project-type :clojure-cli})
          chroma-updates (atom [])
          kg-migrate-calls (atom [])]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries
                      (fn [& {:keys [project-id]}]
                        (when (= project-id "old-project")
                          [{:id "e1" :tags ["scope:project:old-project" "note"]}
                           {:id "e2" :tags ["scope:project:old-project" "convention"]}]))

                      hive-mcp.chroma.core/embedding-configured?
                      (fn [] true)

                      hive-mcp.chroma.core/update-entry!
                      (fn [id updates]
                        (swap! chroma-updates conj {:id id :updates updates}))

                      hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                      (fn [old-id new-id]
                        (swap! kg-migrate-calls conj {:old old-id :new new-id})
                        {:migrated 3})]

          (let [result (parse-mcp-result
                        (migration/handle-rename-project
                         {:old-project-id "old-project"
                          :new-project-id "new-project"
                          :directory dir}))]
            ;; Overall status
            (is (= "success" (:status result)))
            (is (= "old-project" (:old-project-id result)))
            (is (= "new-project" (:new-project-id result)))

            ;; Chroma migration happened
            (is (= 2 (get-in result [:chroma :migrated])))
            (is (= 2 (get-in result [:chroma :updated-scopes])))
            (is (= 2 (count @chroma-updates)))

            ;; Verify Chroma updates have correct new project-id and scope tags
            (let [update1 (first @chroma-updates)]
              (is (= "e1" (:id update1)))
              (is (= "new-project" (get-in update1 [:updates :project-id])))
              (is (some #(= % "scope:project:new-project")
                        (get-in update1 [:updates :tags]))))

            ;; KG edge migration happened
            (is (= 3 (get-in result [:kg-edges :migrated])))
            (is (= 1 (count @kg-migrate-calls)))
            (is (= {:old "old-project" :new "new-project"} (first @kg-migrate-calls)))

            ;; EDN was updated
            (is (true? (get-in result [:edn :updated])))
            (is (= ["old-project"] (get-in result [:edn :aliases])))

            ;; Verify actual .hive-project.edn file
            (let [edn-after (read-edn dir)]
              (is (= "new-project" (:project-id edn-after)))
              (is (= ["old-project"] (:aliases edn-after)))
              (is (= :clojure-cli (:project-type edn-after))))

            ;; Config was registered
            (is (true? (:config-registered result)))))
        (finally
          (cleanup-dir! dir))))))

(deftest rename-without-directory-test
  (testing "rename works without directory (skips EDN update)"
    (with-redefs [hive-mcp.chroma.core/query-entries
                  (fn [& _args] [])

                  hive-mcp.chroma.core/embedding-configured?
                  (fn [] true)

                  hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                  (fn [_ _] {:migrated 0})]

      (let [result (parse-mcp-result
                    (migration/handle-rename-project
                     {:old-project-id "old"
                      :new-project-id "new"}))]
        (is (= "success" (:status result)))
        (is (false? (get-in result [:edn :updated])))
        (is (= "no directory provided" (get-in result [:edn :reason])))))))

;; ============================================================
;; EDN Update Tests
;; ============================================================

(deftest rename-edn-alias-appended-test
  (testing "old-project-id is appended to :aliases vector"
    (let [dir (create-temp-dir!)
          _ (write-edn! dir {:project-id "old-project"
                             :aliases ["even-older"]})]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries (fn [& _] [])
                      hive-mcp.chroma.core/embedding-configured? (fn [] true)
                      hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                      (fn [_ _] {:migrated 0})]

          (migration/handle-rename-project
           {:old-project-id "old-project"
            :new-project-id "new-project"
            :directory dir})

          ;; Verify aliases chain
          (let [edn-after (read-edn dir)]
            (is (= "new-project" (:project-id edn-after)))
            (is (= ["even-older" "old-project"] (:aliases edn-after)))))
        (finally
          (cleanup-dir! dir))))))

(deftest rename-edn-alias-idempotent-test
  (testing "re-running rename doesn't duplicate alias"
    (let [dir (create-temp-dir!)
          _ (write-edn! dir {:project-id "old-project"
                             :aliases ["old-project"]})]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries (fn [& _] [])
                      hive-mcp.chroma.core/embedding-configured? (fn [] true)
                      hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                      (fn [_ _] {:migrated 0})]

          (migration/handle-rename-project
           {:old-project-id "old-project"
            :new-project-id "new-project"
            :directory dir})

          ;; old-project should only appear once
          (let [edn-after (read-edn dir)]
            (is (= "new-project" (:project-id edn-after)))
            (is (= ["old-project"] (:aliases edn-after)))
            (is (= 1 (count (filter #(= "old-project" %) (:aliases edn-after)))))))
        (finally
          (cleanup-dir! dir))))))

(deftest rename-edn-no-existing-file-test
  (testing "rename creates .hive-project.edn if directory exists but file doesn't"
    (let [dir (create-temp-dir!)]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries (fn [& _] [])
                      hive-mcp.chroma.core/embedding-configured? (fn [] true)
                      hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                      (fn [_ _] {:migrated 0})]

          (migration/handle-rename-project
           {:old-project-id "old-project"
            :new-project-id "new-project"
            :directory dir})

          ;; Should have created .hive-project.edn
          (let [edn-after (read-edn dir)]
            (is (some? edn-after))
            (is (= "new-project" (:project-id edn-after)))
            (is (= ["old-project"] (:aliases edn-after)))))
        (finally
          (cleanup-dir! dir))))))

;; ============================================================
;; Config Registration Tests
;; ============================================================

(deftest rename-config-registered-test
  (testing "kg-scope config is re-registered after rename"
    (let [dir (create-temp-dir!)
          _ (write-edn! dir {:project-id "old-project"
                             :parent-id "parent-proj"
                             :aliases []})]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries (fn [& _] [])
                      hive-mcp.chroma.core/embedding-configured? (fn [] true)
                      hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                      (fn [_ _] {:migrated 0})]

          (migration/handle-rename-project
           {:old-project-id "old-project"
            :new-project-id "new-project"
            :directory dir})

          ;; Verify config is registered in kg-scope
          (let [config (kg-scope/get-project-config "new-project")]
            (is (some? config))
            (is (= "new-project" (:project-id config)))
            (is (= ["old-project"] (:aliases config)))))
        (finally
          (cleanup-dir! dir))))))

(deftest rename-alias-resolution-works-test
  (testing "after rename, old-project-id resolves to new via alias index"
    (let [dir (create-temp-dir!)
          _ (write-edn! dir {:project-id "old-project"
                             :aliases []})]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries (fn [& _] [])
                      hive-mcp.chroma.core/embedding-configured? (fn [] true)
                      hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                      (fn [_ _] {:migrated 0})]

          (migration/handle-rename-project
           {:old-project-id "old-project"
            :new-project-id "new-project"
            :directory dir})

          ;; old-project should resolve to new-project via alias
          (is (= "new-project" (kg-scope/resolve-project-id "old-project")))
          ;; new-project should resolve to itself
          (is (= "new-project" (kg-scope/resolve-project-id "new-project"))))
        (finally
          (cleanup-dir! dir))))))

;; ============================================================
;; Error Handling Tests
;; ============================================================

(deftest rename-chroma-failure-non-blocking-test
  (testing "Chroma failure doesn't prevent EDN update"
    (let [dir (create-temp-dir!)
          _ (write-edn! dir {:project-id "old-project"
                             :aliases []})]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries
                      (fn [& _] (throw (Exception. "Chroma unavailable")))

                      hive-mcp.chroma.core/embedding-configured?
                      (fn [] true)

                      hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                      (fn [_ _] {:migrated 0})]

          (let [result (parse-mcp-result
                        (migration/handle-rename-project
                         {:old-project-id "old-project"
                          :new-project-id "new-project"
                          :directory dir}))]
            ;; Result should still succeed overall
            (is (= "success" (:status result)))
            ;; Chroma section should have error info
            (is (= 0 (get-in result [:chroma :migrated])))
            ;; EDN should still be updated
            (is (true? (get-in result [:edn :updated])))))
        (finally
          (cleanup-dir! dir))))))

;; ============================================================
;; Consolidated Tool Integration
;; ============================================================

(deftest consolidated-memory-rename-dispatch-test
  (testing "consolidated memory tool dispatches rename command"
    (let [dir (create-temp-dir!)]
      (try
        (with-redefs [hive-mcp.chroma.core/query-entries (fn [& _] [])
                      hive-mcp.chroma.core/embedding-configured? (fn [] true)
                      hive-mcp.knowledge-graph.edges/migrate-edge-scopes!
                      (fn [_ _] {:migrated 0})]

          ;; Verify the handler is in the handlers map
          (let [handlers (requiring-resolve 'hive-mcp.tools.consolidated.memory/handlers)]
            (is (some? (get @handlers :rename))
                "rename command should be in handlers map")))
        (finally
          (cleanup-dir! dir))))))
