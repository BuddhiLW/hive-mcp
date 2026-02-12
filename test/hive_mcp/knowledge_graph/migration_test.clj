(ns hive-mcp.knowledge-graph.migration-test
  "Tests for KG migration tooling.

   Tests cover:
   - Export produces valid EDN with all entity types
   - Import creates all entities in target store
   - Roundtrip DataScript -> Datahike preserves data
   - Validation catches missing entities"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [hive-mcp.knowledge-graph.migration :as migration]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(use-fixtures :each fixtures/datascript-fixture)

(defn- create-test-edge!
  "Create a test edge for migration testing."
  [id from to relation]
  (conn/transact! [{:kg-edge/id id
                    :kg-edge/from from
                    :kg-edge/to to
                    :kg-edge/relation relation
                    :kg-edge/confidence 0.9
                    :kg-edge/created-at (java.util.Date.)}]))

(defn- create-test-disc!
  "Create a test disc entity for migration testing."
  [path content-hash]
  (conn/transact! [{:disc/path path
                    :disc/content-hash content-hash
                    :disc/analyzed-at (java.util.Date.)
                    :disc/certainty-alpha 5.0
                    :disc/certainty-beta 2.0}]))

(defn- create-test-synthetic!
  "Create a test synthetic node for migration testing."
  [id synth-type members]
  (conn/transact! [{:kg-synthetic/id id
                    :kg-synthetic/type synth-type
                    :kg-synthetic/members (set members)
                    :kg-synthetic/confidence 0.8
                    :kg-synthetic/created-at (java.util.Date.)}]))

;; =============================================================================
;; Export Tests
;; =============================================================================

(deftest export-to-edn-produces-valid-structure
  (testing "export-to-edn returns map with expected keys"
    (let [result (migration/export-to-edn)]
      (is (map? result))
      (is (contains? result :edges))
      (is (contains? result :disc))
      (is (contains? result :synthetic))
      (is (contains? result :exported-at))
      (is (contains? result :counts))
      (is (inst? (:exported-at result))))))

(deftest export-to-edn-captures-all-entities
  (testing "export captures edges, disc, and synthetic nodes"
    ;; Create test data
    (create-test-edge! "edge-1" "entry-a" "entry-b" :implements)
    (create-test-edge! "edge-2" "entry-b" "entry-c" :depends-on)
    (create-test-disc! "/src/test.clj" "abc123")
    (create-test-synthetic! "synth-1" :co-access #{"entry-a" "entry-b"})
    
    (let [result (migration/export-to-edn)]
      (is (= 2 (:edges (:counts result))))
      (is (= 1 (:disc (:counts result))))
      (is (= 1 (:synthetic (:counts result))))
      
      ;; Check actual data
      (is (= 2 (count (:edges result))))
      (is (= 1 (count (:disc result))))
      (is (= 1 (count (:synthetic result)))))))

(deftest export-to-file-creates-readable-edn
  (testing "export-to-file creates a valid EDN file"
    (create-test-edge! "edge-test" "from" "to" :refines)
    
    (let [tmp-file (java.io.File/createTempFile "kg-export-test" ".edn")
          path (.getAbsolutePath tmp-file)]
      (try
        (migration/export-to-file! path)
        
        ;; Verify file exists and is readable
        (is (.exists tmp-file))
        (let [content (slurp path)
              parsed (edn/read-string content)]
          (is (map? parsed))
          (is (= 1 (:edges (:counts parsed)))))
        (finally
          (.delete tmp-file))))))

;; =============================================================================
;; Import Tests
;; =============================================================================

(deftest import-from-edn-creates-entities
  (testing "import creates all entities from EDN data"
    (let [test-data {:edges [{:kg-edge/id "imported-edge"
                              :kg-edge/from "a"
                              :kg-edge/to "b"
                              :kg-edge/relation :implements
                              :kg-edge/confidence 1.0}]
                     :disc [{:disc/path "/imported/file.clj"
                             :disc/content-hash "hash123"}]
                     :synthetic [{:kg-synthetic/id "imported-synth"
                                  :kg-synthetic/type :co-access
                                  :kg-synthetic/members #{"x" "y"}
                                  :kg-synthetic/confidence 0.7}]
                     :counts {:edges 1 :disc 1 :synthetic 1}}
          result (migration/import-from-edn test-data)]
      
      ;; Check import counts
      (is (= 1 (:edges (:imported result))))
      (is (= 1 (:disc (:imported result))))
      (is (= 1 (:synthetic (:imported result))))
      (is (empty? (:errors result)))
      
      ;; Verify entities exist in store
      (let [edges (conn/query '[:find [(pull ?e [*]) ...]
                                :where [?e :kg-edge/id "imported-edge"]])]
        (is (= 1 (count edges))))
      
      (let [disc (conn/query '[:find [(pull ?e [*]) ...]
                               :where [?e :disc/path "/imported/file.clj"]])]
        (is (= 1 (count disc))))
      
      (let [synth (conn/query '[:find [(pull ?e [*]) ...]
                                :where [?e :kg-synthetic/id "imported-synth"]])]
        (is (= 1 (count synth)))))))

(deftest import-handles-duplicate-entities
  (testing "import handles entities with existing IDs"
    ;; Pre-create an edge
    (create-test-edge! "dup-edge" "a" "b" :implements)
    
    ;; Try to import the same edge (should fail/error on unique constraint)
    (let [test-data {:edges [{:kg-edge/id "dup-edge"
                              :kg-edge/from "c"
                              :kg-edge/to "d"
                              :kg-edge/relation :refines}]
                     :disc []
                     :synthetic []
                     :counts {:edges 1 :disc 0 :synthetic 0}}
          result (migration/import-from-edn test-data)]
      ;; Either succeeds (upsert) or records error
      (is (or (= 1 (:edges (:imported result)))
              (seq (:errors result)))))))

;; =============================================================================
;; Roundtrip Tests
;; =============================================================================

(deftest roundtrip-preserves-data
  (testing "export -> reset -> import preserves all data"
    ;; Create test data
    (create-test-edge! "rt-edge-1" "a" "b" :implements)
    (create-test-edge! "rt-edge-2" "b" "c" :depends-on)
    (create-test-disc! "/roundtrip/file.clj" "hash-rt")
    (create-test-synthetic! "rt-synth" :semantic-cluster #{"a" "b" "c"})
    
    ;; Export
    (let [exported (migration/export-to-edn)
          original-counts (:counts exported)]
      (is (= 2 (:edges original-counts)))
      (is (= 1 (:disc original-counts)))
      (is (= 1 (:synthetic original-counts)))
      
      ;; Reset store (fresh database)
      (conn/reset-conn!)
      
      ;; Verify store is empty
      (let [empty-export (migration/export-to-edn)]
        (is (= 0 (:edges (:counts empty-export)))))
      
      ;; Import back
      (let [import-result (migration/import-from-edn exported)]
        (is (empty? (:errors import-result)))
        (is (= 2 (:edges (:imported import-result))))
        (is (= 1 (:disc (:imported import-result))))
        (is (= 1 (:synthetic (:imported import-result)))))
      
      ;; Verify data matches original
      (let [final-export (migration/export-to-edn)]
        (is (= original-counts (:counts final-export)))))))

;; =============================================================================
;; Validation Tests
;; =============================================================================

(deftest validate-migration-detects-complete
  (testing "validation reports success when counts match"
    (create-test-edge! "val-edge" "a" "b" :implements)
    (create-test-disc! "/val/file.clj" "hash")
    (create-test-synthetic! "val-synth" :co-access #{"a" "b"})
    
    (let [result (migration/validate-migration {:edges 1 :disc 1 :synthetic 1})]
      (is (:valid? result))
      (is (= {:edges 1 :disc 1 :synthetic 1} (:expected result)))
      (is (= {:edges 1 :disc 1 :synthetic 1} (:actual result)))
      (is (= {:edges 0 :disc 0 :synthetic 0} (:missing result))))))

(deftest validate-migration-detects-missing
  (testing "validation catches missing entities"
    (create-test-edge! "val-edge" "a" "b" :implements)
    ;; Missing disc and synthetic
    
    (let [result (migration/validate-migration {:edges 1 :disc 2 :synthetic 3})]
      (is (not (:valid? result)))
      (is (= {:edges 1 :disc 2 :synthetic 3} (:expected result)))
      (is (= {:edges 1 :disc 0 :synthetic 0} (:actual result)))
      (is (= {:edges 0 :disc 2 :synthetic 3} (:missing result))))))

(deftest validate-migration-handles-extra-entities
  (testing "validation handles more entities than expected (negative missing)"
    (create-test-edge! "extra-1" "a" "b" :implements)
    (create-test-edge! "extra-2" "b" "c" :refines)
    
    (let [result (migration/validate-migration {:edges 1 :disc 0 :synthetic 0})]
      (is (not (:valid? result)))
      ;; Missing will be negative when we have more than expected
      (is (= -1 (:edges (:missing result)))))))

;; =============================================================================
;; Migrate Store Tests (Dry Run)
;; =============================================================================

(deftest migrate-store-dry-run
  (testing "migrate-store! with dry-run exports without importing"
    (create-test-edge! "dry-edge" "a" "b" :implements)
    
    (let [result (migration/migrate-store! :datascript :datahike
                                           {:dry-run true})]
      (is (:dry-run result))
      (is (= 1 (:edges (:exported result))))
      (is (nil? (:imported result)))
      (is (nil? (:validation result))))))

;; =============================================================================
;; File Roundtrip Tests
;; =============================================================================

(deftest file-export-import-roundtrip
  (testing "export-to-file and import-from-file roundtrip works"
    (create-test-edge! "file-rt-edge" "x" "y" :supersedes)
    (create-test-disc! "/file-rt/test.clj" "file-hash")
    
    (let [tmp-file (java.io.File/createTempFile "kg-roundtrip" ".edn")
          path (.getAbsolutePath tmp-file)]
      (try
        ;; Export
        (let [export-result (migration/export-to-file! path)]
          (is (= 1 (:edges (:counts export-result)))))
        
        ;; Reset
        (conn/reset-conn!)
        
        ;; Import
        (let [import-result (migration/import-from-file! path)]
          (is (= 1 (:edges (:imported import-result))))
          (is (= 1 (:disc (:imported import-result))))
          (is (empty? (:errors import-result))))
        
        ;; Verify
        (let [edges (conn/query '[:find [?id ...]
                                  :where [?e :kg-edge/id ?id]])]
          (is (= #{"file-rt-edge"} (set edges))))
        
        (finally
          (.delete tmp-file))))))
