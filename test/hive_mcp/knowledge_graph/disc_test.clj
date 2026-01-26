(ns hive-mcp.knowledge-graph.disc-test
  "Unit tests for disc entity CRUD operations.

   Tests cover:
   - add-disc! creation and updates
   - get-disc by path
   - update-disc! modifications
   - remove-disc! deletion
   - disc-stats and stale detection

   Each test uses a fresh DataScript connection via fixture."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.disc :as disc]
            [hive-mcp.knowledge-graph.connection :as conn]
            [clojure.java.io :as io]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-kg-fixture
  "Reset KG connection before and after each test."
  [f]
  (conn/reset-conn!)
  (f)
  (conn/reset-conn!))

(use-fixtures :each reset-kg-fixture)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn gen-path
  "Generate a unique file path for testing."
  []
  (str "/test/path/" (subs (str (java.util.UUID/randomUUID)) 0 8) ".clj"))

;; =============================================================================
;; Hash Utility Tests
;; =============================================================================

(deftest compute-hash-test
  (testing "compute-hash returns consistent SHA-256 hex string"
    (let [hash1 (disc/compute-hash "hello world")
          hash2 (disc/compute-hash "hello world")]
      (is (string? hash1))
      (is (= 64 (count hash1)))  ; SHA-256 = 32 bytes = 64 hex chars
      (is (= hash1 hash2)))))

(deftest compute-hash-different-content-test
  (testing "compute-hash returns different hashes for different content"
    (let [hash1 (disc/compute-hash "hello")
          hash2 (disc/compute-hash "world")]
      (is (not= hash1 hash2)))))

;; =============================================================================
;; add-disc! Tests
;; =============================================================================

(deftest add-disc-creates-entity-test
  (testing "add-disc! creates a new disc entity"
    (let [path (gen-path)
          eid (disc/add-disc! {:path path :content-hash "abc123"})]
      (is (number? eid))
      (is (disc/disc-exists? path)))))

(deftest add-disc-stores-all-fields-test
  (testing "add-disc! stores all provided fields"
    (let [path (gen-path)
          analyzed-at (java.util.Date.)
          _ (disc/add-disc! {:path path
                             :content-hash "hash123"
                             :analyzed-at analyzed-at
                             :git-commit "abc1234"
                             :project-id "test-project"})
          disc (disc/get-disc path)]
      (is (= path (:disc/path disc)))
      (is (= "hash123" (:disc/content-hash disc)))
      (is (= analyzed-at (:disc/analyzed-at disc)))
      (is (= "abc1234" (:disc/git-commit disc)))
      (is (= "test-project" (:disc/project-id disc))))))

(deftest add-disc-defaults-project-id-test
  (testing "add-disc! defaults project-id to 'global'"
    (let [path (gen-path)
          _ (disc/add-disc! {:path path :content-hash "test"})
          disc (disc/get-disc path)]
      (is (= "global" (:disc/project-id disc))))))

(deftest add-disc-upserts-on-same-path-test
  (testing "add-disc! upserts when path already exists"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "hash1"})
      (disc/add-disc! {:path path :content-hash "hash2"})
      (let [disc (disc/get-disc path)]
        (is (= "hash2" (:disc/content-hash disc)))))))

(deftest add-disc-rejects-nil-path-test
  (testing "add-disc! rejects nil path"
    (is (thrown? AssertionError
                 (disc/add-disc! {:path nil :content-hash "test"})))))

(deftest add-disc-rejects-empty-path-test
  (testing "add-disc! rejects empty path"
    (is (thrown? AssertionError
                 (disc/add-disc! {:path "" :content-hash "test"})))))

;; =============================================================================
;; get-disc Tests
;; =============================================================================

(deftest get-disc-returns-entity-test
  (testing "get-disc returns the disc entity for a path"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (let [disc (disc/get-disc path)]
        (is (some? disc))
        (is (= path (:disc/path disc)))))))

(deftest get-disc-returns-nil-for-nonexistent-test
  (testing "get-disc returns nil for non-existent path"
    (is (nil? (disc/get-disc "/nonexistent/path.clj")))))

;; =============================================================================
;; update-disc! Tests
;; =============================================================================

(deftest update-disc-modifies-entity-test
  (testing "update-disc! modifies the disc entity"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "old-hash"})
      (disc/update-disc! path {:disc/content-hash "new-hash"})
      (let [disc (disc/get-disc path)]
        (is (= "new-hash" (:disc/content-hash disc)))))))

(deftest update-disc-returns-updated-entity-test
  (testing "update-disc! returns the updated entity"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "hash1"})
      (let [updated (disc/update-disc! path {:disc/git-commit "commit123"})]
        (is (= "commit123" (:disc/git-commit updated)))))))

(deftest update-disc-returns-nil-for-nonexistent-test
  (testing "update-disc! returns nil for non-existent path"
    (is (nil? (disc/update-disc! "/nonexistent/path.clj" {:disc/content-hash "new"})))))

;; =============================================================================
;; remove-disc! Tests
;; =============================================================================

(deftest remove-disc-deletes-entity-test
  (testing "remove-disc! deletes the disc entity"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (is (disc/disc-exists? path))
      (disc/remove-disc! path)
      (is (not (disc/disc-exists? path))))))

(deftest remove-disc-returns-true-on-success-test
  (testing "remove-disc! returns true on success"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (is (true? (disc/remove-disc! path))))))

(deftest remove-disc-returns-nil-for-nonexistent-test
  (testing "remove-disc! returns nil for non-existent path"
    (is (nil? (disc/remove-disc! "/nonexistent/path.clj")))))

;; =============================================================================
;; get-all-discs Tests
;; =============================================================================

(deftest get-all-discs-returns-all-test
  (testing "get-all-discs returns all disc entities"
    (let [path1 (gen-path)
          path2 (gen-path)]
      (disc/add-disc! {:path path1 :content-hash "hash1"})
      (disc/add-disc! {:path path2 :content-hash "hash2"})
      (let [all (disc/get-all-discs)]
        (is (= 2 (count all)))))))

(deftest get-all-discs-filters-by-project-test
  (testing "get-all-discs filters by project-id"
    (let [path1 (gen-path)
          path2 (gen-path)]
      (disc/add-disc! {:path path1 :content-hash "hash1" :project-id "project-a"})
      (disc/add-disc! {:path path2 :content-hash "hash2" :project-id "project-b"})
      (let [project-a-discs (disc/get-all-discs :project-id "project-a")]
        (is (= 1 (count project-a-discs)))
        (is (= "project-a" (:disc/project-id (first project-a-discs))))))))

(deftest get-all-discs-empty-when-no-discs-test
  (testing "get-all-discs returns empty when no disc entities"
    (is (empty? (disc/get-all-discs)))))

;; =============================================================================
;; disc-stats Tests
;; =============================================================================

(deftest disc-stats-total-count-test
  (testing "disc-stats reports total count"
    (disc/add-disc! {:path (gen-path) :content-hash "hash1"})
    (disc/add-disc! {:path (gen-path) :content-hash "hash2"})
    (let [stats (disc/disc-stats)]
      (is (= 2 (:total stats))))))

(deftest disc-stats-by-project-test
  (testing "disc-stats groups by project"
    (disc/add-disc! {:path (gen-path) :content-hash "h1" :project-id "proj-a"})
    (disc/add-disc! {:path (gen-path) :content-hash "h2" :project-id "proj-a"})
    (disc/add-disc! {:path (gen-path) :content-hash "h3" :project-id "proj-b"})
    (let [stats (disc/disc-stats)]
      (is (= 2 (get-in stats [:by-project "proj-a"])))
      (is (= 1 (get-in stats [:by-project "proj-b"]))))))

(deftest disc-stats-empty-db-test
  (testing "disc-stats handles empty database"
    (let [stats (disc/disc-stats)]
      (is (= 0 (:total stats)))
      (is (= 0 (:stale-count stats)))
      (is (empty? (:by-project stats))))))

;; =============================================================================
;; disc-exists? Tests
;; =============================================================================

(deftest disc-exists-returns-true-for-existing-test
  (testing "disc-exists? returns true for existing path"
    (let [path (gen-path)]
      (disc/add-disc! {:path path :content-hash "test"})
      (is (true? (disc/disc-exists? path))))))

(deftest disc-exists-returns-false-for-nonexistent-test
  (testing "disc-exists? returns false for non-existent path"
    (is (false? (disc/disc-exists? "/nonexistent/path.clj")))))
