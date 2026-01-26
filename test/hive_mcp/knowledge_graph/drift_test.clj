(ns hive-mcp.knowledge-graph.drift-test
  "Unit tests for Knowledge Graph drift detection.

   Tests cover:
   - compute-source-hash stability
   - detect-drift with various scenarios
   - find-drifted-entries batch filtering
   - grounding-info creation"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.drift :as drift]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:dynamic *temp-file* nil)

(defn temp-file-fixture
  "Create and cleanup temp file for tests that need filesystem."
  [f]
  (let [temp (java.io.File/createTempFile "drift-test" ".txt")]
    (try
      (binding [*temp-file* temp]
        (f))
      (finally
        (.delete temp)))))

(use-fixtures :each temp-file-fixture)

;; =============================================================================
;; compute-source-hash Tests
;; =============================================================================

(deftest compute-source-hash-stability-test
  (testing "Same content produces same hash"
    (let [content "hello world"
          hash1 (drift/compute-source-hash content)
          hash2 (drift/compute-source-hash content)]
      (is (= hash1 hash2))))

  (testing "Different content produces different hash"
    (let [hash1 (drift/compute-source-hash "hello")
          hash2 (drift/compute-source-hash "world")]
      (is (not= hash1 hash2))))

  (testing "nil content returns nil"
    (is (nil? (drift/compute-source-hash nil))))

  (testing "Hash is lowercase hex string"
    (let [hash (drift/compute-source-hash "test")]
      (is (string? hash))
      (is (re-matches #"^[0-9a-f]+$" hash))
      (is (= 64 (count hash))))))  ;; SHA-256 = 64 hex chars

;; =============================================================================
;; detect-drift Tests
;; =============================================================================

(deftest detect-drift-no-grounding-test
  (testing "Entry without grounding info returns no-grounding"
    (let [entry {:some "data"}
          result (drift/detect-drift entry)]
      (is (false? (:drifted? result)))
      (is (= :no-grounding (:reason result))))))

(deftest detect-drift-source-missing-test
  (testing "Entry with non-existent file returns source-missing"
    (let [entry {:knowledge/grounded-from "/nonexistent/path/file.clj"
                 :knowledge/source-hash "abc123"}
          result (drift/detect-drift entry)]
      (is (true? (:drifted? result)))
      (is (= :source-missing (:reason result)))
      (is (nil? (:current-hash result))))))

(deftest detect-drift-hash-match-test
  (testing "Entry with matching hash is not drifted"
    (spit *temp-file* "original content")
    (let [hash (drift/compute-source-hash "original content")
          entry {:knowledge/grounded-from (.getAbsolutePath *temp-file*)
                 :knowledge/source-hash hash}
          result (drift/detect-drift entry)]
      (is (false? (:drifted? result)))
      (is (nil? (:reason result)))
      (is (= hash (:current-hash result))))))

(deftest detect-drift-hash-mismatch-test
  (testing "Entry with changed file content is drifted"
    (spit *temp-file* "original content")
    (let [old-hash (drift/compute-source-hash "original content")
          entry {:knowledge/grounded-from (.getAbsolutePath *temp-file*)
                 :knowledge/source-hash old-hash}]
      ;; Change file content
      (spit *temp-file* "modified content")
      (let [result (drift/detect-drift entry)]
        (is (true? (:drifted? result)))
        (is (= :hash-mismatch (:reason result)))
        (is (= old-hash (:stored-hash result)))
        (is (not= old-hash (:current-hash result)))))))

;; =============================================================================
;; find-drifted-entries Tests
;; =============================================================================

(deftest find-drifted-entries-test
  (testing "Returns only drifted entries"
    (spit *temp-file* "stable content")
    (let [stable-hash (drift/compute-source-hash "stable content")
          entries [{:id "1" :knowledge/grounded-from (.getAbsolutePath *temp-file*)
                    :knowledge/source-hash stable-hash}
                   {:id "2" :knowledge/grounded-from "/nonexistent/path.clj"
                    :knowledge/source-hash "old-hash"}
                   {:id "3"}]  ;; No grounding
          drifted (drift/find-drifted-entries entries)]
      ;; Only entry 2 should be drifted (source-missing)
      (is (= 1 (count drifted)))
      (is (= "2" (get-in (first drifted) [:entry :id]))))))

(deftest find-drifted-entries-empty-test
  (testing "Returns empty for no entries"
    (is (empty? (drift/find-drifted-entries []))))

  (testing "Returns empty when nothing drifted"
    (spit *temp-file* "content")
    (let [hash (drift/compute-source-hash "content")
          entries [{:knowledge/grounded-from (.getAbsolutePath *temp-file*)
                    :knowledge/source-hash hash}]]
      (is (empty? (drift/find-drifted-entries entries))))))

;; =============================================================================
;; grounding-info Tests
;; =============================================================================

(deftest grounding-info-from-file-test
  (testing "Creates grounding info from file path"
    (spit *temp-file* "file content")
    (let [info (drift/grounding-info (.getAbsolutePath *temp-file*))]
      (is (string? (:knowledge/source-hash info)))
      (is (= (.getAbsolutePath *temp-file*) (:knowledge/grounded-from info)))
      (is (inst? (:knowledge/grounded-at info))))))

(deftest grounding-info-with-content-test
  (testing "Creates grounding info with provided content"
    (let [content "provided content"
          info (drift/grounding-info "/some/path" content)]
      (is (= (drift/compute-source-hash content) (:knowledge/source-hash info)))
      (is (= "/some/path" (:knowledge/grounded-from info))))))

(deftest grounding-info-nonexistent-file-test
  (testing "Returns nil for non-existent file without content"
    (is (nil? (drift/grounding-info "/nonexistent/file.clj")))))
