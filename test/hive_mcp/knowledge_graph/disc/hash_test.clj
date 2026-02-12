(ns hive-mcp.knowledge-graph.disc.hash-test
  "Unit tests for disc.hash pure hash computation functions.

   Tests cover:
   - compute-hash: SHA-256 consistency, uniqueness
   - file-content-hash: file reading, existence checks, error handling"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.disc.hash :as hash]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; compute-hash Tests
;; =============================================================================

(deftest compute-hash-returns-consistent-hex-string-test
  (testing "compute-hash returns consistent SHA-256 hex string"
    (let [hash1 (hash/compute-hash "hello world")
          hash2 (hash/compute-hash "hello world")]
      (is (string? hash1))
      (is (= 64 (count hash1)))  ; SHA-256 = 32 bytes = 64 hex chars
      (is (= hash1 hash2)))))

(deftest compute-hash-different-content-test
  (testing "compute-hash returns different hashes for different content"
    (let [hash1 (hash/compute-hash "hello")
          hash2 (hash/compute-hash "world")]
      (is (not= hash1 hash2)))))

(deftest compute-hash-empty-string-test
  (testing "compute-hash handles empty string"
    (let [h (hash/compute-hash "")]
      (is (string? h))
      (is (= 64 (count h))))))

(deftest compute-hash-nil-coerced-test
  (testing "compute-hash coerces nil to string via str"
    (let [h (hash/compute-hash nil)]
      (is (string? h))
      (is (= 64 (count h)))
      ;; nil coerced to "" via (str nil) = ""
      (is (= h (hash/compute-hash ""))))))

(deftest compute-hash-known-value-test
  (testing "compute-hash produces known SHA-256 for 'test'"
    ;; SHA-256 of "test" = 9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08
    (is (= "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
           (hash/compute-hash "test")))))

;; =============================================================================
;; file-content-hash Tests
;; =============================================================================

(deftest file-content-hash-nonexistent-file-test
  (testing "file-content-hash returns {:exists? false} for non-existent file"
    (let [result (hash/file-content-hash "/nonexistent/path/to/file.clj")]
      (is (false? (:exists? result)))
      (is (nil? (:hash result))))))

(deftest file-content-hash-existing-file-test
  (testing "file-content-hash returns hash for existing file"
    ;; Use this very test file as an existing file
    (let [result (hash/file-content-hash "deps.edn")]
      (is (true? (:exists? result)))
      (is (string? (:hash result)))
      (is (= 64 (count (:hash result)))))))

(deftest file-content-hash-consistent-test
  (testing "file-content-hash returns same hash for same file"
    (let [r1 (hash/file-content-hash "deps.edn")
          r2 (hash/file-content-hash "deps.edn")]
      (is (= (:hash r1) (:hash r2))))))
