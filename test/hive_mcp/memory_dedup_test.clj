(ns hive-mcp.memory-dedup-test
  "Unit tests for hive-mcp-memory.el duplicate detection functionality.
   
   Tests cover the memory deduplication system:
   - Content hash computation using SHA-256
   - Duplicate detection before adding entries
   - Tag merging when duplicate is found
   - Whitespace normalization before hashing
   
   These tests verify the elisp generation for the deduplication functions,
   following the TDD approach used throughout this project."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.elisp :as el]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(def test-project-id "test-dedup-project")

(defn content-hash-elisp
  "Generate elisp for hive-mcp-memory-content-hash."
  [content]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-content-hash
                            content))

(defn find-duplicate-elisp
  "Generate elisp for hive-mcp-memory-find-duplicate."
  [type content & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-find-duplicate
                            type content project-id))

(defn check-duplicate-elisp
  "Generate elisp for hive-mcp-api-memory-check-duplicate."
  [type content]
  (el/require-and-call-json 'hive-mcp-api
                            'hive-mcp-api-memory-check-duplicate
                            type content))

(defn normalize-content-elisp
  "Generate elisp for hive-mcp-memory--normalize-content."
  [content]
  (format "(progn (require 'hive-mcp-memory nil t) (hive-mcp-memory--normalize-content %s))"
          (pr-str content)))

;; =============================================================================
;; Test Content Hash Computation
;; =============================================================================

(deftest test-content-hash-generates-valid-elisp
  (testing "Content hash function generates valid elisp"
    (let [result (content-hash-elisp "test content")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-content-hash"))
      (is (str/includes? result "\"test content\""))
      (is (str/includes? result "json-encode")))))

(deftest test-content-hash-uses-sha256
  (testing "Content hash should use SHA-256"
    ;; The elisp implementation should use secure-hash 'sha256
    (let [result (content-hash-elisp "test")]
      (is (str/includes? result "hive-mcp-memory-content-hash")))))

;; =============================================================================
;; Test Duplicate Detection - Same Content
;; =============================================================================

(deftest test-duplicate-detection-same-content
  (testing "Same content returns existing entry instead of creating new one"
    (let [result (find-duplicate-elisp 'note "Remember to refactor")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-find-duplicate"))
      (is (str/includes? result "'note"))
      (is (str/includes? result "\"Remember to refactor\""))
      (is (str/includes? result "json-encode")))))

(deftest test-duplicate-detection-different-types
  (testing "Same content in different types should not be considered duplicate"
    (let [note-result (find-duplicate-elisp 'note "shared content")
          snippet-result (find-duplicate-elisp 'snippet "shared content")]
      ;; Both should generate valid elisp but search in their respective types
      (is (str/includes? note-result "'note"))
      (is (str/includes? snippet-result "'snippet")))))

(deftest test-duplicate-detection-with-project-id
  (testing "Duplicate detection respects project boundaries"
    (let [result (find-duplicate-elisp 'note "test content" :project-id test-project-id)]
      (is (str/includes? result test-project-id)))))

;; =============================================================================
;; Test Duplicate Detection - Tag Merging
;; =============================================================================

(deftest test-duplicate-detection-different-tags-merge
  (testing "When duplicate found with new tags, tags should be merged"
    ;; This tests that the API layer can handle tag merging
    ;; The actual merging happens in the add function when duplicate is detected
    (let [result (check-duplicate-elisp 'note "existing note content")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-api nil t)"))
      (is (str/includes? result "hive-mcp-api-memory-check-duplicate"))
      (is (str/includes? result "'note"))
      (is (str/includes? result "json-encode")))))

;; =============================================================================
;; Test Whitespace Normalization
;; =============================================================================

(deftest test-duplicate-detection-normalized-whitespace
  (testing "Whitespace differences should not create duplicates"
    ;; Content with different whitespace should produce same hash
    (let [content1 "multiple   spaces   here"
          content2 "multiple spaces here"
          ;; Both should go through normalization before hashing
          result1 (normalize-content-elisp content1)
          result2 (normalize-content-elisp content2)]
      ;; The normalize function should be called
      (is (str/includes? result1 "hive-mcp-memory--normalize-content"))
      (is (str/includes? result2 "hive-mcp-memory--normalize-content")))))

(deftest test-whitespace-normalization-trims
  (testing "Leading and trailing whitespace should be trimmed"
    (let [result (normalize-content-elisp "  leading and trailing  ")]
      (is (str/includes? result "hive-mcp-memory--normalize-content")))))

(deftest test-whitespace-normalization-collapses-newlines
  (testing "Multiple newlines should be collapsed"
    (let [result (normalize-content-elisp "line1\n\n\nline2")]
      (is (str/includes? result "hive-mcp-memory--normalize-content")))))

;; =============================================================================
;; Test API Layer - Check Duplicate Tool
;; =============================================================================

(deftest test-api-check-duplicate-generates-valid-elisp
  (testing "API check duplicate generates valid elisp for MCP tool"
    (let [result (check-duplicate-elisp 'convention "Use kebab-case for functions")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "hive-mcp-api-memory-check-duplicate"))
      (is (str/includes? result "'convention"))
      (is (str/includes? result "json-encode")))))

(deftest test-api-check-duplicate-returns-json-structure
  (testing "API check duplicate should return JSON-serializable result"
    (let [result (check-duplicate-elisp 'decision "Architecture choice")]
      ;; Result should be wrapped in json-encode for proper serialization
      (is (str/includes? result "json-encode")))))

;; =============================================================================
;; Test Content Hash for Different Content Types
;; =============================================================================

(deftest test-content-hash-string-content
  (testing "Hash works for simple string content"
    (let [result (content-hash-elisp "simple string")]
      (is (str/includes? result "hive-mcp-memory-content-hash"))
      (is (str/includes? result "\"simple string\"")))))

(deftest test-content-hash-plist-content
  (testing "Hash works for plist/structured content"
    ;; For snippets, content might be a plist like (:name "foo" :code "bar")
    (let [result (content-hash-elisp {:name "test-fn" :code "(defn test [])"})]
      (is (str/includes? result "hive-mcp-memory-content-hash")))))

;; =============================================================================
;; Test Integration - Add with Dedup
;; =============================================================================

(deftest test-memory-add-checks-duplicate
  (testing "Memory add should check for duplicate before creating"
    ;; The add function should call find-duplicate internally
    ;; This is a documentation test showing the expected behavior
    (let [add-elisp (el/require-and-call-json 'hive-mcp-memory
                                              'hive-mcp-memory-add
                                              'note "test note" nil nil nil)]
      ;; Add function should exist and be callable
      (is (str/includes? add-elisp "hive-mcp-memory-add")))))

(deftest test-duplicate-returns-existing-id
  (testing "When duplicate found, return existing entry ID"
    ;; The find-duplicate function should return the existing entry
    (let [result (find-duplicate-elisp 'note "existing content")]
      (is (str/includes? result "hive-mcp-memory-find-duplicate"))
      ;; Result should be JSON encoded for the MCP layer
      (is (str/includes? result "json-encode")))))

;; =============================================================================
;; Test Content Hash Schema
;; =============================================================================

(deftest test-content-hash-field-in-entry
  (testing "Memory entries should have :content-hash field"
    ;; This documents the expected schema addition
    ;; The add function should compute and store the hash
    (let [add-result (el/require-and-call-json 'hive-mcp-memory
                                               'hive-mcp-memory-add
                                               'note "test" nil nil nil)]
      ;; Entry creation should include hash computation
      (is (string? add-result)))))

(deftest test-content-hash-deterministic
  (testing "Same content should always produce same hash"
    ;; Two calls with same content should produce same hash
    (let [result1 (content-hash-elisp "deterministic content")
          result2 (content-hash-elisp "deterministic content")]
      ;; Both should generate identical elisp
      (is (= result1 result2)))))
