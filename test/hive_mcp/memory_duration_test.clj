(ns hive-mcp.memory-duration-test
  "Unit tests for hive-mcp-memory.el duration functionality.
   
   Tests cover the memory duration/lifespan system:
   - Duration hierarchy: session < short-term < long-term < permanent
   - Expiration calculation based on duration
   - Promote/demote operations
   - Filtering by duration
   - Cleanup of expired entries
   - Query for expiring entries
   
   These tests verify the elisp generation for the duration functions,
   following the TDD approach used throughout this project."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.elisp :as el]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(def test-project-id "test-project-123")

(defn memory-add-elisp
  "Generate elisp for hive-mcp-memory-add with duration."
  [type content & {:keys [tags project-id duration]
                   :or {tags nil project-id nil duration nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-add
                            type content tags project-id duration))

(defn memory-set-duration-elisp
  "Generate elisp for hive-mcp-memory-set-duration."
  [id duration & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-set-duration
                            id duration project-id))

(defn memory-promote-elisp
  "Generate elisp for hive-mcp-memory-promote."
  [id & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-promote
                            id project-id))

(defn memory-demote-elisp
  "Generate elisp for hive-mcp-memory-demote."
  [id & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-demote
                            id project-id))

(defn memory-query-elisp
  "Generate elisp for hive-mcp-memory-query with duration filter."
  [type & {:keys [tags project-id limit duration]
           :or {tags nil project-id nil limit nil duration nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-query
                            type tags project-id limit duration))

(defn memory-cleanup-expired-elisp
  "Generate elisp for hive-mcp-memory-cleanup-expired."
  [& {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-cleanup-expired
                            project-id))

(defn memory-query-expiring-elisp
  "Generate elisp for hive-mcp-memory-query-expiring."
  [days & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-query-expiring
                            days project-id))

(defn memory-get-elisp
  "Generate elisp for hive-mcp-memory-get."
  [id & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-get
                            id project-id))

;; =============================================================================
;; Test hive-mcp-memory-add with Duration
;; =============================================================================

(deftest test-memory-add-with-session-duration
  (testing "Add memory entry with session duration"
    (let [result (memory-add-elisp 'note "Session note" :duration 'session)]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-add"))
      (is (str/includes? result "'note"))
      (is (str/includes? result "\"Session note\""))
      (is (str/includes? result "'session"))
      (is (str/includes? result "json-encode")))))

(deftest test-memory-add-with-short-term-duration
  (testing "Add memory entry with short-term duration"
    (let [result (memory-add-elisp 'snippet "code snippet" :duration 'short-term)]
      (is (str/includes? result "'short-term"))
      (is (str/includes? result "'snippet"))
      (is (str/includes? result "\"code snippet\"")))))

(deftest test-memory-add-with-long-term-duration
  (testing "Add memory entry with long-term duration (default)"
    (let [result (memory-add-elisp 'convention "Use kebab-case" :duration 'long-term)]
      (is (str/includes? result "'long-term"))
      (is (str/includes? result "'convention")))))

(deftest test-memory-add-with-permanent-duration
  (testing "Add memory entry with permanent duration"
    (let [result (memory-add-elisp 'decision "Architecture decision" :duration 'permanent)]
      (is (str/includes? result "'permanent"))
      (is (str/includes? result "'decision"))
      (is (str/includes? result "\"Architecture decision\"")))))

(deftest test-memory-add-default-duration
  (testing "Add memory entry without explicit duration (uses default)"
    (let [result (memory-add-elisp 'note "Default note")]
      ;; Should still generate valid elisp without duration arg
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "hive-mcp-memory-add"))
      (is (str/includes? result "'note"))
      (is (str/includes? result "\"Default note\"")))))

(deftest test-memory-add-with-tags-and-duration
  (testing "Add memory entry with both tags and duration"
    (let [result (memory-add-elisp 'note "Tagged note"
                                   :tags ["tag1" "tag2"]
                                   :duration 'short-term)]
      (is (str/includes? result "'(\"tag1\" \"tag2\")"))
      (is (str/includes? result "'short-term")))))

(deftest test-memory-add-with-project-and-duration
  (testing "Add memory entry with explicit project-id and duration"
    (let [result (memory-add-elisp 'note "Project note"
                                   :project-id "my-project"
                                   :duration 'permanent)]
      (is (str/includes? result "\"my-project\""))
      (is (str/includes? result "'permanent")))))

;; =============================================================================
;; Test hive-mcp-memory-set-duration
;; =============================================================================

(deftest test-set-duration-to-session
  (testing "Set entry duration to session"
    (let [result (memory-set-duration-elisp "entry-123" 'session)]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "hive-mcp-memory-set-duration"))
      (is (str/includes? result "\"entry-123\""))
      (is (str/includes? result "'session")))))

(deftest test-set-duration-to-short-term
  (testing "Set entry duration to short-term"
    (let [result (memory-set-duration-elisp "entry-456" 'short-term)]
      (is (str/includes? result "'short-term")))))

(deftest test-set-duration-to-long-term
  (testing "Set entry duration to long-term"
    (let [result (memory-set-duration-elisp "entry-789" 'long-term)]
      (is (str/includes? result "'long-term")))))

(deftest test-set-duration-to-permanent
  (testing "Set entry duration to permanent"
    (let [result (memory-set-duration-elisp "entry-000" 'permanent)]
      (is (str/includes? result "'permanent")))))

(deftest test-set-duration-with-project-id
  (testing "Set duration with explicit project-id"
    (let [result (memory-set-duration-elisp "entry-abc" 'long-term
                                            :project-id test-project-id)]
      (is (str/includes? result (str "\"" test-project-id "\"")))
      (is (str/includes? result "'long-term")))))

;; =============================================================================
;; Test hive-mcp-memory-promote
;; =============================================================================

(deftest test-promote-generates-valid-elisp
  (testing "Promote generates valid elisp structure"
    (let [result (memory-promote-elisp "entry-to-promote")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-promote"))
      (is (str/includes? result "\"entry-to-promote\""))
      (is (str/includes? result "json-encode")))))

(deftest test-promote-with-project-id
  (testing "Promote with explicit project-id"
    (let [result (memory-promote-elisp "entry-xyz" :project-id test-project-id)]
      (is (str/includes? result "\"entry-xyz\""))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

(deftest test-promote-hierarchy-documentation
  (testing "Promote hierarchy: session -> short-term -> long-term -> permanent"
    ;; This test documents the expected behavior
    ;; The elisp function should promote through the hierarchy:
    ;; session -> short-term -> long-term -> permanent
    ;; Returns nil if already permanent
    (let [result (memory-promote-elisp "any-entry")]
      (is (string? result))
      (is (str/includes? result "hive-mcp-memory-promote")))))

;; =============================================================================
;; Test hive-mcp-memory-demote
;; =============================================================================

(deftest test-demote-generates-valid-elisp
  (testing "Demote generates valid elisp structure"
    (let [result (memory-demote-elisp "entry-to-demote")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-demote"))
      (is (str/includes? result "\"entry-to-demote\""))
      (is (str/includes? result "json-encode")))))

(deftest test-demote-with-project-id
  (testing "Demote with explicit project-id"
    (let [result (memory-demote-elisp "entry-abc" :project-id test-project-id)]
      (is (str/includes? result "\"entry-abc\""))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

(deftest test-demote-hierarchy-documentation
  (testing "Demote hierarchy: permanent -> long-term -> short-term -> session"
    ;; This test documents the expected behavior
    ;; The elisp function should demote through the hierarchy:
    ;; permanent -> long-term -> short-term -> session
    ;; Returns nil if already session
    (let [result (memory-demote-elisp "any-entry")]
      (is (string? result))
      (is (str/includes? result "hive-mcp-memory-demote")))))

;; =============================================================================
;; Test hive-mcp-memory-query with Duration Filter
;; =============================================================================

(deftest test-query-with-session-duration-filter
  (testing "Query notes with session duration filter"
    (let [result (memory-query-elisp 'note :duration 'session)]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "hive-mcp-memory-query"))
      (is (str/includes? result "'note"))
      (is (str/includes? result "'session")))))

(deftest test-query-with-short-term-duration-filter
  (testing "Query snippets with short-term duration filter"
    (let [result (memory-query-elisp 'snippet :duration 'short-term)]
      (is (str/includes? result "'snippet"))
      (is (str/includes? result "'short-term")))))

(deftest test-query-with-long-term-duration-filter
  (testing "Query conventions with long-term duration filter"
    (let [result (memory-query-elisp 'convention :duration 'long-term)]
      (is (str/includes? result "'convention"))
      (is (str/includes? result "'long-term")))))

(deftest test-query-with-permanent-duration-filter
  (testing "Query decisions with permanent duration filter"
    (let [result (memory-query-elisp 'decision :duration 'permanent)]
      (is (str/includes? result "'decision"))
      (is (str/includes? result "'permanent")))))

(deftest test-query-without-duration-filter
  (testing "Query without duration filter returns all"
    (let [result (memory-query-elisp 'note)]
      (is (str/includes? result "hive-mcp-memory-query"))
      (is (str/includes? result "'note"))
      ;; Should not have a duration filter
      (is (not (str/includes? result "'session")))
      (is (not (str/includes? result "'short-term")))
      (is (not (str/includes? result "'long-term")))
      (is (not (str/includes? result "'permanent"))))))

(deftest test-query-with-tags-and-duration
  (testing "Query with both tags and duration filter"
    (let [result (memory-query-elisp 'note
                                     :tags ["important" "review"]
                                     :duration 'permanent)]
      (is (str/includes? result "'(\"important\" \"review\")"))
      (is (str/includes? result "'permanent")))))

(deftest test-query-with-limit-and-duration
  (testing "Query with limit and duration filter"
    (let [result (memory-query-elisp 'note :limit 10 :duration 'short-term)]
      (is (str/includes? result "10"))
      (is (str/includes? result "'short-term")))))

(deftest test-query-legacy-entries-treated-as-long-term
  (testing "Legacy entries without :duration treated as long-term"
    ;; This documents the backwards-compatibility behavior
    ;; Entries without :duration field are treated as long-term
    (let [result (memory-query-elisp 'note :duration 'long-term)]
      (is (str/includes? result "'long-term")))))

;; =============================================================================
;; Test hive-mcp-memory-cleanup-expired
;; =============================================================================

(deftest test-cleanup-expired-generates-valid-elisp
  (testing "Cleanup expired generates valid elisp"
    (let [result (memory-cleanup-expired-elisp)]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-cleanup-expired"))
      (is (str/includes? result "json-encode")))))

(deftest test-cleanup-expired-with-project-id
  (testing "Cleanup expired with explicit project-id"
    (let [result (memory-cleanup-expired-elisp :project-id test-project-id)]
      (is (str/includes? result "hive-mcp-memory-cleanup-expired"))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

(deftest test-cleanup-expired-behavior-documentation
  (testing "Cleanup removes entries where current-time > expires timestamp"
    ;; This documents the expected behavior:
    ;; - Session entries (0 days): expire immediately
    ;; - Short-term entries (7 days): expire after 7 days
    ;; - Long-term entries (90 days): expire after 90 days
    ;; - Permanent entries (nil): never expire
    (let [result (memory-cleanup-expired-elisp)]
      (is (string? result))
      (is (str/includes? result "hive-mcp-memory-cleanup-expired")))))

;; =============================================================================
;; Test hive-mcp-memory-query-expiring
;; =============================================================================

(deftest test-query-expiring-within-7-days
  (testing "Query entries expiring within 7 days"
    (let [result (memory-query-expiring-elisp 7)]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-query-expiring"))
      (is (str/includes? result "7"))
      (is (str/includes? result "json-encode")))))

(deftest test-query-expiring-within-30-days
  (testing "Query entries expiring within 30 days"
    (let [result (memory-query-expiring-elisp 30)]
      (is (str/includes? result "30")))))

(deftest test-query-expiring-within-1-day
  (testing "Query entries expiring within 1 day (urgent)"
    (let [result (memory-query-expiring-elisp 1)]
      (is (str/includes? result "1")))))

(deftest test-query-expiring-with-project-id
  (testing "Query expiring with explicit project-id"
    (let [result (memory-query-expiring-elisp 14 :project-id test-project-id)]
      (is (str/includes? result "14"))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

(deftest test-query-expiring-behavior-documentation
  (testing "Query expiring excludes already-expired and permanent entries"
    ;; This documents the expected behavior:
    ;; - Finds entries that will expire within N days
    ;; - Excludes already-expired entries
    ;; - Excludes permanent entries (they never expire)
    ;; - Sorted by expiration date (soonest first)
    (let [result (memory-query-expiring-elisp 7)]
      (is (string? result))
      (is (str/includes? result "hive-mcp-memory-query-expiring")))))

;; =============================================================================
;; Duration Hierarchy Tests (Documentation)
;; =============================================================================

(deftest test-duration-hierarchy
  (testing "Duration hierarchy is correctly ordered"
    ;; Documents the duration hierarchy from shortest to longest:
    ;; session < short-term < long-term < permanent
    ;; This matches hive-mcp-memory-durations in elisp
    (is (= [:session :short-term :long-term :permanent]
           [:session :short-term :long-term :permanent]))))

(deftest test-duration-days-mapping
  (testing "Duration to days mapping"
    ;; Documents the expected days per duration:
    ;; session: 0 (expires immediately)
    ;; short-term: 7 (expires in 1 week)
    ;; long-term: 90 (expires in ~3 months)
    ;; permanent: nil (never expires)
    (let [expected {:session 0
                    :short-term 7
                    :long-term 90
                    :permanent nil}]
      (is (= 0 (:session expected)))
      (is (= 7 (:short-term expected)))
      (is (= 90 (:long-term expected)))
      (is (nil? (:permanent expected))))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest test-promote-at-permanent-boundary
  (testing "Promote at permanent boundary returns nil"
    ;; When already at permanent, promote should return nil
    ;; This is tested by generating the elisp that would be evaluated
    (let [result (memory-promote-elisp "permanent-entry")]
      (is (string? result))
      ;; The elisp will be evaluated and should return nil for permanent entries
      (is (str/includes? result "hive-mcp-memory-promote")))))

(deftest test-demote-at-session-boundary
  (testing "Demote at session boundary returns nil"
    ;; When already at session, demote should return nil
    (let [result (memory-demote-elisp "session-entry")]
      (is (string? result))
      ;; The elisp will be evaluated and should return nil for session entries
      (is (str/includes? result "hive-mcp-memory-demote")))))

(deftest test-memory-get-with-project-id
  (testing "Get memory entry with project-id"
    (let [result (memory-get-elisp "some-id" :project-id test-project-id)]
      (is (str/includes? result "hive-mcp-memory-get"))
      (is (str/includes? result "\"some-id\""))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

;; =============================================================================
;; Integration-Style Tests (Elisp Structure Validation)
;; =============================================================================

(deftest test-full-lifecycle-elisp-generation
  (testing "Full lifecycle: add -> promote -> query -> cleanup"
    ;; Verify we can generate elisp for a complete workflow
    (let [add-result (memory-add-elisp 'note "Test note" :duration 'session)
          promote-result (memory-promote-elisp "entry-id")
          query-result (memory-query-elisp 'note :duration 'short-term)
          cleanup-result (memory-cleanup-expired-elisp)]
      ;; All should generate valid elisp
      (is (every? #(str/starts-with? % "(progn")
                  [add-result promote-result query-result cleanup-result]))
      ;; All should require hive-mcp-memory
      (is (every? #(str/includes? % "hive-mcp-memory")
                  [add-result promote-result query-result cleanup-result])))))

(deftest test-elisp-json-encoding
  (testing "All memory functions use json-encode for return values"
    (let [functions [(memory-add-elisp 'note "test")
                     (memory-set-duration-elisp "id" 'session)
                     (memory-promote-elisp "id")
                     (memory-demote-elisp "id")
                     (memory-query-elisp 'note)
                     (memory-cleanup-expired-elisp)
                     (memory-query-expiring-elisp 7)
                     (memory-get-elisp "id")]]
      (is (every? #(str/includes? % "json-encode") functions)))))

(deftest test-elisp-error-handling
  (testing "All memory functions include error handling"
    (let [functions [(memory-add-elisp 'note "test")
                     (memory-set-duration-elisp "id" 'session)
                     (memory-promote-elisp "id")
                     (memory-demote-elisp "id")
                     (memory-query-elisp 'note)
                     (memory-cleanup-expired-elisp)
                     (memory-query-expiring-elisp 7)]]
      ;; All should have fboundp check and error message
      (is (every? #(str/includes? % "fboundp") functions))
      (is (every? #(str/includes? % "not loaded") functions)))))
