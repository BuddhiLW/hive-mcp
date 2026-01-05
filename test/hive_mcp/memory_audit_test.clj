(ns hive-mcp.memory-audit-test
  "Unit tests for memory audit log functionality.
   
   Tests cover the memory access and helpfulness tracking system:
   - Access logging: increment count, update last-accessed timestamp
   - Helpfulness feedback: mark helpful/unhelpful
   - Helpfulness ratio calculation
   
   These tests verify the elisp generation for the audit functions,
   following the TDD approach used throughout this project.
   
   Schema additions to memory entries:
   - :access-count    integer (default 0)
   - :last-accessed   timestamp (nil initially)
   - :helpful-count   integer (default 0)
   - :unhelpful-count integer (default 0)
   
   Cleanup integration (documented, not implemented):
   - Low helpfulness ratio (< 0.3) -> candidate for demotion
   - Zero access after 30 days -> candidate for cleanup"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.elisp :as el]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(def test-project-id "test-project-audit-123")

(defn memory-log-access-elisp
  "Generate elisp for hive-mcp-memory-log-access."
  [id & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-log-access
                            id project-id))

(defn memory-mark-helpful-elisp
  "Generate elisp for hive-mcp-memory-mark-helpful."
  [id & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-mark-helpful
                            id project-id))

(defn memory-mark-unhelpful-elisp
  "Generate elisp for hive-mcp-memory-mark-unhelpful."
  [id & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-mark-unhelpful
                            id project-id))

(defn memory-helpfulness-ratio-elisp
  "Generate elisp for hive-mcp-memory-helpfulness-ratio."
  [id & {:keys [project-id] :or {project-id nil}}]
  (el/require-and-call-json 'hive-mcp-memory
                            'hive-mcp-memory-helpfulness-ratio
                            id project-id))

;; =============================================================================
;; Test hive-mcp-memory-log-access
;; =============================================================================

(deftest test-log-access-generates-valid-elisp
  (testing "Log access generates valid elisp structure"
    (let [result (memory-log-access-elisp "entry-123")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-log-access"))
      (is (str/includes? result "\"entry-123\""))
      (is (str/includes? result "json-encode")))))

(deftest test-log-access-with-project-id
  (testing "Log access with explicit project-id"
    (let [result (memory-log-access-elisp "entry-456" :project-id test-project-id)]
      (is (str/includes? result "\"entry-456\""))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

(deftest test-log-access-increments-count
  (testing "Log access increments access-count (behavior documentation)"
    ;; This test documents the expected behavior:
    ;; - access-count starts at 0 for new entries
    ;; - Each call to log-access increments access-count by 1
    ;; - Returns the updated entry with new access-count
    (let [result (memory-log-access-elisp "entry-789")]
      (is (str/includes? result "hive-mcp-memory-log-access"))
      ;; The elisp function should:
      ;; 1. Find entry by ID
      ;; 2. Increment :access-count (or set to 1 if nil)
      ;; 3. Update :last-accessed to current timestamp
      ;; 4. Return updated entry
      )))

(deftest test-log-access-updates-timestamp
  (testing "Log access updates last-accessed timestamp (behavior documentation)"
    ;; This test documents the expected behavior:
    ;; - last-accessed starts as nil for new entries
    ;; - Each call to log-access sets last-accessed to current time
    (let [result (memory-log-access-elisp "entry-abc")]
      (is (str/includes? result "hive-mcp-memory-log-access"))
      ;; The elisp function should update :last-accessed with
      ;; (hive-mcp-memory--timestamp)
      )))

;; =============================================================================
;; Test hive-mcp-memory-mark-helpful
;; =============================================================================

(deftest test-mark-helpful-generates-valid-elisp
  (testing "Mark helpful generates valid elisp structure"
    (let [result (memory-mark-helpful-elisp "entry-123")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-mark-helpful"))
      (is (str/includes? result "\"entry-123\""))
      (is (str/includes? result "json-encode")))))

(deftest test-mark-helpful-with-project-id
  (testing "Mark helpful with explicit project-id"
    (let [result (memory-mark-helpful-elisp "entry-456" :project-id test-project-id)]
      (is (str/includes? result "\"entry-456\""))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

(deftest test-mark-helpful-increments
  (testing "Mark helpful increments helpful-count (behavior documentation)"
    ;; This test documents the expected behavior:
    ;; - helpful-count starts at 0 for new entries
    ;; - Each call to mark-helpful increments helpful-count by 1
    ;; - Returns the updated entry with new helpful-count
    (let [result (memory-mark-helpful-elisp "entry-789")]
      (is (str/includes? result "hive-mcp-memory-mark-helpful"))
      ;; The elisp function should:
      ;; 1. Find entry by ID
      ;; 2. Increment :helpful-count (or set to 1 if nil)
      ;; 3. Return updated entry
      )))

;; =============================================================================
;; Test hive-mcp-memory-mark-unhelpful
;; =============================================================================

(deftest test-mark-unhelpful-generates-valid-elisp
  (testing "Mark unhelpful generates valid elisp structure"
    (let [result (memory-mark-unhelpful-elisp "entry-123")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-mark-unhelpful"))
      (is (str/includes? result "\"entry-123\""))
      (is (str/includes? result "json-encode")))))

(deftest test-mark-unhelpful-with-project-id
  (testing "Mark unhelpful with explicit project-id"
    (let [result (memory-mark-unhelpful-elisp "entry-456" :project-id test-project-id)]
      (is (str/includes? result "\"entry-456\""))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

(deftest test-mark-unhelpful-increments
  (testing "Mark unhelpful increments unhelpful-count (behavior documentation)"
    ;; This test documents the expected behavior:
    ;; - unhelpful-count starts at 0 for new entries
    ;; - Each call to mark-unhelpful increments unhelpful-count by 1
    ;; - Returns the updated entry with new unhelpful-count
    (let [result (memory-mark-unhelpful-elisp "entry-789")]
      (is (str/includes? result "hive-mcp-memory-mark-unhelpful"))
      ;; The elisp function should:
      ;; 1. Find entry by ID
      ;; 2. Increment :unhelpful-count (or set to 1 if nil)
      ;; 3. Return updated entry
      )))

;; =============================================================================
;; Test hive-mcp-memory-helpfulness-ratio
;; =============================================================================

(deftest test-helpfulness-ratio-generates-valid-elisp
  (testing "Helpfulness ratio generates valid elisp structure"
    (let [result (memory-helpfulness-ratio-elisp "entry-123")]
      (is (str/starts-with? result "(progn"))
      (is (str/includes? result "(require 'hive-mcp-memory nil t)"))
      (is (str/includes? result "hive-mcp-memory-helpfulness-ratio"))
      (is (str/includes? result "\"entry-123\""))
      (is (str/includes? result "json-encode")))))

(deftest test-helpfulness-ratio-with-project-id
  (testing "Helpfulness ratio with explicit project-id"
    (let [result (memory-helpfulness-ratio-elisp "entry-456" :project-id test-project-id)]
      (is (str/includes? result "\"entry-456\""))
      (is (str/includes? result (str "\"" test-project-id "\""))))))

(deftest test-helpfulness-ratio-calculation
  (testing "Helpfulness ratio calculation (behavior documentation)"
    ;; This test documents the expected behavior:
    ;; ratio = helpful-count / (helpful-count + unhelpful-count)
    ;; Example: 3 helpful, 1 unhelpful => 3 / 4 = 0.75
    (let [result (memory-helpfulness-ratio-elisp "entry-789")]
      (is (str/includes? result "hive-mcp-memory-helpfulness-ratio"))
      ;; The elisp function should:
      ;; 1. Find entry by ID
      ;; 2. Get helpful-count and unhelpful-count (default 0)
      ;; 3. Calculate ratio = helpful / (helpful + unhelpful)
      ;; 4. Return ratio as float, or nil if no feedback
      )))

(deftest test-helpfulness-ratio-no-feedback
  (testing "Helpfulness ratio with no feedback returns nil (behavior documentation)"
    ;; When both helpful-count and unhelpful-count are 0:
    ;; - Cannot calculate ratio (division by zero)
    ;; - Should return nil to indicate no feedback yet
    (let [result (memory-helpfulness-ratio-elisp "entry-new")]
      (is (str/includes? result "hive-mcp-memory-helpfulness-ratio"))
      ;; The elisp function should return nil when:
      ;; (+ helpful-count unhelpful-count) = 0
      )))

;; =============================================================================
;; Test Schema Documentation
;; =============================================================================

(deftest test-audit-schema-documentation
  (testing "Audit log schema additions are documented"
    ;; Memory entries should have these additional fields:
    ;; - :access-count    integer (default 0)
    ;; - :last-accessed   timestamp (nil initially)  
    ;; - :helpful-count   integer (default 0)
    ;; - :unhelpful-count integer (default 0)
    ;;
    ;; These fields are added lazily - entries created before this
    ;; feature will have nil values, treated as 0.
    (is true "Schema documented in namespace docstring")))

;; =============================================================================
;; Test Cleanup Integration Documentation  
;; =============================================================================

(deftest test-cleanup-integration-documentation
  (testing "Cleanup integration rules are documented"
    ;; Cleanup heuristics (document but don't implement):
    ;; 1. Low helpfulness ratio (< 0.3) -> candidate for demotion
    ;;    - If ratio is below threshold, consider demoting duration
    ;;    - Example: long-term -> short-term
    ;;
    ;; 2. Zero access after 30 days -> candidate for cleanup
    ;;    - If last-accessed is > 30 days ago AND access-count = 0
    ;;    - Or if last-accessed is nil and created > 30 days ago
    ;;    - Consider for cleanup/deletion
    ;;
    ;; These are heuristics for human/agent decision-making,
    ;; not automatic actions.
    (is true "Cleanup rules documented for future implementation")))

;; =============================================================================
;; Integration Test: Full Audit Lifecycle
;; =============================================================================

(deftest test-full-audit-lifecycle-elisp-generation
  (testing "Full audit lifecycle generates correct elisp sequence"
    (let [entry-id "lifecycle-test-entry"
          ;; Simulate: access -> helpful -> unhelpful -> check ratio
          access-elisp (memory-log-access-elisp entry-id)
          helpful-elisp (memory-mark-helpful-elisp entry-id)
          unhelpful-elisp (memory-mark-unhelpful-elisp entry-id)
          ratio-elisp (memory-helpfulness-ratio-elisp entry-id)]

      ;; All should be valid elisp
      (is (str/starts-with? access-elisp "(progn"))
      (is (str/starts-with? helpful-elisp "(progn"))
      (is (str/starts-with? unhelpful-elisp "(progn"))
      (is (str/starts-with? ratio-elisp "(progn"))

      ;; All should reference the same entry
      (is (str/includes? access-elisp entry-id))
      (is (str/includes? helpful-elisp entry-id))
      (is (str/includes? unhelpful-elisp entry-id))
      (is (str/includes? ratio-elisp entry-id))

      ;; Each should call the correct function
      (is (str/includes? access-elisp "log-access"))
      (is (str/includes? helpful-elisp "mark-helpful"))
      (is (str/includes? unhelpful-elisp "mark-unhelpful"))
      (is (str/includes? ratio-elisp "helpfulness-ratio")))))
