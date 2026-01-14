(ns hive-mcp.org-clj.parser-test
  "Tests for org-mode parser extracted from inline tests.
   Following TDD principles - tests must pass before and after refactoring."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.org-clj.parser :as parser]
            [hive-mcp.org-clj.tokenizer :as tok]
            [hive-mcp.org-clj.schemas :as schemas]))

;; =============================================================================
;; Property Line Parsing Tests (tokenizer module)
;; =============================================================================

(deftest test-parse-property-line
  ;; parse-property-line is now public in tokenizer module
  (testing "Basic property parsing"
    (is (= [:ID "abc-123"] (tok/parse-property-line ":ID: abc-123")))
    (is (= [:CREATED "2025-01-01"] (tok/parse-property-line ":CREATED: 2025-01-01"))))

  (testing "Property with spaces in value"
    (is (= [:TITLE "My Important Task"] (tok/parse-property-line ":TITLE: My Important Task"))))

  (testing "Property with special characters"
    (is (= [:URL "https://example.com/path?q=1&r=2"]
           (tok/parse-property-line ":URL: https://example.com/path?q=1&r=2"))))

  (testing "Property with leading/trailing whitespace"
    (is (= [:ID "trimmed"] (tok/parse-property-line "  :ID:   trimmed  "))))

  (testing "Empty value"
    (is (= [:EMPTY ""] (tok/parse-property-line ":EMPTY:"))))

  (testing "Invalid lines return nil"
    (is (nil? (tok/parse-property-line "not a property")))
    (is (nil? (tok/parse-property-line "")))))

;; =============================================================================
;; Property Drawer Parsing Tests
;; =============================================================================

(deftest test-parse-property-drawer
  (testing "Normal property drawer"
    (is (= {:ID "abc-123" :CREATED "2025-01-01"}
           (parser/parse-property-drawer [":ID: abc-123" ":CREATED: 2025-01-01"]))))

  (testing "Empty drawer"
    (is (= {} (parser/parse-property-drawer [])))
    (is (= {} (parser/parse-property-drawer nil))))

  (testing "Drawer with blank lines"
    (is (= {:ID "abc"} (parser/parse-property-drawer [":ID: abc" "" "  "]))))

  (testing "Multi-word values preserved"
    (is (= {:DESCRIPTION "This is a long description with spaces"}
           (parser/parse-property-drawer [":DESCRIPTION: This is a long description with spaces"]))))

  (testing "Special characters in values"
    (is (= {:EXPR "(+ 1 2)" :PATH "/home/user/file.org"}
           (parser/parse-property-drawer [":EXPR: (+ 1 2)" ":PATH: /home/user/file.org"])))))

;; =============================================================================
;; Planning Line Parsing Tests
;; =============================================================================

(deftest test-parse-planning-line
  (testing "CLOSED only"
    (is (= {:closed "2025-01-01 Wed"}
           (parser/parse-planning-line "CLOSED: [2025-01-01 Wed]"))))

  (testing "SCHEDULED only"
    (is (= {:scheduled "2025-01-02 Thu"}
           (parser/parse-planning-line "SCHEDULED: <2025-01-02 Thu>"))))

  (testing "DEADLINE only"
    (is (= {:deadline "2025-01-03 Fri 10:00"}
           (parser/parse-planning-line "DEADLINE: <2025-01-03 Fri 10:00>"))))

  (testing "Multiple planning keywords"
    (is (= {:closed "2025-01-01 Wed 14:30" :scheduled "2025-01-02 Thu"}
           (parser/parse-planning-line "CLOSED: [2025-01-01 Wed 14:30] SCHEDULED: <2025-01-02 Thu>"))))

  (testing "All three keywords"
    (is (= {:closed "2025-01-01" :scheduled "2025-01-02" :deadline "2025-01-03"}
           (parser/parse-planning-line "CLOSED: [2025-01-01] SCHEDULED: <2025-01-02> DEADLINE: <2025-01-03>"))))

  (testing "Empty and nil input"
    (is (= {} (parser/parse-planning-line "")))
    (is (= {} (parser/parse-planning-line nil)))
    (is (= {} (parser/parse-planning-line "   ")))))

;; =============================================================================
;; Property Drawer Extraction Tests
;; =============================================================================

(deftest test-extract-property-drawer
  (testing "Extract drawer from lines"
    (let [lines [":PROPERTIES:" ":ID: test-id" ":END:" "Content here"]
          result (parser/extract-property-drawer lines)]
      (is (= {:ID "test-id"} (:properties result)))
      (is (= ["Content here"] (:remaining-lines result)))))

  (testing "No drawer present"
    (let [lines ["Just some content" "More content"]
          result (parser/extract-property-drawer lines)]
      (is (nil? (:properties result)))
      (is (= lines (:remaining-lines result)))))

  (testing "Unclosed drawer"
    (let [lines [":PROPERTIES:" ":ID: test-id" "Missing END"]
          result (parser/extract-property-drawer lines)]
      (is (nil? (:properties result))))))

;; =============================================================================
;; Headline Metadata Parsing Tests
;; =============================================================================

(deftest test-parse-headline-metadata
  (testing "Planning and properties"
    (let [lines ["CLOSED: [2025-01-01]"
                 ":PROPERTIES:"
                 ":ID: headline-1"
                 ":END:"
                 "Body content"]
          result (parser/parse-headline-metadata lines)]
      (is (= {:closed "2025-01-01"} (:planning result)))
      (is (= {:ID "headline-1"} (:properties result)))
      (is (= ["Body content"] (:remaining-lines result)))))

  (testing "Properties only (no planning)"
    (let [lines [":PROPERTIES:"
                 ":ID: headline-2"
                 ":END:"
                 "Body"]
          result (parser/parse-headline-metadata lines)]
      (is (= {} (:planning result)))
      (is (= {:ID "headline-2"} (:properties result)))))

  (testing "Planning only (no properties)"
    (let [lines ["SCHEDULED: <2025-01-02>"
                 "Body content"]
          result (parser/parse-headline-metadata lines)]
      (is (= {:scheduled "2025-01-02"} (:planning result)))
      (is (= {} (:properties result)))))

  (testing "Empty input"
    (let [result (parser/parse-headline-metadata [])]
      (is (= {} (:planning result)))
      (is (= {} (:properties result)))
      (is (= [] (:remaining-lines result))))))

;; =============================================================================
;; Headline Text Parsing Tests
;; =============================================================================

(deftest test-parse-headline-text
  (testing "Basic headline with TODO"
    (is (= {:type :headline
            :level 2
            :keyword "TODO"
            :priority nil
            :title "Task title"
            :tags []}
           (parser/parse-headline-text "** TODO Task title"))))

  (testing "Headline with priority"
    (is (= {:type :headline
            :level 1
            :keyword "TODO"
            :priority "A"
            :title "Important task"
            :tags []}
           (parser/parse-headline-text "* TODO [#A] Important task"))))

  (testing "Headline with tags"
    (is (= {:type :headline
            :level 2
            :keyword "DONE"
            :priority nil
            :title "Completed task"
            :tags ["work" "urgent"]}
           (parser/parse-headline-text "** DONE Completed task :work:urgent:"))))

  (testing "Full headline with everything"
    (is (= {:type :headline
            :level 3
            :keyword "IN-PROGRESS"
            :priority "B"
            :title "Complex task"
            :tags ["project" "dev"]}
           (parser/parse-headline-text "*** IN-PROGRESS [#B] Complex task :project:dev:"))))

  (testing "Headline without TODO keyword"
    (is (= {:type :headline
            :level 1
            :keyword nil
            :priority nil
            :title "Just a heading"
            :tags []}
           (parser/parse-headline-text "* Just a heading"))))

  (testing "Headline with only tags"
    (is (= {:type :headline
            :level 2
            :keyword nil
            :priority nil
            :title "Tagged heading"
            :tags ["tag1" "tag2" "tag3"]}
           (parser/parse-headline-text "** Tagged heading :tag1:tag2:tag3:"))))

  (testing "Non-headline returns nil"
    (is (nil? (parser/parse-headline-text "Not a headline")))
    (is (nil? (parser/parse-headline-text "")))
    (is (nil? (parser/parse-headline-text nil)))))

;; =============================================================================
;; Document Parsing Integration Tests
;; =============================================================================

(deftest test-parse-document
  (testing "Simple document with file properties"
    (let [doc (parser/parse-document "#+TITLE: My Doc\n* Heading 1")]
      (is (= :document (:type doc)))
      (is (= {:TITLE "My Doc"} (:properties doc)))
      (is (= 1 (count (:headlines doc))))
      (is (= "Heading 1" (-> doc :headlines first :title)))))

  (testing "Document with nested headlines"
    (let [doc (parser/parse-document "* Level 1\n** Level 2\n*** Level 3\n* Another L1")]
      (is (= 2 (count (:headlines doc))))
      (is (= "Level 1" (-> doc :headlines first :title)))
      (is (= "Another L1" (-> doc :headlines second :title)))
      ;; First headline has children
      (is (= 1 (count (-> doc :headlines first :children))))
      ;; That child has its own child
      (is (= 1 (count (-> doc :headlines first :children first :children))))))

  (testing "Document with TODO headlines"
    (let [doc (parser/parse-document "* TODO [#A] Important task :work:\n** DONE Subtask")]
      (is (= "TODO" (-> doc :headlines first :keyword)))
      (is (= "A" (-> doc :headlines first :priority)))
      (is (= ["work"] (-> doc :headlines first :tags)))
      (is (= "DONE" (-> doc :headlines first :children first :keyword)))))

  (testing "Empty document"
    (let [doc (parser/parse-document "")]
      (is (= :document (:type doc)))
      (is (= {} (:properties doc)))
      (is (empty? (:headlines doc))))))

(deftest test-headline-predicates
  (testing "headline? predicate"
    (is (parser/headline? "* Heading"))
    (is (parser/headline? "** TODO Task"))
    (is (parser/headline? "*** Deep"))
    (is (not (parser/headline? "Not a headline")))
    (is (not (parser/headline? "")))
    (is (not (parser/headline? nil))))

  (testing "headline-level extraction"
    (is (= 1 (parser/headline-level "* Heading")))
    (is (= 2 (parser/headline-level "** TODO Task")))
    (is (= 5 (parser/headline-level "***** Deep")))
    (is (nil? (parser/headline-level "Not a headline")))))

;; =============================================================================
;; Schema Validation Tests
;; =============================================================================

(deftest test-validate-headline
  (testing "Valid headline"
    (let [result (parser/validate-headline {:type :headline
                                            :level 1
                                            :title "Test"
                                            :keyword nil
                                            :priority nil
                                            :tags []})]
      (is (:valid result))))

  (testing "Invalid headline - missing required fields"
    (let [result (parser/validate-headline {:type :headline})]
      (is (not (:valid result)))
      (is (some? (:errors result))))))

(deftest test-validate-document
  (testing "Valid document"
    (let [result (parser/validate-document {:type :document
                                            :properties {}
                                            :headlines []})]
      (is (:valid result))))

  (testing "Invalid document - wrong type"
    (let [result (parser/validate-document {:type :not-document
                                            :headlines []})]
      (is (not (:valid result))))))

;; =============================================================================
;; Module-Specific Tests
;; =============================================================================

(deftest test-tokenizer-module
  (testing "todo-keywords set available"
    (is (contains? tok/todo-keywords "TODO"))
    (is (contains? tok/todo-keywords "DONE"))
    (is (not (contains? tok/todo-keywords "INVALID"))))

  (testing "parse-file-property"
    (is (= [:TITLE "My Doc"] (tok/parse-file-property "#+TITLE: My Doc")))
    (is (= [:AUTHOR "John"] (tok/parse-file-property "#+AUTHOR: John")))
    (is (nil? (tok/parse-file-property "Not a file property")))))

(deftest test-schemas-module
  (testing "Schema predicates"
    (is (schemas/valid-headline? {:type :headline :level 1 :title "Test"}))
    (is (not (schemas/valid-headline? {:type :headline})))
    (is (schemas/valid-document? {:type :document :headlines []}))
    (is (not (schemas/valid-document? {:type :wrong})))))
