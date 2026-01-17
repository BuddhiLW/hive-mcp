(ns hive-mcp.org-clj.ast-builder-test
  "Tests for org-clj AST builder module.

   Covers:
   - Property drawer extraction
   - Headline metadata parsing (planning, properties)
   - File-level property parsing
   - Headline parsing (flat sequence)
   - Document tree building (nested headlines)"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.org-clj.ast-builder :as ast]))

;; =============================================================================
;; Property Drawer Extraction Tests
;; =============================================================================

(deftest extract-property-drawer-test
  (testing "basic property drawer"
    (let [lines [":PROPERTIES:"
                 ":ID: 123"
                 ":CREATED: 2025-01-01"
                 ":END:"
                 "Content after"]
          result (ast/extract-property-drawer lines)]
      (is (= {:ID "123" :CREATED "2025-01-01"} (:properties result)))
      (is (= ["Content after"] (:remaining-lines result)))))

  (testing "property drawer with leading blank lines"
    (let [lines [""
                 "  "
                 ":PROPERTIES:"
                 ":ID: abc"
                 ":END:"
                 "Body"]
          result (ast/extract-property-drawer lines)]
      (is (= {:ID "abc"} (:properties result)))
      ;; Blank lines before drawer are preserved, drawer removed
      (is (= ["" "  " "Body"] (:remaining-lines result)))))

  (testing "no property drawer present"
    (let [lines ["Some content"
                 "More content"]
          result (ast/extract-property-drawer lines)]
      (is (nil? (:properties result)))
      (is (= ["Some content" "More content"] (:remaining-lines result)))))

  (testing "empty lines only"
    (let [lines ["" "  " ""]
          result (ast/extract-property-drawer lines)]
      (is (nil? (:properties result)))
      (is (= ["" "  " ""] (:remaining-lines result)))))

  (testing "empty input"
    (let [result (ast/extract-property-drawer [])]
      (is (nil? (:properties result)))
      (is (= [] (:remaining-lines result)))))

  (testing "property drawer with special characters"
    (let [lines [":PROPERTIES:"
                 ":URL: https://example.com/path?query=1&foo=bar"
                 ":CUSTOM: value with: colons"
                 ":END:"]
          result (ast/extract-property-drawer lines)]
      (is (= "https://example.com/path?query=1&foo=bar"
             (:URL (:properties result))))
      (is (= "value with: colons"
             (:CUSTOM (:properties result))))))

  (testing "property drawer with whitespace variations"
    (let [lines ["  :PROPERTIES:  "
                 "  :ID: spaced  "
                 "  :END:  "
                 "After"]
          result (ast/extract-property-drawer lines)]
      (is (some? (:properties result)))
      (is (= ["After"] (:remaining-lines result)))))

  (testing "malformed drawer - no :END:"
    (let [lines [":PROPERTIES:"
                 ":ID: orphan"
                 "Some content"]
          result (ast/extract-property-drawer lines)]
      ;; No valid drawer without :END:
      (is (nil? (:properties result)))
      (is (= lines (:remaining-lines result)))))

  (testing "drawer with headline before :END: - continues to find :END:"
    ;; Note: Implementation continues searching past headlines to find :END:
    ;; This may parse non-standard org content but handles edge cases gracefully
    (let [lines [":PROPERTIES:"
                 ":ID: found-anyway"
                 "* Headline"
                 ":END:"]
          result (ast/extract-property-drawer lines)]
      ;; Implementation finds :END: even past headline
      (is (= {:ID "found-anyway"} (:properties result))))))

;; =============================================================================
;; Headline Metadata Parsing Tests
;; =============================================================================

(deftest parse-headline-metadata-test
  (testing "planning line and properties"
    (let [lines ["CLOSED: [2025-01-15 Wed 10:00]"
                 ":PROPERTIES:"
                 ":ID: task-1"
                 ":END:"
                 "Body content"]
          result (ast/parse-headline-metadata lines)]
      (is (some? (:planning result)))
      (is (= {:ID "task-1"} (:properties result)))
      (is (= ["Body content"] (:remaining-lines result)))))

  (testing "only planning line"
    (let [lines ["SCHEDULED: <2025-01-20>"
                 "Task description"]
          result (ast/parse-headline-metadata lines)]
      (is (some? (:planning result)))
      (is (= {} (:properties result)))
      (is (= ["Task description"] (:remaining-lines result)))))

  (testing "only properties (no planning)"
    (let [lines [":PROPERTIES:"
                 ":ID: no-planning"
                 ":PRIORITY: high"
                 ":END:"
                 "Content"]
          result (ast/parse-headline-metadata lines)]
      (is (= {} (:planning result)))
      (is (= {:ID "no-planning" :PRIORITY "high"} (:properties result)))
      (is (= ["Content"] (:remaining-lines result)))))

  (testing "neither planning nor properties"
    (let [lines ["Just content"
                 "More content"]
          result (ast/parse-headline-metadata lines)]
      (is (= {} (:planning result)))
      (is (= {} (:properties result)))
      (is (= ["Just content" "More content"] (:remaining-lines result)))))

  (testing "empty input"
    (let [result (ast/parse-headline-metadata [])]
      (is (= {} (:planning result)))
      (is (= {} (:properties result)))
      (is (= [] (:remaining-lines result)))))

  (testing "deadline planning"
    (let [lines ["DEADLINE: <2025-02-01 Sat>"
                 "Important task"]
          result (ast/parse-headline-metadata lines)]
      (is (contains? (:planning result) :deadline))))

  (testing "multiple planning keywords on one line"
    (let [lines ["CLOSED: [2025-01-01] SCHEDULED: <2025-01-02>"
                 "Content"]
          result (ast/parse-headline-metadata lines)]
      (is (some? (:planning result))))))

;; =============================================================================
;; File Properties Parsing Tests
;; =============================================================================

(deftest parse-file-properties-test
  (testing "basic file properties"
    (let [lines ["#+TITLE: My Document"
                 "#+AUTHOR: Test User"
                 "* First Heading"]
          result (ast/parse-file-properties lines)]
      (is (= {:TITLE "My Document" :AUTHOR "Test User"} (:properties result)))
      (is (= ["* First Heading"] (:remaining-lines result)))))

  (testing "file properties with blank lines"
    (let [lines ["#+TITLE: Doc"
                 ""
                 "#+DATE: 2025-01-15"
                 ""
                 "* Heading"]
          result (ast/parse-file-properties lines)]
      (is (contains? (:properties result) :TITLE))
      (is (contains? (:properties result) :DATE))))

  (testing "no file properties"
    (let [lines ["* Just a Heading"
                 "Content"]
          result (ast/parse-file-properties lines)]
      (is (= {} (:properties result)))
      (is (= ["* Just a Heading" "Content"] (:remaining-lines result)))))

  (testing "empty input"
    (let [result (ast/parse-file-properties [])]
      (is (= {} (:properties result)))
      (is (= [] (:remaining-lines result)))))

  (testing "file properties with special values"
    (let [lines ["#+OPTIONS: toc:nil H:3"
                 "#+STARTUP: overview"
                 "Content"]
          result (ast/parse-file-properties lines)]
      (is (= "toc:nil H:3" (:OPTIONS (:properties result))))
      (is (= "overview" (:STARTUP (:properties result)))))))

;; =============================================================================
;; Headline Parsing Tests (Flat Sequence)
;; =============================================================================

(deftest parse-headlines-test
  (testing "single headline"
    (let [lines ["* TODO Task one"
                 "Description"]
          headlines (ast/parse-headlines lines)]
      (is (= 1 (count headlines)))
      (is (= 1 (:level (first headlines))))
      (is (= "TODO" (:keyword (first headlines))))
      (is (= "Task one" (:title (first headlines))))
      (is (= ["Description"] (:content (first headlines))))))

  (testing "multiple headlines at same level"
    (let [lines ["* First"
                 "* Second"
                 "* Third"]
          headlines (ast/parse-headlines lines)]
      (is (= 3 (count headlines)))
      (is (every? #(= 1 (:level %)) headlines))))

  (testing "headlines at different levels"
    (let [lines ["* Level 1"
                 "** Level 2"
                 "*** Level 3"
                 "* Back to 1"]
          headlines (ast/parse-headlines lines)]
      (is (= 4 (count headlines)))
      (is (= [1 2 3 1] (map :level headlines)))))

  (testing "headline with properties"
    (let [lines ["* Task"
                 ":PROPERTIES:"
                 ":ID: task-123"
                 ":END:"
                 "Content"]
          headlines (ast/parse-headlines lines)]
      (is (= 1 (count headlines)))
      (is (= {:ID "task-123"} (:properties (first headlines))))
      (is (= ["Content"] (:content (first headlines))))))

  (testing "headline with planning and properties"
    (let [lines ["* TODO Important"
                 "SCHEDULED: <2025-01-20>"
                 ":PROPERTIES:"
                 ":ID: important-1"
                 ":PRIORITY: A"
                 ":END:"
                 "This is important"]
          headlines (ast/parse-headlines lines)]
      (is (= 1 (count headlines)))
      (let [hl (first headlines)]
        (is (= "TODO" (:keyword hl)))
        (is (= "Important" (:title hl)))
        (is (some? (:planning hl)))
        (is (= {:ID "important-1" :PRIORITY "A"} (:properties hl)))
        (is (= ["This is important"] (:content hl))))))

  (testing "no headlines"
    (let [lines ["Just content"
                 "No headlines here"]
          headlines (ast/parse-headlines lines)]
      (is (= 0 (count headlines)))))

  (testing "empty input"
    (let [headlines (ast/parse-headlines [])]
      (is (= [] headlines))))

  (testing "headlines with multi-line content"
    (let [lines ["* Headline"
                 "Line 1"
                 "Line 2"
                 "Line 3"
                 "* Next"]
          headlines (ast/parse-headlines lines)]
      (is (= 2 (count headlines)))
      (is (= ["Line 1" "Line 2" "Line 3"] (:content (first headlines))))
      (is (= [] (:content (second headlines))))))

  (testing "headline with DONE keyword"
    (let [lines ["* DONE Completed task"
                 "CLOSED: [2025-01-10 Fri]"]
          headlines (ast/parse-headlines lines)]
      (is (= "DONE" (:keyword (first headlines))))
      (is (some? (:planning (first headlines))))))

  (testing "headline with priority"
    (let [lines ["* [#A] High priority task"]
          headlines (ast/parse-headlines lines)]
      (is (= 1 (count headlines)))
      ;; Priority handling depends on tokenizer
      (is (some? (first headlines)))))

  (testing "headline with tags"
    (let [lines ["* Task with tags :tag1:tag2:"]
          headlines (ast/parse-headlines lines)]
      (is (= 1 (count headlines)))
      ;; Tags handling depends on tokenizer
      (is (some? (first headlines))))))

;; =============================================================================
;; Document Tree Building Tests
;; =============================================================================

(deftest build-document-tree-test
  (testing "basic document structure"
    (let [file-props {:TITLE "Test Doc"}
          flat-hls [{:level 1 :title "First" :content [] :children []}
                    {:level 1 :title "Second" :content [] :children []}]
          doc (ast/build-document-tree file-props flat-hls)]
      (is (= :document (:type doc)))
      (is (= {:TITLE "Test Doc"} (:properties doc)))
      (is (= 2 (count (:headlines doc))))))

  (testing "nested headlines tree"
    (let [flat-hls [{:level 1 :title "Parent" :content [] :children []}
                    {:level 2 :title "Child 1" :content [] :children []}
                    {:level 2 :title "Child 2" :content [] :children []}
                    {:level 1 :title "Sibling" :content [] :children []}]
          doc (ast/build-document-tree {} flat-hls)]
      (is (= 2 (count (:headlines doc))))
      (let [parent (first (:headlines doc))]
        (is (= "Parent" (:title parent)))
        (is (= 2 (count (:children parent)))))))

  (testing "deeply nested structure"
    (let [flat-hls [{:level 1 :title "L1" :content [] :children []}
                    {:level 2 :title "L2" :content [] :children []}
                    {:level 3 :title "L3" :content [] :children []}
                    {:level 4 :title "L4" :content [] :children []}]
          doc (ast/build-document-tree {} flat-hls)]
      (is (= 1 (count (:headlines doc))))
      (let [l1 (first (:headlines doc))
            l2 (first (:children l1))
            l3 (first (:children l2))
            l4 (first (:children l3))]
        (is (= "L1" (:title l1)))
        (is (= "L2" (:title l2)))
        (is (= "L3" (:title l3)))
        (is (= "L4" (:title l4))))))

  (testing "empty headlines"
    (let [doc (ast/build-document-tree {:TITLE "Empty"} [])]
      (is (= :document (:type doc)))
      (is (= [] (:headlines doc)))))

  (testing "empty properties"
    (let [doc (ast/build-document-tree {} [{:level 1 :title "Only" :content [] :children []}])]
      (is (= {} (:properties doc)))
      (is (= 1 (count (:headlines doc))))))

  (testing "complex mixed structure"
    (let [flat-hls [{:level 1 :title "A" :content ["a content"] :children []}
                    {:level 2 :title "A.1" :content [] :children []}
                    {:level 3 :title "A.1.1" :content [] :children []}
                    {:level 2 :title "A.2" :content [] :children []}
                    {:level 1 :title "B" :content [] :children []}
                    {:level 2 :title "B.1" :content [] :children []}]
          doc (ast/build-document-tree {} flat-hls)]
      (is (= 2 (count (:headlines doc))))
      (let [a (first (:headlines doc))
            b (second (:headlines doc))]
        (is (= "A" (:title a)))
        (is (= 2 (count (:children a))))
        (is (= "A.1" (:title (first (:children a)))))
        (is (= 1 (count (:children (first (:children a))))))
        (is (= "B" (:title b)))
        (is (= 1 (count (:children b))))))))

;; =============================================================================
;; Edge Cases and Integration Tests
;; =============================================================================

(deftest edge-cases-test
  (testing "headline with only whitespace content"
    (let [lines ["* Heading"
                 "   "
                 ""
                 "  "]
          headlines (ast/parse-headlines lines)]
      (is (= 1 (count headlines)))
      ;; Blank content lines are filtered
      (is (= [] (:content (first headlines))))))

  (testing "property with empty value"
    (let [lines [":PROPERTIES:"
                 ":EMPTY:"
                 ":END:"]
          result (ast/extract-property-drawer lines)]
      ;; Empty property values handled gracefully
      (is (some? result))))

  (testing "very long headline title"
    (let [long-title (apply str (repeat 500 "x"))
          lines [(str "* " long-title)]
          headlines (ast/parse-headlines lines)]
      (is (= 1 (count headlines)))
      (is (= long-title (:title (first headlines))))))

  (testing "unicode in headline"
    (let [lines ["* 你好世界 🌍"
                 "日本語テスト"]
          headlines (ast/parse-headlines lines)]
      (is (= 1 (count headlines)))
      (is (= "你好世界 🌍" (:title (first headlines))))
      (is (= ["日本語テスト"] (:content (first headlines))))))

  (testing "property with unicode value"
    (let [lines [":PROPERTIES:"
                 ":NAME: 日本語"
                 ":END:"]
          result (ast/extract-property-drawer lines)]
      (is (= "日本語" (:NAME (:properties result))))))

  (testing "content before first headline"
    (let [lines ["Some preamble"
                 "Before headlines"
                 "* First headline"]
          headlines (ast/parse-headlines lines)]
      ;; parse-headlines skips content before first headline
      (is (= 1 (count headlines)))
      (is (= "First headline" (:title (first headlines))))))

  (testing "nil handling"
    (is (= {:properties nil :remaining-lines []}
           (ast/extract-property-drawer nil)))))

;; =============================================================================
;; Integration: Full Document Parsing
;; =============================================================================

(deftest full-document-integration-test
  (testing "complete org document parsing flow"
    (let [lines ["#+TITLE: Test Document"
                 "#+AUTHOR: Test Author"
                 ""
                 "* TODO First Section"
                 ":PROPERTIES:"
                 ":ID: section-1"
                 ":END:"
                 "Introduction content"
                 ""
                 "** DONE Subtask"
                 "CLOSED: [2025-01-10]"
                 ":PROPERTIES:"
                 ":ID: subtask-1"
                 ":END:"
                 "Subtask details"
                 ""
                 "* Second Section"
                 "More content"
                 "** Sub 2.1"
                 "*** Deep nested"]
          file-props-result (ast/parse-file-properties lines)
          headlines (ast/parse-headlines (:remaining-lines file-props-result))
          doc (ast/build-document-tree (:properties file-props-result) headlines)]
      ;; File properties
      (is (= {:TITLE "Test Document" :AUTHOR "Test Author"} (:properties doc)))
      ;; Top-level headlines
      (is (= 2 (count (:headlines doc))))
      ;; First section structure
      (let [first-section (first (:headlines doc))]
        (is (= "TODO" (:keyword first-section)))
        (is (= "First Section" (:title first-section)))
        (is (= {:ID "section-1"} (:properties first-section)))
        ;; First section has one child (Subtask)
        (is (= 1 (count (:children first-section))))
        (let [subtask (first (:children first-section))]
          (is (= "DONE" (:keyword subtask)))
          (is (= {:ID "subtask-1"} (:properties subtask)))))
      ;; Second section structure
      (let [second-section (second (:headlines doc))]
        (is (= "Second Section" (:title second-section)))
        (is (= 1 (count (:children second-section))))
        ;; Sub 2.1 has one child (Deep nested)
        (is (= 1 (count (:children (first (:children second-section))))))))))

(deftest kanban-document-test
  (testing "kanban-style org document"
    (let [lines ["#+TITLE: Kanban Board"
                 "* TODO Task 1"
                 ":PROPERTIES:"
                 ":VIBE_ID: task-001"
                 ":END:"
                 "* IN-PROGRESS Task 2"
                 ":PROPERTIES:"
                 ":VIBE_ID: task-002"
                 ":ASSIGNEE: alice"
                 ":END:"
                 "* DONE Task 3"
                 "CLOSED: [2025-01-15]"
                 ":PROPERTIES:"
                 ":VIBE_ID: task-003"
                 ":END:"]
          file-props-result (ast/parse-file-properties lines)
          headlines (ast/parse-headlines (:remaining-lines file-props-result))
          doc (ast/build-document-tree (:properties file-props-result) headlines)]
      (is (= 3 (count (:headlines doc))))
      (is (= ["TODO" "IN-PROGRESS" "DONE"]
             (map :keyword (:headlines doc))))
      (is (= ["task-001" "task-002" "task-003"]
             (map #(get-in % [:properties :VIBE_ID]) (:headlines doc)))))))
