;;; hive-mcp-transform-test.el --- ERT tests for hive-mcp-transform -*- lexical-binding: t -*-

;; Copyright (C) 2025 BuddhiLW

;; Author: BuddhiLW
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; TDD tests for the data transformation layer.
;; CLARITY-L: Separates transformation logic from API domain logic.
;; These functions are pure - no side effects, no domain knowledge.

;;; Code:

(require 'ert)

;; Load the module under test
(let ((elisp-dir (expand-file-name "../../elisp" (file-name-directory load-file-name))))
  (add-to-list 'load-path elisp-dir))
(require 'hive-mcp-transform)

;;;; Test: plist-to-alist

(ert-deftest hive-mcp-transform-test-plist-to-alist-simple ()
  "Test simple plist to alist conversion."
  (let ((result (hive-mcp-transform-plist-to-alist '(:id "123" :type "note"))))
    (should (assoc 'id result))
    (should (equal "123" (cdr (assoc 'id result))))
    (should (equal "note" (cdr (assoc 'type result))))))

(ert-deftest hive-mcp-transform-test-plist-to-alist-keyword-stripping ()
  "Test that keyword colons are stripped from keys."
  (let ((result (hive-mcp-transform-plist-to-alist '(:foo "bar"))))
    (should (assoc 'foo result))
    (should-not (assoc :foo result))))

(ert-deftest hive-mcp-transform-test-plist-to-alist-nested ()
  "Test nested plist conversion."
  (let ((result (hive-mcp-transform-plist-to-alist '(:outer (:inner "value")))))
    (should (assoc 'outer result))
    (let ((nested (cdr (assoc 'outer result))))
      (should (assoc 'inner nested))
      (should (equal "value" (cdr (assoc 'inner nested)))))))

(ert-deftest hive-mcp-transform-test-plist-to-alist-list-to-vector ()
  "Test that plain lists are converted to vectors for JSON."
  (let ((result (hive-mcp-transform-plist-to-alist '(:tags ("a" "b" "c")))))
    (let ((tags (cdr (assoc 'tags result))))
      (should (vectorp tags))
      (should (equal ["a" "b" "c"] tags)))))

(ert-deftest hive-mcp-transform-test-plist-to-alist-nil-to-empty-vector ()
  "Test that nil values become empty vectors."
  (let ((result (hive-mcp-transform-plist-to-alist '(:items nil))))
    (let ((items (cdr (assoc 'items result))))
      (should (vectorp items))
      (should (equal [] items)))))

(ert-deftest hive-mcp-transform-test-plist-to-alist-preserves-non-keyword-keys ()
  "Test that non-keyword keys are preserved as-is."
  (let ((result (hive-mcp-transform-plist-to-alist '(foo "bar"))))
    (should (assoc 'foo result))))

(ert-deftest hive-mcp-transform-test-plist-to-alist-empty ()
  "Test empty plist returns empty alist."
  (let ((result (hive-mcp-transform-plist-to-alist nil)))
    (should (null result))))

(ert-deftest hive-mcp-transform-test-plist-to-alist-deeply-nested-list ()
  "Test lists of plists are converted to vectors of alists."
  (let ((result (hive-mcp-transform-plist-to-alist
                 '(:entries ((:id "1" :name "first")
                             (:id "2" :name "second"))))))
    (let ((entries (cdr (assoc 'entries result))))
      (should (vectorp entries))
      (should (= 2 (length entries)))
      (should (assoc 'id (aref entries 0)))
      (should (equal "1" (cdr (assoc 'id (aref entries 0))))))))

;;;; Test: entries-to-vector

(ert-deftest hive-mcp-transform-test-entries-to-vector-basic ()
  "Test converting list of entries to vector of alists."
  (let ((result (hive-mcp-transform-entries-to-vector
                 '((:id "1" :type "note")
                   (:id "2" :type "decision")))))
    (should (vectorp result))
    (should (= 2 (length result)))
    (should (equal "1" (cdr (assoc 'id (aref result 0)))))
    (should (equal "2" (cdr (assoc 'id (aref result 1)))))))

(ert-deftest hive-mcp-transform-test-entries-to-vector-empty ()
  "Test empty list returns empty vector."
  (let ((result (hive-mcp-transform-entries-to-vector nil)))
    (should (vectorp result))
    (should (= 0 (length result)))))

(ert-deftest hive-mcp-transform-test-entries-to-vector-single ()
  "Test single entry returns single-element vector."
  (let ((result (hive-mcp-transform-entries-to-vector '((:id "1")))))
    (should (vectorp result))
    (should (= 1 (length result)))))

;;;; Test: scope-arg-normalize

(ert-deftest hive-mcp-transform-test-scope-arg-nil ()
  "Test nil scope returns nil (auto-filter)."
  (should (null (hive-mcp-transform-scope-arg nil))))

(ert-deftest hive-mcp-transform-test-scope-arg-all ()
  "Test 'all' scope returns t (no filter)."
  (should (eq t (hive-mcp-transform-scope-arg "all"))))

(ert-deftest hive-mcp-transform-test-scope-arg-global ()
  "Test 'global' scope returns symbol."
  (should (eq 'global (hive-mcp-transform-scope-arg "global"))))

(ert-deftest hive-mcp-transform-test-scope-arg-specific ()
  "Test specific scope string is passed through."
  (should (equal "scope:project:foo" (hive-mcp-transform-scope-arg "scope:project:foo"))))

;;;; Test: content-preview

(ert-deftest hive-mcp-transform-test-content-preview-string-short ()
  "Test short string content is returned as-is."
  (should (equal "hello world" (hive-mcp-transform-content-preview "hello world"))))

(ert-deftest hive-mcp-transform-test-content-preview-string-truncated ()
  "Test long string is truncated with ellipsis."
  (let ((long-text (make-string 150 ?x)))
    (let ((result (hive-mcp-transform-content-preview long-text)))
      (should (= 100 (length result)))
      (should (string-suffix-p "..." result)))))

(ert-deftest hive-mcp-transform-test-content-preview-custom-length ()
  "Test custom max length truncation."
  (let ((result (hive-mcp-transform-content-preview "hello world" 8)))
    (should (= 8 (length result)))
    (should (equal "hello..." result))))

(ert-deftest hive-mcp-transform-test-content-preview-plist-description ()
  "Test plist content extracts :description."
  (let ((content '(:description "My description" :code "some code")))
    (should (equal "My description" (hive-mcp-transform-content-preview content)))))

(ert-deftest hive-mcp-transform-test-content-preview-plist-title ()
  "Test plist content extracts :title when no description."
  (let ((content '(:title "My title" :code "some code")))
    (should (equal "My title" (hive-mcp-transform-content-preview content)))))

(ert-deftest hive-mcp-transform-test-content-preview-plist-name ()
  "Test plist content extracts :name when no title/description."
  (let ((content '(:name "My name" :code "some code")))
    (should (equal "My name" (hive-mcp-transform-content-preview content)))))

(ert-deftest hive-mcp-transform-test-content-preview-plist-code ()
  "Test plist content extracts :code as fallback."
  (let ((content '(:code "(defun foo () nil)")))
    (should (equal "(defun foo () nil)" (hive-mcp-transform-content-preview content)))))

(ert-deftest hive-mcp-transform-test-content-preview-plist-fallback ()
  "Test plist content formats to string as last resort."
  (let ((content '(:unknown 123)))
    (let ((result (hive-mcp-transform-content-preview content)))
      (should (stringp result)))))

(ert-deftest hive-mcp-transform-test-content-preview-other ()
  "Test other types are formatted as string."
  (should (stringp (hive-mcp-transform-content-preview 12345)))
  (should (stringp (hive-mcp-transform-content-preview '(a b c)))))

;;;; Test: entry-to-metadata

(ert-deftest hive-mcp-transform-test-entry-to-metadata-basic ()
  "Test extracting metadata from entry."
  (let ((entry '(:id "123" :type note :content "Full content here" 
                 :tags ("tag1" "tag2") :created "2025-01-15T10:00:00+0000")))
    (let ((result (hive-mcp-transform-entry-to-metadata entry)))
      (should (equal "123" (cdr (assoc 'id result))))
      (should (eq 'note (cdr (assoc 'type result))))
      (should (stringp (cdr (assoc 'preview result))))
      (should (vectorp (cdr (assoc 'tags result))))
      (should (equal "2025-01-15T10:00:00+0000" (cdr (assoc 'created result)))))))

(ert-deftest hive-mcp-transform-test-entry-to-metadata-preview-truncation ()
  "Test that preview is truncated."
  (let ((entry (list :id "123" :type 'note 
                     :content (make-string 200 ?x)
                     :tags nil :created "2025-01-15")))
    (let ((result (hive-mcp-transform-entry-to-metadata entry)))
      (should (<= (length (cdr (assoc 'preview result))) 100)))))

(ert-deftest hive-mcp-transform-test-entry-to-metadata-nil-tags ()
  "Test nil tags become empty vector."
  (let ((entry '(:id "123" :type note :content "test" :tags nil :created "2025-01-15")))
    (let ((result (hive-mcp-transform-entry-to-metadata entry)))
      (should (vectorp (cdr (assoc 'tags result))))
      (should (= 0 (length (cdr (assoc 'tags result))))))))

;;;; Test: json-false helper

(ert-deftest hive-mcp-transform-test-bool-for-json-true ()
  "Test t passes through."
  (should (eq t (hive-mcp-transform-bool-for-json t))))

(ert-deftest hive-mcp-transform-test-bool-for-json-false ()
  "Test nil becomes :false for JSON."
  (should (eq :false (hive-mcp-transform-bool-for-json nil))))

;;;; Test: error-result

(ert-deftest hive-mcp-transform-test-error-result-basic ()
  "Test basic error result construction."
  (let ((result (hive-mcp-transform-error-result "not-found")))
    (should (equal "not-found" (cdr (assoc 'error result))))))

(ert-deftest hive-mcp-transform-test-error-result-with-extras ()
  "Test error result with extra key-value pairs."
  (let ((result (hive-mcp-transform-error-result "invalid" 'id "123" 'type "note")))
    (should (equal "invalid" (cdr (assoc 'error result))))
    (should (equal "123" (cdr (assoc 'id result))))
    (should (equal "note" (cdr (assoc 'type result))))))

;;;; Test: stats-to-alist

(ert-deftest hive-mcp-transform-test-stats-to-alist-basic ()
  "Test stats plist to alist conversion."
  (let ((stats '(:todo 5 :doing 2 :review 1 :extra 99)))
    (let ((result (hive-mcp-transform-stats-to-alist stats '(:todo :doing :review))))
      (should (= 3 (length result)))
      (should (equal 5 (cdr (assoc 'todo result))))
      (should (equal 2 (cdr (assoc 'doing result))))
      (should (equal 1 (cdr (assoc 'review result))))
      ;; Extra key not included
      (should-not (assoc 'extra result)))))

(ert-deftest hive-mcp-transform-test-stats-to-alist-empty ()
  "Test empty keys list returns empty alist."
  (let ((result (hive-mcp-transform-stats-to-alist '(:todo 5) nil)))
    (should (null result))))

;;;; Test: position-to-alist

(ert-deftest hive-mcp-transform-test-position-to-alist-full ()
  "Test position alist with all fields."
  (let ((result (hive-mcp-transform-position-to-alist 100 10 5)))
    (should (equal 100 (cdr (assoc 'point result))))
    (should (equal 10 (cdr (assoc 'line result))))
    (should (equal 5 (cdr (assoc 'column result))))))

(ert-deftest hive-mcp-transform-test-position-to-alist-partial ()
  "Test position alist with partial fields."
  (let ((result (hive-mcp-transform-position-to-alist nil 10 5)))
    (should-not (assoc 'point result))
    (should (equal 10 (cdr (assoc 'line result))))
    (should (equal 5 (cdr (assoc 'column result))))))

(ert-deftest hive-mcp-transform-test-position-to-alist-line-column-only ()
  "Test position alist with line and column only."
  (let ((result (hive-mcp-transform-position-to-alist nil 42 0)))
    (should (= 2 (length result)))
    (should (equal 42 (cdr (assoc 'line result))))
    (should (equal 0 (cdr (assoc 'column result))))))

;;;; Test: duplicate-result

(ert-deftest hive-mcp-transform-test-duplicate-result-found ()
  "Test duplicate result when entry exists."
  (let ((entry '(:id "123" :content "test")))
    (let ((result (hive-mcp-transform-duplicate-result t entry "abc123")))
      (should (eq t (cdr (assoc 'exists result))))
      (should (assoc 'entry result))
      (should (equal "123" (cdr (assoc 'id (cdr (assoc 'entry result))))))
      (should (equal "abc123" (cdr (assoc 'content_hash result)))))))

(ert-deftest hive-mcp-transform-test-duplicate-result-not-found ()
  "Test duplicate result when no entry exists."
  (let ((result (hive-mcp-transform-duplicate-result nil nil "def456")))
    (should (eq :false (cdr (assoc 'exists result))))
    (should (null (cdr (assoc 'entry result))))
    (should (equal "def456" (cdr (assoc 'content_hash result))))))

(provide 'hive-mcp-transform-test)
;;; hive-mcp-transform-test.el ends here
