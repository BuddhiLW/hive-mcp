;;; hive-mcp-memory-test.el --- ERT tests for hive-mcp-memory -*- lexical-binding: t -*-

;; Copyright (C) 2025 BuddhiLW

;; Author: BuddhiLW
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Regression tests for pure functions in hive-mcp-memory.
;; These tests pin behavior before the TDD decomposition refactor.
;; Focus: scope system, duration system, content deduplication, core utilities.

;;; Code:

(require 'ert)

;; Load the module under test
(let ((elisp-dir (expand-file-name "../../elisp" (file-name-directory load-file-name))))
  (add-to-list 'load-path elisp-dir)
  (add-to-list 'load-path (expand-file-name "memory" elisp-dir)))
(require 'hive-mcp-memory)

;;;; Test: Core Utilities - ID Generation

(ert-deftest hive-mcp-memory-test-generate-id-format ()
  "Test that generate-id produces correct format: TIMESTAMP-HASH."
  (let ((id (hive-mcp-memory--generate-id)))
    ;; Should match pattern: YYYYMMDDHHMMSS-8hexchars
    (should (string-match-p "^[0-9]\\{14\\}-[a-f0-9]\\{8\\}$" id))))

(ert-deftest hive-mcp-memory-test-generate-id-unique ()
  "Test that generate-id produces unique IDs."
  (let ((id1 (hive-mcp-memory--generate-id))
        (id2 (hive-mcp-memory--generate-id)))
    ;; IDs should be different (hash component varies due to random)
    (should-not (equal id1 id2))))

(ert-deftest hive-mcp-memory-test-timestamp-iso8601 ()
  "Test that timestamp produces ISO 8601 format."
  (let ((ts (hive-mcp-memory--timestamp)))
    ;; Should match ISO 8601 with timezone: YYYY-MM-DDTHH:MM:SS+ZZZZ
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}[+-][0-9]\\{4\\}$" ts))))

;;;; Test: Scope System

(ert-deftest hive-mcp-memory-test-make-scope-tag-global ()
  "Test that global scope tag is created correctly."
  (should (equal "scope:global" (hive-mcp-memory--make-scope-tag 'global))))

(ert-deftest hive-mcp-memory-test-make-scope-tag-domain ()
  "Test that domain scope tag includes the domain name."
  (should (equal "scope:domain:clojure" (hive-mcp-memory--make-scope-tag 'domain "clojure")))
  (should (equal "scope:domain:web-dev" (hive-mcp-memory--make-scope-tag 'domain "web-dev"))))

(ert-deftest hive-mcp-memory-test-make-scope-tag-project ()
  "Test that project scope tag includes the project name."
  (should (equal "scope:project:hive-mcp" (hive-mcp-memory--make-scope-tag 'project "hive-mcp")))
  (should (equal "scope:project:my-app" (hive-mcp-memory--make-scope-tag 'project "my-app"))))

(ert-deftest hive-mcp-memory-test-make-scope-tag-invalid ()
  "Test that invalid scope level raises error."
  (should-error (hive-mcp-memory--make-scope-tag 'invalid "name")))

(ert-deftest hive-mcp-memory-test-parse-scope-tag-global ()
  "Test parsing global scope tag."
  (should (equal '(global . nil) (hive-mcp-memory--parse-scope-tag "scope:global"))))

(ert-deftest hive-mcp-memory-test-parse-scope-tag-domain ()
  "Test parsing domain scope tag extracts name."
  (should (equal '(domain . "clojure") (hive-mcp-memory--parse-scope-tag "scope:domain:clojure")))
  (should (equal '(domain . "web-dev") (hive-mcp-memory--parse-scope-tag "scope:domain:web-dev"))))

(ert-deftest hive-mcp-memory-test-parse-scope-tag-project ()
  "Test parsing project scope tag extracts name."
  (should (equal '(project . "hive-mcp") (hive-mcp-memory--parse-scope-tag "scope:project:hive-mcp"))))

(ert-deftest hive-mcp-memory-test-parse-scope-tag-non-scope ()
  "Test that non-scope tags return nil."
  (should-not (hive-mcp-memory--parse-scope-tag "regular-tag"))
  (should-not (hive-mcp-memory--parse-scope-tag "clojure"))
  (should-not (hive-mcp-memory--parse-scope-tag "")))

(ert-deftest hive-mcp-memory-test-has-scope-tag-p-with-scope ()
  "Test detecting scope tags in a list."
  (should (hive-mcp-memory--has-scope-tag-p '("scope:global" "other")))
  (should (hive-mcp-memory--has-scope-tag-p '("tag1" "scope:project:foo" "tag2")))
  (should (hive-mcp-memory--has-scope-tag-p '("scope:domain:clj"))))

(ert-deftest hive-mcp-memory-test-has-scope-tag-p-without-scope ()
  "Test that non-scope tag lists return nil."
  (should-not (hive-mcp-memory--has-scope-tag-p '("regular" "tags")))
  (should-not (hive-mcp-memory--has-scope-tag-p '()))
  (should-not (hive-mcp-memory--has-scope-tag-p nil)))

(ert-deftest hive-mcp-memory-test-ensure-list-converts-vector ()
  "Test that vectors are converted to lists."
  (should (equal '(1 2 3) (hive-mcp-memory--ensure-list [1 2 3])))
  (should (equal '("a" "b") (hive-mcp-memory--ensure-list ["a" "b"]))))

(ert-deftest hive-mcp-memory-test-ensure-list-preserves-list ()
  "Test that lists are preserved as-is."
  (should (equal '(1 2 3) (hive-mcp-memory--ensure-list '(1 2 3))))
  (should (equal nil (hive-mcp-memory--ensure-list nil))))

;;;; Test: Duration System

(ert-deftest hive-mcp-memory-test-duration-list-order ()
  "Test that durations are ordered shortest to longest."
  (should (equal '(session short-term long-term permanent) hive-mcp-memory-durations)))

(ert-deftest hive-mcp-memory-test-calculate-expires-permanent ()
  "Test that permanent duration returns nil (never expires)."
  (should-not (hive-mcp-memory--calculate-expires 'permanent)))

(ert-deftest hive-mcp-memory-test-calculate-expires-session ()
  "Test that session duration expires immediately (returns current time)."
  (let ((expires (hive-mcp-memory--calculate-expires 'session)))
    ;; Should be close to current time
    (should (stringp expires))
    (should (string-match-p "^[0-9]\\{4\\}-" expires))))

(ert-deftest hive-mcp-memory-test-calculate-expires-short-term ()
  "Test that short-term duration returns future date."
  (let ((expires (hive-mcp-memory--calculate-expires 'short-term)))
    (should (stringp expires))
    ;; Should be in the future
    (should (time-less-p (current-time) (date-to-time expires)))))

(ert-deftest hive-mcp-memory-test-get-entry-duration-present ()
  "Test getting duration from entry with duration."
  (let ((entry '(:id "test" :duration "short-term")))
    (should (eq 'short-term (hive-mcp-memory--get-entry-duration entry)))))

(ert-deftest hive-mcp-memory-test-get-entry-duration-missing ()
  "Test that missing duration defaults to long-term."
  (let ((entry '(:id "test" :content "no duration")))
    (should (eq 'long-term (hive-mcp-memory--get-entry-duration entry)))))

(ert-deftest hive-mcp-memory-test-get-entry-duration-symbol ()
  "Test that symbol duration is handled."
  (let ((entry '(:id "test" :duration permanent)))
    (should (eq 'permanent (hive-mcp-memory--get-entry-duration entry)))))

(ert-deftest hive-mcp-memory-test-entry-expired-p-not-expired ()
  "Test that future expiration returns nil."
  (let ((future (format-time-string "%FT%T%z"
                                    (time-add (current-time) (days-to-time 7)))))
    (let ((entry (list :id "test" :expires future)))
      (should-not (hive-mcp-memory--entry-expired-p entry)))))

(ert-deftest hive-mcp-memory-test-entry-expired-p-expired ()
  "Test that past expiration returns t."
  (let ((past (format-time-string "%FT%T%z"
                                  (time-subtract (current-time) (days-to-time 1)))))
    (let ((entry (list :id "test" :expires past)))
      (should (hive-mcp-memory--entry-expired-p entry)))))

(ert-deftest hive-mcp-memory-test-entry-expired-p-no-expires ()
  "Test that entry without expires returns nil."
  (let ((entry '(:id "test" :content "permanent")))
    (should-not (hive-mcp-memory--entry-expired-p entry))))

;;;; Test: Content Deduplication

(ert-deftest hive-mcp-memory-test-normalize-content-trims ()
  "Test that whitespace is trimmed."
  (should (equal "hello" (hive-mcp-memory--normalize-content "  hello  ")))
  (should (equal "hello" (hive-mcp-memory--normalize-content "\n\thello\n\t"))))

(ert-deftest hive-mcp-memory-test-normalize-content-collapses-spaces ()
  "Test that multiple spaces are collapsed."
  (should (equal "hello world" (hive-mcp-memory--normalize-content "hello   world")))
  (should (equal "a b c" (hive-mcp-memory--normalize-content "a  b   c"))))

(ert-deftest hive-mcp-memory-test-normalize-content-collapses-newlines ()
  "Test that multiple newlines are collapsed."
  (should (equal "line1\nline2" (hive-mcp-memory--normalize-content "line1\n\n\nline2"))))

(ert-deftest hive-mcp-memory-test-content-hash-deterministic ()
  "Test that same content produces same hash."
  (let ((hash1 (hive-mcp-memory-content-hash "test content"))
        (hash2 (hive-mcp-memory-content-hash "test content")))
    (should (equal hash1 hash2))))

(ert-deftest hive-mcp-memory-test-content-hash-different-content ()
  "Test that different content produces different hashes."
  (let ((hash1 (hive-mcp-memory-content-hash "content A"))
        (hash2 (hive-mcp-memory-content-hash "content B")))
    (should-not (equal hash1 hash2))))

(ert-deftest hive-mcp-memory-test-content-hash-sha256-length ()
  "Test that hash is 64 chars (SHA-256 hex)."
  (let ((hash (hive-mcp-memory-content-hash "anything")))
    (should (= 64 (length hash)))
    (should (string-match-p "^[a-f0-9]+$" hash))))

(ert-deftest hive-mcp-memory-test-content-hash-normalized ()
  "Test that whitespace-different content produces same hash."
  (let ((hash1 (hive-mcp-memory-content-hash "  test  "))
        (hash2 (hive-mcp-memory-content-hash "test")))
    (should (equal hash1 hash2))))

(ert-deftest hive-mcp-memory-test-merge-tags-combines ()
  "Test that tags are merged."
  (should (equal '("a" "b" "c")
                 (hive-mcp-memory--merge-tags '("a" "b") '("c")))))

(ert-deftest hive-mcp-memory-test-merge-tags-dedupes ()
  "Test that duplicate tags are removed."
  (should (equal '("a" "b" "c")
                 (hive-mcp-memory--merge-tags '("a" "b") '("b" "c")))))

;;;; Test: Format Conversion

(ert-deftest hive-mcp-memory-test-plist-to-alist-simple ()
  "Test simple plist to alist conversion."
  (let ((result (hive-mcp-memory--plist-to-alist '(:id "123" :type "note"))))
    (should (assoc 'id result))
    (should (equal "123" (cdr (assoc 'id result))))
    (should (equal "note" (cdr (assoc 'type result))))))

(ert-deftest hive-mcp-memory-test-plist-to-alist-nested ()
  "Test nested plist conversion."
  (let ((result (hive-mcp-memory--plist-to-alist '(:outer (:inner "value")))))
    (should (assoc 'outer result))
    (let ((nested (cdr (assoc 'outer result))))
      (should (assoc 'inner nested)))))

(ert-deftest hive-mcp-memory-test-plist-to-alist-list-to-vector ()
  "Test that lists are converted to vectors for JSON."
  (let ((result (hive-mcp-memory--plist-to-alist '(:tags ("a" "b" "c")))))
    (let ((tags (cdr (assoc 'tags result))))
      (should (vectorp tags))
      (should (equal ["a" "b" "c"] tags)))))

;;;; Test: EDN Parsing

(ert-deftest hive-mcp-memory-test-parse-edn-string ()
  "Test parsing EDN string."
  (should (equal "hello" (hive-mcp-memory--parse-edn-string "\"hello\""))))

(ert-deftest hive-mcp-memory-test-parse-edn-keyword ()
  "Test parsing EDN keyword."
  (should (eq :project-id (hive-mcp-memory--parse-edn-string ":project-id"))))

(ert-deftest hive-mcp-memory-test-parse-edn-number ()
  "Test parsing EDN number."
  (should (= 42 (hive-mcp-memory--parse-edn-string "42")))
  (should (= -7 (hive-mcp-memory--parse-edn-string "-7"))))

(ert-deftest hive-mcp-memory-test-parse-edn-vector ()
  "Test parsing EDN vector."
  (should (equal '("a" "b") (hive-mcp-memory--parse-edn-string "[\"a\" \"b\"]"))))

(ert-deftest hive-mcp-memory-test-parse-edn-map ()
  "Test parsing EDN map."
  (let ((result (hive-mcp-memory--parse-edn-string "{:project-id \"hive-mcp\"}")))
    (should (assoc :project-id result))
    (should (equal "hive-mcp" (cdr (assoc :project-id result))))))

(ert-deftest hive-mcp-memory-test-parse-edn-boolean ()
  "Test parsing EDN boolean."
  (should (eq t (hive-mcp-memory--parse-edn-string "true")))
  (should-not (hive-mcp-memory--parse-edn-string "false"))
  (should-not (hive-mcp-memory--parse-edn-string "nil")))

;;;; Test: EDN Parsing Edge Cases (CLARITY-Y compliance)
;; These tests were added after a production bug where comments caused infinite loops.
;; See ADR: "Prefer Established Libraries Over Custom Parsers"

(ert-deftest hive-mcp-memory-test-parse-edn-with-line-comment ()
  "Test parsing EDN map with line comment - regression test for infinite loop bug."
  (let ((result (hive-mcp-memory--parse-edn-string "{:a 1 ;; comment\n :b 2}")))
    (should (assoc :a result))
    (should (assoc :b result))
    (should (= 1 (cdr (assoc :a result))))
    (should (= 2 (cdr (assoc :b result))))))

(ert-deftest hive-mcp-memory-test-parse-edn-with-leading-comment ()
  "Test parsing EDN with comment before content."
  (let ((result (hive-mcp-memory--parse-edn-string ";; header comment\n{:key \"value\"}")))
    (should (assoc :key result))
    (should (equal "value" (cdr (assoc :key result))))))

(ert-deftest hive-mcp-memory-test-parse-edn-with-multiple-comments ()
  "Test parsing EDN with multiple comment lines."
  (let ((result (hive-mcp-memory--parse-edn-string
                 ";; comment 1\n;; comment 2\n{:project-id \"test\"\n ;; inline\n :enabled true}")))
    (should (assoc :project-id result))
    (should (assoc :enabled result))))

(ert-deftest hive-mcp-memory-test-parse-edn-empty-input ()
  "Test parsing empty string returns nil."
  (should-not (hive-mcp-memory--parse-edn-string "")))

(ert-deftest hive-mcp-memory-test-parse-edn-whitespace-only ()
  "Test parsing whitespace-only string returns nil."
  (should-not (hive-mcp-memory--parse-edn-string "   \n\t  ")))

(ert-deftest hive-mcp-memory-test-parse-edn-comment-only ()
  "Test parsing comment-only string returns nil."
  (should-not (hive-mcp-memory--parse-edn-string ";; just a comment\n")))

(ert-deftest hive-mcp-memory-test-parse-edn-does-not-hang ()
  "Ensure parser completes within timeout - guards against infinite loops."
  (with-timeout (2 (ert-fail "Parser hung - possible infinite loop"))
    (hive-mcp-memory--parse-edn-string
     "{:project-id \"test\"\n ;; Source directories\n :watch-dirs [\"src\"]\n ;; Enable feature\n :enabled true}")))

;;;; Test: Entry Scope Matching

(ert-deftest hive-mcp-memory-test-entry-matches-scope-p-global ()
  "Test that global entries match when scope:global is in applicable scopes."
  (let ((entry '(:id "1" :tags ("scope:global" "other"))))
    ;; Global entries match when scope:global is in the applicable scope list
    (should (hive-mcp-memory--entry-matches-scope-p entry '("scope:global" "scope:project:foo")))
    ;; But NOT when scope:global is absent from the filter
    (should-not (hive-mcp-memory--entry-matches-scope-p entry '("scope:project:foo")))))

(ert-deftest hive-mcp-memory-test-entry-matches-scope-p-project ()
  "Test that project-scoped entry matches its project."
  (let ((entry '(:id "1" :tags ("scope:project:hive-mcp"))))
    (should (hive-mcp-memory--entry-matches-scope-p entry '("scope:project:hive-mcp" "scope:global")))
    (should-not (hive-mcp-memory--entry-matches-scope-p entry '("scope:project:other" "scope:global")))))

(ert-deftest hive-mcp-memory-test-entry-matches-scope-p-no-scope-tag ()
  "Test that entries without scope tags are treated as global."
  (let ((entry '(:id "1" :tags ("regular" "tags"))))
    (should (hive-mcp-memory--entry-matches-scope-p entry '("scope:project:any")))))

(provide 'hive-mcp-memory-test)
;;; hive-mcp-memory-test.el ends here
