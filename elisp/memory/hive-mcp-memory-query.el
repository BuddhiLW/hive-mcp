;;; hive-mcp-memory-query.el --- Query operations for memory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Query operations for hive-mcp-memory.
;; Handles query, search, expiring entries, and audit logging.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles read/query operations
;; - Interface Segregation: Separate from CRUD write operations

;;; Code:

(require 'seq)
(require 'hive-mcp-memory-core)
(require 'hive-mcp-memory-project)
(require 'hive-mcp-memory-scope)
(require 'hive-mcp-memory-duration)
(require 'hive-mcp-memory-storage)

;; Forward declarations for CRUD (avoids circular dependency)
(declare-function hive-mcp-memory-crud-get "hive-mcp-memory-crud")
(declare-function hive-mcp-memory-crud-update "hive-mcp-memory-crud")

;;;; Query Functions

(defun hive-mcp-memory-query (type &optional tags project-id limit duration scope-filter)
  "Query memories by TYPE and optional TAGS.
PROJECT-ID specifies the project (defaults to current).
Returns list of matching entries, most recent first.
LIMIT caps the number of results.
DURATION filters by lifespan category.
Entries without :duration are treated as long-term for
backwards compatibility.
SCOPE-FILTER controls scope filtering:
  - nil: apply automatic scope filtering (global + current project)
  - t or \\='all: return all entries regardless of scope
  - \\='global: return only scope:global entries
  - string: filter by specific scope tag"
  (let* ((pid (or project-id (hive-mcp-memory-project-id)))
         (type-str (if (symbolp type) (symbol-name type) type))
         (data (hive-mcp-memory-storage-get-data pid type-str))
         (results data))
    ;; Apply scope filtering
    (unless (or (eq scope-filter t) (eq scope-filter 'all))
      (let ((applicable-scopes
             (cond
              ((eq scope-filter 'global) (list "scope:global"))
              ((stringp scope-filter) (list scope-filter "scope:global"))
              (t (hive-mcp-memory-scope-applicable-tags)))))
        (setq results
              (seq-filter
               (lambda (entry)
                 (hive-mcp-memory-scope-entry-matches-p entry applicable-scopes))
               results))))
    ;; Filter by tags if provided (non-scope tags)
    (when tags
      (let ((non-scope-tags (seq-remove (lambda (tag) (string-prefix-p "scope:" tag)) tags)))
        (when non-scope-tags
          (setq results
                (seq-filter
                 (lambda (entry)
                   (let ((entry-tags (plist-get entry :tags)))
                     ;; Handle both list and vector tags (vectors from JSON)
                     (seq-every-p (lambda (tag) (seq-contains-p entry-tags tag #'equal))
                                  non-scope-tags)))
                 results)))))
    ;; Filter by duration if provided
    (when duration
      (let ((dur-str (symbol-name duration)))
        (setq results
              (seq-filter
               (lambda (entry)
                 (let ((entry-dur (or (plist-get entry :duration) "long-term")))
                   (string= entry-dur dur-str)))
               results))))
    ;; Apply limit
    (if (and limit (> (length results) limit))
        (seq-take results limit)
      results)))

;;;; Expiring Entries

(defun hive-mcp-memory-query-expiring (days &optional project-id)
  "Return entries expiring within DAYS from PROJECT-ID.
Does not include already-expired or permanent entries."
  (let* ((pid (or project-id (hive-mcp-memory-project-id)))
         (threshold (time-add (current-time) (days-to-time days)))
         (results '()))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (dolist (entry (hive-mcp-memory-storage-get-data pid type))
        (when-let* ((expires (plist-get entry :expires)))
          (let ((expires-time (date-to-time expires)))
            ;; Include if: not yet expired AND expires within threshold
            (when (and (time-less-p (current-time) expires-time)
                       (time-less-p expires-time threshold))
              (push entry results))))))
    ;; Sort by expiration date (soonest first)
    (sort results
          (lambda (a b)
            (time-less-p (date-to-time (plist-get a :expires))
                         (date-to-time (plist-get b :expires)))))))

(defun hive-mcp-memory-cleanup-expired (&optional project-id)
  "Delete all expired entries from PROJECT-ID.
Returns the count of deleted entries."
  (let* ((pid (or project-id (hive-mcp-memory-project-id)))
         (count 0))
    (dolist (type '("note" "snippet" "convention" "decision" "conversation"))
      (let* ((data (hive-mcp-memory-storage-get-data pid type))
             (new-data (seq-remove
                        (lambda (entry)
                          (when (hive-mcp-memory-duration-expired-p entry)
                            (setq count (1+ count))
                            t))
                        data)))
        (when (< (length new-data) (length data))
          (hive-mcp-memory-storage-set-data pid type new-data))))
    count))

;;;; Audit Log Functions

(defun hive-mcp-memory-query-log-access (id &optional project-id)
  "Log access to memory entry ID.
Increments :access-count and updates :last-accessed timestamp.
PROJECT-ID specifies the project (defaults to current).
Returns the updated entry, or nil if not found."
  (when-let* ((entry (hive-mcp-memory-crud-get id project-id)))
    (let* ((access-count (or (plist-get entry :access-count) 0))
           (new-count (1+ access-count)))
      (hive-mcp-memory-crud-update id
                                    (list :access-count new-count
                                          :last-accessed (hive-mcp-memory-core-timestamp))
                                    project-id)
      ;; Return updated entry
      (hive-mcp-memory-crud-get id project-id))))

(defun hive-mcp-memory-query-mark-helpful (id &optional project-id)
  "Mark memory entry ID as helpful.
Increments :helpful-count.
PROJECT-ID specifies the project (defaults to current).
Returns the updated entry, or nil if not found."
  (when-let* ((entry (hive-mcp-memory-crud-get id project-id)))
    (let* ((helpful-count (or (plist-get entry :helpful-count) 0))
           (new-count (1+ helpful-count)))
      (hive-mcp-memory-crud-update id
                                    (list :helpful-count new-count)
                                    project-id)
      ;; Return updated entry
      (hive-mcp-memory-crud-get id project-id))))

(defun hive-mcp-memory-query-mark-unhelpful (id &optional project-id)
  "Mark memory entry ID as unhelpful.
Increments :unhelpful-count.
PROJECT-ID specifies the project (defaults to current).
Returns the updated entry, or nil if not found."
  (when-let* ((entry (hive-mcp-memory-crud-get id project-id)))
    (let* ((unhelpful-count (or (plist-get entry :unhelpful-count) 0))
           (new-count (1+ unhelpful-count)))
      (hive-mcp-memory-crud-update id
                                    (list :unhelpful-count new-count)
                                    project-id)
      ;; Return updated entry
      (hive-mcp-memory-crud-get id project-id))))

(defun hive-mcp-memory-query-helpfulness-ratio (id &optional project-id)
  "Calculate helpfulness ratio for memory entry ID.
Returns helpful-count / (helpful-count + unhelpful-count).
Returns nil if no feedback has been given (to avoid division by zero).
PROJECT-ID specifies the project (defaults to current)."
  (when-let* ((entry (hive-mcp-memory-crud-get id project-id)))
    (let* ((helpful (or (plist-get entry :helpful-count) 0))
           (unhelpful (or (plist-get entry :unhelpful-count) 0))
           (total (+ helpful unhelpful)))
      (if (zerop total)
          nil  ; No feedback yet
        (/ (float helpful) total)))))

(provide 'hive-mcp-memory-query)
;;; hive-mcp-memory-query.el ends here
