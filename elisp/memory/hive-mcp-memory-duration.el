;;; hive-mcp-memory-duration.el --- Duration/lifespan system for memory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Duration/lifespan system for hive-mcp-memory.
;; Handles entry expiration, promotion, and demotion.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles duration/expiration logic
;; - Open/Closed: New duration categories can be added via customization
;;
;; Duration hierarchy (shortest to longest):
;;   session    - Expires immediately (0 days)
;;   short-term - Expires in 7 days
;;   long-term  - Expires in 90 days
;;   permanent  - Never expires

;;; Code:

(require 'seq)
(require 'hive-mcp-memory-core)

;;;; Constants

(defconst hive-mcp-memory-duration-list '(session short-term long-term permanent)
  "Valid duration categories, ordered shortest to longest.")

;;;; Customization

(defcustom hive-mcp-memory-duration-days
  '((session . 0) (short-term . 7) (long-term . 90) (permanent . nil))
  "Days before expiration per duration. nil = never expires."
  :type '(alist :key-type symbol :value-type (choice integer (const nil)))
  :group 'hive-mcp-memory)

(defcustom hive-mcp-memory-duration-default 'long-term
  "Default duration for new entries."
  :type 'symbol
  :group 'hive-mcp-memory)

;;;; Expiration Calculation

(defun hive-mcp-memory-duration-calculate-expires (duration)
  "Calculate expiration timestamp for DURATION.
Returns ISO 8601 timestamp or nil for permanent entries."
  (let ((days (alist-get duration hive-mcp-memory-duration-days)))
    (cond
     ((null days) nil)  ; permanent - never expires
     ((= days 0) (hive-mcp-memory-core-timestamp))  ; session - expires immediately
     (t (format-time-string "%FT%T%z"
                            (time-add (current-time)
                                      (days-to-time days)))))))

;;;; Entry Duration Accessors

(defun hive-mcp-memory-duration-get (entry)
  "Get the duration of ENTRY, defaulting to `long-term' for legacy entries."
  (let ((dur (plist-get entry :duration)))
    (if dur
        (if (stringp dur) (intern dur) dur)
      'long-term)))

(defun hive-mcp-memory-duration-expired-p (entry)
  "Return non-nil if ENTRY has expired."
  (when-let* ((expires (plist-get entry :expires)))
    (let ((expires-time (date-to-time expires)))
      (time-less-p expires-time (current-time)))))

;;;; Duration Shifting

(defun hive-mcp-memory-duration-next (current-duration)
  "Return next longer duration after CURRENT-DURATION, or nil if permanent."
  (let* ((pos (seq-position hive-mcp-memory-duration-list current-duration))
         (next-pos (when pos (1+ pos))))
    (when (and next-pos (< next-pos (length hive-mcp-memory-duration-list)))
      (nth next-pos hive-mcp-memory-duration-list))))

(defun hive-mcp-memory-duration-prev (current-duration)
  "Return next shorter duration before CURRENT-DURATION, or nil if session."
  (let* ((pos (seq-position hive-mcp-memory-duration-list current-duration))
         (prev-pos (when (and pos (> pos 0)) (1- pos))))
    (when prev-pos
      (nth prev-pos hive-mcp-memory-duration-list))))

;;;; Validation

(defun hive-mcp-memory-duration-valid-p (duration)
  "Return non-nil if DURATION is a valid duration category."
  (memq duration hive-mcp-memory-duration-list))

(defun hive-mcp-memory-duration-validate (duration)
  "Validate DURATION is a valid duration category.
Signals an error if invalid, returns DURATION if valid."
  (unless (hive-mcp-memory-duration-valid-p duration)
    (error "Invalid duration: %s. Must be one of %s"
           duration hive-mcp-memory-duration-list))
  duration)

(provide 'hive-mcp-memory-duration)
;;; hive-mcp-memory-duration.el ends here
