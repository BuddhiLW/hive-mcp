;;; hive-mcp-memory-core.el --- Core utilities for memory system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Core utilities shared across the hive-mcp-memory system.
;; Contains: ID generation, timestamps, compatibility functions, helper macros.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles core utilities
;; - Dependency Inversion: Other modules depend on this abstraction
;;
;; This module has NO dependencies on other memory modules.

;;; Code:

(require 'cl-lib)

;;;; Compatibility

;; Compatibility: plistp was added in Emacs 29
(unless (fboundp 'plistp)
  (defun plistp (object)
    "Return non-nil if OBJECT is a plist."
    (and (listp object)
         (or (null object)
             (and (keywordp (car object))
                  (plistp (cddr object)))))))

;;;; ID Generation

(defun hive-mcp-memory-core-generate-id ()
  "Generate a unique ID for memory entries.
Format: YYYYMMDDHHMMSS-8hexchars
The timestamp provides sortability, the hash ensures uniqueness."
  (format "%s-%s"
          (format-time-string "%Y%m%d%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 8)))

;;;; Timestamps

(defun hive-mcp-memory-core-timestamp ()
  "Return current ISO 8601 timestamp with timezone."
  (format-time-string "%FT%T%z"))

;;;; List Utilities

(defun hive-mcp-memory-core-ensure-list (seq)
  "Convert SEQ to a list if it's a vector.
Defensive helper for handling JSON arrays that may come as vectors."
  (if (vectorp seq)
      (append seq nil)
    seq))

(defun hive-mcp-memory-core-merge-lists (existing new &optional test-fn)
  "Merge NEW into EXISTING, removing duplicates.
TEST-FN is the comparison function (default `equal')."
  (seq-uniq (append existing new) (or test-fn #'equal)))

(provide 'hive-mcp-memory-core)
;;; hive-mcp-memory-core.el ends here
