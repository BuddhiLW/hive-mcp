;;; hive-mcp-swarm-terminal-idle-test.el --- ERT tests for Layer 2 idle detection -*- lexical-binding: t -*-

;; Copyright (C) 2025 BuddhiLW

;; Author: BuddhiLW
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; TDD tests for Layer 2: Terminal introspection for ling completion detection.
;; Tests the idle detection mechanism that detects when lings go silent
;; without shouting progress.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load modules under test
(require 'hive-mcp-swarm-terminal)

;;;; Test Fixtures

(defvar hive-mcp-swarm-terminal-idle-test--mock-shouts nil
  "Mock shout registry for testing. Maps slave-id -> last-shout-time.")

(defun hive-mcp-swarm-terminal-idle-test--setup ()
  "Setup test fixtures."
  (setq hive-mcp-swarm-terminal-idle-test--mock-shouts (make-hash-table :test 'equal))
  ;; Reset global state
  (when (boundp 'hive-mcp-swarm-terminal--activity-timestamps)
    (clrhash hive-mcp-swarm-terminal--activity-timestamps))
  (when (boundp 'hive-mcp-swarm-terminal--last-shout-timestamps)
    (clrhash hive-mcp-swarm-terminal--last-shout-timestamps)))

(defun hive-mcp-swarm-terminal-idle-test--teardown ()
  "Teardown test fixtures."
  (setq hive-mcp-swarm-terminal-idle-test--mock-shouts nil))

;;;; Test Group 1: Activity Timestamp Tracking

(ert-deftest hive-mcp-swarm-terminal-idle-test-record-activity ()
  "Test that activity timestamps are recorded correctly."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-test-1"))
        ;; Record activity
        (hive-mcp-swarm-terminal--record-activity slave-id)
        ;; Verify timestamp exists and is recent
        (let ((timestamp (hive-mcp-swarm-terminal--get-activity-timestamp slave-id)))
          (should timestamp)
          (should (numberp timestamp))
          ;; Should be within last second
          (should (< (- (float-time) timestamp) 1.0))))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-idle-test-activity-updates ()
  "Test that activity timestamps update on subsequent calls."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-test-1"))
        ;; Record initial activity
        (hive-mcp-swarm-terminal--record-activity slave-id)
        (let ((first-time (hive-mcp-swarm-terminal--get-activity-timestamp slave-id)))
          ;; Wait a bit
          (sleep-for 0.1)
          ;; Record new activity
          (hive-mcp-swarm-terminal--record-activity slave-id)
          (let ((second-time (hive-mcp-swarm-terminal--get-activity-timestamp slave-id)))
            ;; Second timestamp should be newer
            (should (> second-time first-time)))))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

;;;; Test Group 2: Shout Timestamp Tracking

(ert-deftest hive-mcp-swarm-terminal-idle-test-record-shout ()
  "Test that shout timestamps are recorded correctly."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-test-1"))
        ;; Record shout
        (hive-mcp-swarm-terminal--record-shout slave-id)
        ;; Verify timestamp exists
        (let ((timestamp (hive-mcp-swarm-terminal--get-shout-timestamp slave-id)))
          (should timestamp)
          (should (numberp timestamp))
          (should (< (- (float-time) timestamp) 1.0))))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-idle-test-no-shout-returns-nil ()
  "Test that unknown slave returns nil for shout timestamp."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-unknown-999"))
        (should-not (hive-mcp-swarm-terminal--get-shout-timestamp slave-id)))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

;;;; Test Group 3: Idle Detection Logic

(ert-deftest hive-mcp-swarm-terminal-idle-test-not-idle-with-recent-activity ()
  "Test that slave with recent activity is not considered idle."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-test-1")
            (hive-mcp-swarm-terminal-idle-timeout 5.0)) ; 5 second timeout
        ;; Record recent activity
        (hive-mcp-swarm-terminal--record-activity slave-id)
        ;; Should not be idle
        (should-not (hive-mcp-swarm-terminal--slave-idle-p slave-id)))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-idle-test-not-idle-with-recent-shout ()
  "Test that slave with recent shout is not considered idle."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-test-1")
            (hive-mcp-swarm-terminal-idle-timeout 5.0))
        ;; Record old activity
        (puthash slave-id (- (float-time) 10.0)
                 hive-mcp-swarm-terminal--activity-timestamps)
        ;; But recent shout
        (hive-mcp-swarm-terminal--record-shout slave-id)
        ;; Should not be idle because of recent shout
        (should-not (hive-mcp-swarm-terminal--slave-idle-p slave-id)))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-idle-test-idle-with-no-activity-or-shout ()
  "Test that slave with no recent activity AND no recent shout IS idle."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-test-1")
            (hive-mcp-swarm-terminal-idle-timeout 5.0))
        ;; Record old activity (10 seconds ago)
        (puthash slave-id (- (float-time) 10.0)
                 hive-mcp-swarm-terminal--activity-timestamps)
        ;; Record old shout (10 seconds ago)
        (puthash slave-id (- (float-time) 10.0)
                 hive-mcp-swarm-terminal--last-shout-timestamps)
        ;; Should be idle
        (should (hive-mcp-swarm-terminal--slave-idle-p slave-id)))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-idle-test-not-idle-unknown-slave ()
  "Test that unknown slave (no activity record) is not considered idle."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-unknown-999")
            (hive-mcp-swarm-terminal-idle-timeout 5.0))
        ;; Unknown slave should not be considered idle
        (should-not (hive-mcp-swarm-terminal--slave-idle-p slave-id)))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

;;;; Test Group 4: Idle Detection with Working State

(ert-deftest hive-mcp-swarm-terminal-idle-test-idle-only-when-working ()
  "Test that idle detection only triggers for slaves in working state."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let* ((slave-id "swarm-test-1")
             (hive-mcp-swarm-terminal-idle-timeout 5.0)
             (mock-buffer (generate-new-buffer "*test-swarm*")))
        ;; Create global binding for hive-mcp-swarm--slaves
        (defvar hive-mcp-swarm--slaves nil)
        (setq hive-mcp-swarm--slaves (make-hash-table :test 'equal))
        (unwind-protect
            (progn
              ;; Setup mock slave NOT working
              (puthash slave-id (list :buffer mock-buffer :status 'idle)
                       hive-mcp-swarm--slaves)
              ;; Record old activity
              (puthash slave-id (- (float-time) 10.0)
                       hive-mcp-swarm-terminal--activity-timestamps)
              ;; Old shout
              (puthash slave-id (- (float-time) 10.0)
                       hive-mcp-swarm-terminal--last-shout-timestamps)
              ;; Should NOT trigger idle because status is 'idle not 'working
              (should-not (hive-mcp-swarm-terminal--slave-needs-idle-event-p slave-id))
              ;; Now set to working
              (puthash slave-id (list :buffer mock-buffer :status 'working)
                       hive-mcp-swarm--slaves)
              ;; NOW should trigger
              (should (hive-mcp-swarm-terminal--slave-needs-idle-event-p slave-id)))
          (kill-buffer mock-buffer)
          (setq hive-mcp-swarm--slaves nil)))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

;;;; Test Group 5: Clear Functions

(ert-deftest hive-mcp-swarm-terminal-idle-test-clear-slave-timestamps ()
  "Test that clearing slave removes all timestamp records."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-test-1"))
        ;; Record activity and shout
        (hive-mcp-swarm-terminal--record-activity slave-id)
        (hive-mcp-swarm-terminal--record-shout slave-id)
        ;; Verify they exist
        (should (hive-mcp-swarm-terminal--get-activity-timestamp slave-id))
        (should (hive-mcp-swarm-terminal--get-shout-timestamp slave-id))
        ;; Clear
        (hive-mcp-swarm-terminal--clear-slave-timestamps slave-id)
        ;; Verify removed
        (should-not (hive-mcp-swarm-terminal--get-activity-timestamp slave-id))
        (should-not (hive-mcp-swarm-terminal--get-shout-timestamp slave-id)))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

;;;; Test Group 6: Idle Timeout Customization

(ert-deftest hive-mcp-swarm-terminal-idle-test-respects-timeout-setting ()
  "Test that idle detection respects the configurable timeout."
  (hive-mcp-swarm-terminal-idle-test--setup)
  (unwind-protect
      (let ((slave-id "swarm-test-1"))
        ;; Activity 3 seconds ago
        (puthash slave-id (- (float-time) 3.0)
                 hive-mcp-swarm-terminal--activity-timestamps)
        ;; No shout record
        ;; With 5 second timeout - NOT idle
        (let ((hive-mcp-swarm-terminal-idle-timeout 5.0))
          (should-not (hive-mcp-swarm-terminal--slave-idle-p slave-id)))
        ;; With 2 second timeout - IS idle
        (let ((hive-mcp-swarm-terminal-idle-timeout 2.0))
          (should (hive-mcp-swarm-terminal--slave-idle-p slave-id))))
    (hive-mcp-swarm-terminal-idle-test--teardown)))

(provide 'hive-mcp-swarm-terminal-idle-test)
;;; hive-mcp-swarm-terminal-idle-test.el ends here
