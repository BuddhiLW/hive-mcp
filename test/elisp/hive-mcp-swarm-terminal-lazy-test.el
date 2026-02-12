;;; hive-mcp-swarm-terminal-lazy-test.el --- ERT tests for lazy vterm rendering -*- lexical-binding: t -*-

;; Copyright (C) 2026 BuddhiLW

;; Author: BuddhiLW
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; TDD tests for lazy vterm rendering feature.
;; Tests that invisible vterm buffers skip process output flushing
;; to reduce Emacs memory pressure when running 5+ concurrent lings.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load module under test
(require 'hive-mcp-swarm-terminal)

;;;; Test Fixtures

(defun hive-mcp-swarm-terminal-lazy-test--setup ()
  "Setup test fixtures."
  (setq hive-mcp-swarm-terminal--lazy-tick-count 0))

(defun hive-mcp-swarm-terminal-lazy-test--teardown ()
  "Teardown test fixtures."
  (setq hive-mcp-swarm-terminal--lazy-tick-count 0))

;;;; Test Group 1: Customization Defaults

(ert-deftest hive-mcp-swarm-terminal-lazy-test-default-enabled ()
  "Test that lazy render is enabled by default."
  (should (eq hive-mcp-swarm-terminal-lazy-render t)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-default-interval ()
  "Test that lazy render interval defaults to 10."
  (should (= hive-mcp-swarm-terminal-lazy-render-interval 10)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-tick-count-initial ()
  "Test that lazy tick count starts at 0."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (should (= hive-mcp-swarm-terminal--lazy-tick-count 0))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

;;;; Test Group 2: buffer-visible-p

(ert-deftest hive-mcp-swarm-terminal-lazy-test-visible-p-dead-buffer ()
  "Test that dead buffer is not visible."
  (let ((buf (generate-new-buffer "*test-lazy-dead*")))
    (kill-buffer buf)
    (should-not (hive-mcp-swarm-terminal--buffer-visible-p buf))))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-visible-p-hidden-buffer ()
  "Test that a buffer not displayed in any window is not visible."
  (let ((buf (generate-new-buffer "*test-lazy-hidden*")))
    (unwind-protect
        ;; Buffer exists but is NOT displayed in any window
        (should-not (hive-mcp-swarm-terminal--buffer-visible-p buf))
      (kill-buffer buf))))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-visible-p-displayed-buffer ()
  "Test that a buffer displayed in a window IS visible."
  (let ((buf (generate-new-buffer "*test-lazy-visible*")))
    (unwind-protect
        (progn
          ;; Display the buffer in the current window
          (set-window-buffer (selected-window) buf)
          (should (hive-mcp-swarm-terminal--buffer-visible-p buf)))
      (kill-buffer buf))))

;;;; Test Group 3: should-flush-p

(ert-deftest hive-mcp-swarm-terminal-lazy-test-should-flush-lazy-disabled ()
  "Test that should-flush-p always returns t when lazy render is disabled."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render nil)
            (buf (generate-new-buffer "*test-lazy-flush-off*")))
        (unwind-protect
            ;; Buffer is NOT visible, but lazy render is disabled
            (should (hive-mcp-swarm-terminal--should-flush-p buf))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-should-flush-visible ()
  "Test that should-flush-p returns t for visible buffer with lazy render on."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t)
            (buf (generate-new-buffer "*test-lazy-flush-vis*")))
        (unwind-protect
            (progn
              ;; Display buffer
              (set-window-buffer (selected-window) buf)
              (should (hive-mcp-swarm-terminal--should-flush-p buf)))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-should-flush-invisible-not-on-tick ()
  "Test that should-flush-p returns nil for invisible buffer when not on tick interval."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t)
            (hive-mcp-swarm-terminal-lazy-render-interval 10)
            (hive-mcp-swarm-terminal--lazy-tick-count 3) ; Not a multiple of 10
            (buf (generate-new-buffer "*test-lazy-flush-invis*")))
        (unwind-protect
            ;; Buffer is NOT visible AND tick count is not on interval
            (should-not (hive-mcp-swarm-terminal--should-flush-p buf))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-should-flush-invisible-on-tick ()
  "Test that should-flush-p returns t for invisible buffer ON tick interval."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t)
            (hive-mcp-swarm-terminal-lazy-render-interval 10)
            (hive-mcp-swarm-terminal--lazy-tick-count 10) ; IS a multiple of 10
            (buf (generate-new-buffer "*test-lazy-flush-ontick*")))
        (unwind-protect
            ;; Buffer is NOT visible BUT tick count IS on interval
            (should (hive-mcp-swarm-terminal--should-flush-p buf))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-should-flush-tick-zero ()
  "Test that tick count 0 always flushes (zero mod anything is 0)."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t)
            (hive-mcp-swarm-terminal-lazy-render-interval 10)
            (hive-mcp-swarm-terminal--lazy-tick-count 0)
            (buf (generate-new-buffer "*test-lazy-flush-zero*")))
        (unwind-protect
            ;; Tick 0 should always flush (startup behavior)
            (should (hive-mcp-swarm-terminal--should-flush-p buf))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-should-flush-interval-1 ()
  "Test that interval=1 effectively flushes every tick (disable lazy)."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t)
            (hive-mcp-swarm-terminal-lazy-render-interval 1)
            (buf (generate-new-buffer "*test-lazy-flush-int1*")))
        (unwind-protect
            (progn
              ;; Every tick should flush when interval=1
              (setq hive-mcp-swarm-terminal--lazy-tick-count 1)
              (should (hive-mcp-swarm-terminal--should-flush-p buf))
              (setq hive-mcp-swarm-terminal--lazy-tick-count 5)
              (should (hive-mcp-swarm-terminal--should-flush-p buf))
              (setq hive-mcp-swarm-terminal--lazy-tick-count 99)
              (should (hive-mcp-swarm-terminal--should-flush-p buf)))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

;;;; Test Group 4: flush-buffer

(ert-deftest hive-mcp-swarm-terminal-lazy-test-flush-buffer-dead ()
  "Test that flush-buffer returns nil for dead buffer."
  (let ((buf (generate-new-buffer "*test-lazy-fb-dead*")))
    (kill-buffer buf)
    (should-not (hive-mcp-swarm-terminal--flush-buffer buf))))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-flush-buffer-no-process ()
  "Test that flush-buffer returns nil for buffer without process."
  (let ((buf (generate-new-buffer "*test-lazy-fb-noproc*")))
    (unwind-protect
        (let ((hive-mcp-swarm-terminal-lazy-render nil)) ; Force flush
          ;; Buffer has no process - should return nil gracefully
          (should-not (hive-mcp-swarm-terminal--flush-buffer buf)))
      (kill-buffer buf))))

;;;; Test Group 5: Tick Counter Integration

(ert-deftest hive-mcp-swarm-terminal-lazy-test-tick-increments ()
  "Test that tick counter increments during watcher tick."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-auto-shout t))
        ;; Ensure hive-mcp-swarm--slaves is globally bound (defvar)
        (defvar hive-mcp-swarm--slaves nil)
        (setq hive-mcp-swarm--slaves (make-hash-table :test 'equal))
        (unwind-protect
            (progn
              ;; Run one tick
              (hive-mcp-swarm-terminal--completion-watcher-tick)
              (should (= hive-mcp-swarm-terminal--lazy-tick-count 1))
              ;; Run another tick
              (hive-mcp-swarm-terminal--completion-watcher-tick)
              (should (= hive-mcp-swarm-terminal--lazy-tick-count 2))
              ;; Run 8 more ticks
              (dotimes (_ 8)
                (hive-mcp-swarm-terminal--completion-watcher-tick))
              (should (= hive-mcp-swarm-terminal--lazy-tick-count 10)))
          (setq hive-mcp-swarm--slaves nil)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-tick-resets-on-stop ()
  "Test that tick counter resets when watcher is stopped."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (progn
        (setq hive-mcp-swarm-terminal--lazy-tick-count 42)
        ;; Set timer so stop has something to cancel
        (setq hive-mcp-swarm-terminal--completion-timer
              (run-with-timer 999 nil #'ignore))
        (hive-mcp-swarm-terminal-stop-completion-watcher)
        (should (= hive-mcp-swarm-terminal--lazy-tick-count 0)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

;;;; Test Group 6: Hook Management

(ert-deftest hive-mcp-swarm-terminal-lazy-test-hook-added-on-start ()
  "Test that window-buffer-change hook is added when lazy render is enabled."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t))
        ;; Remove hook first to ensure clean state
        (remove-hook 'window-buffer-change-functions
                     #'hive-mcp-swarm-terminal--on-window-buffer-change)
        ;; Start watcher
        (hive-mcp-swarm-terminal-start-completion-watcher #'ignore)
        (unwind-protect
            (should (memq #'hive-mcp-swarm-terminal--on-window-buffer-change
                          window-buffer-change-functions))
          (hive-mcp-swarm-terminal-stop-completion-watcher)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-hook-not-added-when-disabled ()
  "Test that window-buffer-change hook is NOT added when lazy render is disabled."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render nil))
        ;; Remove hook first to ensure clean state
        (remove-hook 'window-buffer-change-functions
                     #'hive-mcp-swarm-terminal--on-window-buffer-change)
        ;; Start watcher with lazy render OFF
        (hive-mcp-swarm-terminal-start-completion-watcher #'ignore)
        (unwind-protect
            (should-not (memq #'hive-mcp-swarm-terminal--on-window-buffer-change
                              window-buffer-change-functions))
          (hive-mcp-swarm-terminal-stop-completion-watcher)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-hook-removed-on-stop ()
  "Test that window-buffer-change hook is removed when watcher stops."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t))
        ;; Start and then stop
        (hive-mcp-swarm-terminal-start-completion-watcher #'ignore)
        (hive-mcp-swarm-terminal-stop-completion-watcher)
        (should-not (memq #'hive-mcp-swarm-terminal--on-window-buffer-change
                          window-buffer-change-functions)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

;;;; Test Group 7: Flush Frequency Math

(ert-deftest hive-mcp-swarm-terminal-lazy-test-flush-frequency ()
  "Test that invisible buffers flush exactly at interval multiples."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t)
            (hive-mcp-swarm-terminal-lazy-render-interval 5)
            (buf (generate-new-buffer "*test-lazy-freq*"))
            (flush-count 0))
        (unwind-protect
            (progn
              ;; Simulate 20 ticks
              (dotimes (i 20)
                (setq hive-mcp-swarm-terminal--lazy-tick-count i)
                (when (hive-mcp-swarm-terminal--should-flush-p buf)
                  (cl-incf flush-count)))
              ;; With interval=5 over 20 ticks (0-19):
              ;; Flush at 0, 5, 10, 15 = 4 flushes
              (should (= flush-count 4)))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-visible-always-flushes ()
  "Test that visible buffers flush every tick regardless of counter."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t)
            (hive-mcp-swarm-terminal-lazy-render-interval 100) ; Very high interval
            (buf (generate-new-buffer "*test-lazy-vis-always*"))
            (flush-count 0))
        (unwind-protect
            (progn
              ;; Display the buffer
              (set-window-buffer (selected-window) buf)
              ;; Simulate 10 ticks - ALL should flush since buffer is visible
              (dotimes (i 10)
                (setq hive-mcp-swarm-terminal--lazy-tick-count (1+ i))
                (when (hive-mcp-swarm-terminal--should-flush-p buf)
                  (cl-incf flush-count)))
              (should (= flush-count 10)))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

;;;; Test Group 8: Edge Cases

(ert-deftest hive-mcp-swarm-terminal-lazy-test-interval-zero-protected ()
  "Test that interval=0 doesn't cause division-by-zero (max 1 guard)."
  (hive-mcp-swarm-terminal-lazy-test--setup)
  (unwind-protect
      (let ((hive-mcp-swarm-terminal-lazy-render t)
            (hive-mcp-swarm-terminal-lazy-render-interval 0) ; Edge case
            (hive-mcp-swarm-terminal--lazy-tick-count 7)
            (buf (generate-new-buffer "*test-lazy-zero-int*")))
        (unwind-protect
            ;; Should not error - max 1 guard prevents div by zero
            ;; With interval=0, (max 1 0) = 1, so flushes every tick
            (should (hive-mcp-swarm-terminal--should-flush-p buf))
          (kill-buffer buf)))
    (hive-mcp-swarm-terminal-lazy-test--teardown)))

(ert-deftest hive-mcp-swarm-terminal-lazy-test-on-window-buffer-change-noop-when-disabled ()
  "Test that window-buffer-change handler is a no-op when lazy render is off."
  (let ((hive-mcp-swarm-terminal-lazy-render nil))
    ;; Should not error even when called directly
    (hive-mcp-swarm-terminal--on-window-buffer-change (selected-frame))))

(provide 'hive-mcp-swarm-terminal-lazy-test)
;;; hive-mcp-swarm-terminal-lazy-test.el ends here
