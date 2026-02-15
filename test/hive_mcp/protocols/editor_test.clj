(ns hive-mcp.protocols.editor-test
  "Contract tests for IEditor protocol implementations."
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [hive-mcp.protocols.editor :as ed]
            [hive-mcp.emacs.editor-adapter :as ema]
            [hive-dsl.result :as result]))

;; =============================================================================
;; Fixture: clean editor atom between tests
;; =============================================================================

(use-fixtures :each
  (fn [f]
    (ed/clear-editor!)
    (f)
    (ed/clear-editor!)))

;; =============================================================================
;; NoopEditor contract tests
;; =============================================================================

(deftest noop-editor-id-test
  (is (= :noop (ed/editor-id (ed/noop-editor)))))

(deftest noop-editor-available?-test
  (is (false? (ed/available? (ed/noop-editor)))))

(deftest noop-editor-eval-expr-returns-err-test
  (testing "1-arity eval-expr returns error Result"
    (let [r (ed/eval-expr (ed/noop-editor) "(+ 1 2)")]
      (is (result/err? r))
      (is (= :editor/not-available (:error r)))))
  (testing "2-arity eval-expr returns error Result"
    (let [r (ed/eval-expr (ed/noop-editor) "(+ 1 2)" {:timeout-ms 5000})]
      (is (result/err? r))
      (is (= :editor/not-available (:error r))))))

(deftest noop-editor-feature-available?-test
  (is (false? (ed/feature-available? (ed/noop-editor) "hive-mcp-swarm"))))

(deftest noop-editor-send-to-terminal-returns-err-test
  (let [r (ed/send-to-terminal (ed/noop-editor) "ling-1" "hello")]
    (is (result/err? r))
    (is (= :editor/not-available (:error r)))))

;; =============================================================================
;; Active editor lifecycle tests
;; =============================================================================

(deftest get-editor-returns-noop-when-unset-test
  (is (instance? hive_mcp.protocols.editor.NoopEditor (ed/get-editor))))

(deftest set-get-clear-lifecycle-test
  (let [editor (ema/->emacsclient-editor)]
    (ed/set-editor! editor)
    (is (ed/editor-set?))
    (is (= :emacsclient (ed/editor-id (ed/get-editor))))
    (ed/clear-editor!)
    (is (not (ed/editor-set?)))
    (is (= :noop (ed/editor-id (ed/get-editor))))))

(deftest set-editor!-rejects-non-ieditor-test
  (is (thrown? AssertionError (ed/set-editor! {:not "an editor"}))))

;; =============================================================================
;; EmacsclientEditor contract tests (with-redefs on ec/ fns)
;; =============================================================================

(deftest emacsclient-editor-id-test
  (is (= :emacsclient (ed/editor-id (ema/->emacsclient-editor)))))

(deftest emacsclient-eval-expr-success-test
  (with-redefs [hive-mcp.emacs.client/eval-elisp
                (fn [_code] {:success true :result "42"})]
    (let [r (ed/eval-expr (ema/->emacsclient-editor) "(+ 1 2)")]
      (is (result/ok? r))
      (is (= "42" (:ok r))))))

(deftest emacsclient-eval-expr-failure-test
  (with-redefs [hive-mcp.emacs.client/eval-elisp
                (fn [_code] {:success false :error "void-function foo"})]
    (let [r (ed/eval-expr (ema/->emacsclient-editor) "(foo)")]
      (is (result/err? r))
      (is (= :editor/eval-failed (:error r))))))

(deftest emacsclient-eval-expr-timeout-test
  (with-redefs [hive-mcp.emacs.client/eval-elisp-with-timeout
                (fn [_code _ms] {:success false :error "timed out" :timed-out true})]
    (let [r (ed/eval-expr (ema/->emacsclient-editor) "(sleep 100)" {:timeout-ms 100})]
      (is (result/err? r))
      (is (= :editor/timeout (:error r))))))

(deftest emacsclient-available?-test
  (with-redefs [hive-mcp.emacs.client/emacs-running? (fn [] true)]
    (is (true? (ed/available? (ema/->emacsclient-editor)))))
  (with-redefs [hive-mcp.emacs.client/emacs-running? (fn [] false)]
    (is (false? (ed/available? (ema/->emacsclient-editor))))))

(deftest emacsclient-feature-available?-test
  (with-redefs [hive-mcp.emacs.client/eval-elisp
                (fn [code]
                  (if (re-find #"featurep" code)
                    {:success true :result "t"}
                    {:success false :error "unexpected"}))]
    (is (true? (ed/feature-available? (ema/->emacsclient-editor) "hive-mcp-swarm"))))
  (with-redefs [hive-mcp.emacs.client/eval-elisp
                (fn [_code] {:success true :result "nil"})]
    (is (false? (ed/feature-available? (ema/->emacsclient-editor) "missing-feature")))))

;; =============================================================================
;; Property: IEditor methods never throw
;; =============================================================================

(deftest noop-methods-never-throw-test
  (testing "Every NoopEditor method returns a value, never throws"
    (let [noop (ed/noop-editor)]
      (are [expr] (some? expr)
        (ed/editor-id noop)
        (ed/eval-expr noop "(anything)")
        (ed/eval-expr noop "(anything)" {:timeout-ms 1000})
        (ed/send-to-terminal noop "id" "text"))
      ;; available? and feature-available? return false (not nil)
      (is (false? (ed/available? noop)))
      (is (false? (ed/feature-available? noop "x"))))))
