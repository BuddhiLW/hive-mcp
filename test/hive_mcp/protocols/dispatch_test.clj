(ns hive-mcp.protocols.dispatch-test
  "Tests for IDispatchContext protocol, TextContext, and RefContext records.

   Validates:
   - TextContext satisfies IDispatchContext protocol
   - resolve-context returns correct shape {:prompt <string>}
   - context-type returns :text for TextContext
   - RefContext satisfies IDispatchContext with :ref type
   - RefContext resolve-context includes refs and kg-nodes when present
   - ensure-context wraps plain strings into TextContext
   - ensure-context passes through existing IDispatchContext instances
   - ->graph-context falls back to TextContext when hive-knowledge unavailable
   - ->text-context and ->ref-context factory functions"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.protocols.dispatch :as dispatch]))

;;; ============================================================================
;;; Protocol Satisfaction
;;; ============================================================================

(deftest text-context-satisfies-protocol-test
  (testing "TextContext satisfies IDispatchContext"
    (let [ctx (dispatch/->text-context "hello")]
      (is (satisfies? dispatch/IDispatchContext ctx)))))

;;; ============================================================================
;;; TextContext - resolve-context
;;; ============================================================================

(deftest text-context-resolve-context-test
  (testing "resolve-context returns map with :prompt key"
    (let [ctx (dispatch/->text-context "do the thing")
          result (dispatch/resolve-context ctx)]
      (is (map? result))
      (is (= "do the thing" (:prompt result)))))

  (testing "resolve-context returns minimal shape (only :prompt)"
    (let [ctx (dispatch/->text-context "task")
          result (dispatch/resolve-context ctx)]
      (is (= {:prompt "task"} result)))))

;;; ============================================================================
;;; TextContext - context-type
;;; ============================================================================

(deftest text-context-type-test
  (testing "context-type returns :text for TextContext"
    (let [ctx (dispatch/->text-context "hello")]
      (is (= :text (dispatch/context-type ctx))))))

;;; ============================================================================
;;; ensure-context
;;; ============================================================================

(deftest ensure-context-wraps-string-test
  (testing "ensure-context wraps a plain string into TextContext"
    (let [ctx (dispatch/ensure-context "plain string")]
      (is (satisfies? dispatch/IDispatchContext ctx))
      (is (= :text (dispatch/context-type ctx)))
      (is (= {:prompt "plain string"} (dispatch/resolve-context ctx)))))

  (testing "ensure-context stringifies non-string non-context values"
    (let [ctx (dispatch/ensure-context 42)]
      (is (satisfies? dispatch/IDispatchContext ctx))
      (is (= {:prompt "42"} (dispatch/resolve-context ctx))))))

(deftest ensure-context-passthrough-test
  (testing "ensure-context passes through existing IDispatchContext"
    (let [original (dispatch/->text-context "already wrapped")
          result (dispatch/ensure-context original)]
      (is (identical? original result))
      (is (= {:prompt "already wrapped"} (dispatch/resolve-context result))))))

;;; ============================================================================
;;; ->graph-context fallback
;;; ============================================================================

(deftest graph-context-fallback-test
  (testing "->graph-context falls back to TextContext when hive-knowledge not on classpath"
    (let [ctx (dispatch/->graph-context "node-123" nil)]
      (is (satisfies? dispatch/IDispatchContext ctx))
      (is (= :text (dispatch/context-type ctx)))
      (is (string? (:prompt (dispatch/resolve-context ctx))))
      (is (clojure.string/includes?
           (:prompt (dispatch/resolve-context ctx))
           "node-123")))))

;;; ============================================================================
;;; Factory function
;;; ============================================================================

(deftest text-context-factory-test
  (testing "->text-context creates a TextContext record"
    (let [ctx (dispatch/->text-context "test prompt")]
      (is (instance? hive_mcp.protocols.dispatch.TextContext ctx))
      (is (= "test prompt" (:prompt ctx))))))

;;; ============================================================================
;;; RefContext - Protocol Satisfaction
;;; ============================================================================

(deftest ref-context-satisfies-protocol-test
  (testing "RefContext satisfies IDispatchContext"
    (let [ctx (dispatch/->ref-context "hello" {})]
      (is (satisfies? dispatch/IDispatchContext ctx)))))

;;; ============================================================================
;;; RefContext - context-type
;;; ============================================================================

(deftest ref-context-type-test
  (testing "context-type returns :ref for RefContext"
    (let [ctx (dispatch/->ref-context "hello" {})]
      (is (= :ref (dispatch/context-type ctx))))))

;;; ============================================================================
;;; RefContext - resolve-context
;;; ============================================================================

(deftest ref-context-resolve-prompt-only-test
  (testing "resolve-context includes prompt (default reconstruct-fn produces minimal context)"
    (let [ctx (dispatch/->ref-context "my task" {})
          result (dispatch/resolve-context ctx)]
      (is (map? result))
      ;; Default reconstruct-fn resolves via requiring-resolve and produces
      ;; minimal output even with empty refs/kg-nodes. Prompt is appended.
      (is (str/includes? (:prompt result) "my task"))
      ;; Reconstructed is non-nil because default fn produces minimal context
      (is (string? (:reconstructed result))))))

(deftest ref-context-resolve-with-refs-test
  (testing "resolve-context includes :refs when ctx-refs provided"
    (let [ctx (dispatch/->ref-context "task" {:ctx-refs {:axioms "ctx-a" :conv "ctx-b"}})
          result (dispatch/resolve-context ctx)]
      (is (seq (:refs result)))
      (is (= 2 (count (:refs result)))))))

(deftest ref-context-resolve-with-kg-nodes-test
  (testing "resolve-context includes :kg-nodes when kg-node-ids provided"
    (let [ctx (dispatch/->ref-context "task" {:kg-node-ids ["node-1" "node-2"]})
          result (dispatch/resolve-context ctx)]
      (is (= ["node-1" "node-2"] (:kg-nodes result))))))

(deftest ref-context-resolve-with-reconstruct-fn-test
  (testing "resolve-context calls reconstruct-fn and prepends result"
    (let [mock-fn (fn [_refs _nodes _scope] "## Compressed Context\nKey insight here")
          ctx (dispatch/->ref-context "implement X"
                                      {:reconstruct-fn mock-fn
                                       :scope "test"})
          result (dispatch/resolve-context ctx)]
      (is (str/includes? (:prompt result) "Compressed Context"))
      (is (str/includes? (:prompt result) "implement X"))
      (is (= "## Compressed Context\nKey insight here" (:reconstructed result))))))

(deftest ref-context-resolve-reconstruct-failure-test
  (testing "resolve-context degrades gracefully when reconstruct-fn throws"
    (let [bad-fn (fn [_ _ _] (throw (RuntimeException. "kaboom")))
          ctx (dispatch/->ref-context "do stuff" {:reconstruct-fn bad-fn})
          result (dispatch/resolve-context ctx)]
      (is (map? result))
      ;; Should still have a prompt (with error + original)
      (is (str/includes? (:prompt result) "do stuff"))
      (is (str/includes? (:prompt result) "kaboom")))))

;;; ============================================================================
;;; RefContext - Factory function
;;; ============================================================================

(deftest ref-context-factory-test
  (testing "->ref-context creates a RefContext record with defaults"
    (let [ctx (dispatch/->ref-context "prompt" {})]
      (is (instance? hive_mcp.protocols.dispatch.RefContext ctx))
      (is (= "prompt" (:prompt ctx)))
      (is (= {} (:ctx-refs ctx)))
      (is (= [] (:kg-node-ids ctx)))
      (is (nil? (:scope ctx)))
      ;; reconstruct-fn now defaults to lazy resolver via requiring-resolve
      (is (fn? (:reconstruct-fn ctx)))))

  (testing "->ref-context populates all fields"
    (let [f (fn [_ _ _] "ctx")
          ctx (dispatch/->ref-context "p" {:ctx-refs {:a "1"}
                                           :kg-node-ids ["n1"]
                                           :scope "proj"
                                           :reconstruct-fn f})]
      (is (= {:a "1"} (:ctx-refs ctx)))
      (is (= ["n1"] (:kg-node-ids ctx)))
      (is (= "proj" (:scope ctx)))
      (is (= f (:reconstruct-fn ctx))))))

;;; ============================================================================
;;; ensure-context - RefContext passthrough
;;; ============================================================================

(deftest ensure-context-ref-passthrough-test
  (testing "ensure-context passes through RefContext instances"
    (let [ref-ctx (dispatch/->ref-context "task" {:scope "test"})
          result (dispatch/ensure-context ref-ctx)]
      (is (identical? ref-ctx result))
      (is (= :ref (dispatch/context-type result))))))
