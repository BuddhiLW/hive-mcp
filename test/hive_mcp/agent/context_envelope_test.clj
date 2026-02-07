(ns hive-mcp.agent.context-envelope-test
  "Tests for L2 Context Envelope builder.

   Tests all modes (inline, deferred) and integration with IDispatchContext.
   Uses with-redefs to mock context-store and KG queries."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.context-envelope :as envelope]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.context.reconstruction :as reconstruction]
            [clojure.string :as str]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn reset-context-store [f]
  (context-store/reset-all!)
  (f)
  (context-store/reset-all!))

(use-fixtures :each reset-context-store)

;; =============================================================================
;; Test Data
;; =============================================================================

(def sample-axioms
  [{:id "ax-1" :content "Never spawn drones from lings"}
   {:id "ax-2" :content "Cap 5-6 lings per Emacs daemon"}])

(def sample-decisions
  [{:id "20260207-dec1" :content "Headless lings via ProcessBuilder"}])

;; =============================================================================
;; build-l2-envelope Tests
;; =============================================================================

(deftest build-l2-envelope-returns-nil-for-empty-refs
  (testing "returns nil when no refs or KG nodes provided"
    (is (nil? (envelope/build-l2-envelope {} [] nil)))
    (is (nil? (envelope/build-l2-envelope nil nil nil)))))

(deftest build-l2-envelope-inline-mode
  (testing "inline mode produces envelope with reconstructed context"
    ;; Store test data in context-store
    (let [ax-id (context-store/context-put! sample-axioms :tags #{"axioms"})
          dec-id (context-store/context-put! sample-decisions :tags #{"decisions"})
          ctx-refs {:axioms ax-id :decisions dec-id}
          ;; Mock KG traversal to avoid needing a real KG backend
          result (with-redefs [reconstruction/gather-kg-context
                               (fn [_ _] {:nodes #{} :edges []})]
                   (envelope/build-l2-envelope ctx-refs [] "hive-mcp" {:mode :inline}))]
      (is (some? result) "Should produce an envelope")
      (is (str/includes? result "L2-CONTEXT") "Should contain L2 marker")
      (is (str/includes? result "mode=inline") "Should indicate inline mode")
      (is (str/includes? result "Reconstructed Context") "Should contain reconstruction output"))))

(deftest build-l2-envelope-deferred-mode
  (testing "deferred mode produces envelope with ref IDs and hydration instructions"
    (let [ctx-refs {:axioms "ctx-123" :decisions "ctx-456"}
          kg-node-ids ["20260207-dec1"]
          result (envelope/build-l2-envelope ctx-refs kg-node-ids "hive-mcp"
                                            {:mode :deferred})]
      (is (some? result) "Should produce an envelope")
      (is (str/includes? result "L2-CONTEXT") "Should contain L2 marker")
      (is (str/includes? result "mode=deferred") "Should indicate deferred mode")
      (is (str/includes? result "ctx-123") "Should contain axioms ref ID")
      (is (str/includes? result "ctx-456") "Should contain decisions ref ID")
      (is (str/includes? result "20260207-dec1") "Should contain KG node ID")
      (is (str/includes? result "context-reconstruct") "Should include hydration instructions")
      (is (str/includes? result "context-get") "Should include individual fetch instructions"))))

(deftest build-l2-envelope-inline-falls-back-to-deferred
  (testing "inline mode falls back to deferred when reconstruction fails"
    ;; Mock reconstruct-context to fail
    (with-redefs [reconstruction/reconstruct-context
                  (fn [_ _ _] (throw (Exception. "mock failure")))]
      (let [ctx-refs {:axioms "ctx-123"}
            result (envelope/build-l2-envelope ctx-refs [] "hive-mcp" {:mode :inline})]
        (is (some? result) "Should produce deferred fallback")
        (is (str/includes? result "mode=deferred") "Should fall back to deferred mode")))))

(deftest build-l2-envelope-kg-only
  (testing "envelope can be built with KG nodes only (no ctx-refs)"
    (let [result (envelope/build-l2-envelope {} ["node-1" "node-2"] "hive-mcp"
                                            {:mode :deferred})]
      (is (some? result) "Should produce envelope for KG-only")
      (is (str/includes? result "node-1") "Should contain KG node ID")
      (is (str/includes? result "node-2") "Should contain second KG node ID"))))

;; =============================================================================
;; envelope-from-dispatch-context Tests
;; =============================================================================

(deftest envelope-from-text-context-returns-nil
  (testing "TextContext produces no L2 envelope (text dispatch is L1)"
    (let [text-ctx (dispatch-ctx/->text-context "Fix the bug in auth.clj")]
      (is (nil? (envelope/envelope-from-dispatch-context text-ctx))))))

(deftest envelope-from-ref-context-produces-envelope
  (testing "RefContext produces L2 envelope with structured refs"
    ;; Store test data
    (let [ax-id (context-store/context-put! sample-axioms :tags #{"axioms"})
          ctx-refs {:axioms ax-id}
          ref-ctx (dispatch-ctx/->ref-context
                    "Fix the bug in auth.clj"
                    {:ctx-refs ctx-refs
                     :kg-node-ids ["20260207-dec1"]
                     :scope "hive-mcp"
                     ;; Override reconstruct-fn to avoid KG dependency
                     :reconstruct-fn (fn [_ _ _] "mock reconstructed context")})
          result (envelope/envelope-from-dispatch-context ref-ctx)]
      (is (some? result) "RefContext should produce L2 envelope")
      (is (str/includes? result "L2-CONTEXT") "Should contain L2 marker"))))

(deftest envelope-from-nil-context-returns-nil
  (testing "nil dispatch-context returns nil"
    (is (nil? (envelope/envelope-from-dispatch-context nil)))))

;; =============================================================================
;; Envelope Size Bounds
;; =============================================================================

(deftest envelope-inline-respects-max-chars
  (testing "inline envelope is bounded by max-inline-chars"
    ;; Create large context to test truncation
    (let [large-data (vec (for [i (range 100)]
                            {:id (str "entry-" i)
                             :content (apply str (repeat 200 "x"))}))
          ax-id (context-store/context-put! large-data :tags #{"axioms"})
          ctx-refs {:axioms ax-id}
          result (with-redefs [reconstruction/reconstruct-context
                               (fn [_ _ _]
                                 ;; Return oversized string
                                 (apply str (repeat 5000 "x")))]
                   (envelope/build-l2-envelope ctx-refs [] "hive-mcp" {:mode :inline}))]
      (when result
        (is (<= (count result) (+ envelope/max-inline-chars 50))
            "Envelope should not exceed max-inline-chars (with small tolerance)")))))

;; =============================================================================
;; Integration: Deferred Envelope Structure Validation
;; =============================================================================

(deftest deferred-envelope-has-valid-structure
  (testing "deferred envelope contains all required sections"
    (let [ctx-refs {:axioms "ctx-ax" :decisions "ctx-dec" :conventions "ctx-conv"}
          kg-node-ids ["node-a" "node-b"]
          result (envelope/build-l2-envelope ctx-refs kg-node-ids "hive-mcp"
                                            {:mode :deferred})]
      ;; Structural checks
      (is (str/starts-with? result "<!-- L2-CONTEXT") "Should start with L2 marker")
      (is (str/includes? result "<!-- /L2-CONTEXT -->") "Should end with closing marker")
      (is (str/includes? result "Context Store References") "Should have refs section")
      (is (str/includes? result "KG Traversal Seeds") "Should have KG section")
      (is (str/includes? result "How to Hydrate") "Should have hydration section")
      (is (str/includes? result "TTL") "Should mention TTL expiry"))))
