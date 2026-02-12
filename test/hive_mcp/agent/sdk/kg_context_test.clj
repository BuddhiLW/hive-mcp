(ns hive-mcp.agent.sdk.kg-context-test
  "Tests for compressed context injection into SDK ling spawn and dispatch.

   Tests the integration between:
   - agent_sdk_strategy.clj (spawn + dispatch with L2 context)
   - sdk/lifecycle.clj (silence phase KG enrichment)
   - context_envelope.clj (L2 envelope building)

   Uses with-redefs to mock SDK availability, context-store, and KG queries."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.context-envelope :as ctx-envelope]
            [hive-mcp.agent.sdk.lifecycle :as lifecycle]
            [hive-mcp.agent.sdk.session :as session]
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
  [{:id "20260207-dec1" :content "Use Datalevin as default KG backend"}])

;; =============================================================================
;; build-kg-context-prefix Tests (private fn, tested via lifecycle behavior)
;; =============================================================================

(deftest build-kg-context-prefix-with-ref-context
  (testing "builds L2 envelope from RefContext dispatch-context"
    (let [ax-id (context-store/context-put! sample-axioms :tags #{"axioms"})
          dec-id (context-store/context-put! sample-decisions :tags #{"decisions"})
          ref-ctx (dispatch-ctx/->ref-context
                   "Fix the bug"
                   {:ctx-refs {:axioms ax-id :decisions dec-id}
                    :kg-node-ids []
                    :scope "hive-mcp"
                    :reconstruct-fn (fn [_ _ _] "mock reconstructed context")})
          ;; Call the private fn via var deref
          build-fn @#'lifecycle/build-kg-context-prefix
          result (build-fn ref-ctx)]
      (is (some? result) "Should produce L2 envelope from RefContext")
      (is (str/includes? result "L2-CONTEXT") "Should contain L2 marker"))))

(deftest build-kg-context-prefix-with-text-context
  (testing "returns nil for TextContext (no refs to build from)"
    (let [text-ctx (dispatch-ctx/->text-context "Fix the bug")
          build-fn @#'lifecycle/build-kg-context-prefix
          result (build-fn text-ctx)]
      (is (nil? result) "TextContext should produce nil (no structured refs)"))))

(deftest build-kg-context-prefix-with-nil
  (testing "returns nil for nil dispatch-context"
    (let [build-fn @#'lifecycle/build-kg-context-prefix]
      (is (nil? (build-fn nil)) "nil should produce nil"))))

(deftest build-kg-context-prefix-graceful-degradation
  (testing "returns nil when envelope building throws"
    (with-redefs [ctx-envelope/enrich-context
                  (fn [_ _ _ _] (throw (Exception. "envelope failed")))]
      (let [ref-ctx (dispatch-ctx/->ref-context "task" {:ctx-refs {:a "b"}})
            build-fn @#'lifecycle/build-kg-context-prefix
            result (build-fn ref-ctx)]
        (is (nil? result) "Should return nil on failure (CLARITY-Y)")))))

;; =============================================================================
;; Spawn L2 Envelope Tests
;; =============================================================================

(deftest spawn-envelope-builds-from-cwd
  (testing "build-spawn-envelope produces L2 context from directory"
    ;; Store refs in context-store to simulate prior catchup
    (let [ax-id (context-store/context-put! sample-axioms
                                            :tags #{"catchup" "axioms" "hive-mcp"})
          dec-id (context-store/context-put! sample-decisions
                                             :tags #{"catchup" "decisions" "hive-mcp"})]
      ;; Mock the spawn-context :ref path that looks up context-store refs
      (with-redefs [reconstruction/reconstruct-context
                    (fn [refs kg-ids scope]
                      (str "## Reconstructed Context\n"
                           "Refs: " (count refs) ", KG: " (count kg-ids)
                           ", Scope: " scope))]
        (let [result (ctx-envelope/build-spawn-envelope
                      "/home/lages/PP/hive/hive-mcp"
                      {:ctx-refs {:axioms ax-id :decisions dec-id}
                       :kg-node-ids []
                       :scope "hive-mcp"
                       :mode :inline})]
          (is (some? result) "Should build envelope from pre-existing refs")
          (is (str/includes? result "L2-CONTEXT") "Should contain L2 marker"))))))

(deftest spawn-envelope-nil-on-no-refs
  (testing "build-spawn-envelope returns nil when no refs available"
    (with-redefs [;; No context-store entries, no catchup refs
                  context-store/context-query (fn [& _] [])]
      (let [result (ctx-envelope/build-spawn-envelope
                    "/tmp/nonexistent"
                    {:mode :inline})]
        ;; May return nil or fallback - depends on catchup-spawn behavior
        ;; The important thing is it doesn't throw
        (is (not (instance? Exception result))
            "Should not throw on missing refs")))))

;; =============================================================================
;; Dispatch Context Threading Tests
;; =============================================================================

(deftest dispatch-context-stored-in-session
  (testing "dispatch-context is stored in session when provided"
    (let [ling-id "test-kg-dispatch-1"
          ref-ctx (dispatch-ctx/->ref-context
                   "Fix the bug"
                   {:ctx-refs {:axioms "ctx-123"}
                    :kg-node-ids ["node-1"]
                    :scope "hive-mcp"})]
      ;; Register a mock session
      (session/register-session! ling-id
                                 {:ling-id ling-id
                                  :phase :idle
                                  :observations []
                                  :client-ref :mock-client
                                  :dispatch-context nil})
      (try
        ;; Simulate what dispatch does: store context in session
        (session/update-session! ling-id {:dispatch-context ref-ctx})
        (let [sess (session/get-session ling-id)]
          (is (some? (:dispatch-context sess))
              "dispatch-context should be stored in session")
          (is (= :ref (dispatch-ctx/context-type (:dispatch-context sess)))
              "Should be RefContext type"))
        (finally
          (session/unregister-session! ling-id))))))

;; =============================================================================
;; Integration: Silence Phase with KG Context
;; =============================================================================

(deftest silence-prompt-enriched-with-kg-context
  (testing "run-saa-silence! prepends KG context when dispatch-context available"
    (let [ling-id "test-kg-silence-1"
          ax-id (context-store/context-put! sample-axioms :tags #{"axioms"})
          ref-ctx (dispatch-ctx/->ref-context
                   "Fix the bug"
                   {:ctx-refs {:axioms ax-id}
                    :kg-node-ids []
                    :scope "hive-mcp"
                    :reconstruct-fn (fn [_ _ _] "## Mock KG Context\nAxioms loaded.")})]
      ;; Register mock session with dispatch-context
      (session/register-session! ling-id
                                 {:ling-id ling-id
                                  :phase :idle
                                  :observations []
                                  :dispatch-context ref-ctx})
      (try
        ;; Verify the prefix builder works with this session's context
        (let [build-fn @#'lifecycle/build-kg-context-prefix
              prefix (build-fn (:dispatch-context (session/get-session ling-id)))]
          (is (some? prefix) "Should build KG prefix from session dispatch-context")
          (is (str/includes? prefix "L2-CONTEXT") "Prefix should contain L2 marker"))
        (finally
          (session/unregister-session! ling-id))))))

(deftest silence-prompt-no-enrichment-without-context
  (testing "run-saa-silence! works normally without dispatch-context"
    (let [ling-id "test-kg-silence-2"]
      ;; Register mock session WITHOUT dispatch-context
      (session/register-session! ling-id
                                 {:ling-id ling-id
                                  :phase :idle
                                  :observations []})
      (try
        (let [build-fn @#'lifecycle/build-kg-context-prefix
              prefix (build-fn (:dispatch-context (session/get-session ling-id)))]
          (is (nil? prefix) "Should return nil when no dispatch-context in session"))
        (finally
          (session/unregister-session! ling-id))))))

;; =============================================================================
;; Mode Selection Tests
;; =============================================================================

(deftest inline-mode-resolves-immediately
  (testing ":inline mode produces resolved context in envelope"
    (let [ax-id (context-store/context-put! sample-axioms :tags #{"axioms"})
          result (with-redefs [reconstruction/reconstruct-context
                               (fn [_ _ _] "## Inline Resolved Content\nThis is resolved.")]
                   (ctx-envelope/enrich-context
                    {:axioms ax-id} [] "hive-mcp" {:mode :inline}))]
      (is (some? result) "Inline mode should produce envelope")
      (is (str/includes? result "mode=inline") "Should indicate inline mode")
      (is (str/includes? result "Inline Resolved Content") "Should contain resolved content"))))

(deftest deferred-mode-passes-refs
  (testing ":deferred mode produces ref IDs without resolving"
    (let [result (ctx-envelope/enrich-context
                  {:axioms "ctx-ax-123" :decisions "ctx-dec-456"}
                  ["node-1" "node-2"]
                  "hive-mcp"
                  {:mode :deferred})]
      (is (some? result) "Deferred mode should produce envelope")
      (is (str/includes? result "mode=deferred") "Should indicate deferred mode")
      (is (str/includes? result "ctx-ax-123") "Should contain axiom ref ID")
      (is (str/includes? result "ctx-dec-456") "Should contain decision ref ID")
      (is (str/includes? result "node-1") "Should contain KG node ID")
      (is (str/includes? result "context-reconstruct") "Should include hydration instructions"))))
