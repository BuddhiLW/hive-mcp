(ns hive-mcp.context.reconstruction-test
  "Tests for context reconstruction stubs.

   Verifies noop fallback shapes and extension delegation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.context.reconstruction :as recon]
            [hive-mcp.extensions.registry :as ext]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def ^:private cr-keys [:cr/e :cr/f :cr/g :cr/h :cr/i :cr/j])

(defn cleanup-extensions [f]
  (doseq [k cr-keys]
    (ext/deregister! k))
  (f)
  (doseq [k cr-keys]
    (ext/deregister! k)))

(use-fixtures :each cleanup-extensions)

;; =============================================================================
;; Noop Fallback Tests
;; =============================================================================

(deftest fetch-ref-data-noop
  (testing "returns empty map when no extension"
    (is (= {} (recon/fetch-ref-data {:axioms "ctx-123"})))))

(deftest fetch-ref-data-noop-nil
  (testing "returns empty map for nil input"
    (is (= {} (recon/fetch-ref-data nil)))))

(deftest gather-kg-context-noop
  (testing "returns nil when no extension"
    (is (nil? (recon/gather-kg-context ["node-A"] "hive-mcp")))))

(deftest gather-kg-context-noop-empty
  (testing "returns nil for empty input"
    (is (nil? (recon/gather-kg-context [] "hive-mcp")))))

(deftest compress-kg-subgraph-noop
  (testing "returns nil when no extension"
    (is (nil? (recon/compress-kg-subgraph {:nodes #{"A"} :edges []})))))

(deftest render-compressed-context-noop
  (testing "returns empty string when no extension"
    (is (= "" (recon/render-compressed-context {:axioms [1 2 3]} nil)))))

(deftest reconstruct-context-noop
  (testing "returns empty string when no extension"
    (is (= "" (recon/reconstruct-context {:axioms "ctx-123"} ["node-A"] "hive-mcp")))))

(deftest reconstruct-context-noop-empty
  (testing "returns empty string for empty inputs"
    (is (= "" (recon/reconstruct-context {} [] "hive-mcp")))))

(deftest catchup-refs-noop
  (testing "returns default opts with scope and empty refs"
    (let [result (recon/catchup-refs->ref-context-opts {} "hive-mcp")]
      (is (map? result))
      (is (= {} (:ctx-refs result)))
      (is (= [] (:kg-node-ids result)))
      (is (= "hive-mcp" (:scope result)))
      (is (fn? (:reconstruct-fn result))))))

(deftest catchup-refs-noop-scope-passthrough
  (testing "scope parameter flows through in noop"
    (let [result (recon/catchup-refs->ref-context-opts {:context-refs {:a "x"}} "my-project")]
      (is (= "my-project" (:scope result))))))

;; =============================================================================
;; Extension Delegation Tests
;; =============================================================================

(deftest fetch-ref-data-delegates
  (testing "delegates to extension when registered"
    (ext/register! :cr/e (fn [refs] {:resolved refs :count (count refs)}))
    (let [result (recon/fetch-ref-data {:axioms "ax-1" :decisions "dec-1"})]
      (is (= {:axioms "ax-1" :decisions "dec-1"} (:resolved result)))
      (is (= 2 (:count result))))))

(deftest gather-kg-context-delegates
  (testing "delegates to extension when registered"
    (ext/register! :cr/f (fn [ids scope] {:nodes (set ids) :edges [] :scope scope}))
    (let [result (recon/gather-kg-context ["A" "B"] "hive-mcp")]
      (is (= #{"A" "B"} (:nodes result)))
      (is (= "hive-mcp" (:scope result))))))

(deftest compress-kg-subgraph-delegates
  (testing "delegates to extension when registered"
    (ext/register! :cr/g (fn [ctx] (str "KG: " (count (:nodes ctx)) " nodes")))
    (let [result (recon/compress-kg-subgraph {:nodes #{"A" "B"} :edges []})]
      (is (= "KG: 2 nodes" result)))))

(deftest render-compressed-context-delegates
  (testing "delegates to extension when registered"
    (ext/register! :cr/h (fn [refs kg] (str "refs=" (count refs) " kg=" (some? kg))))
    (let [result (recon/render-compressed-context {:axioms []} {:nodes #{}})]
      (is (= "refs=1 kg=true" result)))))

(deftest reconstruct-context-delegates
  (testing "delegates to extension when registered"
    (ext/register! :cr/i (fn [_refs _ids scope] (str "reconstructed:" scope)))
    (is (= "reconstructed:hive-mcp"
           (recon/reconstruct-context {} [] "hive-mcp")))))

(deftest catchup-refs-delegates
  (testing "delegates to extension when registered"
    (ext/register! :cr/j (fn [resp scope]
                           {:ctx-refs    (:context-refs resp)
                            :kg-node-ids ["from-ext"]
                            :scope       scope
                            :reconstruct-fn identity}))
    (let [result (recon/catchup-refs->ref-context-opts
                  {:context-refs {:a "x"}} "proj")]
      (is (= {:a "x"} (:ctx-refs result)))
      (is (= ["from-ext"] (:kg-node-ids result)))
      (is (= "proj" (:scope result))))))

;; =============================================================================
;; Return Shape Validation
;; =============================================================================

(deftest noop-return-shapes
  (testing "all noop returns have expected types"
    (is (map? (recon/fetch-ref-data {})))
    (is (nil? (recon/gather-kg-context [] nil)))
    (is (nil? (recon/compress-kg-subgraph nil)))
    (is (string? (recon/render-compressed-context nil nil)))
    (is (string? (recon/reconstruct-context {} [] nil)))
    (let [opts (recon/catchup-refs->ref-context-opts {} "s")]
      (is (map? opts))
      (is (contains? opts :ctx-refs))
      (is (contains? opts :kg-node-ids))
      (is (contains? opts :scope))
      (is (contains? opts :reconstruct-fn)))))

(deftest reconstruct-fn-is-local-stub
  (testing "noop reconstruct-fn points to local reconstruct-context"
    (let [opts (recon/catchup-refs->ref-context-opts {} "test")]
      (is (= "" ((:reconstruct-fn opts) {} [] "test"))))))
