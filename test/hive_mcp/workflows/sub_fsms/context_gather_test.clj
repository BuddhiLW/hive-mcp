(ns hive-mcp.workflows.sub-fsms.context-gather-test
  "Tests for the context-gather sub-FSM.

   Tests the PURE handler layer -- no side effects, no context-store, no KG.
   All external dependencies injected via mock resources.

   Test categories:
   1. Dispatch predicates -- pure boolean functions of state data
   2. Handler unit tests -- each handler with mock resources
   3. FSM integration -- full compile+run with mock resource chain
   4. Graceful degradation -- error handling at each stage
   5. Sub-FSM helper -- run-sub-fsm parent embedding
   6. Output bounds -- 3000 char cap enforcement

   SOLID: D -- Tests depend on abstractions (resources), not concretions.
   CLARITY: T -- Telemetry via test assertions."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.workflows.sub-fsms.context-gather :as ctx]
            [hive.events.fsm :as fsm]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Data
;; =============================================================================

(def sample-axioms
  [{:id "ax-1" :content "Never spawn drones from lings"}
   {:id "ax-2" :content "Cap 5-6 lings per Emacs daemon"}])

(def sample-decisions
  [{:id "dec-1" :content "Use Datalevin as default KG backend"}
   {:id "dec-2" :content "Headless lings via ProcessBuilder"}])

(def sample-kg-context
  {:nodes #{"node-A" "node-B" "node-C"}
   :edges [{:from "node-A" :to "node-B" :relation :implements :confidence 0.9}
           {:from "node-A" :to "node-C" :relation :depends-on :confidence 1.0}]})

(def sample-rendered-context
  "## Reconstructed Context (Compressed)\n\n### Axioms (2 total)\n- Never spawn drones from lings\n- Cap 5-6 lings per Emacs daemon\n\nKG Subgraph (3 nodes, 2 edges):\n  node-A -impl-> node-B [0.9]\n  node-A -dep-> node-C [1.0]")

;; =============================================================================
;; Test Helpers -- Mock Resources
;; =============================================================================

(defn mock-resources
  "Build a mock resources map for handler tests.

   Options:
     :ref-data       -- return value for :fetch-ref-data-fn
     :kg-raw         -- return value for :gather-kg-context-fn
     :compressed     -- return value for :compress-kg-fn
     :rendered       -- return value for :render-context-fn
     :fetch-throws?  -- if true, fetch-ref-data-fn throws
     :kg-throws?     -- if true, gather-kg-context-fn throws
     :compress-throws? -- if true, compress-kg-fn throws
     :render-throws? -- if true, render-context-fn throws"
  ([] (mock-resources {}))
  ([{:keys [ref-data kg-raw compressed rendered
            fetch-throws? kg-throws? compress-throws? render-throws?]
     :or {ref-data   {:axioms sample-axioms :decisions sample-decisions}
          kg-raw     sample-kg-context
          compressed "KG Subgraph (3 nodes, 2 edges):\n  node-A -impl-> node-B\n  node-A -dep-> node-C"
          rendered   sample-rendered-context}}]
   {:fetch-ref-data-fn    (if fetch-throws?
                            (fn [_] (throw (Exception. "context-store down")))
                            (constantly ref-data))
    :gather-kg-context-fn (if kg-throws?
                            (fn [_ _] (throw (Exception. "KG down")))
                            (constantly kg-raw))
    :compress-kg-fn       (if compress-throws?
                            (fn [_] (throw (Exception. "compression failed")))
                            (constantly compressed))
    :render-context-fn    (if render-throws?
                            (fn [_ _] (throw (Exception. "render failed")))
                            (constantly rendered))}))

;; =============================================================================
;; 1. Dispatch Predicate Tests
;; =============================================================================

(deftest test-has-ref-data?
  (testing "has-ref-data? checks for :ref-data key presence"
    (is (true? (ctx/has-ref-data? {:ref-data {}}))
        "Empty map still means fetch was attempted")
    (is (true? (ctx/has-ref-data? {:ref-data {:axioms []}}))
        "Non-empty ref-data returns true")
    (is (false? (ctx/has-ref-data? {}))
        "Missing key returns false")))

(deftest test-has-kg-raw?
  (testing "has-kg-raw? checks for :kg-raw key presence"
    (is (true? (ctx/has-kg-raw? {:kg-raw nil}))
        "nil value still means traversal was attempted")
    (is (true? (ctx/has-kg-raw? {:kg-raw sample-kg-context}))
        "Non-nil kg-raw returns true")
    (is (false? (ctx/has-kg-raw? {}))
        "Missing key returns false")))

(deftest test-has-compressed?
  (testing "has-compressed? checks for :compressed-context key presence"
    (is (true? (ctx/has-compressed? {:compressed-context nil}))
        "nil value still means compression was attempted")
    (is (true? (ctx/has-compressed? {:compressed-context "some text"}))
        "Non-nil compressed returns true")
    (is (false? (ctx/has-compressed? {}))
        "Missing key returns false")))

(deftest test-render-complete?
  (testing "render-complete? requires both :rendered-context and :token-estimate"
    (is (true? (ctx/render-complete? {:rendered-context "text" :token-estimate 5}))
        "Both keys present returns true")
    (is (false? (ctx/render-complete? {:rendered-context "text"}))
        "Missing token-estimate returns false")
    (is (false? (ctx/render-complete? {:token-estimate 5}))
        "Missing rendered-context returns false")
    (is (false? (ctx/render-complete? {}))
        "Both missing returns false")))

;; =============================================================================
;; 2. Handler Unit Tests
;; =============================================================================

(deftest test-handle-fetch-refs
  (testing "handle-fetch-refs fetches ref data via resource fn"
    (let [resources (mock-resources)
          data {:ctx-refs {:axioms "ctx-123" :decisions "ctx-456"}}
          result (ctx/handle-fetch-refs resources data)]
      (is (= {:axioms sample-axioms :decisions sample-decisions}
             (:ref-data result))
          "Stores fetched ref-data")
      (is (= {:axioms "ctx-123" :decisions "ctx-456"} (:ctx-refs result))
          "Preserves original ctx-refs"))))

(deftest test-handle-fetch-refs-empty-refs
  (testing "handle-fetch-refs with empty/nil ctx-refs produces empty map"
    (let [resources (mock-resources)
          result-nil (ctx/handle-fetch-refs resources {})
          result-empty (ctx/handle-fetch-refs resources {:ctx-refs {}})]
      (is (= {} (:ref-data result-nil))
          "nil ctx-refs produces empty ref-data")
      (is (= {} (:ref-data result-empty))
          "Empty ctx-refs produces empty ref-data"))))

(deftest test-handle-fetch-refs-no-fn
  (testing "handle-fetch-refs with nil fetch fn produces empty map"
    (let [resources {}
          data {:ctx-refs {:axioms "ctx-123"}}
          result (ctx/handle-fetch-refs resources data)]
      (is (= {} (:ref-data result))
          "No fetch fn produces empty ref-data"))))

(deftest test-handle-traverse-kg
  (testing "handle-traverse-kg calls gather fn with node IDs and scope"
    (let [called-args (atom nil)
          resources {:gather-kg-context-fn (fn [ids scope]
                                             (reset! called-args {:ids ids :scope scope})
                                             sample-kg-context)}
          data {:kg-node-ids ["node-A" "node-B"] :scope "hive-mcp"}
          result (ctx/handle-traverse-kg resources data)]
      (is (= sample-kg-context (:kg-raw result))
          "Stores KG traversal result")
      (is (= {:ids ["node-A" "node-B"] :scope "hive-mcp"} @called-args)
          "Calls gather fn with correct args"))))

(deftest test-handle-traverse-kg-empty-nodes
  (testing "handle-traverse-kg with empty node IDs produces nil"
    (let [resources (mock-resources)
          result (ctx/handle-traverse-kg resources {:kg-node-ids []})]
      (is (nil? (:kg-raw result))
          "Empty node IDs produces nil kg-raw"))))

(deftest test-handle-traverse-kg-no-fn
  (testing "handle-traverse-kg with nil gather fn produces nil"
    (let [result (ctx/handle-traverse-kg {} {:kg-node-ids ["node-A"] :scope "hive-mcp"})]
      (is (nil? (:kg-raw result))
          "No gather fn produces nil"))))

(deftest test-handle-compress
  (testing "handle-compress calls compress fn with kg-raw"
    (let [called-with (atom nil)
          resources {:compress-kg-fn (fn [kg-ctx]
                                       (reset! called-with kg-ctx)
                                       "compressed-output")}
          data {:kg-raw sample-kg-context}
          result (ctx/handle-compress resources data)]
      (is (= "compressed-output" (:compressed-context result))
          "Stores compressed context")
      (is (= sample-kg-context @called-with)
          "Passes kg-raw to compress fn"))))

(deftest test-handle-compress-no-edges
  (testing "handle-compress with empty edges produces nil"
    (let [resources (mock-resources)
          result (ctx/handle-compress resources {:kg-raw {:nodes #{} :edges []}})]
      (is (nil? (:compressed-context result))
          "Empty edges produces nil compressed-context"))))

(deftest test-handle-compress-nil-kg
  (testing "handle-compress with nil kg-raw produces nil"
    (let [resources (mock-resources)
          result (ctx/handle-compress resources {:kg-raw nil})]
      (is (nil? (:compressed-context result))
          "nil kg-raw produces nil compressed-context"))))

(deftest test-handle-render
  (testing "handle-render combines ref-data and kg-raw into rendered context"
    (let [resources (mock-resources)
          data {:ref-data {:axioms sample-axioms}
                :kg-raw sample-kg-context}
          result (ctx/handle-render resources data)]
      (is (string? (:rendered-context result))
          "Produces rendered-context string")
      (is (= (:rendered-context result) (:context result))
          ":context is alias for :rendered-context")
      (is (pos? (:token-estimate result))
          "Produces positive token estimate"))))

(deftest test-handle-render-token-estimate
  (testing "handle-render token estimate is approximately chars/4"
    (let [rendered (apply str (repeat 400 "x"))  ;; 400 chars
          resources {:render-context-fn (constantly rendered)}
          data {:ref-data {} :kg-raw nil}
          result (ctx/handle-render resources data)]
      (is (= 100 (:token-estimate result))
          "Token estimate = chars / 4"))))

(deftest test-handle-render-no-fn
  (testing "handle-render without render fn produces fallback"
    (let [data {:ref-data {:axioms sample-axioms}
                :kg-raw sample-kg-context}
          result (ctx/handle-render {} data)]
      (is (string? (:rendered-context result))
          "Produces fallback rendered-context")
      (is (str/includes? (:rendered-context result) "no renderer")
          "Fallback mentions missing renderer")
      (is (str/includes? (:rendered-context result) "1 categories")
          "Fallback shows ref count")
      (is (str/includes? (:rendered-context result) "3 nodes")
          "Fallback shows KG node count"))))

(deftest test-handle-render-nil-result
  (testing "handle-render with nil render result produces default"
    (let [resources {:render-context-fn (constantly nil)}
          data {:ref-data {} :kg-raw nil}
          result (ctx/handle-render resources data)]
      (is (string? (:rendered-context result))
          "nil render result produces default string")
      (is (str/includes? (:rendered-context result) "Context (empty)")
          "Default mentions empty context"))))

;; =============================================================================
;; 3. FSM Integration Tests (compile + run with mocks)
;; =============================================================================

(deftest test-full-pipeline-happy-path
  (testing "Full FSM pipeline: fetch -> traverse -> compress -> render -> end"
    (let [resources (mock-resources)
          data {:ctx-refs {:axioms "ctx-123" :decisions "ctx-456"}
                :kg-node-ids ["node-A"]
                :scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Produces :context string")
      (is (= sample-rendered-context (:context result))
          "Context matches mock rendered output")
      (is (pos? (:token-estimate result))
          "Produces positive token estimate")
      (is (map? (:ref-data result))
          "Intermediate :ref-data present")
      (is (map? (:kg-raw result))
          "Intermediate :kg-raw present")
      (is (string? (:compressed-context result))
          "Intermediate :compressed-context present")
      (is (= (:rendered-context result) (:context result))
          ":rendered-context equals :context"))))

(deftest test-full-pipeline-refs-only
  (testing "Pipeline with refs but no KG node IDs"
    (let [resources (mock-resources {:kg-raw nil :compressed nil})
          data {:ctx-refs {:axioms "ctx-123"} :scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Produces context even without KG")
      (is (pos? (:token-estimate result))
          "Has token estimate"))))

(deftest test-full-pipeline-kg-only
  (testing "Pipeline with KG but no refs"
    (let [resources (mock-resources {:ref-data {}})
          data {:kg-node-ids ["node-A"] :scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Produces context even without refs")
      (is (pos? (:token-estimate result))
          "Has token estimate"))))

(deftest test-full-pipeline-empty-input
  (testing "Pipeline with no refs and no KG produces minimal context"
    (let [resources (mock-resources {:ref-data {} :kg-raw nil :compressed nil})
          data {:scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Produces context even with empty input")
      (is (pos? (:token-estimate result))
          "Has positive token estimate"))))

(deftest test-pipeline-preserves-input-keys
  (testing "Pipeline preserves original input keys in result"
    (let [resources (mock-resources)
          data {:ctx-refs {:axioms "ctx-123"}
                :kg-node-ids ["node-A"]
                :scope "hive-mcp"
                :extra-key "should-survive"}
          result (ctx/run-context-gather data resources)]
      (is (= "ctx-123" (get-in result [:ctx-refs :axioms]))
          "Preserves :ctx-refs")
      (is (= ["node-A"] (:kg-node-ids result))
          "Preserves :kg-node-ids")
      (is (= "hive-mcp" (:scope result))
          "Preserves :scope")
      (is (= "should-survive" (:extra-key result))
          "Preserves extra keys"))))

;; =============================================================================
;; 4. Graceful Degradation Tests
;; =============================================================================

(deftest test-fetch-throws-degradation
  (testing "Fetch failure produces empty ref-data, pipeline continues"
    (let [resources (mock-resources {:fetch-throws? true})
          data {:ctx-refs {:axioms "ctx-123"} :scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Pipeline still completes")
      (is (= {} (:ref-data result))
          "ref-data is empty map after fetch failure")
      (is (pos? (:token-estimate result))
          "Still has token estimate"))))

(deftest test-kg-throws-degradation
  (testing "KG failure produces nil kg-raw, pipeline continues"
    (let [resources (mock-resources {:kg-throws? true})
          data {:ctx-refs {:axioms "ctx-123"}
                :kg-node-ids ["node-A"]
                :scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Pipeline still completes")
      (is (nil? (:kg-raw result))
          "kg-raw is nil after KG failure")
      (is (pos? (:token-estimate result))
          "Still has token estimate"))))

(deftest test-compress-throws-degradation
  (testing "Compression failure produces nil compressed, pipeline continues"
    (let [resources (mock-resources {:compress-throws? true})
          data {:ctx-refs {:axioms "ctx-123"}
                :kg-node-ids ["node-A"]
                :scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Pipeline still completes")
      (is (nil? (:compressed-context result))
          "compressed-context is nil after failure")
      (is (pos? (:token-estimate result))
          "Still has token estimate"))))

(deftest test-render-throws-degradation
  (testing "Render failure produces fallback context"
    (let [resources (mock-resources {:render-throws? true})
          data {:ctx-refs {:axioms "ctx-123"}
                :kg-node-ids ["node-A"]
                :scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Pipeline still completes with fallback")
      (is (not (str/blank? (:context result)))
          "Fallback context is not blank")
      (is (pos? (:token-estimate result))
          "Still has token estimate"))))

(deftest test-all-resources-throw-degradation
  (testing "All resource fns failing still produces context"
    (let [resources (mock-resources {:fetch-throws? true
                                     :kg-throws? true
                                     :compress-throws? true
                                     :render-throws? true})
          data {:ctx-refs {:axioms "ctx-123"}
                :kg-node-ids ["node-A"]
                :scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (string? (:context result))
          "Catastrophic failure still produces context")
      (is (pos? (:token-estimate result))
          "Still has token estimate"))))

(deftest test-nil-resources-degradation
  (testing "run-context-gather with no resources produces fallback context"
    (let [data {:scope "hive-mcp"}
          result (ctx/run-context-gather data)]
      (is (string? (:context result))
          "No resources still produces context")
      (is (pos? (:token-estimate result))
          "Still has token estimate"))))

;; =============================================================================
;; 5. Sub-FSM Helper Tests
;; =============================================================================

(deftest test-run-sub-fsm-basic
  (testing "run-sub-fsm extracts keys from parent data and merges result back"
    (let [resources (mock-resources)
          parent-data {:ctx-refs {:axioms "ctx-123"}
                       :kg-node-ids ["node-A"]
                       :scope "hive-mcp"
                       :parent-field "preserved"}
          result (ctx/run-sub-fsm resources parent-data)]
      (is (= "preserved" (:parent-field result))
          "Preserves parent fields")
      (is (string? (:context result))
          "Merges :context into parent")
      (is (pos? (:token-estimate result))
          "Merges :token-estimate into parent")
      (is (map? (:ref-data result))
          "Merges :ref-data into parent"))))

(deftest test-run-sub-fsm-with-overrides
  (testing "run-sub-fsm accepts override map for input keys"
    (let [called-args (atom nil)
          resources {:fetch-ref-data-fn (fn [refs]
                                          (reset! called-args refs)
                                          {:custom sample-axioms})
                     :gather-kg-context-fn (constantly nil)
                     :compress-kg-fn (constantly nil)
                     :render-context-fn (constantly "custom context")}
          parent-data {:ctx-refs {:old "should-be-overridden"}
                       :scope "old-scope"}
          overrides {:ctx-refs {:custom "new-ref"}
                     :scope "override-scope"}
          result (ctx/run-sub-fsm resources parent-data overrides)]
      (is (= {:custom "new-ref"} @called-args)
          "Override ctx-refs are used")
      (is (string? (:context result))
          "Produces context"))))

(deftest test-run-sub-fsm-uses-context-gather-resources-key
  (testing "run-sub-fsm prefers :context-gather-resources from parent resources"
    (let [inner-called (atom false)
          resources {:context-gather-resources
                     {:fetch-ref-data-fn (fn [_]
                                           (reset! inner-called true)
                                           {:inner sample-axioms})
                      :gather-kg-context-fn (constantly nil)
                      :compress-kg-fn (constantly nil)
                      :render-context-fn (constantly "inner-rendered")}
                     ;; These top-level keys should be ignored
                     :fetch-ref-data-fn (constantly {:outer true})}
          parent-data {:ctx-refs {:a "ctx-1"} :scope "test"}
          result (ctx/run-sub-fsm resources parent-data)]
      (is (true? @inner-called)
          "Uses :context-gather-resources nested map")
      (is (= "inner-rendered" (:context result))
          "Rendered by inner resource fn"))))

(deftest test-run-sub-fsm-fallback-production-resources
  (testing "run-sub-fsm falls back to production resources when none provided"
    ;; This test just verifies it doesn't throw -- production resources
    ;; may or may not be available in test context
    (let [result (ctx/run-sub-fsm {} {:scope "test-scope"})]
      (is (map? result)
          "Returns a map")
      (is (string? (:context result))
          "Has a :context string"))))

;; =============================================================================
;; 6. Output Bounds Tests
;; =============================================================================

(deftest test-render-truncation-at-max-chars
  (testing "Output exceeding max-output-chars is truncated"
    (let [huge-output (apply str (repeat 5000 "x"))
          resources {:fetch-ref-data-fn (constantly {})
                     :gather-kg-context-fn (constantly nil)
                     :compress-kg-fn (constantly nil)
                     :render-context-fn (constantly huge-output)}
          data {:scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (<= (count (:context result)) (+ ctx/max-output-chars 1))
          "Output bounded to max-output-chars")
      (is (str/ends-with? (:context result) "[truncated]")
          "Truncated output ends with marker"))))

(deftest test-render-under-limit-not-truncated
  (testing "Output under max-output-chars is not truncated"
    (let [small-output "Short context"
          resources {:fetch-ref-data-fn (constantly {})
                     :gather-kg-context-fn (constantly nil)
                     :compress-kg-fn (constantly nil)
                     :render-context-fn (constantly small-output)}
          data {:scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (= small-output (:context result))
          "Small output not truncated")
      (is (not (str/includes? (:context result) "[truncated]"))
          "No truncation marker"))))

(deftest test-token-estimate-minimum-one
  (testing "Token estimate is at least 1 even for tiny output"
    (let [resources {:fetch-ref-data-fn (constantly {})
                     :gather-kg-context-fn (constantly nil)
                     :compress-kg-fn (constantly nil)
                     :render-context-fn (constantly "x")}
          data {:scope "hive-mcp"}
          result (ctx/run-context-gather data resources)]
      (is (>= (:token-estimate result) 1)
          "Token estimate is at least 1"))))

;; =============================================================================
;; 7. FSM Spec Structure Tests
;; =============================================================================

(deftest test-context-gather-spec-structure
  (testing "context-gather-spec has correct shape"
    (let [spec ctx/context-gather-spec]
      (is (map? (:fsm spec)) "Has :fsm key")
      (is (map? (:opts spec)) "Has :opts key")
      (is (contains? (:fsm spec) ::fsm/start) "Has ::fsm/start state")
      (is (contains? (:fsm spec) ::ctx/traverse-kg) "Has ::traverse-kg state")
      (is (contains? (:fsm spec) ::ctx/compress) "Has ::compress state")
      (is (contains? (:fsm spec) ::ctx/render) "Has ::render state")
      (is (contains? (:fsm spec) ::fsm/error) "Has ::fsm/error state"))))

(deftest test-compile-succeeds
  (testing "context-gather-spec compiles without error"
    (let [compiled (fsm/compile ctx/context-gather-spec)]
      (is (map? compiled) "Returns a compiled FSM map")
      (is (contains? compiled :fsm) "Has :fsm key")
      (is (contains? compiled :opts) "Has :opts key"))))

(deftest test-spec-max-trace
  (testing "Spec has max-trace set to 10 for bounded trace"
    (is (= 10 (get-in ctx/context-gather-spec [:opts :max-trace]))
        "max-trace is 10")))

;; =============================================================================
;; 8. make-production-resources Tests
;; =============================================================================

(deftest test-make-production-resources-returns-map
  (testing "make-production-resources returns a map with all 4 resource keys"
    (let [resources (ctx/make-production-resources)]
      (is (map? resources)
          "Returns a map")
      (is (contains? resources :fetch-ref-data-fn)
          "Has :fetch-ref-data-fn")
      (is (contains? resources :gather-kg-context-fn)
          "Has :gather-kg-context-fn")
      (is (contains? resources :compress-kg-fn)
          "Has :compress-kg-fn")
      (is (contains? resources :render-context-fn)
          "Has :render-context-fn"))))

(deftest test-make-production-resources-fns-are-callable
  (testing "All production resource fns are callable (ifn? covers Vars and fns)"
    (let [resources (ctx/make-production-resources)]
      (is (ifn? (:fetch-ref-data-fn resources))
          "fetch-ref-data-fn is invocable")
      (is (ifn? (:gather-kg-context-fn resources))
          "gather-kg-context-fn is invocable")
      (is (ifn? (:compress-kg-fn resources))
          "compress-kg-fn is invocable")
      (is (ifn? (:render-context-fn resources))
          "render-context-fn is invocable"))))

(deftest test-make-production-resources-fallbacks-dont-throw
  (testing "Fallback fns don't throw when called"
    (let [resources (ctx/make-production-resources)
          ;; These may use real impls or fallbacks depending on classpath.
          ;; Either way, they should not throw. Return value may be nil or map.
          fetch-result (try ((:fetch-ref-data-fn resources) {}) :ok
                            (catch Exception _ :threw))
          render-result (try ((:render-context-fn resources) {} nil)
                             (catch Exception _ :threw))]
      (is (not= :threw fetch-result)
          "fetch-ref-data-fn callable with empty map without throwing")
      (is (string? render-result)
          "render-context-fn returns a string"))))

;; =============================================================================
;; 9. FSM State Transition Verification
;; =============================================================================

(deftest test-fsm-run-returns-data-map-directly
  (testing "fsm/run returns the :data map directly (not wrapped)"
    (let [resources (mock-resources)
          data {:ctx-refs {:axioms "ctx-1"} :kg-node-ids ["n1"] :scope "test"}
          result (ctx/run-context-gather data resources)]
      (is (not (contains? result :fsm))
          "Result is not wrapped FSM state (no :fsm key)")
      (is (not (contains? result :trace))
          "Result has no :trace key (unwrapped)")
      (is (contains? result :context)
          "Result has :context directly at top level"))))

(deftest test-handler-data-threading
  (testing "Each handler assocs its output key and preserves all prior keys"
    (let [resources (mock-resources)
          ;; Step through each handler manually to verify data threading
          d0 {:ctx-refs {:axioms "ctx-1"} :kg-node-ids ["n1"] :scope "hive-mcp"}
          d1 (ctx/handle-fetch-refs resources d0)
          d2 (ctx/handle-traverse-kg resources d1)
          d3 (ctx/handle-compress resources d2)
          d4 (ctx/handle-render resources d3)]
      ;; After fetch-refs
      (is (contains? d1 :ref-data) "d1 has :ref-data")
      (is (= {:axioms "ctx-1"} (:ctx-refs d1)) "d1 preserves :ctx-refs")
      ;; After traverse-kg
      (is (contains? d2 :kg-raw) "d2 has :kg-raw")
      (is (contains? d2 :ref-data) "d2 preserves :ref-data from d1")
      ;; After compress
      (is (contains? d3 :compressed-context) "d3 has :compressed-context")
      (is (contains? d3 :kg-raw) "d3 preserves :kg-raw from d2")
      ;; After render
      (is (contains? d4 :rendered-context) "d4 has :rendered-context")
      (is (contains? d4 :context) "d4 has :context alias")
      (is (contains? d4 :token-estimate) "d4 has :token-estimate")
      (is (contains? d4 :compressed-context) "d4 preserves :compressed-context from d3")
      (is (= "hive-mcp" (:scope d4)) "d4 preserves original :scope"))))

;; =============================================================================
;; 10. Boundary Conditions
;; =============================================================================

(deftest test-render-exactly-at-max-chars
  (testing "Output exactly at max-output-chars is NOT truncated"
    (let [exact-output (apply str (repeat ctx/max-output-chars "x"))
          resources {:fetch-ref-data-fn (constantly {})
                     :gather-kg-context-fn (constantly nil)
                     :compress-kg-fn (constantly nil)
                     :render-context-fn (constantly exact-output)}
          data {:scope "test"}
          result (ctx/run-context-gather data resources)]
      (is (= ctx/max-output-chars (count (:context result)))
          "Exactly at limit is not truncated")
      (is (not (str/includes? (:context result) "[truncated]"))
          "No truncation marker at exact limit"))))

(deftest test-render-one-over-max-chars
  (testing "Output at max-output-chars + 1 IS truncated"
    (let [over-output (apply str (repeat (inc ctx/max-output-chars) "x"))
          resources {:fetch-ref-data-fn (constantly {})
                     :gather-kg-context-fn (constantly nil)
                     :compress-kg-fn (constantly nil)
                     :render-context-fn (constantly over-output)}
          data {:scope "test"}
          result (ctx/run-context-gather data resources)]
      (is (< (count (:context result)) (count over-output))
          "Over-limit output is truncated")
      (is (str/ends-with? (:context result) "[truncated]")
          "Truncation marker present"))))

(deftest test-handle-compress-with-nodes-but-empty-edges
  (testing "Compress with nodes but empty edges vector produces nil"
    (let [resources (mock-resources)
          data {:kg-raw {:nodes #{"a" "b"} :edges []}}
          result (ctx/handle-compress resources data)]
      (is (nil? (:compressed-context result))
          "Empty edges vector -> nil compressed despite non-empty nodes"))))

(deftest test-handle-fetch-refs-exception-recovery
  (testing "Fetch fn exception doesn't propagate, returns empty ref-data"
    (let [resources {:fetch-ref-data-fn (fn [_] (throw (ex-info "Boom" {:type :test})))}
          data {:ctx-refs {:axioms "ctx-1"}}
          result (ctx/handle-fetch-refs resources data)]
      (is (= {} (:ref-data result))
          "Exception produces empty ref-data")
      (is (= {:axioms "ctx-1"} (:ctx-refs result))
          "Original data preserved despite exception"))))

(deftest test-handle-traverse-kg-exception-recovery
  (testing "KG traverse fn exception doesn't propagate, returns nil kg-raw"
    (let [resources {:gather-kg-context-fn (fn [_ _] (throw (ex-info "KG down" {})))}
          data {:kg-node-ids ["n1"] :scope "test"}
          result (ctx/handle-traverse-kg resources data)]
      (is (nil? (:kg-raw result))
          "Exception produces nil kg-raw"))))

(deftest test-handle-compress-exception-recovery
  (testing "Compress fn exception doesn't propagate, returns nil compressed"
    (let [resources {:compress-kg-fn (fn [_] (throw (ex-info "Bad compress" {})))}
          data {:kg-raw {:nodes #{"a"} :edges [{:from "a" :to "b"}]}}
          result (ctx/handle-compress resources data)]
      (is (nil? (:compressed-context result))
          "Exception produces nil compressed-context"))))

(deftest test-run-context-gather-catastrophic-exception
  (testing "run-context-gather catches unexpected exceptions from fsm/run"
    ;; Force a truly unexpected error by passing a non-map as resources
    ;; that would cause NPE deep inside
    (let [result (ctx/run-context-gather {:ctx-refs "not-a-map"} {})]
      (is (string? (:context result))
          "Catastrophic error still produces context string")
      (is (pos? (:token-estimate result))
          "Catastrophic error still has token-estimate"))))
