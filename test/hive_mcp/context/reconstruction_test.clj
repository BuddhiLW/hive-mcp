(ns hive-mcp.context.reconstruction-test
  "Tests for KG-compressed context reconstruction.

   Uses with-redefs to mock context-store and KG queries.
   Tests all 5 pipeline stages + bridge function + graceful degradation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.context.reconstruction :as recon]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.knowledge-graph.queries :as kg-queries]
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
   {:id "ax-2" :content "Cap 5-6 lings per Emacs daemon"}
   {:id "ax-3" :content "Memory is source of truth"}])

(def sample-decisions
  [{:id "20260207-dec1" :content "Use Datalevin as default KG backend"}
   {:id "20260207-dec2" :content "Headless lings via ProcessBuilder"}])

(def sample-conventions
  [{:id "conv-1" :content "Go Context Pattern for MCP tools"}
   {:id "conv-2" :content "Spawn then dispatch separately"}])

;; =============================================================================
;; Step 1: fetch-ref-data
;; =============================================================================

(deftest fetch-ref-data-resolves-entries
  (testing "resolves context-store entries by category"
    (let [ax-id (context-store/context-put! sample-axioms :tags #{"catchup" "axioms"})
          dec-id (context-store/context-put! sample-decisions :tags #{"catchup" "decisions"})
          result (recon/fetch-ref-data {:axioms ax-id :decisions dec-id})]
      (is (= 2 (count result)))
      (is (= sample-axioms (:axioms result)))
      (is (= sample-decisions (:decisions result))))))

(deftest fetch-ref-data-handles-missing-refs
  (testing "missing/expired refs are omitted silently"
    (let [ax-id (context-store/context-put! sample-axioms :tags #{"catchup"})
          result (recon/fetch-ref-data {:axioms ax-id
                                        :decisions "ctx-nonexistent-12345678"})]
      (is (= 1 (count result)))
      (is (= sample-axioms (:axioms result)))
      (is (nil? (:decisions result))))))

(deftest fetch-ref-data-handles-nil-input
  (testing "nil or empty ctx-refs returns nil"
    (is (nil? (recon/fetch-ref-data nil)))
    (is (nil? (recon/fetch-ref-data {})))))

(deftest fetch-ref-data-handles-exceptions
  (testing "exceptions in context-get are caught gracefully"
    (with-redefs [context-store/context-get (fn [_] (throw (Exception. "store down")))]
      (let [result (recon/fetch-ref-data {:axioms "ctx-123"})]
        (is (= {} result))))))

;; =============================================================================
;; Step 2: gather-kg-context
;; =============================================================================

(deftest gather-kg-context-traverses-structural-edges
  (testing "traverses KG from seed nodes, collecting structural edges"
    (with-redefs [kg-queries/traverse
                  (fn [node-id _opts]
                    (case node-id
                      "node-A" [{:node-id "node-B"
                                 :edge {:kg-edge/from "node-A"
                                        :kg-edge/to "node-B"
                                        :kg-edge/relation :implements
                                        :kg-edge/confidence 0.9}
                                 :depth 1
                                 :path ["node-A" "node-B"]}
                                {:node-id "node-C"
                                 :edge {:kg-edge/from "node-A"
                                        :kg-edge/to "node-C"
                                        :kg-edge/relation :depends-on
                                        :kg-edge/confidence 1.0}
                                 :depth 1
                                 :path ["node-A" "node-C"]}]
                      []))]
      (let [result (recon/gather-kg-context ["node-A"] "hive-mcp")]
        (is (= #{"node-A" "node-B" "node-C"} (:nodes result)))
        (is (= 2 (count (:edges result))))
        (is (every? #(contains? % :from) (:edges result)))
        (is (every? #(contains? % :to) (:edges result)))
        (is (every? #(contains? % :relation) (:edges result)))))))

(deftest gather-kg-context-bounds-node-count
  (testing "stops collecting when max-kg-nodes reached"
    (with-redefs [kg-queries/traverse
                  (fn [_ _]
                    ;; Return more nodes than the max
                    (mapv (fn [i]
                            {:node-id (str "node-" i)
                             :edge {:kg-edge/from "seed"
                                    :kg-edge/to (str "node-" i)
                                    :kg-edge/relation :implements
                                    :kg-edge/confidence 1.0}
                             :depth 1
                             :path ["seed" (str "node-" i)]})
                          (range 20)))]
      (let [result (recon/gather-kg-context ["seed"] "hive-mcp")]
        ;; Should be bounded by max-kg-nodes (10)
        (is (<= (count (:nodes result)) (inc recon/max-kg-nodes)))))))

(deftest gather-kg-context-handles-empty-input
  (testing "nil/empty node IDs returns nil"
    (is (nil? (recon/gather-kg-context nil "hive-mcp")))
    (is (nil? (recon/gather-kg-context [] "hive-mcp")))))

(deftest gather-kg-context-handles-traverse-failure
  (testing "failed traversals are skipped gracefully"
    (with-redefs [kg-queries/traverse (fn [_ _] (throw (Exception. "KG down")))]
      (let [result (recon/gather-kg-context ["node-A"] "hive-mcp")]
        ;; Should still return with seed nodes, just no edges
        (is (= #{"node-A"} (:nodes result)))
        (is (empty? (:edges result)))))))

(deftest gather-kg-context-deduplicates-edges
  (testing "duplicate edges from multiple seeds are deduplicated"
    (with-redefs [kg-queries/traverse
                  (fn [node-id _]
                    ;; Both seeds discover the same edge
                    [{:node-id (if (= node-id "A") "B" "A")
                      :edge {:kg-edge/from "A"
                             :kg-edge/to "B"
                             :kg-edge/relation :implements
                             :kg-edge/confidence 0.8}
                      :depth 1
                      :path [node-id (if (= node-id "A") "B" "A")]}])]
      (let [result (recon/gather-kg-context ["A" "B"] "hive-mcp")]
        ;; Edge A->B :implements should appear only once
        (is (= 1 (count (:edges result))))))))

;; =============================================================================
;; Step 3: compress-kg-subgraph
;; =============================================================================

(deftest compress-kg-subgraph-renders-edge-list
  (testing "renders compact edge-list format"
    (let [kg-ctx {:nodes #{"20260207023915-34ca6b05" "20260206235801-2b6fb27a" "20260205191822-13c273bb"}
                  :edges [{:from "20260207023915-34ca6b05" :to "20260206235801-2b6fb27a"
                           :relation :implements :confidence 0.9}
                          {:from "20260206235801-2b6fb27a" :to "20260205191822-13c273bb"
                           :relation :depends-on :confidence 1.0}]}
          result (recon/compress-kg-subgraph kg-ctx)]
      (is (string? result))
      (is (str/includes? result "KG Subgraph (3 nodes, 2 edges):"))
      (is (str/includes? result "-impl->"))
      (is (str/includes? result "-dep->"))
      (is (str/includes? result "[0.9]"))
      (is (str/includes? result "[1.0]")))))

(deftest compress-kg-subgraph-handles-nil
  (testing "nil/empty returns nil"
    (is (nil? (recon/compress-kg-subgraph nil)))
    (is (nil? (recon/compress-kg-subgraph {:nodes #{} :edges []})))))

(deftest compress-kg-subgraph-all-relation-types
  (testing "all structural relations produce valid arrows"
    (doseq [rel [:implements :supersedes :depends-on :refines :contradicts :derived-from :applies-to]]
      (let [kg-ctx {:nodes #{"A" "B"}
                    :edges [{:from "A" :to "B" :relation rel :confidence 1.0}]}
            result (recon/compress-kg-subgraph kg-ctx)]
        (is (string? result) (str "Failed for relation: " rel))
        (is (str/includes? result "->") (str "No arrow for relation: " rel))))))

;; =============================================================================
;; Step 4: render-compressed-context
;; =============================================================================

(deftest render-compressed-context-combines-refs-and-kg
  (testing "combines ref summaries and KG subgraph"
    (let [ref-data {:axioms sample-axioms
                    :decisions sample-decisions}
          kg-ctx {:nodes #{"A" "B"}
                  :edges [{:from "A" :to "B" :relation :implements :confidence 0.9}]}
          result (recon/render-compressed-context ref-data kg-ctx)]
      (is (string? result))
      (is (str/includes? result "## Reconstructed Context (KG-Compressed)"))
      (is (str/includes? result "Axioms"))
      (is (str/includes? result "Decisions"))
      (is (str/includes? result "KG Subgraph")))))

(deftest render-compressed-context-refs-only
  (testing "works with refs but no KG"
    (let [ref-data {:axioms sample-axioms}
          result (recon/render-compressed-context ref-data nil)]
      (is (string? result))
      (is (str/includes? result "Axioms"))
      (is (not (str/includes? result "KG Subgraph"))))))

(deftest render-compressed-context-kg-only
  (testing "works with KG but no refs"
    (let [kg-ctx {:nodes #{"A" "B"}
                  :edges [{:from "A" :to "B" :relation :implements :confidence 0.9}]}
          result (recon/render-compressed-context nil kg-ctx)]
      (is (string? result))
      (is (str/includes? result "KG Subgraph")))))

(deftest render-compressed-context-bounded
  (testing "output is bounded to max-output-chars"
    (let [;; Create large ref data that exceeds budget
          big-axioms (mapv (fn [i] {:id (str "ax-" i)
                                     :content (apply str (repeat 500 "x"))})
                           (range 50))
          ref-data {:axioms big-axioms}
          result (recon/render-compressed-context ref-data nil)]
      (is (<= (count result) (+ recon/max-output-chars 20))))))

(deftest render-compressed-context-empty
  (testing "empty inputs produce minimal output"
    (let [result (recon/render-compressed-context nil nil)]
      (is (string? result))
      (is (str/includes? result "## Reconstructed Context")))))

;; =============================================================================
;; Step 5: reconstruct-context (integration)
;; =============================================================================

(deftest reconstruct-context-full-pipeline
  (testing "full pipeline: refs + KG -> compressed markdown"
    (let [ax-id (context-store/context-put! sample-axioms :tags #{"catchup" "axioms"})
          dec-id (context-store/context-put! sample-decisions :tags #{"catchup" "decisions"})]
      (with-redefs [kg-queries/traverse
                    (fn [_ _]
                      [{:node-id "node-B"
                        :edge {:kg-edge/from "node-A"
                               :kg-edge/to "node-B"
                               :kg-edge/relation :implements
                               :kg-edge/confidence 0.9}
                        :depth 1
                        :path ["node-A" "node-B"]}])]
        (let [result (recon/reconstruct-context
                      {:axioms ax-id :decisions dec-id}
                      ["node-A"]
                      "hive-mcp")]
          (is (string? result))
          (is (str/includes? result "## Reconstructed Context (KG-Compressed)"))
          (is (str/includes? result "Axioms"))
          (is (str/includes? result "KG Subgraph"))
          (is (<= (count result) recon/max-output-chars)))))))

(deftest reconstruct-context-graceful-degradation
  (testing "never throws, returns partial context on failure"
    ;; Total failure - both context-store and KG are down
    (with-redefs [context-store/context-get (fn [_] (throw (Exception. "store down")))
                  kg-queries/traverse (fn [_ _] (throw (Exception. "KG down")))]
      (let [result (recon/reconstruct-context
                    {:axioms "ctx-123"}
                    ["node-A"]
                    "hive-mcp")]
        (is (string? result))
        ;; Should still produce some output, not throw
        (is (pos? (count result)))))))

(deftest reconstruct-context-no-refs-no-kg
  (testing "empty inputs produce valid minimal output"
    (let [result (recon/reconstruct-context {} [] "hive-mcp")]
      (is (string? result))
      (is (str/includes? result "## Reconstructed Context")))))

;; =============================================================================
;; Bridge: catchup-refs->ref-context-opts
;; =============================================================================

(deftest catchup-refs->ref-context-opts-basic
  (testing "converts catchup response to RefContext opts"
    (let [dec-id (context-store/context-put! sample-decisions :tags #{"catchup" "decisions"})
          ax-id (context-store/context-put! sample-axioms :tags #{"catchup" "axioms"})
          catchup-resp {:context-refs {:axioms ax-id :decisions dec-id}}
          result (recon/catchup-refs->ref-context-opts catchup-resp "hive-mcp")]
      (is (map? result))
      (is (= {:axioms ax-id :decisions dec-id} (:ctx-refs result)))
      (is (= "hive-mcp" (:scope result)))
      (is (fn? (:reconstruct-fn result)))
      ;; Should extract KG node IDs from decisions
      (is (vector? (:kg-node-ids result))))))

(deftest catchup-refs->ref-context-opts-extracts-kg-ids
  (testing "extracts KG node IDs from decision entries with :id fields"
    (let [decisions-with-ids [{:id "20260207-dec1" :content "Decision 1"}
                              {:id "20260207-dec2" :content "Decision 2"}]
          dec-id (context-store/context-put! decisions-with-ids :tags #{"catchup" "decisions"})
          result (recon/catchup-refs->ref-context-opts
                  {:context-refs {:decisions dec-id}}
                  "hive-mcp")]
      (is (= ["20260207-dec1" "20260207-dec2"] (:kg-node-ids result))))))

(deftest catchup-refs->ref-context-opts-empty-response
  (testing "handles empty/nil catchup response"
    (let [result (recon/catchup-refs->ref-context-opts {} "hive-mcp")]
      (is (= {} (:ctx-refs result)))
      (is (= [] (:kg-node-ids result)))
      (is (= "hive-mcp" (:scope result)))
      (is (fn? (:reconstruct-fn result))))))
