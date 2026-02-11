(ns hive-mcp.agent.drone.unified-context-test
  "Tests for unified KG traversal context gathering.

   Covers:
   - Pure functions: classify-entries, truncate-content
   - Formatting: format-unified-context, format-entries-section, format-edges-section
   - Seed resolution: resolve-seeds with mocked Chroma
   - Traversal: traverse-from-seeds with mocked KG
   - Full pipeline: gather-unified-context with all stages mocked
   - Extension points: :uc/resolve-seeds, :uc/enrich
   - Graceful degradation: missing Chroma, missing KG, errors
   - Edge cases: empty inputs, nil values, large datasets"
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.agent.drone.unified-context :as uc]
            [hive-mcp.extensions.registry :as ext]))

;; =============================================================================
;; Fixtures — Clean extension registry
;; =============================================================================

(defn clean-extensions-fixture [f]
  (ext/clear-all!)
  (f)
  (ext/clear-all!))

(use-fixtures :each clean-extensions-fixture)

;; =============================================================================
;; Pure Function Tests (no side effects)
;; =============================================================================

(deftest classify-entries-test
  (testing "classifies entries by type"
    (let [entries [{:id "1" :type "convention" :content "Use mapcat"}
                   {:id "2" :type "decision" :content "Use DataScript"}
                   {:id "3" :type "snippet" :content "(defn foo [])"}
                   {:id "4" :type "note" :content "Found issue"}
                   {:id "5" :type "axiom" :content "No micromanagement"}
                   {:id "6" :content "No type field"}]
          result (#'uc/classify-entries entries)]
      (is (= 1 (count (:conventions result))))
      (is (= 1 (count (:decisions result))))
      (is (= 1 (count (:snippets result))))
      ;; notes, axioms, and typeless entries go to :domain
      (is (= 3 (count (:domain result)))))))

(deftest classify-entries-empty-test
  (testing "returns empty categories for empty input"
    (let [result (#'uc/classify-entries [])]
      (is (= [] (:conventions result)))
      (is (= [] (:decisions result)))
      (is (= [] (:snippets result)))
      (is (= [] (:domain result))))))

(deftest classify-entries-all-same-type-test
  (testing "all conventions"
    (let [entries (map #(hash-map :id (str %) :type "convention" :content (str "conv-" %))
                       (range 5))
          result (#'uc/classify-entries entries)]
      (is (= 5 (count (:conventions result))))
      (is (= 0 (count (:decisions result))))
      (is (= 0 (count (:snippets result))))
      (is (= 0 (count (:domain result))))))

  (testing "all decisions"
    (let [entries (map #(hash-map :id (str %) :type "decision" :content (str "dec-" %))
                       (range 3))
          result (#'uc/classify-entries entries)]
      (is (= 3 (count (:decisions result)))))))

(deftest classify-entries-preserves-content-test
  (testing "entries retain their :id and :tags after classification"
    (let [entries [{:id "x1" :type "convention" :content "Rule" :tags ["foo"]}]
          result (#'uc/classify-entries entries)]
      (is (= "x1" (-> result :conventions first :id)))
      (is (= ["foo"] (-> result :conventions first :tags))))))

(deftest truncate-content-test
  (testing "truncates long content"
    (let [long-str (apply str (repeat 600 "x"))
          result (#'uc/truncate-content long-str)]
      (is (<= (count result) (+ uc/max-content-chars 3)))
      (is (str/ends-with? result "..."))))

  (testing "leaves short content unchanged"
    (is (= "hello" (#'uc/truncate-content "hello"))))

  (testing "handles nil gracefully"
    (is (nil? (#'uc/truncate-content nil))))

  (testing "exact boundary — content at max-content-chars not truncated"
    (let [exact (apply str (repeat uc/max-content-chars "a"))]
      (is (= exact (#'uc/truncate-content exact)))))

  (testing "one char over boundary is truncated"
    (let [over (apply str (repeat (inc uc/max-content-chars) "b"))
          result (#'uc/truncate-content over)]
      (is (str/ends-with? result "..."))
      (is (<= (count result) (+ uc/max-content-chars 3)))))

  (testing "empty string unchanged"
    (is (= "" (#'uc/truncate-content "")))))

;; =============================================================================
;; Format Tests
;; =============================================================================

(deftest format-unified-context-test
  (testing "formats non-empty context"
    (let [ctx {:conventions [{:id "1" :content "Use mapcat" :type "convention"}]
               :decisions [{:id "2" :content "Use DS" :type "decision"}]
               :domain []
               :snippets []
               :edges [{:from "1" :to "2" :relation :implements :confidence 0.9}]}
          result (uc/format-unified-context ctx)]
      (is (string? result))
      (is (str/includes? result "Unified Project Context"))
      (is (str/includes? result "Conventions"))
      (is (str/includes? result "Decisions"))
      (is (str/includes? result "KG Structure"))))

  (testing "returns nil for empty context"
    (is (nil? (uc/format-unified-context
               {:conventions [] :decisions [] :domain [] :snippets []}))))

  (testing "returns nil for nil context"
    (is (nil? (uc/format-unified-context nil))))

  (testing "includes domain section when present"
    (let [ctx {:conventions []
               :decisions []
               :domain [{:id "d1" :content "Domain note" :type "note"}]
               :snippets []
               :edges []}
          result (uc/format-unified-context ctx)]
      (is (str/includes? result "Domain Knowledge"))))

  (testing "includes snippets section when present"
    (let [ctx {:conventions []
               :decisions []
               :domain []
               :snippets [{:id "s1" :content "(defn foo [])" :type "snippet"}]
               :edges []}
          result (uc/format-unified-context ctx)]
      (is (str/includes? result "Code Snippets")))))

(deftest format-entries-section-test
  (testing "formats entries with numbered list"
    (let [entries [{:content "First"} {:content "Second"}]
          result (#'uc/format-entries-section "Test" entries)]
      (is (str/starts-with? result "### Test"))
      (is (str/includes? result "1. First"))
      (is (str/includes? result "2. Second"))))

  (testing "returns nil for empty entries"
    (is (nil? (#'uc/format-entries-section "Empty" []))))

  (testing "returns nil for nil entries"
    (is (nil? (#'uc/format-entries-section "Nil" nil))))

  (testing "handles nil content in entries"
    (let [result (#'uc/format-entries-section "Nil Content" [{:content nil}])]
      (is (str/includes? result "(no content)")))))

(deftest format-edges-section-test
  (testing "formats edges with arrow notation"
    (let [edges [{:from "20260207-abc" :to "20260206-def" :relation :implements}
                 {:from "20260206-def" :to "20260205-ghi" :relation :depends-on}]
          result (#'uc/format-edges-section edges)]
      (is (str/includes? result "-impl->"))
      (is (str/includes? result "-dep->"))))

  (testing "returns nil for empty edges"
    (is (nil? (#'uc/format-edges-section []))))

  (testing "all relation types have arrows"
    (let [rels [:implements :supersedes :depends-on :refines
                :contradicts :derived-from :applies-to]
          edges (map-indexed (fn [i rel]
                               {:from (str "n" i) :to (str "n" (inc i)) :relation rel})
                             rels)
          result (#'uc/format-edges-section edges)]
      (is (str/includes? result "-impl->"))
      (is (str/includes? result "-super->"))
      (is (str/includes? result "-dep->"))
      (is (str/includes? result "-ref->"))
      (is (str/includes? result "-contra->"))
      (is (str/includes? result "-from->"))
      (is (str/includes? result "-apply->"))))

  (testing "unknown relation uses name"
    (let [edges [{:from "a" :to "b" :relation :custom-rel}]
          result (#'uc/format-edges-section edges)]
      (is (str/includes? result "custom-rel"))))

  (testing "truncates long IDs to 8 chars"
    (let [edges [{:from "20260207214919-604af5bd" :to "20260207214512-79c34d04"
                  :relation :implements}]
          result (#'uc/format-edges-section edges)]
      ;; Truncated IDs should be 8 chars
      (is (str/includes? result "20260207"))
      ;; Full ID should NOT appear
      (is (not (str/includes? result "20260207214919-604af5bd")))))

  (testing "edge count in header"
    (let [edges (repeat 3 {:from "a" :to "b" :relation :implements})
          result (#'uc/format-edges-section edges)]
      (is (str/includes? result "3 edges")))))

;; =============================================================================
;; Seed Resolution (mocked Chroma)
;; =============================================================================

(deftest resolve-seeds-explicit-ids-test
  (testing "explicit IDs pass through directly"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)]
      (let [seeds (uc/resolve-seeds {:ids ["id-1" "id-2"] :project-id "test"})]
        (is (= ["id-1" "id-2"] seeds))))))

(deftest resolve-seeds-from-task-test
  (testing "semantic search finds seeds from task description"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)
                  hive-mcp.chroma/search-similar
                  (fn [_query & _opts]
                    [{:metadata {:id "sem-1"}}
                     {:metadata {:id "sem-2"}}])]
      (let [seeds (uc/resolve-seeds {:task "authentication flow" :project-id "test"})]
        (is (= 2 (count seeds)))
        (is (some #(= "sem-1" %) seeds))
        (is (some #(= "sem-2" %) seeds))))))

(deftest resolve-seeds-combined-sources-test
  (testing "combines explicit IDs and semantic search results"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)
                  hive-mcp.chroma/search-similar
                  (fn [_query & _opts]
                    [{:metadata {:id "sem-3"}}])]
      (let [seeds (uc/resolve-seeds {:ids ["explicit-1"]
                                     :task "some query"
                                     :project-id "test"})]
        ;; Both explicit and semantic seeds present
        (is (some #(= "explicit-1" %) seeds))
        (is (some #(= "sem-3" %) seeds)))))

  (testing "deduplicates across sources"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)
                  hive-mcp.chroma/search-similar
                  (fn [_query & _opts]
                    [{:metadata {:id "dup-1"}}])]
      (let [seeds (uc/resolve-seeds {:ids ["dup-1"]
                                     :task "query"
                                     :project-id "test"})]
        ;; Should not have duplicates
        (is (= (count seeds) (count (distinct seeds))))))))

(deftest resolve-seeds-max-limit-test
  (testing "respects max-seed-nodes limit"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)
                  hive-mcp.chroma/search-similar
                  (fn [_query & _opts]
                    (map #(hash-map :metadata {:id (str "s-" %)}) (range 20)))]
      (let [seeds (uc/resolve-seeds {:task "big query" :project-id "test"})]
        (is (<= (count seeds) uc/max-seed-nodes))))))

(deftest resolve-seeds-chroma-error-test
  (testing "graceful degradation on Chroma error"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)
                  hive-mcp.chroma/search-similar
                  (fn [& _] (throw (Exception. "Chroma unavailable")))]
      (let [seeds (uc/resolve-seeds {:task "query" :ids ["fallback-1"] :project-id "test"})]
        ;; Falls back to explicit IDs only
        (is (= ["fallback-1"] seeds))))))

(deftest resolve-seeds-empty-task-test
  (testing "empty task skips semantic search"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)]
      (let [seeds (uc/resolve-seeds {:task "" :ids ["only-this"] :project-id "test"})]
        (is (= ["only-this"] seeds))))))

;; =============================================================================
;; Extension Points
;; =============================================================================

(deftest resolve-seeds-extension-test
  (testing ":uc/resolve-seeds extension overrides default"
    (ext/register! :uc/resolve-seeds
                   (fn [_opts] ["ext-seed-1" "ext-seed-2"]))
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)]
      (let [seeds (uc/resolve-seeds {:task "anything" :project-id "test"})]
        (is (= ["ext-seed-1" "ext-seed-2"] seeds)))))

  (testing ":uc/resolve-seeds extension failure falls back to default"
    (ext/register! :uc/resolve-seeds
                   (fn [_opts] (throw (Exception. "extension broke"))))
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)]
      (let [seeds (uc/resolve-seeds {:ids ["fallback"] :project-id "test"})]
        (is (= ["fallback"] seeds))))))

(deftest enrich-context-extension-test
  (testing ":uc/enrich extension adds data to context"
    (ext/register! :uc/enrich
                   (fn [ctx _opts]
                     (assoc ctx :enriched? true :extra-field "bonus")))
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)]
      (let [result (uc/gather-unified-context {:task "test" :project-id "p"})]
        (is (true? (:enriched? result)))
        (is (= "bonus" (:extra-field result))))))

  (testing ":uc/enrich failure returns unenriched context"
    (ext/register! :uc/enrich
                   (fn [_ctx _opts] (throw (Exception. "enrich broke"))))
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)]
      (let [result (uc/gather-unified-context {:task "test" :project-id "p"})]
        ;; Should still have base structure
        (is (= [] (:conventions result)))
        (is (= 0 (:seed-count result)))))))

;; =============================================================================
;; Traversal (mocked KG)
;; =============================================================================

(deftest traverse-from-seeds-test
  (testing "traversal collects nodes via KG edges"
    (with-redefs [hive-mcp.knowledge-graph.queries/traverse
                  (fn [start-id _opts]
                    ;; Return one neighbor per seed
                    [{:node-id (str start-id "-child")
                      :edge {:kg-edge/from start-id
                             :kg-edge/to (str start-id "-child")
                             :kg-edge/relation :implements
                             :kg-edge/confidence 0.9}}])]
      (let [result (#'uc/traverse-from-seeds ["seed-a" "seed-b"] "test-scope")]
        (is (set? (:nodes result)))
        ;; Seeds + their children
        (is (contains? (:nodes result) "seed-a"))
        (is (contains? (:nodes result) "seed-b"))
        (is (contains? (:nodes result) "seed-a-child"))
        (is (contains? (:nodes result) "seed-b-child"))
        ;; Edges collected
        (is (= 2 (count (:edges result))))
        (is (every? #(= :implements (:relation %)) (:edges result))))))

  (testing "traversal with no edges returns seeds only"
    (with-redefs [hive-mcp.knowledge-graph.queries/traverse
                  (fn [_start-id _opts] [])]
      (let [result (#'uc/traverse-from-seeds ["lone-seed"] "scope")]
        (is (= #{"lone-seed"} (:nodes result)))
        (is (= [] (:edges result))))))

  (testing "traversal respects max-traversal-nodes limit"
    (with-redefs [hive-mcp.knowledge-graph.queries/traverse
                  (fn [_start-id _opts]
                    ;; Return many neighbors to overflow limit
                    (map #(hash-map
                           :node-id (str "node-" %)
                           :edge {:kg-edge/from "seed"
                                  :kg-edge/to (str "node-" %)
                                  :kg-edge/relation :depends-on
                                  :kg-edge/confidence 0.8})
                         (range 50)))]
      (let [result (#'uc/traverse-from-seeds ["seed"] "scope")]
        (is (<= (count (:nodes result)) uc/max-traversal-nodes)))))

  (testing "traversal handles KG errors gracefully"
    (with-redefs [hive-mcp.knowledge-graph.queries/traverse
                  (fn [_start-id _opts]
                    (throw (Exception. "KG down")))]
      (let [result (#'uc/traverse-from-seeds ["seed-err"] "scope")]
        ;; Seeds still present even if traversal fails
        (is (contains? (:nodes result) "seed-err")))))

  (testing "nil seeds returns nil"
    (is (nil? (#'uc/traverse-from-seeds nil "scope")))
    (is (nil? (#'uc/traverse-from-seeds [] "scope")))))

(deftest traverse-deduplicates-edges-test
  (testing "duplicate edges from overlapping traversals are deduplicated"
    (with-redefs [hive-mcp.knowledge-graph.queries/traverse
                  (fn [_start-id _opts]
                    ;; Both seeds connect to same node via same relation
                    [{:node-id "shared-child"
                      :edge {:kg-edge/from "origin"
                             :kg-edge/to "shared-child"
                             :kg-edge/relation :implements
                             :kg-edge/confidence 0.9}}])]
      (let [result (#'uc/traverse-from-seeds ["seed-x" "seed-y"] "scope")]
        ;; Edges should be deduplicated by [from, to, relation]
        (let [edge-keys (map (juxt :from :to :relation) (:edges result))
              unique-keys (distinct edge-keys)]
          (is (= (count unique-keys) (count (:edges result)))))))))

;; =============================================================================
;; gather-unified-context Integration (full pipeline)
;; =============================================================================

(deftest gather-unified-context-no-chroma-test
  (testing "returns empty context when Chroma unavailable"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)]
      (let [result (uc/gather-unified-context
                    {:task "test task"
                     :project-id "test-project"})]
        (is (map? result))
        (is (= [] (:conventions result)))
        (is (= [] (:decisions result)))
        (is (= [] (:domain result)))
        (is (= [] (:snippets result)))
        (is (= 0 (:seed-count result)))
        (is (= 0 (:traversal-count result)))))))

(deftest gather-unified-context-with-explicit-ids-test
  (testing "uses explicit IDs as seeds when provided"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)
                  hive-mcp.chroma/get-entry-by-id
                  (fn [id]
                    (case id
                      "test-id-1" {:id "test-id-1" :type "convention" :content "Conv 1" :tags []}
                      "test-id-2" {:id "test-id-2" :type "decision" :content "Dec 1" :tags []}
                      nil))]
      (let [result (uc/gather-unified-context
                    {:task "test"
                     :ids ["test-id-1" "test-id-2"]
                     :project-id "test"})]
        ;; Seeds from explicit IDs
        (is (= 2 (:seed-count result)))
        ;; Entries fetched and classified
        (is (= 1 (count (:conventions result))))
        (is (= 1 (count (:decisions result))))))))

(deftest gather-unified-context-full-pipeline-test
  (testing "end-to-end: semantic seeds → KG traversal → batch-get → classify"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)
                  hive-mcp.chroma/search-similar
                  (fn [_query & _opts]
                    [{:metadata {:id "root-1"}}])
                  hive-mcp.knowledge-graph.queries/traverse
                  (fn [start-id _opts]
                    (when (= start-id "root-1")
                      [{:node-id "child-1"
                        :edge {:kg-edge/from "root-1"
                               :kg-edge/to "child-1"
                               :kg-edge/relation :implements
                               :kg-edge/confidence 0.9}}
                       {:node-id "child-2"
                        :edge {:kg-edge/from "root-1"
                               :kg-edge/to "child-2"
                               :kg-edge/relation :depends-on
                               :kg-edge/confidence 0.8}}]))
                  hive-mcp.chroma/get-entry-by-id
                  (fn [id]
                    (case id
                      "root-1"  {:id "root-1"  :type "axiom"      :content "Root axiom" :tags ["core"]}
                      "child-1" {:id "child-1" :type "convention" :content "Child conv" :tags ["pattern"]}
                      "child-2" {:id "child-2" :type "decision"   :content "Child dec"  :tags ["arch"]}
                      nil))]
      (let [result (uc/gather-unified-context
                    {:task "test authentication"
                     :project-id "test-project"})]
        ;; Seed count
        (is (= 1 (:seed-count result)))
        ;; Traversal found 3 nodes (root + 2 children)
        (is (= 3 (:traversal-count result)))
        ;; Classification
        (is (= 1 (count (:conventions result))))
        (is (= 1 (count (:decisions result))))
        ;; axiom → :domain
        (is (= 1 (count (:domain result))))
        (is (= 0 (count (:snippets result))))
        ;; Edges preserved
        (is (= 2 (count (:edges result))))
        ;; Node IDs tracked
        (is (set? (:node-ids result)))
        (is (= 3 (count (:node-ids result))))))))

(deftest gather-unified-context-truncation-test
  (testing "long content is truncated during classification"
    (let [long-content (apply str (repeat 1000 "x"))]
      (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)
                    hive-mcp.chroma/get-entry-by-id
                    (fn [id]
                      {:id id :type "note" :content long-content :tags []})]
        (let [result (uc/gather-unified-context
                      {:ids ["long-1"] :project-id "test"})]
          ;; Content should be truncated
          (let [entry (first (:domain result))]
            (is (<= (count (:content entry)) (+ uc/max-content-chars 3)))))))))

(deftest gather-unified-context-batch-get-failure-test
  (testing "batch-get failure for individual entries is non-fatal"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)
                  hive-mcp.chroma/get-entry-by-id
                  (fn [id]
                    (case id
                      "ok-1"  {:id "ok-1" :type "note" :content "Works" :tags []}
                      "err-1" (throw (Exception. "fetch failed"))
                      nil))]
      (let [result (uc/gather-unified-context
                    {:ids ["ok-1" "err-1"] :project-id "test"})]
        ;; Should still get the successful entry
        (is (= 1 (count (:domain result))))
        (is (= "ok-1" (-> result :domain first :id)))))))

(deftest gather-unified-context-kg-failure-test
  (testing "KG traversal failure degrades gracefully to seed-only fetch"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)
                  hive-mcp.knowledge-graph.queries/traverse
                  (fn [& _] (throw (Exception. "KG unavailable")))
                  hive-mcp.chroma/get-entry-by-id
                  (fn [id]
                    {:id id :type "convention" :content "Still works" :tags []})]
      (let [result (uc/gather-unified-context
                    {:ids ["fallback-seed"] :project-id "test"})]
        ;; Seed still fetched even without traversal
        (is (= 1 (:seed-count result)))
        (is (= 1 (count (:conventions result))))))))

(deftest gather-unified-context-scope-defaults-test
  (testing "scope defaults to project-id when not specified"
    (let [captured-scope (atom nil)]
      (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)
                    hive-mcp.knowledge-graph.queries/traverse
                    (fn [_start-id opts]
                      (reset! captured-scope (:scope opts))
                      [])]
        (uc/gather-unified-context
         {:ids ["test-id"] :project-id "my-project"})
        (is (= "my-project" @captured-scope)))))

  (testing "explicit scope overrides project-id"
    (let [captured-scope (atom nil)]
      (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)
                    hive-mcp.knowledge-graph.queries/traverse
                    (fn [_start-id opts]
                      (reset! captured-scope (:scope opts))
                      [])]
        (uc/gather-unified-context
         {:ids ["test-id"] :project-id "my-project" :scope "custom-scope"})
        (is (= "custom-scope" @captured-scope))))))

;; =============================================================================
;; Availability Check
;; =============================================================================

(deftest unified-context-available-test
  (testing "available when Chroma configured"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)]
      (is (true? (uc/unified-context-available?)))))

  (testing "unavailable when Chroma not configured"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)]
      (is (false? (uc/unified-context-available?)))))

  (testing "unavailable when Chroma throws"
    (with-redefs [hive-mcp.chroma/embedding-configured?
                  (fn [] (throw (Exception. "Chroma broken")))]
      (is (false? (uc/unified-context-available?))))))

;; =============================================================================
;; Constants / Configuration
;; =============================================================================

(deftest constants-test
  (testing "structural relations set contains expected relations"
    (is (contains? uc/structural-relations :implements))
    (is (contains? uc/structural-relations :supersedes))
    (is (contains? uc/structural-relations :depends-on))
    (is (contains? uc/structural-relations :refines))
    (is (contains? uc/structural-relations :contradicts))
    (is (contains? uc/structural-relations :derived-from))
    (is (contains? uc/structural-relations :applies-to)))

  (testing "max constants are positive"
    (is (pos? uc/max-seed-nodes))
    (is (pos? uc/max-traversal-nodes))
    (is (pos? uc/max-traversal-depth))
    (is (pos? uc/max-content-chars))))
