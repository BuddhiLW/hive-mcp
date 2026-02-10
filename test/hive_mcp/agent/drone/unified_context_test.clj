(ns hive-mcp.agent.drone.unified-context-test
  "Tests for unified KG traversal context gathering."
  (:require [clojure.test :refer [deftest testing is are]]
            [hive-mcp.agent.drone.unified-context :as uc]))

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

(deftest truncate-content-test
  (testing "truncates long content"
    (let [long-str (apply str (repeat 600 "x"))
          result (#'uc/truncate-content long-str)]
      (is (<= (count result) (+ uc/max-content-chars 3)))
      (is (clojure.string/ends-with? result "..."))))

  (testing "leaves short content unchanged"
    (is (= "hello" (#'uc/truncate-content "hello"))))

  (testing "handles nil gracefully"
    (is (nil? (#'uc/truncate-content nil)))))

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
      (is (clojure.string/includes? result "Unified Project Context"))
      (is (clojure.string/includes? result "Conventions"))
      (is (clojure.string/includes? result "Decisions"))
      (is (clojure.string/includes? result "KG Structure"))))

  (testing "returns nil for empty context"
    (is (nil? (uc/format-unified-context
               {:conventions [] :decisions [] :domain [] :snippets []})))))

(deftest format-edges-section-test
  (testing "formats edges with arrow notation"
    (let [edges [{:from "20260207-abc" :to "20260206-def" :relation :implements}
                 {:from "20260206-def" :to "20260205-ghi" :relation :depends-on}]
          result (#'uc/format-edges-section edges)]
      (is (clojure.string/includes? result "-impl->"))
      (is (clojure.string/includes? result "-dep->"))))

  (testing "returns nil for empty edges"
    (is (nil? (#'uc/format-edges-section [])))))

;; =============================================================================
;; gather-unified-context Integration (graceful degradation)
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

(deftest unified-context-available-test
  (testing "available when Chroma configured"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly true)]
      (is (true? (uc/unified-context-available?)))))

  (testing "unavailable when Chroma not configured"
    (with-redefs [hive-mcp.chroma/embedding-configured? (constantly false)]
      (is (false? (uc/unified-context-available?))))))
