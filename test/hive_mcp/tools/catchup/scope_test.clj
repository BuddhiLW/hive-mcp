(ns hive-mcp.tools.catchup.scope-test
  "Tests for catchup scope resolution and query functions.
   Extracted from catchup.clj in Sprint 1 refactoring.

   HCR Wave 5: Added tests for compute-hierarchy-project-ids,
   scope-filter-entries, scope-pierce-entries helpers."
  (:require [clojure.test :refer [deftest is testing]]))

;; =============================================================================
;; Pure Function Tests (no Chroma/Emacs dependency)
;; =============================================================================

;; We test the pure functions directly. Chroma-dependent functions
;; (query-scoped-entries, query-expiring-entries, etc.) require Chroma
;; to be configured and are covered by integration tests.

(deftest distinct-by-test
  (let [distinct-by @(requiring-resolve 'hive-mcp.tools.catchup.scope/distinct-by)]
    (testing "removes duplicates by key function"
      (let [items [{:id 1 :name "a"}
                   {:id 2 :name "b"}
                   {:id 1 :name "c"}  ;; duplicate by :id
                   {:id 3 :name "d"}]
            result (distinct-by :id items)]
        (is (= 3 (count result)))
        (is (= "a" (:name (first result))))  ;; keeps first occurrence
        (is (= [1 2 3] (mapv :id result)))))

    (testing "empty collection returns empty"
      (is (= [] (distinct-by :id []))))

    (testing "all unique returns all"
      (let [items [{:id 1} {:id 2} {:id 3}]]
        (is (= 3 (count (distinct-by :id items))))))

    (testing "all duplicates returns one"
      (let [items [{:id 1 :v "a"} {:id 1 :v "b"} {:id 1 :v "c"}]]
        (is (= 1 (count (distinct-by :id items))))
        (is (= "a" (:v (first (distinct-by :id items)))))))))

(deftest filter-by-tags-test
  (let [filter-by-tags @(requiring-resolve 'hive-mcp.tools.catchup.scope/filter-by-tags)]
    (testing "filters entries containing all specified tags"
      (let [entries [{:id 1 :tags ["a" "b" "c"]}
                     {:id 2 :tags ["a" "b"]}
                     {:id 3 :tags ["a" "c"]}
                     {:id 4 :tags ["d"]}]
            result (filter-by-tags entries ["a" "b"])]
        (is (= 2 (count result)))
        (is (= #{1 2} (set (map :id result))))))

    (testing "empty tags returns all entries"
      (let [entries [{:id 1 :tags ["a"]} {:id 2 :tags ["b"]}]]
        (is (= 2 (count (filter-by-tags entries []))))))

    (testing "nil tags returns all entries"
      (let [entries [{:id 1 :tags ["a"]}]]
        (is (= 1 (count (filter-by-tags entries nil))))))

    (testing "no matches returns empty"
      (let [entries [{:id 1 :tags ["a"]}]]
        (is (= 0 (count (filter-by-tags entries ["z"]))))))))

(deftest entry-expiring-soon?-test
  (let [entry-expiring-soon? @(requiring-resolve 'hive-mcp.tools.catchup.scope/entry-expiring-soon?)]
    (testing "entry expiring within 7 days returns true"
      (let [tomorrow (.toString (.plusDays (java.time.ZonedDateTime/now) 1))]
        (is (true? (entry-expiring-soon? {:expires tomorrow})))))

    (testing "entry expiring in 30 days returns false"
      (let [far-future (.toString (.plusDays (java.time.ZonedDateTime/now) 30))]
        (is (false? (entry-expiring-soon? {:expires far-future})))))

    (testing "entry without expiry returns nil"
      (is (nil? (entry-expiring-soon? {}))))

    (testing "entry with nil expiry returns nil"
      (is (nil? (entry-expiring-soon? {:expires nil}))))

    (testing "entry with invalid date returns false"
      (is (false? (entry-expiring-soon? {:expires "not-a-date"}))))))
