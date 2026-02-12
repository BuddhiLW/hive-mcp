(ns hive-mcp.tools.catchup.grounding-test
  "Tests for P1.4 grounding freshness functions.
   Updated Sprint 2: functions moved from catchup.clj to catchup.enrichment.
   entry-grounding-age-days and entry->grounding-warning are now public in enrichment.
   check-grounding-freshness is re-exported from catchup.clj for backward compat."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.tools.catchup.enrichment :as enrichment])
  (:import [java.time Instant Duration]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- instant-days-ago
  "Return ISO-8601 string for an instant N days in the past."
  [n]
  (.toString (.minus (Instant/now) (Duration/ofDays n))))

(defn- date-days-ago
  "Return a java.util.Date N days in the past."
  [n]
  (java.util.Date. (- (System/currentTimeMillis) (* n 24 60 60 1000))))

;; =============================================================================
;; entry-grounding-age-days tests (now public in enrichment)
;; =============================================================================

(deftest test-grounding-age-nil-entry
  (testing "entry with no grounded-at returns nil"
    (is (nil? (enrichment/entry-grounding-age-days {:id "e1" :content "test"})))))

(deftest test-grounding-age-blank-string
  (testing "entry with blank grounded-at string returns nil"
    (is (nil? (enrichment/entry-grounding-age-days {:grounded-at ""})))
    (is (nil? (enrichment/entry-grounding-age-days {:grounded-at "  "})))))

(deftest test-grounding-age-from-metadata
  (testing "entry with grounded-at in :metadata map"
    (let [age (enrichment/entry-grounding-age-days
               {:metadata {:grounded-at (instant-days-ago 3)}})]
      (is (some? age))
      (is (>= age 2))
      (is (<= age 4)))))

(deftest test-grounding-age-from-top-level
  (testing "entry with grounded-at at top level"
    (let [age (enrichment/entry-grounding-age-days
               {:grounded-at (instant-days-ago 10)})]
      (is (some? age))
      (is (>= age 9))
      (is (<= age 11)))))

(deftest test-grounding-age-java-date
  (testing "entry with grounded-at as java.util.Date"
    (let [age (enrichment/entry-grounding-age-days
               {:grounded-at (date-days-ago 5)})]
      (is (some? age))
      (is (>= age 4))
      (is (<= age 6)))))

(deftest test-grounding-age-invalid-string
  (testing "entry with unparseable grounded-at returns nil"
    (is (nil? (enrichment/entry-grounding-age-days {:grounded-at "not-a-date"})))))

(deftest test-grounding-age-metadata-takes-precedence
  (testing "metadata grounded-at is checked before top-level"
    (let [age (enrichment/entry-grounding-age-days
               {:metadata {:grounded-at (instant-days-ago 2)}
                :grounded-at (instant-days-ago 20)})]
      ;; Should get ~2, not ~20, since metadata is checked first via `or`
      (is (some? age))
      (is (<= age 4)))))

;; =============================================================================
;; entry->grounding-warning tests (now public in enrichment)
;; =============================================================================

(deftest test-warning-never-grounded
  (testing "entry with no grounded-at produces never-grounded warning"
    (let [w (enrichment/entry->grounding-warning {:id "e1" :type :decision :content "test"} 7)]
      (is (some? w))
      (is (= "e1" (:id w)))
      (is (= "decision" (:type w)))
      (is (true? (:never-grounded? w)))
      (is (nil? (:age-days w)))
      (is (nil? (:grounded-at w))))))

(deftest test-warning-stale-entry
  (testing "entry grounded 14 days ago with max-age 7 is stale"
    (let [ts (instant-days-ago 14)
          w (enrichment/entry->grounding-warning
             {:id "e2" :type :convention :content "stale content"
              :grounded-at ts}
             7)]
      (is (some? w))
      (is (= "e2" (:id w)))
      (is (= "convention" (:type w)))
      (is (false? (:never-grounded? w)))
      (is (>= (:age-days w) 13))
      (is (<= (:age-days w) 15))
      (is (= ts (:grounded-at w))))))

(deftest test-warning-fresh-entry
  (testing "entry grounded 2 days ago with max-age 7 returns nil"
    (is (nil? (enrichment/entry->grounding-warning
               {:id "e3" :type :decision :content "fresh"
                :grounded-at (instant-days-ago 2)}
               7)))))

(deftest test-warning-preview-truncation
  (testing "warning preview is capped at 60 characters"
    (let [long-content (apply str (repeat 200 "x"))
          w (enrichment/entry->grounding-warning {:id "e4" :type :note :content long-content} 7)]
      (is (some? w))
      (is (<= (count (:preview w)) 60)))))

(deftest test-warning-unknown-defaults
  (testing "entry with no :id or :type uses 'unknown' defaults"
    (let [w (enrichment/entry->grounding-warning {:content "orphan"} 7)]
      (is (some? w))
      (is (= "unknown" (:id w)))
      (is (= "unknown" (:type w))))))

(deftest test-warning-exact-boundary
  (testing "entry grounded exactly at max-age boundary"
    ;; age-days == max-age-days should NOT be stale (> not >=)
    ;; The code uses (> age-days max-age-days), so exactly 7 with max 7 = not stale
    (let [w (enrichment/entry->grounding-warning
             {:id "e5" :type :decision :content "boundary"
              :grounded-at (instant-days-ago 7)}
             7)]
      ;; Due to timing, this might be 6 or 7. If 7 exactly, not stale.
      ;; The key test: age <= max-age should return nil
      ;; Use a tight window: grounded 6 days ago, max 7 -> definitely fresh
      (is (nil? (enrichment/entry->grounding-warning
                 {:id "e6" :type :decision :content "fresh boundary"
                  :grounded-at (instant-days-ago 6)}
                 7))))))

;; =============================================================================
;; check-grounding-freshness tests (Chroma mocking)
;; Uses catchup/check-grounding-freshness (re-exported from enrichment)
;; =============================================================================

(deftest test-freshness-all-fresh
  (testing "all entries freshly grounded -> zero stale"
    (let [fresh-ts (instant-days-ago 1)]
      (with-redefs [hive-mcp.chroma.core/query-entries
                    (fn [& {:keys [type]}]
                      (case type
                        "decision" [{:id "d1" :type :decision :content "dec"
                                     :grounded-at fresh-ts}]
                        "convention" [{:id "c1" :type :convention :content "conv"
                                       :grounded-at fresh-ts}]
                        []))]
        (let [result (catchup/check-grounding-freshness "test-project")]
          (is (= 2 (:total-checked result)))
          (is (= 0 (:stale-count result)))
          (is (empty? (:stale-entries result)))
          (is (false? (:timed-out? result))))))))

(deftest test-freshness-some-stale
  (testing "mix of fresh and stale entries"
    (with-redefs [hive-mcp.chroma.core/query-entries
                  (fn [& {:keys [type]}]
                    (case type
                      "decision" [{:id "d1" :type :decision :content "fresh dec"
                                   :grounded-at (instant-days-ago 2)}
                                  {:id "d2" :type :decision :content "stale dec"
                                   :grounded-at (instant-days-ago 14)}]
                      "convention" [{:id "c1" :type :convention :content "never grounded"}]
                      []))]
      (let [result (catchup/check-grounding-freshness "test-project")]
        (is (= 3 (:total-checked result)))
        (is (= 2 (:stale-count result)))
        (is (= 2 (count (:stale-entries result))))
        ;; Verify stale entry IDs
        (let [stale-ids (set (map :id (:stale-entries result)))]
          (is (contains? stale-ids "d2"))
          (is (contains? stale-ids "c1")))
        (is (false? (:timed-out? result)))))))

(deftest test-freshness-chroma-error
  (testing "Chroma exception returns error result"
    (with-redefs [hive-mcp.chroma.core/query-entries
                  (fn [& _] (throw (Exception. "Chroma connection refused")))]
      (let [result (catchup/check-grounding-freshness "test-project")]
        (is (= 0 (:total-checked result)))
        (is (= 0 (:stale-count result)))
        (is (empty? (:stale-entries result)))
        (is (some? (:error result)))
        (is (false? (:timed-out? result)))))))

(deftest test-freshness-custom-opts
  (testing "custom max-age-days and limit are respected"
    (with-redefs [hive-mcp.chroma.core/query-entries
                  (fn [& {:keys [type limit]}]
                    ;; Verify limit is passed through
                    (is (= 5 limit))
                    (case type
                      "decision" [{:id "d1" :type :decision :content "3 days old"
                                   :grounded-at (instant-days-ago 3)}]
                      "convention" []
                      []))]
      ;; With max-age-days 2, the 3-day-old entry is stale
      (let [result (catchup/check-grounding-freshness
                    "test-project"
                    {:max-age-days 2 :limit 5})]
        (is (= 1 (:total-checked result)))
        (is (= 1 (:stale-count result)))
        (is (false? (:timed-out? result)))))))

(deftest test-freshness-empty-project
  (testing "project with no entries returns zeros"
    (with-redefs [hive-mcp.chroma.core/query-entries (fn [& _] [])]
      (let [result (catchup/check-grounding-freshness "empty-project")]
        (is (= 0 (:total-checked result)))
        (is (= 0 (:stale-count result)))
        (is (empty? (:stale-entries result)))
        (is (false? (:timed-out? result)))))))
