(ns hive-mcp.algorithms.bloom-filter-test
  (:require [clojure.test :refer :all]
            [hive-mcp.algorithms.bloom-filter :refer :all]))

(deftest bloom-filter-basic-test
  (testing "Basic Bloom filter operations"
    (let [bf (create 100 0.01)
          bf1 (add bf "hello")
          bf2 (add bf1 "world")]
      
      (testing "contains? should return true for added elements"
        (is (contains? bf2 "hello"))
        (is (contains? bf2 "world")))
      
      (testing "contains? should return false for definitely not present elements"
        ;; Note: There's a small chance of false positive, but with 1% FPR,
        ;; it's very unlikely for random strings
        (is (not (contains? bf2 "not-present")))))))

(deftest bloom-filter-estimation-test
  (testing "Bloom filter count estimation"
    (let [bf (create 1000 0.01)
          elements (map str (range 100))
          bf-filled (reduce add bf elements)]
      
      (testing "estimated-count should be close to actual count"
        (let [estimated (estimated-count bf-filled)]
          ;; Estimated count should be within reasonable bounds of actual
          (is (<= 90 estimated 110)))))))

(deftest bloom-filter-merge-test
  (testing "Bloom filter merging"
    (let [bf1 (-> (create 100 0.01)
                  (add "item1")
                  (add "item2"))
          bf2 (-> (create 100 0.01)
                  (add "item2")
                  (add "item3"))
          merged (merge-filters bf1 bf2)]
      
      (testing "Merge should work with compatible filters"
        (when merged
          (is (contains? merged "item1"))
          (is (contains? merged "item2"))
          (is (contains? merged "item3")))))))

(deftest bloom-filter-false-positive-test
  (testing "Bloom filter false positive probability calculation"
    (let [bf (create 100 0.01)
          fpr (false-positive-probability bf)]
      
      (testing "Initial false positive rate should be very low"
        (is (< fpr 0.001)))
      
      (testing "False positive rate should increase as we add elements"
        (let [bf-filled (reduce add bf (map str (range 50)))
              fpr-filled (false-positive-probability bf-filled)]
          (is (> fpr-filled fpr)))))))

(deftest bloom-filter-utility-functions-test
  (testing "Utility functions"
    (let [bf (-> (create 100 0.01)
                 (bulk-add ["a" "b" "c" "d"]))]
      
      (testing "contains-any? should find existing elements"
        (is (contains-any? bf ["a" "x"])))
      
      (testing "contains-all? should return true for all existing elements"
        (is (contains-all? bf ["a" "b" "c"])))
      
      (testing "bits-set-percentage should return a valid percentage"
        (let [percentage (bits-set-percentage bf)]
          (is (>= percentage 0.0))
          (is (<= percentage 100.0))))
      
      (testing "clear should reset the filter"
        (let [cleared (clear bf)]
          (is (= 0 (estimated-count cleared)))
          (is (not (contains? cleared "a"))))))))

(deftest bloom-filter-parameter-validation-test
  (testing "Parameter validation and defaults"
    (let [bf-default (create-with-defaults 100)]
      (testing "create-with-defaults should create a valid filter"
        (is (instance? hive_mcp.algorithms.bloom_filter.BloomFilter bf-default))
        (is (contains? (add bf-default "test") "test"))))))