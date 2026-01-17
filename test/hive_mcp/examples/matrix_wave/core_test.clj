(ns hive-mcp.examples.matrix-wave.core-test
  (:require [clojure.test :refer :all]
            [hive-mcp.examples.matrix-wave.core :refer :all]
            [hive-mcp.examples.matrix-wave.aggregator :refer :all]))

(deftest generate-matrix-test
  (testing "generate-matrix returns a vector"
    (is (vector? (generate-matrix))))

  (testing "generate-matrix has 5 rows"
    (is (= 5 (count (generate-matrix)))))

  (testing "each row in generate-matrix has 5 elements"
    (let [matrix (generate-matrix)]
      (is (every? #(= 5 (count %)) matrix)))))

(deftest full-pipeline-test
  (testing "Full pipeline test"
    (let [matrix (generate-matrix)
          result (aggregate-rows matrix :sum)]
      (is (= [15 40 65 90 115] result)))))