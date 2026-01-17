(ns hive-mcp.examples.matrix-wave.row-ops-test
  (:require [clojure.test :refer :all]
            [hive-mcp.examples.matrix-wave.row-ops :refer [row-sum row-avg row-max]]))

(deftest row-sum-test
  (testing "row-sum calculates correct sum"
    (is (= 15 (row-sum [1 2 3 4 5])))
    (is (= 40 (row-sum [6 7 8 9 10])))))

(deftest row-avg-test
  (testing "row-avg calculates correct average"
    (is (= 3 (row-avg [1 2 3 4 5])))))

(deftest row-max-test
  (testing "row-max finds maximum element"
    (is (= 5 (row-max [1 2 3 4 5])))
    (is (= 10 (row-max [6 7 8 9 10])))))
