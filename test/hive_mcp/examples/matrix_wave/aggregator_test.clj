(ns hive-mcp.examples.matrix-wave.aggregator-test
  (:require [clojure.test :refer :all]
            [hive-mcp.examples.matrix-wave.aggregator :refer :all]))

(deftest aggregate-rows-test
  (let [test-matrix [[1 2 3 4 5]
                     [6 7 8 9 10]
                     [11 12 13 14 15]
                     [16 17 18 19 20]
                     [21 22 23 24 25]]]
    (is (= [15 40 65 90 115] (aggregate-rows test-matrix :sum)))))