(ns hive-mcp.examples.pascal-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.examples.pascal :as p]))

(deftest test-recursive-pascal
  (let [pascal (p/create-recursive-pascal)]
    (testing "value"
      (is (= 1 (p/-value pascal 0 0)))
      (is (= 6 (p/-value pascal 4 2))))
    (testing "row"
      (is (= [1 4 6 4 1] (p/-row pascal 4))))
    (testing "triangle"
      (is (= [[1] [1 1] [1 2 1]] (p/-triangle pascal 3))))))

(deftest test-memoized-pascal
  (let [pascal (p/create-memoized-pascal)]
    (is (= 6 (p/-value pascal 4 2)))
    (is (= [1 4 6 4 1] (p/-row pascal 4)))))

(deftest test-both-same-results
  (let [rec (p/create-recursive-pascal)
        memo (p/create-memoized-pascal)]
    (is (= (p/-triangle rec 5) (p/-triangle memo 5)))))