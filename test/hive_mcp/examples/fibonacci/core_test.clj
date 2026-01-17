(ns hive-mcp.examples.fibonacci.core-test
  (:require [clojure.test :refer :all]
            [hive-mcp.examples.fibonacci.core :refer :all]))

(deftest fib-test
  (testing "base cases"
    (is (= 0 (fib 0)))
    (is (= 1 (fib 1))))
  (testing "recursive"
    (is (= 55 (fib 10)))))