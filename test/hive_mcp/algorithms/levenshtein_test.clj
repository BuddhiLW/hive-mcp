(ns hive-mcp.algorithms.levenshtein-test
  (:require [clojure.test :refer :all]
            [hive-mcp.algorithms.levenshtein :refer [levenshtein-distance]]))

(deftest test-levenshtein-distance
  (testing "Empty strings"
    (is (= 0 (levenshtein-distance "" "")) "Both strings empty")
    (is (= 1 (levenshtein-distance "" "a")) "One string empty, other has one char")
    (is (= 1 (levenshtein-distance "a" "")) "One string empty, other has one char"))

  (testing "Identical strings"
    (is (= 0 (levenshtein-distance "abc" "abc")) "Same string")
    (is (= 0 (levenshtein-distance "" "")) "Both strings empty"))

  (testing "Single char difference"
    (is (= 1 (levenshtein-distance "abc" "abd")) "One char different")
    (is (= 1 (levenshtein-distance "a" "b")) "Single char strings"))

  (testing "Complete difference"
    (is (= 3 (levenshtein-distance "abc" "def")) "No common chars")
    (is (= 2 (levenshtein-distance "ab" "cd")) "No common chars"))

  (testing "Practical example"
    (is (= 3 (levenshtein-distance "kitten" "sitting")) "kitten to sitting"))

  (testing "Edge cases"
    (is (= 5 (levenshtein-distance "abcde" "fghij")) "Longer strings with no common chars")))
