(ns hive-mcp.algorithms.factorial
  "Namespace for factorial-related algorithms.")

(defn factorial
  "Compute the factorial of a non-negative integer n (n!) using a tail-recursive implementation.

  Examples:
    (factorial 0) => 1
    (factorial 1) => 1
    (factorial 5) => 120
    (factorial 10) => 3628800"
  [n]
  (loop [current n
         result 1]
    (if (zero? current)
      result
      (recur (dec current) (* result current)))))