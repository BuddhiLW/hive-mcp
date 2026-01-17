(ns hive-mcp.examples.fibonacci.iterative)

(defn fib-iter [n]
  (loop [a 0 b 1 i n]
    (if (zero? i)
      a
      (recur b (+ a b) (dec i)))))