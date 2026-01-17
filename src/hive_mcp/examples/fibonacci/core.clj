(ns hive-mcp.examples.fibonacci.core)

(defn fib
  "Calculates the nth Fibonacci number."
  [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))