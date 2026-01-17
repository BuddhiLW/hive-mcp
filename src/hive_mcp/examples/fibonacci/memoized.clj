(ns hive-mcp.examples.fibonacci.memoized
  "Memoized fibonacci implementation.
   
   Uses Clojure's built-in memoize for O(n) time with O(n) space trade-off.
   Caches previously computed values to avoid exponential recursion.")

(def fib-memo
  "Memoized fibonacci function - caches results for repeated calls."
  (memoize
   (fn [n]
     (if (<= n 1)
       n
       (+ (fib-memo (dec n)) (fib-memo (- n 2)))))))
