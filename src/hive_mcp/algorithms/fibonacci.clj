(ns hive-mcp.algorithms.fibonacci
  "Namespace for Fibonacci sequence calculations with memoization.")

;; Memoization cache for Fibonacci numbers
(def ^:private fib-cache (atom {0 0, 1 1}))

(defn- fib-helper
  "Helper function to compute Fibonacci numbers using memoization."
  [n]
  (if-let [cached (@fib-cache n)]
    cached
    (let [result (+ (fib-helper (- n 1)) (fib-helper (- n 2)))]
      (swap! fib-cache assoc n result)
      result)))

(defn fibonacci
  "Returns the nth Fibonacci number using memoization for efficiency.

  Examples:
    (fibonacci 0)  => 0
    (fibonacci 1)  => 1
    (fibonacci 5)  => 5
    (fibonacci 10) => 55
    (fibonacci 20) => 6765"
  [n]
  (if (or (neg? n) (not (integer? n)))
    (throw (IllegalArgumentException. "Input must be a non-negative integer."))
    (fib-helper n)))

;; Initialize the cache for the first two Fibonacci numbers
(reset! fib-cache {0 0, 1 1})