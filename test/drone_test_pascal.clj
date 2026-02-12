(ns drone-test-pascal)

(defn comb
  "Calculate the binomial coefficient C(n, k)."
  [n k]
  (if (or (zero? k) (= k n))
    1
    (+ (comb (dec n) (dec k)) (comb (dec n) k))))

(defn pascal-row
  "Generate the nth row of Pascal's triangle (0-indexed)."
  [n]
  (loop [row []
         k 0]
    (if (> k n)
      row
      (recur (conj row (comb n k)) (inc k)))))