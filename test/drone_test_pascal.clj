(ns drone-test-pascal)

(defn pascal-row [n]
  "Generate the nth row of Pascal's triangle (0-indexed)."
  (loop [row []
         k 0]
    (if (> k n)
      row
      (recur (conj row (comb n k)) (inc k)))))

(defn comb [n k]
  "Calculate the binomial coefficient C(n, k)."
  (if (or (zero? k) (= k n))
    1
    (+ (comb (dec n) (dec k)) (comb (dec n) k))))