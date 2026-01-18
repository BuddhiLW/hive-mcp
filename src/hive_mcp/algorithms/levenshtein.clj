(ns hive-mcp.algorithms.levenshtein)

(defn levenshtein-distance
  "Calculates the Levenshtein distance between two strings using dynamic programming.

  The Levenshtein distance is the minimum number of single-character edits
  (insertions, deletions, or substitutions) required to change one string into the other.

  Parameters:
  - s1: The first string.
  - s2: The second string.

  Returns:
  - The minimum number of edits required to transform s1 into s2.

  Example:
  (levenshtein-distance \"kitten\" \"sitting\") ;=> 3"
  [s1 s2]
  (let [m (count s1)
        n (count s2)]
    (cond
      (zero? m) n
      (zero? n) m
      :else
      (let [;; Initialize first row: [0 1 2 ... n]
            init-row (vec (range (inc n)))
            ;; Process each character of s1
            final-row
            (reduce
             (fn [prev-row i]
               (reduce
                (fn [curr-row j]
                  (let [cost (if (= (nth s1 i) (nth s2 j)) 0 1)
                        del (inc (nth curr-row j))        ; deletion
                        ins (inc (nth prev-row (inc j)))  ; insertion
                        sub (+ (nth prev-row j) cost)]    ; substitution
                    (conj curr-row (min del ins sub))))
                [(inc i)]  ; Start row with i+1 (left edge)
                (range n)))
             init-row
             (range m))]
        (peek final-row)))))
