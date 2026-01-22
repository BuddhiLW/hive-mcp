(ns hive-mcp.algorithms.quicksort
  "Namespace for the quicksort algorithm implementation.")

(defn- qsort-partition
  "Partition a collection around a pivot.

  Args:
  - coll: The collection to partition.
  - pivot: The pivot element to partition around.

  Returns:
  - A vector containing two vectors: elements less than the pivot and elements greater than the pivot.

  Example:
  (qsort-partition [3 1 4 1 5 9 2 6] 4)
  ; => [[3 1 1 2] [5 9 6]]"
  [coll pivot]
  (let [[lesser greater] (reduce
                          (fn [[lesser greater] x]
                            (if (< x pivot)
                              [(conj lesser x) greater]
                              [lesser (conj greater x)]))
                          [[] []]
                          coll)]
    [lesser greater]))

(defn quicksort
  "Sort a collection using the quicksort algorithm.
  
  Args:
  - coll: The collection to sort.
  
  Returns:
  - A sorted vector.
  
  Examples:
  (quicksort []) ; => []
  (quicksort [5]) ; => [5]
  (quicksort [3 1 4 1 5 9 2 6]) ; => [1 1 2 3 4 5 6 9]
  (quicksort [1 2 3 4 5]) ; => [1 2 3 4 5]
  (quicksort [5 4 3 2 1]) ; => [1 2 3 4 5]"
  [coll]
  (cond
    (empty? coll) []
    (= (count coll) 1) coll
    :else (let [pivot (first coll)
                [lesser greater] (qsort-partition (rest coll) pivot)]
            (vec (concat (quicksort lesser) [pivot] (quicksort greater))))))