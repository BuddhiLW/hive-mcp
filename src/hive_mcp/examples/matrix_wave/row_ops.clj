(ns hive-mcp.examples.matrix-wave.row-ops)

(defn row-sum
  "Calculate the sum of elements in a row."
  [row]
  (reduce + row))

(defn row-avg
  "Calculate the average of elements in a row."
  [row]
  (quot (reduce + row) (count row)))

(defn row-max
  "Find the maximum element in a row."
  [row]
  (apply max row))
