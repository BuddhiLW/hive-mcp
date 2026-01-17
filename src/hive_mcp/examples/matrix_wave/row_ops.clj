(ns hive-mcp.examples.matrix-wave.row-ops)

(defn row-sum [row]
  "Calculate the sum of elements in a row."
  (reduce + row))

(defn row-avg [row]
  "Calculate the average of elements in a row."
  (quot (reduce + row) (count row)))

(defn row-max [row]
  "Find the maximum element in a row."
  (apply max row))
