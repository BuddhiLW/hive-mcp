(ns hive-mcp.examples.matrix-wave.aggregator
  (:require [hive-mcp.examples.matrix-wave.row-ops :refer [row-sum row-avg row-max]]))

(defn aggregate-rows [matrix op]
  (let [op-fn (case op
               :sum row-sum
               :avg row-avg
               :max row-max)]
    (mapv op-fn matrix)))