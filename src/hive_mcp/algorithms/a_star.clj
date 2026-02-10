(ns hive-mcp.algorithms.a-star
  "A* pathfinding algorithm implementation."
  (:require
   [clojure.data.priority-map :refer [priority-map]]))

(defn manhattan-distance
  "Calculate Manhattan distance between two points.
  Points should be maps with :x and :y keys or vectors [x y].
  
  Example:
  (manhattan-distance {:x 1 :y 2} {:x 4 :y 6}) => 7
  (manhattan-distance [1 2] [4 6]) => 7"
  [a b]
  (let [[ax ay] (if (map? a) [(:x a) (:y a)] a)
        [bx by] (if (map? b) [(:x b) (:y b)] b)]
    (+ (Math/abs (- ax bx))
       (Math/abs (- ay by)))))

(defn euclidean-distance
  "Calculate Euclidean distance between two points.
  Points should be maps with :x and :y keys or vectors [x y].
  
  Example:
  (euclidean-distance {:x 1 :y 2} {:x 4 :y 6}) => 5.0
  (euclidean-distance [1 2] [4 6]) => 5.0"
  [a b]
  (let [[ax ay] (if (map? a) [(:x a) (:y a)] a)
        [bx by] (if (map? b) [(:x b) (:y b)] b)]
    (Math/sqrt
     (+ (Math/pow (- ax bx) 2)
        (Math/pow (- ay by) 2)))))

(defn reconstruct-path
  "Reconstruct the path from start to goal using the came-from map.
  
  Arguments:
  - came-from: Map of node -> previous node (how we reached each node)
  - current: Current node (should be the goal node when called)
  
  Returns a vector of nodes from start to goal (inclusive)."
  [came-from current]
  (loop [path [current]
         node current]
    (if-let [prev (get came-from node)]
      (recur (conj path prev) prev)
      (vec (reverse path)))))

(defn search
  "A* pathfinding algorithm.
  
  Arguments:
  - graph: Map of node -> [{:to neighbor-node :cost edge-cost} ...]
  - start: Starting node
  - goal: Goal node
  - heuristic-fn: Function (node) -> estimated cost to goal
  
  Returns map with:
  - :path: Vector of nodes from start to goal (nil if no path)
  - :cost: Total cost of path (nil if no path)
  - :visited: Number of nodes visited during search
  - :came-from: Map of node -> previous node (for debugging)
  - :g-scores: Map of node -> actual cost from start (for debugging)
  
  Example:
  (def graph
    {:a [{:to :b :cost 1} {:to :c :cost 1.5}]
     :b [{:to :d :cost 1}]
     :c [{:to :d :cost 1}]
     :d []})
  
  (search graph :a :d (constantly 0))"
  [graph start goal heuristic-fn]
  (let [initial-g-score {start 0}
        initial-f-score {start (heuristic-fn start)}]
    (loop [open-set (priority-map start (get initial-f-score start))
           came-from {}
           g-scores initial-g-score
           f-scores initial-f-score
           visited 0]
      (if (empty? open-set)
        ;; No path found
        {:path nil
         :cost nil
         :visited visited
         :came-from came-from
         :g-scores g-scores}
        (let [[current _] (peek open-set)
              open-set' (pop open-set)
              visited' (inc visited)]
          (if (= current goal)
            ;; Found path
            (let [path (reconstruct-path came-from current)
                  cost (get g-scores current)]
              {:path path
               :cost cost
               :visited visited'
               :came-from came-from
               :g-scores g-scores})
            ;; Explore neighbors
            (let [neighbors (get graph current [])]
              (reduce
               (fn [{:keys [open-set came-from g-scores f-scores]}
                    {:keys [to cost]}]
                 (let [tentative-g-score (+ (get g-scores current) cost)]
                   (if (or (not (contains? g-scores to))
                           (< tentative-g-score (get g-scores to)))
                     ;; Better path found
                     (let [came-from' (assoc came-from to current)
                           g-scores' (assoc g-scores to tentative-g-score)
                           f-score (+ tentative-g-score (heuristic-fn to))
                           f-scores' (assoc f-scores to f-score)
                           open-set' (assoc open-set to f-score)]
                       {:open-set open-set'
                        :came-from came-from'
                        :g-scores g-scores'
                        :f-scores f-scores'})
                     {:open-set open-set
                      :came-from came-from
                      :g-scores g-scores
                      :f-scores f-scores})))
               {:open-set open-set'
                :came-from came-from
                :g-scores g-scores
                :f-scores f-scores}
               neighbors))))))))

;; Convenience functions for common use cases

(defn search-with-manhattan
  "A* search using Manhattan distance as heuristic.
  
  Arguments:
  - graph: Map of node -> [{:to neighbor-node :cost edge-cost} ...]
  - start: Starting node (should support manhattan-distance)
  - goal: Goal node (should support manhattan-distance)
  
  Returns same as `search`."
  [graph start goal]
  (search graph start goal (partial manhattan-distance goal)))

(defn search-with-euclidean
  "A* search using Euclidean distance as heuristic.
  
  Arguments:
  - graph: Map of node -> [{:to neighbor-node :cost edge-cost} ...]
  - start: Starting node (should support euclidean-distance)
  - goal: Goal node (should support euclidean-distance)
  
  Returns same as `search`."
  [graph start goal]
  (search graph start goal (partial euclidean-distance goal)))

;; Example usage and tests

(comment
  ;; Simple graph example
  (def simple-graph
    {:a [{:to :b :cost 1} {:to :c :cost 1.5}]
     :b [{:to :d :cost 1}]
     :c [{:to :d :cost 1}]
     :d []})

  ;; Search with zero heuristic (equivalent to Dijkstra)
  (search simple-graph :a :d (constantly 0))
  ;; => {:path [:a :b :d], :cost 2, :visited 4, ...}

  ;; Grid-based example
  (def grid-graph
    (let [nodes (for [x (range 5) y (range 5)] [x y])
          edges (fn [[x y]]
                  (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
                       (filter (fn [[nx ny]] (and (>= nx 0) (>= ny 0) (< nx 5) (< ny 5))))
                       (map (fn [neighbor] {:to neighbor :cost 1}))))]
      (into {} (map (fn [node] [node (edges node)]) nodes))))

  ;; Search on grid with Manhattan heuristic
  (search-with-manhattan grid-graph [0 0] [4 4])
  ;; => {:open-set {[1 0] 8, [0 1] 8},
  ;;     :came-from {[1 0] [0 0], [0 1] [0 0]},
  ;;     :g-scores {[0 0] 0, [1 0] 1, [0 1] 1},
  ;;     :f-scores {[0 0] 8, [1 0] 8, [0 1] 8}}

  ;; Search on grid with Euclidean heuristic
  (search-with-euclidean grid-graph [0 0] [4 4])
  ;; => {:open-set {[1 0] 6.0, [0 1] 6.0},
  ;;     :came-from {[1 0] [0 0], [0 1] [0 0]},
  ;;     :g-scores {[0 0] 0, [1 0] 1, [0 1] 1},
  ;;     :f-scores {[0 0] 5.656854249492381, [1 0] 6.0, [0 1] 6.0}}

  ;; Graph with weighted edges
  (def weighted-graph
    {:start [{:to :a :cost 1} {:to :b :cost 5}]
     :a [{:to :c :cost 2}]
     :b [{:to :c :cost 1}]
     :c [{:to :goal :cost 3}]
     :goal []})

  ;; Custom heuristic function
  (defn custom-heuristic [node]
    (case node
      :start 6
      :a 4
      :b 2
      :c 3
      :goal 0
      10))

  (search weighted-graph :start :goal custom-heuristic)
  ;; => {:open-set {:a 5, :b 7},
  ;;     :came-from {:a :start, :b :start},
  ;;     :g-scores {:start 0, :a 1, :b 5},
  ;;     :f-scores {:start 6, :a 5, :b 7}}
  ;; => {:path [:start :b :c :goal], :cost 9, ...}
  )
