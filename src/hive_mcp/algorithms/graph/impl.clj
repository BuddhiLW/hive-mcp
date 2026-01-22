(ns hive-mcp.algorithms.graph.impl
  "Graph algorithm implementations using adjacency list representation."
  (:require [hive-mcp.algorithms.graph.domain :as domain])
  (:import [clojure.lang PersistentQueue]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjacency List Graph Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adjacency list graph representation.
;; Fields:
;; - nodes: map of node-id -> Node
;; - adj-map: map of node-id -> vector of {:to node-id :weight number}
(defrecord AdjacencyListGraph [nodes adj-map])

(extend-type AdjacencyListGraph
  domain/IGraph

  (add-node [graph node-id node]
    "Add a node to the graph."
    (if (contains? (:nodes graph) node-id)
      graph
      (->AdjacencyListGraph
       (assoc (:nodes graph) node-id node)
       (:adj-map graph))))

  (remove-node [graph node-id]
    "Remove a node from the graph."
    (let [new-nodes (dissoc (:nodes graph) node-id)
          ;; Remove all edges to/from this node
          new-adj (-> (:adjacency-map graph)
                      (dissoc node-id)
                      (->> (map (fn [[from edges]]
                                  [from (vec (remove #(= (:to %) node-id) edges))])))
                      (into {}))]
      (->AdjacencyListGraph new-nodes new-adj)))

  (add-edge [graph from-id to-id weight]
    "Add an edge from from-id to to-id with given weight."
    (if (and (contains? (:nodes graph) from-id)
             (contains? (:nodes graph) to-id))
      (let [edge {:to to-id :weight weight}
            current-edges (get (:adj-map graph) from-id [])
            ;; Remove existing edge if present, then add new one
            new-edges (vec (cons edge (remove #(= (:to %) to-id) current-edges)))]
        (->AdjacencyListGraph
         (:nodes graph)
         (assoc (:adj-map graph) from-id new-edges)))
      graph))

  (remove-edge [graph from-id to-id]
    "Remove edge from from-id to to-id."
    (if (contains? (:nodes graph) from-id)
      (let [current-edges (get (:adj-map graph) from-id [])
            new-edges (vec (remove #(= (:to %) to-id) current-edges))]
        (if (seq new-edges)
          (->AdjacencyListGraph
           (:nodes graph)
           (assoc (:adj-map graph) from-id new-edges))
          (->AdjacencyListGraph
           (:nodes graph)
           (dissoc (:adj-map graph) from-id))))
      graph))

  (get-node [graph node-id]
    "Get node by id."
    (get (:nodes graph) node-id))

  (get-neighbors [graph node-id]
    "Get all neighbors of a node as map of node-id -> weight."
    (if (contains? (:nodes graph) node-id)
      (let [edges (get (:adj-map graph) node-id [])]
        (into {} (map (fn [{:keys [to weight]}] [to weight]) edges)))
      {}))

  (get-edges [graph node-id]
    "Get all edges from a node as vector of {:to node-id :weight number}."
    (if (contains? (:nodes graph) node-id)
      (get (:adj-map graph) node-id [])
      []))

  (node-count [graph]
    "Get total number of nodes in graph."
    (count (:nodes graph)))

  (edge-count [graph]
    "Get total number of edges in graph."
    (reduce + (map count (vals (:adj-map graph))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Factory Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn empty-graph
  "Create an empty graph."
  []
  (->AdjacencyListGraph {} {}))

(defn graph-from-edges
  "Create a graph from a collection of edges.

  Edges should be a collection of maps with :from, :to, :weight keys.
  Nodes will be created automatically with empty data."
  [edges]
  (let [graph (empty-graph)
        ;; Collect all unique node ids
        node-ids (into #{} (concat (map :from edges) (map :to edges)))
        ;; Add all nodes
        graph-with-nodes (reduce (fn [g nid]
                                   (domain/add-node g nid (domain/->Node nid {})))
                                 graph
                                 node-ids)
        ;; Add all edges
        graph-with-edges (reduce (fn [g {:keys [from to weight]}]
                                   (domain/add-edge g from to (or weight 1)))
                                 graph-with-nodes
                                 edges)]
    graph-with-edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BFS Traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bfs
  "Breadth-first traversal from start node. Returns vector of node-ids in visit order."
  [graph start-id]
  (let [visited (atom #{})
        queue (atom PersistentQueue/EMPTY)
        result (atom [])]

    (when (contains? (:nodes graph) start-id)
      (swap! queue conj start-id)
      (swap! visited conj start-id)

      (while (not (empty? @queue))
        (let [current (peek @queue)]
          (swap! queue pop)
          (swap! result conj current)

          (let [neighbors (domain/get-neighbors graph current)]
            (doseq [[neighbor-id _] neighbors]
              (when (not (contains? @visited neighbor-id))
                (swap! queue conj neighbor-id)
                (swap! visited conj neighbor-id)))))))

    @result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DFS Traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dfs
  "Depth-first traversal from start node. Returns vector of node-ids in visit order."
  [graph start-id]
  (let [visited (atom #{})
        result (atom [])]

    (when (contains? (:nodes graph) start-id)
      (letfn [(visit [node-id]
                (when (not (contains? @visited node-id))
                  (swap! visited conj node-id)
                  (swap! result conj node-id)

                  (let [neighbors (domain/get-neighbors graph node-id)]
                    (doseq [[neighbor-id _] neighbors]
                      (visit neighbor-id)))))]

        (visit start-id)))

    @result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dijkstra Shortest Path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dijkstra
  "Find shortest path from start to end. Returns {:distance num :path [node-ids]} or nil if no path."
  [graph start-id end-id]
  (when (and (contains? (:nodes graph) start-id)
             (contains? (:nodes graph) end-id))

    (loop [distances {start-id 0}
           previous {}
           unvisited (into (sorted-set) (keys (:nodes graph)))
           current start-id]

      (cond
        ;; Found the end node
        (= current end-id)
        (let [path (loop [node end-id
                          acc []]
                     (if (contains? previous node)
                       (recur (get previous node) (conj acc node))
                       (conj acc node)))]
          {:distance (get distances end-id)
           :path (vec (reverse path))})

        ;; No path exists
        (empty? unvisited)
        nil

        ;; Continue processing
        :else
        (let [current-dist (get distances current Long/MAX_VALUE)
              neighbors (domain/get-neighbors graph current)

              ;; Update distances for neighbors
              [new-distances new-previous]
              (reduce (fn [[dists prev] [neighbor-id weight]]
                        (let [alt (+ current-dist weight)
                              current-neighbor-dist (get dists neighbor-id Long/MAX_VALUE)]
                          (if (< alt current-neighbor-dist)
                            [(assoc dists neighbor-id alt) (assoc prev neighbor-id current)]
                            [dists prev])))
                      [distances previous]
                      neighbors)

              ;; Remove current from unvisited and get next node with smallest distance
              remaining-unvisited (disj unvisited current)
              next-node (when (seq remaining-unvisited)
                          (apply min-key #(get new-distances % Long/MAX_VALUE) remaining-unvisited))]

          (recur new-distances new-previous remaining-unvisited next-node))))))