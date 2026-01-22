(ns hive-mcp.algorithms.graph.domain)

;; VALUE OBJECTS (records)
(defrecord Node [id data])
(defrecord Edge [from to weight])

;; GRAPH PROTOCOL (DIP - Dependency Inversion)
(defprotocol IGraph
  "Protocol for graph operations."
  (add-node [this node-id node] "Add a node to the graph, return new graph")
  (remove-node [this node-id] "Remove a node from the graph, return new graph")
  (add-edge [this from-id to-id weight] "Add an edge, return new graph")
  (remove-edge [this from-id to-id] "Remove an edge, return new graph")
  (get-node [this node-id] "Get node by id")
  (get-neighbors [this node-id] "Get all neighbors as map of node-id -> weight")
  (get-edges [this node-id] "Get all edges from a node as vector")
  (node-count [this] "Get total number of nodes")
  (edge-count [this] "Get total number of edges"))

;; PURE HELPER FUNCTIONS
(defn make-node
  "Factory function for creating a Node.
  
  Arity 1: Creates a node with the given id and no data.
  Arity 2: Creates a node with the given id and data."
  ([id] (->Node id nil))
  ([id data] (->Node id data)))

(defn make-edge
  "Factory function for creating an Edge.
  
  Arity 2: Creates an edge with the given from and to nodes, and default weight 1.
  Arity 3: Creates an edge with the given from, to nodes, and weight."
  ([from to] (->Edge from to 1))
  ([from to weight] (->Edge from to weight)))

(defn node-id
  "Extracts the id from a node. Works with Node record or raw id."
  [node]
  (if (instance? Node node)
    (:id node)
    node))