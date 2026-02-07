(ns olympus-web.graphs.common
  "Common graph utilities for ReactFlow integration.

   Provides:
   - Dagre layout calculation
   - ReactFlow component wrappers
   - Node/edge conversion helpers"
  (:require [olympus-web.config :as config]
            ["@xyflow/react" :as xyflow]
            ["dagre" :as dagre-lib]))

;; =============================================================================
;; ReactFlow Interop (from npm @xyflow/react v12)
;; =============================================================================

(def ReactFlow (.-ReactFlow xyflow))
(def Controls (.-Controls xyflow))
(def MiniMap (.-MiniMap xyflow))
(def Background (.-Background xyflow))

;; =============================================================================
;; Dagre Layout (from npm dagre)
;; =============================================================================

(defn create-dagre-graph
  "Create a new dagre graph with default settings."
  [direction]
  (let [Graph (.. dagre-lib -graphlib -Graph)
        g (new Graph)]
    (.setDefaultEdgeLabel g (fn [] #js {}))
    (.setGraph g #js {:rankdir (or direction "TB")
                      :nodesep (:nodesep config/dagre-config)
                      :ranksep (:ranksep config/dagre-config)
                      :marginx (:marginx config/dagre-config)
                      :marginy (:marginy config/dagre-config)})
    g))

(defn apply-dagre-layout
  "Apply dagre layout to nodes and edges.
   Returns updated nodes with positions."
  [nodes edges direction]
  (let [g (create-dagre-graph direction)]
    ;; Add nodes to graph
    (doseq [{:keys [id width height]} nodes]
      (.setNode g id #js {:width (or width 180)
                          :height (or height 80)}))
    ;; Add edges to graph
    (doseq [{:keys [source target]} edges]
      (.setEdge g source target))
    ;; Calculate layout
    (.layout dagre-lib g)
    ;; Extract positions
    (mapv (fn [{:keys [id] :as node}]
            (let [dagre-node (.node g id)]
              (assoc node
                     :position {:x (- (.-x dagre-node) (/ (.-width dagre-node) 2))
                                :y (- (.-y dagre-node) (/ (.-height dagre-node) 2))})))
          nodes)))

;; =============================================================================
;; Node Helpers
;; =============================================================================

(defn make-node
  "Create a ReactFlow node map."
  [{:keys [id type data position width height]}]
  {:id id
   :type (or type "default")
   :data (clj->js data)
   :position (or position {:x 0 :y 0})
   :style #js {:width (or width 180)
               :height (or height 80)}})

(defn make-edge
  "Create a ReactFlow edge map."
  [{:keys [id source target type animated label style]}]
  {:id (or id (str source "->" target))
   :source source
   :target target
   :type (or type "smoothstep")
   :animated (boolean animated)
   :label label
   :style (clj->js (or style {}))})

;; =============================================================================
;; Conversion
;; =============================================================================

(defn nodes->js
  "Convert Clojure nodes to JS array for ReactFlow."
  [nodes]
  (clj->js nodes))

(defn edges->js
  "Convert Clojure edges to JS array for ReactFlow."
  [edges]
  (clj->js edges))
