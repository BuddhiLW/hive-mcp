(ns olympus-web.graphs.agent
  "Agent hierarchy graph visualization using ReactFlow.
   
   Shows:
   - Coordinator (purple) at top
   - Lings (blue) as children of coordinator
   - Drones (green) as children of lings"
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [olympus-web.graphs.common :as common]
            [olympus-web.config :as config]))

;; =============================================================================
;; Custom Node Components
;; =============================================================================

(defn agent-node-component
  "Custom ReactFlow node for agents."
  [props]
  (let [data (.-data props)
        agent-type (keyword (.-agentType data))
        status (.-status data)
        agent-id (.-agentId data)
        task (.-task data)]
    (r/as-element
     [:div.agent-node {:class (name agent-type)}
      [:div.node-header
       [:span.node-type (name agent-type)]]
      [:div.node-id (if (> (count agent-id) 20)
                      (str (subs agent-id 0 20) "...")
                      agent-id)]
      [:div.node-status status]
      (when task
        [:div.node-task task])])))

;; =============================================================================
;; Graph Data Conversion
;; =============================================================================

(defn agents->nodes
  "Convert agent data to ReactFlow nodes."
  [agents]
  (mapv (fn [{:keys [id type status task]}]
          (let [dims (get config/node-dimensions :agent)]
            {:id id
             :type "agentNode"
             :data {:agentType (name type)
                    :agentId id
                    :status (name status)
                    :task task}
             :width (:width dims)
             :height (:height dims)}))
        agents))

(defn agents->edges
  "Create edges based on parent-child relationships."
  [agents]
  (keep (fn [{:keys [id parent-id]}]
          (when parent-id
            {:id (str parent-id "->" id)
             :source parent-id
             :target id
             :type "smoothstep"
             :animated false
             :style {:stroke (get-in config/colors [:agent :ling])}}))
        agents))

;; =============================================================================
;; Main Component
;; =============================================================================

(defn agent-graph
  "Main agent graph component."
  []
  (let [agents @(rf/subscribe [:agents/filtered])
        layout @(rf/subscribe [:ui/layout])]
    (r/with-let [;; Create node types map
                 node-types #js {:agentNode agent-node-component}]
      (if (empty? agents)
        ;; Empty state
        [:div {:style {:display "flex"
                       :align-items "center"
                       :justify-content "center"
                       :height "100%"
                       :color "var(--text-secondary)"}}
         [:div {:style {:text-align "center"}}
          [:div {:style {:font-size "3rem" :margin-bottom "1rem"}} "🤖"]
          [:div "No agents connected"]
          [:div {:style {:font-size "0.875rem" :margin-top "0.5rem"}}
           "Spawn lings to see them here"]]]
        ;; Graph view
        (let [raw-nodes (agents->nodes agents)
              raw-edges (agents->edges agents)
              ;; Apply dagre layout
              laid-out-nodes (common/apply-dagre-layout raw-nodes raw-edges (name layout))]
          [:> common/ReactFlow
           {:nodes (common/nodes->js laid-out-nodes)
            :edges (common/edges->js raw-edges)
            :nodeTypes node-types
            :onNodeClick (fn [_ node]
                           (rf/dispatch [:agents/select (.-id node)]))
            :fitView true
            :fitViewOptions #js {:padding 0.2}}
           [:> common/Controls]
           [:> common/MiniMap {:style #js {:backgroundColor "var(--bg-secondary)"}}]
           [:> common/Background {:color "var(--border-color)"
                                  :gap 20
                                  :size 1}]])))))
