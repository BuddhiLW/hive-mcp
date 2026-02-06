(ns olympus-web.graphs.wave
  "Wave/Task graph visualization using ReactFlow.
   
   Shows:
   - Waves as container nodes (orange)
   - Tasks as child nodes with status colors
   - Task flow from pending → running → completed/failed"
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [olympus-web.graphs.common :as common]
            [olympus-web.config :as config]))

;; =============================================================================
;; Custom Node Components
;; =============================================================================

(defn wave-node-component
  "Custom ReactFlow node for waves."
  [props]
  (let [data (.-data props)
        wave-id (.-waveId data)
        status (.-status data)
        task-count (.-taskCount data)]
    (r/as-element
     [:div.wave-node
      [:div {:style {:font-weight "600" :margin-bottom "0.25rem"}}
       "🌊 Wave"]
      [:div {:style {:font-size "0.7rem" :font-family "monospace"}}
       (if (> (count wave-id) 15)
         (str (subs wave-id 0 15) "...")
         wave-id)]
      [:div {:style {:font-size "0.75rem" :color "var(--text-secondary)"}}
       (str task-count " tasks • " status)]])))

(defn task-node-component
  "Custom ReactFlow node for tasks."
  [props]
  (let [data (.-data props)
        file (.-file data)
        status (keyword (.-status data))]
    (r/as-element
     [:div.task-node {:class (name status)}
      [:div {:style {:font-size "0.7rem" :font-family "monospace"}}
       (let [filename (last (clojure.string/split file #"/"))]
         (if (> (count filename) 18)
           (str (subs filename 0 18) "...")
           filename))]
      [:div {:style {:font-size "0.65rem" :margin-top "0.125rem"}}
       (case status
         :pending "⏳"
         :running "⚡"
         :completed "✓"
         :failed "✗"
         "")]])))

;; =============================================================================
;; Graph Data Conversion
;; =============================================================================

(defn waves->nodes
  "Convert wave data to ReactFlow nodes."
  [waves]
  (let [wave-dims (get config/node-dimensions :wave)
        task-dims (get config/node-dimensions :task)]
    (concat
     ;; Wave nodes
     (mapv (fn [{:keys [id status tasks]}]
             {:id id
              :type "waveNode"
              :data {:waveId id
                     :status (name status)
                     :taskCount (count tasks)}
              :width (:width wave-dims)
              :height (:height wave-dims)})
           waves)
     ;; Task nodes
     (mapcat (fn [{:keys [id tasks]}]
               (map-indexed
                (fn [idx {:keys [file status]}]
                  {:id (str id "-task-" idx)
                   :type "taskNode"
                   :data {:file file
                          :status (name status)
                          :waveId id
                          :taskIdx idx}
                   :width (:width task-dims)
                   :height (:height task-dims)})
                tasks))
             waves))))

(defn waves->edges
  "Create edges from waves to their tasks."
  [waves]
  (mapcat (fn [{:keys [id tasks]}]
            (map-indexed
             (fn [idx _task]
               {:id (str id "->task-" idx)
                :source id
                :target (str id "-task-" idx)
                :type "smoothstep"
                :animated false
                :style {:stroke (get-in config/colors [:wave :container])}})
             tasks))
          waves))

;; =============================================================================
;; Main Component
;; =============================================================================

(defn wave-graph
  "Main wave graph component."
  []
  (let [waves @(rf/subscribe [:waves/all])
        layout @(rf/subscribe [:ui/layout])]
    (r/with-let [node-types #js {:waveNode wave-node-component
                                 :taskNode task-node-component}]
      (if (empty? waves)
        ;; Empty state
        [:div {:style {:display "flex"
                       :align-items "center"
                       :justify-content "center"
                       :height "100%"
                       :color "var(--text-secondary)"}}
         [:div {:style {:text-align "center"}}
          [:div {:style {:font-size "3rem" :margin-bottom "1rem"}} "🌊"]
          [:div "No active waves"]
          [:div {:style {:font-size "0.875rem" :margin-top "0.5rem"}}
           "Dispatch a wave to see tasks here"]]]
        ;; Graph view
        (let [raw-nodes (waves->nodes waves)
              raw-edges (waves->edges waves)
              laid-out-nodes (common/apply-dagre-layout raw-nodes raw-edges "LR")]
          [:> common/ReactFlow
           {:nodes (common/nodes->js laid-out-nodes)
            :edges (common/edges->js raw-edges)
            :nodeTypes node-types
            :onNodeClick (fn [_ node]
                           (let [node-id (.-id node)
                                 data (.-data node)]
                             (if (.-waveId data)
                               ;; It's a task node
                               (rf/dispatch [:waves/select-task (.-waveId data) (.-taskIdx data)])
                               ;; It's a wave node
                               (rf/dispatch [:waves/select node-id]))))
            :fitView true
            :fitViewOptions #js {:padding 0.2}}
           [:> common/Controls]
           [:> common/MiniMap {:style #js {:backgroundColor "var(--bg-secondary)"}}]
           [:> common/Background {:color "var(--border-color)"
                                  :gap 20
                                  :size 1}]])))))
