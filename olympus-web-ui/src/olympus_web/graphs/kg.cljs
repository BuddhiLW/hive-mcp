(ns olympus-web.graphs.kg
  "Knowledge Graph visualization using ReactFlow.
   
   Shows:
   - Memory entries as colored nodes by type
   - KG edges as connections with relation labels"
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [olympus-web.graphs.common :as common]
            [olympus-web.config :as config]))

;; =============================================================================
;; Custom Node Component
;; =============================================================================

(defn kg-node-component
  "Custom ReactFlow node for KG entries."
  [props]
  (let [data (.-data props)
        entry-type (keyword (.-entryType data))
        entry-id (.-entryId data)
        preview (.-preview data)
        tag-count (.-tagCount data)]
    (r/as-element
     [:div.kg-node {:class (name entry-type)}
      [:div {:style {:display "flex" :align-items "center" :gap "0.25rem" :margin-bottom "0.25rem"}}
       [:span (case entry-type
                :note "📝"
                :snippet "💻"
                :convention "📋"
                :decision "🎯"
                :axiom "⚡"
                "📄")]
       [:span {:style {:font-size "0.625rem" :font-weight "600" :text-transform "uppercase"}}
        (name entry-type)]]
      [:div {:style {:font-size "0.7rem" :font-family "monospace" :opacity 0.8}}
       (if (> (count entry-id) 18)
         (str (subs entry-id 0 18) "...")
         entry-id)]
      (when preview
        [:div {:style {:font-size "0.7rem" :margin-top "0.25rem" :opacity 0.9}}
         (if (> (count preview) 30)
           (str (subs preview 0 30) "...")
           preview)])
      (when (pos? tag-count)
        [:div {:style {:font-size "0.625rem" :color "var(--text-secondary)" :margin-top "0.25rem"}}
         (str tag-count " tags")])])))

;; =============================================================================
;; Graph Data Conversion
;; =============================================================================

(defn entries->nodes
  "Convert KG entries to ReactFlow nodes."
  [entries]
  (let [dims (get config/node-dimensions :kg)]
    (mapv (fn [{:keys [id type content tags]}]
            {:id id
             :type "kgNode"
             :data {:entryId id
                    :entryType (name type)
                    :preview (when content (subs content 0 (min 50 (count content))))
                    :tagCount (count tags)}
             :width (:width dims)
             :height (:height dims)})
          entries)))

(defn entries->edges
  "Create edges from KG entry edges."
  [entries]
  (mapcat (fn [{:keys [id edges]}]
            (map (fn [{:keys [to relation]}]
                   {:id (str id "->" to "-" (name relation))
                    :source id
                    :target to
                    :type "smoothstep"
                    :animated false
                    :label (name relation)
                    :labelStyle #js {:fontSize 10 :fill "var(--text-secondary)"}
                    :style {:stroke (get-in config/colors [:kg (keyword relation)]
                                            "var(--border-color)")}})
                 edges))
          entries))

;; =============================================================================
;; Main Component
;; =============================================================================

(defn kg-graph
  "Main KG graph component."
  []
  (let [entries @(rf/subscribe [:kg/filtered])
        layout @(rf/subscribe [:ui/layout])]
    (r/with-let [node-types #js {:kgNode kg-node-component}]
      (if (empty? entries)
        ;; Empty state
        [:div {:style {:display "flex"
                       :align-items "center"
                       :justify-content "center"
                       :height "100%"
                       :color "var(--text-secondary)"}}
         [:div {:style {:text-align "center"}}
          [:div {:style {:font-size "3rem" :margin-bottom "1rem"}} "🧠"]
          [:div "No memory entries"]
          [:div {:style {:font-size "0.875rem" :margin-top "0.5rem"}}
           "Add memories via memory_add to see them here"]]]
        ;; Graph view
        (let [raw-nodes (entries->nodes entries)
              raw-edges (entries->edges entries)
              ;; Filter edges to only those with existing targets
              node-ids (set (map :id entries))
              valid-edges (filter #(and (node-ids (:source %))
                                        (node-ids (:target %)))
                                  raw-edges)
              laid-out-nodes (common/apply-dagre-layout raw-nodes valid-edges (name layout))]
          [:> common/ReactFlow
           {:nodes (common/nodes->js laid-out-nodes)
            :edges (common/edges->js valid-edges)
            :nodeTypes node-types
            :onNodeClick (fn [_ node]
                           (rf/dispatch [:kg/select (.-id node)]))
            :fitView true
            :fitViewOptions #js {:padding 0.2}}
           [:> common/Controls]
           [:> common/MiniMap {:style #js {:backgroundColor "var(--bg-secondary)"}}]
           [:> common/Background {:color "var(--border-color)"
                                  :gap 20
                                  :size 1}]])))))
