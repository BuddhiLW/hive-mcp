(ns olympus-web.views.detail
  "Detail panel component for selected items."
  (:require [re-frame.core :as rf]
            [olympus-web.views.project-tree :as project-tree]))

(defn agent-detail
  "Detail view for a selected agent."
  [agent-id]
  (let [agent @(rf/subscribe [:agents/by-id agent-id])
        messages @(rf/subscribe [:hivemind/messages-for-agent agent-id])]
    (when agent
      [:div.detail-panel-content
       [:div.detail-field
        [:div.detail-label "ID"]
        [:div.detail-value {:style {:font-family "monospace" :font-size "0.75rem"}}
         (:id agent)]]
       [:div.detail-field
        [:div.detail-label "Type"]
        [:div.detail-value (name (:type agent))]]
       [:div.detail-field
        [:div.detail-label "Status"]
        [:div.detail-value (name (:status agent))]]
       (when (:task agent)
         [:div.detail-field
          [:div.detail-label "Current Task"]
          [:div.detail-value (:task agent)]])
       (when (:parent-id agent)
         [:div.detail-field
          [:div.detail-label "Parent"]
          [:div.detail-value {:style {:font-family "monospace" :font-size "0.75rem"}}
           (:parent-id agent)]])
       (when (:project-id agent)
         [:div.detail-field
          [:div.detail-label "Project"]
          [:div.detail-value (:project-id agent)]])
       (when (seq messages)
         [:div.detail-field
          [:div.detail-label "Messages"]
          [:div.messages-list
           (for [{:keys [event-type message timestamp]} (take-last 5 messages)]
             ^{:key timestamp}
             [:div.message-item
              [:span.message-type {:class (name event-type)} (name event-type)]
              [:span {:style {:margin-left "0.5rem"}} message]])]])])))

(defn wave-detail
  "Detail view for a selected wave."
  [wave-id]
  (let [wave @(rf/subscribe [:waves/by-id wave-id])]
    (when wave
      [:div.detail-panel-content
       [:div.detail-field
        [:div.detail-label "Wave ID"]
        [:div.detail-value {:style {:font-family "monospace"}} (:id wave)]]
       [:div.detail-field
        [:div.detail-label "Status"]
        [:div.detail-value (name (:status wave))]]
       [:div.detail-field
        [:div.detail-label "Tasks"]
        [:div
         (for [{:keys [file status]} (:tasks wave)]
           ^{:key file}
           [:div {:style {:display "flex" :align-items "center" :gap "0.5rem" :margin-bottom "0.25rem"}}
            [:span.task-node {:class (name status) :style {:padding "0.25rem 0.5rem"}}
             (name status)]
            [:span {:style {:font-size "0.75rem" :font-family "monospace"}} file]])]]])))

(defn kg-detail
  "Detail view for a selected KG entry."
  [entry-id]
  (let [entry @(rf/subscribe [:kg/by-id entry-id])]
    (when entry
      [:div.detail-panel-content
       [:div.detail-field
        [:div.detail-label "ID"]
        [:div.detail-value {:style {:font-family "monospace" :font-size "0.75rem"}}
         (:id entry)]]
       [:div.detail-field
        [:div.detail-label "Type"]
        [:div.detail-value (name (:type entry))]]
       [:div.detail-field
        [:div.detail-label "Duration"]
        [:div.detail-value (name (:duration entry))]]
       (when (seq (:tags entry))
         [:div.detail-field
          [:div.detail-label "Tags"]
          [:div.detail-value
           (for [tag (:tags entry)]
             ^{:key tag}
             [:span {:style {:display "inline-block"
                             :background "var(--bg-tertiary)"
                             :padding "0.125rem 0.5rem"
                             :border-radius "0.25rem"
                             :margin-right "0.25rem"
                             :margin-bottom "0.25rem"
                             :font-size "0.75rem"}}
              tag])]])
       [:div.detail-field
        [:div.detail-label "Content"]
        [:div.detail-value {:style {:max-height "200px"
                                    :overflow-y "auto"
                                    :font-size "0.8rem"
                                    :white-space "pre-wrap"}}
         (:content entry)]]
       (when (seq (:edges entry))
         [:div.detail-field
          [:div.detail-label "Edges"]
          [:div
           (for [{:keys [to relation]} (:edges entry)]
             ^{:key (str to "-" relation)}
             [:div {:style {:font-size "0.75rem" :margin-bottom "0.25rem"}}
              [:span {:style {:color "var(--text-secondary)"}} (str "→ " (name relation) " → ")]
              [:span {:style {:font-family "monospace"}} (subs to 0 20) "..."]])]])])))

(defn detail-panel
  "Main detail panel that shows based on selection."
  []
  (let [{:keys [id type]} @(rf/subscribe [:ui/selected])]
    (when id
      [:div.detail-panel
       [:div.detail-panel-header
        [:span.detail-panel-title
         (case type
           :agent "Agent Details"
           :wave "Wave Details"
           :task "Task Details"
           :kg "Memory Entry"
           :project "Project Details"
           "Details")]
        [:button.detail-panel-close
         {:on-click #(rf/dispatch [:ui/deselect-node])}
         "×"]]
       (case type
         :agent [agent-detail id]
         :wave [wave-detail id]
         :kg [kg-detail id]
         :project [project-tree/project-detail id]
         nil)])))
