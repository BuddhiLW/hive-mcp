(ns olympus-web.views.sidebar
  "Sidebar component with view switcher, stats, and legend."
  (:require [re-frame.core :as rf]))

(defn view-switcher
  "Buttons to switch between graph views."
  []
  (let [current-view @(rf/subscribe [:ui/view])]
    [:div.sidebar-section
     [:div.sidebar-section-title "Views"]
     [:div.view-switcher
      [:button.view-btn
       {:class (when (= current-view :agent-graph) "active")
        :on-click #(rf/dispatch [:ui/set-view :agent-graph])}
       [:span.icon "🤖"]
       [:span "Agents"]]
      [:button.view-btn
       {:class (when (= current-view :wave-graph) "active")
        :on-click #(rf/dispatch [:ui/set-view :wave-graph])}
       [:span.icon "🌊"]
       [:span "Waves"]]
      [:button.view-btn
       {:class (when (= current-view :kg-graph) "active")
        :on-click #(rf/dispatch [:ui/set-view :kg-graph])}
       [:span.icon "🧠"]
       [:span "Knowledge Graph"]]
      [:button.view-btn
       {:class (when (= current-view :project-tree) "active")
        :on-click #(rf/dispatch [:ui/set-view :project-tree])}
       [:span.icon "🌳"]
       [:span "Projects"]]
      [:button.view-btn
       {:class (when (= current-view :terminal-view) "active")
        :on-click #(rf/dispatch [:ui/set-view :terminal-view])}
       [:span.icon "💻"]
       [:span "Terminal"]]]]))

(defn stats-panel
  "Quick stats overview."
  []
  (let [agent-count @(rf/subscribe [:agents/count])
        ling-count @(rf/subscribe [:agents/count-by-type :ling])
        drone-count @(rf/subscribe [:agents/count-by-type :drone])
        wave-count @(rf/subscribe [:waves/count])
        project-count @(rf/subscribe [:projects/count])]
    [:div.sidebar-section
     [:div.sidebar-section-title "Stats"]
     [:div.stats-grid
      [:div.stat-card
       [:div.stat-value agent-count]
       [:div.stat-label "Agents"]]
      [:div.stat-card
       [:div.stat-value ling-count]
       [:div.stat-label "Lings"]]
      [:div.stat-card
       [:div.stat-value drone-count]
       [:div.stat-label "Drones"]]
      [:div.stat-card
       [:div.stat-value project-count]
       [:div.stat-label "Projects"]]]]))

(defn legend-agents
  "Legend for agent graph colors."
  []
  [:div.legend
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#9333ea"}}]
    [:span "Coordinator"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#2563eb"}}]
    [:span "Ling"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#16a34a"}}]
    [:span "Drone"]]])

(defn legend-waves
  "Legend for wave graph colors."
  []
  [:div.legend
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#f97316"}}]
    [:span "Wave"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#6b7280"}}]
    [:span "Pending"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#eab308"}}]
    [:span "Running"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#16a34a"}}]
    [:span "Completed"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#dc2626"}}]
    [:span "Failed"]]])

(defn legend-kg
  "Legend for KG graph colors."
  []
  [:div.legend
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#06b6d4"}}]
    [:span "Note"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#ec4899"}}]
    [:span "Snippet"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#eab308"}}]
    [:span "Convention"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#9333ea"}}]
    [:span "Decision"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "#dc2626"}}]
    [:span "Axiom"]]])

(defn legend-projects
  "Legend for project tree types."
  []
  [:div.legend
   [:div.legend-item
    [:span {:style {:width "12px" :text-align "center"}} "📁"]
    [:span "Workspace"]]
   [:div.legend-item
    [:span {:style {:width "12px" :text-align "center"}} "⚙️"]
    [:span "Service"]]
   [:div.legend-item
    [:span {:style {:width "12px" :text-align "center"}} "🖥️"]
    [:span "Frontend"]]
   [:div.legend-item
    [:span {:style {:width "12px" :text-align "center"}} "📚"]
    [:span "Library"]]
   [:div.legend-item
    [:span {:style {:width "12px" :text-align "center"}} "📦"]
    [:span "Generic"]]])

(defn legend-terminal
  "Legend for terminal view indicators."
  []
  [:div.legend
   [:div.legend-item
    [:div.legend-color {:style {:background-color "var(--status-connected)"
                                :border-radius "50%"
                                :width "8px" :height "8px"}}]
    [:span "Subscribed"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "var(--status-connecting)"
                                :border-radius "50%"
                                :width "8px" :height "8px"}}]
    [:span "Connecting"]]
   [:div.legend-item
    [:div.legend-color {:style {:background-color "var(--ling-blue)"}}]
    [:span "Active Tab"]]])

(defn legend-panel
  "Shows legend based on current view."
  []
  (let [current-view @(rf/subscribe [:ui/view])]
    [:div.sidebar-section
     [:div.sidebar-section-title "Legend"]
     (case current-view
       :agent-graph [legend-agents]
       :wave-graph [legend-waves]
       :kg-graph [legend-kg]
       :project-tree [legend-projects]
       :terminal-view [legend-terminal]
       [legend-agents])]))

(defn recent-messages
  "Shows recent hivemind messages."
  []
  (let [messages @(rf/subscribe [:hivemind/recent-messages 5])]
    [:div.sidebar-section
     [:div.sidebar-section-title "Recent Activity"]
     (if (seq messages)
       [:div.messages-list
        (for [{:keys [agent-id event-type message timestamp]} (reverse messages)]
          ^{:key (str agent-id "-" timestamp)}
          [:div.message-item
           [:div
            [:span.message-time
             (-> (js/Date. timestamp) .toLocaleTimeString)]
            [:span.message-type {:class (name event-type)}
             (name event-type)]]
           [:div {:style {:margin-top "0.25rem"}}
            (or message (str "Agent: " (subs agent-id 0 20) "..."))]])]
       [:div {:style {:color "var(--text-secondary)" :font-size "0.875rem"}}
        "No recent activity"])]))

(defn sidebar
  "Main sidebar component."
  []
  [:aside.sidebar
   [view-switcher]
   [stats-panel]
   [legend-panel]
   [recent-messages]])
