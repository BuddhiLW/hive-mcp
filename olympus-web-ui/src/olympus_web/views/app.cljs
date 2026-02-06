(ns olympus-web.views.app
  "Main app shell view for Olympus Web UI."
  (:require [re-frame.core :as rf]
            [olympus-web.views.sidebar :as sidebar]
            [olympus-web.views.header :as header]
            [olympus-web.views.detail :as detail]
            [olympus-web.views.project-tree :as project-tree]
            [olympus-web.views.terminal :as terminal]
            [olympus-web.graphs.agent :as agent-graph]
            [olympus-web.graphs.wave :as wave-graph]
            [olympus-web.graphs.kg :as kg-graph]))

(defn graph-view
  "Renders the appropriate graph based on current view selection."
  []
  (let [view @(rf/subscribe [:ui/view])]
    [:div.graph-container
     (case view
       :agent-graph [agent-graph/agent-graph]
       :wave-graph [wave-graph/wave-graph]
       :kg-graph [kg-graph/kg-graph]
       :project-tree [project-tree/project-tree-view]
       :terminal-view [terminal/terminal-view]
       ;; Default to agent graph
       [agent-graph/agent-graph])
     ;; Detail panel (overlay)
     [detail/detail-panel]]))

(defn app-shell
  "Main application shell with sidebar, header, and content area."
  []
  (let [sidebar-collapsed? @(rf/subscribe [:ui/sidebar-collapsed?])]
    [:div.app-shell {:class (when sidebar-collapsed? "sidebar-collapsed")}
     [header/header]
     [sidebar/sidebar]
     [:main.main-content
      [graph-view]]]))

(defn app
  "Root app component."
  []
  (let [conn-status @(rf/subscribe [:connection/status])]
    (if (= conn-status :connected)
      [app-shell]
      [:div.loading
       [:div.loading-spinner]
       [:p (case conn-status
             :connecting "Connecting to hive-mcp..."
             :disconnected "Disconnected. Reconnecting..."
             "Loading Olympus...")]])))
