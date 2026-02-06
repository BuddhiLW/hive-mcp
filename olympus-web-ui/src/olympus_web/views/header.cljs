(ns olympus-web.views.header
  "Header component with title and connection status."
  (:require [re-frame.core :as rf]))

(defn connection-indicator
  "Shows WebSocket connection status."
  []
  (let [status @(rf/subscribe [:connection/status])
        error @(rf/subscribe [:connection/error])]
    [:div.connection-status
     [:span.status-dot {:class (name status)}]
     [:span (case status
              :connected "Connected"
              :connecting "Connecting..."
              :disconnected (if error
                              (str "Disconnected: " error)
                              "Disconnected"))]]))

(defn header
  "App header with logo, title, and status."
  []
  [:header.header
   [:div.header-title
    [:span.logo "🏛️"]
    [:span "Olympus"]
    [:span {:style {:font-weight "normal" :opacity 0.7 :font-size "0.875rem"}}
     " — Hive Swarm Dashboard"]]
   [connection-indicator]])
