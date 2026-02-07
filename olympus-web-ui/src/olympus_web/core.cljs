(ns olympus-web.core
  "Main entry point for Olympus Web UI.

   Initializes re-frame app and mounts to DOM.
   Uses Reagent 2.0.1 reagent.dom.client API for React 19 createRoot."
  (:require [reagent.dom.client :as rdc]
            [re-frame.core :as rf]
            ;; WebSocket client (side-effects: registers :ws/open, :ws/close effects)
            [olympus-web.ws.client]
            ;; WebSocket state (side-effects: registers enriched connection subs/events)
            [olympus-web.ws.state]
            ;; Event namespaces (side-effects: register handlers)
            [olympus-web.events.core]
            [olympus-web.events.ws]
            [olympus-web.events.agents]
            [olympus-web.events.waves]
            [olympus-web.events.kg]
            [olympus-web.events.projects]
            [olympus-web.events.terminal]
            ;; Subscription namespace
            [olympus-web.subs.core]
            ;; View
            [olympus-web.views.app :as app]))

;; React 19 root — created once, reused on hot-reload
(defonce root (atom nil))

(defn ^:dev/after-load mount-root
  "Re-render the app. Called on hot-reload."
  []
  (rf/clear-subscription-cache!)
  (when-let [r @root]
    (rdc/render r [app/app])))

(defn init!
  "Initialize the application. Called once on page load."
  []
  (js/console.log "Olympus Web UI initializing...")
  (rf/dispatch-sync [:app/initialize])
  (let [root-el (.getElementById js/document "app")]
    (reset! root (rdc/create-root root-el))
    (rdc/render @root [app/app]))
  (js/console.log "Olympus Web UI ready"))
