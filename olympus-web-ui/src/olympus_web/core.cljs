(ns olympus-web.core
  "Main entry point for Olympus Web UI.
   
   Initializes re-frame app and mounts to DOM."
  (:require [reagent.dom :as rdom]
            [re-frame.core :as rf]
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

(defn ^:dev/after-load mount-root
  "Mount the app to the DOM. Called on hot-reload."
  []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [app/app] root-el)))

(defn init!
  "Initialize the application. Called once on page load."
  []
  (js/console.log "🏛️ Olympus Web UI initializing...")
  (rf/dispatch-sync [:app/initialize])
  (mount-root)
  (js/console.log "🏛️ Olympus Web UI ready"))
