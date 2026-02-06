(ns olympus-web.views.terminal
  "xterm.js terminal component for viewing ling output.

   Architecture:
   - Each tab has its own xterm.js Terminal instance
   - Terminal output is streamed via WebSocket ling-output/subscribe
   - FitAddon handles automatic resizing
   - Tab bar allows switching between multiple lings"
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [olympus-web.config :as config]))

;; =============================================================================
;; xterm.js Terminal Instance Management
;; =============================================================================

(defonce terminals (atom {}))  ;; ling-id -> {:term Terminal :fit FitAddon}

(defn- create-terminal!
  "Create a new xterm.js Terminal instance with FitAddon."
  [ling-id container-el]
  (when container-el
    (let [;; xterm.js 5.x exposes as window.Terminal (from UMD bundle)
          Terminal (or (aget js/window "Terminal")
                       (when (exists? js/Terminal) js/Terminal))
          FitAddon (or (aget js/window "FitAddon")
                       (when (exists? js/FitAddon) js/FitAddon))
          WebLinksAddon (or (aget js/window "WebLinksAddon")
                            (when (exists? js/WebLinksAddon) js/WebLinksAddon))
          term-opts (clj->js (merge config/terminal-options
                                    {:theme (clj->js config/terminal-theme)}))
          term (when Terminal (new Terminal term-opts))
          fit (when (and term FitAddon) (new FitAddon))
          web-links (when (and term WebLinksAddon) (new WebLinksAddon))]
      (when term
        ;; Load addons
        (when fit (.loadAddon term fit))
        (when web-links (.loadAddon term web-links))
        ;; Open terminal in container
        (.open term container-el)
        ;; Fit to container
        (when fit
          (js/setTimeout #(.fit fit) 50))
        ;; Store reference
        (swap! terminals assoc ling-id {:term term :fit fit})
        ;; Write welcome message
        (.writeln term (str "\x1b[36m--- Ling output: " ling-id " ---\x1b[0m"))
        (.writeln term "")
        term))))

(defn- destroy-terminal!
  "Dispose of a terminal instance."
  [ling-id]
  (when-let [{:keys [term]} (get @terminals ling-id)]
    (.dispose term)
    (swap! terminals dissoc ling-id)))

(defn- write-to-terminal!
  "Write lines to an existing terminal."
  [ling-id lines]
  (when-let [{:keys [term]} (get @terminals ling-id)]
    (doseq [line lines]
      (let [text (if (map? line) (:text line) (str line))]
        (.writeln term (str text))))))

(defn- fit-terminal!
  "Refit terminal to its container."
  [ling-id]
  (when-let [{:keys [fit]} (get @terminals ling-id)]
    (when fit
      (try (.fit fit) (catch :default _e nil)))))

;; =============================================================================
;; Terminal Tab Bar
;; =============================================================================

(defn tab-bar
  "Tab bar for switching between ling terminals."
  []
  (let [tabs @(rf/subscribe [:terminal/tab-list])
        active-tab @(rf/subscribe [:terminal/active-tab])]
    [:div.terminal-tab-bar
     ;; Existing tabs
     (for [{:keys [id subscribed?]} tabs]
       ^{:key id}
       [:div.terminal-tab
        {:class (when (= id active-tab) "active")
         :on-click #(rf/dispatch [:terminal/switch-tab id])}
        [:span.terminal-tab-indicator
         {:class (if subscribed? "connected" "connecting")}]
        [:span.terminal-tab-label
         (let [short-id (if (> (count id) 20)
                          (str (subs id 0 18) "...")
                          id)]
           short-id)]
        [:button.terminal-tab-close
         {:on-click (fn [e]
                      (.stopPropagation e)
                      (rf/dispatch [:terminal/close-tab id]))}
         "\u00d7"]])
     ;; Add new tab button
     [:button.terminal-tab-add
      {:on-click #(rf/dispatch [:terminal/list-lings])}
      "+"]]))

;; =============================================================================
;; Ling Picker (shown when + is clicked or in empty state)
;; =============================================================================

(defn ling-picker
  "List of available lings to open in a new terminal tab."
  []
  (let [agents @(rf/subscribe [:agents/all])
        ling-agents (filter #(= (:type %) :ling) agents)
        open-tabs @(rf/subscribe [:terminal/tabs])
        openable (remove #(contains? open-tabs (:id %)) ling-agents)]
    [:div.ling-picker
     [:div.ling-picker-header "Select Ling"]
     (if (seq openable)
       [:div.ling-picker-list
        (for [{:keys [id status task]} openable]
          ^{:key id}
          [:div.ling-picker-item
           {:on-click #(rf/dispatch [:terminal/open-tab id])}
           [:span.ling-picker-id (if (> (count id) 25)
                                   (str (subs id 0 23) "...")
                                   id)]
           [:span.ling-picker-status {:class (name (or status :idle))}
            (name (or status :idle))]
           (when task
             [:span.ling-picker-task (if (> (count task) 40)
                                       (str (subs task 0 38) "...")
                                       task)])])]
       [:div.ling-picker-empty
        "No additional lings available."
        [:br]
        "Headless lings will appear here when spawned."])]))

;; =============================================================================
;; Single Terminal Pane (Form-3 Reagent Component)
;; =============================================================================

(defn terminal-pane
  "A single xterm.js terminal pane for one ling."
  [ling-id]
  (let [prev-buffer-count (atom 0)
        resize-handler (atom nil)]
    (r/create-class
     {:display-name (str "terminal-pane-" ling-id)

      :component-did-mount
      (fn [this]
        (let [node (rdom/dom-node this)]
          ;; Create terminal
          (create-terminal! ling-id node)
          ;; Request history on mount
          (rf/dispatch [:terminal/request-history ling-id 200])
          ;; Setup resize listener
          (let [handler #(fit-terminal! ling-id)]
            (reset! resize-handler handler)
            (.addEventListener js/window "resize" handler))))

      :component-did-update
      (fn [_this _]
        (let [buffer @(rf/subscribe [:terminal/buffer-for ling-id])
              prev-count @prev-buffer-count
              new-count (count buffer)]
          (when (> new-count prev-count)
            ;; Write only new lines
            (let [new-lines (subvec buffer prev-count)]
              (write-to-terminal! ling-id new-lines)
              (reset! prev-buffer-count new-count)))))

      :component-will-unmount
      (fn [_]
        (when-let [handler @resize-handler]
          (.removeEventListener js/window "resize" handler))
        (destroy-terminal! ling-id))

      :reagent-render
      (fn [ling-id]
        @(rf/subscribe [:terminal/buffer-for ling-id])  ;; Force re-render on buffer change
        [:div.terminal-container
         {:style {:width "100%" :height "100%"}}])})))

;; =============================================================================
;; Main Terminal View
;; =============================================================================

(defn terminal-view
  "Main terminal view with tab bar and terminal panes."
  []
  (let [active-tab @(rf/subscribe [:terminal/active-tab])
        tabs @(rf/subscribe [:terminal/tab-list])]
    [:div.terminal-view
     [tab-bar]
     [:div.terminal-panes
      (if active-tab
        ;; Show active terminal
        ^{:key active-tab}
        [terminal-pane active-tab]
        ;; Empty state
        [:div.terminal-empty
         [:div.terminal-empty-icon "\uD83D\uDCBB"]
         [:div.terminal-empty-title "No Terminal Open"]
         [:div.terminal-empty-subtitle
          "Click + to open a ling terminal, or select a ling from the agent graph."]
         [:button.terminal-empty-btn
          {:on-click #(rf/dispatch [:terminal/list-lings])}
          "Browse Available Lings"]])]]))
