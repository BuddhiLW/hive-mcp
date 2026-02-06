(ns olympus-web.views.project-tree
  "Project tree navigator component for Olympus Web UI (HCR Wave 5).

   Shows hierarchical .hive-project.edn structure with:
   - Expandable/collapsible tree nodes
   - Project type icons and scope tags
   - Ling count badges per project
   - Click to filter agents/memory by project scope"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Tree Node Components
;; =============================================================================

(defn- project-type-icon
  "Return icon for project type."
  [project-type]
  (case (keyword (or project-type "generic"))
    :workspace "📁"
    :service "⚙️"
    :frontend "🖥️"
    :library "📚"
    :generic "📦"
    "📦"))

(defn- ling-count-badge
  "Badge showing number of active lings in a project."
  [count]
  (when (and count (pos? count))
    [:span.ling-count-badge count]))

(defn- project-tags
  "Render project tags as small pills."
  [tags]
  (when (seq tags)
    [:div.project-tags
     (for [tag tags]
       ^{:key tag}
       [:span.project-tag tag])]))

(declare tree-node)

(defn tree-children
  "Render children of a tree node."
  [project-id children-map projects-map expanded selected-id depth]
  (let [child-ids (get children-map project-id [])]
    (when (and (seq child-ids) (contains? expanded project-id))
      [:div.tree-children
       (for [child-id child-ids]
         ^{:key child-id}
         [tree-node child-id children-map projects-map expanded selected-id (inc depth)])])))

(defn tree-node
  "Single project tree node with expand/collapse and selection."
  [project-id children-map projects-map expanded selected-id depth]
  (let [project (get projects-map project-id)
        has-children? (seq (get children-map project-id))
        is-expanded? (contains? expanded project-id)
        is-selected? (= selected-id project-id)]
    [:div.tree-node-wrapper
     [:div.tree-node
      {:class (str (when is-selected? "selected ")
                   (when has-children? "has-children"))
       :style {:padding-left (str (+ 8 (* depth 16)) "px")}
       :on-click (fn [e]
                   (.stopPropagation e)
                   (rf/dispatch [:projects/select project-id]))}
      ;; Expand/collapse chevron
      [:span.tree-chevron
       {:on-click (fn [e]
                    (.stopPropagation e)
                    (when has-children?
                      (rf/dispatch [:projects/toggle-expand project-id])))}
       (cond
         (not has-children?) [:span.tree-chevron-spacer]
         is-expanded? "▼"
         :else "▶")]
      ;; Project icon
      [:span.tree-icon (project-type-icon (:type project))]
      ;; Project name
      [:span.tree-label project-id]
      ;; Ling count badge
      [ling-count-badge (:ling-count project)]]
     ;; Children (recursive)
     [tree-children project-id children-map projects-map expanded selected-id depth]]))

;; =============================================================================
;; Tree Actions Toolbar
;; =============================================================================

(defn tree-toolbar
  "Toolbar with expand all / collapse all / refresh actions."
  []
  (let [project-count @(rf/subscribe [:projects/count])]
    [:div.tree-toolbar
     [:span.tree-toolbar-label
      (str project-count " project" (when (not= project-count 1) "s"))]
     [:div.tree-toolbar-actions
      [:button.tree-toolbar-btn
       {:title "Expand All"
        :on-click #(rf/dispatch [:projects/expand-all])}
       "⊞"]
      [:button.tree-toolbar-btn
       {:title "Collapse All"
        :on-click #(rf/dispatch [:projects/collapse-all])}
       "⊟"]]]))

;; =============================================================================
;; Project Detail Panel
;; =============================================================================

(defn project-detail
  "Detail view for a selected project."
  [project-id]
  (let [project @(rf/subscribe [:projects/by-id project-id])
        children @(rf/subscribe [:projects/children-of project-id])]
    (when project
      [:div.detail-panel-content
       [:div.detail-field
        [:div.detail-label "Project ID"]
        [:div.detail-value {:style {:font-family "monospace"}} (:id project)]]
       [:div.detail-field
        [:div.detail-label "Type"]
        [:div.detail-value
         [:span {:style {:margin-right "0.5rem"}} (project-type-icon (:type project))]
         (or (:type project) "generic")]]
       [:div.detail-field
        [:div.detail-label "Path"]
        [:div.detail-value {:style {:font-family "monospace" :font-size "0.75rem"
                                    :word-break "break-all"}}
         (:path project)]]
       (when (:parent-id project)
         [:div.detail-field
          [:div.detail-label "Parent"]
          [:div.detail-value {:style {:font-family "monospace"}} (:parent-id project)]])
       [:div.detail-field
        [:div.detail-label "Active Lings"]
        [:div.detail-value (or (:ling-count project) 0)]]
       (when (:git-root project)
         [:div.detail-field
          [:div.detail-label "Git Root"]
          [:div.detail-value {:style {:font-family "monospace" :font-size "0.75rem"
                                      :word-break "break-all"}}
           (:git-root project)]])
       (when (seq (:tags project))
         [:div.detail-field
          [:div.detail-label "Tags"]
          [:div.detail-value
           (for [tag (:tags project)]
             ^{:key tag}
             [:span {:style {:display "inline-block"
                             :background "var(--bg-tertiary)"
                             :padding "0.125rem 0.5rem"
                             :border-radius "0.25rem"
                             :margin-right "0.25rem"
                             :margin-bottom "0.25rem"
                             :font-size "0.75rem"}}
              tag])]])
       (when (seq children)
         [:div.detail-field
          [:div.detail-label (str "Children (" (count children) ")")]
          [:div
           (for [child children]
             ^{:key (:id child)}
             [:div {:style {:display "flex" :align-items "center" :gap "0.5rem"
                            :margin-bottom "0.25rem" :cursor "pointer"
                            :padding "0.25rem 0.5rem" :border-radius "0.25rem"}
                    :on-click #(rf/dispatch [:projects/select (:id child)])}
              [:span (project-type-icon (:type child))]
              [:span {:style {:font-family "monospace" :font-size "0.8rem"}}
               (:id child)]
              [ling-count-badge (:ling-count child)]])]])
       (when (:last-scanned project)
         [:div.detail-field
          [:div.detail-label "Last Scanned"]
          [:div.detail-value {:style {:font-size "0.75rem"}}
           (:last-scanned project)]])])))

;; =============================================================================
;; Main Project Tree View
;; =============================================================================

(defn project-tree-view
  "Main project tree view - shows hierarchical project structure.
   Used as a graph-level view (replaces ReactFlow for this tab)."
  []
  (let [roots @(rf/subscribe [:projects/roots])
        children-map @(rf/subscribe [:projects/children])
        tree-data @(rf/subscribe [:projects/tree])
        projects-map (:projects tree-data)
        expanded (:expanded tree-data)
        selected-id (:selected-project tree-data)]
    (if (empty? roots)
      ;; Empty state
      [:div {:style {:display "flex"
                     :align-items "center"
                     :justify-content "center"
                     :height "100%"
                     :color "var(--text-secondary)"}}
       [:div {:style {:text-align "center"}}
        [:div {:style {:font-size "3rem" :margin-bottom "1rem"}} "🌳"]
        [:div "No projects discovered"]
        [:div {:style {:font-size "0.875rem" :margin-top "0.5rem"}}
         "Run " [:code "project scan"] " to discover .hive-project.edn files"]]]
      ;; Tree view
      [:div.project-tree-container
       [tree-toolbar]
       [:div.project-tree-scroll
        (for [root-id roots]
          ^{:key root-id}
          [tree-node root-id children-map projects-map expanded selected-id 0])]])))
