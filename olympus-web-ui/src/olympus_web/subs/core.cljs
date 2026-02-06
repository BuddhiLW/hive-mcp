(ns olympus-web.subs.core
  "Core re-frame subscriptions for Olympus Web UI."
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Connection Subscriptions
;; =============================================================================

(rf/reg-sub
 :connection/status
 (fn [db _]
   (get-in db [:connection :status])))

(rf/reg-sub
 :connection/error
 (fn [db _]
   (get-in db [:connection :last-error])))

;; =============================================================================
;; Agent Subscriptions
;; =============================================================================

(rf/reg-sub
 :agents/all
 (fn [db _]
   (vals (:agents db))))

(rf/reg-sub
 :agents/by-id
 (fn [db [_ id]]
   (get-in db [:agents id])))

(rf/reg-sub
 :agents/filtered
 :<- [:agents/all]
 :<- [:ui/filters]
 (fn [[agents filters] _]
   (let [{:keys [agent-types agent-statuses]} filters]
     (filter (fn [a]
               (and (contains? agent-types (:type a))
                    (contains? agent-statuses (:status a))))
             agents))))

(rf/reg-sub
 :agents/by-type
 :<- [:agents/all]
 (fn [agents [_ agent-type]]
   (filter #(= (:type %) agent-type) agents)))

(rf/reg-sub
 :agents/count
 :<- [:agents/all]
 (fn [agents _]
   (count agents)))

(rf/reg-sub
 :agents/count-by-type
 :<- [:agents/all]
 (fn [agents [_ agent-type]]
   (count (filter #(= (:type %) agent-type) agents))))

(rf/reg-sub
 :agents/count-by-status
 :<- [:agents/all]
 (fn [agents [_ status]]
   (count (filter #(= (:status %) status) agents))))

;; =============================================================================
;; Wave Subscriptions
;; =============================================================================

(rf/reg-sub
 :waves/all
 (fn [db _]
   (vals (:waves db))))

(rf/reg-sub
 :waves/by-id
 (fn [db [_ id]]
   (get-in db [:waves id])))

(rf/reg-sub
 :waves/running
 :<- [:waves/all]
 (fn [waves _]
   (filter #(= (:status %) :running) waves)))

(rf/reg-sub
 :waves/count
 :<- [:waves/all]
 (fn [waves _]
   (count waves)))

;; =============================================================================
;; KG Subscriptions
;; =============================================================================

(rf/reg-sub
 :kg/all
 (fn [db _]
   (vals (:kg-entries db))))

(rf/reg-sub
 :kg/by-id
 (fn [db [_ id]]
   (get-in db [:kg-entries id])))

(rf/reg-sub
 :kg/filtered
 :<- [:kg/all]
 :<- [:ui/filters]
 (fn [[entries filters] _]
   (let [{:keys [memory-types]} filters]
     (filter #(contains? memory-types (:type %)) entries))))

(rf/reg-sub
 :kg/by-type
 :<- [:kg/all]
 (fn [entries [_ entry-type]]
   (filter #(= (:type %) entry-type) entries)))

(rf/reg-sub
 :kg/count
 :<- [:kg/all]
 (fn [entries _]
   (count entries)))

;; =============================================================================
;; Hivemind Message Subscriptions
;; =============================================================================

(rf/reg-sub
 :hivemind/messages
 (fn [db _]
   (:hivemind-messages db)))

(rf/reg-sub
 :hivemind/messages-for-agent
 :<- [:hivemind/messages]
 (fn [messages [_ agent-id]]
   (filter #(= (:agent-id %) agent-id) messages)))

(rf/reg-sub
 :hivemind/recent-messages
 :<- [:hivemind/messages]
 (fn [messages [_ n]]
   (take-last (or n 10) messages)))

;; =============================================================================
;; UI Subscriptions
;; =============================================================================

(rf/reg-sub
 :ui/view
 (fn [db _]
   (get-in db [:ui :view])))

(rf/reg-sub
 :ui/layout
 (fn [db _]
   (get-in db [:ui :layout])))

(rf/reg-sub
 :ui/selected-id
 (fn [db _]
   (get-in db [:ui :selected-id])))

(rf/reg-sub
 :ui/selected-type
 (fn [db _]
   (get-in db [:ui :selected-type])))

(rf/reg-sub
 :ui/selected
 (fn [db _]
   {:id (get-in db [:ui :selected-id])
    :type (get-in db [:ui :selected-type])}))

(rf/reg-sub
 :ui/filters
 (fn [db _]
   (get-in db [:ui :filters])))

(rf/reg-sub
 :ui/sidebar-collapsed?
 (fn [db _]
   (get-in db [:ui :sidebar-collapsed?])))

;; =============================================================================
;; Project Tree Subscriptions (HCR Wave 5)
;; =============================================================================

(rf/reg-sub
 :projects/tree
 (fn [db _]
   (:project-tree db)))

(rf/reg-sub
 :projects/all
 (fn [db _]
   (vals (get-in db [:project-tree :projects]))))

(rf/reg-sub
 :projects/by-id
 (fn [db [_ id]]
   (get-in db [:project-tree :projects id])))

(rf/reg-sub
 :projects/roots
 (fn [db _]
   (get-in db [:project-tree :roots])))

(rf/reg-sub
 :projects/children
 (fn [db _]
   (get-in db [:project-tree :children])))

(rf/reg-sub
 :projects/expanded
 (fn [db _]
   (get-in db [:project-tree :expanded])))

(rf/reg-sub
 :projects/selected-project
 (fn [db _]
   (get-in db [:project-tree :selected-project])))

(rf/reg-sub
 :projects/count
 :<- [:projects/all]
 (fn [projects _]
   (count projects)))

(rf/reg-sub
 :projects/total-lings
 :<- [:projects/all]
 (fn [projects _]
   (reduce + 0 (map #(or (:ling-count %) 0) projects))))

(rf/reg-sub
 :projects/children-of
 (fn [db [_ project-id]]
   (let [child-ids (get-in db [:project-tree :children project-id] [])
         projects (get-in db [:project-tree :projects])]
     (mapv #(get projects %) child-ids))))

(rf/reg-sub
 :projects/is-expanded?
 (fn [db [_ project-id]]
   (contains? (get-in db [:project-tree :expanded]) project-id)))

;; =============================================================================
;; Terminal Subscriptions
;; =============================================================================

(rf/reg-sub
 :terminal/tabs
 (fn [db _]
   (get-in db [:terminal :tabs])))

(rf/reg-sub
 :terminal/active-tab
 (fn [db _]
   (get-in db [:terminal :active-tab])))

(rf/reg-sub
 :terminal/active-tab-data
 (fn [db _]
   (let [active (get-in db [:terminal :active-tab])]
     (get-in db [:terminal :tabs active]))))

(rf/reg-sub
 :terminal/available-lings
 (fn [db _]
   (get-in db [:terminal :available-lings])))

(rf/reg-sub
 :terminal/tab-list
 :<- [:terminal/tabs]
 (fn [tabs _]
   (vals tabs)))

(rf/reg-sub
 :terminal/buffer-for
 (fn [db [_ ling-id]]
   (get-in db [:terminal :tabs ling-id :buffer])))
