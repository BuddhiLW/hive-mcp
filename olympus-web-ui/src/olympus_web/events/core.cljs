(ns olympus-web.events.core
  "Core re-frame events for Olympus Web UI.
   
   Includes:
   - DB initialization
   - UI state management
   - View switching"
  (:require [re-frame.core :as rf]
            [olympus-web.db :as db]))

;; =============================================================================
;; Initialization
;; =============================================================================

(rf/reg-event-fx
 :app/initialize
 (fn [_ _]
   {:db db/default-db
    :dispatch [:ws/connect]}))

;; =============================================================================
;; UI Events
;; =============================================================================

(rf/reg-event-db
 :ui/set-view
 (fn [db [_ view]]
   (assoc-in db [:ui :view] view)))

(rf/reg-event-db
 :ui/set-layout
 (fn [db [_ layout]]
   (assoc-in db [:ui :layout] layout)))

(rf/reg-event-db
 :ui/toggle-sidebar
 (fn [db _]
   (update-in db [:ui :sidebar-collapsed?] not)))

(rf/reg-event-db
 :ui/select-node
 (fn [db [_ id node-type]]
   (-> db
       (assoc-in [:ui :selected-id] id)
       (assoc-in [:ui :selected-type] node-type))))

(rf/reg-event-db
 :ui/deselect-node
 (fn [db _]
   (-> db
       (assoc-in [:ui :selected-id] nil)
       (assoc-in [:ui :selected-type] nil))))

;; =============================================================================
;; Filter Events
;; =============================================================================

(rf/reg-event-db
 :ui/toggle-agent-type-filter
 (fn [db [_ agent-type]]
   (update-in db [:ui :filters :agent-types]
              (fn [types]
                (if (contains? types agent-type)
                  (disj types agent-type)
                  (conj types agent-type))))))

(rf/reg-event-db
 :ui/toggle-agent-status-filter
 (fn [db [_ status]]
   (update-in db [:ui :filters :agent-statuses]
              (fn [statuses]
                (if (contains? statuses status)
                  (disj statuses status)
                  (conj statuses status))))))

(rf/reg-event-db
 :ui/toggle-memory-type-filter
 (fn [db [_ memory-type]]
   (update-in db [:ui :filters :memory-types]
              (fn [types]
                (if (contains? types memory-type)
                  (disj types memory-type)
                  (conj types memory-type))))))

;; =============================================================================
;; System Events
;; =============================================================================

(rf/reg-event-db
 :system/health-restored
 (fn [db [_ _msg]]
   ;; Could trigger a refresh of agent list here
   db))
