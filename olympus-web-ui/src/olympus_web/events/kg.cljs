(ns olympus-web.events.kg
  "Knowledge Graph (memory) events for Olympus Web UI.
   
   Handles:
   - Memory entry CRUD
   - KG edge management
   - Memory visualization state"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Memory Entry Events
;; =============================================================================

(rf/reg-event-db
 :kg/entry-added
 (fn [db [_ {:keys [id type content tags scope duration edges]}]]
   (assoc-in db [:kg-entries id]
             {:id id
              :type (keyword (or type :note))
              :content content
              :tags (vec tags)
              :scope scope
              :duration (keyword (or duration :medium))
              :edges (or edges [])
              :created-at (js/Date.now)})))

(rf/reg-event-db
 :kg/entry-updated
 (fn [db [_ {:keys [id] :as updates}]]
   (update-in db [:kg-entries id]
              (fn [entry]
                (when entry
                  (merge entry
                         (cond-> (dissoc updates :id)
                           (:type updates) (update :type keyword)
                           (:duration updates) (update :duration keyword))))))))

(rf/reg-event-db
 :kg/entry-removed
 (fn [db [_ {:keys [id]}]]
   (update db :kg-entries dissoc id)))

;; =============================================================================
;; KG Edge Events
;; =============================================================================

(rf/reg-event-db
 :kg/edge-added
 (fn [db [_ {:keys [from to relation confidence]}]]
   (update-in db [:kg-entries from :edges]
              (fn [edges]
                (conj (or edges [])
                      {:to to
                       :relation (keyword relation)
                       :confidence (or confidence 1.0)})))))

;; =============================================================================
;; Bulk KG Updates
;; =============================================================================

(rf/reg-event-db
 :kg/set-all
 (fn [db [_ entries]]
   (assoc db :kg-entries
          (into {}
                (map (fn [e]
                       [(:id e)
                        (-> e
                            (update :type keyword)
                            (update :duration keyword))]))
                entries))))

;; =============================================================================
;; UI Selection
;; =============================================================================

(rf/reg-event-db
 :kg/select
 (fn [db [_ entry-id]]
   (-> db
       (assoc-in [:ui :selected-id] entry-id)
       (assoc-in [:ui :selected-type] :kg))))
