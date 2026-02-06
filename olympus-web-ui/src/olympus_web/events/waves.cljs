(ns olympus-web.events.waves
  "Wave-related events for Olympus Web UI.
   
   Handles:
   - Wave lifecycle (started, completed)
   - Task status updates
   - Diff review status"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Wave Lifecycle Events
;; =============================================================================

(rf/reg-event-db
 :waves/started
 (fn [db [_ {:keys [wave-id tasks]}]]
   (assoc-in db [:waves wave-id]
             {:id wave-id
              :status :running
              :started-at (js/Date.now)
              :tasks (mapv (fn [t]
                             {:file (:file t)
                              :task (:task t)
                              :status :pending
                              :diff-id nil})
                           tasks)})))

(rf/reg-event-db
 :waves/task-updated
 (fn [db [_ {:keys [wave-id file status diff-id]}]]
   (update-in db [:waves wave-id :tasks]
              (fn [tasks]
                (mapv (fn [t]
                        (if (= (:file t) file)
                          (cond-> t
                            status (assoc :status (keyword status))
                            diff-id (assoc :diff-id diff-id))
                          t))
                      tasks)))))

(rf/reg-event-db
 :waves/completed
 (fn [db [_ {:keys [wave-id status]}]]
   (-> db
       (assoc-in [:waves wave-id :status] (keyword (or status :completed)))
       (assoc-in [:waves wave-id :completed-at] (js/Date.now)))))

;; =============================================================================
;; Bulk Wave Updates
;; =============================================================================

(rf/reg-event-db
 :waves/set-all
 (fn [db [_ waves]]
   (assoc db :waves
          (into {}
                (map (fn [w]
                       [(:id w)
                        (-> w
                            (update :status keyword)
                            (update :tasks
                                    (fn [tasks]
                                      (mapv #(update % :status keyword) tasks))))]))
                waves))))

;; =============================================================================
;; UI Selection
;; =============================================================================

(rf/reg-event-db
 :waves/select
 (fn [db [_ wave-id]]
   (-> db
       (assoc-in [:ui :selected-id] wave-id)
       (assoc-in [:ui :selected-type] :wave))))

(rf/reg-event-db
 :waves/select-task
 (fn [db [_ wave-id task-idx]]
   (-> db
       (assoc-in [:ui :selected-id] [wave-id task-idx])
       (assoc-in [:ui :selected-type] :task))))
