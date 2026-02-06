(ns olympus-web.events.projects
  "Project tree events for Olympus Web UI (HCR Wave 5).

   Handles:
   - Project tree snapshot loading from init-snapshot
   - Tree node expand/collapse
   - Project selection for filtering agents/memory by project scope
   - Real-time project tree updates via WebSocket"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Bulk Load (from init-snapshot or /api/project-tree)
;; =============================================================================

(rf/reg-event-db
 :projects/set-all
 (fn [db [_ tree-data]]
   (let [{:keys [projects roots children total]} tree-data
         ;; Index projects by id
         projects-by-id (into {} (map (fn [p] [(:id p) p]) projects))]
     (-> db
         (assoc-in [:project-tree :projects] projects-by-id)
         (assoc-in [:project-tree :roots] (vec roots))
         (assoc-in [:project-tree :children] (or children {}))
         ;; Auto-expand roots on first load
         (update-in [:project-tree :expanded]
                    (fn [exp]
                      (if (empty? exp)
                        (set roots)
                        exp)))))))

;; =============================================================================
;; Tree Interaction Events
;; =============================================================================

(rf/reg-event-db
 :projects/toggle-expand
 (fn [db [_ project-id]]
   (update-in db [:project-tree :expanded]
              (fn [expanded]
                (if (contains? expanded project-id)
                  (disj expanded project-id)
                  (conj expanded project-id))))))

(rf/reg-event-db
 :projects/expand-all
 (fn [db _]
   (let [all-ids (keys (get-in db [:project-tree :projects]))]
     (assoc-in db [:project-tree :expanded] (set all-ids)))))

(rf/reg-event-db
 :projects/collapse-all
 (fn [db _]
   (assoc-in db [:project-tree :expanded] #{})))

(rf/reg-event-db
 :projects/select
 (fn [db [_ project-id]]
   (-> db
       (assoc-in [:project-tree :selected-project] project-id)
       (assoc-in [:ui :selected-id] project-id)
       (assoc-in [:ui :selected-type] :project))))

(rf/reg-event-db
 :projects/deselect
 (fn [db _]
   (-> db
       (assoc-in [:project-tree :selected-project] nil)
       (assoc-in [:ui :selected-id] nil)
       (assoc-in [:ui :selected-type] nil))))

;; =============================================================================
;; Real-time Update Events
;; =============================================================================

(rf/reg-event-db
 :projects/project-updated
 (fn [db [_ project-data]]
   (let [pid (:id project-data)]
     (assoc-in db [:project-tree :projects pid] project-data))))

(rf/reg-event-db
 :projects/ling-count-updated
 (fn [db [_ {:keys [project-id count]}]]
   (assoc-in db [:project-tree :projects project-id :ling-count] count)))
