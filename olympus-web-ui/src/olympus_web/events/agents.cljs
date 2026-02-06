(ns olympus-web.events.agents
  "Agent-related events for Olympus Web UI.
   
   Handles:
   - Agent spawn/kill lifecycle
   - Status updates from hivemind shouts
   - Agent hierarchy management"
  (:require [re-frame.core :as rf]
            [olympus-web.config :as config]))

;; =============================================================================
;; Hivemind Events (from ling shouts)
;; =============================================================================

(rf/reg-event-db
 :hivemind/event-received
 (fn [db [_ event]]
   (let [{:keys [agent-id event-type message task timestamp]} event
         ;; Normalize event-type to keyword
         event-kw (if (keyword? event-type) event-type (keyword event-type))]
     (-> db
         ;; Update agent status if we know about this agent
         (update-in [:agents agent-id]
                    (fn [agent]
                      (when agent
                        (-> agent
                            (assoc :status (case event-kw
                                             :started :working
                                             :progress :working
                                             :completed :completed
                                             :error :error
                                             :blocked :blocked
                                             (:status agent)))
                            (assoc :last-message message)
                            (cond-> task (assoc :task task))))))
         ;; Add to message buffer (keep last N)
         (update :hivemind-messages
                 (fn [msgs]
                   (let [new-msg {:agent-id agent-id
                                  :event-type event-kw
                                  :message message
                                  :task task
                                  :timestamp (or timestamp (js/Date.now))}]
                     (->> (conj (vec msgs) new-msg)
                          (take-last config/max-hivemind-messages)
                          vec))))))))

;; =============================================================================
;; Agent Lifecycle Events
;; =============================================================================

(rf/reg-event-db
 :agents/spawned
 (fn [db [_ {:keys [agent-id type parent-id project-id task]}]]
   (assoc-in db [:agents agent-id]
             {:id agent-id
              :type (keyword (or type :ling))
              :status :idle
              :parent-id parent-id
              :project-id project-id
              :task task
              :spawned-at (js/Date.now)
              :last-message nil})))

(rf/reg-event-db
 :agents/killed
 (fn [db [_ {:keys [agent-id]}]]
   (update db :agents dissoc agent-id)))

(rf/reg-event-db
 :agents/status-updated
 (fn [db [_ {:keys [agent-id status task message]}]]
   (update-in db [:agents agent-id]
              (fn [agent]
                (when agent
                  (cond-> agent
                    status (assoc :status (keyword status))
                    task (assoc :task task)
                    message (assoc :last-message message)))))))

;; =============================================================================
;; Bulk Agent Updates (for initial load or refresh)
;; =============================================================================

(rf/reg-event-db
 :agents/set-all
 (fn [db [_ agents]]
   (assoc db :agents
          (into {}
                (map (fn [a]
                       [(:id a)
                        (-> a
                            (update :type keyword)
                            (update :status keyword))]))
                agents))))

;; =============================================================================
;; UI Selection
;; =============================================================================

(rf/reg-event-db
 :agents/select
 (fn [db [_ agent-id]]
   (-> db
       (assoc-in [:ui :selected-id] agent-id)
       (assoc-in [:ui :selected-type] :agent))))

(rf/reg-event-db
 :agents/deselect
 (fn [db _]
   (-> db
       (assoc-in [:ui :selected-id] nil)
       (assoc-in [:ui :selected-type] nil))))
