(ns olympus-web.events.terminal
  "Terminal events for xterm.js ling output viewing.

   Handles:
   - Subscribing/unsubscribing to ling output streams
   - Receiving terminal output lines
   - Tab management (open, close, switch)
   - Listing available headless lings"
  (:require [re-frame.core :as rf]
            [olympus-web.events.ws :as ws]))

;; =============================================================================
;; Terminal Tab Management
;; =============================================================================

(rf/reg-event-fx
 :terminal/open-tab
 (fn [{:keys [db]} [_ ling-id]]
   (let [already-open? (get-in db [:terminal :tabs ling-id])]
     (cond-> {:db (-> db
                      (assoc-in [:terminal :tabs ling-id]
                                {:id ling-id
                                 :subscribed? false
                                 :cursor 0
                                 :buffer []})
                      (assoc-in [:terminal :active-tab] ling-id))}
       ;; Send subscribe command via WS if not already subscribed
       (not already-open?)
       (assoc :dispatch [:terminal/subscribe ling-id])))))

(rf/reg-event-fx
 :terminal/close-tab
 (fn [{:keys [db]} [_ ling-id]]
   (let [tabs (dissoc (get-in db [:terminal :tabs]) ling-id)
         active (get-in db [:terminal :active-tab])
         new-active (if (= active ling-id)
                      (first (keys tabs))
                      active)]
     {:db (-> db
              (assoc-in [:terminal :tabs] tabs)
              (assoc-in [:terminal :active-tab] new-active))
      :dispatch [:terminal/unsubscribe ling-id]})))

(rf/reg-event-db
 :terminal/switch-tab
 (fn [db [_ ling-id]]
   (assoc-in db [:terminal :active-tab] ling-id)))

;; =============================================================================
;; WebSocket Commands (send via existing WS connection)
;; =============================================================================

(rf/reg-event-fx
 :terminal/subscribe
 (fn [_ [_ ling-id]]
   {:ws/send {:type :command
              :command "ling-output/subscribe"
              :params {:ling_id ling-id}}}))

(rf/reg-event-fx
 :terminal/unsubscribe
 (fn [_ [_ ling-id]]
   {:ws/send {:type :command
              :command "ling-output/unsubscribe"
              :params {:ling_id ling-id}}}))

(rf/reg-event-fx
 :terminal/request-history
 (fn [_ [_ ling-id last-n]]
   {:ws/send {:type :command
              :command "ling-output/get"
              :params {:ling_id ling-id
                       :last_n (or last-n 200)}
              :request-id (str "history-" ling-id)}}))

(rf/reg-event-fx
 :terminal/list-lings
 (fn [_ _]
   {:ws/send {:type :command
              :command "ling-output/list"
              :params {}
              :request-id "list-headless"}}))

;; =============================================================================
;; WS Send Effect Handler
;; =============================================================================

(rf/reg-fx
 :ws/send
 (fn [msg]
   (when-let [ws @ws/ws-instance]
     (when (= (.-readyState ws) 1) ;; OPEN
       (.send ws (js/JSON.stringify (clj->js msg)))))))

;; =============================================================================
;; Incoming Data Handlers
;; =============================================================================

(rf/reg-event-db
 :terminal/output-received
 (fn [db [_ {:keys [ling_id entries lines cursor]}]]
   (let [ling-id ling_id
         new-lines (or entries (mapv (fn [l] {:text l :ts (js/Date.now)}) lines))]
     (if (get-in db [:terminal :tabs ling-id])
       (-> db
           (update-in [:terminal :tabs ling-id :buffer]
                      (fn [buf]
                        (let [combined (into (vec buf) new-lines)]
                          ;; Keep buffer bounded
                          (if (> (count combined) 10000)
                            (vec (drop (- (count combined) 10000) combined))
                            combined))))
           (assoc-in [:terminal :tabs ling-id :cursor] (or cursor (js/Date.now)))
           (assoc-in [:terminal :tabs ling-id :subscribed?] true))
       db))))

(rf/reg-event-db
 :terminal/history-received
 (fn [db [_ {:keys [ling_id lines]}]]
   (let [entries (mapv (fn [l] {:text l :ts 0}) lines)]
     (if (get-in db [:terminal :tabs ling_id])
       (-> db
           (assoc-in [:terminal :tabs ling_id :buffer] entries)
           (assoc-in [:terminal :tabs ling_id :subscribed?] true))
       db))))

(rf/reg-event-db
 :terminal/available-lings-received
 (fn [db [_ lings]]
   (assoc-in db [:terminal :available-lings] (vec lings))))

(rf/reg-event-db
 :terminal/subscription-confirmed
 (fn [db [_ {:keys [ling_id]}]]
   (assoc-in db [:terminal :tabs ling_id :subscribed?] true)))
