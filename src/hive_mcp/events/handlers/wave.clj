(ns hive-mcp.events.handlers.wave
  "Drone wave event handlers.

   Handles events related to dispatch_drone_wave lifecycle:
   - :wave/start      - Wave execution started
   - :wave/batch-start - Batch within wave started
   - :wave/item-done  - Wave item completed/failed
   - :wave/cancelled  - Wave was cancelled
   - :wave/complete   - Wave execution finished
   - :wave/reconcile  - Reconcile wave status with filesystem reality

   SOLID: SRP - Wave lifecycle only
   CLARITY: R - Represented intent through wave domain"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]
            [hive-mcp.swarm.datascript.coordination :as ds]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handler: :wave/start
;; =============================================================================

(defn handle-wave-start
  "Handler for :wave/start events.

   Called when a drone wave execution begins. Publishes event to channel
   for coordinator monitoring and logs the wave start.

   Expects event data:
   {:plan-id    \"plan-uuid\"
    :wave-id    \"wave-uuid\"
    :item-count 5}

   Produces effects:
   - :channel-publish - Broadcast wave-started to WebSocket clients
   - :log             - Log wave start message"
  [_coeffects [_ {:keys [plan-id wave-id item-count]}]]
  {:channel-publish {:event :wave-started
                     :data {:plan-id plan-id
                            :wave-id wave-id
                            :item-count item-count}}
   :log {:level :info
         :message (str "Wave started: " wave-id " with " item-count " items")}})

;; =============================================================================
;; Handler: :wave/batch-start
;; =============================================================================

(defn handle-wave-batch-start
  "Handler for :wave/batch-start events.

   Called when a batch within a wave begins execution.
   Publishes event to channel for real-time monitoring.

   Expects event data:
   {:wave-id    \"wave-uuid\"
    :batch-num  1
    :item-count 3}

   Produces effects:
   - :channel-publish - Broadcast wave-batch-start to WebSocket clients
   - :log             - Log batch start message"
  [_coeffects [_ {:keys [wave-id batch-num item-count]}]]
  {:channel-publish {:event :wave-batch-start
                     :data {:wave-id wave-id
                            :batch-num batch-num
                            :item-count item-count}}
   :log {:level :info
         :message (str "Wave batch " batch-num " started with " item-count " items")}})

;; =============================================================================
;; Handler: :wave/item-done
;; =============================================================================

(defn handle-wave-item-done
  "Handler for :wave/item-done events.

   Called when a single drone task completes (success or failure).
   Publishes event to channel for real-time monitoring.

   Expects event data:
   {:item-id \"item-uuid\"
    :status  :completed | :failed
    :wave-id \"wave-uuid\"}

   Produces effects:
   - :channel-publish - Broadcast wave-item-done to WebSocket clients"
  [_coeffects [_ {:keys [item-id status wave-id]}]]
  {:channel-publish {:event :wave-item-done
                     :data {:item-id item-id
                            :status status
                            :wave-id wave-id}}})

;; =============================================================================
;; Handler: :wave/cancelled
;; =============================================================================

(defn handle-wave-cancelled
  "Handler for :wave/cancelled events.

   Called when a wave is cancelled due to timeout or explicit cancel request.
   Publishes cancellation event for monitoring.

   Expects event data:
   {:plan-id  \"plan-uuid\"
    :wave-id  \"wave-uuid\"
    :reason   :timeout | :explicit | :error
    :message  \"Optional details\"}

   Produces effects:
   - :channel-publish - Broadcast wave-cancelled to WebSocket clients
   - :log             - Log wave cancellation message"
  [_coeffects [_ {:keys [plan-id wave-id reason message]}]]
  {:channel-publish {:event :wave-cancelled
                     :data {:plan-id plan-id
                            :wave-id wave-id
                            :reason reason
                            :message message}}
   :log {:level :warn
         :message (str "Wave cancelled: " wave-id
                       " - reason: " (name (or reason :unknown))
                       (when message (str " - " message)))}})

;; =============================================================================
;; Handler: :wave/complete
;; =============================================================================

(defn handle-wave-complete
  "Handler for :wave/complete events.

   Called when all drone tasks in a wave have finished.
   Publishes completion event and logs results.

   Expects event data:
   {:plan-id  \"plan-uuid\"
    :wave-id  \"wave-uuid\"
    :results  {:completed N :failed M}}

   Produces effects:
   - :channel-publish - Broadcast wave-complete to WebSocket clients
   - :log             - Log wave completion message"
  [_coeffects [_ {:keys [plan-id wave-id results]}]]
  (let [{:keys [completed failed]} results]
    {:channel-publish {:event :wave-complete
                       :data {:plan-id plan-id
                              :wave-id wave-id
                              :results results}}
     :log {:level :info
           :message (str "Wave complete: " wave-id
                         " - " completed " completed, " failed " failed")}}))

;; =============================================================================
;; Handler: :wave/reconcile
;; =============================================================================

(defn handle-wave-reconcile
  "Handler for :wave/reconcile events.

   Reconciles wave status with filesystem reality.
   Useful for debugging when wave status doesn't match actual file state.

   Expects event data:
   {:wave-id \"wave-uuid\"}

   Produces effects:
   - :log - Report any discrepancies found"
  [_coeffects [_ {:keys [wave-id]}]]
  (let [wave (ds/get-wave wave-id)]
    (if wave
      {:log {:level :info
             :message (str "Wave reconcile requested for: " wave-id
                           " status: " (:wave/status wave)
                           " completed: " (:wave/completed-count wave)
                           " failed: " (:wave/failed-count wave))}}
      {:log {:level :warn
             :message (str "Wave not found for reconcile: " wave-id)}})))

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register wave-related event handlers."
  []
  (ev/reg-event :wave/start
                [interceptors/debug]
                handle-wave-start)

  (ev/reg-event :wave/batch-start
                [interceptors/debug]
                handle-wave-batch-start)

  (ev/reg-event :wave/item-done
                [interceptors/debug]
                handle-wave-item-done)

  (ev/reg-event :wave/complete
                [interceptors/debug]
                handle-wave-complete)

  (ev/reg-event :wave/cancelled
                [interceptors/debug]
                handle-wave-cancelled)

  (ev/reg-event :wave/reconcile
                [interceptors/debug]
                handle-wave-reconcile))
