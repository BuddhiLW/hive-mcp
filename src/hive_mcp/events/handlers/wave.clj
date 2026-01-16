(ns hive-mcp.events.handlers.wave
  "Drone wave event handlers.

   Handles events related to dispatch_drone_wave lifecycle:
   - :wave/start     - Wave execution started
   - :wave/item-done - Wave item completed/failed
   - :wave/complete  - Wave execution finished

   SOLID: SRP - Wave lifecycle only
   CLARITY: R - Represented intent through wave domain"
  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]))
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
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register wave-related event handlers."
  []
  (ev/reg-event :wave/start
                [interceptors/debug]
                handle-wave-start)

  (ev/reg-event :wave/item-done
                [interceptors/debug]
                handle-wave-item-done)

  (ev/reg-event :wave/complete
                [interceptors/debug]
                handle-wave-complete))
