(ns hive-mcp.events.handlers.validated-wave
  "Validated wave event handlers.

   Handles events related to dispatch_validated_wave lifecycle:
   - :validated-wave/start           - Validated wave execution started
   - :validated-wave/iteration-start - Iteration within loop started
   - :validated-wave/success         - Wave completed with validation passing
   - :validated-wave/partial         - Max retries reached, partial success
   - :validated-wave/retry           - Validation failed, retrying with fix tasks

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors :as interceptors]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handler: :validated-wave/start
;; =============================================================================

(defn handle-validated-wave-start
  "Handler for :validated-wave/start events.

   Called when a validated wave execution begins. Publishes event to channel
   for coordinator monitoring.

   Expects event data:
   {:task-count  5
    :max-retries 3
    :lint-level  \"error\"}

   Produces effects:
   - :channel-publish - Broadcast validated-wave-started to WebSocket clients
   - :log             - Log validated wave start message"
  [_coeffects [_ {:keys [task-count max-retries lint-level]}]]
  {:channel-publish {:event :validated-wave-started
                     :data {:task-count task-count
                            :max-retries max-retries
                            :lint-level lint-level}}
   :log {:level :info
         :message (str "Validated wave started: " task-count " tasks, "
                       "max-retries: " max-retries ", lint-level: " lint-level)}})

;; =============================================================================
;; Handler: :validated-wave/iteration-start
;; =============================================================================

(defn handle-validated-wave-iteration-start
  "Handler for :validated-wave/iteration-start events.

   Called when an iteration within the validation loop begins.

   Expects event data:
   {:iteration  1
    :task-count 5}

   Produces effects:
   - :channel-publish - Broadcast iteration start to WebSocket clients
   - :log             - Log iteration start message"
  [_coeffects [_ {:keys [iteration task-count]}]]
  {:channel-publish {:event :validated-wave-iteration-start
                     :data {:iteration iteration
                            :task-count task-count}}
   :log {:level :info
         :message (str "Validated wave iteration " iteration " started with " task-count " tasks")}})

;; =============================================================================
;; Handler: :validated-wave/success
;; =============================================================================

(defn handle-validated-wave-success
  "Handler for :validated-wave/success events.

   Called when validated wave completes with all validations passing.

   Expects event data:
   {:iterations  2
    :wave-id     \"wave-uuid\"
    :duration-ns 1234567890}

   Produces effects:
   - :channel-publish - Broadcast success to WebSocket clients
   - :log             - Log success message"
  [_coeffects [_ {:keys [iterations wave-id duration-ns]}]]
  (let [duration-ms (when duration-ns (/ duration-ns 1000000.0))]
    {:channel-publish {:event :validated-wave-success
                       :data {:iterations iterations
                              :wave-id wave-id
                              :duration-ms duration-ms}}
     :log {:level :info
           :message (str "Validated wave success after " iterations " iteration(s)"
                         (when duration-ms (str " in " (int duration-ms) "ms")))}}))

;; =============================================================================
;; Handler: :validated-wave/partial
;; =============================================================================

(defn handle-validated-wave-partial
  "Handler for :validated-wave/partial events.

   Called when max retries reached with some lint errors remaining.

   Expects event data:
   {:iterations         3
    :remaining-findings 5
    :duration-ns        1234567890}

   Produces effects:
   - :channel-publish - Broadcast partial result to WebSocket clients
   - :log             - Log warning about partial completion"
  [_coeffects [_ {:keys [iterations remaining-findings duration-ns]}]]
  (let [duration-ms (when duration-ns (/ duration-ns 1000000.0))]
    {:channel-publish {:event :validated-wave-partial
                       :data {:iterations iterations
                              :remaining-findings remaining-findings
                              :duration-ms duration-ms}}
     :log {:level :warn
           :message (str "Validated wave partial after " iterations " iteration(s): "
                         remaining-findings " lint errors remain")}}))

;; =============================================================================
;; Handler: :validated-wave/retry
;; =============================================================================

(defn handle-validated-wave-retry
  "Handler for :validated-wave/retry events.

   Called when validation fails and fix tasks are generated for retry.

   Expects event data:
   {:iteration      1
    :finding-count  5
    :fix-task-count 3}

   Produces effects:
   - :channel-publish - Broadcast retry info to WebSocket clients
   - :log             - Log retry details"
  [_coeffects [_ {:keys [iteration finding-count fix-task-count]}]]
  {:channel-publish {:event :validated-wave-retry
                     :data {:iteration iteration
                            :finding-count finding-count
                            :fix-task-count fix-task-count}}
   :log {:level :info
         :message (str "Validated wave retry: iteration " iteration " found "
                       finding-count " errors, generated " fix-task-count " fix tasks")}})

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-handlers!
  "Register validated-wave event handlers."
  []
  (ev/reg-event :validated-wave/start
                [interceptors/debug]
                handle-validated-wave-start)

  (ev/reg-event :validated-wave/iteration-start
                [interceptors/debug]
                handle-validated-wave-iteration-start)

  (ev/reg-event :validated-wave/success
                [interceptors/debug]
                handle-validated-wave-success)

  (ev/reg-event :validated-wave/partial
                [interceptors/debug]
                handle-validated-wave-partial)

  (ev/reg-event :validated-wave/retry
                [interceptors/debug]
                handle-validated-wave-retry))
