(ns hive-mcp.events.effects.lifecycle
  "Lifecycle effect handlers for GC sweep side-effects.

   Effects implemented:
   - :lifecycle/sweep-fx - Execute bounded atom eviction via gc.bounded-atom registry

   Part of gc-fix-4: prevents GC death spiral from unbounded atom growth.

   Usage:
   ```clojure
   (require '[hive-mcp.events.effects.lifecycle :as lifecycle-effects])
   (lifecycle-effects/register-lifecycle-effects!)
   ```"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.gc.bounded-atom :as batom]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State: Last sweep result for query
;; =============================================================================

(defonce ^{:doc "Last sweep result for status queries."}
  *last-sweep-result
  (atom nil))

;; =============================================================================
;; Effect: :lifecycle/sweep-fx
;; =============================================================================

(defn- handle-sweep-fx
  "Execute a :lifecycle/sweep-fx effect -- bounded atom eviction.

   Calls batom/sweep-all! to evict stale/over-capacity entries from
   all registered bounded atoms. Stores the result for status queries.

   Expected data shape:
   {:atom-ids nil}       ; sweep all registered atoms
   {:atom-ids [:my-atom]} ; sweep specific atoms only (future extension)

   The sweep is idempotent -- calling twice in succession will not
   double-evict because entries evicted in the first pass are already gone."
  [{:keys [atom-ids]}]
  (try
    (let [result (if (seq atom-ids)
                   ;; Selective sweep: only specified atoms
                   ;; For now, sweep-all! sweeps everything; selective is a
                   ;; future extension. Log the intent.
                   (do
                     (log/debug "[lifecycle-fx] Selective sweep requested for:" atom-ids
                                "-- sweeping all (selective not yet implemented)")
                     (batom/sweep-all!))
                   ;; Full sweep
                   (batom/sweep-all!))]
      (reset! *last-sweep-result
              (assoc result :timestamp (java.time.Instant/now)))
      (when (pos? (:total-evicted result))
        (log/info "[lifecycle-fx] Sweep result:" (:total-evicted result) "evicted,"
                  (:atom-count result) "atoms swept in"
                  (:duration-ms result) "ms"))
      result)
    (catch Exception e
      (log/error "[lifecycle-fx] Sweep failed:" (.getMessage e))
      {:total-evicted 0
       :per-atom      []
       :duration-ms   0
       :atom-count    0
       :error         (.getMessage e)})))

;; =============================================================================
;; Query API
;; =============================================================================

(defn last-sweep-result
  "Return the last sweep result, or nil if no sweep has been run."
  []
  @*last-sweep-result)

;; =============================================================================
;; Registration
;; =============================================================================

(defn register-lifecycle-effects!
  "Register lifecycle effect handlers.

   Effects registered:
   - :lifecycle/sweep-fx - Execute bounded atom GC sweep

   Called from hive-mcp.events.effects/register-effects!"
  []
  (ev/reg-fx :lifecycle/sweep-fx handle-sweep-fx)
  (log/info "[hive-events.lifecycle] Lifecycle effects registered: :lifecycle/sweep-fx"))

;; =============================================================================
;; Testing Helpers
;; =============================================================================

(defn reset-state!
  "Reset lifecycle effect state. For testing only."
  []
  (reset! *last-sweep-result nil))
