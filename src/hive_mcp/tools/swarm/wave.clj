(ns hive-mcp.tools.swarm.wave
  "Wave execution for batch drone dispatch.

   Executes multiple drone tasks in parallel with bounded concurrency.
   Integrates with DataScript for state tracking and hive-events for
   lifecycle events.

   Usage:
   1. Create plan with tasks
   2. Execute wave (async, bounded concurrency)
   3. Monitor via events or get-wave-status

   SOLID: SRP - Wave orchestration only
   CLARITY: A - Architectural performance via bounded concurrency"
  (:require [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.events.core :as ev]
            [hive-mcp.agent :as agent]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close!]]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]))

;;; =============================================================================
;;; Constants
;;; =============================================================================

(def ^:const default-concurrency
  "Default max concurrent drones."
  3)

(def ^:const drone-timeout-ms
  "Timeout for individual drone execution (10 minutes)."
  600000)

;;; =============================================================================
;;; Plan Management (delegates to datascript)
;;; =============================================================================

(defn create-plan!
  "Create a change plan with multiple tasks.

   Arguments:
     tasks  - Collection of {:file \"path\" :task \"description\"}
     preset - Optional drone preset (default: \"drone-worker\")

   Returns:
     The generated plan-id"
  [tasks & [preset]]
  (ds/create-plan! tasks (or preset "drone-worker")))

(defn get-pending-items
  "Get pending items for a plan."
  [plan-id]
  (ds/get-pending-items plan-id))

(defn get-plan-items
  "Get all items for a plan."
  [plan-id]
  (ds/get-plan-items plan-id))

;;; =============================================================================
;;; Drone Execution
;;; =============================================================================

(defn- execute-drone-task
  "Execute a single drone task.

   Arguments:
     item   - Change item map
     preset - Drone preset

   Returns:
     Map with :success :result or :error"
  [{:keys [change-item/id change-item/file change-item/task]} preset]
  (try
    (log/info "Executing drone task for item:" id "file:" file)
    ;; Update status to dispatched
    (ds/update-item-status! id :dispatched)

    ;; Delegate to drone
    (let [result (agent/delegate-drone!
                  {:task (str "File: " file "\n\nTask: " task)
                   :files [file]
                   :preset preset
                   :trace true})]
      (if (= :success (:status result))
        {:success true
         :item-id id
         :result (:result result)}
        {:success false
         :item-id id
         :error (or (:error result) "Drone execution failed")}))
    (catch Exception e
      (log/error e "Drone task failed for item:" id)
      {:success false
       :item-id id
       :error (.getMessage e)})))

;;; =============================================================================
;;; Wave Execution (core.async bounded concurrency)
;;; =============================================================================

(defn- item->work-unit
  "Convert item to work unit for async processing."
  [item preset]
  {:item item :preset preset})

(defn execute-wave!
  "Execute a wave for a plan with bounded concurrency.

   Arguments:
     plan-id     - Plan to execute
     opts        - Map with :concurrency (default 3), :trace (default true)

   Returns:
     Wave-id immediately. Wave executes asynchronously.
     Monitor via events or get-wave-status."
  [plan-id & [{:keys [concurrency trace] :or {concurrency default-concurrency trace true}}]]
  (let [plan (ds/get-plan plan-id)]
    (when-not plan
      (throw (ex-info "Plan not found" {:plan-id plan-id})))

    (let [items (get-pending-items plan-id)
          preset (:change-plan/preset plan)
          wave-id (ds/create-wave! plan-id {:concurrency concurrency})]

      ;; Update plan status
      (ds/update-plan-status! plan-id :in-progress)

      ;; Emit wave start event
      (when trace
        (ev/dispatch [:wave/start {:plan-id plan-id
                                   :wave-id wave-id
                                   :item-count (count items)}]))

      ;; Execute items with bounded concurrency using core.async
      (go
        (let [work-ch (chan)
              result-ch (chan)]

          ;; Producer: push all items to work channel
          (go
            (doseq [item items]
              (>! work-ch (item->work-unit item preset)))
            (close! work-ch))

          ;; Workers: process with bounded concurrency
          (dotimes [_ concurrency]
            (go-loop []
              (when-let [{:keys [item preset]} (<! work-ch)]
                (let [result (execute-drone-task item preset)]
                  ;; Update item status
                  (if (:success result)
                    (do
                      (ds/update-item-status! (:item-id result) :completed
                                              {:result (str (:result result))})
                      (ds/update-wave-counts! wave-id {:completed 1 :active -1}))
                    (do
                      (ds/update-item-status! (:item-id result) :failed
                                              {:result (:error result)})
                      (ds/update-wave-counts! wave-id {:failed 1 :active -1})))

                  ;; Emit item done event
                  (when trace
                    (ev/dispatch [:wave/item-done {:item-id (:item-id result)
                                                   :status (if (:success result) :completed :failed)
                                                   :wave-id wave-id}]))

                  (>! result-ch result))
                (recur))))

          ;; Collector: wait for all results
          (let [total (count items)]
            (loop [completed 0
                   failed 0
                   n 0]
              (if (< n total)
                (let [result (<! result-ch)]
                  (recur (if (:success result) (inc completed) completed)
                         (if-not (:success result) (inc failed) failed)
                         (inc n)))
                ;; All done
                (let [final-status (if (pos? failed) :partial-failure :completed)]
                  (ds/complete-wave! wave-id final-status)
                  (ds/update-plan-status! plan-id (if (pos? failed) :failed :completed))

                  ;; Emit wave complete event
                  (when trace
                    (ev/dispatch [:wave/complete {:plan-id plan-id
                                                  :wave-id wave-id
                                                  :results {:completed completed
                                                            :failed failed}}]))))))))

      ;; Return wave-id immediately
      wave-id)))

;;; =============================================================================
;;; Status Queries
;;; =============================================================================

(defn get-wave-status
  "Get current status of a wave execution.

   Returns map with:
     :wave-id :plan-id :status :active-count :completed-count :failed-count"
  [wave-id]
  (when-let [wave (ds/get-wave wave-id)]
    {:wave-id wave-id
     :plan-id (:wave/plan wave)
     :status (:wave/status wave)
     :active-count (:wave/active-count wave)
     :completed-count (:wave/completed-count wave)
     :failed-count (:wave/failed-count wave)
     :started-at (:wave/started-at wave)
     :completed-at (:wave/completed-at wave)}))

(defn get-plan-status
  "Get current status of a change plan.

   Returns map with plan info and all items."
  [plan-id]
  (when-let [plan (ds/get-plan plan-id)]
    {:plan-id plan-id
     :status (:change-plan/status plan)
     :preset (:change-plan/preset plan)
     :items (mapv (fn [item]
                    {:item-id (:change-item/id item)
                     :file (:change-item/file item)
                     :status (:change-item/status item)
                     :result (:change-item/result item)})
                  (get-plan-items plan-id))}))

;;; =============================================================================
;;; MCP Handler
;;; =============================================================================

(defn handle-dispatch-drone-wave
  "Handle dispatch_drone_wave MCP tool call.

   Parameters:
     tasks   - Array of {:file :task} objects (required)
     preset  - Drone preset (default: \"drone-worker\")
     trace   - Emit progress events (default: true)

   Returns:
     JSON with wave-id for immediate response.
     Actual execution happens asynchronously."
  [{:keys [tasks preset trace]}]
  (try
    (when (empty? tasks)
      (throw (ex-info "tasks array is required and must not be empty" {})))

    ;; Normalize task keys (MCP sends string keys)
    (let [normalized-tasks (mapv (fn [t]
                                   {:file (or (get t "file") (:file t))
                                    :task (or (get t "task") (:task t))})
                                 tasks)
          plan-id (create-plan! normalized-tasks preset)
          wave-id (execute-wave! plan-id {:trace (if (nil? trace) true trace)})]
      {:type "text"
       :text (json/write-str {:status "wave_started"
                              :plan_id plan-id
                              :wave_id wave-id
                              :item_count (count tasks)
                              :message "Wave execution started. Monitor via HIVEMIND piggyback or get_wave_status."})})
    (catch Exception e
      (log/error e "dispatch_drone_wave failed")
      {:type "text"
       :text (json/write-str {:error (.getMessage e)})})))
