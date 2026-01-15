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
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.events.core :as ev]
            [hive-mcp.agent :as agent]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close!]]
            [clojure.string :as str]
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
;;; Edit Registration and Dependency Inference
;;; =============================================================================

(defn- register-edits!
  "Register all plan items as edits in the logic database."
  [items]
  (doseq [{:keys [change-item/id change-item/file]} items]
    (logic/add-edit! id file :modify)))

(defn- infer-test-dependencies!
  "Infer dependencies between source and test files.

   Heuristics:
   - foo_test.clj depends on foo.clj
   - test/foo_test.clj depends on src/foo.clj"
  [items]
  (let [;; Build map: base-name → edit-id for source files
        source-map (->> items
                        (remove #(str/includes? (:change-item/file %) "_test"))
                        (reduce (fn [m {:keys [change-item/id change-item/file]}]
                                  (let [base (-> file
                                                 (str/replace #"^.*/src/" "")
                                                 (str/replace #"\.clj[sx]?$" ""))]
                                    (assoc m base id)))
                                {}))
        ;; Find test files
        test-items (filter #(str/includes? (:change-item/file %) "_test") items)]

    ;; Link test to source
    (doseq [{:keys [change-item/id change-item/file]} test-items]
      (let [base (-> file
                     (str/replace #"^.*/test/" "")
                     (str/replace #"_test\.clj[sx]?$" ""))]
        (when-let [source-id (get source-map base)]
          (logic/add-edit-dependency! source-id id)
          (log/debug "Inferred dependency:" source-id "→" id))))))

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
     cwd    - Optional working directory override for path resolution

   Returns:
     Map with :success :result or :error"
  [{:keys [change-item/id change-item/file change-item/task]} preset cwd]
  (try
    (log/info "Executing drone task for item:" id "file:" file "cwd:" cwd)
    ;; Update status to dispatched
    (ds/update-item-status! id :dispatched)

    ;; Delegate to drone
    (let [result (agent/delegate-drone!
                  {:task (str "File: " file "\n\nTask: " task)
                   :files [file]
                   :preset preset
                   :trace true
                   :cwd cwd})]
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
  [item preset cwd]
  {:item item :preset preset :cwd cwd})

(defn- execute-batch!
  "Execute a single batch of items with bounded concurrency.
   Returns channel that emits {:completed N :failed N} when batch is done."
  [batch-items preset cwd concurrency wave-id trace]
  (let [result-ch (chan)]
    (go
      (let [work-ch (chan)
            inner-result-ch (chan)]

        ;; Producer: push all items
        (go
          (doseq [item batch-items]
            (>! work-ch (item->work-unit item preset cwd)))
          (close! work-ch))

        ;; Workers: bounded concurrency
        (dotimes [_ (min concurrency (count batch-items))]
          (go-loop []
            (when-let [{:keys [item preset cwd]} (<! work-ch)]
              (let [result (execute-drone-task item preset cwd)]
                (if (:success result)
                  (do
                    (ds/update-item-status! (:item-id result) :completed
                                            {:result (str (:result result))})
                    (ds/update-wave-counts! wave-id {:completed 1 :active -1}))
                  (do
                    (ds/update-item-status! (:item-id result) :failed
                                            {:result (:error result)})
                    (ds/update-wave-counts! wave-id {:failed 1 :active -1})))

                (when trace
                  (ev/dispatch [:wave/item-done {:item-id (:item-id result)
                                                 :status (if (:success result) :completed :failed)
                                                 :wave-id wave-id}]))

                (>! inner-result-ch result))
              (recur))))

        ;; Collect results
        (loop [completed 0 failed 0 n 0]
          (if (< n (count batch-items))
            (let [r (<! inner-result-ch)]
              (recur (if (:success r) (inc completed) completed)
                     (if-not (:success r) (inc failed) failed)
                     (inc n)))
            (>! result-ch {:completed completed :failed failed})))))

    result-ch))

(defn execute-wave!
  "Execute a wave for a plan with bounded concurrency and conflict-aware batching.

   Arguments:
     plan-id     - Plan to execute
     opts        - Map with :concurrency (default 3), :trace (default true), :cwd (optional)

   Returns:
     Wave-id immediately. Wave executes asynchronously.
     Monitor via events or get-wave-status.

   Batching behavior:
     - Items editing the same file are placed in separate batches (conflict prevention)
     - Test files wait for their source files (dependency inference)
     - Batches execute sequentially; items within a batch execute in parallel"
  [plan-id & [{:keys [concurrency trace cwd] :or {concurrency default-concurrency trace true}}]]
  (let [plan (ds/get-plan plan-id)]
    (when-not plan
      (throw (ex-info "Plan not found" {:plan-id plan-id})))

    (let [items (get-pending-items plan-id)
          preset (:change-plan/preset plan)
          wave-id (ds/create-wave! plan-id {:concurrency concurrency})]

      ;; Update plan status
      (ds/update-plan-status! plan-id :in-progress)

      ;; 1. REGISTER EDITS in logic db
      (register-edits! items)
      (log/info "Registered" (count items) "edits for batch computation")

      ;; 2. INFER DEPENDENCIES (test → source)
      (infer-test-dependencies! items)

      ;; 3. COMPUTE SAFE BATCHES
      (let [edit-ids (mapv :change-item/id items)
            batches (logic/compute-batches edit-ids)
            item-map (into {} (map (juxt :change-item/id identity) items))]

        (log/info "Computed" (count batches) "batches for" (count items) "items")

        ;; Emit wave start event
        (when trace
          (ev/dispatch [:wave/start {:plan-id plan-id
                                     :wave-id wave-id
                                     :item-count (count items)
                                     :batch-count (count batches)}]))

        ;; 4. EXECUTE BATCH-BY-BATCH (sequential batches, parallel within)
        (go
          (loop [remaining-batches batches
                 total-completed 0
                 total-failed 0
                 batch-num 1]
            (if (empty? remaining-batches)
              ;; All batches done
              (let [final-status (if (pos? total-failed) :partial-failure :completed)]
                ;; 5. CLEANUP: Reset edits from logic db
                (logic/reset-edits!)
                (log/info "Wave complete. Batches:" (count batches)
                          "Completed:" total-completed "Failed:" total-failed)

                (ds/complete-wave! wave-id final-status)
                (ds/update-plan-status! plan-id (if (pos? total-failed) :failed :completed))

                (when trace
                  (ev/dispatch [:wave/complete {:plan-id plan-id
                                                :wave-id wave-id
                                                :results {:completed total-completed
                                                          :failed total-failed
                                                          :batches (count batches)}}])))

              ;; Execute current batch
              (let [batch (first remaining-batches)
                    batch-items (mapv #(get item-map %) batch)]

                (log/info "Executing batch" batch-num "of" (count batches)
                          "with" (count batch-items) "items")

                (when trace
                  (ev/dispatch [:wave/batch-start {:wave-id wave-id
                                                   :batch-num batch-num
                                                   :item-count (count batch-items)}]))

                ;; Execute batch and await completion
                (let [{:keys [completed failed]} (<! (execute-batch! batch-items preset cwd
                                                                     concurrency wave-id trace))]
                  (recur (rest remaining-batches)
                         (+ total-completed completed)
                         (+ total-failed failed)
                         (inc batch-num))))))))

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
     cwd     - Working directory override for path resolution (optional)

   Returns:
     JSON with wave-id for immediate response.
     Actual execution happens asynchronously."
  [{:keys [tasks preset trace cwd]}]
  (try
    (when (empty? tasks)
      (throw (ex-info "tasks array is required and must not be empty" {})))

    ;; Normalize task keys (MCP sends string keys)
    (let [normalized-tasks (mapv (fn [t]
                                   {:file (or (get t "file") (:file t))
                                    :task (or (get t "task") (:task t))})
                                 tasks)
          plan-id (create-plan! normalized-tasks preset)
          wave-id (execute-wave! plan-id {:trace (if (nil? trace) true trace)
                                          :cwd cwd})]
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
