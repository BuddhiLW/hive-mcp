(ns hive-mcp.tools.swarm.wave.batching
  "Batch computation and bounded-concurrency execution for wave operations."
  (:require [hive-mcp.tools.swarm.wave.domain :as domain]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.events.core :as ev]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close!]]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn register-edits!
  "Register all plan items as edits in the logic database."
  [items]
  (doseq [{:keys [change-item/id change-item/file]} items]
    (logic/add-edit! id file :modify))
  (count items))

(defn reset-edits!
  "Reset the logic database edits before a new wave."
  []
  (logic/reset-edits!))

(defn infer-test-dependencies!
  "Infer dependencies between source and test files."
  [items]
  (let [source-map (->> items
                        (remove #(str/includes? (:change-item/file %) "_test"))
                        (reduce (fn [m {:keys [change-item/id change-item/file]}]
                                  (let [base (-> file
                                                 (str/replace #"^.*/src/" "")
                                                 (str/replace #"\.clj[sx]?$" ""))]
                                    (assoc m base id)))
                                {}))
        test-items (filter #(str/includes? (:change-item/file %) "_test") items)
        inferred (atom 0)]

    (doseq [{:keys [change-item/id change-item/file]} test-items]
      (let [base (-> file
                     (str/replace #"^.*/test/" "")
                     (str/replace #"_test\.clj[sx]?$" ""))]
        (when-let [source-id (get source-map base)]
          (logic/add-edit-dependency! source-id id)
          (swap! inferred inc)
          (log/debug "Inferred dependency:" source-id "->" id))))

    @inferred))

(defn compute-batches
  "Compute conflict-free batches from items."
  [edit-ids]
  (if (empty? edit-ids)
    []
    (let [batches (logic/compute-batches edit-ids)]
      (if (empty? batches)
        [edit-ids]
        batches))))

(defn prepare-batches
  "Prepare items for batch execution."
  [items]
  (let [_ (reset-edits!)
        _ (register-edits! items)
        deps-count (infer-test-dependencies! items)

        edit-ids (mapv :change-item/id items)
        batches (compute-batches edit-ids)
        item-map (into {} (map (juxt :change-item/id identity) items))]

    (log/info "Prepared batches:"
              {:item-count (count items)
               :batch-count (count batches)
               :dependencies-inferred deps-count})

    {:batches batches
     :item-map item-map
     :batch-count (count batches)}))

(defn item->work-unit
  "Convert item to work unit for async processing."
  [item batch-spec]
  {:item item
   :preset (:preset batch-spec)
   :cwd (:cwd batch-spec)
   :skip-auto-apply (:skip-auto-apply batch-spec)
   :wave-id (:wave-id batch-spec)})

(defn blocking-take-with-keepalive!
  "Block on channel take with periodic keepalive messages to prevent nREPL timeout."
  [ch interval-ms progress-fn]
  (loop []
    (let [timeout-ch (async/timeout interval-ms)
          [result port] (async/alts!! [ch timeout-ch])]
      (if (= port ch)
        result
        (do
          (println (progress-fn))
          (flush)
          (recur))))))

(defn spawn-workers!
  "Spawn worker threads for bounded concurrency using real threads for blocking I/O."
  [work-ch result-ch execute-fn concurrency item-count]
  (dotimes [_ (min concurrency item-count)]
    (async/thread
      (loop []
        (when-let [work-unit (async/<!! work-ch)]
          (let [{:keys [item]} work-unit
                item-id (:change-item/id item)
                exec-result (try
                              (execute-fn work-unit)
                              (catch Throwable e
                                (log/error {:event :wave/worker-exception
                                            :item-id item-id
                                            :file (:change-item/file item)
                                            :wave-id (:wave-id work-unit)
                                            :error-type :uncaught-exception
                                            :exception-class (.getName (class e))
                                            :message (.getMessage e)})
                                (domain/failure-result
                                 item-id
                                 (str "Worker exception: " (.getMessage e)))))]
            (async/>!! result-ch exec-result))
          (recur))))))

(defn collect-results
  "Collect results from worker channels."
  [result-ch item-count]
  (go
    (loop [completed 0
           failed 0
           results []
           n 0]
      (if (< n item-count)
        (let [r (<! result-ch)]
          (recur (if (:success r) (inc completed) completed)
                 (if-not (:success r) (inc failed) failed)
                 (conj results r)
                 (inc n)))
        {:completed completed
         :failed failed
         :results results}))))

(defn execute-batch!
  "Execute a single batch of items with bounded concurrency."
  [batch-spec execute-fn]
  (let [{:keys [items]} batch-spec
        concurrency 3
        item-count (count items)
        result-ch (chan)]

    (async/thread
      (let [work-ch (chan)
            inner-result-ch (chan)]

        (async/thread
          (doseq [item items]
            (async/>!! work-ch (item->work-unit item batch-spec)))
          (close! work-ch))

        (spawn-workers! work-ch inner-result-ch execute-fn concurrency item-count)

        (let [batch-result (async/<!! (collect-results inner-result-ch item-count))]
          (async/>!! result-ch batch-result))))

    result-ch))

(defn execute-batch-blocking!
  "Execute a batch with blocking wait and keepalive."
  [batch-spec execute-fn]
  (blocking-take-with-keepalive!
   (execute-batch! batch-spec execute-fn)
   domain/keepalive-interval-ms
   #(format "[wave:%s] batch %d/%d in progress (%d items)..."
            (:wave-id batch-spec)
            (:batch-num batch-spec)
            (:total-batches batch-spec)
            (count (:items batch-spec)))))

(defn execute-all-batches!
  "Execute all batches sequentially, items within batches in parallel."
  [batches item-map wave-spec wave-id execute-fn]
  (let [{:keys [preset cwd skip-auto-apply trace]} wave-spec
        total-batches (count batches)]

    (loop [remaining batches
           total-completed 0
           total-failed 0
           batch-results []
           batch-num 1]

      (if (empty? remaining)
        {:total-completed total-completed
         :total-failed total-failed
         :batch-results batch-results}

        (let [batch (first remaining)
              batch-items (mapv #(get item-map %) batch)
              batch-spec (domain/->batch-spec
                          {:items batch-items
                           :preset preset
                           :cwd cwd
                           :wave-id wave-id
                           :skip-auto-apply skip-auto-apply
                           :trace trace
                           :batch-num batch-num
                           :total-batches total-batches})]

          (log/info "Executing batch" batch-num "of" total-batches
                    "with" (count batch-items) "items")

          (when trace
            (ev/dispatch [:wave/batch-start {:wave-id wave-id
                                             :batch-num batch-num
                                             :item-count (count batch-items)}]))

          (let [{:keys [completed failed] :as result}
                (execute-batch-blocking! batch-spec execute-fn)]

            (recur (rest remaining)
                   (+ total-completed completed)
                   (+ total-failed failed)
                   (conj batch-results result)
                   (inc batch-num))))))))
