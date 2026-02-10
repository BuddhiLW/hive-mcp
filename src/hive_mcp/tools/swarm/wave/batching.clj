(ns hive-mcp.tools.swarm.wave.batching
  "Batch computation and execution for wave operations.

   Responsibilities:
   - Edit registration in logic database
   - Test dependency inference (source → test)
   - Batch computation (conflict-aware)
   - core.async bounded concurrency execution

   SOLID-S: Single responsibility - batching logic only.
   CLARITY-A: Architectural performance via bounded concurrency."
  (:require [hive-mcp.tools.swarm.wave.domain :as domain]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.events.core :as ev]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close!]]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Edit Registration
;;; ============================================================

(defn register-edits!
  "Register all plan items as edits in the logic database.

   Arguments:
     items - Collection of change items with :change-item/id and :change-item/file

   Returns:
     Number of edits registered."
  [items]
  (doseq [{:keys [change-item/id change-item/file]} items]
    (logic/add-edit! id file :modify))
  (count items))

(defn reset-edits!
  "Reset the logic database edits before a new wave.
   CRITICAL: Prevents stale edits from previous waves."
  []
  (logic/reset-edits!))

;;; ============================================================
;;; Dependency Inference
;;; ============================================================

(defn infer-test-dependencies!
  "Infer dependencies between source and test files.

   Heuristics:
   - foo_test.clj depends on foo.clj
   - test/foo_test.clj depends on src/foo.clj

   Arguments:
     items - Collection of change items

   Returns:
     Number of dependencies inferred."
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
        test-items (filter #(str/includes? (:change-item/file %) "_test") items)
        inferred (atom 0)]

    ;; Link test to source
    (doseq [{:keys [change-item/id change-item/file]} test-items]
      (let [base (-> file
                     (str/replace #"^.*/test/" "")
                     (str/replace #"_test\.clj[sx]?$" ""))]
        (when-let [source-id (get source-map base)]
          (logic/add-edit-dependency! source-id id)
          (swap! inferred inc)
          (log/debug "Inferred dependency:" source-id "→" id))))

    @inferred))

;;; ============================================================
;;; Batch Computation
;;; ============================================================

(defn compute-batches
  "Compute conflict-free batches from items.

   Items editing the same file are placed in separate batches.
   Test files wait for their source files (via dependency inference).

   Arguments:
     edit-ids - Vector of edit IDs

   Returns:
     Vector of vectors, each containing non-conflicting edit IDs."
  [edit-ids]
  (if (empty? edit-ids)
    []
    (let [batches (logic/compute-batches edit-ids)]
      ;; Fallback: if batching returns empty, treat all as single batch
      (if (empty? batches)
        [edit-ids]
        batches))))

(defn prepare-batches
  "Prepare items for batch execution.

   Arguments:
     items - Collection of change items

   Returns:
     Map with :batches :item-map :batch-count"
  [items]
  (let [;; Reset and register
        _ (reset-edits!)
        _ (register-edits! items)
        deps-count (infer-test-dependencies! items)

        ;; Compute batches
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

;;; ============================================================
;;; Work Unit Conversion
;;; ============================================================

(defn item->work-unit
  "Convert item to work unit for async processing.

   Arguments:
     item            - Change item map
     batch-spec      - BatchSpec record

   Returns:
     Map suitable for worker channel."
  [item batch-spec]
  {:item item
   :preset (:preset batch-spec)
   :cwd (:cwd batch-spec)
   :skip-auto-apply (:skip-auto-apply batch-spec)
   :wave-id (:wave-id batch-spec)})

;;; ============================================================
;;; nREPL Keepalive
;;; ============================================================

(defn blocking-take-with-keepalive!
  "Block on channel take with periodic keepalive messages to *out*.
   Prevents nREPL socket timeout by emitting progress every interval-ms.

   Arguments:
     ch          - core.async channel to take from
     interval-ms - Milliseconds between keepalive messages
     progress-fn - Zero-arg function that returns a progress string

   Returns:
     The value taken from the channel.

   CLARITY-T: Progress visibility for long-running wave operations.
   Without this, bb-mcp's nREPL socket times out after 30s of silence."
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

;;; ============================================================
;;; Worker Spawning
;;; ============================================================

(defn spawn-workers!
  "Spawn worker threads for bounded concurrency.

   Uses real threads (not go blocks) because execute-fn does blocking I/O
   (HTTP calls to OpenRouter/hive-agent). Blocking in go blocks exhausts
   the core.async fixed thread pool (8 threads) and deadlocks batch transitions.

   Arguments:
     work-ch     - Channel providing work units
     result-ch   - Channel to send results to
     execute-fn  - Function to execute each work unit
     concurrency - Max concurrent workers
     item-count  - Number of items (for worker count calculation)

   Returns:
     nil (side-effectful - spawns threads)"
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

;;; ============================================================
;;; Result Collection
;;; ============================================================

(defn collect-results
  "Collect results from worker channels.

   Arguments:
     result-ch  - Channel receiving results
     item-count - Expected number of results

   Returns:
     Channel that emits {:completed N :failed N :results [...]}."
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

;;; ============================================================
;;; Batch Execution
;;; ============================================================

(defn execute-batch!
  "Execute a single batch of items with bounded concurrency.

   Uses async/thread for the orchestrator since workers do blocking I/O.
   The work-ch producer uses a thread too to avoid go-block starvation.

   Arguments:
     batch-spec - BatchSpec record
     execute-fn - Function to execute each item (receives work-unit)

   Returns:
     Channel that emits {:completed N :failed N :results [...]}."
  [batch-spec execute-fn]
  (let [{:keys [items]} batch-spec
        concurrency 3  ;; Default concurrency for batch
        item-count (count items)
        result-ch (chan)]

    (async/thread
      (let [work-ch (chan)
            inner-result-ch (chan)]

        ;; Producer: push all items to work channel (thread-safe)
        (async/thread
          (doseq [item items]
            (async/>!! work-ch (item->work-unit item batch-spec)))
          (close! work-ch))

        ;; Spawn workers (already uses threads)
        (spawn-workers! work-ch inner-result-ch execute-fn concurrency item-count)

        ;; Collect results — use blocking take on the go-based collector
        (let [batch-result (async/<!! (collect-results inner-result-ch item-count))]
          (async/>!! result-ch batch-result))))

    result-ch))

(defn execute-batch-blocking!
  "Execute a batch with blocking wait and keepalive.

   Arguments:
     batch-spec - BatchSpec record
     execute-fn - Function to execute each item

   Returns:
     Map with :completed :failed :results"
  [batch-spec execute-fn]
  (blocking-take-with-keepalive!
   (execute-batch! batch-spec execute-fn)
   domain/keepalive-interval-ms
   #(format "[wave:%s] batch %d/%d in progress (%d items)..."
            (:wave-id batch-spec)
            (:batch-num batch-spec)
            (:total-batches batch-spec)
            (count (:items batch-spec)))))

;;; ============================================================
;;; Batch Iteration
;;; ============================================================

(defn execute-all-batches!
  "Execute all batches sequentially, items within batches in parallel.

   Arguments:
     batches    - Vector of edit-id vectors (from compute-batches)
     item-map   - Map of edit-id → change item
     wave-spec  - WaveSpec record
     wave-id    - Wave identifier
     execute-fn - Function to execute each item

   Returns:
     Map with :total-completed :total-failed :batch-results"
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

          ;; Emit batch start event
          (when trace
            (ev/dispatch [:wave/batch-start {:wave-id wave-id
                                             :batch-num batch-num
                                             :item-count (count batch-items)}]))

          ;; Execute batch (blocking)
          (let [{:keys [completed failed] :as result}
                (execute-batch-blocking! batch-spec execute-fn)]

            (recur (rest remaining)
                   (+ total-completed completed)
                   (+ total-failed failed)
                   (conj batch-results result)
                   (inc batch-num))))))))
