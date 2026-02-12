(ns hive-mcp.tools.multi-async
  "Async batch dispatch for cross-tool operations.

   Wraps tools.multi/run-multi in a future, stores results in context-store.
   Enables non-blocking batch execution with collect-by-id semantics.

   Architecture:
     multi_async.clj (THIS — async wrapper)
       -> tools/multi.clj (batch engine)
       -> channel/context_store.clj (result storage)
       -> dns/result.clj (error handling)

   Lifecycle:
     1. run-multi-async: launch future, return batch-id immediately
     2. collect-async-result: poll for result by batch-id
     3. list-async-batches: enumerate active batches
     4. cancel-async-batch: future-cancel a running batch

   Design decision: Phase 3 of Multi DSL plan (20260211212019-b8499942)"
  (:require [hive-mcp.channel.context-store :as ctx]
            [hive-mcp.dns.result :as result]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const default-result-ttl-ms
  "Default TTL for async batch results in context-store (10 minutes)."
  600000)

;; =============================================================================
;; Batch Registry (in-process state for future lifecycle management)
;; =============================================================================
;;
;; Futures cannot be serialized into context-store, so the registry holds
;; the future reference + metadata locally. Completed results are stored
;; in context-store for cross-agent access and TTL-based auto-eviction.

(defonce ^{:doc "Registry of async batches.
  {batch-id -> {:future, :status, :ops, :created-at, :result-ctx-id, :error}}"}
  batches
  (atom {}))

;; =============================================================================
;; ID Generation
;; =============================================================================

(defn- generate-batch-id
  "Generate a unique batch ID: batch-<uuid>."
  []
  (str "batch-" (java.util.UUID/randomUUID)))

;; =============================================================================
;; Lazy Resolution (avoid circular deps with tools.multi)
;; =============================================================================

(defn- resolve-run-multi
  "Lazily resolve hive-mcp.tools.multi/run-multi.
   Returns the function or nil if unavailable."
  []
  (try
    (requiring-resolve 'hive-mcp.tools.multi/run-multi)
    (catch Exception e
      (log/error {:event :async-resolve-error :error (ex-message e)})
      nil)))

;; =============================================================================
;; Internal Helpers
;; =============================================================================

(defn- now-ms
  "Current epoch milliseconds."
  []
  (System/currentTimeMillis))

(defn- make-batch-entry
  "Build a batch registry entry for a newly dispatched batch."
  [fut ops-count created-at]
  {:future        fut
   :status        "running"
   :ops           ops-count
   :created-at    created-at
   :completed-at  nil
   :result-ctx-id nil
   :error         nil})

(defn- complete-batch!
  "Mark a batch as completed and store result in context-store.
   Called from within the batch future on successful completion."
  [batch-id batch-result ttl-ms]
  (result/try-effect* :multi-async/store-failed
                      (let [ctx-id (ctx/context-put! batch-result
                                                     :tags #{"async-batch" (str "batch:" batch-id)}
                                                     :ttl-ms ttl-ms)]
                        (swap! batches update batch-id assoc
                               :status        "completed"
                               :result-ctx-id ctx-id
                               :completed-at  (now-ms))
                        (log/info "[multi-async] Batch completed" batch-id "ctx-id:" ctx-id)
                        ctx-id)))

(defn- fail-batch!
  "Mark a batch as failed with error details.
   Called from within the batch future on exception."
  [batch-id error-msg]
  (swap! batches update batch-id assoc
         :status       "failed"
         :error        error-msg
         :completed-at (now-ms))
  (log/error "[multi-async] Batch failed" batch-id "error:" error-msg))

;; =============================================================================
;; Public API: Dispatch
;; =============================================================================

(defn run-multi-async
  "Execute a vector of cross-tool operations asynchronously.
   Wraps tools.multi/run-multi in a future, stores result in context-store.

   Returns immediately with batch descriptor:
   {:batch-id   \"batch-<uuid>\"
    :status     \"running\"
    :ops        N
    :created-at <epoch-ms>}

   On engine resolution failure, returns Result error:
   {:error :multi-async/engine-unavailable ...}

   Options:
   - :ttl-ms    — TTL for result in context-store (default: 600000 = 10 min)
   - :dry-run   — validate and plan only, don't execute"
  [ops & {:keys [ttl-ms dry-run] :or {ttl-ms default-result-ttl-ms}}]
  (if-let [run-multi-fn (resolve-run-multi)]
    (let [batch-id   (generate-batch-id)
          created-at (now-ms)
          ops-count  (count ops)
          fut        (future
                       (try
                         (let [r (if dry-run
                                   (run-multi-fn ops :dry-run true)
                                   (run-multi-fn ops))]
                           (complete-batch! batch-id r ttl-ms)
                           r)
                         (catch Exception e
                           ;; Only mark as failed if not already cancelled
                           ;; (future-cancel triggers InterruptedException which
                           ;;  would overwrite "cancelled" status without this guard)
                           (when (= "running" (:status (get @batches batch-id)))
                             (fail-batch! batch-id (ex-message e)))
                           nil)))]
      (swap! batches assoc batch-id (make-batch-entry fut ops-count created-at))
      (log/info "[multi-async] Batch dispatched" batch-id "ops:" ops-count)
      {:batch-id   batch-id
       :status     "running"
       :ops        ops-count
       :created-at created-at})
    ;; Engine not available
    (result/err :multi-async/engine-unavailable
                {:message "Batch engine not available (hive-mcp.tools.multi/run-multi not resolved)"})))

;; =============================================================================
;; Public API: Collect
;; =============================================================================

(defn collect-async-result
  "Retrieve the result of an async batch by batch-id.

   Returns one of:
   {:status \"running\"   :batch-id ... :ops N :elapsed-ms M}
   {:status \"completed\" :batch-id ... :results <run-multi-output>}
   {:status \"failed\"    :batch-id ... :error \"...\"}
   {:status \"cancelled\" :batch-id ...}
   {:status \"expired\"   :batch-id ... :message \"...\"}
   {:status \"not-found\" :batch-id ...}"
  [batch-id]
  (if-let [entry (get @batches batch-id)]
    (case (:status entry)
      "running"
      {:status     "running"
       :batch-id   batch-id
       :ops        (:ops entry)
       :created-at (:created-at entry)
       :elapsed-ms (- (now-ms) (:created-at entry))}

      "completed"
      (if-let [ctx-entry (ctx/context-get (:result-ctx-id entry))]
        {:status   "completed"
         :batch-id batch-id
         :results  (:data ctx-entry)}
        {:status  "expired"
         :batch-id batch-id
         :message "Result TTL exceeded, data evicted from context-store"})

      "failed"
      {:status   "failed"
       :batch-id batch-id
       :error    (:error entry)}

      "cancelled"
      {:status   "cancelled"
       :batch-id batch-id}

      ;; Unknown status fallback
      {:status     "unknown"
       :batch-id   batch-id
       :raw-status (:status entry)})

    ;; Batch-id not in registry
    {:status "not-found" :batch-id batch-id}))

;; =============================================================================
;; Public API: List
;; =============================================================================

(defn list-async-batches
  "List all tracked async batches with their status.
   Returns a vector of batch summary maps (without :future refs)."
  []
  (mapv (fn [[batch-id {:keys [status ops created-at completed-at error]}]]
          (cond-> {:batch-id   batch-id
                   :status     status
                   :ops        ops
                   :created-at created-at}
            completed-at      (assoc :completed-at completed-at)
            (= "running" status) (assoc :elapsed-ms (- (now-ms) created-at))
            error             (assoc :error error)))
        @batches))

;; =============================================================================
;; Public API: Cancel
;; =============================================================================

(defn cancel-async-batch
  "Cancel a running async batch via future-cancel.

   Returns:
   {:cancelled true  :batch-id ...}              — successfully cancelled
   {:cancelled false :batch-id ... :reason \"not-running\"} — terminal state
   {:cancelled false :batch-id ... :reason \"not-found\"}   — invalid id"
  [batch-id]
  (if-let [entry (get @batches batch-id)]
    (if (= "running" (:status entry))
      (do
        (future-cancel (:future entry))
        (swap! batches update batch-id assoc
               :status       "cancelled"
               :completed-at (now-ms))
        (log/info "[multi-async] Batch cancelled" batch-id)
        {:cancelled true :batch-id batch-id})
      {:cancelled false :batch-id batch-id :reason "not-running"})
    {:cancelled false :batch-id batch-id :reason "not-found"}))

;; =============================================================================
;; Lifecycle: Cleanup & GC
;; =============================================================================

(defn gc-completed!
  "Remove terminal batches (completed/failed/cancelled) whose results have
   expired from context-store. Returns count of entries purged from registry."
  []
  (let [terminal #{"completed" "failed" "cancelled"}
        to-purge (filterv (fn [[_id entry]]
                            (and (terminal (:status entry))
                                 (if-let [ctx-id (:result-ctx-id entry)]
                                   (nil? (ctx/context-get ctx-id))
                                   true)))
                          @batches)
        ids      (mapv first to-purge)]
    (when (seq ids)
      (swap! batches #(apply dissoc % ids))
      (log/info "[multi-async] GC purged" (count ids) "terminal batches"))
    (count ids)))

(defn reset-all!
  "Cancel all in-flight futures and clear batch registry. For testing."
  []
  (doseq [[_ {:keys [future]}] @batches]
    (when (and future (not (future-done? future)))
      (future-cancel future)
      (try (deref future 100 nil) (catch Exception _))))
  (reset! batches {}))
