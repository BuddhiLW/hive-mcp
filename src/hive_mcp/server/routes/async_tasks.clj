(ns hive-mcp.server.routes.async-tasks
  "Registry of in-flight async tool calls: bounded, inspectable, cancellable.

   Boundary layer. Decisions live in `async-task.state` (pure); shapes live in
   `async-task.schema`. This namespace only holds state and performs effects.

   Execution goes through `ITaskRunner`, so a caller may substitute the
   thread-backed runner. The default is backed by `hive-weave`: a fixed pool
   rather than unbounded `future` threads, and `safe-future-call` for a task
   that carries its own deadline.

   Rationale, incident and the alternative designs considered:
   memory 20260907-async-boundable (see kanban card for this work)."
  (:require [hive-dsl.bounded-atom :refer [bounded-atom bget bput! bkeys bclear!
                                           bounded-swap! register-sweepable!]]
            [hive-weave.pool :as pool]
            [hive-weave.safe :as safe]
            [hive-mcp.server.routes.async-task.state :as st]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent Future]))

;; =============================================================================
;; Execution port (DIP)
;; =============================================================================

(defprotocol ITaskRunner
  "How a task is started, stopped and observed.

   The seam exists so the lifecycle rules can be exercised without real
   concurrency, and so the execution strategy can change without touching the
   registry above it."
  (-run    [this f]      "Start f. Returns an opaque handle.")
  (-cancel [this handle] "Interrupt the handle's work. Returns true if this call stopped it.")
  (-view   [this handle] "Project the handle into a schema/HandleView."))

(def default-pool-size
  "Concurrent async tool calls; beyond this they QUEUE rather than run.

   These calls are long (an ingest runs for hours), so this bounds genuinely
   slow work rather than tuning throughput."
  8)

(def default-queue-capacity
  "Queued calls before the submitting thread runs the task itself.

   Reaching it turns an async call synchronous, which is correct backpressure
   and a bad surprise, so it should take a pathological backlog."
  256)

(defrecord WeavePoolRunner [pool]
  ITaskRunner
  (-run [_ f] (pool/submit! pool f))
  (-cancel [_ handle] (.cancel ^Future handle true))
  (-view [_ handle]
    (if (nil? handle)
      {:present? false :done? false :cancelled? false}
      {:present?   true
       :done?      (.isDone ^Future handle)
       :cancelled? (.isCancelled ^Future handle)})))

(defonce ^:private default-runner
  (delay (->WeavePoolRunner (pool/make-pool {:name           "mcp-async"
                                             :size           default-pool-size
                                             :queue-capacity default-queue-capacity}))))

(defonce ^{:doc "The active ITaskRunner. Rebind to substitute execution."}
  runner
  (atom nil))

(defn current-runner []
  (or @runner @default-runner))

;; =============================================================================
;; Registry
;; =============================================================================

(defonce ^{:doc "task-id -> entry. Bounded: a registry that only grows is a leak."}
  tasks
  (bounded-atom {:max-entries     500
                 :ttl-ms          1800000
                 :eviction-policy :lru}))
(register-sweepable! tasks :async-tasks)

(defn- entry-view
  "Registry row with its handle projected, which is what the pure layer takes."
  [task-id]
  (when-let [e (bget tasks task-id)]
    (-> e
        (assoc :handle (-view (current-runner) (:handle-obj e)))
        (dissoc :handle-obj))))

(defn get-task
  [task-id]
  (when-let [e (entry-view task-id)]
    (st/summarize task-id e (System/currentTimeMillis))))

(defn list-tasks
  []
  (let [now (System/currentTimeMillis)]
    (->> (bkeys tasks)
         (keep (fn [id] (when-let [e (entry-view id)] (st/summarize id e now))))
         (sort-by :elapsed-ms)
         vec)))

(defn running-tasks
  []
  (vec (remove (comp st/terminal? :state) (list-tasks))))

(defn- set-state!
  [task-id state]
  (when-let [e (bget tasks task-id)]
    (bput! tasks task-id (assoc e :state state))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn- bounded-body
  "Wrap `f` so a task carrying `timeout-ms` ends itself at the deadline.

   The wait happens on the POOL thread, never on the caller's: the caller has
   already been acknowledged. `safe-future-call` returns the timeout as a
   value, so the deadline needs no scheduler of its own."
  [task-id f timeout-ms]
  (if (and timeout-ms (pos-int? timeout-ms))
    (fn []
      (let [res (safe/safe-future-call {:timeout-ms timeout-ms :name task-id} f)]
        (when (= :weave/timeout (:error res))
          (log/warn "async task exceeded its bound" {:task-id task-id :timeout-ms timeout-ms})
          (set-state! task-id :task/timed-out))
        res))
    f))

(defn- complete-state!
  "Record that a task's body returned, without overwriting why it stopped.

   Atomic on purpose. A read-modify-write through bget/bput! loses a
   concurrent cancel: the body's `finally` can read :task/running, a cancel
   can then write :task/cancelled, and the `finally` overwrites it with
   :task/done, erasing the only record that someone stopped it. bounded-swap!
   sees the raw entry map, so the decision and the write are one step."
  [task-id]
  (bounded-swap! tasks
                 (fn [m]
                   (if (get m task-id)
                     (update-in m [task-id :data :state] st/completion-state)
                     m))))

(defn submit!
  "Run `f` under `task-id`, registering a cancellable handle. Returns the handle."
  [{:keys [task-id tool caller-id timeout-ms f]}]
  (bput! tasks task-id {:tool       tool
                        :caller-id  caller-id
                        :state      :task/queued
                        :started-at (System/currentTimeMillis)
                        :timeout-ms timeout-ms
                        :handle-obj nil})
  (let [body   (bounded-body task-id f timeout-ms)
        handle (-run (current-runner)
                     (fn []
                       (set-state! task-id :task/running)
                       (try
                         (body)
                         (finally
                           (complete-state! task-id)))))]
    (when-let [e (bget tasks task-id)]
      (bput! tasks task-id (assoc e :handle-obj handle)))
    handle))

(defn cancel!
  "Stop `task-id`. Returns an `AsyncCancelOutcome`; never throws for a stranger.

   Whether to act is decided by the pure layer, so a refusal is a value rather
   than a side effect that did nothing."
  [task-id]
  (let [outcome (st/cancel-outcome task-id (entry-view task-id))]
    (when (st/stops-the-work? outcome)
      (let [handle (:handle-obj (bget tasks task-id))
            ok?    (-cancel (current-runner) handle)]
        (set-state! task-id :task/cancelled)
        (log/info "async task cancelled" {:task-id task-id :interrupted? ok?})))
    outcome))

(defn forget!
  "Drop finished rows. Running ones are kept."
  []
  (let [gone (->> (bkeys tasks)
                  (filter #(some-> (entry-view %) st/collectable?))
                  vec)]
    ;; bounded-atom has no per-key delete, and `bput! nil` would store a nil
    ;; ROW rather than remove one. bounded-swap! sees the raw entry map, so
    ;; dissoc there is the actual removal.
    (when (seq gone)
      (bounded-swap! tasks #(apply dissoc % gone)))
    {:forgotten (count gone)}))

(defn reset-registry!
  "Clear every row. For tests and for a cold boot, not for operational use."
  []
  (bclear! tasks))
