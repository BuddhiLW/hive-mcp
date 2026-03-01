(ns hive-mcp.telemetry.bounded-atom-metrics
  "Prometheus metrics for bounded atoms (gc-fix-7).

   Provides observability into bounded atom sizes, eviction rates,
   and sweep stats exposed via the existing Prometheus infrastructure.

   Metrics:
   - Gauge:     hive_bounded_atom_entries{name=\"X\"}     — current entry count
   - Gauge:     hive_bounded_atom_capacity{name=\"X\"}    — max-entries config
   - Counter:   hive_bounded_atom_evictions_total{name=\"X\", reason=\"ttl|capacity\"}
   - Gauge:     hive_bounded_atom_utilization{name=\"X\"} — entries/capacity (0.0-1.0)
   - Counter:   hive_lifecycle_sweep_total                — number of sweeps run
   - Gauge:     hive_lifecycle_sweep_last_evicted         — evictions in last sweep
   - Histogram: hive_lifecycle_sweep_duration_seconds     — sweep latency

   Usage:
     (require '[hive-mcp.telemetry.bounded-atom-metrics :as bam])

     ;; Create an instrumented bounded atom
     (def store (bam/instrumented-bounded-atom
                  {:max-entries 1000 :ttl-ms 300000 :name \"task-cache\"}))

     ;; Or instrument an existing one
     (bam/instrument! my-existing-batom \"my-store\")

     ;; Metrics auto-update on bput!/sweep! via callbacks"
  (:require [iapetos.core :as prometheus]
            [iapetos.export :as export]
            [hive-dsl.bounded-atom :as ba]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Metric names (following existing convention: namespace-qualified keywords)
;; =============================================================================

(def ^:private entries-name       :hive-bounded-atom/entries)
(def ^:private capacity-name      :hive-bounded-atom/capacity)
(def ^:private evictions-name     :hive-bounded-atom/evictions-total)
(def ^:private utilization-name   :hive-bounded-atom/utilization)
(def ^:private sweep-total-name   :hive-lifecycle/sweep-total)
(def ^:private sweep-evicted-name :hive-lifecycle/sweep-last-evicted)
(def ^:private sweep-duration-name :hive-lifecycle/sweep-duration-seconds)

;; =============================================================================
;; Collector definitions
;; =============================================================================

(def ^:private entries-collector
  (prometheus/gauge
   entries-name
   {:description "Current entry count per bounded atom"
    :labels [:name]}))

(def ^:private capacity-collector
  (prometheus/gauge
   capacity-name
   {:description "Max entries capacity per bounded atom"
    :labels [:name]}))

(def ^:private evictions-collector
  (prometheus/counter
   evictions-name
   {:description "Cumulative evictions per bounded atom by reason"
    :labels [:name :reason]}))

(def ^:private utilization-collector
  (prometheus/gauge
   utilization-name
   {:description "Utilization ratio (entries/capacity) per bounded atom, 0.0-1.0"
    :labels [:name]}))

(def ^:private sweep-total-collector
  (prometheus/counter
   sweep-total-name
   {:description "Total number of sweeps executed"}))

(def ^:private sweep-evicted-collector
  (prometheus/gauge
   sweep-evicted-name
   {:description "Number of entries evicted in the last sweep"}))

(def ^:private sweep-duration-collector
  (prometheus/histogram
   sweep-duration-name
   {:description "Sweep execution duration distribution"
    :buckets [0.0001 0.0005 0.001 0.005 0.01 0.05 0.1 0.5 1.0]}))

;; =============================================================================
;; Registry — separate from main prometheus registry to avoid coupling
;; =============================================================================

(defonce registry
  (delay
    (-> (prometheus/collector-registry)
        (prometheus/register
         entries-collector
         capacity-collector
         evictions-collector
         utilization-collector
         sweep-total-collector
         sweep-evicted-collector
         sweep-duration-collector))))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- reason->str
  "Convert a reason keyword to a Prometheus-safe label string.
   :capacity -> \"capacity\", :ttl -> \"ttl\", nil -> \"unknown\"."
  [reason]
  (if (keyword? reason)
    (name reason)
    (str (or reason "unknown"))))

(defn- name->str
  "Convert a name (keyword or string) to a Prometheus label string.
   :my-store -> \"my-store\", \"my-store\" -> \"my-store\"."
  [n]
  (if (keyword? n)
    (name n)
    (str n)))

;; =============================================================================
;; Metric update functions
;; =============================================================================

(defn set-entries!
  "Set current entry count gauge for a bounded atom."
  [atom-name entries]
  (prometheus/set
   (@registry entries-name {:name (name->str atom-name)})
   entries))

(defn set-capacity!
  "Set capacity gauge for a bounded atom."
  [atom-name capacity]
  (prometheus/set
   (@registry capacity-name {:name (name->str atom-name)})
   capacity))

(defn inc-evictions!
  "Increment eviction counter for a bounded atom.
   reason: :ttl, :capacity, or :ttl+capacity"
  [atom-name count reason]
  (when (and count (pos? count))
    (let [n (name->str atom-name)]
      ;; Split ttl+capacity into individual reason counters
      (case reason
        :ttl+capacity
        (do
          (prometheus/inc
           (@registry evictions-name {:name n :reason "ttl"})
           count)
          (prometheus/inc
           (@registry evictions-name {:name n :reason "capacity"})
           count))
        ;; Single reason
        (prometheus/inc
         (@registry evictions-name {:name n :reason (reason->str reason)})
         count)))))

(defn set-utilization!
  "Set utilization gauge for a bounded atom."
  [atom-name entries capacity]
  (let [ratio (if (pos? capacity)
                (/ (double entries) (double capacity))
                0.0)]
    (prometheus/set
     (@registry utilization-name {:name (name->str atom-name)})
     ratio)))

(defn inc-sweep-total!
  "Increment the sweep counter."
  []
  (prometheus/inc (@registry sweep-total-name)))

(defn set-sweep-last-evicted!
  "Set the last sweep evicted count gauge."
  [n]
  (prometheus/set (@registry sweep-evicted-name) n))

(defn observe-sweep-duration!
  "Observe sweep duration in seconds."
  [seconds]
  (when seconds
    (prometheus/observe (@registry sweep-duration-name) seconds)))

;; =============================================================================
;; Callback factories — create on-evict/on-sweep callbacks for bounded atoms
;; =============================================================================

(defn make-on-evict-callback
  "Create an :on-evict callback that updates Prometheus metrics.

   Called after evictions during bput!/bounded-swap!/bounded-reset!
   with {:name, :evicted-count, :reason, :entries}."
  [atom-name capacity]
  (fn [{:keys [evicted-count reason entries]}]
    (let [n (name->str atom-name)]
      (inc-evictions! n evicted-count reason)
      (set-entries! n entries)
      (set-utilization! n entries capacity))))

(defn make-on-sweep-callback
  "Create an :on-sweep callback that updates Prometheus metrics.

   Called after sweep! with {:name, :evicted-count, :remaining, :duration-ms}."
  [atom-name capacity]
  (fn [{:keys [evicted-count remaining duration-ms]}]
    (let [n (name->str atom-name)]
      (inc-sweep-total!)
      (set-sweep-last-evicted! evicted-count)
      (when duration-ms
        (observe-sweep-duration! (/ duration-ms 1000.0)))
      ;; Update per-atom metrics
      (set-entries! n remaining)
      (set-utilization! n remaining capacity)
      (when (pos? evicted-count)
        (inc-evictions! n evicted-count :ttl)))))

;; =============================================================================
;; Instrumented bounded atom constructor
;; =============================================================================

(defn instrumented-bounded-atom
  "Create a bounded atom with Prometheus metrics instrumentation built in.

   All standard bounded-atom options are supported, plus:
     :name — required string name for metrics labels

   Metrics auto-update on every bput!/bounded-swap!/bounded-reset!/sweep!.

   Example:
     (def store (instrumented-bounded-atom
                  {:max-entries 1000 :ttl-ms 300000 :name \"task-cache\"}))"
  [{:keys [max-entries name] :as opts}]
  (assert (some? name) "instrumented bounded atoms require a :name for metrics labels")
  (let [capacity max-entries
        on-evict (make-on-evict-callback name capacity)
        on-sweep (make-on-sweep-callback name capacity)
        batom (ba/bounded-atom (assoc opts
                                      :on-evict on-evict
                                      :on-sweep on-sweep))]
    ;; Initialize capacity gauge
    (set-capacity! name capacity)
    ;; Initialize entries/utilization at 0
    (set-entries! name 0)
    (set-utilization! name 0 capacity)
    batom))

(defn instrument!
  "Instrument an existing bounded atom with Prometheus metrics.

   Returns a new bounded atom map with metrics callbacks injected.
   The underlying atom data is preserved — only callbacks are added.

   atom-name: string name for metrics labels
   batom: existing bounded atom map

   Example:
     (def store (ba/bounded-atom {:max-entries 100}))
     (def instrumented (instrument! store \"my-store\"))"
  [batom atom-name]
  (let [capacity (get-in batom [:opts :max-entries])
        on-evict (make-on-evict-callback atom-name capacity)
        on-sweep (make-on-sweep-callback atom-name capacity)
        current-entries (ba/bcount batom)]
    ;; Initialize gauges
    (set-capacity! atom-name capacity)
    (set-entries! atom-name current-entries)
    (set-utilization! atom-name current-entries capacity)
    ;; Return augmented batom with callbacks
    (-> batom
        (assoc :name atom-name)
        (assoc-in [:opts :on-evict] on-evict)
        (assoc-in [:opts :on-sweep] on-sweep))))

;; =============================================================================
;; Snapshot — poll all registered bounded atoms for current metrics
;; =============================================================================

(defn snapshot-all!
  "Poll the sweepable registry and update Prometheus gauges for all
   registered bounded atoms. Useful for periodic scrape or health checks."
  []
  (doseq [[name-kw batom] (ba/registered-sweepables)]
    (let [n (name->str (or (:name batom) name-kw))
          entries (ba/bcount batom)
          capacity (get-in batom [:opts :max-entries])]
      (set-entries! n entries)
      (set-capacity! n capacity)
      (set-utilization! n entries capacity))))

;; =============================================================================
;; Metrics export
;; =============================================================================

(defn metrics-response
  "Export bounded atom metrics in Prometheus exposition format.
   Returns text/plain suitable for /metrics endpoint."
  []
  (export/text-format @registry))

(defn init!
  "Initialize the bounded atom metrics registry.
   Safe to call multiple times — collectors are idempotent."
  []
  (try
    @registry
    (log/info "Bounded atom Prometheus metrics initialized")
    true
    (catch Exception e
      (log/error e "Failed to initialize bounded atom metrics")
      false)))

(comment
  ;; REPL examples

  ;; Initialize
  (init!)

  ;; Create instrumented atom
  (def store (instrumented-bounded-atom
               {:max-entries 5 :ttl-ms 30000 :name "test-store"}))

  ;; Put some entries
  (ba/bput! store :k1 "hello")
  (ba/bput! store :k2 "world")

  ;; Check metrics
  (println (metrics-response))

  ;; Fill to trigger evictions
  (dotimes [i 10]
    (ba/bput! store (keyword (str "k" i)) (str "val-" i)))

  ;; Sweep
  (ba/sweep! store :test-store)

  ;; Check after sweep
  (println (metrics-response))

  ;; Snapshot all registered
  (snapshot-all!))
