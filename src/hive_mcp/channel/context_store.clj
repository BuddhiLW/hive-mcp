(ns hive-mcp.channel.context-store
  "Ephemeral context store for pass-by-reference agent communication.

   ConcurrentHashMap-backed store for structured Clojure data with TTL
   auto-eviction. Agents store context once, pass IDs in messages (~50
   tokens vs 2000+ for serialized payloads).

   Lifecycle: context-put! → context-get (by ID) → auto-evict after TTL.
   Reaper runs every 60s to clean expired entries.

   Thread-safe: ConcurrentHashMap for O(1) get/put, no atom contention."
  (:require [taoensso.timbre :as log])
  (:import [java.util.concurrent ConcurrentHashMap ScheduledExecutorService
            Executors TimeUnit]
           [java.util.function BiFunction]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const default-ttl-ms
  "Default time-to-live for context entries (5 minutes)."
  300000)

(def ^:const reaper-interval-sec
  "Reaper runs every 60 seconds."
  60)

;; =============================================================================
;; Store (ConcurrentHashMap)
;; =============================================================================

(defonce ^{:doc "ConcurrentHashMap<String, Map> — the context store."}
  ^ConcurrentHashMap store
  (ConcurrentHashMap.))

(defonce ^{:doc "ScheduledExecutorService for reaper. nil when stopped."}
  reaper-executor
  (atom nil))

;; =============================================================================
;; ID Generation
;; =============================================================================

(defn- generate-ctx-id
  "Generate a context ID: ctx-{timestamp}-{8hex}.
   Timestamp provides rough ordering; hex suffix provides uniqueness."
  []
  (let [ts (System/currentTimeMillis)
        hex (format "%08x" (bit-and (hash (random-uuid)) 0xFFFFFFFF))]
    (str "ctx-" ts "-" hex)))

;; =============================================================================
;; Internal Helpers
;; =============================================================================

(defn- now-ms
  "Current epoch milliseconds."
  []
  (System/currentTimeMillis))

(defn- expired?
  "Check if an entry has expired."
  [entry]
  (> (now-ms) (:expires-at entry)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn context-put!
  "Store data in the context store. Returns ctx-id.

   Options:
   - :tags    — set of string tags for querying (default #{})
   - :ttl-ms  — time-to-live in milliseconds (default 300000 = 5 min)"
  [data & {:keys [tags ttl-ms] :or {tags #{} ttl-ms default-ttl-ms}}]
  (let [id (generate-ctx-id)
        now (now-ms)
        entry {:id id
               :data data
               :tags (set tags)
               :created-at now
               :ttl-ms ttl-ms
               :expires-at (+ now ttl-ms)
               :access-count 0
               :last-accessed nil}]
    (.put store id entry)
    (log/debug "[context-store] put" id "tags:" tags "ttl:" ttl-ms)
    id))

(defn context-get
  "Retrieve entry by ID. Returns entry map or nil.
   Increments :access-count and updates :last-accessed atomically.
   Returns nil for expired entries (lazy eviction on read)."
  [ctx-id]
  (let [result (volatile! nil)]
    (.computeIfPresent store ctx-id
                       (reify java.util.function.BiFunction
                         (apply [_ _k entry]
                           (if (expired? entry)
                             (do (vreset! result nil)
                                 nil) ;; returning nil removes the key
                             (let [now (now-ms)
                                   updated (-> entry
                                               (update :access-count inc)
                                               (assoc :last-accessed now))]
                               (vreset! result updated)
                               updated)))))
    @result))

(defn context-query
  "Query entries by tags. Returns entries whose tags are a superset of query tags.
   Excludes expired entries (lazy eviction).

   Options:
   - :tags  — set of tags to match (entries must have ALL of these)
   - :limit — max entries to return (default: 100)"
  [& {:keys [tags limit] :or {limit 100}}]
  (let [query-tags (set tags)
        results (java.util.ArrayList.)]
    (doseq [entry (vals (into {} store))]
      (when (and (not (expired? entry))
                 (every? (:tags entry) query-tags))
        (.add results entry)))
    ;; Sort by created-at descending (newest first), take limit
    (->> (vec results)
         (sort-by :created-at >)
         (take limit)
         vec)))

(defn context-evict!
  "Remove entry by ID. Returns true if entry existed, false otherwise."
  [ctx-id]
  (some? (.remove store ctx-id)))

(defn evict-by-tags!
  "Evict all entries matching ANY of the given tags.
   Returns count of entries evicted. Used for session cleanup:
   when a session wraps/completes, evict cached context for that agent.

   Example: (evict-by-tags! #{\"agent:swarm-xyz-123\"}) evicts all
   entries tagged with that agent ID."
  [tags]
  (let [query-tags (set tags)
        evicted (atom 0)]
    (doseq [[id entry] (into {} store)]
      (when (and (not (expired? entry))
                 (some query-tags (:tags entry)))
        (when (.remove store id)
          (swap! evicted inc))))
    (let [n @evicted]
      (when (pos? n)
        (log/info "[context-store] evicted" n "entries by tags:" query-tags))
      n)))

(defn context-stats
  "Return store statistics.
   {:total N :oldest <ts> :newest <ts> :bytes-approx N}"
  []
  (let [entries (vec (vals (into {} store)))
        live (remove expired? entries)
        live-vec (vec live)
        timestamps (map :created-at live-vec)]
    {:total (count live-vec)
     :oldest (when (seq timestamps) (apply min timestamps))
     :newest (when (seq timestamps) (apply max timestamps))
     :bytes-approx (reduce + 0 (map #(count (pr-str (:data %))) live-vec))}))

;; =============================================================================
;; Reaper
;; =============================================================================

(defn reap-expired!
  "Remove all expired entries from the store. Returns count removed."
  []
  (let [removed (atom 0)]
    (doseq [[id entry] (into {} store)]
      (when (expired? entry)
        (when (.remove store id)
          (swap! removed inc))))
    (let [n @removed]
      (when (pos? n)
        (log/info "[context-store] reaped" n "expired entries"))
      n)))

(defn start-reaper!
  "Start the background reaper. Idempotent — no-op if already running."
  []
  (when-not @reaper-executor
    (let [executor (Executors/newSingleThreadScheduledExecutor)]
      (.scheduleAtFixedRate executor
                            ^Runnable (fn []
                                        (try
                                          (reap-expired!)
                                          (catch Exception e
                                            (log/error e "[context-store] reaper error"))))
                            reaper-interval-sec
                            reaper-interval-sec
                            TimeUnit/SECONDS)
      (reset! reaper-executor executor)
      (log/info "[context-store] reaper started, interval:" reaper-interval-sec "s"))))

(defn stop-reaper!
  "Stop the background reaper. Idempotent — no-op if not running."
  []
  (when-let [^ScheduledExecutorService executor @reaper-executor]
    (.shutdownNow executor)
    (reset! reaper-executor nil)
    (log/info "[context-store] reaper stopped")))

;; =============================================================================
;; Reset (for testing)
;; =============================================================================

(defn reset-all!
  "Clear all entries and stop reaper. For testing."
  []
  (.clear store)
  (stop-reaper!))
