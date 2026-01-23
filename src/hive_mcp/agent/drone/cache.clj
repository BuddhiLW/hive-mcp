(ns hive-mcp.agent.drone.cache
  "Drone result caching to avoid redundant work.

   Features:
   - Task fingerprinting (hash of task + file contents)
   - LRU eviction with configurable max entries
   - TTL-based expiration
   - File-change invalidation
   - Hit rate metrics for monitoring"
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Cache State
;;; ============================================================

;; Cache for drone task results. Maps fingerprint -> {:result :expires :hits :last-access :created}
(defonce drone-cache (atom {}))

;; Metrics for cache performance monitoring
(defonce cache-metrics
  (atom {:hits 0
         :misses 0
         :evictions 0
         :total-saved-ms 0}))

;;; ============================================================
;;; Configuration
;;; ============================================================

(def ^:private default-cache-ttl-minutes 30)
(def ^:private max-cache-entries 100)

;;; ============================================================
;;; Fingerprinting
;;; ============================================================

(defn- compute-file-hash
  "Compute hash of file contents. Returns nil if file unreadable."
  [file-path]
  (try
    (hash (slurp file-path))
    (catch Exception _
      nil)))

(defn task-fingerprint
  "Generate unique fingerprint for a task + files combination.

   The fingerprint captures:
   - Task description hash
   - File contents hashes (not just paths)
   - Sorted for determinism

   Returns nil if any file is unreadable (cache miss forced)."
  [task files]
  (let [task-hash (hash task)
        file-hashes (when (seq files)
                      (mapv (fn [f]
                              (let [h (compute-file-hash f)]
                                (when h [f h])))
                            (sort files)))]
    ;; Only fingerprint if all files are readable
    (when (or (empty? files) (every? some? file-hashes))
      (str task-hash "-" (hash file-hashes)))))

;;; ============================================================
;;; Eviction
;;; ============================================================

(defn- evict-expired!
  "Remove expired entries from cache."
  []
  (let [now (System/currentTimeMillis)
        expired-keys (for [[k v] @drone-cache
                           :when (< (:expires v) now)]
                       k)]
    (when (seq expired-keys)
      (swap! drone-cache #(apply dissoc % expired-keys))
      (swap! cache-metrics update :evictions + (count expired-keys))
      (log/debug "Evicted expired cache entries" {:count (count expired-keys)}))))

(defn- evict-lru!
  "Evict least-recently-used entries if cache exceeds max size."
  []
  (when (> (count @drone-cache) max-cache-entries)
    (let [to-evict (- (count @drone-cache) max-cache-entries)
          sorted-by-access (->> @drone-cache
                                (sort-by (fn [[_ v]] (:last-access v 0)))
                                (take to-evict)
                                (map first))]
      (swap! drone-cache #(apply dissoc % sorted-by-access))
      (swap! cache-metrics update :evictions + (count sorted-by-access))
      (log/debug "LRU eviction" {:evicted (count sorted-by-access)}))))

;;; ============================================================
;;; Cache Operations
;;; ============================================================

(defn cache-result!
  "Store a drone result in cache with TTL.

   Arguments:
     fingerprint  - Task fingerprint from task-fingerprint
     result       - The drone execution result map
     ttl-minutes  - Time-to-live in minutes (default: 30)

   Note: Only caches successful results (status = :completed)"
  ([fingerprint result]
   (cache-result! fingerprint result default-cache-ttl-minutes))
  ([fingerprint result ttl-minutes]
   (when (and fingerprint
              (= :completed (:status result)))
     (evict-expired!)
     (evict-lru!)
     (let [now (System/currentTimeMillis)]
       (swap! drone-cache assoc fingerprint
              {:result result
               :expires (+ now (* ttl-minutes 60000))
               :created now
               :last-access now
               :hits 0
               :duration-ms (:duration-ms result 0)})
       (log/info "Cached drone result" {:fingerprint (subs fingerprint 0 (min 20 (count fingerprint)))
                                        :ttl-minutes ttl-minutes
                                        :files-modified (:files-modified result)})))))

(defn get-cached
  "Retrieve a cached result if valid. Returns nil on miss.

   Updates last-access time and hit count on successful retrieval.
   Tracks saved time in metrics based on original execution duration."
  [fingerprint]
  (when fingerprint
    (if-let [entry (get @drone-cache fingerprint)]
      (let [now (System/currentTimeMillis)]
        (if (< now (:expires entry))
          ;; Cache hit - update tracking
          (do
            (swap! drone-cache update fingerprint
                   (fn [e] (-> e
                               (assoc :last-access now)
                               (update :hits inc))))
            (swap! cache-metrics (fn [m]
                                   (-> m
                                       (update :hits inc)
                                       (update :total-saved-ms + (:duration-ms entry 0)))))
            (log/info "Cache HIT" {:fingerprint (subs fingerprint 0 (min 20 (count fingerprint)))
                                   :saved-ms (:duration-ms entry 0)})
            (:result entry))
          ;; Expired - treat as miss and remove
          (do
            (swap! drone-cache dissoc fingerprint)
            (swap! cache-metrics update :misses inc)
            nil)))
      ;; Not in cache
      (do
        (swap! cache-metrics update :misses inc)
        nil))))

(defn invalidate-cache!
  "Invalidate cache entries containing specific files.

   Call this when files change outside of drone execution.
   Returns number of entries invalidated."
  [files]
  (when (seq files)
    (let [file-set (set files)
          to-remove (for [[k v] @drone-cache
                          :when (some file-set (get-in v [:result :files-modified] []))]
                      k)
          count-removed (count to-remove)]
      (when (seq to-remove)
        (swap! drone-cache #(apply dissoc % to-remove))
        (swap! cache-metrics update :evictions + count-removed)
        (log/info "Invalidated cache entries" {:files files :removed count-removed}))
      count-removed)))

(defn invalidate-by-fingerprint!
  "Invalidate a specific cache entry by fingerprint."
  [fingerprint]
  (when (contains? @drone-cache fingerprint)
    (swap! drone-cache dissoc fingerprint)
    (swap! cache-metrics update :evictions inc)
    true))

;;; ============================================================
;;; Statistics & Management
;;; ============================================================

(defn cache-stats
  "Get cache statistics for monitoring.

   Returns:
     :entries     - Current cache entry count
     :hits        - Total cache hits
     :misses      - Total cache misses
     :hit-rate    - Hit rate as percentage (0-100)
     :evictions   - Total evictions
     :saved-ms    - Estimated time saved by cache hits
     :memory-kb   - Approximate memory usage in KB
     :oldest-entry-age-ms - Age of oldest entry in ms"
  []
  (let [{:keys [hits misses evictions total-saved-ms]} @cache-metrics
        entries @drone-cache
        total (+ hits misses)
        now (System/currentTimeMillis)
        oldest-age (when (seq entries)
                     (- now (apply min (map (comp :created second) entries))))]
    {:entries (count entries)
     :hits hits
     :misses misses
     :hit-rate (if (pos? total)
                 (double (/ (* 100 hits) total))
                 0.0)
     :evictions evictions
     :saved-ms total-saved-ms
     ;; Rough estimate: average 2KB per cached result
     :memory-kb (* 2 (count entries))
     :oldest-entry-age-ms oldest-age}))

(defn clear-cache!
  "Clear all cached drone results. Useful for testing or manual reset."
  []
  (let [entry-count (count @drone-cache)]
    (reset! drone-cache {})
    (reset! cache-metrics {:hits 0 :misses 0 :evictions 0 :total-saved-ms 0})
    (log/info "Cache cleared" {:entries-removed entry-count})
    entry-count))

(defn list-cached-tasks
  "List all cached task fingerprints with metadata (for debugging)."
  []
  (for [[fp entry] @drone-cache]
    {:fingerprint fp
     :files-modified (get-in entry [:result :files-modified])
     :created (:created entry)
     :expires (:expires entry)
     :hits (:hits entry)
     :duration-ms (:duration-ms entry)}))

;;; ============================================================
;;; Cache-Aware Dispatch
;;; ============================================================

(defn with-cache
  "Wrap drone delegation with caching. Returns cached result if available,
   otherwise executes delegate-fn and caches the result.

   Arguments:
     task        - Task description
     files       - List of files (used for fingerprinting)
     delegate-fn - Function to call if cache miss (takes no args)
     opts        - Optional map with:
                   :ttl-minutes - Cache TTL (default: 30)
                   :skip-cache  - If true, bypass cache entirely

   Returns:
     Result map with added :cache-hit? boolean

   Example:
     (with-cache task files
       #(drone/delegate! {:task task :files files} delegate-fn))"
  [task files delegate-fn & [{:keys [ttl-minutes skip-cache]
                              :or {ttl-minutes default-cache-ttl-minutes
                                   skip-cache false}}]]
  (let [fingerprint (when-not skip-cache
                      (task-fingerprint task files))]
    ;; Try cache first
    (if-let [cached-result (and fingerprint (get-cached fingerprint))]
      ;; Cache hit - return with marker
      (do
        (log/info "Drone cache HIT - skipping execution"
                  {:task (subs task 0 (min 50 (count task)))
                   :files files})
        (assoc cached-result :cache-hit? true))
      ;; Cache miss - execute and cache
      (let [result (delegate-fn)]
        (when fingerprint
          (cache-result! fingerprint result ttl-minutes))
        (assoc result :cache-hit? false)))))
