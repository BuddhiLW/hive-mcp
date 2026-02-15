(ns hive-mcp.agent.drone.cache
  "Drone result caching with task fingerprinting, LRU eviction, and TTL expiration."
  (:require [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Cache for drone task results. Maps fingerprint -> {:result :expires :hits :last-access :created}
(defonce drone-cache (atom {}))

(defonce cache-metrics
  (atom {:hits 0
         :misses 0
         :evictions 0
         :total-saved-ms 0}))

(def ^:private default-cache-ttl-minutes 30)
(def ^:private max-cache-entries 100)

(defn- compute-file-hash
  "Compute hash of file contents, returning nil if file unreadable."
  [file-path]
  (rescue nil (hash (slurp file-path))))

(defn task-fingerprint
  "Generate unique fingerprint for a task + files combination."
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

(defn cache-result!
  "Store a successful drone result in cache with TTL."
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
  "Retrieve a cached result if valid, returning nil on miss."
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
  "Invalidate cache entries containing specific files."
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

(defn cache-stats
  "Get cache statistics for monitoring."
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
     :memory-kb (* 2 (count entries))
     :oldest-entry-age-ms oldest-age}))

(defn clear-cache!
  "Clear all cached drone results."
  []
  (let [entry-count (count @drone-cache)]
    (reset! drone-cache {})
    (reset! cache-metrics {:hits 0 :misses 0 :evictions 0 :total-saved-ms 0})
    (log/info "Cache cleared" {:entries-removed entry-count})
    entry-count))

(defn list-cached-tasks
  "List all cached task fingerprints with metadata."
  []
  (for [[fp entry] @drone-cache]
    {:fingerprint fp
     :files-modified (get-in entry [:result :files-modified])
     :created (:created entry)
     :expires (:expires entry)
     :hits (:hits entry)
     :duration-ms (:duration-ms entry)}))

(defn with-cache
  "Wrap drone delegation with caching, returning cached result if available."
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
