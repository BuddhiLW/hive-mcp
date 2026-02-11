(ns hive-mcp.memory.store.chroma
  "ChromaDB implementation of IMemoryStore protocol.

   Wraps existing hive-mcp.chroma functions into protocol methods.
   This is Phase 1 of vectordb abstraction - enabling pluggable backends
   later (e.g., Milvus, DataScript-for-testing).

   DDD: Repository pattern - ChromaMemoryStore is the Chroma aggregate adapter."
  (:require [hive-mcp.protocols.memory :as proto]
            [hive-mcp.chroma :as chroma]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


(defrecord ChromaMemoryStore [config-atom]
  ;; =========================================================================
  ;; IMemoryStore - Core Protocol (16 methods)
  ;; =========================================================================
  proto/IMemoryStore

  ;; --- Connection Lifecycle ---

  (connect! [_this config]
    (try
      (chroma/configure! (select-keys config [:host :port :collection-name]))
      ;; Verify connection by attempting collection access
      (when (chroma/embedding-configured?)
        (chroma/chroma-available?))
      (swap! config-atom merge config)
      {:success? true
       :backend "chroma"
       :errors []
       :metadata (select-keys @config-atom [:host :port :collection-name])}
      (catch Exception e
        {:success? false
         :backend "chroma"
         :errors [(.getMessage e)]
         :metadata {}})))

  (disconnect! [_this]
    (try
      (chroma/reset-collection-cache!)
      {:success? true :errors []}
      (catch Exception e
        {:success? false :errors [(.getMessage e)]})))

  (connected? [_this]
    (chroma/embedding-configured?))

  (health-check [_this]
    (let [start-ms (System/currentTimeMillis)]
      (try
        (let [available? (chroma/chroma-available?)
              latency-ms (- (System/currentTimeMillis) start-ms)
              stats (when available? (chroma/collection-stats))]
          {:healthy? (boolean available?)
           :latency-ms latency-ms
           :backend "chroma"
           :entry-count (when stats (:count stats))
           :errors (if available? [] ["Chroma not reachable"])
           :checked-at (str (java.time.ZonedDateTime/now
                             (java.time.ZoneId/systemDefault)))})
        (catch Exception e
          {:healthy? false
           :latency-ms (- (System/currentTimeMillis) start-ms)
           :backend "chroma"
           :entry-count nil
           :errors [(.getMessage e)]
           :checked-at (str (java.time.ZonedDateTime/now
                             (java.time.ZoneId/systemDefault)))}))))

  ;; --- CRUD Operations ---

  (add-entry! [_this entry]
    (chroma/index-memory-entry! entry))

  (get-entry [_this id]
    (chroma/get-entry-by-id id))

  (update-entry! [_this id updates]
    (chroma/update-entry! id updates))

  (delete-entry! [_this id]
    (chroma/delete-entry! id)
    true)

  (query-entries [_this opts]
    (let [{:keys [type project-id project-ids limit include-expired?]
           :or {limit 100 include-expired? false}} opts]
      (chroma/query-entries :type type
                            :project-id project-id
                            :project-ids project-ids
                            :limit limit
                            :include-expired? include-expired?)))

  ;; --- Semantic Search ---

  (search-similar [_this query-text opts]
    (let [{:keys [limit type project-ids]} opts]
      (chroma/search-similar query-text
                             :limit (or limit 10)
                             :type type
                             :project-ids project-ids)))

  (supports-semantic-search? [_this]
    (chroma/embedding-configured?))

  ;; --- Expiration Management ---

  (cleanup-expired! [_this]
    (chroma/cleanup-expired!))

  (entries-expiring-soon [_this days opts]
    (let [{:keys [project-id]} opts]
      (chroma/entries-expiring-soon days :project-id project-id)))

  ;; --- Duplicate Detection ---

  (find-duplicate [_this type content-hash opts]
    (let [{:keys [project-id]} opts]
      (chroma/find-duplicate type content-hash :project-id project-id)))

  ;; --- Store Management ---

  (store-status [_this]
    (let [status (chroma/status)]
      {:backend "chroma"
       :configured? (:configured? status)
       :entry-count (try (:count (chroma/collection-stats)) (catch Exception _ nil))
       :supports-search? (:configured? status)}))

  (reset-store! [_this]
    (chroma/reset-collection-cache!)
    true)

  ;; =========================================================================
  ;; IMemoryStoreWithAnalytics - Optional Extension (3 methods)
  ;; =========================================================================
  proto/IMemoryStoreWithAnalytics

  (log-access! [_this id]
    (when-let [entry (chroma/get-entry-by-id id)]
      (let [new-count (inc (or (:access-count entry) 0))]
        (chroma/update-entry! id {:access-count new-count}))))

  (record-feedback! [_this id feedback]
    (when-let [entry (chroma/get-entry-by-id id)]
      (let [field (case feedback
                    :helpful :helpful-count
                    :unhelpful :unhelpful-count)
            new-count (inc (or (get entry field) 0))]
        (chroma/update-entry! id {field new-count}))))

  (get-helpfulness-ratio [_this id]
    (when-let [entry (chroma/get-entry-by-id id)]
      (let [helpful (or (:helpful-count entry) 0)
            unhelpful (or (:unhelpful-count entry) 0)
            total (+ helpful unhelpful)]
        {:helpful-count helpful
         :unhelpful-count unhelpful
         :total total
         :ratio (if (pos? total)
                  (double (/ helpful total))
                  0.0)})))

  ;; =========================================================================
  ;; IMemoryStoreWithStaleness - Optional Extension (3 methods)
  ;; =========================================================================
  proto/IMemoryStoreWithStaleness

  (update-staleness! [_this id staleness-opts]
    (chroma/update-staleness! id staleness-opts))

  (get-stale-entries [_this threshold opts]
    (let [{:keys [project-id type]} opts
          entries (chroma/query-entries :project-id project-id
                                        :type type
                                        :limit 10000)]
      (->> entries
           (filter (fn [entry]
                     (let [alpha (or (:staleness-alpha entry) 1)
                           beta (or (:staleness-beta entry) 1)
                           prob (/ (double beta) (+ alpha beta))]
                       (> prob threshold))))
           vec)))

  (propagate-staleness! [_this source-id depth]
    ;; Phase 2 feature - propagate via KG edges
    ;; For now, just update the source entry's depth
    (when-let [entry (chroma/get-entry-by-id source-id)]
      (let [kg-outgoing (:kg-outgoing entry)
            updated-count (atom 0)]
        (doseq [dep-id kg-outgoing
                :when (and dep-id (seq dep-id))]
          (try
            (chroma/update-staleness! dep-id
                                      {:beta (inc (or (:staleness-beta
                                                       (chroma/get-entry-by-id dep-id)) 1))
                                       :source :transitive
                                       :depth (inc depth)})
            (swap! updated-count inc)
            (catch Exception e
              (log/debug "Failed to propagate staleness to" dep-id ":" (.getMessage e)))))
        @updated-count))))


(defn create-store
  "Create a new Chroma-backed memory store.

   Options (optional, also configurable via connect!):
     :host            - Chroma server host (default: localhost)
     :port            - Chroma server port (default: 8000)
     :collection-name - Collection name (default: hive-mcp-memory)

   Returns an IMemoryStore implementation.

   Example:
     (def store (create-store))
     (proto/connect! store {:host \"localhost\" :port 8000})
     (proto/set-store! store)"
  ([]
   (create-store {}))
  ([opts]
   (log/info "Creating ChromaMemoryStore" (when (seq opts) opts))
   (->ChromaMemoryStore (atom (merge {:host "localhost"
                                      :port 8000
                                      :collection-name "hive-mcp-memory"}
                                     opts)))))
