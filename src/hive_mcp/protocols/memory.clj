(ns hive-mcp.protocols.memory
  "Protocol definitions for memory storage backends."
  (:require [clojure.string]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IMemoryStore Protocol (Core CRUD + Search)
;;; ============================================================================

(defprotocol IMemoryStore
  "Storage backend protocol for memory entries."

  ;;; =========================================================================
  ;;; Connection Lifecycle
  ;;; =========================================================================

  (connect! [this config]
    "Initialize connection to the storage backend.")

  (disconnect! [this]
    "Close connection and release backend resources.")

  (connected? [this]
    "Check if this store has an active connection.")

  (health-check [this]
    "Verify backend health and reachability.")

;;; =========================================================================
  ;;; CRUD Operations
  ;;; =========================================================================

  (add-entry! [this entry]
    "Add a new memory entry to the store.")

  (get-entry [this id]
    "Get a memory entry by ID.")

  (update-entry! [this id updates]
    "Update an existing entry's attributes.")

  (delete-entry! [this id]
    "Delete an entry from the store.")

  (query-entries [this opts]
    "Query entries with filtering.")

  ;;; =========================================================================
  ;;; Semantic Search (Vector-based)
  ;;; =========================================================================

  (search-similar [this query-text opts]
    "Semantic similarity search.")

  (supports-semantic-search? [this]
    "Check if this store supports semantic search.")

  ;;; =========================================================================
  ;;; Expiration Management
  ;;; =========================================================================

  (cleanup-expired! [this]
    "Delete all expired entries.")

  (entries-expiring-soon [this days opts]
    "Get entries expiring within the given number of days.")

  ;;; =========================================================================
  ;;; Duplicate Detection
  ;;; =========================================================================

  (find-duplicate [this type content-hash opts]
    "Find entry with matching content-hash.")

  ;;; =========================================================================
  ;;; Store Management
  ;;; =========================================================================

  (store-status [this]
    "Get store status and configuration info.")

  (reset-store! [this]
    "Reset the store to empty state."))

;;; ============================================================================
;;; Active Store Management
;;; ============================================================================

(defonce ^:private active-store (atom nil))

(defn set-store!
  "Set the active memory store implementation."
  [store]
  {:pre [(satisfies? IMemoryStore store)]}
  (reset! active-store store)
  store)

(defn get-store
  "Get the active memory store, or throw if none set."
  []
  (or @active-store
      (throw (ex-info "No memory store configured. Call set-store! first."
                      {:hint "Initialize with chroma-store or datascript-store"}))))

(defn store-set?
  "Check if a memory store has been configured."
  []
  (some? @active-store))

(defn reset-active-store!
  "Reset the active store atom to nil."
  []
  (when-let [store @active-store]
    (try
      (disconnect! store)
      (catch Exception _)))
  (reset! active-store nil))

;;; ============================================================================
;;; Lifecycle Convenience Functions
;;; ============================================================================

(defn connect-active-store!
  "Connect the active store with the given config."
  [config]
  (connect! (get-store) config))

(defn active-store-healthy?
  "Check if the active store is connected and healthy."
  []
  (when (store-set?)
    (try
      (:healthy? (health-check @active-store))
      (catch Exception _ false))))

(defn active-store-status
  "Get comprehensive status of the active store."
  []
  (when (store-set?)
    (let [store @active-store]
      (merge (store-status store)
             (try (health-check store)
                  (catch Exception e
                    {:healthy? false :errors [(.getMessage e)]}))))))

;;; ============================================================================
;;; IMemoryStoreWithAnalytics Protocol (Optional Extension)
;;; ============================================================================

(defprotocol IMemoryStoreWithAnalytics
  "Optional extension for analytics tracking."

  (log-access! [this id]
    "Log an access event for an entry.")

  (record-feedback! [this id feedback]
    "Record helpfulness feedback for an entry.")

  (get-helpfulness-ratio [this id]
    "Calculate helpfulness ratio for an entry."))

(defn analytics-store?
  "Check if the store supports analytics tracking."
  [store]
  (satisfies? IMemoryStoreWithAnalytics store))

;;; ============================================================================
;;; IMemoryStoreWithStaleness Protocol (Optional Extension)
;;; ============================================================================

(defprotocol IMemoryStoreWithStaleness
  "Optional extension for staleness tracking."

  (update-staleness! [this id staleness-opts]
    "Update staleness tracking fields for an entry.")

  (get-stale-entries [this threshold opts]
    "Get entries with staleness probability above threshold.")

  (propagate-staleness! [this source-id depth]
    "Propagate staleness from source entry to dependents."))

(defn staleness-store?
  "Check if the store supports staleness tracking."
  [store]
  (satisfies? IMemoryStoreWithStaleness store))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defn content-hash
  "Compute SHA-256 hash of normalized content."
  [content]
  (let [content-str (if (string? content) content (pr-str content))
        normalized (-> content-str
                       clojure.string/trim
                       (clojure.string/replace #"[ \t]+" " ")
                       (clojure.string/replace #"\n+" "\n"))
        md (java.security.MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest md (.getBytes normalized "UTF-8"))]
    (apply str (map #(format "%02x" %) hash-bytes))))

(defn generate-id
  "Generate a unique timestamped ID for memory entries."
  []
  (let [ts (java.time.LocalDateTime/now)
        fmt (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")
        random-hex (format "%08x" (rand-int Integer/MAX_VALUE))]
    (str (.format ts fmt) "-" random-hex)))

(defn iso-timestamp
  "Return current ISO 8601 timestamp."
  []
  (str (java.time.ZonedDateTime/now
        (java.time.ZoneId/systemDefault))))
