(ns hive-mcp.swarm.datalevin.listeners
  "Listener shim for Datalevin connections.

   Datalevin (unlike DataScript) has no native listen!/unlisten! API.
   This module provides equivalent semantics via an atom-based listener
   registry and a transact-with-notify! wrapper.

   Pattern:
     (listen! conn :my-key (fn [tx-report] ...))
     (transact-with-notify! conn tx-data)   ;; fires all listeners
     (unlisten! conn :my-key)

   The tx-report passed to callbacks is the raw Datalevin tx-report
   returned by datalevin.core/transact! — it contains:
     :db-before  - DB value before transaction
     :db-after   - DB value after transaction
     :tx-data    - Vector of Datom changes
     :tx-meta    - Transaction metadata (if any)

   Thread safety:
   - Listener registry uses atom (swap! is atomic)
   - Callbacks are invoked synchronously after transact! returns
   - Callbacks MUST be fast and non-blocking (same contract as DataScript)
   - Exceptions in callbacks are caught and logged (never poison the tx)"

  (:require [datalevin.core :as dl]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Listener Registry (per-connection)
;;; =============================================================================

;; Maps connection identity → {key → callback-fn}
;; Using connection identity (System/identityHashCode) as the outer key
;; so multiple Datalevin connections can have independent listener sets.
(defonce ^:private registries (atom {}))

(defn- conn-key
  "Stable identity key for a Datalevin connection object."
  [conn]
  (System/identityHashCode conn))

(defn- get-listeners
  "Get the listener map for a connection. Returns {} if none."
  [conn]
  (get @registries (conn-key conn) {}))

;;; =============================================================================
;;; Public API (mirrors datascript.core/listen! and unlisten!)
;;; =============================================================================

(defn listen!
  "Register a transaction listener on a Datalevin connection.

   Arguments:
     conn     - Datalevin connection (from dl/get-conn)
     key      - Keyword key for this listener (for unlisten!)
     callback - (fn [tx-report] ...) called after each transact-with-notify!

   Returns the key.

   Contract:
   - Callbacks are invoked synchronously after successful transact!
   - Callbacks MUST be fast and non-blocking
   - Exceptions in callbacks are caught and logged (never break the tx caller)
   - Registering the same key twice replaces the previous callback"
  [conn key callback]
  {:pre [(some? conn) (keyword? key) (fn? callback)]}
  (swap! registries assoc-in [(conn-key conn) key] callback)
  (log/debug "Datalevin listener registered" {:key key})
  key)

(defn unlisten!
  "Remove a transaction listener from a Datalevin connection.

   Arguments:
     conn - Datalevin connection
     key  - The key used in listen!

   Returns nil. Safe to call with non-existent keys."
  [conn key]
  {:pre [(some? conn) (keyword? key)]}
  (swap! registries update (conn-key conn) dissoc key)
  (log/debug "Datalevin listener removed" {:key key})
  nil)

(defn clear-listeners!
  "Remove all listeners for a connection.
   Called during connection close/reset to prevent stale callbacks."
  [conn]
  (swap! registries dissoc (conn-key conn))
  (log/debug "Datalevin listeners cleared"))

(defn listener-count
  "Number of active listeners for a connection. Useful for diagnostics."
  [conn]
  (count (get-listeners conn)))

;;; =============================================================================
;;; transact-with-notify!
;;; =============================================================================

(defn- fire-listeners!
  "Invoke all registered listeners for a connection with the tx-report.
   Catches and logs exceptions per-listener so one bad callback
   never poisons the transaction or other listeners."
  [conn tx-report]
  (doseq [[key callback] (get-listeners conn)]
    (try
      (callback tx-report)
      (catch Exception e
        (log/warn e "Datalevin listener threw exception"
                  {:key key :error (ex-message e)})))))

(defn transact-with-notify!
  "Transact data to a Datalevin connection and notify all listeners.

   Drop-in replacement for datalevin.core/transact! that adds
   DataScript-compatible listener notification semantics.

   Arguments:
     conn    - Datalevin connection
     tx-data - Transaction data (maps, vectors, or retract tuples)

   Returns the tx-report from datalevin.core/transact!

   Listener notification:
   - Listeners are only fired on successful transactions
   - All listeners receive the same tx-report
   - Listener exceptions are caught and logged (never propagate)
   - If no listeners are registered, behaves identically to dl/transact!"
  [conn tx-data]
  (let [tx-report (dl/transact! conn tx-data)]
    (when (seq (get-listeners conn))
      (fire-listeners! conn tx-report))
    tx-report))
