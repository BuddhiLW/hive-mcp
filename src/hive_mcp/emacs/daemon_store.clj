(ns hive-mcp.emacs.daemon-store
  "Singleton daemon store for shared access across modules.

   Provides a shared IEmacsDaemon store instance that can be accessed
   from swarm sync, emacsclient, and coordinator modules.

   SOLID-S: Single Responsibility - store lifecycle management only.
   SOLID-D: Depends on IEmacsDaemon protocol abstraction.
   DDD: Infrastructure layer - singleton access to repository."
  (:require [hive-mcp.emacs.daemon :as daemon]
            [hive-mcp.emacs.daemon-ds :as daemon-ds]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Singleton Instance
;;; =============================================================================

(defonce ^{:doc "Singleton daemon store instance. Lazily initialized on first access."}
  daemon-store
  (delay
    (log/info "Initializing singleton daemon store")
    (daemon-ds/create-store)))

(defn get-store
  "Get the shared daemon store instance.
   Lazily initializes on first call.

   Returns:
     IEmacsDaemon implementation (DataScriptDaemonStore)"
  []
  @daemon-store)

;;; =============================================================================
;;; Convenience API (delegates to protocol)
;;; =============================================================================

(defn register!
  "Register a new Emacs daemon. Delegates to IEmacsDaemon.register!"
  [daemon-id opts]
  (daemon/register! (get-store) daemon-id opts))

(defn heartbeat!
  "Update daemon heartbeat. Delegates to IEmacsDaemon.heartbeat!"
  [daemon-id]
  (daemon/heartbeat! (get-store) daemon-id))

(defn mark-error!
  "Mark daemon as errored. Delegates to IEmacsDaemon.mark-error!"
  [daemon-id error-message]
  (daemon/mark-error! (get-store) daemon-id error-message))

(defn mark-terminated!
  "Mark daemon as terminated. Delegates to IEmacsDaemon.mark-terminated!"
  [daemon-id]
  (daemon/mark-terminated! (get-store) daemon-id))

(defn bind-ling!
  "Bind a ling to a daemon. Delegates to IEmacsDaemon.bind-ling!"
  [daemon-id ling-id]
  (daemon/bind-ling! (get-store) daemon-id ling-id))

(defn unbind-ling!
  "Unbind a ling from a daemon. Delegates to IEmacsDaemon.unbind-ling!"
  [daemon-id ling-id]
  (daemon/unbind-ling! (get-store) daemon-id ling-id))

(defn get-daemon
  "Get daemon by ID. Delegates to IEmacsDaemon.get-daemon"
  [daemon-id]
  (daemon/get-daemon (get-store) daemon-id))

(defn get-all-daemons
  "Get all registered daemons. Delegates to IEmacsDaemon.get-all-daemons"
  []
  (daemon/get-all-daemons (get-store)))

(defn get-daemons-by-status
  "Get daemons by status. Delegates to IEmacsDaemon.get-daemons-by-status"
  [status]
  (daemon/get-daemons-by-status (get-store) status))

(defn get-daemon-for-ling
  "Find daemon for a ling. Delegates to IEmacsDaemon.get-daemon-for-ling"
  [ling-id]
  (daemon/get-daemon-for-ling (get-store) ling-id))

(defn cleanup-stale!
  "Cleanup stale daemons. Delegates to IEmacsDaemon.cleanup-stale!"
  ([]
   (daemon/cleanup-stale! (get-store)))
  ([opts]
   (daemon/cleanup-stale! (get-store) opts)))

;;; =============================================================================
;;; Default Daemon ID
;;; =============================================================================

(defn default-daemon-id
  "Get the default daemon ID from environment or use 'server'.
   This matches the socket name used by *emacs-socket-name* in emacsclient.clj."
  []
  (or (System/getenv "EMACS_SOCKET_NAME") "server"))

(defn ensure-default-daemon!
  "Ensure the default daemon is registered.
   Call this during system startup to bootstrap daemon tracking.

   Returns:
     Transaction report from register! or nil if already exists"
  []
  (let [id (default-daemon-id)]
    (when-not (get-daemon id)
      (log/info "Registering default daemon:" id)
      (register! id {:socket-name id}))))

;;; =============================================================================
;;; Heartbeat Loop (Background Thread)
;;; =============================================================================

(def ^:private heartbeat-interval-ms
  "Interval between heartbeats in milliseconds. Default: 30 seconds."
  30000)

(def ^:private stale-cleanup-interval-ms
  "Interval between stale daemon cleanup checks. Default: 2 minutes."
  (* 2 60 1000))

(defonce ^{:private true
           :doc "Heartbeat loop state: {:running? bool :thread Thread :last-heartbeat inst :last-cleanup inst}"}
  heartbeat-state
  (atom {:running? false
         :thread nil
         :last-heartbeat nil
         :last-cleanup nil}))

(defn- heartbeat-loop
  "Background loop that sends heartbeats and cleans up stale daemons.
   Runs until running? becomes false."
  []
  (log/info "Daemon heartbeat loop started")
  (let [daemon-id (default-daemon-id)
        last-cleanup-at (atom (System/currentTimeMillis))]
    (while (:running? @heartbeat-state)
      (try
        ;; Send heartbeat for default daemon
        (when (get-daemon daemon-id)
          (heartbeat! daemon-id)
          (swap! heartbeat-state assoc :last-heartbeat (java.util.Date.)))

        ;; Periodic stale cleanup
        (let [now (System/currentTimeMillis)
              since-cleanup (- now @last-cleanup-at)]
          (when (>= since-cleanup stale-cleanup-interval-ms)
            (when-let [stale-ids (seq (cleanup-stale!))]
              (log/warn "Heartbeat: marked" (count stale-ids) "daemons as stale:" stale-ids))
            (reset! last-cleanup-at now)
            (swap! heartbeat-state assoc :last-cleanup (java.util.Date.))))

        (catch Exception e
          (log/error "Heartbeat loop error:" (.getMessage e))))

      ;; Sleep until next heartbeat
      (Thread/sleep heartbeat-interval-ms)))
  (log/info "Daemon heartbeat loop stopped"))

(defn start-heartbeat-loop!
  "Start the background heartbeat loop.
   Idempotent - does nothing if already running.

   Returns:
     Current heartbeat state"
  []
  (if (:running? @heartbeat-state)
    (do
      (log/debug "Heartbeat loop already running")
      @heartbeat-state)
    (let [thread (Thread. ^Runnable heartbeat-loop "daemon-heartbeat")]
      (.setDaemon thread true) ; JVM can exit without waiting for this thread
      (swap! heartbeat-state assoc :running? true :thread thread)
      (.start thread)
      @heartbeat-state)))

(defn stop-heartbeat-loop!
  "Stop the background heartbeat loop.
   Idempotent - does nothing if not running.

   Returns:
     Current heartbeat state"
  []
  (if-not (:running? @heartbeat-state)
    (do
      (log/debug "Heartbeat loop not running")
      @heartbeat-state)
    (do
      (swap! heartbeat-state assoc :running? false)
      ;; Thread will exit on next iteration
      (when-let [thread (:thread @heartbeat-state)]
        (.interrupt thread))
      (swap! heartbeat-state assoc :thread nil)
      @heartbeat-state)))

(defn heartbeat-status
  "Get current heartbeat loop status.

   Returns:
     Map with :running?, :last-heartbeat, :last-cleanup, :daemon-status"
  []
  (let [daemon-id (default-daemon-id)
        daemon (get-daemon daemon-id)]
    (merge (select-keys @heartbeat-state [:running? :last-heartbeat :last-cleanup])
           {:daemon-id daemon-id
            :daemon-status (:emacs-daemon/status daemon)
            :daemon-error-count (:emacs-daemon/error-count daemon)})))
