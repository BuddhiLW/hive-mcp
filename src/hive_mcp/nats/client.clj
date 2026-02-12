(ns hive-mcp.nats.client
  "NATS client wrapper for push-based drone notifications, enabling real-time
   communication in the Hive system. Manages connection lifecycle, publish,
   subscribe, and health checks.

   Implements a graceful degradation model: when NATS is unavailable, the system
   automatically falls back to polling-based collect mechanisms, ensuring
   reliability without fatal errors.

   Depends on the Java NATS client (io.nats.client) for low-level NATS protocol
   handling. State is managed via atoms: connection (io.nats.client.Connection),
   dispatcher (async message handling), and subscriptions (subject-to-Subscription
   tracking for cleanup)."

  (:require [clojure.data.json :as json]
            [taoensso.timbre :as log])
  (:import [io.nats.client Nats Options$Builder Connection$Status
            MessageHandler]
           [java.time Duration]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

;; Atom holding the io.nats.client.Connection instance, or nil if disconnected.
(defonce ^:private connection
  (atom nil))

;; Atom holding the Dispatcher for asynchronous message handling, or nil if disconnected.
(defonce ^:private dispatcher
  (atom nil))

;; Atom mapping subject strings to Subscription objects for cleanup during disconnect.
(defonce ^:private subscriptions
  (atom {}))  ;; subject -> Subscription

;; =============================================================================
;; Connection Lifecycle
;; =============================================================================

(defn connected?
  "Return true if NATS connection is active and connected."
  []
  (some-> @connection (.getStatus) (= Connection$Status/CONNECTED)))

(defn start!
  "Connect to NATS server. Non-fatal on failure — logs warning and returns nil.

   Options:
     :url                - NATS server URL (default: nats://localhost:4222)
     :connection-timeout - Connection timeout in ms (default: 5000)
     :max-reconnects     - Max reconnect attempts (default: 5)
     :reconnect-wait     - Wait between reconnects in ms (default: 1000)

   Sets connectionName to 'hive-mcp' for NATS server monitoring and debugging."
  [{:keys [url connection-timeout max-reconnects reconnect-wait]
    :or {url "nats://localhost:4222"
         connection-timeout 5000
         max-reconnects 5
         reconnect-wait 1000}}]
  (try
    (let [opts (-> (Options$Builder.)
                   (.server url)
                   (.connectionTimeout (Duration/ofMillis connection-timeout))
                   (.maxReconnects max-reconnects)
                   (.reconnectWait (Duration/ofMillis reconnect-wait))
                   (.connectionName "hive-mcp")
                   (.build))
          conn (Nats/connect opts)]
      (reset! connection conn)
      (reset! dispatcher (.createDispatcher conn))
      (log/info "[NATS] Connected to" url))
    (catch Exception e
      (log/warn "[NATS] Connection failed (non-fatal):" (.getMessage e)))))

(defn stop!
  "Disconnect from NATS server. Safe to call when not connected."
  []
  (when-let [conn @connection]
    (try (.close conn) (catch Exception _))
    (reset! connection nil)
    (reset! dispatcher nil)
    (reset! subscriptions {})
    (log/info "[NATS] Disconnected")))

;; =============================================================================
;; Publish / Subscribe
;; =============================================================================

(defn publish!
  "Publish JSON message to subject. Serializes data to JSON string using
   clojure.data.json and UTF-8 encoding. No-op if disconnected."
  [subject data]
  (when-let [conn @connection]
    (when (connected?)
      (.publish conn subject (.getBytes (json/write-str data) "UTF-8")))))

(defn subscribe!
  "Subscribe to subject with handler fn. Handler receives parsed JSON map.
   Implements MessageHandler via reify, deserializing incoming message data
   from JSON using clojure.data.json with keyword keys. Logs warnings on
   handler exceptions. Tracks subscription in the subscriptions atom for cleanup."
  [subject handler-fn]
  (when-let [disp @dispatcher]
    (let [sub (.subscribe disp subject
                          (reify MessageHandler
                            (onMessage [_ msg]
                              (try
                                (let [data (json/read-str (String. (.getData msg) "UTF-8") :key-fn keyword)]
                                  (handler-fn data))
                                (catch Exception e
                                  (log/warn "[NATS] Handler error on" subject ":" (.getMessage e)))))))]
      (swap! subscriptions assoc subject sub)
      sub)))

(defn unsubscribe!
  "Unsubscribe from a subject. Safe to call when not subscribed."
  [subject]
  (when-let [_sub (get @subscriptions subject)]
    (when-let [disp @dispatcher]
      (.unsubscribe disp subject))
    (swap! subscriptions dissoc subject)))