(ns hive-mcp.telemetry.health
  "Centralized catastrophic event handling.

   CLARITY-T: All failures flow through here for:
   - Structured logging
   - WebSocket emission (Emacs visibility)
   - DataScript persistence (post-mortem)
   - Circuit breaker state updates

   This module is the SINGLE place for all catastrophic event handling.
   No more scattered try/catches that swallow errors silently."
  (:require [taoensso.timbre :as log]
            [hive-mcp.channel :as channel]
            [datascript.core :as d]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-mcp.telemetry.prometheus :as prom]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Constants (Value Objects)
;;; =============================================================================

(def severities
  "Valid severity levels for health events.

   :info  - Informational, no immediate action needed
   :warn  - Warning, degraded but functional
   :error - Error, feature unavailable
   :fatal - Fatal, system unstable"
  #{:info :warn :error :fatal})

(def error-types
  "Known error types that can occur in the system.
   Each represents a specific failure mode with known recovery strategies."
  #{:harvest-failed           ; Memory/wrap harvest operation failed
    :emacs-unreachable        ; Cannot communicate with Emacs
    :chroma-unavailable       ; Chroma vector DB not responding
    :websocket-death          ; WebSocket connection died
    :nrepl-disconnect         ; nREPL server disconnected
    :restart-collision        ; Attempted restart while previous in progress
    :hot-reload-failed        ; Hot reload of namespace failed
    :wrap-crystallize-failed  ; Session crystallization failed
    })

;;; =============================================================================
;;; DataScript Schema Extension
;;; =============================================================================

(def health-event-schema
  "DataScript schema for health events.
   These attributes extend the swarm datascript schema."
  {:health-event/id
   {:db/doc "Unique identifier for the health event"
    :db/unique :db.unique/identity}

   :health-event/type
   {:db/doc "Type of error (from error-types set)"}

   :health-event/severity
   {:db/doc "Severity level: :info :warn :error :fatal"}

   :health-event/message
   {:db/doc "Human-readable error message"}

   :health-event/context
   {:db/doc "Additional context map (optional, extra data)"}

   :health-event/timestamp
   {:db/doc "When the event occurred"}

   :health-event/recoverable?
   {:db/doc "Whether the error is recoverable"}})

;;; =============================================================================
;;; Private Helpers
;;; =============================================================================

(defn- validate-event!
  "Validate event structure. Throws ExceptionInfo on invalid input."
  [{:keys [type severity]}]
  (when-not (contains? error-types type)
    (throw (ex-info (str "Unknown error type: " type)
                    {:type :validation-error
                     :error-type type
                     :valid-types error-types})))
  (when-not (contains? severities severity)
    (throw (ex-info (str "Invalid severity: " severity)
                    {:type :validation-error
                     :severity severity
                     :valid-severities severities}))))

(defn- log-by-severity!
  "Log with appropriate Timbre level based on severity."
  [{:keys [type severity message context recoverable? event-id]}]
  (let [log-data {:event-id event-id
                  :type type
                  :message message
                  :context context
                  :recoverable? recoverable?}]
    (case severity
      :info  (log/info :health-event log-data)
      :warn  (log/warn :health-event log-data)
      :error (log/error :health-event log-data)
      :fatal (log/error :health-event (assoc log-data :fatal? true)))))

(defn- emit-to-channel!
  "Emit health event to WebSocket channel for Emacs visibility."
  [{:keys [type severity message context recoverable? event-id timestamp]}]
  (channel/emit-event! :health-event
                       {:event-id event-id
                        :error-type type
                        :severity severity
                        :message message
                        :context context
                        :recoverable? recoverable?
                        :timestamp timestamp}))

(defn- persist-to-datascript!
  "Store health event in DataScript for post-mortem analysis.
   Note: DataScript doesn't allow nil values, so we filter them out."
  [{:keys [type severity message context recoverable? event-id timestamp]}]
  (let [db-conn (conn/get-conn)
        ;; Build entity map, excluding nil values (DataScript doesn't allow nil)
        base-entity {:health-event/id event-id
                     :health-event/type type
                     :health-event/severity severity
                     :health-event/message message
                     :health-event/timestamp timestamp
                     :health-event/recoverable? recoverable?}
        ;; Only add context if non-nil
        entity (if context
                 (assoc base-entity :health-event/context context)
                 base-entity)]
    (d/transact! db-conn [entity])))

;;; =============================================================================
;;; Public API
;;; =============================================================================

(defn emit-health-event!
  "Central function for ALL catastrophic events.

   event: {:type keyword (from error-types)
           :severity keyword (from severities)
           :message string
           :context map (optional, extra data)
           :recoverable? boolean}

   Actions:
   1. Validate event structure
   2. Record Prometheus metric (CLARITY-T: Telemetry first)
   3. Enrich with ID and timestamp
   4. Log with appropriate level
   5. Emit to WebSocket (if connected)
   6. Store in DataScript (for post-mortem)

   Returns: event-id string for correlation"
  [{:keys [type severity message context recoverable?] :as event}]
  ;; 1. Validate
  (validate-event! event)

  ;; 2. Record Prometheus metric (CLARITY-T: Telemetry first)
  (prom/inc-errors-total! type recoverable?)

  ;; 3. Enrich with ID and timestamp
  (let [event-id (conn/gen-id "health-event")
        timestamp (conn/now)
        enriched-event (assoc event
                              :event-id event-id
                              :timestamp timestamp)]

    ;; 4. Log with appropriate level
    (log-by-severity! enriched-event)

    ;; 5. Emit to WebSocket (fire and forget, don't fail if channel down)
    (try
      (emit-to-channel! enriched-event)
      (catch Exception e
        (log/debug "Failed to emit health event to channel" {:error (.getMessage e)})))

    ;; 6. Persist to DataScript
    (try
      (persist-to-datascript! enriched-event)
      (catch Exception e
        (log/warn "Failed to persist health event to DataScript"
                  {:event-id event-id :error (.getMessage e)})))

    ;; Return event-id for correlation
    event-id))

(defn get-recent-errors
  "Query recent errors from DataScript for debugging.

   Options:
   - :limit - Maximum number of errors to return (default: 20)
   - :type  - Filter by error type keyword
   - :since - Filter by timestamp (java.util.Date)"
  [& {:keys [limit type since] :or {limit 20}}]
  (let [db-conn (conn/get-conn)
        db @db-conn
        ;; Build query based on filters
        base-query '[:find [(pull ?e [*]) ...]
                     :in $
                     :where [?e :health-event/id _]]
        type-query (if type
                     '[:find [(pull ?e [*]) ...]
                       :in $ ?type
                       :where
                       [?e :health-event/id _]
                       [?e :health-event/type ?type]]
                     base-query)
        results (if type
                  (d/q type-query db type)
                  (d/q base-query db))
        ;; Sort by timestamp descending
        sorted (->> results
                    (sort-by :health-event/timestamp)
                    reverse)
        ;; Apply since filter if provided
        filtered (if since
                   (filter #(.after (:health-event/timestamp %) since) sorted)
                   sorted)]
    ;; Apply limit
    (take limit filtered)))

(defn health-summary
  "Get health summary: error counts by type, last error times, circuit states.

   Returns:
   {:error-counts {:harvest-failed 5, :chroma-unavailable 2, ...}
    :last-error-times {:harvest-failed #inst \"2026-01-16T...\", ...}
    :total-errors 7
    :severity-counts {:error 5, :warn 2, ...}}"
  []
  (let [db-conn (conn/get-conn)
        db @db-conn
        all-events (d/q '[:find [(pull ?e [*]) ...]
                          :where [?e :health-event/id _]]
                        db)]
    {:error-counts
     (->> all-events
          (group-by :health-event/type)
          (map (fn [[k v]] [k (count v)]))
          (into {}))

     :last-error-times
     (->> all-events
          (group-by :health-event/type)
          (map (fn [[k v]]
                 [k (->> v
                         (map :health-event/timestamp)
                         (sort)
                         last)]))
          (into {}))

     :severity-counts
     (->> all-events
          (group-by :health-event/severity)
          (map (fn [[k v]] [k (count v)]))
          (into {}))

     :total-errors
     (count all-events)}))

(comment
  ;; Usage examples

  ;; Emit a health event
  (emit-health-event! {:type :harvest-failed
                       :severity :error
                       :message "Failed to harvest wrap data"
                       :context {:agent-id "ling-123" :reason "Chroma timeout"}
                       :recoverable? true})

  ;; Get recent errors
  (get-recent-errors)
  (get-recent-errors :limit 5)
  (get-recent-errors :type :harvest-failed)

  ;; Get health summary
  (health-summary))
