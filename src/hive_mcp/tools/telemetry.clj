(ns hive-mcp.tools.telemetry
  "MCP tools for querying telemetry systems (Prometheus, Loki).

   Provides:
   - prometheus_query - PromQL queries for metrics
   - loki_query       - LogQL queries for logs

   CLARITY-T: Telemetry first - enables self-observability.
   CLARITY-Y: Graceful degradation on connection failures."
  (:require [hive-mcp.tools.core :refer [mcp-json]]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private default-prometheus-url
  "Default Prometheus API endpoint."
  "http://localhost:9090")

(def ^:private default-loki-url
  "Default Loki API endpoint."
  "http://localhost:3100")

(def ^:private http-timeout-ms
  "HTTP request timeout in milliseconds."
  10000)

;; =============================================================================
;; Prometheus HTTP API Client
;; =============================================================================

(defn- get-prometheus-url
  "Get Prometheus URL from env or use default."
  []
  (or (System/getenv "PROMETHEUS_URL") default-prometheus-url))

(defn- query-prometheus
  "Execute a PromQL query against Prometheus.

   Arguments:
     query      - PromQL query string
     time-range - Optional time range (e.g., \"1h\", \"30m\")

   Returns:
     Parsed JSON response from Prometheus API."
  [query & [{:keys [time-range]}]]
  (let [base-url (get-prometheus-url)
        ;; Use query_range for range queries, query for instant
        endpoint (if time-range
                   (str base-url "/api/v1/query_range")
                   (str base-url "/api/v1/query"))
        params (cond-> {:query query}
                 time-range (merge
                             (let [now (quot (System/currentTimeMillis) 1000)
                                   ;; Parse time range (e.g., "1h" -> 3600, "30m" -> 1800)
                                   duration-seconds (cond
                                                      (str/ends-with? time-range "h")
                                                      (* 3600 (parse-long (str/replace time-range #"h$" "")))
                                                      (str/ends-with? time-range "m")
                                                      (* 60 (parse-long (str/replace time-range #"m$" "")))
                                                      (str/ends-with? time-range "s")
                                                      (parse-long (str/replace time-range #"s$" ""))
                                                      :else 3600)]
                               {:start (- now duration-seconds)
                                :end now
                                :step (max 15 (quot duration-seconds 100))})))]
    (log/debug "Querying Prometheus:" endpoint params)
    (http/get endpoint
              {:query-params params
               :as :json
               :socket-timeout http-timeout-ms
               :connection-timeout http-timeout-ms})))

;; =============================================================================
;; Handler: prometheus_query
;; =============================================================================

(defn handle-prometheus-query
  "Query Prometheus using PromQL.

   Arguments:
     query      - PromQL query string (required)
     time_range - Optional time range (e.g., \"1h\", \"30m\", \"5m\")

   Examples:
     prometheus_query(query: \"up\")
     prometheus_query(query: \"rate(hive_mcp_events_total[5m])\")
     prometheus_query(query: \"hive_mcp_wave_success_rate\", time_range: \"1h\")

   Returns:
     {:status \"success\"
      :data {:resultType \"vector\" :result [...]}}"
  [{:keys [query time_range]}]
  (try
    (when (str/blank? query)
      (throw (ex-info "query parameter is required" {:error-type :validation})))

    (let [response (query-prometheus query {:time-range time_range})
          body (:body response)]
      (log/info "Prometheus query successful:" query)
      (mcp-json {:status "success"
                 :query query
                 :time_range time_range
                 :data body}))

    (catch clojure.lang.ExceptionInfo e
      (log/warn "Prometheus query validation error:" (.getMessage e))
      (mcp-json {:status "error"
                 :error (.getMessage e)
                 :error_type (name (or (:error-type (ex-data e)) :validation))}))

    (catch java.net.ConnectException e
      (log/warn "Prometheus connection failed:" (.getMessage e))
      (mcp-json {:status "error"
                 :error "Cannot connect to Prometheus"
                 :error_type "connection"
                 :details (.getMessage e)
                 :prometheus_url (get-prometheus-url)}))

    (catch java.net.SocketTimeoutException e
      (log/warn "Prometheus query timed out:" (.getMessage e))
      (mcp-json {:status "error"
                 :error "Prometheus query timed out"
                 :error_type "timeout"
                 :timeout_ms http-timeout-ms}))

    (catch Exception e
      (log/error e "Prometheus query failed")
      (mcp-json {:status "error"
                 :error (.getMessage e)
                 :error_type "unknown"}))))

;; =============================================================================
;; Loki HTTP API Client
;; =============================================================================

(defn- get-loki-url
  "Get Loki URL from env or use default."
  []
  (or (System/getenv "LOKI_URL") default-loki-url))

(defn parse-time-range
  "Parse a time range string to milliseconds.

   Supports: 'Nh' (hours), 'Nm' (minutes), 'Nd' (days), 'Ns' (seconds).

   Arguments:
     time-range - String like \"1h\", \"30m\", \"7d\"

   Returns:
     Duration in milliseconds, or nil if invalid."
  [time-range]
  (when (and time-range (not (str/blank? time-range)))
    (try
      (let [s (str/trim time-range)]
        (cond
          (str/ends-with? s "h")
          (* 3600000 (parse-long (str/replace s #"h$" "")))

          (str/ends-with? s "m")
          (* 60000 (parse-long (str/replace s #"m$" "")))

          (str/ends-with? s "d")
          (* 86400000 (parse-long (str/replace s #"d$" "")))

          (str/ends-with? s "s")
          (* 1000 (parse-long (str/replace s #"s$" "")))

          :else nil))
      (catch Exception _ nil))))

(defn validate-logql-query
  "Validate a LogQL query syntax.

   LogQL queries MUST contain a stream selector in curly braces: {label=\"value\"}

   Arguments:
     query - LogQL query string

   Returns:
     nil if valid, error string if invalid."
  [query]
  (cond
    (nil? query) "query parameter is required"
    (str/blank? query) "query cannot be empty"
    (not (str/includes? query "{")) "query must contain a stream selector (e.g., {job=\"hive-mcp\"})"
    :else nil))

(defn parse-loki-response
  "Parse Loki API response to a list of log entries.

   Loki returns:
   {:status \"success\"
    :data {:resultType \"streams\"
           :result [{:stream {:job \"...\" :level \"...\"}
                     :values [[timestamp-ns message] ...]}]}}

   We transform to:
   {:status :success
    :entries [{:timestamp <epoch-ms>
               :labels {:job \"...\" :level \"...\"}
               :message \"...\"}]}"
  [response]
  (try
    (if (= "success" (:status response))
      (let [streams (get-in response [:data :result] [])
            entries (for [{:keys [stream values]} streams
                          [timestamp-ns message] values]
                      {:timestamp (when timestamp-ns
                                    ;; Loki timestamps are nanoseconds
                                    (quot (parse-long timestamp-ns) 1000000))
                       :labels stream
                       :message message})]
        {:status :success
         :entries (vec entries)})
      {:status :error
       :error (or (:error response) "Unknown Loki error")})
    (catch Exception e
      {:status :error
       :error (.getMessage e)})))

(defn query-loki-http!
  "Execute a LogQL query against Loki HTTP API.

   Arguments:
     url - Full URL including query parameters

   Returns:
     Parsed JSON response from Loki API."
  [url]
  (let [response (http/get url
                           {:as :json
                            :socket-timeout http-timeout-ms
                            :connection-timeout http-timeout-ms})]
    (:body response)))

(defn- build-loki-query-url
  "Build Loki query URL with parameters.

   Arguments:
     base-url   - Loki base URL
     query      - LogQL query string
     limit      - Max entries to return
     time-range - Optional time range (e.g., \"1h\")

   Returns:
     Full URL string with query parameters."
  [base-url query limit time-range]
  (let [now-ns (* (System/currentTimeMillis) 1000000)
        duration-ms (parse-time-range time-range)
        params (cond-> {:query query
                        :limit (or limit 100)}
                 duration-ms (assoc :start (- now-ns (* duration-ms 1000000))
                                    :end now-ns))]
    (str base-url "/loki/api/v1/query_range?"
         (str/join "&" (map (fn [[k v]]
                              (str (name k) "=" (java.net.URLEncoder/encode (str v) "UTF-8")))
                            params)))))

;; Alias for test compatibility
(def build-loki-url build-loki-query-url)

;; =============================================================================
;; Handler: loki_query
;; =============================================================================

(defn handle-loki-query
  "Query Loki using LogQL to retrieve log entries.

   Arguments:
     query      - LogQL query string (required)
     limit      - Maximum number of entries to return (default: 100)
     time_range - Optional time range (e.g., \"1h\", \"30m\", \"7d\")

   Examples:
     loki_query(query: \"{job=\"hive-mcp\"}\")
     loki_query(query: \"{job=\"hive-mcp\"} |= \"error\"\", limit: 50)
     loki_query(query: \"{namespace=\"default\"}\", time_range: \"1h\")

   Returns:
     {:status \"success\"
      :entries [{:timestamp ... :labels {...} :message \"...\"}]}"
  [{:keys [query limit time_range]}]
  (try
    ;; Validate query
    (when-let [validation-error (validate-logql-query query)]
      (throw (ex-info validation-error {:error-type :validation})))

    (let [base-url (get-loki-url)
          url (build-loki-query-url base-url query limit time_range)
          _ (log/debug "Querying Loki:" url)
          response (query-loki-http! url)
          parsed (parse-loki-response response)]

      (log/info "Loki query successful:" query
                "- entries:" (count (:entries parsed)))

      (mcp-json {:status (name (:status parsed))
                 :query query
                 :limit (or limit 100)
                 :time_range time_range
                 :entries (:entries parsed)
                 :entry_count (count (:entries parsed))}))

    (catch clojure.lang.ExceptionInfo e
      (log/warn "Loki query validation error:" (.getMessage e))
      (mcp-json {:status "error"
                 :error (.getMessage e)
                 :error_type (name (or (:error-type (ex-data e)) :validation))}))

    (catch java.net.ConnectException e
      (log/warn "Loki connection failed:" (.getMessage e))
      (mcp-json {:status "error"
                 :error "Cannot connect to Loki"
                 :error_type "connection"
                 :details (.getMessage e)
                 :loki_url (get-loki-url)}))

    (catch java.net.SocketTimeoutException e
      (log/warn "Loki query timed out:" (.getMessage e))
      (mcp-json {:status "error"
                 :error "Loki query timed out"
                 :error_type "timeout"
                 :timeout_ms http-timeout-ms}))

    (catch Exception e
      (log/error e "Loki query failed")
      (mcp-json {:status "error"
                 :error (.getMessage e)
                 :error_type "unknown"}))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def tools
  "Telemetry MCP tool definitions."
  [{:name "prometheus_query"
    :description "Query Prometheus using PromQL. Returns metric data from the Prometheus HTTP API. Use for monitoring hive-mcp metrics, checking wave success rates, drone execution times, etc."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"
                                       :description "PromQL query string (e.g., 'up', 'rate(hive_mcp_events_total[5m])', 'hive_mcp_wave_success_rate')"}
                               :time_range {:type "string"
                                            :description "Optional time range for range queries (e.g., '1h', '30m', '5m'). If not specified, executes an instant query."}}
                  :required ["query"]}
    :handler handle-prometheus-query}

   {:name "loki_query"
    :description "Query Loki using LogQL to retrieve log entries. Returns log entries from the Loki HTTP API. Use for searching logs, debugging drone failures, tracing request flows, etc."
    :inputSchema {:type "object"
                  :properties {:query {:type "string"
                                       :description "LogQL query string. Must include stream selector (e.g., '{job=\"hive-mcp\"}', '{job=\"hive-mcp\"} |= \"error\"', '{namespace=\"default\"} |~ \"drone.*failed\"')"}
                               :limit {:type "integer"
                                       :description "Maximum number of entries to return (default: 100)"}
                               :time_range {:type "string"
                                            :description "Optional time range (e.g., '1h', '30m', '7d'). If not specified, queries recent logs."}}
                  :required ["query"]}
    :handler handle-loki-query}])

(comment
  ;; REPL testing - Prometheus
  (handle-prometheus-query {:query "up"})
  (handle-prometheus-query {:query "hive_mcp_wave_success_rate"})
  (handle-prometheus-query {:query "rate(hive_mcp_events_total[5m])" :time_range "1h"})

  ;; REPL testing - Loki
  (handle-loki-query {:query "{job=\"hive-mcp\"}"})
  (handle-loki-query {:query "{job=\"hive-mcp\"} |= \"error\"" :limit 50})
  (handle-loki-query {:query "{namespace=\"default\"}" :time_range "1h"}))
