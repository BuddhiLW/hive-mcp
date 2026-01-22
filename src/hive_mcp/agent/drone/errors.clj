(ns hive-mcp.agent.drone.errors
  "Error classification for drone agents.

   CLARITY-Y (Yield Safe Failure): Named error types enable graceful degradation.
   CLARITY-R (Represented Intent): Error categories represent semantic intent.
   CLARITY-T (Telemetry First): Structured errors for Prometheus/Loki integration.

   Extracted from agent/drone.clj for 200 LOC compliance."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Error Type Definitions
;;; ============================================================

(def error-types
  "Canonical error categories for drone telemetry.

   Used by Prometheus drones_failed_total metric with error-type label."
  #{:nrepl-connection  ; Connection failures (refused, not connected)
    :nrepl-timeout     ; Timeouts (socket timeout, evaluation timeout)
    :nrepl-eval-error  ; Evaluation errors (syntax, runtime, compiler)
    :validation        ; Input validation errors
    :conflict          ; File conflict errors
    :execution         ; General execution errors
    :exception})       ; Unknown/fallback category

;;; ============================================================
;;; Internal Helpers
;;; ============================================================

(defn- get-root-cause
  "Extract root cause from nested exceptions.
   Checks both Java .getCause() and ex-data :cause key."
  [ex]
  (loop [e ex]
    (let [;; Check Java cause first
          java-cause (when (instance? Throwable e) (.getCause e))
          ;; Fall back to ex-data :cause
          ex-data-cause (when (instance? clojure.lang.IExceptionInfo e)
                          (:cause (ex-data e)))
          cause (or java-cause ex-data-cause)]
      (if cause
        (recur cause)
        e))))

(defn- exception-type-name
  "Get the simple class name of an exception."
  [ex]
  (when ex
    (.getSimpleName (class ex))))

;;; ============================================================
;;; Error Classification
;;; ============================================================

(defn classify-nrepl-error
  "Classify an exception into structured error categories.

   Returns one of:
   - :nrepl-connection - Connection failures (refused, not connected)
   - :nrepl-timeout    - Timeouts (socket timeout, evaluation timeout)
   - :nrepl-eval-error - Evaluation errors (syntax, runtime, compiler)
   - :validation       - Input validation errors
   - :conflict         - File conflict errors
   - :execution        - General execution errors
   - :exception        - Unknown/fallback category

   Used by Prometheus drones_failed_total metric with error-type label."
  [ex]
  (let [ex-data-map (ex-data ex)
        message (or (ex-message ex) "")
        message-lower (str/lower-case message)
        root-cause (get-root-cause ex)
        root-type (exception-type-name root-cause)]
    (cond
      ;; Check ex-data for explicit type
      (= :validation (:type ex-data-map))
      :validation

      (= :conflict (:type ex-data-map))
      :conflict

      ;; Connection errors
      (or (= "ConnectException" root-type)
          (str/includes? message-lower "connection refused")
          (str/includes? message-lower "cider not connected")
          (str/includes? message-lower "no nrepl connection")
          (str/includes? message-lower "failed to connect"))
      :nrepl-connection

      ;; Timeout errors
      (or (= "SocketTimeoutException" root-type)
          (= "TimeoutException" root-type)
          (str/includes? message-lower "timed out")
          (str/includes? message-lower "timeout"))
      :nrepl-timeout

      ;; Evaluation/compilation errors
      (or (str/includes? message-lower "compilerexception")
          (str/includes? message-lower "syntax error")
          (str/includes? message-lower "classnotfoundexception")
          (str/includes? message-lower "nullpointerexception")
          (str/includes? message-lower "arithmeticexception")
          (str/includes? message-lower "unable to resolve symbol"))
      :nrepl-eval-error

      ;; Fallback
      :else
      :exception)))

;;; ============================================================
;;; Error Structuring
;;; ============================================================

(defn structure-error
  "Wrap an exception with structured error data for telemetry.

   Returns a map with:
   - :error-type  - Classified error category (keyword)
   - :message     - Human-readable error message
   - :stacktrace  - Stack trace as string (truncated to 2000 chars)
   - :ex-data     - Original ex-data from the exception (if any)

   Used for Prometheus metrics and debugging."
  [ex]
  (let [error-type (classify-nrepl-error ex)
        message (or (ex-message ex) (str ex))
        stacktrace (when ex
                     (let [sw (java.io.StringWriter.)
                           pw (java.io.PrintWriter. sw)]
                       (.printStackTrace ex pw)
                       (let [trace (str sw)]
                         ;; Truncate to avoid metric cardinality explosion
                         (subs trace 0 (min 2000 (count trace))))))
        ex-data-map (ex-data ex)]
    {:error-type error-type
     :message message
     :stacktrace stacktrace
     :ex-data ex-data-map}))

;;; ============================================================
;;; Error Response Creation
;;; ============================================================

(def ^:private error-messages
  "Human-readable messages for each error type."
  {:nrepl-connection "Failed to connect to nREPL server"
   :nrepl-timeout "nREPL evaluation timed out"
   :nrepl-eval-error "nREPL evaluation failed"
   :validation-failed "Input validation failed"})

(def ^:private error-suggestions
  "Actionable suggestions for each error type."
  {:nrepl-connection "Check if nREPL server is running on port"
   :nrepl-timeout "Increase timeout or simplify evaluation"
   :nrepl-eval-error "Check code syntax and dependencies"
   :validation-failed "Verify input parameters match expected schema"})

(defn make-nrepl-error
  "Create a structured nREPL error response with context and suggestions.

   Arguments:
     error-type - One of :nrepl-connection, :nrepl-timeout, :nrepl-eval-error
     details    - Map containing contextual details:
                  - :drone-id   - ID of the drone that encountered the error
                  - :port       - nREPL port (if applicable)
                  - :timeout-ms - Timeout value (if applicable)

   Returns a map suitable for telemetry and user feedback:
   - :error-type  - The classified error type keyword
   - :drone-id    - Drone identifier for correlation
   - :port        - nREPL port (for connection errors)
   - :timeout-ms  - Timeout value (for timeout errors)
   - :message     - Human-readable error message
   - :suggestion  - Actionable suggestion for resolution

   CLARITY-T: Structured error telemetry for Prometheus/Loki integration."
  [error-type details]
  {:error-type error-type
   :drone-id (:drone-id details)
   :port (:port details)
   :timeout-ms (:timeout-ms details)
   :message (get error-messages error-type "Unknown nREPL error")
   :suggestion (get error-suggestions error-type "Check nREPL server status")})
