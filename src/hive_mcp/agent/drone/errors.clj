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

;;; ============================================================
;;; Error Categorization for Retry Logic
;;; ============================================================

(def transient-error-types
  "Errors that are transient and should be retried.
   These typically resolve themselves with time or model switching."
  #{:rate-limit      ; 429 - retry with backoff or switch model
    :timeout         ; Request timed out - retry or simplify
    :network         ; Network connectivity issues
    :server-error    ; 500+ - server-side issues
    :overloaded      ; Model overloaded - switch or wait
    :nrepl-timeout}) ; nREPL timeout - retry

(def permanent-error-types
  "Errors that are permanent and should NOT be retried.
   These require user intervention or are fundamentally unfixable."
  #{:auth-error      ; 401/403 - invalid API key
    :invalid-request ; 400 - malformed request
    :validation      ; Input validation failed
    :conflict        ; File conflicts
    :not-found       ; Resource not found
    :nrepl-eval-error}) ; Code error - won't fix itself

(def partial-error-types
  "Errors where partial results may be salvageable.
   Attempt to extract useful data before failing."
  #{:incomplete      ; Model returned partial response
    :truncated       ; Response was cut off
    :empty-response}) ; Empty but valid response

(defn classify-http-error
  "Classify HTTP status codes into error types.

   Returns keyword error type for the given HTTP status."
  [status error-body]
  (let [error-msg (or (get-in error-body [:error :message]) "")
        error-msg-lower (str/lower-case error-msg)]
    (cond
      ;; Rate limiting
      (= status 429)
      :rate-limit

      ;; Auth errors
      (#{401 403} status)
      :auth-error

      ;; Bad request
      (= status 400)
      :invalid-request

      ;; Not found
      (= status 404)
      :not-found

      ;; Server errors
      (>= status 500)
      (cond
        (str/includes? error-msg-lower "overloaded") :overloaded
        (str/includes? error-msg-lower "capacity") :overloaded
        :else :server-error)

      ;; Default
      :else :exception)))

(defn classify-exception
  "Classify an exception for retry decisions.

   Returns keyword error type based on exception analysis."
  [ex]
  (let [ex-data-map (ex-data ex)
        message (or (ex-message ex) "")
        message-lower (str/lower-case message)
        status (:status ex-data-map)]
    (cond
      ;; HTTP status in ex-data
      status
      (classify-http-error status (:error ex-data-map))

      ;; Rate limit messages
      (or (str/includes? message-lower "rate limit")
          (str/includes? message-lower "too many requests")
          (str/includes? message-lower "429"))
      :rate-limit

      ;; Timeout messages
      (or (str/includes? message-lower "timed out")
          (str/includes? message-lower "timeout")
          (str/includes? message-lower "deadline exceeded"))
      :timeout

      ;; Network errors
      (or (str/includes? message-lower "connection refused")
          (str/includes? message-lower "network")
          (str/includes? message-lower "socket")
          (str/includes? message-lower "unreachable"))
      :network

      ;; Auth errors
      (or (str/includes? message-lower "unauthorized")
          (str/includes? message-lower "forbidden")
          (str/includes? message-lower "api key"))
      :auth-error

      ;; Empty/incomplete response
      (or (str/includes? message-lower "empty response")
          (str/includes? message-lower "no content"))
      :empty-response

      ;; Server overloaded
      (or (str/includes? message-lower "overloaded")
          (str/includes? message-lower "capacity"))
      :overloaded

      ;; Fall back to nREPL classification
      :else
      (classify-nrepl-error ex))))

(defn transient-error?
  "Check if an error is transient and should be retried.

   Arguments:
     error-type - Keyword error type from classify-exception

   Returns true if the error is likely to resolve with retry."
  [error-type]
  (contains? transient-error-types error-type))

(defn permanent-error?
  "Check if an error is permanent and should NOT be retried.

   Arguments:
     error-type - Keyword error type from classify-exception

   Returns true if the error requires user intervention."
  [error-type]
  (contains? permanent-error-types error-type))

(defn partial-error?
  "Check if an error may have salvageable partial results.

   Arguments:
     error-type - Keyword error type from classify-exception

   Returns true if partial results may be extractable."
  [error-type]
  (contains? partial-error-types error-type))

;;; ============================================================
;;; Retry Metrics
;;; ============================================================

(defonce retry-metrics
  ;; Metrics for retry operations. Thread-safe via atom.
  (atom {:total-retries 0
         :successful-retries 0
         :failed-after-retries 0
         :model-switches 0
         :by-error-type {}}))

(defn reset-retry-metrics!
  "Reset all retry metrics to zero."
  []
  (reset! retry-metrics {:total-retries 0
                         :successful-retries 0
                         :failed-after-retries 0
                         :model-switches 0
                         :by-error-type {}}))

(defn get-retry-metrics
  "Get current retry metrics snapshot."
  []
  @retry-metrics)

(defn- record-retry!
  "Record a retry attempt."
  [error-type]
  (swap! retry-metrics
         (fn [m]
           (-> m
               (update :total-retries inc)
               (update-in [:by-error-type error-type :attempts] (fnil inc 0))))))

(defn- record-retry-success!
  "Record a successful retry."
  [error-type]
  (swap! retry-metrics
         (fn [m]
           (-> m
               (update :successful-retries inc)
               (update-in [:by-error-type error-type :successes] (fnil inc 0))))))

(defn- record-retry-failure!
  "Record a failed retry (exhausted retries)."
  [error-type]
  (swap! retry-metrics
         (fn [m]
           (-> m
               (update :failed-after-retries inc)
               (update-in [:by-error-type error-type :failures] (fnil inc 0))))))

(defn- record-model-switch!
  "Record a model switch during retry."
  []
  (swap! retry-metrics update :model-switches inc))

;;; ============================================================
;;; Retry Logic with Exponential Backoff
;;; ============================================================

(def default-retry-config
  "Default retry configuration."
  {:max-retries 3
   :initial-backoff-ms 1000
   :max-backoff-ms 30000
   :backoff-multiplier 2
   :jitter-factor 0.1})

(defn- calculate-backoff
  "Calculate backoff delay with exponential increase and jitter.

   Arguments:
     attempt    - Current attempt number (0-indexed)
     config     - Retry configuration map

   Returns delay in milliseconds."
  [attempt {:keys [initial-backoff-ms max-backoff-ms backoff-multiplier jitter-factor]}]
  (let [;; Exponential backoff: base * multiplier^attempt
        base-delay (* initial-backoff-ms (Math/pow backoff-multiplier attempt))
        ;; Cap at max
        capped-delay (min base-delay max-backoff-ms)
        ;; Add jitter (±jitter-factor)
        jitter-range (* capped-delay jitter-factor)
        jitter (- (* 2 jitter-range (Math/random)) jitter-range)]
    (long (max 0 (+ capped-delay jitter)))))

(defn with-retry
  "Execute a function with retry logic and exponential backoff.

   Arguments:
     f      - Function to execute (no args)
     opts   - Options map:
              :max-retries       - Maximum retry attempts (default: 3)
              :initial-backoff-ms - Initial delay in ms (default: 1000)
              :max-backoff-ms    - Maximum delay in ms (default: 30000)
              :backoff-multiplier - Exponential multiplier (default: 2)
              :jitter-factor     - Random jitter factor (default: 0.1)
              :on-retry          - Callback (fn [attempt error delay-ms])
              :on-model-switch   - Callback (fn [old-model new-model]) for model switching
              :model-switch-fn   - Function to switch models on rate-limit

   Returns the result of f, or throws the last exception after all retries.

   CLARITY-Y: Yields safe failure after exhausting retries.
   CLARITY-T: Records metrics for all retry operations."
  [f {:keys [max-retries on-retry on-model-switch model-switch-fn drone-id]
      :as opts
      :or {max-retries 3}}]
  (let [config (merge default-retry-config opts)]
    (loop [attempt 0
           last-error nil
           current-model nil]
      (if (> attempt max-retries)
        ;; Exhausted retries
        (let [error-type (when last-error (classify-exception last-error))]
          (record-retry-failure! error-type)
          (throw (ex-info (str "Retry exhausted after " max-retries " attempts: "
                               (ex-message last-error))
                          {:attempts (inc attempt)
                           :last-error-type error-type
                           :drone-id drone-id
                           :original-error last-error}
                          last-error)))

        ;; Attempt execution
        (let [result (try
                       {:success true :value (f)}
                       (catch Exception e
                         {:success false :error e}))]
          (if (:success result)
            ;; Success
            (do
              (when (pos? attempt)
                (record-retry-success! (when last-error (classify-exception last-error))))
              (:value result))

            ;; Failed - check if retryable
            (let [error (:error result)
                  error-type (classify-exception error)]
              (cond
                ;; Permanent error - fail fast
                (permanent-error? error-type)
                (do
                  (record-retry-failure! error-type)
                  (throw error))

                ;; Rate limit with model switching
                (and (= error-type :rate-limit) model-switch-fn)
                (let [new-model (model-switch-fn current-model)]
                  (when on-model-switch
                    (on-model-switch current-model new-model))
                  (record-model-switch!)
                  (record-retry! error-type)
                  (let [delay-ms (calculate-backoff attempt config)]
                    (when on-retry
                      (on-retry attempt error delay-ms))
                    (Thread/sleep delay-ms)
                    (recur (inc attempt) error new-model)))

                ;; Transient error - retry with backoff
                (transient-error? error-type)
                (do
                  (record-retry! error-type)
                  (let [delay-ms (calculate-backoff attempt config)]
                    (when on-retry
                      (on-retry attempt error delay-ms))
                    (Thread/sleep delay-ms)
                    (recur (inc attempt) error current-model)))

                ;; Unknown error - conservative retry once
                (zero? attempt)
                (do
                  (record-retry! error-type)
                  (let [delay-ms (calculate-backoff attempt config)]
                    (when on-retry
                      (on-retry attempt error delay-ms))
                    (Thread/sleep delay-ms)
                    (recur (inc attempt) error current-model)))

                ;; Unknown error after retry - fail
                :else
                (do
                  (record-retry-failure! error-type)
                  (throw error))))))))))

;;; ============================================================
;;; Recovery Strategies
;;; ============================================================

(defn extract-partial-result
  "Attempt to extract useful data from a partial/failed response.

   Arguments:
     response - The response that may contain partial data
     error    - The exception that occurred

   Returns map with:
     :salvaged - Any data that could be extracted
     :gap      - Description of what was missing"
  [response error]
  (let [ex-data-map (ex-data error)
        error-type (classify-exception error)]
    (cond
      ;; Empty response - nothing to salvage
      (= error-type :empty-response)
      {:salvaged nil
       :gap "Model returned empty response"}

      ;; Check for partial content in ex-data
      (:partial-content ex-data-map)
      {:salvaged (:partial-content ex-data-map)
       :gap "Response was truncated"}

      ;; Check response itself for partial data
      (and response (map? response) (:content response))
      {:salvaged (:content response)
       :gap "Response completed but validation failed"}

      ;; Nothing salvageable
      :else
      {:salvaged nil
       :gap (str "No partial data available: " (ex-message error))})))

(defn suggest-recovery-action
  "Suggest a recovery action based on error type.

   Arguments:
     error-type - Classified error type

   Returns map with:
     :action      - Keyword action type
     :description - Human-readable description
     :auto        - Whether this can be automated"
  [error-type]
  (case error-type
    :rate-limit
    {:action :switch-model
     :description "Switch to alternate free-tier model"
     :auto true}

    :timeout
    {:action :simplify-task
     :description "Reduce task complexity and retry"
     :auto false}

    :overloaded
    {:action :wait-and-retry
     :description "Wait for model capacity and retry"
     :auto true}

    :network
    {:action :retry
     :description "Retry with exponential backoff"
     :auto true}

    :empty-response
    {:action :retry-with-different-model
     :description "Model may be struggling - try alternate"
     :auto true}

    :auth-error
    {:action :check-api-key
     :description "Verify OPENROUTER_API_KEY environment variable"
     :auto false}

    :invalid-request
    {:action :check-request
     :description "Review task format and parameters"
     :auto false}

    ;; Default
    {:action :escalate
     :description "Escalate to coordinator for review"
     :auto false}))
