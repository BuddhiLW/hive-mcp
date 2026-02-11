(ns hive-mcp.agent.drone.errors
  "Error classification and retry logic for drone agents."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def error-types
  "Canonical error categories for drone telemetry."
  #{:nrepl-connection
    :nrepl-timeout
    :nrepl-eval-error
    :validation
    :conflict
    :execution
    :exception})

(defn- get-root-cause
  "Extract root cause from nested exceptions."
  [ex]
  (loop [e ex]
    (let [java-cause (when (instance? Throwable e) (.getCause e))
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

(defn classify-nrepl-error
  "Classify an exception into a structured error category keyword."
  [ex]
  (let [ex-data-map (ex-data ex)
        message (or (ex-message ex) "")
        message-lower (str/lower-case message)
        root-cause (get-root-cause ex)
        root-type (exception-type-name root-cause)]
    (cond
      (= :validation (:type ex-data-map))
      :validation

      (= :conflict (:type ex-data-map))
      :conflict

      (or (= "ConnectException" root-type)
          (str/includes? message-lower "connection refused")
          (str/includes? message-lower "cider not connected")
          (str/includes? message-lower "no nrepl connection")
          (str/includes? message-lower "failed to connect"))
      :nrepl-connection

      (or (= "SocketTimeoutException" root-type)
          (= "TimeoutException" root-type)
          (str/includes? message-lower "timed out")
          (str/includes? message-lower "timeout"))
      :nrepl-timeout

      (or (str/includes? message-lower "compilerexception")
          (str/includes? message-lower "syntax error")
          (str/includes? message-lower "classnotfoundexception")
          (str/includes? message-lower "nullpointerexception")
          (str/includes? message-lower "arithmeticexception")
          (str/includes? message-lower "unable to resolve symbol"))
      :nrepl-eval-error

      :else
      :exception)))

(defn structure-error
  "Wrap an exception with structured error data for telemetry."
  [ex]
  (let [error-type (classify-nrepl-error ex)
        message (or (ex-message ex) (str ex))
        stacktrace (when ex
                     (let [sw (java.io.StringWriter.)
                           pw (java.io.PrintWriter. sw)]
                       (.printStackTrace ex pw)
                       (let [trace (str sw)]
                         (subs trace 0 (min 2000 (count trace))))))
        ex-data-map (ex-data ex)]
    {:error-type error-type
     :message message
     :stacktrace stacktrace
     :ex-data ex-data-map}))

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
  "Create a structured nREPL error response with context and suggestions."
  [error-type details]
  {:error-type error-type
   :drone-id (:drone-id details)
   :port (:port details)
   :timeout-ms (:timeout-ms details)
   :message (get error-messages error-type "Unknown nREPL error")
   :suggestion (get error-suggestions error-type "Check nREPL server status")})

(def transient-error-types
  "Errors that are transient and should be retried."
  #{:rate-limit
    :timeout
    :network
    :server-error
    :overloaded
    :nrepl-timeout})

(def permanent-error-types
  "Errors that are permanent and should NOT be retried."
  #{:auth-error
    :invalid-request
    :validation
    :conflict
    :not-found
    :nrepl-eval-error})

(def partial-error-types
  "Errors where partial results may be salvageable."
  #{:incomplete
    :truncated
    :empty-response})

(defn classify-http-error
  "Classify HTTP status codes into error type keywords."
  [status error-body]
  (let [error-msg (or (get-in error-body [:error :message]) "")
        error-msg-lower (str/lower-case error-msg)]
    (cond
      (= status 429)
      :rate-limit

      (#{401 403} status)
      :auth-error

      (= status 400)
      :invalid-request

      (= status 404)
      :not-found

      (>= status 500)
      (cond
        (str/includes? error-msg-lower "overloaded") :overloaded
        (str/includes? error-msg-lower "capacity") :overloaded
        :else :server-error)

      :else :exception)))

(defn classify-exception
  "Classify an exception for retry decisions."
  [ex]
  (let [ex-data-map (ex-data ex)
        message (or (ex-message ex) "")
        message-lower (str/lower-case message)
        status (:status ex-data-map)]
    (cond
      status
      (classify-http-error status (:error ex-data-map))

      (or (str/includes? message-lower "rate limit")
          (str/includes? message-lower "too many requests")
          (str/includes? message-lower "429"))
      :rate-limit

      (or (str/includes? message-lower "timed out")
          (str/includes? message-lower "timeout")
          (str/includes? message-lower "deadline exceeded"))
      :timeout

      (or (str/includes? message-lower "connection refused")
          (str/includes? message-lower "network")
          (str/includes? message-lower "socket")
          (str/includes? message-lower "unreachable"))
      :network

      (or (str/includes? message-lower "unauthorized")
          (str/includes? message-lower "forbidden")
          (str/includes? message-lower "api key"))
      :auth-error

      (or (str/includes? message-lower "empty response")
          (str/includes? message-lower "no content"))
      :empty-response

      (or (str/includes? message-lower "overloaded")
          (str/includes? message-lower "capacity"))
      :overloaded

      :else
      (classify-nrepl-error ex))))

(defn transient-error?
  "Check if an error is transient and should be retried."
  [error-type]
  (contains? transient-error-types error-type))

(defn permanent-error?
  "Check if an error is permanent and should NOT be retried."
  [error-type]
  (contains? permanent-error-types error-type))

(defn partial-error?
  "Check if an error may have salvageable partial results."
  [error-type]
  (contains? partial-error-types error-type))

(defonce retry-metrics
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

(def default-retry-config
  "Default retry configuration."
  {:max-retries 3
   :initial-backoff-ms 1000
   :max-backoff-ms 30000
   :backoff-multiplier 2
   :jitter-factor 0.1})

(defn- calculate-backoff
  "Calculate backoff delay with exponential increase and jitter."
  [attempt {:keys [initial-backoff-ms max-backoff-ms backoff-multiplier jitter-factor]}]
  (let [base-delay (* initial-backoff-ms (Math/pow backoff-multiplier attempt))
        capped-delay (min base-delay max-backoff-ms)
        jitter-range (* capped-delay jitter-factor)
        jitter (- (* 2 jitter-range (Math/random)) jitter-range)]
    (long (max 0 (+ capped-delay jitter)))))

(defn with-retry
  "Execute a function with retry logic and exponential backoff."
  [f {:keys [max-retries on-retry on-model-switch model-switch-fn drone-id]
      :as opts
      :or {max-retries 3}}]
  (let [config (merge default-retry-config opts)]
    (loop [attempt 0
           last-error nil
           current-model nil]
      (if (> attempt max-retries)
        (let [error-type (when last-error (classify-exception last-error))]
          (record-retry-failure! error-type)
          (throw (ex-info (str "Retry exhausted after " max-retries " attempts: "
                               (ex-message last-error))
                          {:attempts (inc attempt)
                           :last-error-type error-type
                           :drone-id drone-id
                           :original-error last-error}
                          last-error)))

        (let [result (try
                       {:success true :value (f)}
                       (catch Exception e
                         {:success false :error e}))]
          (if (:success result)
            (do
              (when (pos? attempt)
                (record-retry-success! (when last-error (classify-exception last-error))))
              (:value result))

            (let [error (:error result)
                  error-type (classify-exception error)]
              (cond
                (permanent-error? error-type)
                (do
                  (record-retry-failure! error-type)
                  (throw error))

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

                (transient-error? error-type)
                (do
                  (record-retry! error-type)
                  (let [delay-ms (calculate-backoff attempt config)]
                    (when on-retry
                      (on-retry attempt error delay-ms))
                    (Thread/sleep delay-ms)
                    (recur (inc attempt) error current-model)))

                (zero? attempt)
                (do
                  (record-retry! error-type)
                  (let [delay-ms (calculate-backoff attempt config)]
                    (when on-retry
                      (on-retry attempt error delay-ms))
                    (Thread/sleep delay-ms)
                    (recur (inc attempt) error current-model)))

                :else
                (do
                  (record-retry-failure! error-type)
                  (throw error))))))))))

(defn extract-partial-result
  "Attempt to extract useful data from a partial/failed response."
  [response error]
  (let [ex-data-map (ex-data error)
        error-type (classify-exception error)]
    (cond
      (= error-type :empty-response)
      {:salvaged nil
       :gap "Model returned empty response"}

      (:partial-content ex-data-map)
      {:salvaged (:partial-content ex-data-map)
       :gap "Response was truncated"}

      (and response (map? response) (:content response))
      {:salvaged (:content response)
       :gap "Response completed but validation failed"}

      :else
      {:salvaged nil
       :gap (str "No partial data available: " (ex-message error))})))

(defn suggest-recovery-action
  "Suggest a recovery action based on error type."
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

    {:action :escalate
     :description "Escalate to coordinator for review"
     :auto false}))
