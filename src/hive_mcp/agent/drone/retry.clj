(ns hive-mcp.agent.drone.retry
  "Retry logic for drone agents with exponential backoff.

   CLARITY-Y (Yield Safe Failure): Graceful degradation through smart retries.
   CLARITY-T (Telemetry First): All retries are logged for observability.

   Error Categories:
     :transient  - Rate limits, timeouts, temporary failures (can retry)
     :permanent  - Auth errors, invalid requests (fail fast)
     :unknown    - Unclassified (cautious retry with limit)

   Recovery Strategies:
     - Rate limit: Wait + retry with alternative model
     - Timeout: Reduce complexity/max-steps + retry
     - Auth error: Fail fast with actionable message
     - Model error: Try fallback model"
  (:require [hive-mcp.agent.config :as config]
            [hive-mcp.telemetry.prometheus :as prom]
            [taoensso.timbre :as log]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Error Classification
;;; ============================================================

(defn rate-limit?
  "Check if exception indicates a rate limit error."
  [ex]
  (let [data (ex-data ex)
        msg (str (ex-message ex))]
    (or (= 429 (:status data))
        (str/includes? (str/lower-case msg) "rate limit")
        (str/includes? (str/lower-case msg) "too many requests")
        (str/includes? (str/lower-case msg) "quota exceeded"))))

(defn timeout?
  "Check if exception indicates a timeout error."
  [ex]
  (let [msg (str (ex-message ex))
        cause (.getCause ex)]
    (or (instance? java.net.SocketTimeoutException ex)
        (instance? java.net.SocketTimeoutException cause)
        (str/includes? (str/lower-case msg) "timed out")
        (str/includes? (str/lower-case msg) "timeout"))))

(defn auth-error?
  "Check if exception indicates an authentication error."
  [ex]
  (let [data (ex-data ex)
        msg (str (ex-message ex))]
    (or (= 401 (:status data))
        (= 403 (:status data))
        (str/includes? (str/lower-case msg) "unauthorized")
        (str/includes? (str/lower-case msg) "invalid api key")
        (str/includes? (str/lower-case msg) "authentication")
        (str/includes? (str/lower-case msg) "forbidden"))))

(defn model-error?
  "Check if exception indicates a model-specific error (model unavailable, etc)."
  [ex]
  (let [data (ex-data ex)
        msg (str (ex-message ex))]
    (or (= 404 (:status data))
        (str/includes? (str/lower-case msg) "model not found")
        (str/includes? (str/lower-case msg) "model is not available")
        (str/includes? (str/lower-case msg) "model does not exist"))))

(defn server-error?
  "Check if exception indicates a server error (5xx)."
  [ex]
  (let [data (ex-data ex)]
    (and (:status data)
         (<= 500 (:status data) 599))))

(defn empty-response?
  "Check if exception indicates an empty response from the model."
  [ex]
  (let [msg (str (ex-message ex))]
    (str/includes? (str/lower-case msg) "empty response")))

(defn classify-error
  "Classify an exception into error categories.

   Returns:
     :transient  - Can retry (rate limits, timeouts, server errors)
     :permanent  - Don't retry (auth errors, invalid requests)
     :model-fallback - Try alternative model
     :unknown    - Unclassified (limited retry)"
  [ex]
  (cond
    (rate-limit? ex) :transient
    (timeout? ex) :transient
    (server-error? ex) :transient
    (empty-response? ex) :model-fallback
    (model-error? ex) :model-fallback
    (auth-error? ex) :permanent
    :else :unknown))

(defn error-category->label
  "Convert error category to Prometheus-friendly label."
  [category]
  (name category))

;;; ============================================================
;;; Retry Configuration
;;; ============================================================

(def default-retry-opts
  "Default retry configuration."
  {:max-retries 3
   :initial-delay-ms 1000      ; 1 second
   :max-delay-ms 30000         ; 30 seconds
   :backoff-multiplier 2.0
   :jitter-factor 0.2})        ; Add up to 20% random jitter

(defn calculate-delay
  "Calculate delay for retry attempt with exponential backoff and jitter.

   Arguments:
     attempt            - Current retry attempt (0-indexed)
     initial-delay-ms   - Initial delay in milliseconds
     max-delay-ms       - Maximum delay cap
     backoff-multiplier - Multiplier for each attempt
     jitter-factor      - Randomness factor (0.0 - 1.0)

   Returns:
     Delay in milliseconds with jitter applied."
  [attempt {:keys [initial-delay-ms max-delay-ms backoff-multiplier jitter-factor]
            :or {initial-delay-ms 1000
                 max-delay-ms 30000
                 backoff-multiplier 2.0
                 jitter-factor 0.2}}]
  (let [base-delay (* initial-delay-ms (Math/pow backoff-multiplier attempt))
        capped-delay (min base-delay max-delay-ms)
        jitter (* capped-delay jitter-factor (rand))]
    (long (+ capped-delay jitter))))

;;; ============================================================
;;; Model Fallback
;;; ============================================================

(defn get-fallback-model
  "Get a fallback model for the given model/preset.

   Returns nil if no fallback available.

   Strategy:
   - If using :coding model, try :coding-alt
   - If using :coding-alt, try :docs (simpler tasks)
   - Otherwise return nil (no fallback)"
  [current-model preset]
  (let [task-type (config/preset->task-type (or preset "drone-worker"))
        alt-task-type (case task-type
                        :coding :coding-alt
                        :coding-alt :docs
                        nil)]
    (when alt-task-type
      (let [fallback (config/get-model alt-task-type)]
        (when (and fallback (not= fallback current-model))
          fallback)))))

;;; ============================================================
;;; Recovery Strategies
;;; ============================================================

(defn recovery-strategy
  "Determine recovery strategy based on error classification.

   Returns a map with:
     :action       - :retry, :retry-with-fallback, :fail
     :delay-ms     - Delay before retry (if retrying)
     :model        - Alternative model to use (if fallback)
     :max-steps    - Reduced max-steps (for timeout recovery)
     :reason       - Human-readable explanation"
  [ex attempt opts]
  (let [category (classify-error ex)
        delay-ms (calculate-delay attempt opts)
        {:keys [max-retries]} (merge default-retry-opts opts)
        within-retries? (< attempt max-retries)]
    (case category
      :transient
      (if within-retries?
        {:action :retry
         :delay-ms (if (rate-limit? ex)
                     (max delay-ms 5000) ; Min 5s for rate limits
                     delay-ms)
         :reason (if (rate-limit? ex)
                   "Rate limited - backing off"
                   "Transient error - retrying")}
        {:action :fail
         :reason "Max retries exceeded for transient error"})

      :model-fallback
      (let [current-model (:model opts)
            fallback (get-fallback-model current-model (:preset opts))]
        (if (and fallback within-retries?)
          {:action :retry-with-fallback
           :model fallback
           :delay-ms delay-ms
           :reason (str "Model error - trying fallback: " fallback)}
          {:action :fail
           :reason "No fallback model available"}))

      :permanent
      {:action :fail
       :reason (str "Permanent error: " (ex-message ex))}

      ;; :unknown - cautious retry with reduced attempts
      (if (< attempt (min 2 max-retries))
        {:action :retry
         :delay-ms delay-ms
         :reason "Unknown error - cautious retry"}
        {:action :fail
         :reason "Unknown error - max cautious retries exceeded"}))))

;;; ============================================================
;;; Core Retry Logic
;;; ============================================================

(defn with-retry
  "Execute function f with retry logic and recovery strategies.

   Arguments:
     f    - Function to execute (takes a map of options)
     opts - Options map:
            :max-retries       - Max retry attempts (default: 3)
            :initial-delay-ms  - Initial backoff delay (default: 1000)
            :max-delay-ms      - Maximum backoff delay (default: 30000)
            :backoff-multiplier - Exponential multiplier (default: 2.0)
            :jitter-factor     - Random jitter (default: 0.2)
            :model             - Current model name
            :preset            - Preset name for fallback lookup
            :drone-id          - Drone ID for telemetry
            :task-id           - Task ID for telemetry
            :on-retry          - Callback (fn [attempt ex strategy])

   Returns:
     Result from f on success.

   Throws:
     Original exception after all retries exhausted."
  [f opts]
  (let [merged-opts (merge default-retry-opts opts)
        {:keys [max-retries drone-id task-id on-retry]} merged-opts]
    (loop [attempt 0
           current-opts merged-opts
           last-exception nil]
      (if (> attempt max-retries)
        ;; All retries exhausted
        (do
          (log/error {:event :drone/retry-exhausted
                      :drone-id drone-id
                      :task-id task-id
                      :attempts (inc attempt)
                      :last-error (ex-message last-exception)})
          (throw last-exception))

        ;; Try execution and capture result outside try block for recur
        (let [result (try
                       (when (pos? attempt)
                         (log/info {:event :drone/retry-attempt
                                    :drone-id drone-id
                                    :task-id task-id
                                    :attempt attempt
                                    :model (:model current-opts)}))
                       {:status :success :value (f current-opts)}
                       (catch Exception e
                         {:status :error :exception e}))]
          (if (= :success (:status result))
            ;; Success - return the value
            (:value result)
            ;; Error - handle retry logic outside try block
            (let [e (:exception result)
                  strategy (recovery-strategy e attempt merged-opts)
                  category (classify-error e)]

              ;; CLARITY-T: Record retry metrics
              (prom/record-retry! {:drone-id drone-id
                                   :attempt attempt
                                   :error-category category
                                   :action (:action strategy)})

              ;; Telemetry logging
              (log/warn {:event :drone/error-caught
                         :drone-id drone-id
                         :task-id task-id
                         :attempt attempt
                         :error-category category
                         :action (:action strategy)
                         :reason (:reason strategy)
                         :error-message (ex-message e)})

              ;; Invoke callback if provided
              (when on-retry
                (on-retry attempt e strategy))

              (case (:action strategy)
                :retry
                (do
                  (Thread/sleep (:delay-ms strategy))
                  (recur (inc attempt) current-opts e))

                :retry-with-fallback
                (do
                  (Thread/sleep (:delay-ms strategy))
                  (recur (inc attempt)
                         (assoc current-opts :model (:model strategy))
                         e))

                ;; :fail
                (throw e)))))))))

;;; ============================================================
;;; Convenience Wrappers
;;; ============================================================

(defn retry-drone-execution
  "Wrap drone execution with retry logic.

   Designed to integrate with drone/delegate!

   Arguments:
     execute-fn - The drone execution function
     opts       - Execution options (task, files, preset, etc.)

   Returns:
     Execution result with :retry-info metadata."
  [execute-fn opts]
  (let [start-time (System/currentTimeMillis)
        retry-count (atom 0)
        models-tried (atom [(:model opts)])]
    (try
      (let [result (with-retry
                     (fn [exec-opts]
                       (execute-fn exec-opts))
                     (assoc opts
                            :on-retry (fn [attempt _ex strategy]
                                        (swap! retry-count inc)
                                        (when-let [new-model (:model strategy)]
                                          (swap! models-tried conj new-model)))))]
        (assoc result
               :retry-info {:retries @retry-count
                            :models-tried @models-tried
                            :total-duration-ms (- (System/currentTimeMillis) start-time)}))
      (catch Exception e
        (throw (ex-info "Drone execution failed after retries"
                        {:retries @retry-count
                         :models-tried @models-tried
                         :total-duration-ms (- (System/currentTimeMillis) start-time)
                         :original-error (ex-message e)}
                        e))))))

;;; ============================================================
;;; Testing/Simulation Helpers
;;; ============================================================

(defn simulate-error
  "Create a simulated exception for testing retry logic.

   Type can be: :rate-limit, :timeout, :auth, :model, :server, :empty, :unknown"
  [error-type]
  (case error-type
    :rate-limit (ex-info "Rate limit exceeded" {:status 429})
    :timeout (java.net.SocketTimeoutException. "Request timed out")
    :auth (ex-info "Invalid API key" {:status 401})
    :model (ex-info "Model not found" {:status 404})
    :server (ex-info "Internal server error" {:status 500})
    :empty (ex-info "OpenRouter returned empty response" {})
    :unknown (ex-info "Something went wrong" {})))
