(ns hive-mcp.resilience
  "Resilient evaluation with graceful degradation.

   Following CLARITY principle 'Yield safe failure' - provides fallback strategies
   and safe wrappers for REPL evaluation that never throw exceptions unexpectedly.

   This namespace implements the Fallback pattern (GoF) combined with retry logic
   for handling transient failures in REPL evaluation.

   Key patterns:
   - Fallback Strategy: Try explicit mode, fall back to silent on failure
   - Retry Pattern: Handle transient network/connection failures
   - Safe Wrapper: Never throw, always return structured error data"
  (:require [hive-mcp.evaluator :as evaluator]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; ============================================================================
;; Core Resilience Functions
;; ============================================================================

(defn eval-with-fallback
  "Evaluate code with fallback strategy if primary evaluator fails.

   This implements graceful degradation: if the primary evaluator fails,
   automatically fall back to an alternative evaluator.

   Parameters:
   - primary-evaluator: Primary ReplEvaluator instance
   - fallback-evaluator: Fallback ReplEvaluator instance (or nil to skip fallback)
   - code: String containing code to evaluate

   Returns:
   Map with :success, :result, :evaluator-used, and optionally :error, :fallback-used

   Example:
     (eval-with-fallback cider-eval nrepl-eval \"(+ 1 2)\")

   CLARITY: Yield safe failure - gracefully degrades to fallback evaluator"
  [primary-evaluator fallback-evaluator code]
  (let [primary-result (evaluator/eval-code primary-evaluator code)]

    (if (:success primary-result)
      ;; Primary evaluator succeeded
      (do
        (log/debug :eval-success {:evaluator (type primary-evaluator)})
        (assoc primary-result :evaluator-used :primary))

      ;; Primary evaluator failed - try fallback if available
      (if fallback-evaluator
        (do
          (log/warn :eval-fallback
                    {:primary-type (type primary-evaluator)
                     :fallback-type (type fallback-evaluator)
                     :error (:error primary-result)
                     :code-preview (subs code 0 (min 50 (count code)))})

          (let [fallback-result (evaluator/eval-code fallback-evaluator code)]

            (if (:success fallback-result)
              ;; Fallback succeeded
              (do
                (log/info :eval-fallback-success {:fallback-type (type fallback-evaluator)})
                (assoc fallback-result
                       :evaluator-used :fallback
                       :fallback-used true
                       :primary-error (:error primary-result)))

              ;; Both failed
              (do
                (log/error :eval-all-evaluators-failed
                           {:primary-error (:error primary-result)
                            :fallback-error (:error fallback-result)})
                (assoc fallback-result
                       :evaluator-used :fallback
                       :fallback-used true
                       :primary-error (:error primary-result))))))

        ;; No fallback available
        (do
          (log/error :eval-failed-no-fallback {:error (:error primary-result)})
          (assoc primary-result :evaluator-used :primary))))))

(defn eval-with-retry
  "Evaluate code with automatic retry on transient failures.

   This handles temporary failures like network hiccups, connection timeouts,
   or race conditions. Implements exponential backoff between retries.

   Parameters:
   - evaluator: ReplEvaluator instance
   - code: String containing code to evaluate
   - opts: Map with keys:
     :max-retries - Maximum number of retry attempts (default: 3)
     :delay-ms - Initial delay between retries in milliseconds (default: 100)
     :backoff-multiplier - Multiplier for exponential backoff (default: 2)

   Returns:
   Map with :success, :result, :attempts, and optionally :error

   Example:
     (eval-with-retry evaluator \"(+ 1 2)\"
       {:max-retries 5 :delay-ms 200})

   CLARITY: Yield safe failure - handles transient failures with retry"
  [evaluator code {:keys [max-retries delay-ms backoff-multiplier]
                   :or {max-retries 3 delay-ms 100 backoff-multiplier 2}}]
  (loop [attempt 1
         current-delay delay-ms]
    (let [result (evaluator/eval-code evaluator code)]

      (if (:success result)
        ;; Success
        (do
          (when (> attempt 1)
            (log/info :eval-retry-success {:attempts attempt}))
          (assoc result :attempts attempt))

        ;; Failed - should we retry?
        (if (< attempt max-retries)
          (do
            (log/warn :eval-retry
                      {:attempt attempt
                       :max-retries max-retries
                       :delay-ms current-delay
                       :error (:error result)})
            (Thread/sleep current-delay)
            (recur (inc attempt)
                   (* current-delay backoff-multiplier)))

          ;; Exhausted retries
          (do
            (log/error :eval-retry-exhausted
                       {:attempts attempt
                        :error (:error result)})
            (assoc result :attempts attempt)))))))

(defn safe-eval
  "Never-throw wrapper for code evaluation.

   This function guarantees it will never throw an exception. Instead, it always
   returns a map with either {:ok result} or {:error message :type exception-type}.

   This is the safest evaluation method for use in APIs, servers, or anywhere
   exceptions would be problematic.

   Parameters:
   - evaluator: ReplEvaluator instance
   - code: String containing code to evaluate

   Returns:
   Map with either:
   - {:ok result} on success
   - {:error message :type exception-type :code code-preview} on failure

   Example:
     (safe-eval evaluator \"(+ 1 2)\")
     ;; => {:ok \"3\"}

     (safe-eval evaluator \"(/ 1 0)\")
     ;; => {:error \"Division by zero\" :type \"ArithmeticException\" ...}

   CLARITY: Yield safe failure - never throws, always returns structured data"
  [evaluator code]
  (try
    (let [result (evaluator/eval-code evaluator code)]
      (if (:success result)
        {:ok (:result result)}
        {:error (:error result)
         :type "EvaluationFailure"
         :code (subs code 0 (min 50 (count code)))}))

    (catch Throwable e
      ;; This should rarely happen as eval-code handles its own errors,
      ;; but we catch Throwable to be absolutely safe
      (log/error e :safe-eval-caught-exception
                 {:code (subs code 0 (min 50 (count code)))})
      {:error (.getMessage e)
       :type (.getName (class e))
       :code (subs code 0 (min 50 (count code)))})))

;; ============================================================================
;; Combined Strategies
;; ============================================================================

(defn resilient-eval
  "Maximum resilience: combines retry, fallback, and safe-eval patterns.

   This is the most robust evaluation method, combining:
   1. Retry logic for transient failures
   2. Fallback to alternative evaluator on failure
   3. Safe wrapper that never throws

   Use this when evaluation must succeed at all costs and you need maximum
   reliability. For performance-critical paths, use simpler strategies.

   Parameters:
   - primary-evaluator: Primary ReplEvaluator instance
   - fallback-evaluator: Fallback ReplEvaluator instance (or nil)
   - code: String containing code to evaluate
   - opts: Map with keys:
     :max-retries - Maximum retry attempts, default 3
     :retry-delay-ms - Initial retry delay, default 100

   Returns:
   Map with either:
   - {:ok result :strategy-used {...}} on success
   - {:error message :strategy-used {...}} on failure

   The :strategy-used map contains diagnostic information about which
   resilience strategies were employed (retries, fallback, etc.)

   Example:
     (resilient-eval cider-eval nrepl-eval \"(+ 1 2)\"
       {:max-retries 5})

   CLARITY: Yield safe failure - maximum resilience with all patterns"
  [primary-evaluator fallback-evaluator code {:keys [max-retries retry-delay-ms]
                                              :or {max-retries 3
                                                   retry-delay-ms 100}}]
  (try
    ;; First try primary evaluator with retry
    (let [retry-opts {:max-retries max-retries
                      :delay-ms retry-delay-ms
                      :backoff-multiplier 2}
          primary-result (eval-with-retry primary-evaluator code retry-opts)]

      (if (:success primary-result)
        ;; Primary succeeded
        {:ok (:result primary-result)
         :strategy-used {:evaluator :primary
                         :attempts (:attempts primary-result)}}

        ;; Primary failed - try fallback if available
        (if fallback-evaluator
          (let [fallback-result (eval-with-retry fallback-evaluator code retry-opts)]
            (if (:success fallback-result)
              {:ok (:result fallback-result)
               :strategy-used {:evaluator :fallback
                               :attempts (:attempts fallback-result)
                               :primary-error (:error primary-result)}}
              {:error (:error fallback-result)
               :strategy-used {:evaluator :fallback
                               :primary-error (:error primary-result)
                               :fallback-error (:error fallback-result)
                               :attempts (:attempts fallback-result)}}))

          ;; No fallback
          {:error (:error primary-result)
           :strategy-used {:evaluator :primary
                           :attempts (:attempts primary-result)}})))

    (catch Throwable e
      (log/error e :resilient-eval-caught-exception
                 {:code (subs code 0 (min 50 (count code)))})
      {:error (.getMessage e)
       :strategy-used {:exception-type (.getName (class e))}})))

;; ============================================================================
;; Convenience Predicates
;; ============================================================================

(defn success?
  "Check if evaluation result indicates success.
   Works with both eval-code results {:success true} and safe-eval results {:ok ...}"
  [result]
  (or (:success result)
      (contains? result :ok)))

(defn failed?
  "Check if evaluation result indicates failure.
   Works with both eval-code results {:success false} and safe-eval results {:error ...}"
  [result]
  (not (success? result)))

(defn get-value
  "Extract the actual value from any evaluation result format.
   Returns nil if evaluation failed."
  [result]
  (cond
    (contains? result :ok) (:ok result)
    (:success result) (:result result)
    :else nil))

(defn get-error
  "Extract error message from any evaluation result format.
   Returns nil if evaluation succeeded."
  [result]
  (when (failed? result)
    (:error result)))

;; ============================================================================
;; Circuit Breaker Pattern (Future Enhancement)
;; ============================================================================

(defprotocol CircuitBreaker
  "Protocol for circuit breaker state management and execution."
  (call [this f] "Execute function f if circuit breaker allows it.")
  (state [this] "Get the current state of the circuit breaker."))

(defrecord CircuitBreakerState
           [state failure-count last-failure-time options]
  ;; Record to hold the state and configuration of the circuit breaker.
  )

(defn make-circuit-breaker
  "Create a new circuit breaker with the given options.

   Options:
   - :failure-threshold: Number of failures before opening the circuit (default: 5)
   - :reset-timeout-ms: Time in milliseconds before attempting to close the circuit (default: 30000)
   - :half-open-max-calls: Maximum number of calls allowed in half-open state (default: 3)"
  [opts]
  (let [options (merge {:failure-threshold 5
                        :reset-timeout-ms 30000
                        :half-open-max-calls 3}
                       opts)
        state (atom :closed)
        failure-count (atom 0)
        last-failure-time (atom 0)]
    (->CircuitBreakerState state failure-count last-failure-time options)))

(extend-type CircuitBreakerState
  CircuitBreaker
  (call [this f]
    (let [{:keys [failure-threshold reset-timeout-ms half-open-max-calls]} (:options this)
          current-state @(:state this)
          _current-failure-count @(:failure-count this)
          current-last-failure-time @(:last-failure-time this)
          now (System/currentTimeMillis)]
      (case current-state
        :closed
        (try
          (let [result (f)]
            (reset! (:failure-count this) 0)
            result)
          (catch Exception e
            (swap! (:failure-count this) inc)
            (when (>= @(:failure-count this) failure-threshold)
              (reset! (:state this) :open)
              (reset! (:last-failure-time this) now))
            (throw (ex-info "Circuit breaker: call failed in closed state" {:error (.getMessage e)}))))

        :open
        (let [time-since-failure (- now current-last-failure-time)]
          (if (>= time-since-failure reset-timeout-ms)
            (do
              (reset! (:state this) :half-open)
              (reset! (:failure-count this) 0)
              (call this f))
            (throw (ex-info "Circuit breaker is open" {:state :open}))))

        :half-open
        (if (< @(:failure-count this) half-open-max-calls)
          (try
            (let [result (f)]
              (reset! (:state this) :closed)
              (reset! (:failure-count this) 0)
              result)
            (catch Exception e
              (swap! (:failure-count this) inc)
              (reset! (:state this) :open)
              (reset! (:last-failure-time this) now)
              (throw (ex-info "Circuit breaker: call failed in half-open state" {:error (.getMessage e)}))))
          (throw (ex-info "Circuit breaker: half-open call limit exceeded" {:state :half-open}))))))

  (state [this]
    @(:state this)))

(defn eval-with-circuit-breaker
  "Evaluate code using the circuit breaker pattern.

   Parameters:
   - circuit-breaker: CircuitBreaker instance
   - evaluator: ReplEvaluator instance
   - code: String containing code to evaluate
   - opts: Additional options (currently unused)

   Returns:
   Map with :success, :result, and optionally :error"
  [circuit-breaker evaluator code _opts]
  (try
    (let [result (call circuit-breaker #(evaluator/eval-code evaluator code))]
      {:success true
       :result result})
    (catch Exception e
      {:success false
       :error (.getMessage e)})))
