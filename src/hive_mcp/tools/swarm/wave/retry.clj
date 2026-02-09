(ns hive-mcp.tools.swarm.wave.retry
  "Unified retry logic for wave execution.

   CLARITY-Y (Yield Safe Failure): Graceful degradation through smart retries.
   CLARITY-T (Telemetry First): All retries are logged for observability.

   Error Categories:
     :nrepl     - nREPL connection issues (short delays, 2 retries)
     :conflict  - File conflicts (long delays, 5 retries - waiting for lock)
     :openrouter - API errors (model fallback, exponential backoff)
     :permanent - Auth errors, invalid requests (fail fast)
     :unknown   - Unclassified (cautious retry with limit)

   Based on:
     - Wave File Conflict Retry decision (memory: 20260204224320-176de548)
     - drone/retry.clj patterns"
  (:require [hive-mcp.tools.swarm.wave.domain :as domain]
            [hive-mcp.telemetry.health :as health]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================
;;; Error Classification Patterns
;;; ============================================================

(def ^:private nrepl-error-patterns
  "Patterns that indicate an nREPL transient failure (worth retrying).

   CLARITY-Y: Yield safe failure - distinguish transient from permanent errors."
  [#"(?i)connection.*refused"
   #"(?i)socket.*closed"
   #"(?i)nrepl.*not.*available"
   #"(?i)nrepl.*disconnect"
   #"(?i)timeout"
   #"(?i)network.*unreachable"
   #"(?i)connection.*reset"])

(def ^:private file-conflict-pattern
  "Pattern that indicates a file conflict error (worth retrying after wait).

   RACE CONDITION FIX: When drone A holds a claim and drone B gets conflict,
   drone B should wait and retry instead of failing immediately. Drone A may
   complete and release the claim, allowing drone B to proceed.

   CLARITY-Y: Transient conflicts become success with patience."
  #"(?i)file.*conflict.*detected|files.*locked.*by.*another.*drone")

(def ^:private openrouter-error-patterns
  "Patterns indicating OpenRouter API transient errors."
  [#"(?i)rate.*limit"
   #"(?i)too.*many.*requests"
   #"(?i)quota.*exceeded"
   #"(?i)model.*not.*available"
   #"(?i)service.*unavailable"
   #"(?i)internal.*server.*error"
   #"(?i)502|503|504"])

(def ^:private permanent-error-patterns
  "Patterns indicating permanent errors (don't retry)."
  [#"(?i)unauthorized"
   #"(?i)invalid.*api.*key"
   #"(?i)authentication.*failed"
   #"(?i)forbidden"
   #"(?i)invalid.*request"
   #"(?i)malformed"])

;;; ============================================================
;;; Error Classification Functions
;;; ============================================================

(defn transient-nrepl-error?
  "Check if error message indicates a transient nREPL failure.

   Returns true if the error matches known transient patterns."
  [error-msg]
  (when error-msg
    (some #(re-find % error-msg) nrepl-error-patterns)))

(defn file-conflict-error?
  "Check if error message indicates a file conflict (another drone holds the claim).

   File conflicts are transient - they clear when the holding drone completes.
   Worth retrying with backoff.

   RACE CONDITION FIX: Prevents misleading 'conflict' failures when the actual
   work completes successfully by another drone."
  [error-msg]
  (when error-msg
    (re-find file-conflict-pattern error-msg)))

(defn openrouter-error?
  "Check if error indicates an OpenRouter API transient error."
  [error-msg]
  (when error-msg
    (some #(re-find % error-msg) openrouter-error-patterns)))

(defn permanent-error?
  "Check if error indicates a permanent failure (don't retry)."
  [error-msg]
  (when error-msg
    (some #(re-find % error-msg) permanent-error-patterns)))

(defn classify-wave-error
  "Classify an error message into categories.

   Returns:
     :nrepl     - nREPL connection issues
     :conflict  - File lock conflicts
     :openrouter - OpenRouter API issues
     :permanent - Authentication/validation errors
     :unknown   - Unclassified errors"
  [error-msg]
  (cond
    (permanent-error? error-msg) :permanent
    (file-conflict-error? error-msg) :conflict
    (transient-nrepl-error? error-msg) :nrepl
    (openrouter-error? error-msg) :openrouter
    :else :unknown))

;;; ============================================================
;;; Retry Strategy Selection
;;; ============================================================

(defn select-retry-config
  "Select appropriate retry configuration based on error type.

   Arguments:
     error-type - Keyword from classify-wave-error

   Returns:
     Retry config map with :max-retries, :initial-delay-ms, etc."
  [error-type]
  (case error-type
    :nrepl domain/nrepl-retry-config
    :conflict domain/conflict-retry-config
    :openrouter domain/openrouter-retry-config
    :permanent {:max-retries 0}
    ;; :unknown - cautious retry
    {:max-retries 1
     :initial-delay-ms 1000
     :backoff-multiplier 2
     :max-delay-ms 5000}))

(defn calculate-delay
  "Calculate delay for retry attempt with exponential backoff.

   Arguments:
     attempt - Current retry attempt (0-indexed)
     config  - Retry config map

   Returns:
     Delay in milliseconds."
  [attempt {:keys [initial-delay-ms max-delay-ms backoff-multiplier jitter-factor]
            :or {initial-delay-ms 1000
                 max-delay-ms 30000
                 backoff-multiplier 2.0
                 jitter-factor 0}}]
  (let [base-delay (* initial-delay-ms (Math/pow backoff-multiplier attempt))
        capped-delay (min base-delay max-delay-ms)
        jitter (if (pos? jitter-factor)
                 (* capped-delay jitter-factor (rand))
                 0)]
    (long (+ capped-delay jitter))))

(defn recoverable?
  "Check if error type is recoverable (worth retrying)."
  [error-type]
  (not= error-type :permanent))

(defn select-retry-strategy
  "Determine recovery strategy based on error classification.

   Arguments:
     error-msg - Error message string
     attempt   - Current attempt number
     opts      - Options map with :item-id, :file, etc.

   Returns:
     Map with:
       :error-type  - Classified error type
       :config      - Retry config
       :recoverable? - Whether retry is worthwhile
       :delay-ms    - Calculated delay (if recoverable)
       :action      - :retry | :fail
       :reason      - Human-readable explanation"
  [error-msg attempt _opts]
  (let [error-type (classify-wave-error error-msg)
        config (select-retry-config error-type)
        max-retries (:max-retries config 0)
        within-limit? (< attempt max-retries)
        recoverable (and (recoverable? error-type) within-limit?)]

    {:error-type error-type
     :config config
     :recoverable? recoverable
     :delay-ms (when recoverable
                 (calculate-delay attempt config))
     :action (if recoverable :retry :fail)
     :reason (cond
               (not (recoverable? error-type))
               (str "Permanent error: " error-msg)

               (not within-limit?)
               (format "Max retries (%d) exhausted for %s error"
                       max-retries (name error-type))

               :else
               (format "%s error - retry %d/%d"
                       (name error-type) (inc attempt) max-retries))}))

;;; ============================================================
;;; Health Event Emission
;;; ============================================================

(defn emit-retry-health-event!
  "Emit health event for retry telemetry.

   CLARITY-T: All retries are observable."
  [{:keys [error-type item-id file attempt delay-ms]}]
  (try
    (health/emit-health-event!
     {:type (case error-type
              :nrepl :nrepl-disconnect
              :conflict :file-conflict
              :openrouter :openrouter-error
              :wave-retry)
      :severity :warn
      :message (format "%s error, retrying (attempt %d, delay %dms)"
                       (name error-type) (inc attempt) delay-ms)
      :context {:item-id item-id
                :file file
                :attempt (inc attempt)
                :error-type error-type}
      :recoverable? true})
    (catch Exception _)))

;;; ============================================================
;;; Core Retry Wrapper
;;; ============================================================

(defn with-wave-retry
  "Execute function f with unified retry logic.

   Selects retry strategy based on error classification:
   - :nrepl     → short delays, 2 retries
   - :conflict  → long delays, 5 retries (wait for lock release)
   - :openrouter → model fallback, exponential backoff
   - :permanent → fail fast
   - :unknown   → cautious 1 retry

   Arguments:
     f    - Function to execute (no args)
     opts - Options map:
            :item-id     - Item identifier for logging
            :file        - File being processed
            :on-retry    - Callback fn [attempt error strategy]

   Returns:
     Result from f on success.

   Throws:
     Original exception after all retries exhausted."
  [f {:keys [item-id file on-retry] :as opts}]
  (loop [attempt 0
         _last-error nil]
    (let [result (try
                   {:status :success :value (f)}
                   (catch Throwable e
                     {:status :error :exception e :message (ex-message e)}))]

      (if (= :success (:status result))
        (:value result)

        ;; Error occurred - check retry strategy
        (let [error-msg (:message result)
              strategy (select-retry-strategy error-msg attempt opts)]

          ;; Log retry attempt
          (log/warn {:event :wave/retry-check
                     :item-id item-id
                     :file file
                     :attempt attempt
                     :error-type (:error-type strategy)
                     :action (:action strategy)
                     :reason (:reason strategy)
                     :error-message error-msg})

          (if (= :retry (:action strategy))
            (do
              ;; Emit health event for telemetry
              (emit-retry-health-event!
               {:error-type (:error-type strategy)
                :item-id item-id
                :file file
                :attempt attempt
                :delay-ms (:delay-ms strategy)})

              ;; Call on-retry callback if provided
              (when on-retry
                (on-retry attempt (:exception result) strategy))

              ;; Wait and retry
              (Thread/sleep (:delay-ms strategy))
              (recur (inc attempt) (:exception result)))

            ;; Fail
            (do
              (log/error {:event :wave/retry-exhausted
                          :item-id item-id
                          :file file
                          :attempts (inc attempt)
                          :error-type (:error-type strategy)
                          :error-message error-msg})
              (throw (:exception result)))))))))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defn retry-task-execution
  "Wrap drone task execution with retry logic.

   Arguments:
     execute-fn - Task execution function (no args)
     item       - Change item map
     opts       - Additional options

   Returns:
     TaskResult with retry info on success/failure."
  [execute-fn item opts]
  (let [{:keys [change-item/id change-item/file]} item
        retry-count (atom 0)
        errors-seen (atom [])]
    (try
      (let [result (with-wave-retry
                     execute-fn
                     (merge opts
                            {:item-id id
                             :file file
                             :on-retry (fn [attempt ex strategy]
                                         (swap! retry-count inc)
                                         (swap! errors-seen conj
                                                {:attempt attempt
                                                 :error (ex-message ex)
                                                 :type (:error-type strategy)}))}))]
        (cond-> result
          (pos? @retry-count)
          (assoc :retry-info {:retries @retry-count
                              :errors @errors-seen})))

      (catch Throwable e
        (domain/failure-result
         id
         (ex-message e)
         {:retries @retry-count
          :errors @errors-seen
          :final-error (ex-message e)})))))
