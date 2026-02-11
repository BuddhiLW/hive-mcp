(ns hive-mcp.tools.swarm.wave.retry
  "Unified retry logic for wave execution with error classification and strategy selection."
  (:require [hive-mcp.tools.swarm.wave.domain :as domain]
            [hive-mcp.telemetry.health :as health]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private nrepl-error-patterns
  "Patterns indicating nREPL transient failures."
  [#"(?i)connection.*refused"
   #"(?i)socket.*closed"
   #"(?i)nrepl.*not.*available"
   #"(?i)nrepl.*disconnect"
   #"(?i)timeout"
   #"(?i)network.*unreachable"
   #"(?i)connection.*reset"])

(def ^:private file-conflict-pattern
  "Pattern indicating a file conflict error."
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

(defn transient-nrepl-error?
  "Check if error message indicates a transient nREPL failure."
  [error-msg]
  (when error-msg
    (some #(re-find % error-msg) nrepl-error-patterns)))

(defn file-conflict-error?
  "Check if error message indicates a file conflict."
  [error-msg]
  (when error-msg
    (re-find file-conflict-pattern error-msg)))

(defn openrouter-error?
  "Check if error indicates an OpenRouter API transient error."
  [error-msg]
  (when error-msg
    (some #(re-find % error-msg) openrouter-error-patterns)))

(defn permanent-error?
  "Check if error indicates a permanent failure."
  [error-msg]
  (when error-msg
    (some #(re-find % error-msg) permanent-error-patterns)))

(defn classify-wave-error
  "Classify an error message into :nrepl, :conflict, :openrouter, :permanent, or :unknown."
  [error-msg]
  (cond
    (permanent-error? error-msg) :permanent
    (file-conflict-error? error-msg) :conflict
    (transient-nrepl-error? error-msg) :nrepl
    (openrouter-error? error-msg) :openrouter
    :else :unknown))

(defn select-retry-config
  "Select appropriate retry configuration based on error type."
  [error-type]
  (case error-type
    :nrepl domain/nrepl-retry-config
    :conflict domain/conflict-retry-config
    :openrouter domain/openrouter-retry-config
    :permanent {:max-retries 0}
    {:max-retries 1
     :initial-delay-ms 1000
     :backoff-multiplier 2
     :max-delay-ms 5000}))

(defn calculate-delay
  "Calculate delay for retry attempt with exponential backoff."
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
  "Determine recovery strategy based on error classification."
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

(defn emit-retry-health-event!
  "Emit health event for retry telemetry."
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

(defn with-wave-retry
  "Execute function f with unified retry logic based on error classification."
  [f {:keys [item-id file on-retry] :as opts}]
  (loop [attempt 0
         _last-error nil]
    (let [result (try
                   {:status :success :value (f)}
                   (catch Throwable e
                     {:status :error :exception e :message (ex-message e)}))]

      (if (= :success (:status result))
        (:value result)

        (let [error-msg (:message result)
              strategy (select-retry-strategy error-msg attempt opts)]

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
              (emit-retry-health-event!
               {:error-type (:error-type strategy)
                :item-id item-id
                :file file
                :attempt attempt
                :delay-ms (:delay-ms strategy)})

              (when on-retry
                (on-retry attempt (:exception result) strategy))

              (Thread/sleep (:delay-ms strategy))
              (recur (inc attempt) (:exception result)))

            (do
              (log/error {:event :wave/retry-exhausted
                          :item-id item-id
                          :file file
                          :attempts (inc attempt)
                          :error-type (:error-type strategy)
                          :error-message error-msg})
              (throw (:exception result)))))))))

(defn retry-task-execution
  "Wrap drone task execution with retry logic, returning TaskResult with retry info."
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
