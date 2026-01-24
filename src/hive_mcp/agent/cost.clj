(ns hive-mcp.agent.cost
  "Token cost tracking and budget management for drones.

   Provides:
   - Token counting (4 chars ≈ 1 token approximation)
   - Per-drone/per-wave usage tracking
   - Budget limits with enforcement
   - Usage dashboard data

   CLARITY-T: Telemetry first - observable cost metrics.
   CLARITY-Y: Yield safe failure - fail gracefully on budget exceeded."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Constants
;;; =============================================================================

(def ^:const chars-per-token
  "Approximation: 4 characters ≈ 1 token.
   This is a reasonable estimate for English text and code."
  4)

(def ^:const free-tier-rate
  "Cost per token for free-tier models (e.g., mistralai/devstral-2512:free).
   Free-tier models have $0 cost."
  0.0)

(def ^:const default-budgets
  "Default token budgets for cost control.

   :per-drone  - Max tokens per single drone task
   :per-wave   - Max tokens across all drones in a wave
   :per-hour   - Rate limit to prevent runaway usage"
  {:per-drone 8000
   :per-wave 50000
   :per-hour 200000})

;;; =============================================================================
;;; State
;;; =============================================================================

;; Token usage tracking state.
;; Shape:
;; {:drones {drone-id {:input-tokens N :output-tokens N :total-tokens N
;;                     :timestamp Instant :task-preview "..."}}
;;  :waves {wave-id {:total-tokens N :drone-count N :started-at Instant}}
;;  :hourly {:tokens-used N :window-start Instant}
;;  :cumulative {:total-input N :total-output N :total-requests N :saved-vs-premium N}}
(defonce ^:private *usage
  (atom {:drones {}
         :waves {}
         :hourly {:tokens-used 0
                  :window-start (java.time.Instant/now)}
         :cumulative {:total-input 0
                      :total-output 0
                      :total-requests 0
                      :saved-vs-premium 0}}))

;; Configurable budget limits.
(defonce ^:private *budgets
  (atom default-budgets))

;;; =============================================================================
;;; Token Counting
;;; =============================================================================

(defn count-tokens
  "Count tokens in a text string using 4-char approximation.

   Arguments:
     text - String to count tokens for

   Returns:
     Approximate token count (integer)"
  [text]
  (if (string? text)
    (int (Math/ceil (/ (count text) chars-per-token)))
    0))

(defn count-message-tokens
  "Count tokens in an LLM message (role + content).

   Adds ~4 tokens overhead for role/structure."
  [{:keys [role content]}]
  (+ 4 ;; Message structure overhead
     (count-tokens (name (or role "user")))
     (count-tokens (or content ""))))

(defn count-messages-tokens
  "Count total tokens in a message sequence."
  [messages]
  (reduce + 0 (map count-message-tokens messages)))

(defn count-tool-call-tokens
  "Count tokens in a tool call (name + arguments).

   Arguments:
     tool-call - Map with :name and :arguments"
  [{:keys [name arguments]}]
  (+ 10 ;; Tool call structure overhead
     (count-tokens (or name ""))
     (count-tokens (if (string? arguments)
                     arguments
                     (str arguments)))))

;;; =============================================================================
;;; Budget Management
;;; =============================================================================

(defn get-budgets
  "Get current budget configuration."
  []
  @*budgets)

(defn set-budgets!
  "Update budget configuration.

   Arguments:
     new-budgets - Map with :per-drone :per-wave :per-hour keys

   Returns:
     Updated budget configuration"
  [new-budgets]
  (swap! *budgets merge new-budgets))

(defn reset-budgets!
  "Reset budgets to defaults."
  []
  (reset! *budgets default-budgets))

(defn- refresh-hourly-window!
  "Refresh hourly window if expired (1 hour elapsed)."
  []
  (let [now (java.time.Instant/now)
        window-start (get-in @*usage [:hourly :window-start])
        elapsed-hours (.toHours (java.time.Duration/between window-start now))]
    (when (>= elapsed-hours 1)
      (swap! *usage assoc :hourly {:tokens-used 0
                                   :window-start now}))))

(defn check-budget
  "Check if a request would exceed budget limits.

   Arguments:
     estimated-tokens - Estimated tokens for the request
     context          - Map with :drone-id :wave-id for context-specific checks

   Returns:
     {:allowed? true/false
      :reason   \"Budget exceeded reason\" (if not allowed)
      :limits   {:per-drone N :per-wave N :per-hour N}
      :current  {:drone-tokens N :wave-tokens N :hour-tokens N}}"
  [estimated-tokens {:keys [drone-id wave-id]}]
  (refresh-hourly-window!)
  (let [budgets @*budgets
        usage @*usage
        drone-tokens (get-in usage [:drones drone-id :total-tokens] 0)
        wave-tokens (get-in usage [:waves wave-id :total-tokens] 0)
        hour-tokens (get-in usage [:hourly :tokens-used] 0)

        ;; Check limits
        drone-exceeded? (> (+ drone-tokens estimated-tokens)
                           (:per-drone budgets))
        wave-exceeded? (and wave-id
                            (> (+ wave-tokens estimated-tokens)
                               (:per-wave budgets)))
        hour-exceeded? (> (+ hour-tokens estimated-tokens)
                          (:per-hour budgets))]

    {:allowed? (not (or drone-exceeded? wave-exceeded? hour-exceeded?))
     :reason (cond
               drone-exceeded? (format "Drone budget exceeded: %d + %d > %d"
                                       drone-tokens estimated-tokens (:per-drone budgets))
               wave-exceeded? (format "Wave budget exceeded: %d + %d > %d"
                                      wave-tokens estimated-tokens (:per-wave budgets))
               hour-exceeded? (format "Hourly rate limit exceeded: %d + %d > %d"
                                      hour-tokens estimated-tokens (:per-hour budgets))
               :else nil)
     :limits budgets
     :current {:drone-tokens drone-tokens
               :wave-tokens wave-tokens
               :hour-tokens hour-tokens}}))

;;; =============================================================================
;;; Cost Estimation
;;; =============================================================================

;; Premium model rates (per 1M tokens) for savings comparison
(def ^:private premium-rates
  "Reference rates for premium models to calculate savings.
   Based on typical API pricing."
  {:claude-sonnet {:input 3.0 :output 15.0}    ;; $3/1M input, $15/1M output
   :gpt-4         {:input 10.0 :output 30.0}   ;; $10/1M input, $30/1M output
   :default       {:input 2.0 :output 8.0}})   ;; Conservative estimate

(defn estimate-premium-cost
  "Estimate what the same request would cost with premium models.

   Arguments:
     input-tokens  - Number of input tokens
     output-tokens - Number of output tokens
     model-key     - Which premium model to compare (default: :default)

   Returns:
     Estimated cost in USD"
  [input-tokens output-tokens & [model-key]]
  (let [rates (get premium-rates (or model-key :default))
        input-cost (* input-tokens (/ (:input rates) 1000000))
        output-cost (* output-tokens (/ (:output rates) 1000000))]
    (+ input-cost output-cost)))

;;; =============================================================================
;;; Usage Tracking
;;; =============================================================================

(defn track-drone-usage!
  "Track token usage for a drone task.

   Arguments:
     drone-id - Drone identifier
     usage    - Map with :input-tokens :output-tokens :task-preview :wave-id

   Returns:
     Updated drone usage record"
  [drone-id {:keys [input-tokens output-tokens task-preview wave-id]}]
  (let [total-tokens (+ (or input-tokens 0) (or output-tokens 0))
        premium-cost (estimate-premium-cost input-tokens output-tokens)
        now (java.time.Instant/now)]

    (swap! *usage
           (fn [u]
             (cond-> u
               ;; Update drone-specific tracking
               true (assoc-in [:drones drone-id]
                              {:input-tokens input-tokens
                               :output-tokens output-tokens
                               :total-tokens total-tokens
                               :timestamp now
                               :task-preview (subs (or task-preview "") 0
                                                   (min 80 (count (or task-preview ""))))})

               ;; Update wave tracking (if wave-id provided)
               wave-id (update-in [:waves wave-id :total-tokens] (fnil + 0) total-tokens)
               wave-id (update-in [:waves wave-id :drone-count] (fnil inc 0))

               ;; Update hourly tracking
               true (update-in [:hourly :tokens-used] + total-tokens)

               ;; Update cumulative tracking
               true (update-in [:cumulative :total-input] + input-tokens)
               true (update-in [:cumulative :total-output] + output-tokens)
               true (update-in [:cumulative :total-requests] inc)
               true (update-in [:cumulative :saved-vs-premium] + premium-cost))))

    ;; Log usage
    (log/info "Tracked drone usage"
              {:drone-id drone-id
               :input-tokens input-tokens
               :output-tokens output-tokens
               :total-tokens total-tokens
               :premium-savings (format "$%.4f" premium-cost)})

    ;; Return updated record
    (get-in @*usage [:drones drone-id])))

(defn start-wave-tracking!
  "Start tracking a new wave.

   Arguments:
     wave-id - Wave identifier

   Returns:
     Wave tracking record"
  [wave-id]
  (let [now (java.time.Instant/now)]
    (swap! *usage assoc-in [:waves wave-id]
           {:total-tokens 0
            :drone-count 0
            :started-at now})
    (get-in @*usage [:waves wave-id])))

(defn complete-wave-tracking!
  "Mark wave tracking as complete and record summary.

   Arguments:
     wave-id - Wave identifier

   Returns:
     Final wave usage record"
  [wave-id]
  (let [wave-usage (get-in @*usage [:waves wave-id])]
    (when wave-usage
      (swap! *usage update-in [:waves wave-id]
             assoc :completed-at (java.time.Instant/now))
      (log/info "Wave completed"
                {:wave-id wave-id
                 :total-tokens (:total-tokens wave-usage)
                 :drone-count (:drone-count wave-usage)}))
    wave-usage))

;;; =============================================================================
;;; Context Truncation (Budget Enforcement)
;;; =============================================================================

(defn truncate-context
  "Truncate context to fit within token budget.

   Strategy:
   1. Keep system message (first message)
   2. Keep most recent messages
   3. Truncate middle messages

   Arguments:
     messages      - Message sequence
     token-budget  - Max tokens allowed

   Returns:
     {:messages truncated-messages
      :truncated? true/false
      :original-tokens N
      :final-tokens N}"
  [messages token-budget]
  (let [original-tokens (count-messages-tokens messages)]
    (if (<= original-tokens token-budget)
      {:messages messages
       :truncated? false
       :original-tokens original-tokens
       :final-tokens original-tokens}

      ;; Need to truncate
      (let [system-msg (first messages)
            other-msgs (rest messages)
            ;; Reserve tokens for system message
            system-tokens (count-message-tokens system-msg)
            remaining-budget (- token-budget system-tokens)

            ;; Take messages from the end (most recent)
            reversed (reverse other-msgs)
            selected (loop [msgs []
                            remaining remaining-budget
                            [msg & rest] reversed]
                       (if (nil? msg)
                         msgs
                         (let [msg-tokens (count-message-tokens msg)]
                           (if (> msg-tokens remaining)
                             msgs
                             (recur (cons msg msgs)
                                    (- remaining msg-tokens)
                                    rest)))))

            final-messages (cons system-msg selected)
            final-tokens (count-messages-tokens final-messages)]

        (log/warn "Context truncated"
                  {:original-tokens original-tokens
                   :final-tokens final-tokens
                   :messages-dropped (- (count messages) (count final-messages))})

        {:messages final-messages
         :truncated? true
         :original-tokens original-tokens
         :final-tokens final-tokens}))))

;;; =============================================================================
;;; Dashboard Data
;;; =============================================================================

(defn get-usage-summary
  "Get comprehensive usage summary for dashboard.

   Returns:
     {:cumulative {:total-tokens N
                   :total-requests N
                   :saved-vs-premium N (USD)}
      :hourly {:tokens-used N
               :remaining N
               :window-start Instant}
      :recent-drones [{:drone-id :tokens :task-preview :timestamp}]
      :active-waves [{:wave-id :tokens :drone-count :started-at}]
      :budgets {:per-drone N :per-wave N :per-hour N}}"
  []
  (refresh-hourly-window!)
  (let [usage @*usage
        budgets @*budgets
        cumulative (:cumulative usage)
        hourly (:hourly usage)

        ;; Get recent drones (last 10, sorted by timestamp)
        recent-drones (->> (:drones usage)
                           (map (fn [[id data]]
                                  (assoc data :drone-id id)))
                           (sort-by :timestamp #(compare %2 %1))
                           (take 10)
                           (mapv #(select-keys % [:drone-id :total-tokens :task-preview :timestamp])))

        ;; Get active/recent waves (last 5)
        recent-waves (->> (:waves usage)
                          (map (fn [[id data]]
                                 (assoc data :wave-id id)))
                          (sort-by :started-at #(compare %2 %1))
                          (take 5)
                          (mapv #(select-keys % [:wave-id :total-tokens :drone-count :started-at :completed-at])))]

    {:cumulative {:total-tokens (+ (:total-input cumulative) (:total-output cumulative))
                  :total-input (:total-input cumulative)
                  :total-output (:total-output cumulative)
                  :total-requests (:total-requests cumulative)
                  :saved-vs-premium (:saved-vs-premium cumulative)}
     :hourly {:tokens-used (:tokens-used hourly)
              :remaining (- (:per-hour budgets) (:tokens-used hourly))
              :window-start (:window-start hourly)}
     :recent-drones recent-drones
     :recent-waves recent-waves
     :budgets budgets}))

(defn identify-high-token-tasks
  "Identify tasks that consumed the most tokens.

   Arguments:
     limit - Max number of tasks to return (default: 10)

   Returns:
     List of {:drone-id :total-tokens :task-preview :timestamp}"
  [& [limit]]
  (let [n (or limit 10)]
    (->> (:drones @*usage)
         (map (fn [[id data]]
                (assoc data :drone-id id)))
         (sort-by :total-tokens >)
         (take n)
         (mapv #(select-keys % [:drone-id :total-tokens :task-preview :timestamp])))))

;;; =============================================================================
;;; Reset Functions
;;; =============================================================================

(defn reset-usage!
  "Reset all usage tracking. For testing or manual reset."
  []
  (reset! *usage {:drones {}
                  :waves {}
                  :hourly {:tokens-used 0
                           :window-start (java.time.Instant/now)}
                  :cumulative {:total-input 0
                               :total-output 0
                               :total-requests 0
                               :saved-vs-premium 0}})
  (log/info "Cost tracking reset"))

(defn reset-hourly!
  "Reset only hourly tracking. For manual rate limit reset."
  []
  (swap! *usage assoc :hourly {:tokens-used 0
                               :window-start (java.time.Instant/now)})
  (log/info "Hourly cost tracking reset"))
