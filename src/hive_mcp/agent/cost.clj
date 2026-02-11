(ns hive-mcp.agent.cost
  "Token cost tracking and budget management for drones."
  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const chars-per-token
  "Approximation: 4 characters per token."
  4)

(def ^:const free-tier-rate
  "Cost per token for free-tier models."
  0.0)

(def ^:const default-budgets
  "Default token budgets for cost control."
  {:per-drone 8000
   :per-wave 50000
   :per-hour 200000})

(defonce ^:private *usage
  (atom {:drones {}
         :waves {}
         :hourly {:tokens-used 0
                  :window-start (java.time.Instant/now)}
         :cumulative {:total-input 0
                      :total-output 0
                      :total-requests 0
                      :saved-vs-premium 0}}))

(defonce ^:private *budgets
  (atom default-budgets))

(defn count-tokens
  "Count tokens in a text string using 4-char approximation."
  [text]
  (if (string? text)
    (int (Math/ceil (/ (count text) chars-per-token)))
    0))

(defn count-message-tokens
  "Count tokens in an LLM message including overhead."
  [{:keys [role content]}]
  (+ 4
     (count-tokens (name (or role "user")))
     (count-tokens (or content ""))))

(defn count-messages-tokens
  "Count total tokens in a message sequence."
  [messages]
  (reduce + 0 (map count-message-tokens messages)))

(defn count-tool-call-tokens
  "Count tokens in a tool call."
  [{:keys [name arguments]}]
  (+ 10
     (count-tokens (or name ""))
     (count-tokens (if (string? arguments)
                     arguments
                     (str arguments)))))

(defn get-budgets
  "Get current budget configuration."
  []
  @*budgets)

(defn set-budgets!
  "Update budget configuration."
  [new-budgets]
  (swap! *budgets merge new-budgets))

(defn reset-budgets!
  "Reset budgets to defaults."
  []
  (reset! *budgets default-budgets))

(defn- refresh-hourly-window!
  "Refresh hourly window if expired."
  []
  (let [now (java.time.Instant/now)
        window-start (get-in @*usage [:hourly :window-start])
        elapsed-hours (.toHours (java.time.Duration/between window-start now))]
    (when (>= elapsed-hours 1)
      (swap! *usage assoc :hourly {:tokens-used 0
                                   :window-start now}))))

(defn check-budget
  "Check if a request would exceed budget limits."
  [estimated-tokens {:keys [drone-id wave-id]}]
  (refresh-hourly-window!)
  (let [budgets @*budgets
        usage @*usage
        drone-tokens (get-in usage [:drones drone-id :total-tokens] 0)
        wave-tokens (get-in usage [:waves wave-id :total-tokens] 0)
        hour-tokens (get-in usage [:hourly :tokens-used] 0)
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

(def ^:private premium-rates
  "Reference rates for premium models to calculate savings."
  {:claude-sonnet {:input 3.0 :output 15.0}
   :gpt-4         {:input 10.0 :output 30.0}
   :default       {:input 2.0 :output 8.0}})

(defn estimate-premium-cost
  "Estimate what the same request would cost with premium models."
  [input-tokens output-tokens & [model-key]]
  (let [rates (get premium-rates (or model-key :default))
        input-cost (* input-tokens (/ (:input rates) 1000000))
        output-cost (* output-tokens (/ (:output rates) 1000000))]
    (+ input-cost output-cost)))

(defn track-drone-usage!
  "Track token usage for a drone task."
  [drone-id {:keys [input-tokens output-tokens task-preview wave-id]}]
  (let [total-tokens (+ (or input-tokens 0) (or output-tokens 0))
        premium-cost (estimate-premium-cost input-tokens output-tokens)
        now (java.time.Instant/now)]

    (swap! *usage
           (fn [u]
             (cond-> u
               true (assoc-in [:drones drone-id]
                              {:input-tokens input-tokens
                               :output-tokens output-tokens
                               :total-tokens total-tokens
                               :timestamp now
                               :task-preview (subs (or task-preview "") 0
                                                   (min 80 (count (or task-preview ""))))})
               wave-id (update-in [:waves wave-id :total-tokens] (fnil + 0) total-tokens)
               wave-id (update-in [:waves wave-id :drone-count] (fnil inc 0))
               true (update-in [:hourly :tokens-used] + total-tokens)
               true (update-in [:cumulative :total-input] + input-tokens)
               true (update-in [:cumulative :total-output] + output-tokens)
               true (update-in [:cumulative :total-requests] inc)
               true (update-in [:cumulative :saved-vs-premium] + premium-cost))))

    (log/info "Tracked drone usage"
              {:drone-id drone-id
               :input-tokens input-tokens
               :output-tokens output-tokens
               :total-tokens total-tokens
               :premium-savings (format "$%.4f" premium-cost)})

    (get-in @*usage [:drones drone-id])))

(defn start-wave-tracking!
  "Start tracking a new wave."
  [wave-id]
  (let [now (java.time.Instant/now)]
    (swap! *usage assoc-in [:waves wave-id]
           {:total-tokens 0
            :drone-count 0
            :started-at now})
    (get-in @*usage [:waves wave-id])))

(defn complete-wave-tracking!
  "Mark wave tracking as complete."
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

(defn truncate-context
  "Truncate context to fit within token budget, keeping system and recent messages."
  [messages token-budget]
  (let [original-tokens (count-messages-tokens messages)]
    (if (<= original-tokens token-budget)
      {:messages messages
       :truncated? false
       :original-tokens original-tokens
       :final-tokens original-tokens}

      (let [system-msg (first messages)
            other-msgs (rest messages)
            system-tokens (count-message-tokens system-msg)
            remaining-budget (- token-budget system-tokens)
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

(defn get-usage-summary
  "Get comprehensive usage summary for dashboard."
  []
  (refresh-hourly-window!)
  (let [usage @*usage
        budgets @*budgets
        cumulative (:cumulative usage)
        hourly (:hourly usage)
        recent-drones (->> (:drones usage)
                           (map (fn [[id data]]
                                  (assoc data :drone-id id)))
                           (sort-by :timestamp #(compare %2 %1))
                           (take 10)
                           (mapv #(select-keys % [:drone-id :total-tokens :task-preview :timestamp])))
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
  "Identify tasks that consumed the most tokens."
  [& [limit]]
  (let [n (or limit 10)]
    (->> (:drones @*usage)
         (map (fn [[id data]]
                (assoc data :drone-id id)))
         (sort-by :total-tokens >)
         (take n)
         (mapv #(select-keys % [:drone-id :total-tokens :task-preview :timestamp])))))

(defn reset-usage!
  "Reset all usage tracking."
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
  "Reset only hourly tracking."
  []
  (swap! *usage assoc :hourly {:tokens-used 0
                               :window-start (java.time.Instant/now)})
  (log/info "Hourly cost tracking reset"))
