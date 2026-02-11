(ns hive-mcp.agent.drone.feedback
  "Drone feedback loop for learning from execution results and pattern-based routing."
  (:require [hive-mcp.tools.memory.crud :as mem-crud]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def result-classes
  "Canonical result classifications for drone telemetry."
  #{:success
    :rate-limited
    :timeout
    :empty-response
    :model-error
    :file-conflict
    :validation-error
    :connection-error
    :unknown-failure})

(defn classify-result
  "Classify a drone execution result into a pattern category."
  [drone-result]
  (let [status (:status drone-result)
        error (or (:error drone-result) "")
        error-lower (str/lower-case (str error))
        output (:output drone-result)
        result (:result drone-result)]
    (cond
      (= status :completed) :success
      (= status :success) :success

      (or (re-find #"rate.?limit" error-lower)
          (re-find #"429" error-lower)
          (re-find #"too.?many.?requests" error-lower))
      :rate-limited

      (or (re-find #"timeout" error-lower)
          (re-find #"timed.?out" error-lower)
          (re-find #"deadline.?exceeded" error-lower))
      :timeout

      (or (and (nil? output) (nil? result))
          (and (string? output) (str/blank? output))
          (and (string? result) (str/blank? result)))
      :empty-response

      (or (re-find #"model.?error" error-lower)
          (re-find #"invalid.?response" error-lower)
          (re-find #"content.?filter" error-lower)
          (re-find #"safety" error-lower))
      :model-error

      (or (re-find #"conflict" error-lower)
          (re-find #"locked" error-lower)
          (re-find #"claimed" error-lower))
      :file-conflict

      (or (re-find #"validation" error-lower)
          (re-find #"invalid.?input" error-lower)
          (re-find #"schema" error-lower))
      :validation-error

      (or (re-find #"connection" error-lower)
          (re-find #"network" error-lower)
          (re-find #"unreachable" error-lower)
          (re-find #"econnrefused" error-lower))
      :connection-error

      :else :unknown-failure)))

(defn- make-pattern-content
  "Create content string for pattern memory entry."
  [task-type model result-class duration-ms]
  (str "Drone pattern: " (name task-type)
       " with " model
       " -> " (name result-class)
       (when duration-ms (str " (" duration-ms "ms)"))))

(defn- make-pattern-tags
  "Create tags for pattern memory entry."
  [task-type model result-class]
  ["drone-pattern"
   (str "task:" (name task-type))
   (str "model:" model)
   (str "result:" (name result-class))])

(defn record-pattern!
  "Record a drone execution pattern to memory."
  [task-type model result-class & [{:keys [duration-ms directory agent-id]}]]
  (try
    (let [content (make-pattern-content task-type model result-class duration-ms)
          tags (make-pattern-tags task-type model result-class)
          params {:type "note"
                  :content content
                  :tags tags
                  :duration "short"
                  :directory directory
                  :agent_id agent-id}]
      (mem-crud/handle-add params)
      (log/debug "Recorded drone pattern" {:task-type task-type
                                           :model model
                                           :result result-class}))
    (catch Exception e
      (log/warn e "Failed to record drone pattern"))))

(defn query-patterns
  "Query historical patterns for a task type and/or model."
  [{:keys [task-type model limit directory]
    :or {limit 20}}]
  (try
    (let [tags (cond-> ["drone-pattern"]
                 task-type (conj (str "task:" (name task-type)))
                 model (conj (str "model:" model)))
          result (mem-crud/handle-query {:type "note"
                                         :tags tags
                                         :limit limit
                                         :directory directory})
          parsed (when (:text result)
                   (json/read-str (:text result) :key-fn keyword))]
      (or (:entries parsed) []))
    (catch Exception e
      (log/warn e "Failed to query drone patterns")
      [])))

(defn get-success-rate
  "Calculate success rate for a task-type/model combination."
  [task-type model & [{:keys [directory]}]]
  (let [patterns (query-patterns {:task-type task-type
                                  :model model
                                  :directory directory
                                  :limit 100})]
    (when (seq patterns)
      (let [total (count patterns)
            successes (count (filter #(str/includes? (or (:content %) "") "-> :success")
                                     patterns))]
        {:success-count successes
         :total-count total
         :success-rate (if (pos? total)
                         (double (/ successes total))
                         0.0)}))))

(def ^:private min-samples-for-routing
  "Minimum pattern samples before making routing decisions."
  3)

(def ^:private failure-threshold
  "Success rate below which a model/task combo is avoided."
  0.3)

(defn should-avoid-combo?
  "Check if a task-type/model combination should be avoided based on historical patterns."
  [task-type model & [opts]]
  (when-let [stats (get-success-rate task-type model opts)]
    (and (>= (:total-count stats) min-samples-for-routing)
         (< (:success-rate stats) failure-threshold))))

(defn recommend-model
  "Recommend a model for a task type based on historical patterns."
  [task-type available-models & [{:keys [_directory] :as opts}]]
  (if (empty? available-models)
    nil
    (let [model-stats (for [model available-models]
                        (let [stats (get-success-rate task-type model opts)]
                          {:model model
                           :success-rate (or (:success-rate stats) 0.5)
                           :samples (or (:total-count stats) 0)}))
          sorted (sort-by (juxt #(if (pos? (:samples %)) 0 1)
                                #(- (:success-rate %)))
                          model-stats)]
      (:model (first sorted)))))

(defn aggregate-weekly-stats
  "Aggregate pattern statistics for analysis."
  [& [{:keys [directory]}]]
  (let [patterns (query-patterns {:limit 500 :directory directory})]
    (if (empty? patterns)
      {:message "No patterns recorded yet"
       :by-model {}
       :by-task-type {}
       :by-result {}
       :problematic []
       :top-performers []}

      (let [parsed (for [p patterns]
                     (let [content (or (:content p) "")
                           task-match (re-find #"task:(\S+)" (str (:tags p)))
                           model-match (re-find #"model:(\S+)" (str (:tags p)))
                           result-match (re-find #"result:(\S+)" (str (:tags p)))]
                       {:task-type (or (second task-match) "unknown")
                        :model (or (second model-match) "unknown")
                        :result (or (second result-match) "unknown")
                        :success? (str/includes? content "-> :success")}))

            by-model (group-by :model parsed)
            model-stats (into {}
                              (for [[model entries] by-model]
                                [model {:total (count entries)
                                        :successes (count (filter :success? entries))
                                        :success-rate (double (/ (count (filter :success? entries))
                                                                 (count entries)))}]))

            by-task (group-by :task-type parsed)
            task-stats (into {}
                             (for [[task entries] by-task]
                               [task {:total (count entries)
                                      :successes (count (filter :success? entries))
                                      :success-rate (double (/ (count (filter :success? entries))
                                                               (count entries)))}]))

            by-result (frequencies (map :result parsed))

            combo-groups (group-by (juxt :task-type :model) parsed)
            problematic (->> combo-groups
                             (map (fn [[[task model] entries]]
                                    {:task-type task
                                     :model model
                                     :total (count entries)
                                     :success-rate (double (/ (count (filter :success? entries))
                                                              (count entries)))}))
                             (filter #(and (>= (:total %) 3)
                                           (< (:success-rate %) 0.3)))
                             (sort-by :success-rate))

            top-performers (->> combo-groups
                                (map (fn [[[task model] entries]]
                                       {:task-type task
                                        :model model
                                        :total (count entries)
                                        :success-rate (double (/ (count (filter :success? entries))
                                                                 (count entries)))}))
                                (filter #(and (>= (:total %) 3)
                                              (>= (:success-rate %) 0.8)))
                                (sort-by :success-rate >))]

        {:total-patterns (count patterns)
         :by-model model-stats
         :by-task-type task-stats
         :by-result by-result
         :problematic (vec problematic)
         :top-performers (vec top-performers)}))))

(defn generate-recommendations
  "Generate prompt improvement recommendations based on patterns."
  [& [opts]]
  (let [stats (aggregate-weekly-stats opts)
        problematic (:problematic stats)]
    (if (empty? problematic)
      [{:recommendation "No problematic patterns detected"
        :action "Continue monitoring drone executions"}]
      (for [{:keys [task-type model success-rate total]} problematic]
        {:task-type task-type
         :model model
         :success-rate success-rate
         :samples total
         :recommendation (cond
                           (< success-rate 0.1)
                           (str "Consider removing " model " from " task-type " tasks - very high failure rate")

                           (< success-rate 0.2)
                           (str "Review prompts for " task-type " tasks with " model " - likely prompt/model mismatch")

                           :else
                           (str "Monitor " task-type "/" model " combo - below acceptable threshold"))
         :action (cond
                   (str/includes? (str task-type) "refactor")
                   "Try simpler refactoring prompts or break into smaller steps"

                   (str/includes? (str task-type) "implement")
                   "Provide more context and examples in implementation prompts"

                   :else
                   "Review task prompts for clarity and specificity")}))))
